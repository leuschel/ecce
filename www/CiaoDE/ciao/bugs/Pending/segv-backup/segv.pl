:- module(_,_,[pillow]).

:- use_module(library(aggregates), [findall/3]).
:- use_module(library(sort), [sort/2]).
:- use_module(library(lists), [length/2,contains1/2]).
:- use_module(library(file_utils), [file_to_string/2]).
:- use_module(library(terms), [atom_concat/2]).
:- use_module(library(read), [read/1]).
:- use_module(library(system), [file_exists/2]).
:- use_module(library(streams), [open_input/2, close_input/1]).

data_directory('db/').

main :-
	get_internals,
	wdbi(comps_actualization, comps).

:- data db_def/3.
:- data db_tables/2.
:- data table_def/5.
:- data table_order/3.
:- data table_fields/5.
:- data field_def/4.
:- data type_def/2.
:- data type_arg/4.

internal_table(db_def, 3).
internal_table(db_tables, 2).
internal_table(table_def, 5).
internal_table(table_order, 3).
internal_table(table_fields, 5).
internal_table(field_def, 4).
internal_table(type_def, 2).
internal_table(type_arg, 4).

get_internals :-
        internal_table(Table, N),
          functor(TableCall, Table, N),
          get_table_facts(Table, TableCall),
        fail ; true.

get_table_facts(Table, Fact) :-
        data_directory(Dir),
        atom_concat(Dir, Table, File),
        ( file_exists(File, 4) ->
            atom_concat(File, '.lck', FileLock),
            open(FileLock, read, SLock, [lock]),
            meta_fact(Fact, MFact), % optimization to avoid repeated expansion
            open_input(File, IH),
            repeat,
              read(Data),
            ( Data == end_of_file, !
            ; Data = Fact,
              assertz_fact(MFact),
              fail
            ),
            close_input(IH),
            close(SLock)
        ; true
        ).

:- meta_predicate meta_fact(fact,_).

meta_fact(X,X).
                         
wdbi(Table, DB) :-
        db_tables(DB, Table), % Deleting this there is no segv
        table_def(Table, _, _, _, IniNrows),
        ( contains1(FormInput, '_rows'=Rows) ->
            Nrows = Rows
        ; Nrows = IniNrows
        ),
        get_fields(Table, Fields),
        wdbi_show_search(DB, Table, Fields, Nrows, FormInput).


get_fields(Table, Fields) :-
        findall(Field_Info,
                table_fields_info(Table, Field_Info),
                Fields_u),
        sort(Fields_u, Fields).

table_fields_info(Table, f(Order, Field, Req, InKey)) :-
        table_fields(Table, Arg, Field, Req, InKey),
        atom_number(Arg, Order).

wdbi_show_search(DB, Table, Fields, Nrows, FormInput) :-
        db_tb_header(DB, Table, DbTbHead),
        length(Fields, NFields),
        fields_header(Fields, Headers),
        fields_inputs(Fields, FormInput, Table, Inputs, Fixing),
        output_template('templates/search.html',
                        [dbtbhead=DbTbHead, db=DB, table=Table,
                         nfields=NFields, headers=Headers, inputs=Inputs,
                         fixing=Fixing,nrows=Nrows]).

db_tb_header(DB, Table, DbTbHead) :-
        db_def(DB, DBName, _),
        table_def(Table, TbName, _, _, _),
        get_DB_defs(DB_defs),
        get_Table_defs(DB, Table_defs),
        open_info_js([db=DB], OpenInfoDB),
        open_info_js([table=Table], OpenInfoTb),
        submit_if_selected(SiS),
        choose_select('_db', DB_defs, SiS, ChDbSelect),
        choose_select('_table', Table_defs, SiS, ChTbSelect),
        get_template('templates/db_tb_head.html',
                     [openinfodb=OpenInfoDB, dbname=DBName,
                      chdbselect=ChDbSelect, openinfotb=OpenInfoTb,
                      tbname=TbName, chtbselect=ChTbSelect, db=DB],
                     DbTbHead).

get_DB_defs(DB_defs) :-
        findall(def(DB, DBname, DBinfo),
                db_def(DB, DBname, DBinfo), DB_defs_u),
        sort(DB_defs_u, DB_defs).

get_Table_defs(DB, Table_defs) :-
        findall(def(Table, TableName, TableInfo),
                ( db_tables(DB, Table),
                  table_def(Table, TableName, TableInfo, _, _) ),
                Table_defs_u),
        sort(Table_defs_u, Table_defs).

choose_select(What, Defs, OnChange, Select) :-
        Select = env(select, [name=What, onChange=OnChange],
                     [env(option, [value='', selected], '-choose-') | Options]),
        choose_select_opts(Defs, Options).

choose_select_opts([], []).
choose_select_opts([def(Code, Name, _Info)|Defs], [Opt|Opts]) :-
        Opt = env(option, [value=Code], Name),
        choose_select_opts(Defs, Opts).

open_info_js(Params, OpenInfo) :-
        url_query(Params, URLParamsStr),
        atom_codes(URLParams, URLParamsStr),
        atom_concat(['javascript:open_info(\'',URLParams, '\')'], OpenInfo).

fields_header([], []).
fields_header([f(_,Field,_,_)|Fs], [H|Hs]) :-
        open_info_js([field=Field], OpenInfo),
        field_def(Field, FieldName, _, _),
        H = env(td, [class=tbhead],
                env(a, [href=OpenInfo, class=nombre], FieldName)),
        fields_header(Fs, Hs).

fields_inputs(Fields, FormInput, Table, Inputs, Fixing) :-
        get_cookies(Cookies),
        extract_field_values(Fields, FormInput, Cookies, FVals, FNames),
        fields_inputs_(FNames, FVals, Table, Inputs),
        fields_fixing(FNames, Fixing).

extract_field_values([], _, _, [], []).
extract_field_values([f(_,F,_,_)|Fs], FormInput, Cookies, [F=V|FVs], [F|FNs])
        :-
        ( contains1(FormInput, F=V) -> true
        ; contains1(Cookies, F=V) -> true
        ; V = '*'
        ),
        extract_field_values(Fs, FormInput, Cookies, FVs, FNs).

fields_inputs_([], _, _, []).
fields_inputs_([Field|Fs], FVals, Table, [I,'\n'|Is]) :-
        I = env(td, [valign=top], Input),
        field_input(Field, FVals, Table, Input),
        fields_inputs_(Fs, FVals, Table, Is).

field_input(Field, FVals, Table, Input) :-
        field_def(Field, _, _, Type),
        type_def(Type, Kind),
        contains1(FVals, Field=Value),
        onchange_submit_js(Table, Field, OnChange),
        field_input_of_kind(Kind, Type, Field, Value, FVals, OnChange, Table,
                            Input).

fields_fixing([], []).
fields_fixing([Field|Fs], [I|Is]) :-
        atom_concat(['javascript:set_cookie_val(\'',Field,'\')'], JsSetCookie),
        I = env(td, [class=tbfixing],
                env(a,[href=JsSetCookie],env(span,[class=peque],'Fix'))),
        fields_fixing(Fs, Is).

onchange_submit_js(Table, Field, OnChange) :-
        ( conditional_dependency(Table, Field) ->
            OnChange = [onChange='submit(this.form)']
        ; OnChange = []
        ).

conditional_dependency(Table, Field) :-
        table_fields(Table, _Order, OtherField, _Req, _InKey),
        field_def(OtherField, _, _, Type),
        type_def(Type, conditional),
        type_arg(Type, depends_on, _, Field).

field_input_of_kind(Kind, Type, Field, Value, FVals, OnChange, Table,
                    Input) :-
        (
          ( Kind = text
          ; Kind = integer
          ; Kind = float
          ; Kind = identifier
          ; Kind = computation
          ) ->
            type_arg(Type, size, _, Size),
            Input =
              input$[type=text, class=formu, name=Field, size=Size,
                     value=Value | OnChange]
        ;
          Kind = textarea ->
            type_arg(Type, width, _, Width),
            type_arg(Type, height, _, Height),
            Input =
              env(textarea,
                  [name=Field, cols=Width, rows=Height, wrap=virtual |
                   OnChange], Value)
        ;
          Kind = date ->
            ( type_arg(Type, help, _, yes) ->
                date_order_or_ymd(Type, Order),
                date_order_help(Order, Help)
            ;
              Help = []
            ),
            Input =
              [input$[type=text, class=formu, name=Field, size=10,
                      value=Value | OnChange]
               | Help]
        ;
          Kind = external_key ->
            type_arg(Type, defined_in, _, DefTable),
            ( DefTable = Table ->
                type_arg(Type, type, _, PrimType),
                type_def(PrimType, PrimKind),
                field_input_of_kind(PrimKind, PrimType, Field, Value, FVals,
                                    OnChange, Table, Input)
            ;
              options_from_table(DefTable, Type, Value, Options),
              Input = env(select, [name=Field | OnChange],
                          [env(option, [value='*'], '*') | Options])
            )
        ).

options_from_table(DefTable, Type, Value, Options) :-
        type_arg(Type, defined_as, _, DefField),
        table_fields(DefTable, ArgF_a, DefField, _, _),
        atom_number(ArgF_a, ArgF),
	get_values_from_table_column(DefTable, ArgF, Values_U),
	sort(Values_U, Values),
	values_to_options(Values, Value, Options).

values_to_options([], _, []).
values_to_options([Value|Values], SelValue,
                  [env(option, [value=Value|SelectedP], Value)|Envs]) :-
        selected_param(Value, SelValue, SelectedP),
        values_to_options(Values, SelValue, Envs).

selected_param(Value, SelValue, SelectedP) :-
        SelValue = Value -> SelectedP = [selected] ; SelectedP = [].

output_template(File, _Params) :-
        file_to_string(File, _Str).

get_template(File, Params, HTML) :-
        file_to_string(File, Str),
        html_template(Str, HTML, Params).

submit_if_selected('if (this.selectedIndex > 0) submit(this.form)').

date_order_help(dmy, [' d/m/y']).
date_order_help(mdy, [' m/d/y']).
date_order_help(ymd, [' y/m/d']).

date_order_or_ymd(Type, Order) :-
        ( type_arg(Type, order, _, Order) -> true
        ; Order = ymd
        ).


get_values_from_table_column(Table, Arg, Values) :-
	data_directory(Dir),
        atom_concat(Dir, Table, File),
        ( file_exists(File, 4) ->
            atom_concat(File, '.lck', FileLock),
            open(FileLock, read, SLock, [lock]),
            open_input(File, IH),
	    get_values_arg(Arg, Values),
            close_input(IH),
            close(SLock)
        ; Values = []
        ).

get_values_arg(Arg, Values) :-
display('+'),
	read(Data),
display('*'), nl,
	( Data == end_of_file ->
	    Values = []
	;
	    arg(Arg, Data, Value),
 display(Value), nl,
	    Values = [Value | Values_],
	    get_values_arg(Arg, Values_)
	).
