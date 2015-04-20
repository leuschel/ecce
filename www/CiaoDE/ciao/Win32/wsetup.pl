:- module(wsetup,[main/1],[assertions,isomodes]).
% This, for making it a user file when debugging:
% :- use_package([assertions,isomodes]).

% --------------------------------------------------------------------------
:- comment(title,"Setup Script for Ciao Installation under Windows").
% --------------------------------------------------------------------------

:- comment(module,"This module performs the installation of Ciao under
   Windows. 

   If called with no argument, or with an argument @tt{main} it performs 
   the following actions:

   @begin{itemize}

   @item Create the default compiler header and prototype @tt{.bat}
         files for executables.

%    @item Build the info index (@file{dir}) in
%          @file{SRC/doc/reference}, the setup script
%          (@file{ciao-mode-init.el}) in @file{CIAOSRC} and the @apl{emacs}
%          mode (@file{ciao.el}) in @file{SRC/emacs}.

%    @item Create a file @tt{ciao.reg} suitable for entering the
%          Ciao/Prolog-related associations into the Windows registry
%          using @apl{regedit}.

   @end{itemize}

   If called with a single argument @tt{client} it performs only the
   creation of the registry update file (in
   @tt{c:/WINDOWS/TEMP/ciaocl.reg}). This is useful for updating the
   registry of the clients (which typically cannot write into the
   shared areas where Ciao would be installed) in a server/client
   setup.

   ").

:- comment(author,"Daniel Cabeza").
:- comment(author,"Manuel Hermenegildo").

% --------------------------------------------------------------------------

:- use_module(setup_bat).
:- use_module(utilities).

:- use_module(library(lists), [append/3,list_concat/2]).
:- use_module(library(streams), [open_output/2, close_output/1]).

:- use_module(library(system), [
        getenvstr/2, 
        working_directory/2,
        cd/1,
        cyg2win/3]).
:- use_module(library('make/system_extra'),
	[cat/2,
         delete_file/1,
         readf/2,
         replace_strings_in_file/3,
	 '-'/1,
	 writef/3]).
:- use_module(registry,[win_reg/3]).

:- use_module(library(system)).
:- use_module(library(distutils)).
:- use_module(library(file_utils)).

% Extension used for executables in Windows:
:- include(library('compiler/win_exec_ext')).

main([]) :-
	!,
	wsetup:main([main]).
main([T]) :-
	!,
	setup_mess(['                                           \n',
		    '*    **** Ciao Windows ',T,' installation ****    \n',
                    '*                                           \n']),

        working_directory(SDir,SDir), % SRC
%	ciaodesrc(SDir),
        display(SDir),nl,
	winpath(SDir,SPath),
        atom_codes(SPath,CiaoPath),
        display_string(CiaoPath),nl,
	get_os(OS), atom_codes(OS, OSC),
	get_arch(Arch), atom_codes(Arch, ArchC),
        list_concat([CiaoPath,"\\ciao\\bin\\", OSC, ArchC, "\\ciaoengine.exe"], EnginePath),
        list_concat(["""",EnginePath,""""],EngineQuot),
	replace_characters(CiaoPath, 0'\\, 0'/, SRCS),
        atom_codes(SRC,SRCS),
	(  T == main
        ->
	   atom_concat(SDir,'/doc',IDir),
	   make_infoindex(IDir),
	   atom_concat(SRC,'/ciao/emacs-mode',EDir),
	   -'make_ciao-mode-init'(SRC,IDir,EDir),
	   -make_ciaomode(SRC,IDir,EDir),

           -make_header(SDir),
           -make_bats(EngineQuot),
           -make_foremacs(SRC)

%           CReg = 'ciao.reg'
        ;
%	    CReg =  'c:/WINDOWS/TEMP/ciaocl.reg'
	    true
	),

%	setup_mess(['Building Win register file ',CReg,'.\n']),
%         ( getenvstr('OS',"Windows_NT") -> % Also for Windows 2000
%             Engine = EnginePath
%         ; Engine = EngineQuot
%         ),
%        exec_ext(ExeExt),
%        ciaoreg(CiaoPath, Engine, ExeExt, Registry),
%        win_reg(Registry, String, []),
%        open_output(CReg,Out),
%        display_string(String),
%        close_output(Out),

	setup_mess([
  '                                           \n',
  '*    **** Installation Complete ****       \n',
%   '*                                          \n',
%   '*    You may need to reboot for the changes in the registry take effect\n',
  '*                                          \n']),
 	line,
% Done in bat, after regedit...
%	display('Hit any key to exit... '),flush_output,
%	get_code(_),
	true.
main(Args) :-
	display_list(['ERROR: illegal arguments '|Args]).

make_infoindex(IDir) :-
	atom_concat(IDir,'/dir', InfoIndex),
	setup_mess(['Building ',InfoIndex, ' (info index).\n']),
	file_to_string('ciao/doc/common/CiaoHead.info', String, Tail0),
	file_to_string('doc/ciao.infoindex', Tail0, Tail1),
	% The list of extra packages must be determined dinamically
	file_to_string('doc/lpdoc.infoindex', Tail1, Tail2),
	file_to_string('doc/ciaopp.infoindex', Tail2, Tail3),
	file_to_string('ciao/doc/common/CiaoTail.info', Tail3, []),
	string_to_file(String, InfoIndex).

:- comment(bug, "1.13 must be changed with the procedure to calculate
   versions!!!").

:- comment(bug, "Needs more integration with the predicates for common
   installation.").

:- comment(bug, "We need also fix dirs_auto.pl, and ciaopp auto
	generated prolog files in order to let ciaopp and lpdoc run in
	windows.").

'make_ciao-mode-init'(SDir,IDir,EDir) :-
	cd(EDir),
	setup_mess(['Building ',EDir,'/ciao-mode-init.el (emacs setup).\n']),
	atom_codes(EDir,EDirS),
	atom_codes(IDir,IDirS),
	replace_strings_in_file([["<CIAOLIBDIR>", EDirS], 
                                 ["<LPDOCDIR>",   IDirS]],
                                'ciao-mode-init.el.skel',
				'ciao-mode-init.el'),
        atom_codes(SDir,SDirS),
	%% This was specific to Win, but note that now being done in general 
	%% also for xemacs, so it may not be necessary here...
	list_concat([
	   ";; Specific to Windows installation:\n",
	   ";; Location of Ciao shell\n",
	   "(setq ciao-system (convert-standard-filename \n",
	   "      \"",SDirS,"/bin/ciaosh-1.13.bat\"))\n",
	   ";; Location of info manuals\n",
	   "(setq Info-default-directory-list  (cons \n",
	   "      \"",SDirS,"/doc\" \n",
	   "      Info-default-directory-list))\n",
           %%% Should put this in a separate file in SRC/emacs-mode and cat it now:
           ";; Make things nicer (but check if you are already doing it)\n",
	   "(global-font-lock-mode)\n",
	   "(transient-mark-mode t)\n",
	   ";; Help for using the Windows command.com as your shell\n",
	   ";; (comment out if you use bash, etc.):\n",
	   "(setq process-coding-system-alist
	    '((\"cmdproxy\" . (raw-text-dos . raw-text-dos))))\n",
	   ";; Preventing ctrln-m's from being printed in the shell\n",
	   "(add-hook 'comint-output-filter-functions ",
	   "  'shell-strip-ctrl-m nil t)\n",
	   "; -----",
	   "----------------------------------------------------------------\n"
		    ],NewLisp),
	writef(NewLisp,append,'ciao-mode-init.el'),
	cd(SDir).

make_foremacs(SDir):-
	atom_codes(SDir, SDirS),
        list_concat([
                        ";; Include this line in your ~/.emacs file:\n",
                        "(load-file \"", SDirS,  "/ciao-mode-init.el\")\n"],
                        ForEmacs),
        cd(SDir),
        writef(ForEmacs, write, 'ForEmacs.txt').

make_ciaomode(SDir,IDir,EDir) :-
	cd(EDir),
	setup_mess(['Building ',EDir,'/ciao.el (emacs mode).\n']),
	atom_codes(EDir,EDirS),
	atom_codes(IDir,IDirS),
	replace_strings_in_file([[ "\n", "\n;" ]],
                                'ciao-mode-init.el','ciao-mode-init.tmp'),
	cat(['ciao.el.header','ciao-mode-init.tmp','ciao.el.body'],'ciao.el.tmp'),
	delete_file('ciao-mode-init.tmp'),
	replace_strings_in_file([[ "<CIAOREALLIBDIR>", EDirS],
                                 ["<LPDOCDIR>", IDirS]],
                                'ciao.el.tmp','ciao.el'),
        delete_file('ciao.el.tmp'),
	cd(SDir).

make_header(CiaoPath) :-
	get_os(OS),
	get_arch(Arch),
	setup_mess(['Building header to ',CiaoPath,
                    '/ciao/bin/', OS, Arch, '/ciaoengine.exe.\n']),
%        open_output('$/ciao/lib/compiler/header', Out),
        atom_concat(CiaoPath, '/ciao/lib/compiler/header', HeaderPath),
        open_output(HeaderPath, Out),
        display('#!/bin/sh\n'),
        display('INSTENGINE=\"'),
        display(CiaoPath),
        display_list(['/ciao/bin/',OS,Arch,'/ciaoengine.exe\"\n']),
        display('ENGINE=${CIAOENGINE:-${INSTENGINE}}\n'),
        display('exec "$ENGINE" "$@" -C -b $0\n\^L\n'),
        close_output(Out).

:- pred ciaoreg(+string, +string, +atm, -string).

ciaoreg(CiaoPath, Engine, ExeExt, Reg) :-
        append(CiaoPath, "\\ciao\\Win32\\ciaoexe.ico,0", ExeIco),
        append(CiaoPath, "\\ciao\\Win32\\ciaoitf.ico,0", ItfIco),
        append(CiaoPath, "\\ciao\\Win32\\ciaopo.ico,0", PoIco),
        append(CiaoPath, "\\ciao\\Win32\\ciaoasr.ico,0", AsrIco),
        append(CiaoPath, "\\ciao\\Win32\\ciaopl.ico,0", PlIco),
        append(CiaoPath, "\\ciao\\Win32\\ciaoscrt.ico,0", ScrtIco),
        append(Engine, " %2 %3 %4 %5 %6 %7 %8 %9 -C -b ""%1""",
               ExeCommand),
        append(Engine," -u ""%1"" -C -i -b $\\bin\\ciaosh", LoadFileCmd),
        append(Engine," ""%1"" -C -b $\\bin\\ciaoc", MakeExecCmd),
        append(Engine,
               " ""%1"" %2 %3 %4 %5 %6 %7 %8 %9 -C -b $\\bin\\ciao-shell",
               ExeScrtCmd),
        append(Engine, " -C -b %s %s",IIS_string),
        ROOT = 'HKEY_CLASSES_ROOT',
        Reg = [
        % .cpx files
        [[ROOT,ExeExt],
          '@'="ciaoexefile"],
        [[ROOT,ciaoexefile],
          '@'="Ciao executable"%,
         % 'EditFlags'=0x00000000
        ],
        [[ROOT,ciaoexefile,'DefaultIcon'],
          '@'=ExeIco],
        [[ROOT,ciaoexefile,shell],
          '@'=""],
        [[ROOT,ciaoexefile,shell,open],
          'EditFlags'=0x01000000],
        [[ROOT,ciaoexefile,shell,open,command],
          '@'=ExeCommand],
        % .pl files
        [[ROOT,'.pl'],
          '@'="ciaofile"],
        [[ROOT,ciaofile],
          '@'="Ciao Prolog source",
          'EditFlags'=0x00000000],
        [[ROOT,ciaofile,'DefaultIcon'],
          '@'=PlIco],
        [[ROOT,ciaofile,shell],
          '@'=""],
        [[ROOT,ciaofile,shell,open],
          'EditFlags'=0x01000000],
        [[ROOT,ciaofile,shell,open,command],
          '@'=""],
        [[ROOT,ciaofile,shell,load_file],
          '@'="Load into Toplevel shell"],
        [[ROOT,ciaofile,shell,load_file,command],
          '@'=LoadFileCmd],
        [[ROOT,ciaofile,shell,make_executable],
          '@'="Make executable"],
        [[ROOT,ciaofile,shell,make_executable,command],
          '@'=MakeExecCmd],
        % .pls files
        [[ROOT,'.pls'],
          '@'="ciaoplsfile"],
        [[ROOT,ciaoplsfile],
          '@'="Ciao Prolog script",
          'EditFlags'=0x00000000],
        [[ROOT,ciaoplsfile,'DefaultIcon'],
          '@'=ScrtIco],
        [[ROOT,ciaoplsfile,shell],
          '@'=""],
        [[ROOT,ciaoplsfile,shell,open],
          'EditFlags'=0x01000000],
        [[ROOT,ciaoplsfile,shell,open,command],
          '@'=ExeScrtCmd],
        % .itf files
        [[ROOT,'.itf'],
          '@'="ciaoitffile"],
        [[ROOT,ciaoitffile],
          '@'="Ciao interface file",
          'EditFlags'=0x00000000],
        [[ROOT,ciaoitffile,'DefaultIcon'],
          '@'=ItfIco],
        [[ROOT,ciaoitffile,shell],
          '@'=""],
        % .po files
        [[ROOT,'.po'],
          '@'="ciaopofile"],
        [[ROOT,ciaopofile],
          '@'="Ciao object file",
          'EditFlags'=0x00000000],
        [[ROOT,ciaopofile,'DefaultIcon'],
          '@'=PoIco],
        [[ROOT,ciaopofile,shell],
          '@'=""],
        % .asr files
        [[ROOT,'.asr'],
          '@'="ciaoasrfile"],
        [[ROOT,ciaoasrfile],
          '@'="Ciao assertions file",
          'EditFlags'=0x00000000],
        [[ROOT,ciaoasrfile,'DefaultIcon'],
          '@'=AsrIco],
        [[ROOT,ciaoasrfile,shell],
          '@'=""],
	[['HKEY_LOCAL_MACHINE','SOFTWARE','Ciao Prolog'],
	 ciao_dir=CiaoPath],
        % For Microsoft's IIS
        [['HKEY_LOCAL_MACHINE','SYSTEM','CurrentControlSet',
          'Services','W3SVC','Parameters','Script Map'],
          ExeExt=IIS_string]
        ].
