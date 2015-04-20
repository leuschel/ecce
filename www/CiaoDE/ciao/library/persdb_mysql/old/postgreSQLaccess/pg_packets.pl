:- module(pg_packets,
	[
	    int32/2,int16/2,null_ended_string/2,first_null_ended_string/3,
	    first_N_chars/4,
	    fieldsDescrList2String/4, 
	    is_zero_bit/2,sizeInBits2SizeInBytes/2,
%% until here, exported only for debugging
	    startup_packet/3,
	    errorResponse_packet/2,
	    authentication_packet/1,
	    authenticationOk_packet/1,
	    authenticationUnencryptedPassword_packet/1,
	    authenticationEncryptedPassword_packet/2,
	    authenticationKerberosV4_packet/1,
	    authenticationKerberosV5_packet/1,
	    unencryptedPassword_packet/2,
	    backendKeyData_packet/3,%% packet from protocol 2.0, not still supported
	    noticeResponse_packet/2,
	    readyForQuery_packet/1, %% packet from protocol 2.0, not still supported
	    query_packet/2,
	    cursorResponse_packet/2,
	    completedResponse_packet/2,
	    rowDescription_packet/5,
	    rowsData_packet/6,
	    asciiRow_packet/7,
	    binaryRow_packet/7
	],
	[assertions, regtypes, basicmodes]).

:- use_module(library(lists)).
:- use_module(library(messages)).


%% TO DELETE: for testing
 :- multifile issue_debug_messages/1.
 :- data issue_debug_messages/1.
 issue_debug_messages('pg_packets').

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Startup packets       %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% FrontEnd packets
startup_packet(DbName,User,FrontEndPacket):-
	startup_packet(296,1,0,DbName,User,"","",FrontEndPacket).

startup_packet(PacketSize, ProtMajor, ProtMinor, DbName, User, 
   AdditionalArguments, OptionalTty, FrontEndPacket) :-
	int32(PacketSize,PacketSizeCharList), %size of the packet in bytes
	int16(ProtMajor,ProtMajorCharList), % protocol major
	int16(ProtMinor,ProtMinorCharList), % protocol minor
	limString64(DbName,DBCharList), % database name
	limString32(User,UserCharList), % user name
	limString64(AdditionalArguments,AdditionalArgumentsCharList), % additional arguments to be passed to the backend by the postmaster
	limString64("",UnusedCharList),
	limString64(OptionalTty,OptionalTtyCharList), %the optional tty the backend should use for debugging messages
	list_concat([PacketSizeCharList, ProtMajorCharList, ProtMinorCharList, DBCharList, UserCharList, AdditionalArgumentsCharList, UnusedCharList, OptionalTtyCharList], FrontEndPacket).

% postmaster packets
errorResponse_packet(ErrorMessage,['E'|ErrorMessage]). 
authentication_packet([0'R|_AuthenticationType]). 
authenticationOk_packet([0'R|SuccessfulAuth]) :-
	int32(0,SuccessfulAuth).
authenticationUnencryptedPassword_packet([0'R|UnencrPwdRequired]) :-
	int32(3,UnencrPwdRequired).
authenticationEncryptedPassword_packet(Salt,[0'R|EncrPwdRequiredAndSalt]) :-
	int32(4,EncrPwdRequired),
	length(Salt,2), % the salt to use when encrypting the password
	append(EncrPwdRequired,Salt,EncrPwdRequiredAndSalt).
authenticationKerberosV4_packet([0'R| KV4AuthRequired]) :-
	int32(1,KV4AuthRequired).
authenticationKerberosV5_packet([0'R| KV5AuthRequired]) :-
	int32(2,KV5AuthRequired).

% FrontEnd packets
unencryptedPassword_packet(Password,FrontEndPacket) :-
	null_ended_string(Password,NPassword),% tail : password and a NULL char
	length(NPassword,NPasswordSize),
	PacketSize is NPasswordSize + 4, % header size + tail size
	int32(PacketSize,PacketSizeString),% header : size of the packet
	append(PacketSizeString,NPassword,FrontEndPacket).
	
% Backend packets
backendKeyData_packet(ID,SecretKey,[0'K|IDAndSecretKey]) :-
%% The cancellation key will let us issue CancelRequest messages
%% We consider it as the end of the startup phase
	int32(ID,IDStr), % the process ID of this backend
	int32(SecretKey,SKStr), % the secret key of this backend
	append(IDStr,SKStr,IDAndSecretKey),
	error_message("BackendKeyData packet not supported in postgres Frontend/Backend protocol 1.0"),
	fail.
%% New packet in postgres Frontend/Backend protocol 2.0 !!!
%% CancelRequest(F) is also new in 2.0

%% errorResponse_packet is also a backend packet (defined in postmaster packets).

noticeResponse_packet(Notice,[0'N|Notice]).
%% A warning message has been issued.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Query packets         %% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Backend packet
readyForQuery_packet([0'Z]):-
	error_message("ReadyForQuery packet not supported in postgres Frontend/Backend protocol 1.0"),
	fail.
%% New packet in postgres Frontend/Backend protocol 2.0 !!!
%% The backend is ready for a new query cycle. We consider readyForQuery as the start of a query cycle.

% Frontend packet
query_packet(SQLCommand,[0'Q|NullEndedSQLCommand]) :-
	null_ended_string(SQLCommand, NullEndedSQLCommand).
%% SQLCommand is the query itself

% Backend packet
cursorResponse_packet(CursorName,[0'P|NullEndedCursorName]) :-
	null_ended_string(CursorName,NullEndedCursorName).
%% The name of the cursor will be "blank" if the cursor is implicit

% Backend packet
completedResponse_packet(CommandTag,[0'C|NullEndedCommandTag]) :-
	null_ended_string(CommandTag,NullEndedCommandTag).
%% The command tag usually identifies which SQL command was completed

% Backend packet
%% FromRowDescrPacket is the packet to be dealt. Its first part is expected to be a RowDescription packet ([0'T|RowDescriptionPacketContent]). Its second (and last) part is AfterRowDescriptionPacket.
rowDescription_packet(NumberOfFields,FieldsDescriptionList,[0'T|FromRowDescrContentPacket],AfterRowDescriptionPacket,[0'T|RowDescriptionContentPacket]) 
:-
	first_N_chars(NumberOfFieldsStr,FromFieldsDescrPacket,FromRowDescrContentPacket,2),
	int16(NumberOfFields,NumberOfFieldsStr),
	fieldsDescrList2String(FieldsDescriptionList,NumberOfFields,FromFieldsDescrPacket,AfterRowDescriptionPacket),
	append(RowDescriptionContentPacket,AfterRowDescriptionPacket,FromRowDescrContentPacket),
	debug_message("RowDescription packet is ~s",[RowDescriptionContentPacket]).

fieldsDescrList2String([],0,Packet,Packet).
fieldsDescrList2String([[FieldName,FieldTypeID,TypeSize]|RestOfFieldsDescr],NumberOfFields,FromFieldsDescrPacket,AfterRowDescriptionPacket) :-
	first_null_ended_string(FieldNameN,Tail2,FromFieldsDescrPacket),
	debug_message("FieldNameN, Tail2, FromFieldsDescrPacket are ~w, ~w, ~w",[FieldNameN,Tail2, FromFieldsDescrPacket]),
	first_N_chars(FieldTypeIDStr,Tail3,Tail2,4),
	first_N_chars(TypeSizeStr,Tail4,Tail3,2),
	null_ended_string(FieldName,FieldNameN),
	debug_message("FieldName is ~s",[FieldName]),
	int32(FieldTypeID,FieldTypeIDStr),
	int16(TypeSize,TypeSizeStr),
	debug_message("FieldName,FieldTypeID,TypeSize are ~s, ~w, ~w",[FieldName,FieldTypeID,TypeSize]),
	NumberOfFieldsLess1 is NumberOfFields -1,
	debug_message("Making a recursive call with RestOfFieldsDescr,NumberOfFieldsLess1,Tail4,AfterRowDescriptionPacket equal to ~w, ~w, ~w, ~w",[RestOfFieldsDescr,NumberOfFieldsLess1,Tail4,AfterRowDescriptionPacket]),
	fieldsDescrList2String(RestOfFieldsDescr,NumberOfFieldsLess1,Tail4,AfterRowDescriptionPacket).	

%% TO UPDATE : RowDescription is different in 2.0 protocol, because a fourth field is added to metadata info (an int32 specifying the type modifier)
%% Then, fieldsDescrList2String must be modified


% Backend packet
rowsData_packet(NumberOfFields,FieldsDescriptionList,[ValuesList|RestOfRows],FromRowsDataPacket,AfterRowsDataPacket,[RowPacket|RestOfRowsDataPacket]) :-
	asciiRow_packet(NumberOfFields,FieldsDescriptionList,_BitMap,ValuesList,FromRowsDataPacket,AfterRowPacket,RowPacket),
	!,
	rowsData_packet(NumberOfFields,FieldsDescriptionList,RestOfRows,AfterRowPacket,AfterRowsDataPacket,RestOfRowsDataPacket).

rowsData_packet(NumberOfFields,FieldsDescriptionList,[ValuesList|RestOfRows],FromRowsDataPacket,AfterRowsDataPacket,[RowPacket|RestOfRowsDataPacket]) :-
	binaryRow_packet(NumberOfFields,FieldsDescriptionList,_BitMap,ValuesList,FromRowsDataPacket,AfterRowPacket,RowPacket),
	!,
	rowsData_packet(NumberOfFields,FieldsDescriptionList,RestOfRows,AfterRowPacket,AfterRowsDataPacket,RestOfRowsDataPacket).

rowsData_packet(_NumberOfFields,_FieldsDescrList,[],FromRowsDataPacket,FromRowsDataPacket,[]). 
% if there's no more BinaryRows or AsciiRows, there's no more rows to deal with

% Backend packet	
asciiRow_packet(NumberOfFields,FieldsDescrList,BitMap,ValuesList,[0'D|FromRowPacketContent],AfterRowPacket,[0'D|AsciiRowPacketContent]) :-
	sizeInBits2SizeInBytes(NumberOfFields,BitMapSizeInChars),
	first_N_chars(BitMap,FromBitMapPacket,FromRowPacketContent,BitMapSizeInChars),
	debug_message("BitMap,FromBitMapPacket,FromRowPacketContent,BitMapSizeInChars are ~w, ~w, ~w, ~w",[BitMap,FromBitMapPacket,FromRowPacketContent,BitMapSizeInChars]),
	valuesListAscii(NumberOfFields,FieldsDescrList,BitMap,FromBitMapPacket,AfterRowPacket, ValuesList),
	debug_message("ValuesList is ~w",[ValuesList]),
	list_concat([AsciiRowPacketContent,AfterRowPacket],FromRowPacketContent).

valuesListAscii(0,_,_,_,_,[]).
valuesListAscii(NumberOfFields,FieldDescrList,BitMap,FromValuesPacket,AfterValuesPacket,ValuesList) :-
	NumberOfFields>0,
	valuesListAsciiAux(1,NumberOfFields,FieldDescrList,BitMap,FromValuesPacket,AfterValuesPacket,ValuesList).

valuesListAsciiAux(FieldPos,NumberOfFields,[],_BitMap,AfterValuesPacket,AfterValuesPacket,[]) :-
 	FieldPos is NumberOfFields + 1,
	!,
	debug_message("Finishing valuesListAsciiAux recursive call"),
	debug_message(" FieldPos,NumberOfFields,AfterValuesPacket,AfterValuesPacket are: ~w, ~w, ~w, ~w", [FieldPos,NumberOfFields,AfterValuesPacket,AfterValuesPacket]).

% if a value is NULL, then NULL will be added to the values' list
valuesListAsciiAux(FieldPos,NumberOfFields,[[_FieldName,FieldTypeID,TypeSize]|RestOfFieldsDescr],BitMap,FromThisValuePacket,AfterValuesPacket,[[NULL,FieldTypeID,TypeSize,0]|ValuesList]) :-
	FieldPosPlus1 is FieldPos + 1, 
	is_zero_bit(FieldPos,BitMap),
	debug_message("A NULL field was found in bitmap ~w and position ~w",[BitMap,FieldPos]),
	!,
	valuesListAsciiAux(FieldPosPlus1,NumberOfFields,RestOfFieldsDescr,BitMap,FromThisValuePacket,AfterValuesPacket,ValuesList).

valuesListAsciiAux(FieldPos,NumberOfFields,[[_FieldName,FieldTypeID,TypeSize]|RestOfFieldsDescr],BitMap,FromThisValuePacket,AfterValuesPacket,[[ASCIIRawValue,FieldTypeID,TypeSize,SizeToRead]|ValuesList]) :-
	FieldPosPlus1 is FieldPos + 1, 
	is_set_bit(FieldPos,BitMap),
	debug_message("A non-null field was found in bitmap ~w and position ~w",[BitMap,FieldPos]),
	!,
	first_N_chars(SizeStr,AfterSizePacket,FromThisValuePacket,4),
	int32(Size,SizeStr),
	SizeToRead is Size-4,
	first_N_chars(ASCIIRawValue,AfterThisValuePacket,AfterSizePacket,SizeToRead),
	debug_message("ASCIIRawValue,SizeToRead,FieldTypeSize,FieldTypeID are ~w, ~w, ~w, ~w",[ASCIIRawValue,SizeToRead,TypeSize,FieldTypeID]),
	valuesListAsciiAux(FieldPosPlus1,NumberOfFields,RestOfFieldsDescr,BitMap,AfterThisValuePacket,AfterValuesPacket,ValuesList).

% Backend packet	
% BinaryRow and AsciiRow differ only in two questions:
%  - their first character ('B' in BinaryRow, 'D' in AsciiRow)
%   - the way of computing the size of the value of a field. In AsciiRow, the size of the value of the field includes the 4 bytes of the size field; in BinaryRow this 4 bytes are not added.

binaryRow_packet(NumberOfFields,FieldsDescrList,BitMap,ValuesList,[0'D|FromRowPacketContent],AfterRowPacket,[0'B|BinaryRowPacketContent]) :-
	sizeInBits2SizeInBytes(NumberOfFields,BitMapSizeInChars),
	first_N_chars(BitMap,FromBitMapPacket,FromRowPacketContent,BitMapSizeInChars),
	debug_message("BitMap,FromBitMapPacket,FromRowPacketContent,BitMapSizeInChars are ~w, ~w, ~w, ~w",[BitMap,FromBitMapPacket,FromRowPacketContent,BitMapSizeInChars]),
	valuesListBinary(NumberOfFields,FieldsDescrList,BitMap,FromBitMapPacket,AfterRowPacket, ValuesList),
	debug_message("ValuesList is ~w",[ValuesList]),
	list_concat([BinaryRowPacketContent,AfterRowPacket],FromRowPacketContent).

valuesListBinary(0,_,_,_,_,[]).
valuesListBinary(NumberOfFields,FieldDescrList,BitMap,FromValuesPacket,AfterValuesPacket,ValuesList) :-
	NumberOfFields>0,
	valuesListBinaryAux(1,NumberOfFields,FieldDescrList,BitMap,FromValuesPacket,AfterValuesPacket,ValuesList).

valuesListBinaryAux(FieldPos,NumberOfFields,[],_BitMap,AfterValuesPacket,AfterValuesPacket,[]) :-
 	FieldPos is NumberOfFields + 1,
	!,
	debug_message("Finishing valuesListBinaryAux recursive call"),
	debug_message(" FieldPos,NumberOfFields,AfterValuesPacket,AfterValuesPacket are: ~w, ~w, ~w, ~w", [FieldPos,NumberOfFields,AfterValuesPacket,AfterValuesPacket]).

% if a value is NULL, then NULL will be added to the values' list
valuesListBinaryAux(FieldPos,NumberOfFields,[[_FieldName,FieldTypeID,TypeSize]|RestOfFieldsDescr],BitMap,FromThisValuePacket,AfterValuesPacket,[[NULL,FieldTypeID,TypeSize,0]|ValuesList]) :-
	FieldPosPlus1 is FieldPos + 1, 
	is_zero_bit(FieldPos,BitMap),
	debug_message("A NULL field was found in bitmap ~w and position ~w",[BitMap,FieldPos]),
	!,
	valuesListBinaryAux(FieldPosPlus1,NumberOfFields,RestOfFieldsDescr,BitMap,FromThisValuePacket,AfterValuesPacket,ValuesList).

valuesListBinaryAux(FieldPos,NumberOfFields,[[_FieldName,FieldTypeID,TypeSize]|RestOfFieldsDescr],BitMap,FromThisValuePacket,AfterValuesPacket,[[ASCIIRawValue,FieldTypeID,TypeSize,Size]|ValuesList]) :-
	FieldPosPlus1 is FieldPos + 1, 
	is_set_bit(FieldPos,BitMap),
	debug_message("A non-null field was found in bitmap ~w and position ~w",[BitMap,FieldPos]),
	!,
	first_N_chars(SizeStr,AfterSizePacket,FromThisValuePacket,4),
	int32(Size,SizeStr),
% this is the only difference with valuesListAscii: there SizeToRead was Size-4, here SizeToRead is Size
	first_N_chars(ASCIIRawValue,AfterThisValuePacket,AfterSizePacket,Size),
	debug_message("ASCIIRawValue,Size,FieldTypeSize,FieldTypeID are ~w, ~w, ~w, ~w",[ASCIIRawValue,Size,TypeSize,FieldTypeID]),
	valuesListBinaryAux(FieldPosPlus1,NumberOfFields,RestOfFieldsDescr,BitMap,AfterThisValuePacket,AfterValuesPacket,ValuesList).


% Position is the number of bit to test (1st bit is the most significant, last bit is the least one). BytesList is a char list.
is_zero_bit(Position,BytesList) :-
	sizeInBits2SizeInBytes(Position, NumberOfByte),
	NumberOfBit is Position - (NumberOfByte-1)*8,
	nth(NumberOfByte,BytesList,Byte),

	BitsToShift is 8-NumberOfBit,
	ShiftedByte is (Byte>>BitsToShift), %the last bit is the wanted one
	0 is ShiftedByte /\ 1. % to test the last bit, we use 01 as an and-mask.
% Position is the number of bit to test (1st bit is the most significant, last bit is the least one). BytesList is a char list.
is_set_bit(Position,BytesList) :-
	sizeInBits2SizeInBytes(Position, NumberOfByte),
	NumberOfBit is Position - (NumberOfByte-1)*8,
	nth(NumberOfByte,BytesList,Byte),
	BitsToShift is 8-NumberOfBit,
	ShiftedByte is (Byte>>BitsToShift), %the last bit is the wanted one
	255 is ShiftedByte \/ 254. % to test the last bit, we use FE as an or-mask.

sizeInBits2SizeInBytes(NumberOfBits,NumberOfBytes):-
	0 is mod(NumberOfBits,8),
	!,
	NumberOfBytes is (NumberOfBits//8).
sizeInBits2SizeInBytes(NumberOfBits,NumberOfBytes):-
	Mod is mod(NumberOfBits,8),
	Mod>0,
	NumberOfBytes is (NumberOfBits//8)+1.


% TO MAKE: formatData % '' or "" for strings
% TO MAKE: postgres2CIAOData
% every type in postgreSQL has a number which identifies it
fieldTypeID(int2,21).
fieldTypeID(varchar,1043).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function Call packets %% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Termination packets   %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%



%% Predicates dealing with the detailed format of each message
int32(N,Str) :-
	var(N),
	var(Str),
	!,
	error_message("One of the arguments must be non-variable (int32/2)").
int32(N,[B3,B2,B1,B0]) :-
%% [B3,B2,B1,B0] is the four-byte representation of the positive integer N
	integer(N),
	!,
        B0 is N /\ 255,
        B1 is (N >> 8) /\ 255,
        B2 is (N >> 16) /\ 255,
        B3 is (N >> 24) /\ 255.
int32(N,[B3,B2,B1,B0]) :-
%%% [B3,B2,B1,B0] is the four-byte representation of the positive integer N
	var(N),
	!,
	N is (B0 + (B1 << 8) + (B2 << 16) + (B3 << 24)).
int32(N,_) :-
	nonvar(N),
	!,
	error_message("couldn't convert ~w into a four-byte string",[N]),
	fail.
int32(_,Str) :-
	error_message("couldn't convert ~w into an integer. Size is four characters?" ,[Str]).

int16(N,Str) :-
	var(N),
	var(Str),
	!,
	error_message("One of the arguments must be non-variable (int16/2)").
int16(N,[B1,B0]) :-
%% [B1,B0] is the two-byte representation of the positive integer N
	integer(N),
	!,
	B0 is N /\ 255,
	B1 is (N >> 8) /\ 255.
int16(N,[B1,B0]) :-
%% [B1,B0] is the two-byte representation of the positive integer N
	var(N),
	!,
	N is (B0 + (B1 << 8)).
int16(N,_) :-
	nonvar(N),
	!,
	error_message("couldn't convert ~w into a two-byte string",[N]),
	fail.
int16(_,Str) :-
	error_message("couldn't convert ~w into an integer. Size is two characters?" ,[Str]).

limString64(Str,CharList):-
	limStringN(Str,CharList,64).
limString32(Str,CharList):-
	limStringN(Str,CharList,32).


%% null_ended_string(Str,NStr). NStr is the concatenation of Str and a NULL character
null_ended_string([], [0]).
null_ended_string([X|Xs], [X|NXs]):-
        null_ended_string(Xs, NXs).

%% First is the first null-ended string appearing in String. Rest is the rest of the string after the null character.
first_null_ended_string(First,Rest,String) :-
	nth(Pos,String,0),
	length(First,Pos),
	append(First,Rest,String),
	!.
first_null_ended_string(_First,_Rest,String) :-
	error_message("Null character is not included in ~w",[String]),
	fail.

first_N_chars(First,Rest,String,N) :-
	length(First,N),
	append(First,Rest,String),
	!.
first_N_chars(_First,_Rest,String,N) :-
	error_message("couldn't get the first ~w characters of ~s",[N,String]),
	fail.

%% Auxiliar predicates
limStringN(Str,CharList,N):-
%% CharList is a string of size N, the concatenation of Str and the necessary NULL characters 
	append(Str,NullList,CharList),
	length(CharList,N),
	null_list(NullList).
	 
null_list([0]).
null_list([0|Rest]):-
	null_list(Rest).
