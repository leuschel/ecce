/**********  29-July-1998 Insertion : **********/
// Message definitions file.
//  Added to have consistency in server to client messages.

#define _OK (_T("ok"))
#define _UNNIDENTIFIED_COMMAND (_T("Error : unnidentified command "))
#define _NO_USER_SPECIFIED (_T("Error : logon parameter expected (user name)"))
#define _NO_PASSWORD_SPECIFIED (_T("Error : logon parameter expected (password)"))
#define _NO_DSN_SPECIFIED (_T("Error : logon parameter expected (data source name)"))
#define _NO_NEW_DOC (_T("Error in server : couldn't create a new document associated to the database"))
#define _FAILED_CONNECTION (_T("Error : failed to connect to database "))
#define _INCORRECT_PASSWORD (_T("Error : password is not correct"))
#define _HANDLE_NOT_FOUND (_T("Error : cannot find the specified database handle on this server"))
#define _EMPTY_STMT_HANDLE (_T("Error : empty statement handle"))
#define _STMT_HANDLE_NOT_FOUND ("Error : Unknown statement handle")
#define _FAILED_SQL (_T("Error : Failed to execute sql statement : "))
#define _END_OF_FETCH (_T("Fetch : at eof"))

/****************************************************************
	Server replies to client messages
	_________________________________


DBSERVER :
----------
----------

MESSAGE PARSING : _UNNIDENTIFIED_COMMAND
===============
LOGON : _NO_USER_SPECIFIED, _NO_PASSWORD_SPECIFIED, _NO_DSN_SPECIFIED, 
=====	_NO_NEW_DOC, _INCORRECT_PASSWORD, 
		_FAILED_CONNECTION,
			(nice use : _FAILED_CONNECTION + Dsn + ", user " + User)
		"DbHandleN", being N a natural number (successful logon).
LOGOFF : _OK, _HANDLE_NOT_FOUND
======
SQL : _HANDLE_NOT_FOUND
===
FETCH : _HANDLE_NOT_FOUND, _EMPTY_STMT_HANDLE
=====
DIRECTSQL : _HANDLE_NOT_FOUND
=========
FREE : _HANDLE_NOT_FOUND, _EMPTY_STMT_HANDLE
====

SERVERDOC
---------
---------

DOSQL : _OK (successful insertion or deletion)
=====	_FAILED_SQL,
			(nice use : _FAILED_SQL + m_DbAccess.m_strLastError)
		"StmtHandleN", being N a natural number (successful selection)

DOFETCH : _FAILED_SQL, _END_OF_FETCH,
=======	  AnswerField1,...,AnswerFieldN (successful fetch)
				// Add commas between fields
				if (i > 0)	Reply += ",";
				Reply += FieldVal;

DODIRECTSQL : _OK(e.g. successful insertion or deletion), 
===========	  _FAILED_SQL,
			  table([Answer1Field1,...,Answer1FieldN],...[AnswerMField1,...,AnswerMFieldN]).
					Reply = "";
						Reply += "table(["; 
						Reply += "[";
						Reply += FieldVal;
						Reply += ",";
						Reply += "]";
					Reply += "])";

DOFREE : _STMT_HANDLE_NOT_FOUND, _OK
======

****************************************************************/

