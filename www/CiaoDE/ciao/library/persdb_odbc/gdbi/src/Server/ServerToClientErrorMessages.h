/**********  17-August-1998 Insertion : **********/
// Message definitions file.
//  Added to have consistency in server to client messages.
/**********  25-February-1999 Insertion : **********/
// Added new messages :_NO_TABLE_SPECIFIED, _TABLE_NOT_FOUND,
//                     _TABLES_NOT_READ, _ATTRIBUTES_NOT_READ, _AP_NOT_DUP

#define _OK (_T("ok"))
#define _UNNIDENTIFIED_COMMAND (_T("Error : unnidentified command "))
#define _NO_USER_SPECIFIED (_T("Error : logon parameter expected (user name) "))
#define _NO_PASSWORD_SPECIFIED (_T("Error : logon parameter expected (password) "))
#define _NO_DSN_SPECIFIED (_T("Error : parameter expected (data source name) "))
#define _NO_DB_HANDLE_SPECIFIED (_T("Error : parameter expected (database handle) "))
#define _NO_TABLE_SPECIFIED (_T("Error : parameter expected (table name) "))
#define _NO_NEW_DOC (_T("Error in server : couldn't create a new document associated to the database "))
#define _TABLES_NOT_READ (_T("Error : couldn't read database tables "))
#define _ATTRIBUTES_NOT_READ (_T("Error : couldn't read table attributes "))
#define _TABLE_NOT_FOUND (_T("Error : cannot find the specified table in this database handle "))
#define _FAILED_CONNECTION (_T("Error : failed to connect to database "))
#define _INCORRECT_PASSWORD (_T("Error : password is not correct "))
#define _HANDLE_NOT_FOUND (_T("Error : cannot find the specified database handle in this server "))
#define _EMPTY_STMT_HANDLE (_T("Error : empty statement handle "))
#define _STMT_HANDLE_NOT_FOUND ("Error : Unknown statement handle ")
#define _FAILED_SQL (_T("Error : Failed to execute sql statement : "))
#define _END_OF_FETCH (_T("Fetch : at eof"))
#define _AP_NOT_DUP (_T("Error : could not convert response to a correct term"))

/****************************************************************
	Use of the previous messages (sent from server to client)
	____________________________


DBSERVER :
----------
----------

MESSAGE PARSING : _UNNIDENTIFIED_COMMAND
===============
LOGON : _NO_USER_SPECIFIED, _NO_PASSWORD_SPECIFIED, _NO_DSN_SPECIFIED, 
=====	_NO_NEW_DOC, _INCORRECT_PASSWORD, 
		_FAILED_CONNECTION,
			(nice use : _FAILED_CONNECTION + Dsn + ", user " + User)
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
DATABASETABLES : _NO_DB_HANDLE_SPECIFIED, _HANDLE_NOT_FOUND
==============
TABLEATTRIBUTES : _NO_DB_HANDLE_SPECIFIED, _NO_TABLE_SPECIFIED,
===============	  _HANDLE_NOT_FOUND, 


SERVERDOC
---------
---------

DOSQL : _OK (successful insertion or deletion)
=====	_FAILED_SQL,
			(nice use : _FAILED_SQL + m_DbAccess.m_strLastError)

DOFETCH : _FAILED_SQL, _END_OF_FETCH, _AP_NOT_DUP 
=======
DODIRECTSQL : _OK(e.g. successful insertion or deletion), 
===========	  _FAILED_SQL, _AP_NOT_DUP 

DOFREE : _STMT_HANDLE_NOT_FOUND, _OK
======
DATABASETABLES : _TABLES_NOT_READ,
==============	 
TABLEATTRIBUTES : _TABLES_NOT_READ, _ATTRIBUTES_NOT_READ
===============
****************************************************************/

