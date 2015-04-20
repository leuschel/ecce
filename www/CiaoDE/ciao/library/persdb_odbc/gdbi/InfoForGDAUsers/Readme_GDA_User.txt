Client - Server Database Interface.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a simple implementation of a client-server interface to a
database, using ODBC. The server part must run under a Windows (NT/95)
platform, and the client can be run on either Windows or Unix systems.
The Server and the Windows Client are compiled with Microsoft visual
C++ version 5.0.  The Unix Client was compiled on a Solaris machine.


Server
~~~~~~

The server can connect to several databases. Opening a new document in
the server requests the user to specify data source name login and
password. If the connection to the database succeeds the open document
will be used by the server to perform sql queries sent to this
specific database.

The server port is 2020.

Client
~~~~~~

The user should connect to a remote server by specifying an address
and a port (typically 2020).  After the connection has been
established, the user can send commands to the server.  If the command
execution fails, the client will receive an error message.

Possible commands are :

* Logon DataSourceName - establishes a connection between the client
and a given database on the server, according to the DataSource name
given. If the connection succeeds, the return value will be a string
(a database handle) to be used by the client to perform operations on
this database.

  Example of use:
	Logon MusicDB
  Examples of answers: 
	DbHandle7 
	Error : failed to connect to database MusicDB, user 

* LogonUser UserName Password DataSourceName - same as Logon but
specifying a user name and his password (if the password is NULL, ""
is needed)

  Example of use:
	Logon Singer singer_password MusicDB
  Examples of answers: 
	DbHandle7 
	Error : failed to connect to database Inexistent, user john

* Logoff DatabaseHandle - using the handle returned by the logon
command.  Disconnects the client from the specified database.

  Example of use:
	Logoff DbHandle1
  Examples of answers: 
	ok
	Error : cannot find the specified database handle in this server

* Sql DatabaseHandle SqlStmt - using the handle returned by the logon
command.  Execute an sql statement on the server and the given
database.

	The sql statement is specified as the rest of the command
arguments, and should not be quoted.

	If the statement is of the form "select ..." the return value
is a string (statement handle) which will be used to get the answer
´rows´ one by one (see Fetch command), or an error message if it
fails.

	If the statement is an "update" or an "insert" statement, the
return value will be "ok" (or an error message if it fails).
	
	Stored procedures or other complex sql statements are not
supported for now.

  Examples of use:
	sql DbHandle0 select * from titles where title_id > 100
	sql DbHandle3 INSERT INTO department(dept_id,dept_name) VALUES
        (230,'Eastern Sales') 

  Examples of answers: 
	StmtHandle3
	ok

	Error : Failed to execute sql statement : : column
'InexistentAttribute' not found

* Fetch DatabaseHandle StatementHandle - using the handle returned by
the logon command and the handle returned by the ´Sql´ command.  Let
us to get the rows of an answer one by one. The reply will be in the
form : [Attribute1Value,...,AttributeNValue]

  Example of use:
	fetch DbHandle3 StmtHandle0
  Examples of answers: 
	402,'Final NS4 test','',38,'1998-02-24 15:36:30:000'
	Fetch : at eof
	Error : Unknown statement handle
	Error : cannot find the specified database handle in this server 

* DirectSql DatabaseHandle SqlStmt - using the handle returned by the
logon command.  Execute an sql statement on the server and the given
database.

	The sql statement is specified as the rest of the command
arguments and should not be quoted.

	If the statement is of the form "select ..." the return value
is a table in the form : table([Row1,Row2,...])  where
Row1,Row2,... are the rows of the table. Each row is a Prolog list and
each member in the list is a field of this row.
	
	If the "select" statement fails, an error message is returned.

	If the statement is an "update" or an "insert" statement, the
return value will be "ok" (or an error message if it fails).
	
	Stored procedures or other complex sql statements are not supported for now.

  Example of use:
	DirectSql DbHandle0 select interpret, count(title) from titles
        group by interpret 
  Examples of answers: 
	ok
	table([['I want you','Bob Dylan',25],['Tom''s Dinner','Suzanne
        Vega',167]]) 
	Error : Failed to execute sql statement : Access violation:
        table 'InexistentTable' not found 
	
* Free DatabaseHandle StatementHandle - using the handle returned by
the logon command and the handle returned by the ´Sql´ command.  Close
a statement handle.

  Example of use:
	free DbHandle0 StmtHandle0
  Examples of answers: 
	ok
	Error : cannot find the specified database handle in this server 
	Error : Unknown statement handle

* DatabaseTables DatabaseHandle - using the handle returned by the
logon command .  Returns the table names of the given database.

  Example of use:
	databasetables DbHandle17
  Examples of answers: 
	['AUTHORS','EDITORIALS','TITLES']
	Error : cannot find the specified database handle in this server 

*TableAttributes DatabaseHandle TableName

  Example of use:
	tableAttributes DbHandle17 titles
  Examples of answers: 
	[['title_id','integer'],['title','varchar'],['publication_date','timestamp']]  

	Error : cannot find the specified database handle in this
server WARNING: at the moment, when tableAttributes DbHandle16
InexistentTable, no error message is returned

The Server commands grammar is summarized in GDAServerCommands.txt .

UnixClient
~~~~~~~~~~

This contains the implementation for Unix platforms. The address of
the remote server and the port should be specified in the config.txt
file.  The user should type the messages into the standard input and
will get the reply in the standard output.


Installation
~~~~~~~~~~~~

Server setup:    see install_GDA_Server.txt
NT Client setup: see install_GDA_NTClient.txt
(Unix GDA client is provided with CIAO distribution).

						CLIP, 25-February-1999 
						  (modified from initial version)
