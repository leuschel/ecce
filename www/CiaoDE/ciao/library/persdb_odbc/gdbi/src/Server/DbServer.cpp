// Server.cpp : implementation file
//

#include "stdafx.h"
#include "Server.h"
#include "DbServer.h"
#include "ServerSocket.h"
#include "ServerDoc.h"
#include "ConnectedSocket.h"
#include "ServerToClientErrorMessages.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CDbServer

/**********  21-July-1998 Insertion : **********/
#define _DSN_USER_SEPARATION " , "
#define _FIRST_DBHANDLE_NUMBER 0

CDbServer::CDbServer()
: m_pServerSocket(NULL)
{
	m_Started = false;
	m_CountDbHandles = _FIRST_DBHANDLE_NUMBER;
}

CDbServer::~CDbServer()
{
}


BEGIN_MESSAGE_MAP(CDbServer, CWnd)
	//{{AFX_MSG_MAP(CDbServer)
		// NOTE - the ClassWizard will add and remove mapping macros here.
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()


/////////////////////////////////////////////////////////////////////////////
// CDbServer message handlers

BOOL CDbServer::Start(UINT port)
{
	m_pServerSocket = new CServerSocket(this);

	m_Started = (m_pServerSocket->Start(port) == TRUE);

	return m_Started;
}

void CDbServer::Stop()
{
  if (m_pServerSocket) {
	  m_pServerSocket->Stop();
	  delete m_pServerSocket;
  }
}

void CDbServer::Disconnect(CConnectedSocket* pSocket)
{
	pSocket->Close();
}

BOOL CDbServer::Send(CConnectedSocket* pSocket, LPCTSTR message)
{
	return pSocket->Send(message);
}

BOOL CDbServer::Receive(CConnectedSocket* pSocket)
{
	CString message;
	if (pSocket->Receive(message))
	{
		ParseMessage(pSocket, message);
		return TRUE;
	}
	else
		return FALSE;
}

void CDbServer::OnReceive(CConnectedSocket* pSocket)
{
	Receive(pSocket);
}

void CDbServer::ParseMessage(CConnectedSocket* pSocket, CString Message)
{
	CString Command, Params, Reply;
	ParseCommand(Message, Command, Params);
/**********  21-July-1998 Modification : **********/
// now we have two kinds of logon : 
	// a) only Dsn is given ('Logon')
	// b) Dsn, user and password are given ('LogonUser')
	if (Command.CompareNoCase(_T("Logon")) == 0) {
		DoLogon(Params, Reply, FALSE); // third parameter: BOOL UserGiven
	}
	else if (Command.CompareNoCase(_T("LogonUser")) == 0) {
		DoLogon(Params, Reply, TRUE);
	}
	else if (Command.CompareNoCase(_T("Logoff")) == 0) {
		DoLogoff(Params, Reply);
	}
/**********  21-July-1998 Insertion : **********/
// now you can get directly the answer (DirectSql),or a statement handle (Sql)
	else if (Command.CompareNoCase(_T("Sql")) == 0) 
	{
		DoSql(Params, Reply);
	} else if (Command.CompareNoCase(_T("DirectSql")) == 0) {
		DoDirectSql(Params,Reply);
	} else if (Command.CompareNoCase(_T("Fetch")) == 0) {
		DoFetch(Params, Reply);
	} else if (Command.CompareNoCase(_T("Free")) == 0) {
		DoFree(Params, Reply);
/**********  25-February-1999 Insertion : **********/
	} else if (Command.CompareNoCase(_T("DatabaseTables")) ==0) {
		DoDatabaseTables(Params, Reply);
	} else if (Command.CompareNoCase(_T("TableAttributes")) ==0) {
		DoTableAttributes(Params, Reply);
	} else {
		Reply = _UNNIDENTIFIED_COMMAND + Command;
	}
	Send(pSocket, Reply);
}

// Events
void CDbServer::OnConnect(CConnectedSocket* pSocket)
{
}

void CDbServer::OnDisconnect(CConnectedSocket* pSocket)
{
	delete pSocket;
}

void CDbServer::OnAccept()
{
	CConnectedSocket* pSocket = new CConnectedSocket(this);
	m_pServerSocket->Accept(*pSocket);
}

/////////////////////////////////////////////////////////////////////////////

void CDbServer::GetWord(CString& Input, CString& Word, CString& Rest, TCHAR delimiter)
{
	// Remove white spaces
	Input.TrimLeft(); Input.TrimRight();

	int index = Input.Find(delimiter);
	if (index == -1) {
		Word = Input;
		Rest = _T("");
	} else {
		Word = Input.Left(index);
		Rest = Input.Right(Input.GetLength() - index - 1);
	}
}

void CDbServer::GetWord(CString& Input, CString& Word, TCHAR delimiter)
{
	// Remove white spaces
	Input.TrimLeft(); Input.TrimRight();
	Word = Input;
}

void CDbServer::ParseCommand(CString& Msg, CString& Cmd, CString& Params)
{
	GetWord(Msg, Cmd, Params);
}

/**********  25-February-1999 TO BE IMPROVED : **********/
// To detect excedent arguments in command parsing

void CDbServer::DoLogon(CString& Params, CString& Reply, BOOL UserGiven)
{/**********  21-July-1998 Modification : **********/
	extern CServerApp theApp;
	CServerDoc *pDoc;
	CServerDoc *pPreviousDoc;
	CFrameWnd *pFrame;
	BOOLEAN connected, can_connect;
	CString First, Second, Third, Rest, Dsn, LoginInfo, User, Password, Tail;
	CCommandLineInfo cmdInfo;

	connected=FALSE;
	can_connect=TRUE;

	/*** ARGUMENTS PARSING : ***/
	if (!UserGiven)
	{ // one argument: Dsn
		Dsn=_T(Params);		User=_T("");	Password=_T("");
		can_connect=TRUE;
	}
	else
	{
		GetWord(Params, First, Rest);
		if (First==_T(""))
		{
			Reply=_NO_USER_SPECIFIED;
			can_connect=FALSE;
		}
		else if (Rest==_T(""))
		{
			Reply=_NO_PASSWORD_SPECIFIED;
			can_connect=FALSE;
		}
		else
		{ 
			GetWord(Rest, Second, Third);
			if (Third==_T("")) 
			{	
				Reply=_NO_DSN_SPECIFIED;
				can_connect=FALSE;
			}
			else
			{ // three arguments (user name, password, data source name)
				User=First;		Password=Second;	Dsn=Third;	
				can_connect=TRUE;
			}
		}
	}
	/*** END OF ARGUMENTS PARSING ***/	
	
	if (can_connect)
	{
		pPreviousDoc = FindDsnUserDocument(Dsn,User);
		if (pPreviousDoc != NULL) {	
			connected=TRUE; // connected, but password must be checked
		}
		CMultiDocTemplate* pTemplate = theApp.m_pServerTemplate;
		pDoc=(CServerDoc *)(pTemplate->CreateNewDocument());
		if (pDoc == NULL) {
			Reply=_NO_NEW_DOC;
			connected=FALSE;
		}
		else if (!pDoc->m_DbAccess.Connect(Dsn, User, Password)) 
		{	// not connected to the database
			if (!connected)
			{
				Reply=_FAILED_CONNECTION + Dsn + ", user " + User;
			}
			else // user previously connected, with a different password
			{
				connected=FALSE;
				Reply=_INCORRECT_PASSWORD;
			}
		}
		else if (connected) // and pDoc->m_DbAccess.Connect(Dsn, User, Password)
		{
			pDoc->OnCloseDocument(); // close the new document
			pDoc=pPreviousDoc;		 // use the existing document
		}
		else // user was not connected, pDoc has been created
		{
			connected=TRUE; 
			pFrame=(pTemplate->CreateNewFrame(pDoc,NULL));
			if (pFrame == NULL) {  
				AfxMessageBox(_T("Couldn't create a new frame"));
			}
			else // pFrame created
			{		// display window
				pFrame->ShowWindow(SW_SHOWNORMAL);
				pFrame->UpdateWindow();
				pFrame->SetFocus();
				pFrame->SetWindowText(_T(Dsn + _DSN_USER_SEPARATION + User));
				pDoc->SetTitle(_T(Dsn + _DSN_USER_SEPARATION + User));
				pDoc->AddLine(_T("Connected to database : " + Dsn + 
									_DSN_USER_SEPARATION + User));
				connected=TRUE;
			} 
		}
	}
	if ((pDoc!=NULL) && (connected))// document existed or has been created
	{
		CString strDbHandle = GetNewDbHandle();
		Reply = strDbHandle;
		m_MapDbHandles.SetAt(strDbHandle, pDoc);
	} 
}

void CDbServer::DoLogoff(CString& Params, CString& Reply)
{
	CString strDbHandle;
	GetWord(Params, strDbHandle);
	CServerDoc* pDoc;
/**********  21-July-1998 Modification : **********/
// If there's only one DbHandle 
//	associated to the document, close it
	extern CServerApp theApp;
	CMultiDocTemplate* pTemplate = theApp.m_pServerTemplate;
	if (m_MapDbHandles.Lookup(strDbHandle, (CObject*&)pDoc)) {
		m_MapDbHandles.RemoveKey(strDbHandle);
	if (CountDbHandles(pDoc)==0){
			pDoc->OnCloseDocument();
		}
		Reply = _OK;
	} else {
		Reply = _HANDLE_NOT_FOUND;
	}
}

void CDbServer::DoSql(CString& Params, CString& Reply)
{
	CString strDbHandle, Sql, Statement, rest;
	GetWord(Params, strDbHandle, Sql);
	GetWord(Sql, Statement, rest);

	CServerDoc* pDoc;
	if (m_MapDbHandles.Lookup(strDbHandle, (CObject*&)pDoc)) {
		pDoc->DoSql(Statement, Sql, Reply);
	} else {
		Reply = _HANDLE_NOT_FOUND;
	}
}

void CDbServer::DoFetch(CString& Params, CString& Reply)
{
	CString strStmtHandle, strDbHandle, rest;
	GetWord(Params, strDbHandle, rest);
	GetWord(rest, strStmtHandle, rest);

	CServerDoc* pDoc;
	if (m_MapDbHandles.Lookup(strDbHandle, (CObject*&)pDoc)) {
		if (strStmtHandle.IsEmpty()) {
			Reply = _EMPTY_STMT_HANDLE;
		} else {
			pDoc->DoFetch(strStmtHandle, Reply);
		}
	} else {
		Reply = _HANDLE_NOT_FOUND;
	}
}

/**********  21-July-1998 Insertion : **********/
// old 'DoSql', obtains directly the answer
void CDbServer::DoDirectSql(CString & Params, CString & Reply)
{
	CString strDbHandle, Sql, Statement, rest;
	GetWord(Params, strDbHandle, Sql);
	GetWord(Sql, Statement, rest);

	CServerDoc* pDoc;
	if (m_MapDbHandles.Lookup(strDbHandle, (CObject*&)pDoc)) {
		pDoc->DoDirectSql(Statement, Sql, Reply);
	} else {
		Reply = _HANDLE_NOT_FOUND;
	}
}


void CDbServer::DoFree(CString& Params, CString& Reply)
{
	CString strStmtHandle, strDbHandle, rest;
	GetWord(Params, strDbHandle, rest);
	GetWord(rest, strStmtHandle, rest);

	CServerDoc* pDoc;
	if (m_MapDbHandles.Lookup(strDbHandle, (CObject*&)pDoc)) {
		if (strStmtHandle.IsEmpty()) {
			Reply = _EMPTY_STMT_HANDLE;
		} else {
			pDoc->DoFree(strStmtHandle, Reply);
		}
	} else {
		Reply = _HANDLE_NOT_FOUND;
	}
}

/**********  21-July-1998 Modification : **********/
CServerDoc* CDbServer::FindDsnUserDocument(LPCTSTR Dsn, LPCTSTR User)
{
	// Iterate on all documents and find one which is connected to the specified 
	//  dsn and user
	extern CServerApp theApp;
	CMultiDocTemplate* pTemplate = theApp.m_pServerTemplate;
	POSITION pos = pTemplate->GetFirstDocPosition();
	while (pos != NULL) {
		//  possible exceptions thrown by CString are not dealt
		CServerDoc* pDoc = (CServerDoc*)pTemplate->GetNextDoc(pos);
		CString SourceNameUser;
		SourceNameUser =_T((pDoc->GetDataSourceName())+ _DSN_USER_SEPARATION
										+(pDoc->GetUserName()));
		CString DsnUserArguments=_T(Dsn);
		DsnUserArguments = _T(DsnUserArguments + _DSN_USER_SEPARATION + User);
		if (SourceNameUser.CompareNoCase(DsnUserArguments) == 0)
			return pDoc;
	}
	return NULL;
}


/**********  25-February-1999 Insertion : **********/
void CDbServer::DoDatabaseTables(CString &Params, CString &Reply)
{

	BOOL all_tables=FALSE;
	BOOL parseOK=TRUE;
	CServerDoc* pDoc;
	CString DbHandle;
	CString First, Second, Third, Rest;

		GetWord(Params, First, Rest);
		if (First==_T(""))
		{
			Reply=_NO_DB_HANDLE_SPECIFIED;
			parseOK= FALSE;
		}
		else if (Rest==_T(""))
		{
/**********  25-February-1999 TO BE ADDED : **********/
// Add a second parameter with a maximum number of tables to be given.
//		Now, all the tables are returned.
			all_tables=TRUE; 
			parseOK= TRUE;
		}


		if (parseOK)
		{
			DbHandle=First;
			if (m_MapDbHandles.Lookup(DbHandle, (CObject*&)pDoc)) 
			{
				pDoc->DoDatabaseTables(Reply);
			} 
			else 
			{
				Reply = _HANDLE_NOT_FOUND;
			}
		}
		
}

/**********  25-February-1999 Insertion : **********/
/**********  25-February-1999 TO BE IMPROVED : **********/
// If the table is not in the database, return an error message (now nothing is returned)
void CDbServer::DoTableAttributes(CString &Params, CString &Reply)
{
	CServerDoc *pDoc;
	BOOL parseOK=TRUE;
	CString First, Second, Third, Rest;
	CString DbHandle, TableName;
	BOOL all_attributes=FALSE;

		GetWord(Params, First, Rest);
		if (First==_T(""))
		{
			Reply=_NO_DB_HANDLE_SPECIFIED;
			parseOK= FALSE;
		}
		else if (Rest==_T(""))
		{
			/**********  25-February-1999 TO BE IMPROVED : **********/ 
			// A null second parameter could be interpreted as all the tables' attributes
			Reply=_NO_TABLE_SPECIFIED;
			parseOK= FALSE;
		}
		else
		{
			GetWord(Rest, Second, Third);
			if (Third==_T(""))
			{
				/**********  25-February-1999 TO BE ADDED : **********/
				// Add a third parameter with a maximum number of attributes to be given.
				//		Now, all the attributes are returned.
				all_attributes=TRUE; 
				parseOK= TRUE;
			}
		}

			
		if (parseOK)
		{
			DbHandle=First;
			TableName=Second;
			if (m_MapDbHandles.Lookup(DbHandle, (CObject*&)pDoc)) 
			{
				pDoc->DoTableAttributes(TableName,Reply);
			} 
			else 
			{
				Reply = _HANDLE_NOT_FOUND;
			}
		}
}

CString CDbServer::GetNewDbHandle()
{
	CString s;
	s.Format("DbHandle%d", m_CountDbHandles++);
	return s;
}

int CDbServer::CountDbHandles(CServerDoc * pDoc)
{
	CServerDoc *pResult;
	CString strDbHandle;
	int key, count;

	POSITION pos = m_MapDbHandles.GetStartPosition();	
	count=0;
	for (key=(_FIRST_DBHANDLE_NUMBER);key<m_CountDbHandles;key++)
	{
		strDbHandle.Format("DbHandle%d",key);
		m_MapDbHandles.Lookup(strDbHandle,(CObject *&)pResult);
		if ((pos != NULL) && (pResult==pDoc))
		{
			count++;
		}
	}
	return count;
}

