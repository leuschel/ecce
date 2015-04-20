#if !defined(AFX_SERVER_H__7F5DADB3_9172_11D1_9F85_00AA0035F2B6__INCLUDED_)
#define AFX_SERVER_H__7F5DADB3_9172_11D1_9F85_00AA0035F2B6__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// Server.h : header file
//

class CServerSocket;
class CConnectedSocket;
class CServerDoc;
/////////////////////////////////////////////////////////////////////////////
// CDbServer window

class CDbServer : public CWnd
{
// Construction
public:
	CDbServer();
	virtual ~CDbServer();

	bool IsStarted() { return m_Started; };

// Operations
protected:
	void Disconnect(CConnectedSocket* pSocket);
	BOOL Send(CConnectedSocket* pSocket, LPCTSTR message);
	BOOL Receive(CConnectedSocket* pSocket);
	
	virtual void ParseMessage(CConnectedSocket* pSocket, CString Message);

public :
	BOOL Start(UINT port);
	void Stop();

// Events
public:
	virtual void OnConnect(CConnectedSocket* pSocket);
	virtual void OnDisconnect(CConnectedSocket* pSocket);
	virtual void OnReceive(CConnectedSocket* pSocket);
	virtual void OnAccept();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CDbServer)
	//}}AFX_VIRTUAL

// Implementation
public:
	void GetWord(CString& Input, CString& Word, CString& Rest, TCHAR delimiter = _T(' '));
	void GetWord(CString& Input, CString& Word, TCHAR delimiter = _T(' '));

	void ParseCommand(CString& Msg, CString& Cmd, CString& Params);
/**********  21-July-1998 Modification : **********/
// third parameter indicates if a user name is given
	void DoLogon(CString& Params, CString& Reply, BOOL UserGiven);
/**********  21-July-1998 Modification : **********/
// close the document when logoff
	void DoLogoff(CString& Params, CString& Reply);
	void DoSql(CString& Params, CString& Reply);
	void DoFetch(CString& Params, CString& Reply);
/**********  21-July-1998 Insertion : **********/
// to get directly the answer (old DoSql)
	void DoDirectSql(CString& Params, CString& Reply);
	void DoFree(CString& Params, CString& Reply);

/**********  21-July-1998 Deletion : **********/
//	CServerDoc* FindDsnDocument(LPCTSTR Dsn); 
/**********  21-July-1998 Insertion : **********/
	CServerDoc* FindDsnUserDocument(LPCTSTR Dsn,LPCTSTR User);
/**********  21-July-1998 Insertion : **********/
// to know the number of handles associated to a document
	int CountDbHandles(CServerDoc * pDoc);
/**********  25-February-1999 Insertion : **********/
	void DoDatabaseTables(CString &Params, CString& Reply);
	void DoTableAttributes(CString &Params, CString& Reply);

	// Db handles
	CString GetNewDbHandle();
	int m_CountDbHandles;

	// Generated message map functions
protected:
	//{{AFX_MSG(CDbServer)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()

// Attributes
protected:
	CServerSocket* m_pServerSocket;
	bool m_Started;

	CMapStringToOb m_MapDbHandles;

};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_SERVER_H__7F5DADB3_9172_11D1_9F85_00AA0035F2B6__INCLUDED_)
