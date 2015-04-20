#if !defined(AFX_SERVERSOCKET_H__7F5DAD9A_9172_11D1_9F85_00AA0035F2B6__INCLUDED_)
#define AFX_SERVERSOCKET_H__7F5DAD9A_9172_11D1_9F85_00AA0035F2B6__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// ServerSocket.h : header file
//

class CDbServer;
class CConnectedSocket;

/////////////////////////////////////////////////////////////////////////////
// CServerSocket command target

// This class implements the basic functionality of the server socket
// It listens to connections, keeps a table of connected clients, and ofcourse 
// sends and receives data to/from clients

class CServerSocket : public CAsyncSocket
{
// Attributes
public:

// Operations
public:
	CServerSocket(CDbServer* pServer);
	virtual ~CServerSocket();

	// create the socket and start listening
	BOOL Start(UINT port);
	void Stop();

// Overrides
public:
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CServerSocket)
	public:
	virtual void OnAccept(int nErrorCode);
	//}}AFX_VIRTUAL

	// Generated message map functions
	//{{AFX_MSG(CServerSocket)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG

// Implementation
protected:
	CDbServer* m_pServer;
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_SERVERSOCKET_H__7F5DAD9A_9172_11D1_9F85_00AA0035F2B6__INCLUDED_)
