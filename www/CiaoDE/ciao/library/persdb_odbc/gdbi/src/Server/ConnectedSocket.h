#if !defined(AFX_CONNECTEDSOCKET_H__7F5DAD9B_9172_11D1_9F85_00AA0035F2B6__INCLUDED_)
#define AFX_CONNECTEDSOCKET_H__7F5DAD9B_9172_11D1_9F85_00AA0035F2B6__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// ConnectedSocket.h : header file
//

class CDbServer;

/////////////////////////////////////////////////////////////////////////////
// CConnectedSocket command target

// this class implements the functionality of the server side of a connected client socket
// 

class CConnectedSocket : public CAsyncSocket
{
// Attributes
public:

// Operations
public:
	CConnectedSocket(CDbServer* pServer);
	virtual ~CConnectedSocket();

	BOOL Send(LPCTSTR data);
	BOOL Receive(CString& data);

// Overrides
public:
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CConnectedSocket)
	public:
	virtual void OnReceive(int nErrorCode);
	virtual void OnClose(int nErrorCode);
	virtual void OnConnect(int nErrorCode);
	//}}AFX_VIRTUAL

// Implementation
protected:
	CDbServer* m_pServer;
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_CONNECTEDSOCKET_H__7F5DAD9B_9172_11D1_9F85_00AA0035F2B6__INCLUDED_)
