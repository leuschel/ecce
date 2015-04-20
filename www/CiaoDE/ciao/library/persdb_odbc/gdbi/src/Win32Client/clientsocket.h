#if !defined(AFX_CLIENTSOCKET_H__7F5DADB5_9172_11D1_9F85_00AA0035F2B6__INCLUDED_)
#define AFX_CLIENTSOCKET_H__7F5DADB5_9172_11D1_9F85_00AA0035F2B6__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000
// clientsocket.h : header file
//

class CClientDlg;
/////////////////////////////////////////////////////////////////////////////
// CClientSocket command target

class CClientSocket : public CAsyncSocket
{
// Attributes
public:

// Operations
public:
	CClientSocket(CClientDlg* pDlg);
	virtual ~CClientSocket();

	BOOL Start(LPCTSTR address, UINT port);
	void Stop();
	BOOL Send(LPCTSTR data);
	BOOL Receive(CString& data);
	SOCKET GetSocketHandle() { return m_hSocket; };

	CClientDlg* m_pDlg;

// Overrides
public:
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CClientSocket)
	public:
	virtual void OnClose(int nErrorCode);
	virtual void OnConnect(int nErrorCode);
	virtual void OnReceive(int nErrorCode);
	//}}AFX_VIRTUAL

	// Generated message map functions
	//{{AFX_MSG(CClientSocket)
		// NOTE - the ClassWizard will add and remove member functions here.
	//}}AFX_MSG

};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_CLIENTSOCKET_H__7F5DADB5_9172_11D1_9F85_00AA0035F2B6__INCLUDED_)
