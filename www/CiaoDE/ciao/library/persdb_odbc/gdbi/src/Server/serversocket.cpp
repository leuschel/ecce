// ServerSocket.cpp : implementation file
//

#include "stdafx.h"
#include "DbServer.h"
#include "ServerSocket.h"
#include "ConnectedSocket.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CServerSocket

CServerSocket::CServerSocket(CDbServer* pServer)
{
	m_pServer = pServer;
}

CServerSocket::~CServerSocket()
{
}


// Do not edit the following lines, which are needed by ClassWizard.
#if 0
BEGIN_MESSAGE_MAP(CServerSocket, CAsyncSocket)
	//{{AFX_MSG_MAP(CServerSocket)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()
#endif	// 0

/////////////////////////////////////////////////////////////////////////////
// CServerSocket member functions

BOOL CServerSocket::Start(UINT port)
{
	DWORD blockingmode = 0;
	return (CAsyncSocket::Create(port) &&
			CAsyncSocket::Listen());
}

void CServerSocket::Stop()
{
	CAsyncSocket::Close();
}

void CServerSocket::OnAccept(int nErrorCode) 
{
	// TODO: Add your specialized code here and/or call the base class
	m_pServer->OnAccept();

	CAsyncSocket::OnAccept(nErrorCode);
}
