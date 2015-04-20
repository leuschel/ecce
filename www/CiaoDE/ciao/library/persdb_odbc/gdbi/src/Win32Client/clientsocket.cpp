// clientsocket.cpp : implementation file
//

#include "stdafx.h"
#include "Client.h"
#include "clientsocket.h"
#include <AfxPriv.h>
#include "ClientDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif


#define READ_BUFFER_SIZE 512
/////////////////////////////////////////////////////////////////////////////
// CClientSocket

CClientSocket::CClientSocket(CClientDlg* pDlg)
{
	m_pDlg = pDlg;
}

CClientSocket::~CClientSocket()
{
}

/////////////////////////////////////////////////////////////////////////////
// CClientSocket member functions

BOOL CClientSocket::Start(LPCTSTR address, UINT port)
{
	DWORD blockingmode = 0;
	return (CAsyncSocket::Create(0,SOCK_STREAM,0) &&
			CAsyncSocket::IOCtl(FIONBIO,&blockingmode) &&
			CAsyncSocket::Connect(address,port) &&
			CAsyncSocket::AsyncSelect(FD_READ | FD_CONNECT | FD_CLOSE ));
}

void CClientSocket::Stop()
{
	CAsyncSocket::Close();
}

BOOL CClientSocket::Send(LPCTSTR data)
{
	return (CAsyncSocket::Send((const void*)data,strlen(data)+1) != SOCKET_ERROR);
}

BOOL CClientSocket::Receive(CString& data)
{
	char buff[READ_BUFFER_SIZE+1];
  buff[READ_BUFFER_SIZE] = '\0';
	int result,i;
	BOOL Done = FALSE;
	BOOL FirstBlock = TRUE;

	do
	{
		result = CAsyncSocket::Receive((void*)buff,READ_BUFFER_SIZE,MSG_PEEK);

		if (result != SOCKET_ERROR)
		{
			for (i=0;buff[i]!=NULL && i<result;i++);

			if (i<result)
			{
				// read message including NULL terminator and stop
				result = CAsyncSocket::Receive((void*)buff,i+1);
				Done = TRUE;
			}
			else
			{
				// read socket and continue (no NULL terminator found)
				result = CAsyncSocket::Receive((void*)buff,i);
			}

			data += buff;
		}

	} while (result != SOCKET_ERROR && result > 0 && !Done);

	if (result == SOCKET_ERROR && GetLastError() != WSAEWOULDBLOCK)
		return FALSE;

	return TRUE;
}
	
void CClientSocket::OnClose(int nErrorCode) 
{
	// TODO: Add your specialized code here and/or call the base class
//	m_pClient->OnDisconnect();
	CAsyncSocket::OnClose(nErrorCode);
}

void CClientSocket::OnConnect(int nErrorCode) 
{
	// TODO: Add your specialized code here and/or call the base class
//	m_pClient->OnConnect();
	
	CAsyncSocket::OnConnect(nErrorCode);
}

void CClientSocket::OnReceive(int nErrorCode) 
{
	CString Reply;
	Receive(Reply);
	m_pDlg->DisplayReply(Reply);

	CAsyncSocket::OnReceive(nErrorCode);
}
