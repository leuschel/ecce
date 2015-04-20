// ConnectedSocket.cpp : implementation file
//

#include "stdafx.h"
#include "DbServer.h"
#include "ConnectedSocket.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

#define READ_BUFFER_SIZE 512
/////////////////////////////////////////////////////////////////////////////
// CConnectedSocket

CConnectedSocket::CConnectedSocket(CDbServer* pServer)
{
	m_pServer = pServer;
}

CConnectedSocket::~CConnectedSocket()
{
}

/////////////////////////////////////////////////////////////////////////////
// CConnectedSocket member functions

void CConnectedSocket::OnConnect(int nErrorCode) 
{
	m_pServer->OnConnect(this);	
	CAsyncSocket::OnConnect(nErrorCode);
}

void CConnectedSocket::OnClose(int nErrorCode) 
{
	CAsyncSocket::OnClose(nErrorCode);	
	m_pServer->OnDisconnect(this);
}

void CConnectedSocket::OnReceive(int nErrorCode) 
{
	m_pServer->OnReceive(this);	
	CAsyncSocket::OnReceive(nErrorCode);
}

BOOL CConnectedSocket::Send(LPCTSTR data)
{
	return (CAsyncSocket::Send((const void*)data,strlen(data)+1) != SOCKET_ERROR);
}

BOOL CConnectedSocket::Receive(CString& data)
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
