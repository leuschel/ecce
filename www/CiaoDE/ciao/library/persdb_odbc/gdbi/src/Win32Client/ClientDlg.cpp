// ClientDlg.cpp : implementation file
//

#include "stdafx.h"
#include "Client.h"
#include "ClientDlg.h"
#include "ClientSocket.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CClientDlg dialog

CClientDlg::CClientDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CClientDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CClientDlg)
	m_Reply = _T("");
	m_Message = _T("");
	m_Address = _T("");
	m_Port = 0;
	//}}AFX_DATA_INIT
	// Note that LoadIcon does not require a subsequent DestroyIcon in Win32
	m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
	m_pClientSocket = NULL;
}

void CClientDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CClientDlg)
	DDX_Control(pDX, IDC_SEND, m_SendButton);
	DDX_Control(pDX, IDC_DISCONNECT, m_DisconnectButton);
	DDX_Control(pDX, IDC_CONNECT, m_ConnectButton);
	DDX_Text(pDX, IDC_REPLY, m_Reply);
	DDX_Text(pDX, IDC_MESSAGE, m_Message);
	DDX_Text(pDX, IDC_ADDRESS, m_Address);
	DDX_Text(pDX, IDC_PORT, m_Port);
	//}}AFX_DATA_MAP
}

BEGIN_MESSAGE_MAP(CClientDlg, CDialog)
	//{{AFX_MSG_MAP(CClientDlg)
	ON_WM_SYSCOMMAND()
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	ON_BN_CLICKED(IDC_SEND, OnSend)
	ON_WM_DESTROY()
	ON_BN_CLICKED(IDC_CONNECT, OnConnect)
	ON_BN_CLICKED(IDC_DISCONNECT, OnDisconnect)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CClientDlg message handlers

BOOL CClientDlg::OnInitDialog()
{
	CDialog::OnInitDialog();

	// Add "About..." menu item to system menu.

	// IDM_ABOUTBOX must be in the system command range.
	ASSERT((IDM_ABOUTBOX & 0xFFF0) == IDM_ABOUTBOX);
	ASSERT(IDM_ABOUTBOX < 0xF000);

	CMenu* pSysMenu = GetSystemMenu(FALSE);
	if (pSysMenu != NULL)
	{
		CString strAboutMenu;
		strAboutMenu.LoadString(IDS_ABOUTBOX);
		if (!strAboutMenu.IsEmpty())
		{
			pSysMenu->AppendMenu(MF_SEPARATOR);
			pSysMenu->AppendMenu(MF_STRING, IDM_ABOUTBOX, strAboutMenu);
		}
	}

	// Set the icon for this dialog.  The framework does this automatically
	//  when the application's main window is not a dialog
	SetIcon(m_hIcon, TRUE);			// Set big icon
	SetIcon(m_hIcon, FALSE);		// Set small icon
	
	Disable(m_SendButton);
	Disable(m_DisconnectButton);
	Enable(m_ConnectButton);
	
	return TRUE;  // return TRUE  unless you set the focus to a control
}

void CClientDlg::Enable(CButton& button)
{
	button.ModifyStyle(WS_DISABLED,0,
		SWP_NOSIZE|SWP_NOMOVE|SWP_NOZORDER|SWP_NOACTIVATE);
	button.Invalidate();
	button.UpdateWindow();
	//button.SetButtonStyle(button.GetButtonStyle() | BS_DEFPUSHBUTTON);	
}

void CClientDlg::Disable(CButton& button)
{
	button.ModifyStyle(0, WS_DISABLED,
		SWP_NOSIZE|SWP_NOMOVE|SWP_NOZORDER|SWP_NOACTIVATE);
	button.Invalidate();
	button.UpdateWindow();
//	button.SetButtonStyle(button.GetButtonStyle() & ~BS_DEFPUSHBUTTON);
}

void CClientDlg::OnSysCommand(UINT nID, LPARAM lParam)
{
	CDialog::OnSysCommand(nID, lParam);
}

// If you add a minimize button to your dialog, you will need the code below
//  to draw the icon.  For MFC applications using the document/view model,
//  this is automatically done for you by the framework.

void CClientDlg::OnPaint() 
{
	if (IsIconic())
	{
		CPaintDC dc(this); // device context for painting

		SendMessage(WM_ICONERASEBKGND, (WPARAM) dc.GetSafeHdc(), 0);

		// Center icon in client rectangle
		int cxIcon = GetSystemMetrics(SM_CXICON);
		int cyIcon = GetSystemMetrics(SM_CYICON);
		CRect rect;
		GetClientRect(&rect);
		int x = (rect.Width() - cxIcon + 1) / 2;
		int y = (rect.Height() - cyIcon + 1) / 2;

		// Draw the icon
		dc.DrawIcon(x, y, m_hIcon);
	}
	else
	{
		CDialog::OnPaint();
	}
}

// The system calls this to obtain the cursor to display while the user drags
//  the minimized window.
HCURSOR CClientDlg::OnQueryDragIcon()
{
	return (HCURSOR) m_hIcon;
}

void CClientDlg::OnSend() 
{
	UpdateData(TRUE);
	if (m_pClientSocket)	{
		m_pClientSocket->Send(m_Message);
	}
}

void CClientDlg::DisplayReply(LPCTSTR szText) 
{
	m_Reply = szText;
	UpdateData(FALSE);
}

void CClientDlg::OnDestroy() 
{
	CDialog::OnDestroy();
	OnDisconnect();
}

void CClientDlg::OnConnect() 
{
	UpdateData(TRUE);

	m_pClientSocket = new CClientSocket(this);
	if (! m_pClientSocket->Start(m_Address, m_Port)) {
		CString s;
		s.Format("Cannot connect to server : address %s port %d", m_Address, m_Port);
		AfxMessageBox(s);
		delete m_pClientSocket;
		m_pClientSocket = NULL;
	} else {
		Enable(m_SendButton);
		Enable(m_DisconnectButton);
		Disable(m_ConnectButton);
	}
}

void CClientDlg::OnDisconnect() 
{
	if (m_pClientSocket)	{
		m_pClientSocket->Stop();
		delete m_pClientSocket;
		m_pClientSocket = NULL;
		Disable(m_SendButton);
		Disable(m_DisconnectButton);
		Enable(m_ConnectButton);
	}
}
