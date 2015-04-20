// LoginDlg.cpp : implementation file
//

#include "stdafx.h"
#include "Server.h"
#include "LoginDlg.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CLoginDlg dialog


CLoginDlg::CLoginDlg(CWnd* pParent /*=NULL*/)
	: CDialog(CLoginDlg::IDD, pParent)
{
	//{{AFX_DATA_INIT(CLoginDlg)
	m_Dsn = _T("");
	m_Login = _T("");
	m_Password = _T("");
	m_SaveValues = FALSE;
	//}}AFX_DATA_INIT
	m_Dsn = AfxGetApp()->GetProfileString(_T("LoginDlg"),_T("Dsn"),_T(""));
	m_Login = AfxGetApp()->GetProfileString(_T("LoginDlg"),_T("Login"),_T(""));
	m_Password = AfxGetApp()->GetProfileString(_T("LoginDlg"),_T("Password"),_T(""));
	m_SaveValues = AfxGetApp()->GetProfileInt(_T("LoginDlg"),_T("SaveValues"), 0);
}


void CLoginDlg::DoDataExchange(CDataExchange* pDX)
{
	CDialog::DoDataExchange(pDX);
	//{{AFX_DATA_MAP(CLoginDlg)
	DDX_Text(pDX, IDC_DSN, m_Dsn);
	DDX_Text(pDX, IDC_LOGIN, m_Login);
	DDX_Text(pDX, IDC_PASSWORD, m_Password);
	DDX_Check(pDX, IDC_CHECK_SAVEVALUES, m_SaveValues);
	//}}AFX_DATA_MAP
}


BEGIN_MESSAGE_MAP(CLoginDlg, CDialog)
	//{{AFX_MSG_MAP(CLoginDlg)
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CLoginDlg message handlers

BOOL CLoginDlg::OnInitDialog() 
{
	CDialog::OnInitDialog();
			
	return TRUE;  // return TRUE unless you set the focus to a control
	              // EXCEPTION: OCX Property Pages should return FALSE
}

void CLoginDlg::OnOK() 
{
	UpdateData(TRUE);
	AfxGetApp()->WriteProfileInt(_T("LoginDlg"),_T("SaveValues"), m_SaveValues );
	if (m_SaveValues) {
		AfxGetApp()->WriteProfileString(_T("LoginDlg"),_T("Dsn"),m_Dsn);
		AfxGetApp()->WriteProfileString(_T("LoginDlg"),_T("Login"),m_Login);
		AfxGetApp()->WriteProfileString(_T("LoginDlg"),_T("Password"),m_Password);
	}	
	
	CDialog::OnOK();
}
