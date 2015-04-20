// ClientDlg.h : header file
//

#if !defined(AFX_CLIENTDLG_H__394E27A3_F161_11D1_BBFE_0000E8DB52FA__INCLUDED_)
#define AFX_CLIENTDLG_H__394E27A3_F161_11D1_BBFE_0000E8DB52FA__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

class CClientSocket;
/////////////////////////////////////////////////////////////////////////////
// CClientDlg dialog

class CClientDlg : public CDialog
{
// Construction
public:
	CClientDlg(CWnd* pParent = NULL);	// standard constructor

// Dialog Data
	//{{AFX_DATA(CClientDlg)
	enum { IDD = IDD_CLIENT_DIALOG };
	CButton	m_SendButton;
	CButton	m_DisconnectButton;
	CButton	m_ConnectButton;
	CString	m_Reply;
	CString	m_Message;
	CString	m_Address;
	UINT	m_Port;
	//}}AFX_DATA

	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CClientDlg)
	protected:
	virtual void DoDataExchange(CDataExchange* pDX);	// DDX/DDV support
	//}}AFX_VIRTUAL

// Implementation
protected:
	HICON m_hIcon;

public:
	CClientSocket* m_pClientSocket;
	void DisplayReply(LPCTSTR szText);
	void Enable(CButton& button);
	void Disable(CButton& button);

	// Generated message map functions
	//{{AFX_MSG(CClientDlg)
	virtual BOOL OnInitDialog();
	afx_msg void OnSysCommand(UINT nID, LPARAM lParam);
	afx_msg void OnPaint();
	afx_msg HCURSOR OnQueryDragIcon();
	afx_msg void OnSend();
	afx_msg void OnDestroy();
	afx_msg void OnConnect();
	afx_msg void OnDisconnect();
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_CLIENTDLG_H__394E27A3_F161_11D1_BBFE_0000E8DB52FA__INCLUDED_)
