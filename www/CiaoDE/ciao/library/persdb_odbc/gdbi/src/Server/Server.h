// Server.h : main header file for the SERVER application
//

#if !defined(AFX_SERVER_H__394E278A_F161_11D1_BBFE_0000E8DB52FA__INCLUDED_)
#define AFX_SERVER_H__394E278A_F161_11D1_BBFE_0000E8DB52FA__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000

#ifndef __AFXWIN_H__
	#error include 'stdafx.h' before including this file for PCH
#endif

#include "resource.h"       // main symbols

#include "DbServer.h"       // main symbols

/////////////////////////////////////////////////////////////////////////////
// CServerApp:
// See Server.cpp for the implementation of this class
//

class CServerApp : public CWinApp
{
public:
	CServerApp();

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CServerApp)
	public:
	virtual BOOL InitInstance();
	virtual int ExitInstance();
	//}}AFX_VIRTUAL

// Implementation
	CDbServer m_DbServer;
	CMultiDocTemplate* m_pServerTemplate;

	//{{AFX_MSG(CServerApp)
	afx_msg void OnAppAbout();
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};


/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_SERVER_H__394E278A_F161_11D1_BBFE_0000E8DB52FA__INCLUDED_)
