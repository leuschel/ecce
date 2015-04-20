// ServerView.cpp : implementation of the CServerView class
//

#include "stdafx.h"
#include "Server.h"

#include "ServerDoc.h"
#include "ServerView.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CServerView

IMPLEMENT_DYNCREATE(CServerView, CEditView)

BEGIN_MESSAGE_MAP(CServerView, CEditView)
	//{{AFX_MSG_MAP(CServerView)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CServerView construction/destruction

CServerView::CServerView()
{
	// TODO: add construction code here

}

CServerView::~CServerView()
{
}

BOOL CServerView::PreCreateWindow(CREATESTRUCT& cs)
{
	// TODO: Modify the Window class or styles here by modifying
	//  the CREATESTRUCT cs

	BOOL bPreCreated = CEditView::PreCreateWindow(cs);
	cs.style &= ~(ES_AUTOHSCROLL|WS_HSCROLL);	// Enable word-wrapping
	cs.style |= ES_READONLY;

	return bPreCreated;
}

/////////////////////////////////////////////////////////////////////////////
// CServerView drawing

void CServerView::OnDraw(CDC* pDC)
{
	CServerDoc* pDoc = GetDocument();
	ASSERT_VALID(pDoc);

	// TODO: add draw code for native data here
}

/////////////////////////////////////////////////////////////////////////////
// CServerView diagnostics

#ifdef _DEBUG
void CServerView::AssertValid() const
{
	CEditView::AssertValid();
}

void CServerView::Dump(CDumpContext& dc) const
{
	CEditView::Dump(dc);
}

CServerDoc* CServerView::GetDocument() // non-debug version is inline
{
	ASSERT(m_pDocument->IsKindOf(RUNTIME_CLASS(CServerDoc)));
	return (CServerDoc*)m_pDocument;
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CServerView message handlers

void CServerView::AddLine(LPCTSTR strLine)
{
	CEdit& edit = GetEditCtrl();
	int LastLine=edit.GetLineCount() - 1;

	edit.SetSel(edit.LineIndex(LastLine),-1);
	CString Line = strLine;
	edit.ReplaceSel(Line+"\r\n");
}
