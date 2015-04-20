// ServerDoc.h : interface of the CServerDoc class
//
/////////////////////////////////////////////////////////////////////////////

#if !defined(AFX_SERVERDOC_H__394E2792_F161_11D1_BBFE_0000E8DB52FA__INCLUDED_)
#define AFX_SERVERDOC_H__394E2792_F161_11D1_BBFE_0000E8DB52FA__INCLUDED_

#if _MSC_VER >= 1000
#pragma once
#endif // _MSC_VER >= 1000


#include "DbAccess.h"
/////////////////////////////////////////////////////////////////////////////
class CStmtInfo
{
public:
	CRecordset m_RecordSet;
	CPtrArray m_FieldsInfo;
	int m_FieldCount;

	CStmtInfo(CDatabase* pDB)
	: m_RecordSet(pDB)
	{
	}

	~CStmtInfo()
	{
		// Cleanup field info array
		for (int i = 0; i < m_FieldsInfo.GetSize(); i++) {
			CODBCFieldInfo* pFieldInfo = (CODBCFieldInfo*)m_FieldsInfo.GetAt(i);
			delete pFieldInfo;
		}
		m_FieldsInfo.RemoveAll();
	}
};

/////////////////////////////////////////////////////////////////////////////


class CServerDoc : public CDocument
{
protected: // create from serialization only
	CServerDoc();
	DECLARE_DYNCREATE(CServerDoc)

// Attributes
public:
	CDBAccess m_DbAccess;
	CMapStringToPtr m_MapCursors;
	int m_CursorCounter;

	CString& GetDataSourceName() { return m_DbAccess.m_strDataSource; }
/**********  21-July-1998 Insertion : **********/
	CString& GetUserName() { return m_DbAccess.m_strLogin; }
// Operations
public:

	void DoSql(CString& Statement, CString& Sql, CString& Reply);
	void DoDirectSql(CString& Statement, CString& Sql, CString& Reply);
	void DoDatabaseTables(CString& Reply);
	void DoTableAttributes(CString& Table, CString& Reply);
	void DoFetch(CString& StmtHandle, CString& Reply);
	void DoFree(CString& StmtHandle, CString& Reply);

	void AddLine(LPCTSTR szText);

	CTime SQLDate2CTime(TIMESTAMP_STRUCT* pDate);
	CString SQLDate2String(TIMESTAMP_STRUCT* pDate);

// Overrides
	// ClassWizard generated virtual function overrides
	//{{AFX_VIRTUAL(CServerDoc)
	public:
	virtual BOOL OnNewDocument();
	virtual void Serialize(CArchive& ar);
	virtual void OnCloseDocument();
	protected:
	virtual BOOL SaveModified();
	//}}AFX_VIRTUAL

// Implementation
public:
	virtual ~CServerDoc();
#ifdef _DEBUG
	virtual void AssertValid() const;
	virtual void Dump(CDumpContext& dc) const;
#endif

protected:

// Generated message map functions
protected:
	//{{AFX_MSG(CServerDoc)
		// NOTE - the ClassWizard will add and remove member functions here.
		//    DO NOT EDIT what you see in these blocks of generated code !
	//}}AFX_MSG
	DECLARE_MESSAGE_MAP()
};

/////////////////////////////////////////////////////////////////////////////

//{{AFX_INSERT_LOCATION}}
// Microsoft Developer Studio will insert additional declarations immediately before the previous line.

#endif // !defined(AFX_SERVERDOC_H__394E2792_F161_11D1_BBFE_0000E8DB52FA__INCLUDED_)
