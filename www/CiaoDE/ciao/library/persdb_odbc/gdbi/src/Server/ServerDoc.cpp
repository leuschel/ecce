// ServerDoc.cpp : implementation of the CServerDoc class
//

#include "stdafx.h"
#include "Server.h"

#include "ServerDoc.h"
#include "LoginDlg.h"
#include "ServerView.h"
#include "ServerToClientErrorMessages.h"
/**********  25-February-1999 Insertion : **********/			
#include "TableSet.h"
#include "Columnst.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#undef THIS_FILE
static char THIS_FILE[] = __FILE__;
#endif

/////////////////////////////////////////////////////////////////////////////
// CServerDoc

#define _DSN_USER_SEPARATION " , "
IMPLEMENT_DYNCREATE(CServerDoc, CDocument)

BEGIN_MESSAGE_MAP(CServerDoc, CDocument)
	//{{AFX_MSG_MAP(CServerDoc)
		// NOTE - the ClassWizard will add and remove mapping macros here.
		//    DO NOT EDIT what you see in these blocks of generated code!
	//}}AFX_MSG_MAP
END_MESSAGE_MAP()

/////////////////////////////////////////////////////////////////////////////
// CServerDoc construction/destruction

CServerDoc::CServerDoc()
{
	m_CursorCounter = 0;
}

CServerDoc::~CServerDoc()
{
	POSITION pos = m_MapCursors.GetStartPosition();
	CString strStmt;
	CStmtInfo* pInfo;
	while (pos != NULL) {
		m_MapCursors.GetNextAssoc(pos, strStmt, (void*&)pInfo);
		delete pInfo;
	}
	m_MapCursors.RemoveAll();
}

BOOL CServerDoc::OnNewDocument()
{
	if (!CDocument::OnNewDocument())
		return FALSE;

	// Create a database connection
	CLoginDlg dlg;
	if (dlg.DoModal() == IDOK) {
		if (m_DbAccess.Connect(dlg.m_Dsn, dlg.m_Login, dlg.m_Password)) {
/**********  21-July-1998 Insertion : **********/
// next line inserted only to set the document title
			SetTitle(_T(dlg.m_Dsn + _DSN_USER_SEPARATION + dlg.m_Login));
/**********  21-July-1998 Modification : **********/
			AddLine(_T("Connected to database : " + dlg.m_Dsn 
							+ _DSN_USER_SEPARATION + dlg.m_Login));
		} else {
			AfxMessageBox(_T("Failed to connect to database"));
			return FALSE;
		}
	} else {
		return FALSE;
	}

	return TRUE;
}

void CServerDoc::OnCloseDocument() 
{
	m_DbAccess.Disconnect();
	CDocument::OnCloseDocument();
}

/////////////////////////////////////////////////////////////////////////////
// CServerDoc serialization

void CServerDoc::Serialize(CArchive& ar)
{
	// CEditView contains an edit control which handles all serialization
	((CEditView*)m_viewList.GetHead())->SerializeRaw(ar);
}

/////////////////////////////////////////////////////////////////////////////
// CServerDoc diagnostics

#ifdef _DEBUG
void CServerDoc::AssertValid() const
{
	CDocument::AssertValid();
}

void CServerDoc::Dump(CDumpContext& dc) const
{
	CDocument::Dump(dc);
}
#endif //_DEBUG

/////////////////////////////////////////////////////////////////////////////
// CServerDoc commands


void CServerDoc::AddLine(LPCTSTR szText)
{
	((CServerView*)m_viewList.GetHead())->AddLine(szText);
}

BOOL CServerDoc::SaveModified() 
{
	return TRUE;
}

/////////////////////////////////////////////////////////////////////////////

void CServerDoc::DoSql(CString& Statement, CString& Sql, CString& Reply)
{
	Reply = "";
	AddLine("Executing sql : " + Sql);

	// Select statement - must create a recordset and save it
	if (Statement.CompareNoCase("SELECT") == 0) {
		// Create new statement info to contain recordset and field info
		CStmtInfo* pStmtInfo = new CStmtInfo(m_DbAccess.GetDB());

 		if (! m_DbAccess.TryOpenRecordset(pStmtInfo->m_RecordSet,Sql,TRUE)) {
			Reply = _FAILED_SQL + m_DbAccess.m_strLastError;
			delete pStmtInfo;
			return;
		} else {
			CString strStmtHandle;
			strStmtHandle.Format("StmtHandle%d", m_CursorCounter++);

			pStmtInfo->m_FieldCount = pStmtInfo->m_RecordSet.GetODBCFieldCount();
			for (int i = 0; i < pStmtInfo->m_FieldCount; i++) {
				CODBCFieldInfo* pFieldInfo = new CODBCFieldInfo;
				pStmtInfo->m_RecordSet.GetODBCFieldInfo(i, *pFieldInfo);
				pStmtInfo->m_FieldsInfo.Add(pFieldInfo);
			}
			m_MapCursors.SetAt(strStmtHandle, pStmtInfo);
			Reply = strStmtHandle;
		}
	} else {
		// Other statement - for now assume that this handles all the rest
		if (m_DbAccess.TryExecuteSQLStatement(Sql)) {
			Reply = _OK;
		} else {
/**********  17-August-1998 Modification : **********/	
// Other statements error messages are also interesting
			Reply = _FAILED_SQL + m_DbAccess.m_strLastError;
		}
	}
}

/**********  25-February-1999 Insertion : **********/			
void CServerDoc::DoDatabaseTables(CString &Reply)
{
	BOOL end=FALSE;
	
	CTables tableRecordset(m_DbAccess.GetDB());

	AddLine(" Looking for tables in the database : ");
	tableRecordset.m_strTypeParam = "'TABLE'"; // VIEW for view access 
	if (!(tableRecordset.Open()))
	{
		Reply = _TABLES_NOT_READ ;
	}
	else
	{
		Reply = "[";
		end=(tableRecordset.IsEOF());
		while (!end)
		{
			Reply += "'" + tableRecordset.m_strName + "'";
			end=(tableRecordset.IsEOF());
			if (!end)
			{
				tableRecordset.MoveNext();
			}
			end=(tableRecordset.IsEOF());
			if (!end)
			{
				Reply+=",";
			}
		}
		tableRecordset.Close();
		Reply += "]";
	}
	AddLine("returned : " + Reply);
}
// other tableRecordset attributes which could be accessed: 
//	m_strOwner (table owner)
//	m_strNameParam, m_strOwnerParam, m_strRemarks (description), 
//	m_strQualifier and m_strQualifierParam (sometimes is the database file 
//	ubication),
//	m_strType and m_strTypeParam (to difference tables, views, ...)



/**********  25-February-1999 Insertion : **********/			
void CServerDoc::DoTableAttributes(CString &TableName, CString &Reply)
{

	
	BOOL table_found=FALSE;
	BOOL end_of_tables=FALSE;
	BOOL end=FALSE;

	CTables tableRecordset(m_DbAccess.GetDB());
	CColumns ColumnsRecordSet(m_DbAccess.GetDB());


	AddLine(" Looking for " + TableName + " attributes in the database : ");
	tableRecordset.m_strTypeParam = "'TABLE'"; // 'VIEW' for view access 
	if (!(tableRecordset.Open()))
	{
		Reply = _TABLES_NOT_READ ;
	}
	else
	{
		end_of_tables=(tableRecordset.IsEOF());
		end=end_of_tables;
		while (!end)
		{
			table_found=(TableName.CompareNoCase(tableRecordset.m_strName)==0);
			if (!table_found)
			{
				tableRecordset.MoveNext();
				end_of_tables=tableRecordset.IsEOF();
				end=end_of_tables;
			}
			else
			{
				end=TRUE; // table_found
				ASSERT(!ColumnsRecordSet.IsOpen());
				ColumnsRecordSet.m_strQualifierParam = tableRecordset.m_strQualifier;
				ColumnsRecordSet.m_strOwnerParam = tableRecordset.m_strOwner;
				ColumnsRecordSet.m_strTableNameParam = tableRecordset.m_strName;
				if (ColumnsRecordSet.m_pDatabase == NULL)
				{
					ColumnsRecordSet.m_pDatabase = tableRecordset.m_pDatabase;
				}
				if (!ColumnsRecordSet.Open())
				{
					Reply=_ATTRIBUTES_NOT_READ;
				}
				else // ColumnsRecordset is open
				{
					Reply="[";
					while (!ColumnsRecordSet.IsEOF())
					{
						Reply += "['" + ColumnsRecordSet.m_strColumnName +
								"','" + ColumnsRecordSet.m_strTypeName + "']";
						ColumnsRecordSet.MoveNext();
						if (!ColumnsRecordSet.IsEOF())
						{
							Reply += ",";
						}
					}
					Reply +="]";
					ColumnsRecordSet.Close();
				}

			}
		}
	}
	AddLine("returned : " + Reply);
}



/**********  25-February-1999 Insertion : **********/
BOOL TryDuplicateApostrophes(CString & Term)
{
	BOOL result;
	int index=0;
	int size;
	
	TRY
	{
		CString Left, Right;
	
		result=TRUE;
		size=Term.GetLength();
		index=0; 
		while (index<size)
		{
			if (Term[index]==_T('\''))
			{
				Left=Term.Left(index+1);
				Right=Term.Right(size-index-1);
				Term=Left + "'" + Right;
				index++;
				size=Term.GetLength();
			}
			index ++;
		}
	}
	CATCH(CMemoryException,e)
	{
		result=FALSE;
	}
	END_CATCH
	return result;
}


void CServerDoc::DoFetch(CString& StmtHandle, CString& Reply)
{
	BOOL ok_dup;
	
	CStmtInfo* pStmtInfo;
	if (! m_MapCursors.Lookup(StmtHandle, (void*&)pStmtInfo)) {
		Reply = _STMT_HANDLE_NOT_FOUND;
		return;
	}

	if (pStmtInfo->m_RecordSet.IsEOF()) {
		Reply = _END_OF_FETCH;
		return;
	}

	CDBVariant var;
	for (int i = 0; i < pStmtInfo->m_FieldCount; i++) {

		CODBCFieldInfo* pFieldInfo = (CODBCFieldInfo*)pStmtInfo->m_FieldsInfo.GetAt(i);
		pStmtInfo->m_RecordSet.GetFieldValue(pFieldInfo->m_strName,var);
		CString FieldVal;

		// Translate field value to string according to the type
		switch (var.m_dwType) {
			case DBVT_NULL :
				FieldVal = "NULL";
				break;
			case DBVT_BOOL :
				FieldVal.Format("%d", var.m_boolVal);
				break;
			case DBVT_UCHAR :
				FieldVal.Format("%c", var.m_chVal);
				break;
			case DBVT_SHORT :
				FieldVal.Format("%d", var.m_iVal);
				break;
			case DBVT_LONG :
				FieldVal.Format("%d", var.m_lVal);
				break;
			case DBVT_SINGLE :
				FieldVal.Format("%f", var.m_fltVal);
				break;
			case DBVT_DOUBLE :
				FieldVal.Format("%f", var.m_dblVal);
				break;
			case DBVT_DATE :
				FieldVal.Format("'%s'", SQLDate2String(var.m_pdate));
				break;
			case DBVT_STRING :
				FieldVal.Format("%s", *var.m_pstring);
/**********  25-February-1999 Insertion : **********/
				ok_dup=TryDuplicateApostrophes(FieldVal);
				FieldVal=_T("'") + FieldVal + _T("'");
				break;
			case DBVT_BINARY :
				FieldVal = _T("Binary");
				break;
		}
		// Add commas between fields
		if (i > 0)	Reply += ",";
		Reply += FieldVal;
	}
	pStmtInfo->m_RecordSet.MoveNext();
/**********  25-February-1999 Insertion : **********/
	if (!ok_dup)
	{
		Reply = _AP_NOT_DUP ;
	}
	AddLine("returned : " + Reply);
}



/**********  21-July-1998 Insertion : **********/
// This is the old DoSql, sometimes is useful (i.e. db_findall 
//	implementation is more efficient bringing the full table)
// 'Reply' will contain the answer to the SQL query
void CServerDoc::DoDirectSql(CString& Statement, CString& Sql, CString& Reply)
{
	BOOL ok_dup;

	Reply = "";
	AddLine("Executing sql : " + Sql);


	if (Statement.CompareNoCase("SELECT") == 0) {
		// Need a recordset for select statements

		CRecordset rs(m_DbAccess.GetDB());
		CDBVariant var;
		int i;

 		if (! m_DbAccess.TryOpenRecordset(rs,Sql,TRUE)) {
			Reply = _FAILED_SQL + m_DbAccess.m_strLastError;
			return;
		}

		// Get information on the fields
		Reply += "table(["; 
		int FieldCount = rs.GetODBCFieldCount();
		CPtrArray FieldsInfo;
		for (i = 0; i < FieldCount; i++) {
/**********  21-July-1998 Modification : **********/
// First 'table' argument is not needed (it contains the attribute names)
			#ifdef _FIRST_TABLE_ARGUMENT_NEEDED
				if (i > 0) Reply += ",";
			#endif
			CODBCFieldInfo* pFieldInfo = new CODBCFieldInfo;
			rs.GetODBCFieldInfo(i, *pFieldInfo);
			FieldsInfo.Add(pFieldInfo);
			#ifdef _FIRST_TABLE_ARGUMENT_NEEDED
				Reply += pFieldInfo->m_strName;
			#endif
		}
		#ifdef _FIRST_TABLE_ARGUMENT_NEEDED
			Reply += "],[";
		#endif

		BOOL bFirst = TRUE;
		while (! rs.IsEOF()) {
			if (bFirst) {
				bFirst = FALSE;
			} else {
				Reply += ",";
			}

			Reply += "[";

			for (i = 0; i < FieldCount; i++) {

				CODBCFieldInfo* pFieldInfo = (CODBCFieldInfo*)FieldsInfo.GetAt(i);
				rs.GetFieldValue(pFieldInfo->m_strName,var);
				CString FieldVal;

				// Translate field value to string according to the type
				switch (var.m_dwType) {
					case DBVT_NULL :
						FieldVal = "NULL";
						break;
					case DBVT_BOOL :
						FieldVal.Format("%d", var.m_boolVal);
						break;
					case DBVT_UCHAR :
						FieldVal.Format("%c", var.m_chVal);
						break;
					case DBVT_SHORT :
						FieldVal.Format("%d", var.m_iVal);
						break;
					case DBVT_LONG :
						FieldVal.Format("%d", var.m_lVal);
						break;
					case DBVT_SINGLE :
						FieldVal.Format("%f", var.m_fltVal);
						break;
					case DBVT_DOUBLE :
						FieldVal.Format("%f", var.m_dblVal);
						break;
					case DBVT_DATE :
						FieldVal.Format("'%s'", SQLDate2String(var.m_pdate));
						break;
					case DBVT_STRING :
						FieldVal.Format("%s", *var.m_pstring);
/**********  25-February-1999 Insertion : **********/
						ok_dup=TryDuplicateApostrophes(FieldVal);
						FieldVal=_T("'") + FieldVal + _T("'");
						break;
					case DBVT_BINARY :
						FieldVal = _T("Binary");
						break;
				}
				// Add commas between fields
				if (i > 0)	Reply += ",";
				Reply += FieldVal;
			}
			Reply += "]";
			rs.MoveNext();
		}
		rs.Close();
		Reply += "])";

		// Cleanup field info array
		for (i = 0; i < FieldsInfo.GetSize(); i++) {
			CODBCFieldInfo* pFieldInfo = (CODBCFieldInfo*)FieldsInfo.GetAt(i);
			delete pFieldInfo;
		}
		FieldsInfo.RemoveAll();
	} else {
		// Other statement - for now assume that this handles all the rest
		if (m_DbAccess.TryExecuteSQLStatement(Sql)) {
			Reply = _OK;
		} else {
/**********  17-August-1998 Modification : **********/	
// Other statements error messages are also interesting
			Reply = _FAILED_SQL + m_DbAccess.m_strLastError;
		}
	}
/**********  25-February-1999 Insertion : **********/
	if (!ok_dup)
	{
		Reply = _AP_NOT_DUP ;
	}
	AddLine("returned : " + Reply);
}

void CServerDoc::DoFree(CString& StmtHandle, CString& Reply)
{
	CStmtInfo* pStmtInfo;
	if (! m_MapCursors.Lookup(StmtHandle, (void*&)pStmtInfo)) {
		Reply = _STMT_HANDLE_NOT_FOUND;
		return;
	}
	pStmtInfo->m_RecordSet.Close();
	m_MapCursors.RemoveKey(StmtHandle);
	delete pStmtInfo;
	Reply = _OK;

}

CTime CServerDoc::SQLDate2CTime(TIMESTAMP_STRUCT* pDate)
{

/**********  25-February-1999 TO BE IMPROVED : **********/ 
// avoid next two lines
	if (pDate->year < 1971)		pDate->year = 1971;
	if (pDate->year > 2037)		pDate->year = 2037;

	if (pDate->month < 1)		pDate->month = 1;
	if (pDate->month > 12)		pDate->month = 12;

	if (pDate->day < 1)			pDate->day = 1;
	if (pDate->day > 31)		pDate->day = 31;

	if (pDate->hour < 0)		pDate->hour = 0;
	if (pDate->hour == 0)		pDate->hour = 24;
	if (pDate->hour > 24)		pDate->hour = 24;

	if (pDate->minute < 0)		pDate->minute = 0;
	if (pDate->minute > 59)		pDate->minute = 59;

	if (pDate->second < 0)		pDate->second = 0;
	if (pDate->second > 59)		pDate->second = 59;

	return CTime(pDate->year, pDate->month, pDate->day, 
		pDate->hour, pDate->minute, pDate->second);
}

#define DB_DATE_FORMAT "%Y-%m-%d %H:%M:%S:000"

CString CServerDoc::SQLDate2String(TIMESTAMP_STRUCT* pDate)
{
	CTime t = SQLDate2CTime(pDate);
	return t.Format(DB_DATE_FORMAT);
}


