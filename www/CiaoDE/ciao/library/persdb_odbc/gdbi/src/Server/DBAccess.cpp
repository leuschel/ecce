#include "stdafx.h"
#include "DBAccess.h"

CDBAccess::CDBAccess(LPCSTR DSN, LPCSTR User, LPCSTR Password)
{
	m_strDataSource = DSN;
	m_strLogin = User;
	m_strPassword = Password;
}

BOOL CDBAccess::Connect(LPCSTR DSN, LPCSTR User, LPCSTR Password)
{
	if (DSN)
		m_strDataSource = DSN;
	if (User)
		m_strLogin = User;
	if (Password)
		m_strPassword = Password;

	BOOL result;
	CString strConnect = (m_strDataSource.IsEmpty() ? "" : "DSN="+m_strDataSource);
	strConnect += (m_strLogin.IsEmpty() ? "" : ";UID="+m_strLogin);
	strConnect += (m_strPassword.IsEmpty() ? "" : ";PWD="+m_strPassword);

	if (m_DB.IsOpen()) m_DB.Close();
	TRY
	{
		CCmdTarget tmp;
		tmp.BeginWaitCursor();
		result=m_DB.OpenEx(strConnect,CDatabase::noOdbcDialog);
		tmp.EndWaitCursor();
	}
	CATCH( CDBException , e )
	{
		result=false;
		m_strLastError=e->m_strError;
		m_strLastNativeError=e->m_strStateNativeOrigin;
	}
	END_CATCH
	return result;
}

void CDBAccess::Disconnect()
{
	if (m_DB.IsOpen()) m_DB.Close();
}

BOOL CDBAccess::TryOpenRecordset(CRecordset & rs,LPCTSTR sql,BOOL bReadOnly)
{
	BOOL result;
	TRY
	{
		CCmdTarget tmp;
		tmp.BeginWaitCursor();
		result=rs.Open(CRecordset::snapshot, sql, (bReadOnly?CRecordset::readOnly:CRecordset::none));
		tmp.EndWaitCursor();
	}
	CATCH (CDBException,e)
	{
		result=false;
		m_strLastError=e->m_strError;
		m_strLastNativeError=e->m_strStateNativeOrigin;
	}
	END_CATCH
	return result;
}


BOOL CDBAccess::TryExecuteSQLStatement(LPCTSTR sql)
{
	BOOL result = true;

	TRY
	{
		CCmdTarget tmp;
		tmp.BeginWaitCursor();
		m_DB.ExecuteSQL(sql);
		tmp.EndWaitCursor();
	}
	CATCH (CDBException,e)
	{
		result = false;
		m_strLastError=e->m_strError;
		m_strLastNativeError=e->m_strStateNativeOrigin;
	}
	END_CATCH

	return result;
}


CStoredProcCall::CStoredProcCall(CDatabase* pdb, LPCSTR ProcName, LPCSTR RetFieldName)
	: CRecordset(pdb)
{
	m_Result = 0;
	m_nFields = (RetFieldName!=NULL);
	m_nDefaultType = forwardOnly;

	m_ProcName = ProcName;

	m_RetFieldName = RetFieldName;
}

void CStoredProcCall::Reset(LPCSTR ProcName, LPCSTR RetFieldName)
{
	Close();

	m_Result = 0;
	m_nFields = (RetFieldName!=NULL);
	m_nDefaultType = forwardOnly;

	m_ProcName = ProcName;
	
	m_RetFieldName = RetFieldName;
}

CString CStoredProcCall::GetDefaultConnect()
{
	return _T("ODBC;DSN=DALET_SYBASE");
}

CString CStoredProcCall::GetDefaultSQL()
{
	return _T("{call "+m_ProcName+"}");
}

void CStoredProcCall::DoFieldExchange(CFieldExchange* pFX)
{
	if (!m_RetFieldName.IsEmpty())
	{
		pFX->SetFieldType(CFieldExchange::outputColumn);
		RFX_Long(pFX, _T("["+m_RetFieldName+"]"),m_Result);
	}
}
