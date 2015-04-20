#ifndef _DBACCESS_H_
#define _DBACCESS_H_

// CDBAccess

class CDBAccess
{
public:
	CDBAccess(LPCSTR DSN = NULL, LPCSTR User = NULL, LPCSTR Password = NULL);
	~CDBAccess() {};

	BOOL Connect(LPCSTR DSN = NULL, LPCSTR User = NULL, LPCSTR Password = NULL);
	void Disconnect();
	BOOL TryOpenRecordset(CRecordset &rs, LPCSTR sql = NULL, BOOL bReadOnly = TRUE);
	BOOL TryExecuteSQLStatement(LPCTSTR sql);

private:
	CDatabase	m_DB;

public:
	CString		m_strDataSource;
	CString		m_strLogin;
	CString		m_strPassword;
	CString		m_strLastError;
	CString		m_strLastNativeError;

public:
	CDatabase* GetDB() { return &m_DB; }
};

// Auxiliary recordset for invoking stored_proc create_account
class CStoredProcCall : public CRecordset
{
public:
	CStoredProcCall(CDatabase* pDatabase, LPCSTR ProcName, LPCSTR RetFieldName = NULL);
	void Reset(LPCSTR ProcName, LPCSTR RetFieldName = NULL);

	long m_Result;
	virtual CString GetDefaultConnect();
	virtual CString GetDefaultSQL();
	virtual void DoFieldExchange(CFieldExchange* pFX);

private:
	CString m_ProcName;
	CString m_RetFieldName;
};


#endif //_DBACCESS_H_