:- module(subversion, _, [foreign_interface]).

:- extra_compiler_opts(['-I/usr/include/subversion-1','-I/usr/include/apr-0']).
:- use_foreign_library(_, ['apr-0', 'svn_client-1']).

:- foreign_inline("
#include <apr_general.h>
#include <svn_client.h>
#include <svn_pools.h>
").

:- initialization(apr_initialize).

:- true pred apr_initialize + (foreign(c_apr_initialize)) -->

"
void c_apr_initialize(void) {
  apr_initialize();
}
".

:- true pred apr_terminate + (foreign(c_apr_terminate)) -->

"
void c_apr_terminate(void) {
  apr_terminate();
}
".

:- foreign_inline("
svn_error_t * c_svn_info(char *target, const svn_wc_entry_t **entry,
    apr_pool_t *pool)
{
  svn_wc_adm_access_t *adm_access;
  apr_pool_t * subpool = (apr_pool_t *)svn_pool_create(pool);

  SVN_ERR (svn_wc_adm_probe_open(&adm_access, NULL, target, FALSE, 0,
    subpool));
  SVN_ERR (svn_wc_entry(entry, target, adm_access, FALSE, pool));
  svn_pool_destroy(subpool);
  return NULL;
}
").

:- true pred svn_revision(in(Target), go(Revision)):: atm * int +
(foreign(c_svn_revision), returns(Revision)) # "Unifies @var{Revision}
with the current revision number of the working copy @var{Target}."
-->

"
int c_svn_revision(char *target) {
  int revision;
  const svn_wc_entry_t *entry;
  apr_pool_t * pool = (apr_pool_t *)svn_pool_create(NULL);
  if(c_svn_info(target, &entry, pool))
  {
    /* throw an error */
    return -1;
  }
  revision = (int)(entry->revision);
  svn_pool_destroy(pool);
  return revision;
}
".
