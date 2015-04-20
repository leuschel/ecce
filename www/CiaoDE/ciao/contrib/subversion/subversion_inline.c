/* Warning: This file has been automatically generated from subversion.pl */

/* Global declaration */

#include <apr_general.h>
#include <svn_client.h>
#include <svn_pools.h>

/* apr_initialize/0 */

void c_apr_initialize(void) {
  apr_initialize();
}

/* apr_terminate/0 */

void c_apr_terminate(void) {
  apr_terminate();
}

/* Global declaration */

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

/* svn_revision/2 */

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
