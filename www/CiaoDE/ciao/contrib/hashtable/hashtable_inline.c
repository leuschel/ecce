/* Warning: This file has been automatically generated from hashtable.pl */

/* Global declaration */
#include "hashtab.h"


/* ht_count/2 */
long ht_count_(ht_tab *t)
{
  return ht_count(t);
}

/* ht_key/2 */
void * ht_key_(ht_tab *t)
{
  return ht_key(t);
}

/* hkeyl_/2 */
long ht_keyl_(ht_tab *t)
{
  return ht_keyl(t);
}

/* ht_stuff/2 */
void * ht_stuff_(ht_tab *t)
{
  return ht_stuff(t);
}

/* ht_add2/4 */
long ht_add2(ht_tab *t, ub1 *key, void *stuff) {
  return ht_add(t, key, strlen(key), stuff);
}

/* ht_next/2 */
long ht_next_(ht_tab *t)
{
  return ht_next(t);
}
