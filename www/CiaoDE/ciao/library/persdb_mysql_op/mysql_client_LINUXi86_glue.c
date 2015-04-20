#include "/home/asap/asap-online-demo/CiaoDE/ciao/include/LINUXi86/ciao_gluecode.h"

void *mysql_init(long);
BOOL gluecode_init(struct worker *w) {
  ciao_term t0;
  ciao_term t1;
  long c0;
  void *c1;
  ciao_term u1;
  DECL_STATE;
  INIT_STATE;
  ciao_frame_begin_s(state);
  t0 = ciao_ref(state, X(0));
  t1 = ciao_ref(state, X(1));
  if (!ciao_is_integer_s(state, t0)) ERROR_IN_ARG(X(0), 0 + 1, INTEGER);
  c0 = ciao_to_integer_s(state, t0);
  IMPLICIT_STATE;
  GLUECODE_TRY(c1 = mysql_init(c0));
  u1 = ciao_pointer_to_address(state, c1);
  if (!ciao_unify_s(state, u1, t1)) return FALSE;
  ciao_frame_end_s(state);
  return TRUE;
}

void *mysql_real_connect(void *, char *, char *, char *, char *, long, void *, long);
BOOL gluecode_connect(struct worker *w) {
  ciao_term t0;
  ciao_term t1;
  ciao_term t2;
  ciao_term t3;
  ciao_term t4;
  ciao_term t5;
  ciao_term t6;
  ciao_term t7;
  ciao_term t8;
  void *c0;
  char *c1;
  char *c2;
  char *c3;
  char *c4;
  long c5;
  void *c6;
  long c7;
  void *c8;
  ciao_term u8;
  DECL_STATE;
  INIT_STATE;
  ciao_frame_begin_s(state);
  t0 = ciao_ref(state, X(0));
  t1 = ciao_ref(state, X(1));
  t2 = ciao_ref(state, X(2));
  t3 = ciao_ref(state, X(3));
  t4 = ciao_ref(state, X(4));
  t5 = ciao_ref(state, X(5));
  t6 = ciao_ref(state, X(6));
  t7 = ciao_ref(state, X(7));
  t8 = ciao_ref(state, X(8));
  if (!ciao_is_address(state, t0)) USAGE_FAULT("foreign interface: pointer conversion received ill argument (needed $address/1)");
  if (!ciao_is_atom_s(state, t1)) ERROR_IN_ARG(X(1), 1 + 1, STRICT_ATOM);
  if (!ciao_is_atom_s(state, t2)) ERROR_IN_ARG(X(2), 2 + 1, STRICT_ATOM);
  if (!ciao_is_atom_s(state, t3)) ERROR_IN_ARG(X(3), 3 + 1, STRICT_ATOM);
  if (!ciao_is_atom_s(state, t4)) ERROR_IN_ARG(X(4), 4 + 1, STRICT_ATOM);
  if (!ciao_is_integer_s(state, t5)) ERROR_IN_ARG(X(5), 5 + 1, INTEGER);
  if (!ciao_is_address(state, t6)) USAGE_FAULT("foreign interface: pointer conversion received ill argument (needed $address/1)");
  if (!ciao_is_integer_s(state, t7)) ERROR_IN_ARG(X(7), 7 + 1, INTEGER);
  c0 = ciao_address_to_pointer(state, t0);
  c1 = ciao_atom_name_dup_s(state, t1);
  c2 = ciao_atom_name_dup_s(state, t2);
  c3 = ciao_atom_name_dup_s(state, t3);
  c4 = ciao_atom_name_dup_s(state, t4);
  c5 = ciao_to_integer_s(state, t5);
  c6 = ciao_address_to_pointer(state, t6);
  c7 = ciao_to_integer_s(state, t7);
  IMPLICIT_STATE;
  GLUECODE_TRY(c8 = mysql_real_connect(c0, c1, c2, c3, c4, c5, c6, c7));
  u8 = ciao_pointer_to_address(state, c8);
  free(c1);
  free(c2);
  free(c3);
  free(c4);
  if (!ciao_unify_s(state, u8, t8)) return FALSE;
  ciao_frame_end_s(state);
  return TRUE;
}

void mysql_close(void *);
BOOL gluecode_disconnect(struct worker *w) {
  ciao_term t0;
  void *c0;
  DECL_STATE;
  INIT_STATE;
  ciao_frame_begin_s(state);
  t0 = ciao_ref(state, X(0));
  if (!ciao_is_address(state, t0)) USAGE_FAULT("foreign interface: pointer conversion received ill argument (needed $address/1)");
  c0 = ciao_address_to_pointer(state, t0);
  IMPLICIT_STATE;
  GLUECODE_TRY(mysql_close(c0));
  ciao_frame_end_s(state);
  return TRUE;
}

long num_rows(void *);
BOOL gluecode_num_rows(struct worker *w) {
  ciao_term t0;
  ciao_term t1;
  void *c0;
  long c1;
  ciao_term u1;
  DECL_STATE;
  INIT_STATE;
  ciao_frame_begin_s(state);
  t0 = ciao_ref(state, X(0));
  t1 = ciao_ref(state, X(1));
  if (!ciao_is_address(state, t0)) USAGE_FAULT("foreign interface: pointer conversion received ill argument (needed $address/1)");
  c0 = ciao_address_to_pointer(state, t0);
  IMPLICIT_STATE;
  GLUECODE_TRY(c1 = num_rows(c0));
  u1 = ciao_integer_s(state, c1);
  if (!ciao_unify_s(state, u1, t1)) return FALSE;
  ciao_frame_end_s(state);
  return TRUE;
}

long num_fields(void *);
BOOL gluecode_num_fields(struct worker *w) {
  ciao_term t0;
  ciao_term t1;
  void *c0;
  long c1;
  ciao_term u1;
  DECL_STATE;
  INIT_STATE;
  ciao_frame_begin_s(state);
  t0 = ciao_ref(state, X(0));
  t1 = ciao_ref(state, X(1));
  if (!ciao_is_address(state, t0)) USAGE_FAULT("foreign interface: pointer conversion received ill argument (needed $address/1)");
  c0 = ciao_address_to_pointer(state, t0);
  IMPLICIT_STATE;
  GLUECODE_TRY(c1 = num_fields(c0));
  u1 = ciao_integer_s(state, c1);
  if (!ciao_unify_s(state, u1, t1)) return FALSE;
  ciao_frame_end_s(state);
  return TRUE;
}

void mysql_free_result(void *);
BOOL gluecode_free_result(struct worker *w) {
  ciao_term t0;
  void *c0;
  DECL_STATE;
  INIT_STATE;
  ciao_frame_begin_s(state);
  t0 = ciao_ref(state, X(0));
  if (!ciao_is_address(state, t0)) USAGE_FAULT("foreign interface: pointer conversion received ill argument (needed $address/1)");
  c0 = ciao_address_to_pointer(state, t0);
  IMPLICIT_STATE;
  GLUECODE_TRY(mysql_free_result(c0));
  ciao_frame_end_s(state);
  return TRUE;
}

long mysql_query(void *, char *);
BOOL gluecode_query(struct worker *w) {
  ciao_term t0;
  ciao_term t1;
  ciao_term t2;
  void *c0;
  char *c1;
  long c2;
  ciao_term u2;
  DECL_STATE;
  INIT_STATE;
  ciao_frame_begin_s(state);
  t0 = ciao_ref(state, X(0));
  t1 = ciao_ref(state, X(1));
  t2 = ciao_ref(state, X(2));
  if (!ciao_is_address(state, t0)) USAGE_FAULT("foreign interface: pointer conversion received ill argument (needed $address/1)");
  if (!ciao_is_char_code_list(state, t1)) ERROR_IN_ARG(X(1), 1 + 1, CHARACTER_CODE_LIST);
  c0 = ciao_address_to_pointer(state, t0);
  c1 = ciao_list_to_str(state, t1);
  IMPLICIT_STATE;
  GLUECODE_TRY(c2 = mysql_query(c0, c1));
  u2 = ciao_integer_s(state, c2);
  free(c1);
  if (!ciao_unify_s(state, u2, t2)) return FALSE;
  ciao_frame_end_s(state);
  return TRUE;
}

void *mysql_use_result(void *);
BOOL gluecode_use_result(struct worker *w) {
  ciao_term t0;
  ciao_term t1;
  void *c0;
  void *c1;
  ciao_term u1;
  DECL_STATE;
  INIT_STATE;
  ciao_frame_begin_s(state);
  t0 = ciao_ref(state, X(0));
  t1 = ciao_ref(state, X(1));
  if (!ciao_is_address(state, t0)) USAGE_FAULT("foreign interface: pointer conversion received ill argument (needed $address/1)");
  c0 = ciao_address_to_pointer(state, t0);
  IMPLICIT_STATE;
  GLUECODE_TRY(c1 = mysql_use_result(c0));
  u1 = ciao_pointer_to_address(state, c1);
  if (!ciao_unify_s(state, u1, t1)) return FALSE;
  ciao_frame_end_s(state);
  return TRUE;
}

void *mysql_fetch_row(void *);
BOOL gluecode_fetch_row(struct worker *w) {
  ciao_term t0;
  ciao_term t1;
  void *c0;
  void *c1;
  ciao_term u1;
  DECL_STATE;
  INIT_STATE;
  ciao_frame_begin_s(state);
  t0 = ciao_ref(state, X(0));
  t1 = ciao_ref(state, X(1));
  if (!ciao_is_address(state, t0)) USAGE_FAULT("foreign interface: pointer conversion received ill argument (needed $address/1)");
  c0 = ciao_address_to_pointer(state, t0);
  IMPLICIT_STATE;
  GLUECODE_TRY(c1 = mysql_fetch_row(c0));
  u1 = ciao_pointer_to_address(state, c1);
  if (!ciao_unify_s(state, u1, t1)) return FALSE;
  ciao_frame_end_s(state);
  return TRUE;
}

void *mysql_fetch_fields(void *);
BOOL gluecode_fetch_fields(struct worker *w) {
  ciao_term t0;
  ciao_term t1;
  void *c0;
  void *c1;
  ciao_term u1;
  DECL_STATE;
  INIT_STATE;
  ciao_frame_begin_s(state);
  t0 = ciao_ref(state, X(0));
  t1 = ciao_ref(state, X(1));
  if (!ciao_is_address(state, t0)) USAGE_FAULT("foreign interface: pointer conversion received ill argument (needed $address/1)");
  c0 = ciao_address_to_pointer(state, t0);
  IMPLICIT_STATE;
  GLUECODE_TRY(c1 = mysql_fetch_fields(c0));
  u1 = ciao_pointer_to_address(state, c1);
  if (!ciao_unify_s(state, u1, t1)) return FALSE;
  ciao_frame_end_s(state);
  return TRUE;
}

char *mysql_error(void *);
BOOL gluecode_error_string(struct worker *w) {
  ciao_term t0;
  ciao_term t1;
  void *c0;
  char *c1;
  ciao_term u1;
  DECL_STATE;
  INIT_STATE;
  ciao_frame_begin_s(state);
  t0 = ciao_ref(state, X(0));
  t1 = ciao_ref(state, X(1));
  if (!ciao_is_address(state, t0)) USAGE_FAULT("foreign interface: pointer conversion received ill argument (needed $address/1)");
  c0 = ciao_address_to_pointer(state, t0);
  IMPLICIT_STATE;
  GLUECODE_TRY(c1 = mysql_error(c0));
  u1 = ciao_atom_s(state, c1);
  if (!ciao_unify_s(state, u1, t1)) return FALSE;
  ciao_frame_end_s(state);
  return TRUE;
}

char *nth_string(long, void *);
BOOL gluecode_nth_string(struct worker *w) {
  ciao_term t0;
  ciao_term t1;
  ciao_term t2;
  long c0;
  void *c1;
  char *c2;
  ciao_term u2;
  DECL_STATE;
  INIT_STATE;
  ciao_frame_begin_s(state);
  t0 = ciao_ref(state, X(0));
  t1 = ciao_ref(state, X(1));
  t2 = ciao_ref(state, X(2));
  if (!ciao_is_integer_s(state, t0)) ERROR_IN_ARG(X(0), 0 + 1, INTEGER);
  if (!ciao_is_address(state, t1)) USAGE_FAULT("foreign interface: pointer conversion received ill argument (needed $address/1)");
  c0 = ciao_to_integer_s(state, t0);
  c1 = ciao_address_to_pointer(state, t1);
  IMPLICIT_STATE;
  GLUECODE_TRY(c2 = nth_string(c0, c1));
  u2 = ciao_str_to_list(state, c2);
  if (!ciao_unify_s(state, u2, t2)) return FALSE;
  ciao_frame_end_s(state);
  return TRUE;
}

char *nth_field_type(long, void *);
BOOL gluecode_nth_field_type(struct worker *w) {
  ciao_term t0;
  ciao_term t1;
  ciao_term t2;
  long c0;
  void *c1;
  char *c2;
  ciao_term u2;
  DECL_STATE;
  INIT_STATE;
  ciao_frame_begin_s(state);
  t0 = ciao_ref(state, X(0));
  t1 = ciao_ref(state, X(1));
  t2 = ciao_ref(state, X(2));
  if (!ciao_is_integer_s(state, t0)) ERROR_IN_ARG(X(0), 0 + 1, INTEGER);
  if (!ciao_is_address(state, t1)) USAGE_FAULT("foreign interface: pointer conversion received ill argument (needed $address/1)");
  c0 = ciao_to_integer_s(state, t0);
  c1 = ciao_address_to_pointer(state, t1);
  IMPLICIT_STATE;
  GLUECODE_TRY(c2 = nth_field_type(c0, c1));
  u2 = ciao_atom_s(state, c2);
  if (!ciao_unify_s(state, u2, t2)) return FALSE;
  ciao_frame_end_s(state);
  return TRUE;
}

void *mysql_store_result(void *);
BOOL gluecode_store_result(struct worker *w) {
  ciao_term t0;
  ciao_term t1;
  void *c0;
  void *c1;
  ciao_term u1;
  DECL_STATE;
  INIT_STATE;
  ciao_frame_begin_s(state);
  t0 = ciao_ref(state, X(0));
  t1 = ciao_ref(state, X(1));
  if (!ciao_is_address(state, t0)) USAGE_FAULT("foreign interface: pointer conversion received ill argument (needed $address/1)");
  c0 = ciao_address_to_pointer(state, t0);
  IMPLICIT_STATE;
  GLUECODE_TRY(c1 = mysql_store_result(c0));
  u1 = ciao_pointer_to_address(state, c1);
  if (!ciao_unify_s(state, u1, t1)) return FALSE;
  ciao_frame_end_s(state);
  return TRUE;
}

void mysql_client_init(char *module) {
  define_c_mod_predicate(module, "init", 2, gluecode_init);
  define_c_mod_predicate(module, "connect", 9, gluecode_connect);
  define_c_mod_predicate(module, "disconnect", 1, gluecode_disconnect);
  define_c_mod_predicate(module, "num_rows", 2, gluecode_num_rows);
  define_c_mod_predicate(module, "num_fields", 2, gluecode_num_fields);
  define_c_mod_predicate(module, "free_result", 1, gluecode_free_result);
  define_c_mod_predicate(module, "query", 3, gluecode_query);
  define_c_mod_predicate(module, "use_result", 2, gluecode_use_result);
  define_c_mod_predicate(module, "fetch_row", 2, gluecode_fetch_row);
  define_c_mod_predicate(module, "fetch_fields", 2, gluecode_fetch_fields);
  define_c_mod_predicate(module, "error_string", 2, gluecode_error_string);
  define_c_mod_predicate(module, "nth_string", 3, gluecode_nth_string);
  define_c_mod_predicate(module, "nth_field_type", 3, gluecode_nth_field_type);
  define_c_mod_predicate(module, "store_result", 2, gluecode_store_result);
}

void mysql_client_end(char *module) {
  undefine_c_mod_predicate(module, "init", 2);
  undefine_c_mod_predicate(module, "connect", 9);
  undefine_c_mod_predicate(module, "disconnect", 1);
  undefine_c_mod_predicate(module, "num_rows", 2);
  undefine_c_mod_predicate(module, "num_fields", 2);
  undefine_c_mod_predicate(module, "free_result", 1);
  undefine_c_mod_predicate(module, "query", 3);
  undefine_c_mod_predicate(module, "use_result", 2);
  undefine_c_mod_predicate(module, "fetch_row", 2);
  undefine_c_mod_predicate(module, "fetch_fields", 2);
  undefine_c_mod_predicate(module, "error_string", 2);
  undefine_c_mod_predicate(module, "nth_string", 3);
  undefine_c_mod_predicate(module, "nth_field_type", 3);
  undefine_c_mod_predicate(module, "store_result", 2);
}

