/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
/**
   s2 loadable module binding an sqlite3 API...

   Author: Stephan Beal (sgbeal@googlemail.com), 2016-03-05, ported
   over from earlier work in libfossil.

   License: same as s2 (dual Public Domain/MIT)
*/

#include "s2x_sq3.h"
#include <assert.h>

/* Only for debuggering... */
#include <stdio.h>
#define MARKER(pfexp)                                               \
  do{ printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__);   \
    printf pfexp;                                                   \
  } while(0)

#define MY_TYPEID(X) (S2X_SQ3_TYPE_IDS.X)

#define VERBOSE_FINALIZERS 0


/**
   IDs (array indexes) for various keys we cache for this plugin.

   The order of these entries MUST match up with the array defined in
   s2x_sq3_cached_string().
 */
enum Sq3CachedKeys {
/**
   Flag used to mark whether we're in the "final" step of an aggregate
   UDF. It gets declared in the scope of the UDF call(), set to
   either true (for the final call) or false.
*/
KeyUdfFinal = 0,
KeyColumnCount = 1,
KeyColumnNames = 2,
KeyParameterCount = 3
};

/**
   Fetches one of the sqlite plugin's cached strings, creating it if
   needed, and returns it. The strings are cached in s2_stash_set(),
   so they're global and will live as long as se. That's not ideal, in
   terms of lifetime, but we don't otherwise have a convenient place
   to store them because they're needed in places where we don't
   always have access to a db instance.

   Returns NULL if no cache entry can be inserted (out of memory).
*/
static cwal_value * s2x_sq3_cached_string( s2_engine * se,
                                           enum Sq3CachedKeys keyId ){
  static char const * keys[] = {
  /* The order of these strigns MUST match that defined by
     Sq3CachedKeys. They must all begin with the 4 bytes "sq3:". */
  "sq3:$udfFinal",
  "sq3:columnCount",
  "sq3:columnNames",
  "sq3:parameterCount"
  };
  char const * key = 0;
  cwal_value * got = 0;
  switch(keyId){
    case KeyUdfFinal:
    case KeyColumnCount:
    case KeyColumnNames:
    case KeyParameterCount:
      key = keys[(int)keyId];
      break;
    default:
      assert(!"invalid cache key ID");
      return NULL;
  }
  assert(key);
  got = s2_stash_get(se, key);
  if(!got){
    /* Insert it into the stash ... */
    int rc;
    char const * snip = key + 4 /* skip "sq3:" part */;
    got = cwal_new_xstring_value(se->e, snip, cwal_strlen(snip));
    if(!got) return NULL /* OOM */;
    cwal_value_ref(got);
    rc = s2_stash_set(se, key, got);
    cwal_value_unref(got);
    if(rc) return NULL;
    assert(cwal_value_refcount(got)>0);
  }
  return got;
}

cwal_hash * s2x_udf_store( cwal_value * dbSelf, s2x_sq3 * db ){
  cwal_hash * h = 0;
  assert(dbSelf ? 1 : !!db);
  if(dbSelf && !db){
    s2x_sq3_extract_cwal(dbSelf, NULL, NULL, &db);
    assert(db && "Internal misuse");
  }
  if(!db->udfStore){
#define MYSTERIOUS_REFCOUNT_WORKAROUND 1
    cwal_value * hV;
#if MYSTERIOUS_REFCOUNT_WORKAROUND
    /* Ugly workaround for a mysteriously-short refcount on db->udfStore */
    int rc = 0;
#endif
    h = cwal_new_hash(db->se->e, 11);
    if(!h) return 0;
    hV = cwal_hash_value(h);
#if MYSTERIOUS_REFCOUNT_WORKAROUND
    cwal_value_ref(hV);
    rc = cwal_prop_set_v(dbSelf, hV, hV);
    cwal_value_unref(hV);
    if(rc){
      return 0;
    }
#endif
#undef MYSTERIOUS_REFCOUNT_WORKAROUND
    cwal_value_ref(hV);
    db->udfStore = hV;
    if(dbSelf){
      cwal_value_rescope( cwal_value_scope(dbSelf), db->udfStore );
    }
  }else{
    h = cwal_value_get_hash(db->udfStore);
    assert(h && "Wha? Not a hash?");
    assert(cwal_value_refcount(db->udfStore) && "But we ref'd it and rescoped it!");
  }
  return h;
}

static void cwal_finalizer_f_s2x_db( cwal_engine * e, void * m ){
  if(m){
    s2x_sq3 * db = (s2x_sq3 *)m;
    cwal_value * tmp;
    if(e){/*avoid unused param warning*/}
#if VERBOSE_FINALIZERS
    MARKER(("Finalizing s2x_sq3 @%p\n", m));
#endif
    if(db->udfStore){
      tmp = db->udfStore;
      db->udfStore = 0;
      assert(cwal_value_refcount(tmp) && "But we ref'd it!");
      cwal_value_unref(tmp);
    }
    db->vSelf = 0;
    s2x_sq3_close( db );
  }
}

void cwal_finalizer_f_s2x_stmt( cwal_engine * e, void * m ){
  if(m){
    s2x_stmt * st = (s2x_stmt*)m;
    if(e){/*avoid unused param warning*/}
    st->vSelf = 0;
#if VERBOSE_FINALIZERS
    MARKER(("Finalizing s2x_stmt @%p\n", m));
#endif
    s2x_stmt_finalize( st );
  }
}

#undef VERBOSE_FINALIZERS

static cwal_value * s2x_sq3_prototype( s2_engine * se );

/**
  If protoStorage contains a "Stmt" property, it is returned, else
  the Stmt class prototype is installed in protoStorage before
  it is returned. Returns 0 on error (almost certainly an OOM).
*/
static cwal_value * s2x_stmt_prototype( s2_engine * se, cwal_value * protoStorage );

static int s2x_sq3_openmode_to_flags(char const * openMode, cwal_size_t openModeLen, int startFlags){
  int openFlags = startFlags;
  cwal_size_t pos = 0;
  for( ; pos<openModeLen; ++pos ){
    switch(openMode[pos]){
      case 'r':
        openFlags &= ~S2X_SQ3_OPEN_F_RW;
        openFlags |= S2X_SQ3_OPEN_F_RO;
        break;
      case 'w':
        openFlags &= ~S2X_SQ3_OPEN_F_RO;
        openFlags |= S2X_SQ3_OPEN_F_RW;
        break;
      case 'c':
        openFlags &= ~S2X_SQ3_OPEN_F_RO;
        openFlags |= S2X_SQ3_OPEN_F_CREATE;
        break;
      case 'T':
        openFlags |= S2X_SQ3_OPEN_F_TRACE_SQL;
      default:
        break;
    }
  }
  return openFlags;
}


static int cwal_value_rescoper_f_s2x_sq3( cwal_scope * s, cwal_value * v ){
  cwal_native * n = 0;
  s2x_sq3 * db = 0;
  int rc = s2x_sq3_extract_cwal(v, &n, NULL, &db);
  assert(n);
  assert(db);
  assert(0==rc);
  /* MARKER(("Rescoping db@%p\n", (void*)v)); */
  if(db->udfStore){
    rc = cwal_value_rescope(s, db->udfStore);
  }
  return rc;
}

/*
** Creates a new cwal_value (cwal_native) wrapper for the given s2x_sq3
** instance. If addDtor is true then a finalizer is installed for the
** instance, else it is assumed to live native-side and gets no
** destructor installed (in which case we will eventually have a problem
** when such a db is destroyed outside of the script API, unless we
** rewrite these to use weak references instead, but that might require
** one more level of struct indirection).
**
** On success, returns 0 and assigns *rv to the new Db value.
**
** If 0!=db->filename.used then the new value gets a 'name' property
** set to the contents of db->filename.
**
** Maintenance reminder: this routine must return cwal_rc_t codes.
**
** TODO? Wrap up cwal_weakref of db handle, instead of the db handle
** itself? We only need this if DB instances are manipulated from
** outside of script-space (and those manipulations would have to
** be responsible for registering/unregistering the weak ref'd pointer).
*/
static int s2x_sq3_new_native( s2_engine * se, s2x_sq3 * db, char addDtor,
                               cwal_value **rv){
  cwal_native * n;
  cwal_value * nv;
  int rc = 0;
  char const * fname = NULL;
  cwal_size_t nameLen = 0;
  cwal_value * tmpV = NULL;
  assert(se && db && rv);
  n = cwal_new_native(se->e, db,
                      addDtor ? cwal_finalizer_f_s2x_db : NULL,
                      MY_TYPEID(sq3));
  if(!n) return CWAL_RC_OOM;
  nv = cwal_native_value(n);
  cwal_native_set_rescoper( n, cwal_value_rescoper_f_s2x_sq3 );
  cwal_value_ref(nv);
  /* Set up  "filename" property... */
  fname = s2x_sq3_filename(db, &nameLen);
  if(fname){
    tmpV = cwal_new_string_value(se->e, fname, (cwal_size_t)nameLen);
    cwal_value_ref(tmpV);
    rc = tmpV
      ? cwal_prop_set(nv, "filename", 8, tmpV)
      : CWAL_RC_OOM;
    cwal_value_unref(tmpV);
    tmpV = 0;
    fname = 0;
  }

  if(!rc){
    *rv = nv;
    cwal_value_unhand(nv);
  }else{
    /*
      Achtung: if addDtor then on error the dtor will be called
      here.
    */
    cwal_value_unref(nv);
  }
  return rc;
}


#define THIS_DB s2_engine * se = s2_engine_from_args(args);             \
  cwal_native * dbNat = 0; cwal_value * dbV = 0; s2x_sq3 * db = 0; \
  int const _rc = s2x_sq3_extract_cwal(args->self, &dbNat, &dbV, &db); \
  if(_rc){ return cwal_cb_throw(args, _rc,                              \
                              "'this' is not (or is no longer) "      \
                              "a Db instance."); }                    \
  assert(se);                                      \
  assert(db);                                      \
  if(!se){/*potentially unused var*/}

#define THIS_STMT s2_engine * se = s2_engine_from_args(args);           \
  cwal_native * nat = 0; cwal_value * natV = 0; s2x_stmt * stmt = 0;    \
  int const _rc = s2x_stmt_extract_cwal(args->self, &nat, &natV, &stmt); \
  if(_rc){ return cwal_cb_throw(args, _rc,                                \
                              "'this' is not (or is no longer) "        \
                              "a Stmt instance."); }                    \
  assert(se);                                                           \
  assert(stmt);                                                         \
  if(!se){/*potentially unused var*/}

static int cb_s2x_sq3_finalize( cwal_callback_args const * args,
                               cwal_value **rv ){
  THIS_DB;
  cwal_native_clear( dbNat, 1 );
  *rv = cwal_value_undefined();
  return 0;
}

static int cb_s2x_stmt_finalize( cwal_callback_args const * args,
                                 cwal_value **rv ){
  THIS_STMT;
  cwal_native_clear( nat, 1 );
  *rv = cwal_value_undefined();
  return 0;
}

#define TEST_DF 0
/* just testing an unrelated UDF... */
#if TEST_DF
#include <stdlib.h>
#include <string.h>
#include <time.h>
/**
   A simple example of writing a custom SQL function for sqlite3.

   It provides results of a so-called Fudge-dice roll. Fudge dice (dF)
   are used by the Fudge roleplaying system (http://fudgerpg.com).

   Call it with 0, 1 or 2 arguments:

   0 args: same as calling dF(4).

   1 arg: arg1 is an integer telling how many dice to roll.

   2 args: arg2 is any value. The TYPE of the value is used to
   dynamically set the return type. If arg2 is an INTEGER (the
   default) or DOUBLE then a number of the appropriate type is
   returned to the caller via sqlite_result_xxx(context,...). If arg2
   is a string then a description showing the results of each die roll
   of the set, plus the total, is sent back.

   You may bind this function to your db by calling something like:

   sqlite3_create_function( myDb, "dF", -1, SQLITE_ANY, 0, sqlite_func_dF, 0, 0 );

   Author: stephan at wanderinghorse dot net

   License: Public Domain
*/
void sqlite_func_dF(
  sqlite3_context *context,
  int argc,
  sqlite3_value **argv )
{
    enum {BufLen = 128};
    char buf[BufLen+1] = {0};
    char * bPos = buf;
    int count = 0;
    int returnT;
    int reti = 0;
    static char seeded = 0;
    if( !seeded && (seeded=1) ){
      srand( time(NULL) );
    }

    if( argc > 2 ){
        sqlite3_result_error( context, "dF() function requires 0, 1, or 2 arguments: (0==roll 4dF and return int), (1==roll [ARG0]dF and return int), (2=roll [ARG0]dF and return result as TYPEOF(ARG1))", -1 );
        return;
    }

    returnT = (argc<2) ? SQLITE_INTEGER : sqlite3_value_type(argv[1]);
    if( 0 == argc ){
        count = 4;
    }
    else{
        count = sqlite3_value_int(argv[0]);
        if( count < 1 ) count = 4;
    }
    if(count > BufLen/2+20){
        sqlite3_result_error( context, "dF() can't buffer that many dice.", -1 );
        return;
    }
    
    if( SQLITE_TEXT == returnT ){
        /* bPos += sprintf(bPos, "dF: "); */
    }
    {
        int rnd;
        char marker;
        while( count-- ){
            rnd = rand() % 3 - 1;
            if( SQLITE_TEXT == returnT ) {
                marker = ((0==rnd) ? '0' : ((rnd>0) ? '+' : '-'));
                *bPos++ = marker;
                *bPos++ = ' ';
            }
            reti += rnd;
        }
    }
    if( SQLITE_TEXT == returnT ){
        bPos += sprintf(bPos, "= %d", reti);
        *bPos = 0;
        sqlite3_result_text( context, buf, strlen(buf), SQLITE_TRANSIENT );
    }
    else if( SQLITE_FLOAT == returnT ){
        sqlite3_result_double(context, 1.0 * reti);
    }
    else{
        sqlite3_result_int(context, reti);
    }
    return;
}
#endif

int cb_s2x_sq3_ctor( cwal_callback_args const * args,
                    cwal_value **rv ){
  cwal_value * v = NULL;
  int rc;
  s2_engine * se = s2_engine_from_args(args);
  char const * fn;
  char const * openMode;
  cwal_size_t openModeLen = 0;
  cwal_size_t fnLen = 0;
  int openFlags = 0;
  s2x_sq3 * dbP = 0;
  cwal_value * proto =
    (cwal_value *)cwal_args_state(args, MY_TYPEID(sq3_prototype));
  if(!proto){
    return cwal_cb_throw(args, CWAL_RC_UNSUPPORTED,
                       "sqlite3 prototype is missing. Make sure this "
                       "module is not both statically and "
                       "dynamically linked!");
  }
  assert(se);
  fn = args->argc
    ? cwal_value_get_cstr(args->argv[0], &fnLen)
    : NULL;
  if(!fn){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                   "Expecting a string (db file name) argument.");
  }
  openMode = (args->argc>1)
    ? cwal_value_get_cstr(args->argv[1], &openModeLen)
    : NULL;
  if(!openMode){
    openFlags = S2X_SQ3_OPEN_F_RWC;
  }else{
    openFlags = s2x_sq3_openmode_to_flags( openMode, openModeLen,
                                           openFlags );
  }
  dbP = s2x_sq3_malloc(se);
  if(!dbP) return CWAL_RC_OOM;
  rc = s2x_sq3_open( se, dbP, fn, openFlags );
  if(rc){
    if(dbP->error.msg.used){
      rc = cwal_cb_throw(args, CWAL_RC_ERROR,
                   "Db open failed: code #%d: %s",
                   dbP->error.code,
                   (char const *)dbP->error.msg.mem);
    }else{
      rc = cwal_cb_throw(args, CWAL_RC_ERROR,
                      "Db open failed "
                      "with code #%d (%s).", rc,
                      cwal_rc_cstr(rc));
    }
    s2x_sq3_close(dbP);
  }
  else{
    s2x_sq3_busy_timeout(dbP, 5000)
      /* Workaround: we have client-side code whichs sets this, but
         concurrent CGI access can still trigger a busy because (it
         seems) the timing is such that starting our post-OPEN
         transaction to run db init code fails with BUSY. Hopefully
         setting a default busy timeout will alleviate that. Client
         code may still override it.
      */;
    rc = s2x_sq3_new_native(se, dbP, 1, &v);
    if(rc){
      s2x_sq3_close(dbP);
      assert(!v);
    }else{
      cwal_value_prototype_set( v, proto );
    }
  }
  if(rc){
    assert(!v);
  }else{
#if TEST_DF
    sqlite3_create_function(dbP->dbh, "dF", -1, SQLITE_UTF8,
                            0, sqlite_func_dF, 0, 0);
#endif
    assert(v);
    *rv  = v;
  }
  return rc;
}

#undef TEST_DF

/**
   Returns a new cwal_array value containing the result column names
   of the given statement. Returns NULL on error (allocation failed),
   else an array value.
*/
static cwal_value * s2x_stmt_col_names( cwal_engine * e,
                                           s2x_stmt * st ){
  cwal_value * aryV = NULL;
  cwal_array * ary = NULL;
  char const * colName = NULL;
  int i = 0;
  int rc = 0;
  cwal_value * newVal = NULL;
  assert(st);
  if( ! st->colCount ) return NULL;
  ary = cwal_new_array(e);
  if( ! ary ) return NULL;
  aryV = cwal_array_value(ary);
  cwal_value_ref(aryV);
  rc = cwal_array_reserve(ary, (cwal_size_t)st->colCount);
  if(rc) goto end;
  for( i = 0; (0==rc) && (i < st->colCount); ++i ){
    colName = s2x_stmt_col_name(st, i);
    if( ! colName ) rc = CWAL_RC_OOM;
    else{
      newVal = cwal_new_string_value(e, colName,
                                     cwal_strlen(colName));
      if( NULL == newVal ){
        rc = CWAL_RC_OOM;
      }
      else{
        cwal_value_ref(newVal);
        rc = cwal_array_set( ary, i, newVal );
        cwal_value_unref( newVal );
      }
    }
  }
  end:
  if( 0 == rc ){
    cwal_value_unhand(aryV);
  }
  else{
    cwal_value_unref(aryV);
    aryV = NULL;
  }
  return aryV;
}

/**
   Finishes the setup of a newly-created statement. protoStorage must
   be the prototype object for the database class (that's where the
   Stmt prototype gets stored). nv must be the cwal_native part of the
   new statement, which must be newly-created and have a refcount of 1
   or 0. st must be the new statement itself, which must be bound to
   nv. Returns 0 on success. On error the statement is in an undefined
   state and must be finalized by unref'ing nv. The only error case,
   provided all arguments are correct, is CWAL_RC_OOM.
*/
static int s2x_sq3_setup_new_stmt(s2_engine * se,
                                  cwal_value * protoStorage,
                                  cwal_value * nv,
                                  s2x_stmt * st){
  int rc;
  cwal_value * v;
  cwal_value * key;
  cwal_engine * e = se->e;
  assert(se);
  assert(protoStorage);
  assert(nv);
  assert(st);
  /* TODO: cache the keys used here somewhere (protoStorage?), so that
     we're not continually re-creating the same strings. */
#define VCHECK if(!v){ rc = CWAL_RC_OOM; goto end; } cwal_value_ref(v)
#define SET(KID) VCHECK;                                \
  key = s2x_sq3_cached_string(se, KID);                 \
  rc = key ? cwal_prop_set_v(nv, key, v) : CWAL_RC_OOM; \
  cwal_value_unref(v); v = 0; if(rc) goto end

  v = cwal_new_integer(e, (cwal_int_t)st->colCount);
  SET(KeyColumnCount);
  v = cwal_new_integer(e, (cwal_int_t)st->paramCount);
  SET(KeyParameterCount);
  if(st->colCount){
    v = s2x_stmt_col_names(e, st);
    SET(KeyColumnNames);
  }
  v = s2x_stmt_prototype(se, protoStorage);
  VCHECK;
  cwal_value_prototype_set(nv, v);
  cwal_value_unref(v);
  st->vSelf = nv;
#undef SET
#undef VCHECK
  assert(!rc);
  end:
  return rc;
}

static int cb_s2x_sq3_changes( cwal_callback_args const * args,
                               cwal_value **rv ){
  cwal_value * c;
  THIS_DB;
  c = cwal_new_integer(args->engine,
                       (cwal_int_t)sqlite3_changes(db->dbh));
  if(!c) return CWAL_RC_OOM;
  *rv = c;
  return 0;
}

static int cb_s2x_sq3_total_changes( cwal_callback_args const * args,
                                     cwal_value **rv ){
  cwal_value * c;
  THIS_DB;
  c = cwal_new_integer(args->engine,
                       (cwal_int_t)sqlite3_total_changes(db->dbh));
  if(!c) return CWAL_RC_OOM;
  *rv = c;
  return 0;
}

static int cb_s2x_sq3_prepare( cwal_callback_args const * args,
                              cwal_value **rv ){
  s2x_stmt st = s2x_stmt_empty;
  int rc;
  char const * sql;
  cwal_size_t sqlLen = 0;
  cwal_value * nv = NULL;
  cwal_engine * e = args->engine;
  s2x_stmt * st2 = NULL;
  THIS_DB;
  sql = args->argc
    ? cwal_value_get_cstr(args->argv[0], &sqlLen)
    : NULL;
  if(!sql || !sqlLen){
    return cwal_exception_setf(e,
                               CWAL_RC_MISUSE,
                               "Expecting a non-empty string "
                               "argument (SQL).");
  }
  assert(!st.stmt);
  rc = s2x_sq3_prepare( db, &st, "%.*s", (int)sqlLen, sql);
  if(rc){
    rc = s2x_sq3_toss(db);
    assert(!st.stmt);
  }else{
    st2 = s2x_stmt_malloc(e);
    nv = st2
      ? cwal_new_native_value(e,
                              st2, cwal_finalizer_f_s2x_stmt,
                              MY_TYPEID(stmt))
      : NULL;
    if(!nv){
      if(st2) s2x_stmt_finalize(st2);
      s2x_stmt_finalize(&st);
      st2 = NULL;
      rc = CWAL_RC_OOM;
    }else{
      void const * kludge = st2->allocStamp;
      cwal_value_ref(nv);
      *st2 = st;
      st2->allocStamp = kludge;
      rc = s2x_sq3_setup_new_stmt(se,
                                  cwal_value_prototype_get(se->e,
                                                           dbV),
                                  nv, st2);
      if(!rc) *rv = nv;
    }
  }
  if(rc && nv){
    cwal_value_unref(nv);
  }        
  return rc;
}

static int cb_s2x_sq3_filename( cwal_callback_args const * args,
                               cwal_value **rv ){
  char const * fname = NULL;
  cwal_size_t nameLen = 0;
  THIS_DB;
  fname = s2x_sq3_filename(db, &nameLen);
  *rv = fname
    ? cwal_new_string_value(args->engine, fname, nameLen)
    : cwal_value_null();
  return *rv ? 0 : CWAL_RC_OOM;
}

#if 0
static int cb_s2x_sq3_name( cwal_callback_args const * args,
                               cwal_value **rv ){
  char const * fname = NULL;
  THIS_DB;
  fname = s2x_sq3_name(db);
  *rv = fname
    ? cwal_new_string_value(args->engine, fname, cwal_strlen(fname))
    : cwal_value_null();
  return *rv ? 0 : CWAL_RC_OOM;
}
#endif

/**
   Proxy for the various step() implementations.

   mode: 0==Tuple, 1==Array, >1==Object, -1==no row data (just boolean
   indicator), -2==return stmt object itself.
*/
static int s2x_stmt_step_impl( cwal_callback_args const * args,
                               cwal_value **rv,
                               int mode ){
  int rc;
  int scode;
  THIS_STMT;
  scode = s2x_stmt_step( stmt );
  switch(scode){
    case S2X_RC_STEP_DONE:
      rc = 0;
      *rv = (-2==mode)
        ? natV
        : cwal_value_undefined();
      break;
    case S2X_RC_STEP_ROW:{
      if(0==mode){
        /* Tuple mode */
        *rv = s2x_stmt_row_to_tuple(stmt);
      }else if(1==mode){
        /* Array mode */
        *rv = s2x_stmt_row_to_array(stmt);
      }else if(mode>0){
          /* Object mode */
          cwal_array const * colNames =
            cwal_value_get_array( cwal_prop_get(args->self,
                                                 "columnNames", 11) );
          *rv = colNames
            ? s2x_stmt_row_to_object2(stmt, colNames)
            : s2x_stmt_row_to_object(stmt);
      }else if(-1==mode){
        /* "Normal" mode */
        *rv = cwal_value_true();
      }else{
        assert(-2==mode);
        *rv = natV;
      }
      rc = *rv ? 0 : CWAL_RC_OOM;
      break;
    }
    default:
      rc = s2x_sq3_toss(stmt->db);
      break;
  }
  return rc;
}

static int cb_s2x_stmt_step_tuple( cwal_callback_args const * args, cwal_value **rv ){
  return s2x_stmt_step_impl( args, rv, 0 );
}

static int cb_s2x_stmt_step_array( cwal_callback_args const * args, cwal_value **rv ){
  return s2x_stmt_step_impl( args, rv, 1 );
}

static int cb_s2x_stmt_step_object( cwal_callback_args const * args, cwal_value **rv ){
  return s2x_stmt_step_impl( args, rv, 2 );
}

static int cb_s2x_stmt_step( cwal_callback_args const * args, cwal_value **rv ){
  return s2x_stmt_step_impl( args, rv, -1 );
}

#if 0
/* not needed - role filled by Stmt.exec() */
static int cb_s2x_stmt_step_stmt( cwal_callback_args const * args, cwal_value **rv ){
  return s2x_stmt_step_impl( args, rv, -2 );
}
#endif

static int cb_s2x_stmt_reset( cwal_callback_args const * args, cwal_value **rv ){
  char resetCounter = 0;
  int rc;
  THIS_STMT;
  if(1 < args->argc) resetCounter = cwal_value_get_bool(args->argv[1]);
  rc = s2x_stmt_reset2(stmt, resetCounter);
  if(!rc) *rv = args->self;
  return rc
    ? s2x_sq3_toss( stmt->db )
    : 0;
}

static int cb_s2x_stmt_clear_bindings( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  THIS_STMT;
  rc = s2x_stmt_clear_bindings(stmt);
  if(!rc) *rv = args->self;
  return rc
    ? s2x_sq3_toss( stmt->db )
    : 0;
}

static int cb_s2x_stmt_row_to_array( cwal_callback_args const * args,
                                     cwal_value **rv ){
  int rc;
  THIS_STMT;
  if(!stmt->rowCount) return cwal_cb_throw(args, CWAL_RC_MISUSE,
                                     "Cannot fetch row data from an "
                                     "unstepped statement.");
  *rv = s2x_stmt_row_to_array(stmt);
  if(*rv) rc = 0;
  else rc = cwal_cb_throw(args, CWAL_RC_ERROR,
                    "Unknown error converting statement "
                    "row to Array.");
  return rc;
}

static int cb_s2x_stmt_row_to_tuple( cwal_callback_args const * args,
                                     cwal_value **rv ){
  int rc;
  THIS_STMT;
  if(!stmt->rowCount) return cwal_cb_throw(args, CWAL_RC_MISUSE,
                                     "Cannot fetch row data from an "
                                     "unstepped statement.");
  *rv = s2x_stmt_row_to_tuple(stmt);
  if(*rv) rc = 0;
  else rc = cwal_cb_throw(args, CWAL_RC_ERROR,
                    "Unknown error converting statement "
                    "row to Tuple.");
  return rc;
}

static int cb_s2x_stmt_row_to_object( cwal_callback_args const * args,
                                      cwal_value **rv ){
  int rc;
  THIS_STMT;
  if(!stmt->rowCount) return cwal_cb_throw(args, CWAL_RC_MISUSE,
                                     "Cannot fetch row data from "
                                     "an unstepped statement.");
  *rv = s2x_stmt_row_to_object(stmt);
  if(*rv) rc = 0;
  else rc = cwal_cb_throw(args, CWAL_RC_ERROR,
                    "Unknown error converting statement row "
                    "to Object.");
  return rc;
}


static int cb_s2x_stmt_bind( cwal_callback_args const * args,
                             cwal_value **rv ){
  cwal_int_t ndx = -999;
  int rc;
  THIS_STMT;
  if(!args->argc){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                   "Expecting (integer Index [,Value=null]) or "
                   "(Array|Tuple|Object|undefined) arguments.");
  }
  if(cwal_value_undefined() == args->argv[0]){
    /* Special case to simplify some script code */
    *rv = args->self;
    return 0;
  }else if(cwal_value_is_integer(args->argv[0])){
    ndx = cwal_value_get_integer(args->argv[0]);
    if(1>ndx){
      return cwal_cb_throw(args, CWAL_RC_RANGE,
                     "SQL bind() indexes are 1-based.");
    }
  }
  rc = s2x_stmt_bind2(stmt,
                      (int)(-999==ndx ? 1 : ndx),
                      (-999==ndx)
                      ? args->argv[0]
                      : ((args->argc>1)
                         ? args->argv[1]
                         : NULL));
  if(!rc) *rv = args->self;
  return rc;
}

static int cb_s2x_stmt_exec( cwal_callback_args const * args, cwal_value **rv ){
  int scode = 0, rc = 0;
  cwal_int_t i;
  THIS_STMT;
  for( i = 0; i < args->argc; ++i ){
    rc = s2x_stmt_bind2(stmt, (int)i+1, args->argv[i]);
    if(rc) return rc;
  }
  scode = s2x_stmt_step(stmt);
  switch(scode){
    case S2X_RC_STEP_DONE:
    case S2X_RC_STEP_ROW:
      rc = s2x_stmt_reset(stmt);
      break;
    default:
      rc = s2x_sq3_toss(stmt->db);
      break;
  }
  if(!rc){
    *rv = args->self;
  }
  return rc;  
}

static int cb_s2x_stmt_get( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  cwal_int_t ndx;
  THIS_STMT;
  if(!args->argc || !cwal_value_is_integer(args->argv[0])){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                   "Expecting Index arguments.");
  }
  else if(!stmt->colCount){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                   "This is not a fetch-style statement.");
  }
  ndx = cwal_value_get_integer(args->argv[0]);
  if((ndx<0) || (ndx>=stmt->colCount)){
    return cwal_cb_throw(args, CWAL_RC_RANGE,
                   "Column index %d is out of range. "
                   "Valid range is (%d..%d).", ndx,
                   0, stmt->colCount-1);
  }
  rc = s2x_stmt_to_value( stmt, (uint16_t)ndx, rv );
  if(rc && (CWAL_RC_EXCEPTION!=rc) && (CWAL_RC_OOM!=rc)){
    rc = cwal_cb_throw( args, rc, "Get-by-index failed with code %d (%s).",
                  rc, cwal_rc_cstr(rc));
  }
  return rc;
}

static int cb_s2x_sq3_exec_impl( cwal_callback_args const * args,
                                cwal_value **rv,
                                char isMulti ){
  int rc = 0;
  int argIndex = 0;
  THIS_DB;
  do{ /* loop on the arguments, expecting SQL for each one... */
    cwal_size_t sqlLen = 0;
    char const * sql = (args->argc>argIndex)
      ? cwal_value_get_cstr(args->argv[argIndex], &sqlLen)
      : NULL;
    if(!sql || !sqlLen){
      rc = (argIndex>0)
        ? 0
        : cwal_cb_throw(args,
                  CWAL_RC_MISUSE,
                  "Expecting a non-empty string/buffer "
                  "argument (SQL).");
      break;
    }
    /* MARKER(("SQL:<<<%.*s>>>\n", (int)sqlLen, sql)); */
    rc = isMulti
      ? s2x_sq3_exec_multi( db, "%.*s", (int)sqlLen, sql )
      : s2x_sq3_exec( db, "%.*s", (int)sqlLen, sql )
      ;
    if(rc) rc = s2x_sq3_toss( db );
  }while(!rc && (++argIndex < args->argc));
  if(!rc) *rv = args->self;
  return rc;
}

static int cb_s2x_sq3_exec_multi( cwal_callback_args const * args,
                                 cwal_value **rv ){
  return cb_s2x_sq3_exec_impl( args, rv, 1 );
}

static int cb_s2x_sq3_exec( cwal_callback_args const * args,
                           cwal_value **rv ){
  return cb_s2x_sq3_exec_impl( args, rv, 0 );
}

static int cb_s2x_sq3_last_insert_id( cwal_callback_args const * args,
                                     cwal_value **rv ){
  THIS_DB;
  *rv = cwal_new_integer(args->engine,
                         (cwal_int_t)s2x_sq3_last_insert_id(db));
  return *rv ? 0 : CWAL_RC_OOM;
}

/**
   If !cb, this is a no-op, else if cb is-a Function, it is call()ed,
   if it is a String/Buffer, it is eval'd, else an exception is
   thrown.  self is the 'this' for the call(). Result is placed in
   *rv.  Returns 0 on success.
   */
static int s2x_stmt_each_call_proxy( s2_engine * se, s2x_stmt * st,
                                        cwal_value * cb, cwal_value * self,
                                        cwal_value **rv ){
  char const * cstr;
  cwal_size_t strLen = 0;
  char const useNewScope = 1
    /*
      We need a new scope to avoid that evaluation of a string callback
      vacuums up self.
    */;
  if(!cb){
    if(st){/*avoid unused param warning*/}
    return 0;
  }
  else if((cstr = cwal_value_get_cstr(cb,&strLen))){
    /** TODO: tokenize this only the first time. */
    s2_ptoker pr = s2_ptoker_empty;
    cwal_scope scope = cwal_scope_empty;
    int rc;
    /**
       Bug Reminder: in stack traces and assertion traces the
       script name/location info is wrong in this case (empty and
       relative to cstr, respectively) because s2 doesn't have
       a way to map this string back to a source location at this
       point.
    */
    if(useNewScope){
      rc = cwal_scope_push2(se->e, &scope);
      if(rc) return rc;
    }
    s2_ptoker_init_v2(se->e, &pr, cstr, (int)strLen, 0);
    pr.name = "Db.each() callback";
    *rv = 0;
    rc = s2_eval_ptoker(se, &pr, 0, rv);
    if(CWAL_RC_RETURN==rc){
      rc = 0;
      *rv = cwal_propagating_take(se->e);
    }
    if(rc){
      assert(!*rv);
    }
    if(useNewScope){
      assert(se->scopes.current->cwalScope == &scope);
      cwal_scope_pop2(se->e, *rv);
      assert(!scope.level);
    }
    return rc;
  }else if(cwal_value_is_function(cb)){
    cwal_function * f = cwal_value_get_function(cb);
    if(useNewScope){
      return cwal_function_call(f, self, rv, 0, NULL );
    }else{
      cwal_scope * sc = cwal_scope_current_get(se->e);
      return cwal_function_call_in_scope(sc, f, self, rv,
                                         0, NULL );
    }
  }else {
    return cwal_exception_setf(se->e, CWAL_RC_MISUSE,
                               "Don't now how to handle callback "
                               "of type '%s'.",
                               cwal_value_type_name(cb));
  }
}

/**
   Script usage:

   db.each({
   sql: "SQL CODE", // required
   bind: X, // parameter value or array of values to bind.
            // Passing the undefined value is the same as
            // not passing any value.
   mode: 'o' || 'a' || 't', // object/array (default)/tuple
   each: string | function // called for each row
   })

   Only the 'sql' property is required, and 'bind' must be set
   if the SQL contains any binding placeholders.

   In the scope of the callback, 'this' will resolve to the current
   row data, either in object or array form (depending on the 'mode'
   property). If the callback throws, that exception is propagated.
   If it returns a literal false (as opposed to another falsy value)
   then iteration stops without an error.

   In addition, the following scope-level variables are set:

   - rowNumber: 1-based number of the row (the iteration count).

   - columnNames: array of column names for the result set.
  
   Example callbacks:

   <<<EOF
     print(rowNumber, columnNames, this);
     print(this.0 + this.1); // array-mode column access
   EOF

   proc(){
     print(rowNumber, columnNames, this);
     print(this.colA + this.colB); // object-mode column access
   }

   Using the string form should be ever so slightly more efficient.
*/
static int cb_s2x_sq3_each( cwal_callback_args const * args, cwal_value **rv ){
  int rc = 0;
  cwal_value const * sql /* SQL string */;
  char const * csql /* SQL c-string */;
  cwal_size_t sqlLen = 0 /* byte length of sql */;
  s2x_stmt st = s2x_stmt_empty;
  int scode /* result code from step() */;
  cwal_value * props /* the properties object (args->argv[0]) */;
  cwal_value * vMode /* props['mode'] property */;
  int mode = 0 /* row data type: 0=tuple, 1=array, 2=object */;
  cwal_value * bind /* bindable parameter values */;
  cwal_value * callback
    /* the callback string/function/array specified by the 'each'
       property. */;
  cwal_array * cbArray = NULL /* "this" IFF in array mode */;
  cwal_tuple * cbTuple = NULL /* "this" IFF in tuple mode */;
  cwal_value * cbSelf = NULL
    /* the "this" value for each row in the callback. Will be either
       an array(==>cbArray), a tuple(==>cbTuple), or an object. */;
  cwal_array * colNames = 0 /* list of column names */;
  cwal_value * colNamesV = 0 /* Value pointer to colNames */;
  cwal_array * destArray = 0
    /* If the 'each' property is an array, append each row to that
       array. In that case, we can skip certain processing bits
       which are needed for the string/function callback. */
    ;
  cwal_int_t rowNum /* current row number (1-based) for
                       string/function callback */;
  cwal_engine * e = args->engine;
  cwal_value * keyRowNum = 0;
  THIS_DB;
  if(!args->argc || !cwal_value_is_object(args->argv[0])){
    return cwal_exception_setf(e, CWAL_RC_MISUSE,
                               "Expecting Object parameter.");
  }
  props = args->argv[0];
  sql = cwal_prop_get(props, "sql", 3 );
  csql = sql ? cwal_value_get_cstr(sql, &sqlLen) : NULL;
  if(!csql){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                   "Missing 'sql' string/buffer property.");
  }
  vMode = cwal_prop_get( props, "mode", 4 );
  if(vMode){
    char const * vStr = cwal_value_get_cstr(vMode, 0);
    switch(vStr ? (int)*vStr : 0){
      case (int)'o':
      case (int)'O':
        mode = 2;
        break;
      case (int)'a':
      case (int)'A':
        mode = 1;
        break;
      case 0:
      case (int)'t':
      case (int)'T':
      default:
        mode = 0;
        break;
    }
  }
  bind = cwal_prop_get( props, "bind", 4 );
  callback = cwal_prop_get( props, "each", 4 );
  if(callback){
    if(!cwal_value_is_function(callback)
       && !cwal_value_get_cstr(callback, 0)
       && !(destArray=cwal_value_get_array(callback))){
      callback = NULL;
    }
  }
  rc = s2x_sq3_prepare(db, &st, "%.*s", (int)sqlLen, csql);
  if(rc){
    return s2x_sq3_toss(db);
  }
  if(bind && !cwal_value_is_undef(bind)){
    rc = s2x_stmt_bind2( &st, 1, bind );
    if(rc) goto end;
  }
  if(st.colCount &&
     (2==mode || !destArray)){
    /* If we're using a callback function/string or Object-mode rows,
       we need the column names. We don't need these if we're using
       destArray AND mode is 0 or 1. */
    colNamesV = s2x_stmt_col_names(e,&st);
    if(!colNamesV){
      rc = CWAL_RC_OOM;
      goto end;
    }
    cwal_value_ref(colNamesV);
    rc = s2_var_set( se, 0, "columnNames", 11, colNamesV );
    cwal_value_unref(colNamesV);
    if(rc){
      colNamesV = NULL;
      goto end;
    }
    assert(1==cwal_value_refcount(colNamesV) && "expecting scope to hold a ref!");
    colNames = cwal_value_get_array(colNamesV);
    assert(cwal_array_length_get(colNames) == (cwal_size_t)st.colCount);
  }
  if(!destArray){
    rc = s2_var_set( se, 0, "columnCount", 11,
                     cwal_new_integer(e, (cwal_int_t)st.colCount) );
    if(rc) goto end;
  }
    
  /*
    Step through each row and handle the 'each' callback
    function/string/array...
  */
  for( rowNum = 1;
       S2X_RC_STEP_ROW == (scode = s2x_stmt_step( &st ));
       ++rowNum){
    if(callback && !cbSelf){
      /** Init callback info if needed */
      cbTuple = (0==mode)
        ? cwal_new_tuple(se->e, (uint16_t)st.colCount)
        : NULL;
      cbArray = (1==mode)
        ? cwal_new_array(e)
        : 0;
      cbSelf = cbArray
        ? cwal_array_value(cbArray)
        : (cbTuple
           ? cwal_tuple_value(cbTuple)
           : cwal_new_object_value(e));
      if(!cbSelf){
        rc = CWAL_RC_OOM;
        break;
      }else if(!destArray){
        /* Set up "this" if we're using a callback function/string... */
        cwal_value_ref(cbSelf);
        rc = s2_var_set_v(se, 0,
                          se->cache.keyThis/*shameless internal state misuse!*/,
                          cbSelf);
        cwal_value_unref(cbSelf);
        if(rc){
          cbSelf = 0;
          goto end;
        }
        /* else the scope is holding a reference to cbSelf */
      }
    }
    if(cbSelf){
      /**
         Set up this.XXX to each column's value, where
         XXX is either the column index (for array mode)
         or column name (for object mode).
      */
      cwal_value * frv = 0 /* callback result */;
      int i = 0;
      cwal_value_ref(cbSelf);
      for( ; !rc && (i < st.colCount); ++i ){
        /* Collect the result columns... */
        cwal_value * cv;
        cv = NULL;
        rc = s2x_stmt_to_value(&st, i, &cv);
        if(rc && (CWAL_RC_EXCEPTION!=rc) && (CWAL_RC_OOM!=rc)){
          rc = cwal_exception_setf(e, rc,
                                   "Conversion from db column to "
                                   "value failed with code "
                                   "%d (%s).",
                                   rc, cwal_rc_cstr(rc));
        }
        if(cbArray) rc = cwal_array_set( cbArray, i, cv);
        else if(cbTuple) rc = cwal_tuple_set( cbTuple, (uint16_t)i, cv);
        else{
          cwal_value * key;
          assert(colNames);
          assert(cwal_value_is_object(cbSelf));
          key = cwal_array_get(colNames, i);
          assert(key);
          rc = cwal_prop_set_v( cbSelf, key, cv);
        }
        if(rc) cwal_value_unref(cv);
      }
      if(rc) goto do_break;
      if(destArray){
        /* Append the row to the destination array */
        cwal_value_ref(cbSelf);
        rc = cwal_array_append(destArray, cbSelf);
        cwal_value_unref(cbSelf);
        if(rc) goto do_break;
      }else{
        /* Call/eval the callback...  */
        if(!keyRowNum){
          /* Pedantically cache the "rowNumber" key to potentially
             save some allocations... */
          keyRowNum = cwal_new_string_value(se->e, "rowNumber", 9);
          if(!keyRowNum){
            rc = CWAL_RC_OOM;
            goto do_break;
          }
          cwal_value_ref(keyRowNum);
        }
        rc = s2_var_set_v( se, 0, keyRowNum, cwal_new_integer(e, rowNum) );
        if(rc) goto do_break /* leave potentially leaked new integer for the
                                call() scope to clean up */;
        rc = s2x_stmt_each_call_proxy(se, &st, callback, cbSelf, &frv);
        if(rc) goto do_break;
        else if(frv == cwal_value_false()/*yes, a ptr comparison*/){
          /* If the function returns literal false, stop
             looping without an error. */
          goto do_break;
        }else if(frv){
          /* If the callback returned anything, make sure it's cleaned
             up now, if needed/possible, rather than waiting on the
             death of the scope... */
          cwal_refunref(frv);
        }
      }
      assert(!rc);
      cwal_value_unref(cbSelf);
      cbSelf = 0 /* Causes the next iteration to create a new
                    array/tuple/object per row, overwriting "this". */;
      continue;
      do_break:
      cwal_value_unref(cbSelf);
      cbSelf = 0;
      break;
    }
    /* this is only reached if the 'each' property is not set. That's
       a legal, but unusual, use. */
  }
  if(S2X_RC_STEP_ERROR==scode){
    cwal_size_t msgLen = 0;
    char const * msg = 0;
    int const dbRc = s2x_sq3_err_get(st.db, &msg, &msgLen);
    assert(scode == dbRc);
    rc = cwal_exception_setf(e, dbRc,
                             "Stepping cursor failed with code #%d: %.*s",
                             dbRc, (int)msgLen, msg);
  }
  end:
  cwal_value_unref(keyRowNum);
  if(st.stmt){
    s2x_stmt_finalize( &st );
  }
  if(!rc) *rv = args->self;
  return rc;
}

/**
   Value DB.selectValue(string SQL [, bind = undefined [,defaultResult=undefined]])
*/
static int cb_s2x_sq3_select_value( cwal_callback_args const * args, cwal_value **rv ){
  int rc = 0, dbrc = 0;
  char const * sql;
  cwal_size_t sqlLen = 0;
  s2x_stmt st = s2x_stmt_empty;
  cwal_value * bind = 0;
  cwal_value * dflt = 0;
  THIS_DB;
  sql = args->argc ? cwal_value_get_cstr(args->argv[0], &sqlLen) : 0;
  if(!sql || !sqlLen){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting SQL string/buffer parameter.");
  }
  bind = args->argc>1 ? args->argv[1] : 0;

  dbrc = s2x_sq3_prepare( db, &st, "%.*s", (int)sqlLen, sql );
  if(dbrc){
    rc = s2x_sq3_toss(db);
    assert(rc);
    goto end;
  }

  if(bind){
    if(2==args->argc && !st.paramCount){
      dflt = bind;
    }else{
      rc = s2x_stmt_bind2(&st, 1, bind);
    }
    bind = 0;
  }

  if( !rc && (S2X_RC_STEP_ROW == (dbrc=s2x_stmt_step(&st)))){
    cwal_value * xrv = 0;
    rc = s2x_stmt_to_value(&st, 0, &xrv);
    if(!rc){
      *rv = xrv ? xrv : cwal_value_undefined();
    }
  }else if(S2X_RC_STEP_DONE == dbrc){
    *rv = dflt ? dflt : cwal_value_undefined();
  }else if(S2X_RC_STEP_ERROR == dbrc){
    assert(st.db->error.code);
    rc = s2x_sq3_toss(st.db);
  }
  end:
  s2x_stmt_finalize(&st);
  return rc;  
}


/**
   Array Db.selectValues(string|buffer SQL [, bind = undefined])
*/
static int cb_s2x_sq3_select_values( cwal_callback_args const * args, cwal_value **rv ){
  int rc = 0, dbrc = 0;
  char const * sql;
  cwal_size_t sqlLen = 0;
  s2x_stmt st = s2x_stmt_empty;
  cwal_value * bind = 0;
  cwal_array * ar = 0;
  cwal_value * arV = 0;
  THIS_DB;
  sql = args->argc ? cwal_value_get_cstr(args->argv[0], &sqlLen) : 0;
  if(!sql || !sqlLen){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting SQL string/buffer argument.");
  }
  bind = args->argc>1 ? args->argv[1] : 0;

  dbrc = s2x_sq3_prepare( db, &st, "%.*s", (int)sqlLen, sql );
  if(dbrc){
    rc = s2x_sq3_toss(db);
    assert(rc);
    goto end;
  }

  ar = cwal_new_array(args->engine);
  if(!ar){
    rc = CWAL_RC_OOM;
    goto end;
  }
  arV = cwal_array_value(ar);
  cwal_value_ref(arV);

  if(bind){
    rc = s2x_stmt_bind2(&st, 1, bind);
    bind = 0;
  }

  while( !rc && (S2X_RC_STEP_ROW == (dbrc=s2x_stmt_step(&st)))){
    cwal_value * col = 0;
    rc = s2x_stmt_to_value(&st, 0, &col);
    if(!rc){
      cwal_value_ref(col);
      rc = cwal_array_append(ar, col);
      cwal_value_unref(col);
    }
  }

  end:
  s2x_stmt_finalize(&st);
  if(rc){
    cwal_value_unref(arV);
  }else if(arV){
    *rv = arV;
    cwal_value_unhand(arV);
  }
  return rc;  
}

/**
   Script usage:

   void db.busyTimeout(integer ms)

   where ms is a number of milliseconds. Returns the 'this'
   value. Throws if the argument is invalid or if
   sqlite3_busy_timeout() returns an error (its error codes are, as of
   this writing, undocumented).
*/
static int cb_s2x_sq3_busy_timeout( cwal_callback_args const * args, cwal_value **rv ){
  cwal_value * arg = args->argc ? args->argv[0] : 0;
  cwal_int_t ms = 0;
  THIS_DB;
  if(!arg || !cwal_value_is_number(arg) || (ms=cwal_value_get_integer(arg))<0){
    return cwal_cb_throw( args, CWAL_RC_MISUSE,
                        "Expecting a non-negative integer argument.");
  }else{
    int rc = s2x_sq3_busy_timeout(db, (int)ms);
    if(rc && CWAL_RC_OOM!=rc){
      rc = cwal_cb_throw( args, rc,
                        "sqlite3_busy_timeout( db, %d ) "
                        "failed with sqlite result code "
                        "%d.", (int)ms, rc );
    }else if(!rc){
      *rv = args->self;
    }
    return rc;
  }
}


static int cb_s2x_sq3_trans_begin( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  THIS_DB;
  rc = s2x_sq3_transaction_begin(db);
  if(rc){
    rc = s2x_sq3_toss(db);
  }
  if(!rc) *rv = args->self;
  return rc;
}

static int cb_s2x_sq3_trans_end( cwal_callback_args const * args, cwal_value **rv, int mode ){
  int rc;
  THIS_DB;
  if(db->beginCount<=0){
    return cwal_cb_throw( args, CWAL_RC_RANGE, "No transaction is active.");
  }
  if(mode < 0){
    rc = s2x_sq3_rollback_force(db);
  }else{
    rc = s2x_sq3_transaction_end(db, mode ? 1 : 0);
  }
  if(rc){
    if(db->error.code){
      rc = s2x_sq3_toss(db);
    }else{
      rc = cwal_cb_throw( args, rc,
                    "error code during %s: %d (%s)",
                    (mode<0
                     ? "forced rollback"
                     : (mode>0
                        ? "rollback"
                        : "commit")
                     ),
                    rc, cwal_rc_cstr(rc) );
    }
  }else{
    *rv = args->self;
  }
  return rc;
}

static int cb_s2x_sq3_trans_commit( cwal_callback_args const * args, cwal_value **rv ){
  return cb_s2x_sq3_trans_end( args, rv, 0 );
}

static int cb_s2x_sq3_trans_rollback( cwal_callback_args const * args, cwal_value **rv ){
  return cb_s2x_sq3_trans_end( args, rv,
                              (args->argc
                               && cwal_value_get_bool(args->argv[0]))
                              ? -1 /* force immediate rollback */
                              : 1 );
}

static int cb_s2x_sq3_trans_state( cwal_callback_args const * args, cwal_value **rv ){
  int bc;
  THIS_DB;
  bc = db->beginCount > 0 ? db->beginCount : 0;
  *rv = cwal_new_integer(args->engine,
                         (cwal_int_t)(db->doRollback ? -bc : bc));
  return *rv ? 0 : CWAL_RC_OOM;
}

static int cb_s2x_sq3_trans( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  cwal_function * func;
  THIS_DB;
  func = args->argc ? cwal_value_function_part(args->engine, args->argv[0]) : 0;
  if(!func){
    return cwal_cb_throw(args, CWAL_RC_MISUSE, "Expecting a Function argument.");
  }
  rc = s2x_sq3_transaction_begin( db );
  if(rc){
    return db->error.code
      ? s2x_sq3_toss(db)
      : cwal_cb_throw(args, CWAL_RC_ERROR,
                "Error #%d (%s) while starting transaction.",
                rc, cwal_rc_cstr(rc));
  }
  rc = cwal_function_call( func, args->self, 0, 0, 0 );
  if(rc){
    s2x_sq3_transaction_end( db, 1 );
#if 0
    /* Just proving to myself that this rollback is still called in
       the face of an s2-level exit()/fatal()/assert(). It does. We
       can in fact preempt a FATAL result here, but doing so is a bad
       idea.
    */
    if(CWAL_RC_FATAL==rc){
      cwal_exception_set(args->engine, cwal_propagating_take(args->engine));
      rc = CWAL_RC_EXCEPTION;
    }
#endif
  }else{
    rc = s2x_sq3_transaction_end( db, 0 );
    if(rc){
      rc = db->error.code
        ? s2x_sq3_toss(db)
        : cwal_cb_throw(args, CWAL_RC_ERROR,
                  "Error #%d (%s) while committing transaction.",
                  rc, cwal_rc_cstr(rc));
    }else{
      *rv = args->self;
    }
  }
  return rc;
}

static void s2x_sq3_result_error(s2_engine * se, sqlite3_context * context,
                                 int rc){
  if(CWAL_RC_OOM == rc){
    if(se){/*avoid unused param warning*/}
    sqlite3_result_error_nomem(context);
  }else{
    /* if s2_engine_err_has() is non-0, it's very likely that
       that state is about to trump any error we'd report via
       this mechanism... */
    /* s2_engine_err_reset(se); */
    /* cwal_exception_set(se->e, 0); */
    sqlite3_result_error(context,
                         "TODO: improve reporting of script-side UDF errors.",
                         -1);
  }
}

/**
   sqlite3 UDF which wraps all script-side UDFs defined
   by this binding layer.
*/
static void cb_s2x_sq3_udf(
  sqlite3_context *context,
  int argc,
  sqlite3_value **argv
){
  int rc = 0;
  cwal_value * rv = 0;
  cwal_value * fv = (cwal_value *)sqlite3_user_data(context);
  cwal_function * f = cwal_value_get_function(fv);
  int i;
  char errWasSet = 0;
  uint16_t cargc = 0;
  cwal_engine * e = cwal_value_engine(fv);
  enum { MaxArgs = 20 };
  cwal_value * cargv[MaxArgs + 1] = {0};
  s2_engine * se = s2_engine_from_state(e);
  cwal_scope scope = cwal_scope_empty;
  assert(se);
  assert(f);
  assert(fv);
  assert(e);
  if(argc > (int)MaxArgs){
    sqlite3_result_error(context, "Too many SQL function arguments.", -1);
    return;
  }
  rc = cwal_scope_push2( se->e, &scope );
  if(rc){
    if(CWAL_RC_OOM==rc) sqlite3_result_error_nomem(context);
    else sqlite3_result_error(context,
                              "Unknown error while pushing a new cwal scope.",
                              -1 );
    return;
  }
  assert( scope.parent );
  cargv[MaxArgs] = 0;
  for( i = 0; i < argc && i < MaxArgs; ++i ){
    cargv[i] = 0;
    rc = s2x_sq3_value_to_cwal(e, argv[i], &cargv[i]);
    if(rc){
      sqlite3_result_error(context,
                           "Conversion from sqlite to cwal value failed.",
                           -1);
      errWasSet = 1;
      assert(!cargv[i]);
      break;
    }
    cwal_value_ref(cargv[i]);
    ++cargc;
  }

  if(!rc){
    cwal_value * finalKey = s2x_sq3_cached_string(se, KeyUdfFinal);
    rc = finalKey
      ? cwal_scope_chain_set_with_flags_v( &scope, 0, finalKey,
                                           cwal_value_false(),
                                           CWAL_VAR_F_CONST)
      : CWAL_RC_OOM;
    if(!rc){
      rc = cwal_function_call_in_scope( &scope, f, fv,
                                        &rv, cargc, cargv);
      cwal_value_ref(rv);
    }
  }
  for( i = 0; i < cargc; ++i ){
    cwal_value_unref(cargv[i]);
    cargv[i] = 0;
  }
  if(rc){
    assert(!rv);
    if(!errWasSet){
      s2x_sq3_result_error(se, context, rc);
    }
  }else{
    s2x_sq3_result(context, rv);
    cwal_value_unref(rv);
  }
  cwal_scope_pop( se->e );
}

/**
   sqlite3 UDF which wraps all "final" script-side aggregate UDFs
   defined by this binding layer.
*/
static void cb_s2x_sq3_udf_final(
  sqlite3_context *context
){
  int rc = 0;
  cwal_value * rv = 0;
  cwal_value * fv = (cwal_value *)sqlite3_user_data(context);
  cwal_function * f = cwal_value_get_function(fv);
  cwal_engine * e = cwal_value_engine(fv);
  s2_engine * se = s2_engine_from_state(e);
  cwal_scope scope = cwal_scope_empty;
  cwal_value * finalKey;
  assert(se);
  assert(f);
  assert(fv);
  rc = cwal_scope_push2( se->e, &scope );
  if(rc){
    if(CWAL_RC_OOM==rc) sqlite3_result_error_nomem(context);
    else sqlite3_result_error(context,
                              "Unknown error while pushing a new cwal scope.",
                              -1 );
    return;
  }
  assert(!rc && "can't fail if args are okay" );
  assert( scope.parent );
  finalKey = s2x_sq3_cached_string(se, KeyUdfFinal );
  if(!finalKey){
    sqlite3_result_error_nomem(context);
  }else{
    rc = cwal_scope_chain_set_with_flags_v( &scope, 0, finalKey,
                                            cwal_value_true(),
                                            CWAL_VAR_F_CONST);
    if(!rc){
      rc = cwal_function_call_in_scope( &scope, f, fv, &rv, 0, 0 );
      cwal_value_ref(rv);
    }
    if(rc){
      s2x_sq3_result_error(se, context, rc);
    }else{
      s2x_sq3_result(context, rv);
    }
    if(rv) cwal_value_unref(rv);
  }
  cwal_scope_pop( se->e );
}

/**
  Internal helper for processing arguments to the script-space UDF
  creation function.
*/
static int cb_udf_decode_obj( cwal_callback_args const * args,
                              cwal_value * opt,
                              cwal_function ** step,
                              cwal_function ** final,
                              int * isAggregate,
                              int * sqFuncFlags ){
  cwal_value * fi = 0;
  cwal_value * st = cwal_prop_get(opt, "step", 4);
  if(!st || !cwal_value_is_function(st)){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                      "Missing 'step' function.");
  }
  *step = cwal_value_get_function(st);

#if 0
  fi = cwal_prop_get(opt, "final", 5);
  if(fi && !cwal_value_is_function(st)){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                      "'final' is not a function.");
  }
#endif

  *isAggregate = fi
    ? 1 : (int)cwal_value_get_bool(cwal_prop_get(opt,"aggregate",9));

  if(!fi && *isAggregate){
    fi = st;
  }
  if(final){
    *final = fi ? cwal_value_get_function(fi) : NULL;
  }
  st = cwal_prop_get( opt, "deterministic", 13);
  if(st && cwal_value_get_bool(st)){
    *sqFuncFlags = SQLITE_DETERMINISTIC;
    /* MARKER(("flagging as deterministic.\n")); */
  }else{
    /* MARKER(("NOT flagging as deterministic.\n")); */
  }
  return 0;
}

/**
   Signature:

   sqlite3 udf( string name [, bool isAggregate=false], function callback );

   sqlite3 udf( string name, Object config );

   Config:
   {
     step: proc,
     // step/final _have_ to be the same callback :/
     // final: proc, // optional for aggregates
     aggregate: bool, // ignored if final is set, else final is set to step.
     deterministic: bool // default = false
   }

   Optionally, the "deterministic" flag may be set on the callback
   itself, but that one is only checked for the non-config-object call
   forms.
*/
static int cb_s2x_sq3_create_udf( cwal_callback_args const * args,
                                  cwal_value **rv ){
  int rc;
  cwal_size_t nameLen = 0;
  char const * name =0 ;
  cwal_function * funcStep = 0;
  cwal_value * fv = 0;
  cwal_hash * h;
  int aggregate = 0;
  cwal_value * keyNorm = 0 /* normalized-case key */;
  int sfFlags = 0;
  char wasFromObj = 0;
  THIS_DB;
  if(2==args->argc
     && cwal_props_can(args->argv[1])
     && !cwal_value_is_function(args->argv[1])
     ){
    /* (string name, Object) */
    rc = cb_udf_decode_obj(args, args->argv[1], &funcStep, 0,
                           &aggregate, &sfFlags);
    if(rc) return rc;
    wasFromObj = 1;
    name = cwal_value_get_cstr(args->argv[0], &nameLen);
    fv = cwal_function_value(funcStep);
  }else if(args->argc>2){
    /* (string name, bool isAggregate, Function) */
    name = cwal_value_get_cstr(args->argv[0], &nameLen);
    aggregate = cwal_value_get_bool(args->argv[1]);
    fv = args->argv[2];
    funcStep = cwal_value_get_function(fv);
  }else{
    /* (string name, Function) */
    fv = args->argc>1 ? args->argv[1] : 0;
    funcStep = fv ? cwal_value_get_function(fv) : 0;
    name = funcStep ? cwal_value_get_cstr(args->argv[0], &nameLen) : 0;
  }
  if(!funcStep || !nameLen){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                      "Expecting (%sString [, bool isAggregate=false],"
                      "Function) arguments.",
                      (funcStep && !nameLen) ? "non-empty-" : "");
  }
  assert(fv);
  if(!wasFromObj){
    cwal_value const * vnd = cwal_prop_get( fv, "deterministic", 13);
    if(vnd && cwal_value_get_bool(vnd)){
      sfFlags = SQLITE_DETERMINISTIC;
    }
  }
  h = s2x_udf_store( args->self, db );
  if(!h) return CWAL_RC_OOM;
  rc = cwal_utf8_case_fold( args->engine, name, nameLen, &keyNorm, 1 );
  if(rc) return rc;
  assert(keyNorm);
  cwal_value_ref(keyNorm);
  rc = cwal_hash_insert_v( h, keyNorm, fv, 0 );
  if(rc){
    /* Overwriting with the same names leaves the old copy in sqlite
       (not sure why), which causes it to step on a stale fv
       pointer. */
    rc = cwal_cb_throw(args, rc, "Hash insert of [%.*s] failed with code %s!",
                    (int)nameLen, name, cwal_rc_cstr(rc));
    goto end;
  }
  if(aggregate){
    rc = sqlite3_create_function(db->dbh, name, -1, SQLITE_UTF8 | sfFlags,
                                 fv, 0, cb_s2x_sq3_udf, cb_s2x_sq3_udf_final);
  }else{
    rc = sqlite3_create_function(db->dbh, name, -1, SQLITE_UTF8 | sfFlags,
                                 fv, cb_s2x_sq3_udf, 0, 0);
  }
  if(rc){
    cwal_hash_remove_v( h, keyNorm );
    rc = cwal_cb_throw(args, S2X_RC_DB,
                    "UDF creation failed with sqlite3 code #%d.",
                    rc);
  }else{
    *rv = args->self;
  }
  end:
  assert(cwal_value_refcount(keyNorm) && "but we ref'd it?");
  cwal_value_unref(keyNorm);
  return rc;
}


cwal_value * s2x_stmt_prototype( s2_engine * se, cwal_value * protoStorage ){
  int rc = 0;
  cwal_value * proto;
  static char const * pKey = "Stmt";
  cwal_size_t const pKeyLen = 4;
  cwal_value * pKeyV = 0;
  proto = cwal_prop_get( protoStorage, pKey, pKeyLen );
  if(proto) return proto;
  pKeyV = cwal_new_xstring_value(se->e, pKey, pKeyLen);
  if(!pKeyV){
    rc = CWAL_RC_OOM;
    goto end;
  }
  cwal_value_ref(pKeyV);
  
  proto = cwal_new_object_value(se->e);
  if(!proto){
    cwal_value_unref(pKeyV);
    rc = CWAL_RC_OOM;
    goto end;
  }
  cwal_value_ref(proto);
  rc = cwal_prop_set_with_flags_v( protoStorage, pKeyV, proto,
                                   CWAL_VAR_F_CONST );
  cwal_value_unref(pKeyV);
  cwal_value_unref(proto);
  if(rc) goto end;

  assert(cwal_value_refcount(pKeyV) && "But... ?");
  rc = s2_typename_set_v( se, proto, pKeyV );
  if(rc) goto end;
  {
    s2_func_def const funcs[] = {
      S2_FUNC2("bind", cb_s2x_stmt_bind),
      S2_FUNC2("clearBindings", cb_s2x_stmt_clear_bindings),
      S2_FUNC2("exec", cb_s2x_stmt_exec),
      S2_FUNC2("finalize", cb_s2x_stmt_finalize),
      S2_FUNC2("get", cb_s2x_stmt_get),
      S2_FUNC2("reset", cb_s2x_stmt_reset),
      S2_FUNC2("rowToArray", cb_s2x_stmt_row_to_array),
      S2_FUNC2("rowToTuple", cb_s2x_stmt_row_to_tuple),
      S2_FUNC2("rowToObject", cb_s2x_stmt_row_to_object),
      S2_FUNC2("step", cb_s2x_stmt_step),
      S2_FUNC2("stepArray", cb_s2x_stmt_step_array),
      S2_FUNC2("stepTuple", cb_s2x_stmt_step_tuple),
      S2_FUNC2("stepObject", cb_s2x_stmt_step_object),
      /* S2_FUNC2("stepStmt", cb_s2x_stmt_step_stmt), */
      s2_func_def_empty_m
    };
    rc = s2_install_functions(se, proto, funcs, 0);
    if(rc) goto end;
  }

  {
    /* Stmt.each() impl. */
    char const * src =
      "proc(func){"
        "affirm typeinfo(iscallable func) && typeinfo(iscallable func.call); "
        "while(this.step()) false === func.call(this) && break;"
        "return this"
      "}";
      int const srcLen = (int)cwal_strlen(src);
      rc = s2_set_from_script(se, src, srcLen, proto, "each", 4);
  }
  end:
  return rc ? NULL : proto;
}

cwal_value * s2x_sq3_prototype( s2_engine * se ){
  int rc = 0;
  cwal_value * proto;
  cwal_value * v;
  static char const * pKey = "sqlite3";
  cwal_size_t pKeyLen = 7 /* cwal_strlen(pKey) */;
  cwal_engine * e;
  e = s2_engine_engine(se);
  assert(se && e);
  proto = cwal_new_object_value(e);
  if(!proto){
    rc = CWAL_RC_OOM;
    goto end;
  }
  cwal_value_ref(proto);
  if(rc) goto end;
#define VCHECK if(!v){ rc = CWAL_RC_OOM; goto end; } cwal_value_ref(v)
#define SET(NAME)                                           \
  VCHECK;                                                   \
  rc = cwal_prop_set( proto, NAME, cwal_strlen(NAME), v );  \
  cwal_value_unref(v); \
  if(rc) {goto end; } (void)0
#define FUNC(NAME,FP,STATE,TYPEID)                           \
  v = cwal_new_function_value(e, FP, STATE, 0, TYPEID );     \
  SET(NAME)
  
  v = cwal_new_xstring_value(e, pKey, pKeyLen);
  VCHECK;
  rc = s2_typename_set_v(se, proto, v);
  cwal_value_unref(v);
  if(rc) goto end;

  FUNC("open", cb_s2x_sq3_ctor, proto, MY_TYPEID(sq3_prototype));
  rc = s2_ctor_method_set(se, proto, cwal_value_get_function(v));
  {
    /* We stashed the prototype in open(), noting that that is a
       (void*) stash, not a (cwal_value*) stash, meaning that there is
       still a lifetime-level issue to solve (see below). */
    assert((proto ==
            cwal_function_state_get(cwal_value_get_function(v),
                                    MY_TYPEID(sq3_prototype)))
           && "But we just stashed this!");
    rc = s2_stash_hidden_member(v, proto)
      /* keeps proto alive so long as nobody calls
         cwal_props_clear(v). */;
  }
  if(rc) goto end;
  {
    s2_func_def const funcs[] = {
      S2_FUNC2("transactionState", cb_s2x_sq3_trans_state),
      S2_FUNC2("transaction", cb_s2x_sq3_trans),
      S2_FUNC2("totalChanges", cb_s2x_sq3_total_changes),
      S2_FUNC2("selectValues", cb_s2x_sq3_select_values),
      S2_FUNC2("selectValue", cb_s2x_sq3_select_value),
      S2_FUNC2("rollback", cb_s2x_sq3_trans_rollback),
      S2_FUNC2("prepare", cb_s2x_sq3_prepare),
      S2_FUNC2("lastInsertId", cb_s2x_sq3_last_insert_id),
      S2_FUNC2("getFilename", cb_s2x_sq3_filename),
      S2_FUNC2("execMulti", cb_s2x_sq3_exec_multi),
      S2_FUNC2("exec", cb_s2x_sq3_exec),
      S2_FUNC2("each", cb_s2x_sq3_each),
      S2_FUNC2("createUDF", cb_s2x_sq3_create_udf),
      S2_FUNC2("commit", cb_s2x_sq3_trans_commit),
      S2_FUNC2("close", cb_s2x_sq3_finalize),
      S2_FUNC2("changes", cb_s2x_sq3_changes),
      S2_FUNC2("busyTimeout", cb_s2x_sq3_busy_timeout),
      S2_FUNC2("begin", cb_s2x_sq3_trans_begin),
      s2_func_def_empty_m
    };
    rc = s2_install_functions(se, proto, funcs, 0);
    if(rc) goto end;
  }


  {
    /* selectRow(sql,bind,asTuple) impl. */
    char const * src =
      "proc(s,b,t){"/*sql, bind, asTuple*/
        "return this.prepare(s).bind(b)"
        "[t?'stepTuple':'stepObject']()"
        /* return will indirectly finalize() the anonymous stmt. */
      "}";
    rc = s2_set_from_script(se, src, (int)cwal_strlen(src),
                            proto, "selectRow", 9);
    if(rc) goto end;
    /* selectRows(sql,bind,asTuple) impl: */
    src = "proc(s,b,t){"/*sql, bind, asTuple*/
        "const S = this.prepare(s).bind(b), m = t?'stepTuple':'stepObject', rc=[];"
        "var v; while(v=S[m]())rc[]=v; S.finalize();"
        "return rc;"
      "}";
    rc = s2_set_from_script(se, src, (int)cwal_strlen(src),
                            proto, "selectRows", 10);
    if(rc) goto end;
  }

  v = s2x_stmt_prototype(se, proto) /* installs Stmt in the proto */;
  if(!v) rc = CWAL_RC_OOM;

#undef SET
#undef FUNC
#undef VCHECK
  end:
  if(rc) cwal_value_unref(proto);
  else cwal_value_unhand(proto);
  return rc ? NULL : proto;
}

/**
   s2 module initialization routine.

   Installs an object named "sqlite3" into ns, wrapping the sqlite3
   API:

   https://sqlite.org
*/

static int s2_module_init_sqlite3( s2_engine * se, cwal_value ** rv ){
  cwal_value * v;
  int rc;
  v = s2x_sq3_prototype(se);
  if(v){
    *rv = v;
    rc = 0;
  }else{
    rc = s2_engine_err_has(se);
    if(!rc) rc = CWAL_RC_OOM;
  }
  return rc;
}

S2_MODULE_REGISTER_(sqlite3);

#undef MARKER
#undef THIS_DB
#undef THIS_STMT
#undef MY_TYPEID
