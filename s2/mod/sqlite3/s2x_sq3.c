/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
/*
  Author: Stephan Beal (sgbeal@googlemail.com)

  Origin: derived from code which was vaguely derived from code by
  D. Richard Hipp.

  License: Pulic Domain
  
  *****************************************************************************
   This file contains the s2x_sq3_xxx() and s2x_stmt_xxx() parts of the
   API.
  
   Maintenance reminders:
  
   When returning dynamically allocated memory to the client, it needs
   to come from cwal_malloc(), as opposed to sqlite3_malloc(), so that
   it is legal to pass to cwal_free().
*/
#include "s2x_sq3.h"
#include <assert.h>
#include <stddef.h> /* NULL on linux */
#include <time.h> /* time() and friends */
#include <string.h> /* memcmp() */

/* Only for debugging */
#define MARKER(pfexp)                                               \
  do{ printf("MARKER: %s:%d:%s():\t",__FILE__,__LINE__,__func__);   \
    printf pfexp;                                                   \
  } while(0)


const s2x_sq3 s2x_sq3_empty = s2x_sq3_empty_m;
const s2x_stmt s2x_stmt_empty = s2x_stmt_empty_m;

static const int cwal_type_id_s2x_sq3_prototype = 0;
const s2x_type_ids S2X_SQ3_TYPE_IDS = {
&s2x_sq3_empty,
&cwal_type_id_s2x_sq3_prototype,
&s2x_stmt_empty
};

#if 0
/**
    cwal_list_visitor_f() impl which requires that obj be NULL or
    a (s2x_st*), which it passed to s2x_stmt_finalize().
 */
static int cwal_list_v_s2x_stmt_finalize(void * obj, void * visitorState ){
  if(obj) s2x_stmt_finalize( (s2x_stmt*)obj );
  return 0;
}
#endif

char * s2x_strndup( cwal_engine * e, char const * src, cwal_int_t len ){
  if(!src) return NULL;
  else{
    cwal_buffer b = cwal_buffer_empty;
    if(len<0) len = (cwal_int_t)cwal_strlen(src);
    cwal_buffer_append( e, &b, src, (cwal_size_t)len );
    return (char*)b.mem;
  }
}

char * s2x_strdup( cwal_engine * e, char const * src ){
  return s2x_strndup(e, src, -1);
}

char const * s2x_buffer_cstr(cwal_buffer const * b){
  return b ? (char const *)b->mem : 0;
}

char const * s2x_buffer_cstr2(cwal_buffer const * b, cwal_size_t * len){
  if(len) *len = b ? b->used : 0;
  return b ? (char const *)b->mem : 0;
}

int s2x_strcmp(const char *zA, const char *zB){
  if( zA==0 ) return zB ? -1 : 0;
  else if( zB==0 ) return 1;
  else{
    int a, b;
    do{
      a = *zA++;
      b = *zB++;
    }while( a==b && a!=0 );
    return ((unsigned char)a) - (unsigned char)b;
  }
}


/*int s2x_strcmp_cmp( void const * lhs, void const * rhs ){
  return s2x_strcmp((char const *)lhs, (char const *)rhs);
  }*/

int s2x_strncmp(const char *zA, const char *zB, cwal_size_t nByte){
  if( !zA ) return zB ? -1 : 0;
  else if( !zB ) return +1;
  else if(!nByte) return 0;
  else{
    int a, b;
    do{
      a = *zA++;
      b = *zB++;
    }while( a==b && a!=0 && (--nByte)>0 );
    return (nByte>0) ? (((unsigned char)a) - (unsigned char)b) : 0;
  }
}

int s2x_buffer_compare(cwal_buffer const * lhs, cwal_buffer const * rhs){
  cwal_size_t const szL = lhs->used;
  cwal_size_t const szR = rhs->used;
  cwal_size_t const sz = (szL<szR) ? szL : szR;
  int rc = memcmp(lhs->mem, rhs->mem, sz);
  if(0 == rc){
    rc = (szL==szR)
      ? 0
      : ((szL<szR) ? -1 : 1);
  }
  return rc;
}



void s2x_sq3_clear_strings(s2x_sq3 * db, char alsoErrorState ){
  assert(db->se);
  cwal_free(db->se->e, db->filename);
  db->filename = NULL;
  if(alsoErrorState) cwal_error_clear(db->se->e, &db->error);
}

int s2x_sq3_err_get( s2x_sq3 const * db, char const ** msg, cwal_size_t * len ){
  return db
    ? cwal_error_get(&db->error, msg, len)
    : CWAL_RC_MISUSE;
}

s2x_sq3 * s2x_stmt_db( s2x_stmt * stmt ){
  return stmt ? stmt->db : NULL;
}

/**
    Resets db->error state based on the given code and the current
    error string from the db driver. Returns S2X_RC_DB on success,
    some other non-0 value on error (most likely CWAL_RC_OOM while
    allocating the error string - that's the only other error case as
    long as db is opened). Results are undefined if !db or db is not
    opened.
 */
static int s2x_err_from_db( s2x_sq3 * db, int dbCode ){
  assert(db && db->dbh);
  return cwal_error_set(db->se->e, &db->error, S2X_RC_DB,
                        "Db error #%d: %s",
                        dbCode, sqlite3_errmsg(db->dbh));
}

char const * s2x_stmt_sql( s2x_stmt * stmt, cwal_size_t * len ){
  char const * rc = stmt ? (char const *)stmt->sql.mem : 0;
  if(len) *len = rc ? stmt->sql.used : 0U;
  return rc;
}

char const * s2x_sq3_filename(s2x_sq3 const * db, cwal_size_t * len){
  if(!db) return NULL;
  if(len && db->filename) *len = cwal_strlen(db->filename);
  return db->filename;
}

s2x_sq3_id_t s2x_sq3_last_insert_id(s2x_sq3 *db){
  return (db && db->dbh)
    ? (s2x_sq3_id_t)sqlite3_last_insert_rowid(db->dbh)
    : -1;
}

int cwal_list_visitor_f_cwal_free(void * obj, void * visitorState ){
  cwal_engine * e = (cwal_engine *)visitorState;
  assert(e);
  if(obj) cwal_free( e, obj );
  return 0;
}

/**
    Cleans up db->beforeCommit and its contents.
 */
static void s2x_sq3_cleanup_beforeCommit( s2x_sq3 * db ){
  assert(db->se);
  cwal_list_visit( &db->beforeCommit, -1, cwal_list_visitor_f_cwal_free,
                   db->se->e );
  cwal_list_reserve(db->se->e, &db->beforeCommit, 0);
}

cwal_size_t s2x_sq3_stmt_cache_clear(s2x_sq3 * db){
  cwal_size_t rc = 0;
  if(db && db->cacheHead){
    s2x_stmt * st;
    s2x_stmt * next = 0;
    for( st = db->cacheHead; st; st = next, ++rc ){
      next = st->next;
      st->next = 0;
      s2x_stmt_finalize( st );
    }
    db->cacheHead = 0;
  }
  return rc;
}

void s2x_sq3_close( s2x_sq3 * const db ){
  void const * allocStamp = db->allocStamp;
  s2_engine * se = db->se;
  assert(se);
  while(db->beginCount>0){
    s2x_sq3_transaction_end(db, 1);
  }
  s2x_sq3_stmt_cache_clear(db);
  assert(!db->cacheHead);
  if(0!=db->openStatementCount){
    db->deathCounter = db->openStatementCount;
#if 0
    MARKER(("WARNING: %d open statement(s) left on db [%s] "
            "when it is being closed. "
            "This can lead to Undefined Behaviour!\n",
            (int)db->openStatementCount, db->filename));
#endif
  }
  if(db->dbh){
    sqlite3_close_v2(db->dbh);
    /* ignoring results in the style of "destructors may not
       throw". */
  }
  s2x_sq3_clear_strings(db, 1);
  s2x_sq3_cleanup_beforeCommit(db);
  if(db->vSelf){
    cwal_native * nat = cwal_value_get_native(db->vSelf);
    assert(nat);
    db->vSelf = 0;
    cwal_native_clear( nat, 0 )
      /* reminder: the native binding's cwal-level finalizer calls
         this routine, so we don't want to pass truthy as the 2nd
         parameter, as that calls the cwal-side finalizer. We do,
         however, need to clean up db->udfStore if this function
         happens to get called from a different path than the
         finalizer... */
      ;
    if(db->udfStore){
      assert(cwal_value_refcount(db->udfStore));
      cwal_value_unref(db->udfStore);
      db->udfStore = 0;
    }
  }
  assert(!db->udfStore);
  if(db->deathCounter){
    if(&s2x_sq3_empty == db->allocStamp){
      /* We are leaking this handle, _probably_ temporarily, until the
         final whx_stmt which refers to it will free it.*/
#if 0
      MARKER(("We have no choice but to leak this db handle! "
              "The alternative is stepping on a stale pointer "
              "when statements finalize!\n"));
#endif
    }else{
      /* If !db->allocStamp then this is a stack-allocated db handle
         and we're now in undefined territory */
      MARKER(("WARNING: a stack-allocated whx_sq3 handle is left "
              "with %d active statement handles.\n",
              (int)db->deathCounter));
    }
  }else{
    *db = s2x_sq3_empty;
    if(&s2x_sq3_empty == allocStamp){
      cwal_free2( se->e, db, sizeof(s2x_sq3) );
    }else{
      db->allocStamp = allocStamp;
      db->se = se;
    }
  }
}

void s2x_sq3_err_reset( s2x_sq3 * db ){
  if(db && (db->error.code||db->error.msg.used)){
    cwal_error_reset(&db->error);
  }
}


int s2x_sq3_attach(s2x_sq3 * db, const char *zDbName, cwal_size_t dbLen,
                   const char *zLabel, cwal_size_t labelLen){
  if(!dbLen) dbLen = cwal_strlen(zDbName);
  if(!labelLen) labelLen = cwal_strlen(zLabel);
  return (db && db->dbh && zDbName && *zDbName && zLabel && *zLabel)
    ? s2x_sq3_exec(db, "ATTACH DATABASE %.*Q AS %.*s",
                   (int)dbLen, zDbName, (int)labelLen, zLabel)
    : CWAL_RC_MISUSE;
}

int s2x_sq3_detach(s2x_sq3 * db, const char *zLabel, cwal_size_t labelLen){
  if(!labelLen) labelLen = cwal_strlen(zLabel);
  return (db && db->dbh && zLabel && *zLabel)
    ? s2x_sq3_exec(db, "DETACH DATABASE %.*s", (int)labelLen, zLabel)
    : CWAL_RC_MISUSE;
}

char * s2x_sq3_julian_to_iso8601( s2x_sq3 * db, double j,
                                 char msPrecision,
                                 char localTime){
  char * s = NULL;
  s2x_stmt * st = NULL;
  if(db && db->dbh && (j>=0.0)){
    char const * sql;
    if(msPrecision){
      sql = localTime
        ? "SELECT strftime('%%Y-%%m-%%dT%%H:%%M:%%f',?, 'localtime')"
        : "SELECT strftime('%%Y-%%m-%%dT%%H:%%M:%%f',?)";
    }else{
      sql = localTime
        ? "SELECT strftime('%%Y-%%m-%%dT%%H:%%M:%%S',?, 'localtime')"
        : "SELECT strftime('%%Y-%%m-%%dT%%H:%%M:%%S',?)";
    }
    s2x_sq3_prepare_cached(db, &st, sql);
    if(st){
      s2x_stmt_bind_double( st, 1, j );
      if( S2X_RC_STEP_ROW==s2x_stmt_step(st) ){
        s = s2x_strdup(db->se->e, s2x_stmt_g_text(st, 0, NULL));
      }
      s2x_stmt_cached_yield(st);
    }
  }
  return s;
}

char * s2x_sq3_unix_to_iso8601( s2x_sq3 * db, s2x_sq3_time_t t, char localTime ){
  char * s = NULL;
  s2x_stmt * st = NULL;
  if(db && db->dbh && (t>=0)){
    char const * sql = localTime
      ? "SELECT datetime(?, 'unixepoch', 'localtime')"
      : "SELECT datetime(?, 'unixepoch')"
      ;
    s2x_sq3_prepare_cached(db, &st, sql);
    if(st){
      s2x_stmt_bind_int64( st, 1, t );
      if( S2X_RC_STEP_ROW==s2x_stmt_step(st) ){
        cwal_size_t n = 0;
        char const * v = s2x_stmt_g_text(st, 0, &n);
        s = (v&&n)
          ? s2x_strndup(db->se->e, v, (cwal_int_t)n)
          : NULL;
      }
      s2x_stmt_cached_yield(st);
    }
  }
  return s;
}


int s2x_sq3_preparev( s2x_sq3 *db, s2x_stmt * tgt, char const * sql, va_list args ){
  if(!db || !tgt || !sql) return CWAL_RC_MISUSE;
  else if(!db->dbh){
    return cwal_error_set(db->se->e, &db->error, CWAL_RC_NOT_FOUND, "Db is not opened.");
  }else if(!*sql){
    return cwal_error_set(db->se->e, &db->error, CWAL_RC_RANGE, "SQL is empty.");
  }else if(tgt->stmt){
    return cwal_error_set(db->se->e, &db->error, CWAL_RC_ALREADY_EXISTS,
                         "Error: attempt to re-prepare "
                         "active statement.");
  }
  else{
    int rc;
    cwal_buffer buf = cwal_buffer_empty;
    s2x_stmt_t * liteStmt = NULL;
    s2_engine * se = db->se;
    tgt->db = db /* needed for cleanup when error handling */;
    rc = cwal_buffer_printfv( se->e, &buf, sql, args );
    if(!rc){
      sql = (char const *)buf.mem;
      if(!sql || !*sql){
        rc = cwal_error_set(db->se->e, &db->error, CWAL_RC_RANGE,
                            "Input SQL is empty.");
      }else{
        /*
          Achtung: if sql==NULL here, or evaluates to a no-op
          (e.g. only comments or spaces), prepare_v2 succeeds but has
          a NULL liteStmt, which is why we handle the empty-SQL case
          specially. We don't want that specific behaviour leaking up
          through the API. Though doing so would arguably more correct
          in a generic API, for this particular API we have no reason
          to be able to handle empty SQL. Were we do let through
          through we'd have to add a flag to s2x_stmt to tell us
          whether it's really prepared or not, since checking of
          st->stmt would no longer be useful.
        */
        rc = sqlite3_prepare_v2(db->dbh, sql, (int)buf.used,
                                &liteStmt, 0);
        if(rc){
          rc = s2_error_set(db->se, &db->error, S2X_RC_DB,
                             "Db statement preparation failed. "
                             "Error #%d: %s. SQL: %.*s",
                             rc, sqlite3_errmsg(db->dbh),
                             (int)buf.used, (char const *)buf.mem);
        }else if(!liteStmt){
          /* SQL was empty. In sqlite this is allowed, but this API will
             disallow this because it leads to headaches downstream.
          */
          rc = s2_error_set(db->se, &db->error, CWAL_RC_RANGE,
                             "Input SQL is empty.");
        }
      }
    }
    if(!rc){
      assert(liteStmt);
      ++db->openStatementCount;
      tgt->stmt = liteStmt;
      tgt->db = db;
      tgt->sql = buf /*transfer ownership*/;
      tgt->colCount = sqlite3_column_count(tgt->stmt);
      tgt->paramCount = sqlite3_bind_parameter_count(tgt->stmt);
    }else{
      tgt->db = 0;
      assert(!liteStmt);
      cwal_buffer_clear(db->se->e, &buf);
    }
    return rc;
  }
}

int s2x_sq3_prepare( s2x_sq3 *db, s2x_stmt * tgt, char const * sql, ... ){
  int rc;
  va_list args;
  va_start(args,sql);
  rc = s2x_sq3_preparev( db, tgt, sql, args );
  va_end(args);
  return rc;
}

enum s2x_stmt_flags_e {
/**
    s2x_stmt::flags bit indicating that s2x_sq3_preparev_cache() has
    doled out this statement, effectively locking it until
    s2x_stmt_cached_yield() is called to release it.
 */
S2X_STMT_F_CACHE_HELD = 0x01
};

int s2x_sq3_prepare_cachedv( s2x_sq3 * db, s2x_stmt ** rv,
                            char const * sql, va_list args ){
  int rc = 0;
  cwal_buffer buf = cwal_buffer_empty;
  s2x_stmt * st = NULL;
  s2x_stmt * cs = NULL;
  if(!db || !rv || !sql) return CWAL_RC_MISUSE;
  else if(!*sql) return CWAL_RC_RANGE;
  rc = cwal_buffer_printfv( db->se->e, &buf, sql, args );
  if(rc) goto end;
  for( cs = db->cacheHead; cs; cs = cs->next ){
    if(0==s2x_buffer_compare(&buf, &cs->sql)){
      if(cs->flags & S2X_STMT_F_CACHE_HELD){
        rc = s2_error_set(db->se, &db->error, CWAL_RC_ACCESS,
                           "Cached statement is already in use. "
                           "Do not use cached statements if recursion "
                           "involving the statement is possible, and use "
                           "s2x_stmt_cached_yield() to release them "
                           "for further (re)use. SQL: %.*s",
                          (int)cs->sql.used, (char const *)cs->sql.mem);
        goto end;
      }
      cs->flags |= S2X_STMT_F_CACHE_HELD;
      *rv = cs;
      goto end;
    }
  }
  st = s2x_stmt_malloc(db->se->e);
  if(!st){
    rc = CWAL_RC_OOM;
    goto end;
  }
  rc = s2x_sq3_prepare( db, st, "%.*s", (int)buf.used,
                        (char const *)buf.mem );
  if(!rc){
    st->next = db->cacheHead;
    db->cacheHead = st;
    *rv = st;
    st->flags = S2X_STMT_F_CACHE_HELD;
  }else{
    s2x_stmt_finalize(st);
  }
  end:
  cwal_buffer_clear(db->se->e, &buf);
  return rc;
}

int s2x_sq3_prepare_cached( s2x_sq3 * db, s2x_stmt ** st, char const * sql, ... ){
  int rc;
  va_list args;
  va_start(args,sql);
  rc = s2x_sq3_prepare_cachedv( db, st, sql, args );
  va_end(args);
  return rc;
}

int s2x_stmt_cached_yield( s2x_stmt * st ){
  if(!st || !st->db || !st->stmt) return CWAL_RC_MISUSE;
  else if(!(st->flags & S2X_STMT_F_CACHE_HELD)) {
    return s2_error_set(st->db->se, &st->db->error, CWAL_RC_MISUSE,
                         "s2x_stmt_cached_yield() was passed a "
                         "statement which is not marked as cached. "
                         "SQL: %.*s",
                         (int)st->sql.used, (char const *)st->sql.mem);
  }else{
    s2x_stmt_reset(st);
    s2x_stmt_clear_bindings(st);
    st->flags &= ~S2X_STMT_F_CACHE_HELD;
    return 0;
  }
}



int s2x_sq3_before_commitv( s2x_sq3 * db,
                           char const * sql, va_list args ){
  int rc = 0;
  char * cp = NULL;
  cwal_buffer buf = cwal_buffer_empty;
  if(!db || !sql) return CWAL_RC_MISUSE;
  else if(!*sql) return CWAL_RC_RANGE;
  rc = cwal_buffer_printfv( db->se->e, &buf, sql, args );
  if(!rc){
    cp = (char *)buf.mem /* transfer ownership */;
    buf = cwal_buffer_empty;
    rc = cwal_list_append(db->se->e, &db->beforeCommit, cp);
    if(rc) cwal_free(db->se->e, cp);
  }
  cwal_buffer_clear(db->se->e, &buf);
  return rc;
}

int s2x_sq3_before_commit( s2x_sq3 *db, char const * sql, ... ){
  int rc;
  va_list args;
  va_start(args,sql);
  rc = s2x_sq3_before_commitv( db, sql, args );
  va_end(args);
  return rc;
}

int s2x_stmt_finalize( s2x_stmt * stmt ){
  if(!stmt) return CWAL_RC_MISUSE;
  else{
    void const * allocStamp = stmt->allocStamp;
    s2x_sq3 * const db = stmt->db;
    cwal_engine * const ce = db ? db->se->e : 0;
    /*
      Potential TODO: if (se) then do a weak-ref check for db. This
      would require adding a cwal_weakref to s2x_sq3_stmt and
      initializing it during s2x_sq3_prepare(). Alternately, keep
      track of all statements within the db handle and close them
      if they haven't been closed when the db is closed.
    */
    if(stmt->vSelf){
      cwal_native * nat = cwal_value_get_native(stmt->vSelf);
      assert(nat);
      stmt->vSelf = 0;
      cwal_native_clear( nat, 0 );
    }
    if(db){
      if(stmt->sql.mem){
        /* ^^^ b/c that buffer is set at the same time
           that openStatementCount is incremented.
        */
        --stmt->db->openStatementCount;
      }
      if((stmt->flags & S2X_STMT_F_CACHE_HELD)
         && db->cacheHead){
        /* It appears to be cached - let's remove it. */
        s2x_stmt * s;
        s2x_stmt * prev = 0;
        for( s = db->cacheHead; s; prev = s, s = s->next ){
          if(s == stmt){
            if(prev){
              assert(prev->next == s);
              prev->next = s->next;
            }else{
              assert(s == db->cacheHead);
              db->cacheHead = s->next;
            }
            s->next = 0;
            break;
          }
        }
      }
    }
    if(ce){
      cwal_buffer_clear(ce, &stmt->sql);
    }else{
      assert(!stmt->sql.mem && "We have memory but no (de)allocator!");
    }
    if(stmt->stmt){
      sqlite3_finalize( stmt->stmt );
    }
    *stmt = s2x_stmt_empty;
    if(db && db->deathCounter && 0==--db->deathCounter
       && &s2x_sq3_empty==db->allocStamp){
      //MARKER(("whx_stmt is freeing its whx_sq3 handle.\n"));
      *db = s2x_sq3_empty;
      cwal_free2(ce, db, sizeof(s2x_sq3));
    }      
    if(&s2x_stmt_empty==allocStamp){
      cwal_free2(ce, stmt, sizeof(s2x_stmt));
    }else{
      stmt->allocStamp = allocStamp;
    }
    return 0;
  }
}

int s2x_stmt_step( s2x_stmt * stmt ){
  if(!stmt || !stmt->stmt) return CWAL_RC_MISUSE;
  else{
    int const rc = sqlite3_step(stmt->stmt);
    assert(stmt->db);
    switch( rc ){
      case SQLITE_ROW:
        ++stmt->rowCount;
        return S2X_RC_STEP_ROW;
      case SQLITE_DONE:
        return S2X_RC_STEP_DONE;
      default:
        s2_error_set(stmt->db->se, &stmt->db->error,
                     S2X_RC_STEP_ERROR,
                     "sqlite error #%d: %s",
                     rc, sqlite3_errmsg(stmt->db->dbh));
        return S2X_RC_STEP_ERROR;
    }
  }
}

int s2x_sq3_eachv( s2x_sq3 * db, s2x_stmt_each_f callback,
                  void * callbackState, char const * sql, va_list args ){
  if(!db || !db->dbh || !callback || !sql) return CWAL_RC_MISUSE;
  else if(!*sql) return CWAL_RC_RANGE;
  else{
    s2x_stmt st = s2x_stmt_empty;
    int rc;
    rc = s2x_sq3_preparev( db, &st, sql, args );
    if(!rc){
      rc = s2x_stmt_each( &st, callback, callbackState );
      s2x_stmt_finalize( &st );
    }
    return rc;
  }
}

int s2x_sq3_each( s2x_sq3 * db, s2x_stmt_each_f callback,
                 void * callbackState, char const * sql, ... ){
  int rc;
  va_list args;
  va_start(args,sql);
  rc = s2x_sq3_eachv( db, callback, callbackState, sql, args );
  va_end(args);
  return rc;
}

int s2x_stmt_each( s2x_stmt * stmt, s2x_stmt_each_f callback,
                   void * callbackState ){
  if(!stmt || !callback) return CWAL_RC_MISUSE;
  else{
    int strc;
    int rc = 0;
    char doBreak = 0;
    while( !doBreak && (S2X_RC_STEP_ROW == (strc=s2x_stmt_step(stmt)))){
      rc = callback( stmt, callbackState );
      switch(rc){
        case 0: continue;
        case CWAL_RC_BREAK:
          rc = 0;
          CWAL_SWITCH_FALL_THROUGH;
        default:
          doBreak = 1;
          break;
      }
    }
    return rc
      ? rc
      : ((S2X_RC_STEP_ERROR==strc)
         ? S2X_RC_DB
         : 0);
  }
}

int s2x_stmt_reset2( s2x_stmt * stmt, char resetRowCounter ){
  if(!stmt || !stmt->stmt || !stmt->db) return CWAL_RC_MISUSE;
  else{
    int const rc = sqlite3_reset(stmt->stmt);
    if(resetRowCounter) stmt->rowCount = 0;
    return rc
      ? s2x_err_from_db(stmt->db, rc)
      : 0;
  }
}

int s2x_stmt_reset( s2x_stmt * stmt ){
  return s2x_stmt_reset2(stmt, 0);
}


int s2x_stmt_clear_bindings( s2x_stmt * stmt ){
  if(!stmt || !stmt->stmt || !stmt->db) return CWAL_RC_MISUSE;
  else{
    int const rc = sqlite3_clear_bindings(stmt->stmt);
    return rc
      ? s2x_err_from_db(stmt->db, rc)
      : 0;
  }
}

int s2x_stmt_col_count( s2x_stmt const * stmt ){
  return (!stmt || !stmt->stmt)
    ? -1
    : stmt->colCount
    ;
}

char const * s2x_stmt_col_name(s2x_stmt * stmt, int index){
  return (stmt && stmt->stmt && (index>=0 && index<stmt->colCount))
    ? sqlite3_column_name(stmt->stmt, index)
    : NULL;
}

int s2x_stmt_param_count( s2x_stmt const * stmt ){
  return (!stmt || !stmt->stmt)
    ? -1
    : stmt->paramCount;
}

/**
    UNTESTED.
   
    Binds a series of values using a formatting string.
   
    The string may contain the following characters, each of which
    refers to the next argument in the args list:
   
    '-': binds a NULL and expects a NULL placeholder
    in the argument list (for consistency's sake).
   
    'i': binds an int32
   
    'I': binds an int64
   
    'R': binds a s2x_sq3_id_t ('R' as in 'RID')
   
    'f': binds a double
   
    's': binds a (char const *) as a string or NULL.
   
    'S': binds a (char const *) as a blob or NULL.
   
    'b': binds a (cwal_buffer const *) as a string or NULL.
   
    'B': binds a (cwal_buffer const *) as a blob or NULL.
   
    ' ': spaces are allowed for readability and are ignored.
 */
int s2x_stmt_bind_fmtv( s2x_stmt * st, char const * fmt, va_list args ){
  int rc = 0, ndx;
  char const * pos = fmt;
  if(!fmt ||
     !(st && st->stmt && st->db && st->db->dbh)) return CWAL_RC_MISUSE;
  else if(!*fmt) return CWAL_RC_RANGE;
  for( ndx = 1; !rc && *pos; ++pos, ++ndx ){
    if(ndx > st->paramCount){
      rc = s2_error_set(st->db->se, &st->db->error, CWAL_RC_RANGE,
                        "Column index %d is out of bounds.", ndx);
      break;
    }
    switch(*pos){
      case ' ':
        --ndx;
        continue;
      case '-':
        va_arg(args,void const *) /* skip arg */;
        rc = s2x_stmt_bind_null(st, ndx);
        break;
      case 'i':
        rc = s2x_stmt_bind_int32(st, ndx, va_arg(args,int32_t));
        break;
      case 'I':
        rc = s2x_stmt_bind_int64(st, ndx, va_arg(args,int64_t));
        break;
      case 'R':
        rc = s2x_stmt_bind_id(st, ndx, va_arg(args,s2x_sq3_id_t));
        break;
      case 'f':
        rc = s2x_stmt_bind_double(st, ndx, va_arg(args,double));
        break;
      case 's':{/* C-string as TEXT or NULL */
        char const * s = va_arg(args,char const *);
        rc = s
          ? s2x_stmt_bind_text(st, ndx, s, -1, 1)
          : s2x_stmt_bind_null(st, ndx);
        break;
      }
      case 'S':{ /* C-string as BLOB or NULL */
        void const * s = va_arg(args,void const *);
        rc = s
          ? s2x_stmt_bind_blob(st, ndx, s, -1, 1)
          : s2x_stmt_bind_null(st, ndx);
        break;
      }
      case 'b':{ /* cwal_buffer as TEXT or NULL */
        cwal_buffer const * b = va_arg(args,cwal_buffer const *);
        rc = (b && b->mem)
          ? s2x_stmt_bind_text(st, ndx, (char const *)b->mem,
                               (cwal_int_t)b->used, 1)
          : s2x_stmt_bind_null(st, ndx);
        break;
      }
      case 'B':{ /* cwal_buffer as BLOB or NULL */
        cwal_buffer const * b = va_arg(args,cwal_buffer const *);
        rc = (b && b->mem)
          ? s2x_stmt_bind_blob(st, ndx, b->mem, (cwal_int_t)b->used, 1)
          : s2x_stmt_bind_null(st, ndx);
        break;
      }
      default:
        rc = s2_error_set(st->db->se, &st->db->error, CWAL_RC_RANGE,
                          "Invalid format character: '%c'", *pos);
        break;
    }
  }
  return rc;
}

#define BIND_PARAM_CHECK \
  if(!(stmt && stmt->stmt && stmt->db && stmt->db->dbh)) return CWAL_RC_MISUSE; else
#define BIND_PARAM_CHECK2 BIND_PARAM_CHECK \
  if(ndx<1 || ndx>stmt->paramCount) return CWAL_RC_RANGE; else
int s2x_stmt_bind_null( s2x_stmt * stmt, int ndx ){
  BIND_PARAM_CHECK2 {
    int const rc = sqlite3_bind_null( stmt->stmt, ndx );
    return rc ? s2x_err_from_db(stmt->db, rc) : 0;
  }
}

int s2x_stmt_bind_int32( s2x_stmt * stmt, int ndx, int32_t v ){
  BIND_PARAM_CHECK2 {
    int const rc = sqlite3_bind_int( stmt->stmt, ndx, (int)v );
    return rc ? s2x_err_from_db(stmt->db, rc) : 0;
  }
}

int s2x_stmt_bind_int64( s2x_stmt * stmt, int ndx, int64_t v ){
  BIND_PARAM_CHECK2 {
    int const rc = sqlite3_bind_int64( stmt->stmt, ndx, (sqlite3_int64)v );
    return rc ? s2x_err_from_db(stmt->db, rc) : 0;
  }
}

int s2x_stmt_bind_id( s2x_stmt * stmt, int ndx, s2x_sq3_id_t v ){
  BIND_PARAM_CHECK2 {
    int const rc = sqlite3_bind_int64( stmt->stmt, ndx, (sqlite3_int64)v );
    return rc ? s2x_err_from_db(stmt->db, rc) : 0;
  }
}

int s2x_stmt_bind_double( s2x_stmt * stmt, int ndx, double v ){
  BIND_PARAM_CHECK2 {
    int const rc = sqlite3_bind_double( stmt->stmt, ndx, (double)v );
    return rc ? s2x_err_from_db(stmt->db, rc) : 0;
  }
}

int s2x_stmt_bind_blob( s2x_stmt * stmt, int ndx, void const * src,
                        cwal_size_t len, char makeCopy ){
  BIND_PARAM_CHECK2 {
    int rc;
    rc = sqlite3_bind_blob( stmt->stmt, ndx, src, (int)len,
                            makeCopy ? SQLITE_TRANSIENT : SQLITE_STATIC );
    return rc ? s2x_err_from_db(stmt->db, rc) : 0;
  }
}

int s2x_stmt_bind_text( s2x_stmt * stmt, int ndx, char const * src,
                        cwal_int_t len, char makeCopy ){
  BIND_PARAM_CHECK {
    int rc;
    if(len<0) len = cwal_strlen((char const *)src);
    rc = sqlite3_bind_text( stmt->stmt, ndx, src, len,
                            makeCopy ? SQLITE_TRANSIENT : SQLITE_STATIC );
    return rc ? s2x_err_from_db(stmt->db, rc) : 0;
  }
}


int s2x_stmt_bind_null_name( s2x_stmt * stmt, char const * param ){
  BIND_PARAM_CHECK{
    return s2x_stmt_bind_null( stmt,
                               sqlite3_bind_parameter_index( stmt->stmt,
                                                             param) );
  }
}

int s2x_stmt_bind_int32_name( s2x_stmt * stmt, char const * param, int32_t v ){
  BIND_PARAM_CHECK {
    return s2x_stmt_bind_int32( stmt,
                                sqlite3_bind_parameter_index( stmt->stmt,
                                                              param),
                                v);
  }
}

int s2x_stmt_bind_int64_name( s2x_stmt * stmt, char const * param, int64_t v ){
  BIND_PARAM_CHECK {
    return s2x_stmt_bind_int64( stmt,
                                sqlite3_bind_parameter_index( stmt->stmt,
                                                              param),
                                v);
  }
}

int s2x_stmt_bind_id_name( s2x_stmt * stmt, char const * param, s2x_sq3_id_t v ){
  BIND_PARAM_CHECK {
    return s2x_stmt_bind_id( stmt,
                             sqlite3_bind_parameter_index( stmt->stmt,
                                                           param),
                             v);
  }
}

int s2x_stmt_bind_double_name( s2x_stmt * stmt, char const * param, double v ){
  BIND_PARAM_CHECK {
    return s2x_stmt_bind_double( stmt,
                                 sqlite3_bind_parameter_index( stmt->stmt,
                                                               param),
                                 v);
  }
}

int s2x_stmt_bind_text_name( s2x_stmt * stmt, char const * param,
                             char const * v, cwal_int_t n,
                             char makeCopy ){
  BIND_PARAM_CHECK {
    return s2x_stmt_bind_text(stmt,
                              sqlite3_bind_parameter_index( stmt->stmt,
                                                            param),
                              v, n, makeCopy);
  }
}

int s2x_stmt_bind_blob_name( s2x_stmt * stmt, char const * param,
                             void const * v, cwal_int_t len,
                             char makeCopy ){
  BIND_PARAM_CHECK {
    return s2x_stmt_bind_blob(stmt,
                         sqlite3_bind_parameter_index( stmt->stmt,
                                                       param),
                              v, len, makeCopy);
  }
}

int s2x_stmt_param_index( s2x_stmt * stmt, char const * param){
  return (stmt && stmt->stmt)
    ? sqlite3_bind_parameter_index( stmt->stmt, param)
    : -1;
}

#undef BIND_PARAM_CHECK
#undef BIND_PARAM_CHECK2

#define GET_CHECK if(!stmt || !stmt->colCount) return CWAL_RC_MISUSE; \
  else if((ndx<0) || (ndx>=stmt->colCount)) return CWAL_RC_RANGE; else

int s2x_stmt_get_int32( s2x_stmt * stmt, int ndx, int32_t * v ){
  GET_CHECK {
    if(v) *v = (int32_t)sqlite3_column_int(stmt->stmt, ndx);
    return 0;
  }
}
int s2x_stmt_get_int64( s2x_stmt * stmt, int ndx, int64_t * v ){
  GET_CHECK {
    if(v) *v = (int64_t)sqlite3_column_int64(stmt->stmt, ndx);
    return 0;
  }
}

int s2x_stmt_get_double( s2x_stmt * stmt, int ndx, double * v ){
  GET_CHECK {
    if(v) *v = (double)sqlite3_column_double(stmt->stmt, ndx);
    return 0;
  }
}

int s2x_stmt_get_id( s2x_stmt * stmt, int ndx, s2x_sq3_id_t * v ){
  GET_CHECK {
    if(v) *v = (4==sizeof(s2x_sq3_id_t))
      ? (s2x_sq3_id_t)sqlite3_column_int(stmt->stmt, ndx)
      : (s2x_sq3_id_t)sqlite3_column_int64(stmt->stmt, ndx);
    return 0;
  }
}

int s2x_stmt_get_text( s2x_stmt * stmt, int ndx, char const **out,
                       cwal_size_t * outLen ){
  GET_CHECK {
    unsigned char const * t = (out || outLen)
      ? sqlite3_column_text(stmt->stmt, ndx)
      : NULL;
    if(out) *out = (char const *)t;
    if(outLen){
      int const x = sqlite3_column_bytes(stmt->stmt, ndx);
      *outLen = (x>0) ? (cwal_size_t)x : 0;
    }
    return 0;
  }
}

int s2x_stmt_get_blob( s2x_stmt * stmt, int ndx, void const **out,
                       cwal_size_t * outLen ){
  GET_CHECK {
    void const * t = (out || outLen)
      ? sqlite3_column_blob(stmt->stmt, ndx)
      : NULL;
    if(out) *out = t;
    if(outLen){
      if(!t) *outLen = 0;
      else{
        int sz = sqlite3_column_bytes(stmt->stmt, ndx);
        *outLen = (sz>=0) ? (cwal_size_t)sz : 0;
      }
    }
    return 0;
  }
}

#undef GET_CHECK

s2x_sq3_id_t s2x_stmt_g_id( s2x_stmt * stmt, int index ){
  s2x_sq3_id_t rv = -1;
  s2x_stmt_get_id(stmt, index, &rv);
  return rv;
}
int32_t s2x_stmt_g_int32( s2x_stmt * stmt, int index ){
  int32_t rv = 0;
  s2x_stmt_get_int32(stmt, index, &rv);
  return rv;
}
int64_t s2x_stmt_g_int64( s2x_stmt * stmt, int index ){
  int64_t rv = 0;
  s2x_stmt_get_int64(stmt, index, &rv);
  return rv;
}
double s2x_stmt_g_double( s2x_stmt * stmt, int index ){
  double rv = 0;
  s2x_stmt_get_double(stmt, index, &rv);
  return rv;
}

char const * s2x_stmt_g_text( s2x_stmt * stmt, int index,
                              cwal_size_t * outLen ){
  char const * rv = NULL;
  s2x_stmt_get_text(stmt, index, &rv, outLen);
  return rv;
}


/**
   If (SQLITE_TRACE_STMT==t), this function outputs tracing info using
   fprintf((FILE*)C,...), defaulting to stdout if zFILE is 0.
*/
static int s2x_db_sql_trace_v2(unsigned t,void* C,void* P,void* X){
  /*
    https://www.sqlite.org/c3ref/trace_v2.html
    https://www.sqlite.org/c3ref/c_trace.html
  */
  switch(t){
    case SQLITE_TRACE_STMT:{
      char const *zSql = (char const *)X ;
      int const n = (int)cwal_strlen(zSql);
      FILE * zFILE = (FILE *)C;
      static int counter = 0;
      fprintf(zFILE ? (FILE*)zFILE : stdout,
              "SQL TRACE #%d: %.*s%s\n", ++counter,
              n, zSql, (n>0 && zSql[n-1]==';') ? "" : ";");
      break;
    }
    default:
      if(P){/*unused*/}
      break;
  }
  return 0;
}

#if 0
/*
   SQL function for debugging.
  
   The print() function writes its arguments to s2x_output()
   if the bound s2x_cx->cxConfig.sqlPrint flag is true.
*/
static void s2x_sq3_sql_print(
  sqlite3_context *context,
  int argc,
  sqlite3_value **argv
){
#if 1
  MARKER(("FIXME: missing sql_print bits.\n"));
#else
  s2_engine * se = (s2_engine*)sqlite3_user_data(context);
  assert(se);
  if( f->cxConfig.sqlPrint ){
    int i;
    for(i=0; i<argc; i++){
      char c = i==argc-1 ? '\n' : ' ';
      cwal_outputf(se->e, "%s%c", sqlite3_value_text(argv[i]), c);
    }
  }
#endif
}
#endif

#if 0
/*
   SQL function to return the number of seconds since 1970.  This is
   the same as strftime('%s','now') but is more compact.
*/
static void s2x_sq3_now_udf(
  sqlite3_context *context,
  int argc,
  sqlite3_value **argv
){
  sqlite3_result_int64(context, (sqlite3_int64)time(0));
}
#endif

s2x_sq3 * s2x_sq3_malloc(s2_engine * se){
  s2x_sq3 * rc = (s2x_sq3 *)cwal_malloc(se->e, sizeof(s2x_sq3));
  if(rc){
    *rc = s2x_sq3_empty;
    rc->se = se;
    rc->allocStamp = &s2x_sq3_empty;
  }
  return rc;
}

s2x_stmt * s2x_stmt_malloc(cwal_engine * e){
  s2x_stmt * rc = (s2x_stmt *)cwal_malloc2(e, sizeof(s2x_stmt));
  if(rc){
    *rc = s2x_stmt_empty;
    rc->allocStamp = &s2x_stmt_empty;
  }
  return rc;
}

/**
   Callback for use with sqlite3_commit_hook(). The argument must be a
   (s2x_sq3*). This function returns 0 only if it surmises that
   s2x_sq3_transaction_end() triggered the COMMIT. On error it might
   assert() or abort() the application, so this really is just a
   sanity check for something which "must not happen."
*/
static int s2x_sq3_verify_begin_was_not_called(void * db_){
#if 0
  return 0;
#else
  s2x_sq3 * db = (s2x_sq3 *)db_;
  assert(db && "What else could it be?");
  assert(db->dbh && "Else we can't have been called by sqlite3, could we have?");
  if(db->beginCount>0){
#if 0
    s2_fatal(CWAL_RC_MISUSE,"SQL: db@%p: COMMIT was called from "
             "outside of s2x_sq3_transaction_end() while a "
             "s2x_sq3_transaction_begin()-started transaction "
             "is pending. db->beginCount=%d", (void *)db, db->beginCount)
      /* does not return */;
#else
    /**
       20191108: It turns out that using an (ON CONFLICT ROLLBACK)
       clause in a table's constraints can cause our transaction state
       to get misaligned with sqlite3's view of it. Using ON CONFLICT
       ABORT does what we expect here, though.
    */
    s2_throw(db->se, CWAL_RC_ASSERT,"SQL: db@%p: COMMIT was called from "
             "outside of s2x_sq3_transaction_end() while a "
             "s2x_sq3_transaction_begin()-started transaction "
             "is pending. db->beginCount=%d", (void *)db, db->beginCount);
#endif
    return 2;
  }
  /* we have no context: sqlite3_result_error(context, "s2x_mtime_of_manifest_file() failed", -1);  */
  else return 0;
#endif
}

int s2x_sq3_open( s2_engine * se,
                 s2x_sq3 * db, char const * dbFile,
                 int openFlags ){
  int rc;
  s2x_dbh_t * dbh = NULL;
  int isMem = 0;
  if(!se || !db || !dbFile) return CWAL_RC_MISUSE;
  else if(db->dbh) return CWAL_RC_MISUSE;
  else if(!(isMem = (!*dbFile || 0==s2x_strcmp(":memory:", dbFile)))
          && !(S2X_SQ3_OPEN_F_CREATE & openFlags)
          && !s2_file_is_accessible(dbFile, 0)){
    return s2_error_set(db->se, &db->error, CWAL_RC_NOT_FOUND,
                         "DB file not found or not accessible: %s",
                        dbFile);
  }
  else{
    int sOpenFlags = 0;
    db->se = se;
    if(isMem){
      sOpenFlags = SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE;
    }else{
      if(S2X_SQ3_OPEN_F_RO & openFlags){
        sOpenFlags |= SQLITE_OPEN_READONLY;
      }else{
        if(S2X_SQ3_OPEN_F_RW & openFlags){
          sOpenFlags |= SQLITE_OPEN_READWRITE;
        }
        if(S2X_SQ3_OPEN_F_CREATE & openFlags){
          sOpenFlags |= SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE;
        }
        if(!sOpenFlags) sOpenFlags = SQLITE_OPEN_READONLY;
      }
    }
    rc = sqlite3_open_v2( dbFile, &dbh, sOpenFlags, NULL );
    if(rc){
      if(dbh){
        /* By some complete coincidence, S2X_RC_DB==SQLITE_CANTOPEN. */
        rc = s2_error_set(db->se, &db->error, S2X_RC_DB,
                           "Opening db file [%s] failed with "
                           "sqlite code #%d: %s",
                           dbFile, rc, sqlite3_errmsg(dbh));
      }else{
        rc = s2_error_set(db->se, &db->error, S2X_RC_DB,
                           "Opening db file [%s] failed with "
                           "sqlite code #%d",
                           dbFile, rc);
      }
      /* MARKER(("Error msg: %s\n", (char const *)db->error.msg.mem)); */
    }else{
      assert(!db->filename);
      if(!*dbFile || ':'==*dbFile){
        /* assume "" or ":memory:" or some such: don't canonicalize it,
           but copy it nonetheless for consistency. */
        db->filename = s2x_strdup(db->se->e, dbFile);
      }else{
#if 1
        db->filename = s2x_strdup(db->se->e, dbFile);
        /* FIXME: missing path funcs from fossil... */
#else
        cwal_buffer tmp = cwal_buffer_empty;
        rc = s2x_file_canonical_name(dbFile, &tmp, 0);
        if(!rc){
          db->filename = (char *)tmp.mem
            /* transfering ownership */;
        }else if(tmp.mem){
          cwal_buffer_clear(&tmp);
        }
#endif
      }
      if(rc){
        goto end;
      }else if(!db->filename){
        rc = CWAL_RC_OOM;
        goto end;
      }
    }
    assert(!db->beginCount);
    db->dbh = dbh;
    if(S2X_SQ3_OPEN_F_TRACE_SQL & openFlags){
      s2x_sq3_sqltrace_enable(db, stdout);
    }
    if(db->se){
      /* FIXME: check result codes here. */
      sqlite3_commit_hook(dbh, s2x_sq3_verify_begin_was_not_called, db);
      s2x_sq3_busy_timeout(db, 3000);
#if 0
      sqlite3_wal_autocheckpoint(dbh, 1);  /* Set to checkpoint frequently */
      sqlite3_exec(dbh, "PRAGMA foreign_keys=OFF;", 0, 0, 0);
      sqlite3_create_function(dbh, "now", 0, SQLITE_ANY, 0,
                              s2x_sq3_now_udf, 0, 0);
#endif
    }/*if(db->se)*/
  }
  end:
  if(rc){
#if 1
    /* This is arguable... */
    if(db->se && db->error.code && !db->se->e->err.code){
      /* COPY db's error state as f's. */
      cwal_error_copy(db->se->e, &db->error, &db->se->e->err );
    }
#endif
    if(dbh){
      sqlite3_close(dbh);
      db->dbh = NULL;
    }
  }else{
    assert(db->dbh);
  }
  return rc;
}

int s2x_sq3_exec_multiv( s2x_sq3 * db, const char * sql, va_list args){
  if(!db || !db->dbh || !sql) return CWAL_RC_MISUSE;
  else{
    cwal_buffer buf = cwal_buffer_empty;
    int rc = 0;
    char const * z;
    char const * zEnd = NULL;
    rc = cwal_buffer_printfv( db->se->e, &buf, sql, args );
    if(rc){
      cwal_buffer_clear(db->se->e, &buf);
      return rc;
    }
    z = s2x_buffer_cstr(&buf);
    while( (SQLITE_OK==rc) && *z ){
      s2x_stmt_t * pStmt = NULL;
      rc = sqlite3_prepare_v2(db->dbh, z, buf.used, &pStmt, &zEnd);
      if( SQLITE_OK != rc ){
        rc = s2x_err_from_db(db, rc);
        break;
      }
      if(pStmt){
        while( SQLITE_ROW == sqlite3_step(pStmt) ){}
        rc = sqlite3_finalize(pStmt);
        if(rc) rc = s2x_err_from_db(db, rc);
      }
      buf.used -= (zEnd-z);
      z = zEnd;
    }
    cwal_buffer_clear(db->se->e, &buf);
    return rc;
  }
}

int s2x_sq3_exec_multi( s2x_sq3 * db, const char * sql, ...){
  if(!db || !db->dbh || !sql) return CWAL_RC_MISUSE;
  else{
    int rc;
    va_list args;
    va_start(args,sql);
    rc = s2x_sq3_exec_multiv( db, sql, args );
    va_end(args);
    return rc;
  }
}

int s2x_sq3_execv( s2x_sq3 * db, const char * sql, va_list args){
  if(!db || !db->dbh || !sql) return CWAL_RC_MISUSE;
  else{
    s2x_stmt st = s2x_stmt_empty;
    int rc = 0;
    rc = s2x_sq3_preparev( db, &st, sql, args );
    if(rc) return rc;
    /* rc = s2x_stmt_step( &st ); */
    while(S2X_RC_STEP_ROW == (rc=s2x_stmt_step(&st))){}
    s2x_stmt_finalize(&st);
    return (S2X_RC_STEP_ERROR==rc)
      ? S2X_RC_DB
      : 0;
  }
}

int s2x_sq3_exec( s2x_sq3 * db, const char * sql, ...){
  if(!db || !db->dbh || !sql) return CWAL_RC_MISUSE;
  else{
    int rc;
    va_list args;
    va_start(args,sql);
    rc = s2x_sq3_execv( db, sql, args );
    va_end(args);
    return rc;
  }
}

int s2x_sq3_changes_recent(s2x_sq3 * db){
  return (db && db->dbh)
    ? sqlite3_changes(db->dbh)
    : 0;
}

int s2x_sq3_changes_total(s2x_sq3 * db){
  return (db && db->dbh)
    ? sqlite3_total_changes(db->dbh)
    : 0;
}

/**
    Sets db->priorChanges to sqlite3_total_changes(db->dbh).
 */
static void s2x_sq3_reset_change_count(s2x_sq3 * db){
  db->priorChanges = sqlite3_total_changes(db->dbh);
}

int s2x_sq3_transaction_begin(s2x_sq3 * db){
  if(!db || !db->dbh) return CWAL_RC_MISUSE;
  else {
    int rc = (0==db->beginCount)
      ? s2x_sq3_exec(db,"BEGIN TRANSACTION")
      : 0;
    if(!rc){
      if(1 == ++db->beginCount){
        s2x_sq3_reset_change_count(db);
      }
    }
    return rc;
  }
}

int s2x_sq3_db_transaction_level(s2x_sq3 const * db){
  return db->beginCount;
}

int s2x_sq3_transaction_commit(s2x_sq3 * db){
  return (db && db->dbh)
    ? s2x_sq3_transaction_end(db, 0)
    : CWAL_RC_MISUSE;
}

int s2x_sq3_transaction_rollback(s2x_sq3 * db){
  return (db && db->dbh)
    ? s2x_sq3_transaction_end(db, 1)
    : CWAL_RC_MISUSE;
}

int s2x_sq3_rollback_force( s2x_sq3 * db ){
  if(!db || !db->dbh) return CWAL_RC_MISUSE;
  else{
    int rc;
    db->beginCount = 0;
    s2x_sq3_cleanup_beforeCommit(db);
    rc = s2x_sq3_exec(db, "ROLLBACK");
    s2x_sq3_reset_change_count(db);
    return rc;
  }
}

int s2x_sq3_transaction_end(s2x_sq3 * db, char doRollback){
  int rc = 0;
  if(!db || !db->dbh) return CWAL_RC_MISUSE;
  else if (db->beginCount<=0){
    return s2_error_set(db->se, &db->error, CWAL_RC_RANGE,
                        "No transaction is active.");
  }
  if(doRollback) ++db->doRollback
    /* ACHTUNG: note that db->dbRollback is set before continuing so
       that if we return due to a non-0 beginCount then the rollback
       flag propagates through the transaction's stack.
    */
    ;
  if(--db->beginCount > 0) return 0;
  assert(0==db->beginCount && "The commit-hook check relies on this.");
  assert(db->doRollback>=0);
  if((0==db->doRollback)
     && (db->priorChanges < sqlite3_total_changes(db->dbh))){
    /* Execute before-commit hooks and leaf checks */
    cwal_size_t x = 0;
    for( ; !rc && (x < db->beforeCommit.count); ++x ){
      char const * sql = (char const *)db->beforeCommit.list[x];
      /* MARKER(("Running before-commit code: [%s]\n", sql)); */
      if(sql) rc = s2x_sq3_exec_multi( db, "%s", sql );
    }
    db->doRollback = rc ? 1 : 0;
  }
  s2x_sq3_cleanup_beforeCommit(db);
  s2x_sq3_reset_change_count(db);
  rc = s2x_sq3_exec(db, db->doRollback ? "ROLLBACK" : "COMMIT");
  db->doRollback = 0;
  return rc;
}

int s2x_sq3_get_int32v( s2x_sq3 * db, int32_t * rv,
                       char const * sql, va_list args){
  /* Potential fixme: the s2x_sq3_get_XXX() funcs are 95%
     code duplicates. We "could" replace these with a macro
     or supermacro, though the latter would be problematic
     in the context of an amalgamation build.
  */
  if(!db || !db->dbh || !rv || !sql || !*sql) return CWAL_RC_MISUSE;
  else{
    s2x_stmt st = s2x_stmt_empty;
    int rc = 0;
    rc = s2x_sq3_preparev( db, &st, sql, args );
    if(rc) return rc;
    rc = s2x_stmt_step( &st );
    switch(rc){
      case S2X_RC_STEP_ROW:
        *rv = sqlite3_column_int(st.stmt, 0);
        CWAL_SWITCH_FALL_THROUGH;
      case S2X_RC_STEP_DONE:
        rc = 0;
        break;
      default:
        assert(S2X_RC_STEP_ERROR==rc);
        break;
    }
    s2x_stmt_finalize(&st);
    return rc;
  }
}

int s2x_sq3_get_int32( s2x_sq3 * db, int32_t * rv,
                      char const * sql,
                      ... ){
  int rc;
  va_list args;
  va_start(args,sql);
  rc = s2x_sq3_get_int32v(db, rv, sql, args);
  va_end(args);
  return rc;
}

int s2x_sq3_get_int64v( s2x_sq3 * db, int64_t * rv,
                       char const * sql, va_list args){
  if(!db || !db->dbh || !rv || !sql || !*sql) return CWAL_RC_MISUSE;
  else{
    s2x_stmt st = s2x_stmt_empty;
    int rc = 0;
    rc = s2x_sq3_preparev( db, &st, sql, args );
    if(rc) return rc;
    rc = s2x_stmt_step( &st );
    switch(rc){
      case S2X_RC_STEP_ROW:
        *rv = sqlite3_column_int64(st.stmt, 0);
        CWAL_SWITCH_FALL_THROUGH;
      case S2X_RC_STEP_DONE:
        rc = 0;
        break;
      default:
        assert(S2X_RC_STEP_ERROR==rc);
        break;
    }
    s2x_stmt_finalize(&st);
    return rc;
  }
}

int s2x_sq3_get_int64( s2x_sq3 * db, int64_t * rv,
                      char const * sql, ... ){
  int rc;
  va_list args;
  va_start(args,sql);
  rc = s2x_sq3_get_int64v(db, rv, sql, args);
  va_end(args);
  return rc;
}


int s2x_sq3_get_idv( s2x_sq3 * db, s2x_sq3_id_t * rv,
                       char const * sql, va_list args){
  if(!db || !db->dbh || !rv || !sql || !*sql) return CWAL_RC_MISUSE;
  else{
    s2x_stmt st = s2x_stmt_empty;
    int rc = 0;
    rc = s2x_sq3_preparev( db, &st, sql, args );
    if(rc) return rc;
    rc = s2x_stmt_step( &st );
    switch(rc){
      case S2X_RC_STEP_ROW:
        *rv = (s2x_sq3_id_t)sqlite3_column_int64(st.stmt, 0);
        CWAL_SWITCH_FALL_THROUGH;
      case S2X_RC_STEP_DONE:
        rc = 0;
        break;
      default:
        assert(S2X_RC_STEP_ERROR==rc);
        break;
    }
    s2x_stmt_finalize(&st);
    return rc;
  }
}

int s2x_sq3_get_id( s2x_sq3 * db, s2x_sq3_id_t * rv,
                      char const * sql, ... ){
  int rc;
  va_list args;
  va_start(args,sql);
  rc = s2x_sq3_get_idv(db, rv, sql, args);
  va_end(args);
  return rc;
}


int s2x_sq3_get_sizev( s2x_sq3 * db, cwal_size_t * rv,
                      char const * sql, va_list args){
  if(!db || !db->dbh || !rv || !sql || !*sql) return CWAL_RC_MISUSE;
  else{
    s2x_stmt st = s2x_stmt_empty;
    int rc = 0;
    rc = s2x_sq3_preparev( db, &st, sql, args );
    if(rc) return rc;
    rc = s2x_stmt_step( &st );
    switch(rc){
      case S2X_RC_STEP_ROW:{
        sqlite3_int64 const i = sqlite3_column_int64(st.stmt, 0);
        if(i<0){
          rc = CWAL_RC_RANGE;
          break;
        }
        *rv = (cwal_size_t)i;
        rc = 0;
        break;
      }
      case S2X_RC_STEP_DONE:
        rc = 0;
        break;
      default:
        assert(S2X_RC_STEP_ERROR==rc);
        break;
    }
    s2x_stmt_finalize(&st);
    return rc;
  }
}

int s2x_sq3_get_size( s2x_sq3 * db, cwal_size_t * rv,
                      char const * sql, ... ){
  int rc;
  va_list args;
  va_start(args,sql);
  rc = s2x_sq3_get_sizev(db, rv, sql, args);
  va_end(args);
  return rc;
}


int s2x_sq3_get_doublev( s2x_sq3 * db, double * rv,
                       char const * sql, va_list args){
  if(!db || !db->dbh || !rv || !sql || !*sql) return CWAL_RC_MISUSE;
  else{
    s2x_stmt st = s2x_stmt_empty;
    int rc = 0;
    rc = s2x_sq3_preparev( db, &st, sql, args );
    if(rc) return rc;
    rc = s2x_stmt_step( &st );
    switch(rc){
      case S2X_RC_STEP_ROW:
        *rv = sqlite3_column_double(st.stmt, 0);
        CWAL_SWITCH_FALL_THROUGH;
      case S2X_RC_STEP_DONE:
        rc = 0;
        break;
      default:
        assert(S2X_RC_STEP_ERROR==rc);
        break;
    }
    s2x_stmt_finalize(&st);
    return rc;
  }
}

int s2x_sq3_get_double( s2x_sq3 * db, double * rv,
                      char const * sql,
                      ... ){
  int rc;
  va_list args;
  va_start(args,sql);
  rc = s2x_sq3_get_doublev(db, rv, sql, args);
  va_end(args);
  return rc;
}


int s2x_sq3_get_textv( s2x_sq3 * db, char ** rv,
                      cwal_size_t *rvLen,
                      char const * sql, va_list args){
  if(!db || !db->dbh || !rv || !sql || !*sql) return CWAL_RC_MISUSE;
  else{
    s2x_stmt st = s2x_stmt_empty;
    int rc = 0;
    rc = s2x_sq3_preparev( db, &st, sql, args );
    if(rc) return rc;
    rc = s2x_stmt_step( &st );
    switch(rc){
      case S2X_RC_STEP_ROW:{
        char const * str = (char const *)sqlite3_column_text(st.stmt, 0);
        int const len = sqlite3_column_bytes(st.stmt,0);
        if(!str){
          *rv = NULL;
          if(rvLen) *rvLen = 0;
        }else{
          char * x = s2x_strndup(db->se->e, str, len);
          if(!x){
            rc = CWAL_RC_OOM;
          }else{
            *rv = x;
            if(rvLen) *rvLen = (cwal_size_t)len;
            rc = 0;
          }
        }
        break;
      }
      case S2X_RC_STEP_DONE:
        *rv = NULL;
        if(rvLen) *rvLen = 0;
        rc = 0;
        break;
      default:
        assert(S2X_RC_STEP_ERROR==rc);
        break;
    }
    s2x_stmt_finalize(&st);
    return rc;
  }
}

int s2x_sq3_get_text( s2x_sq3 * db, char ** rv,
                     cwal_size_t * rvLen,
                     char const * sql, ... ){
  int rc;
  va_list args;
  va_start(args,sql);
  rc = s2x_sq3_get_textv(db, rv, rvLen, sql, args);
  va_end(args);
  return rc;
}

int s2x_sq3_get_blobv( s2x_sq3 * db, void ** rv,
                      cwal_size_t *rvLen,
                      char const * sql, va_list args){
  if(!db || !db->dbh || !rv || !sql || !*sql) return CWAL_RC_MISUSE;
  else{
    s2x_stmt st = s2x_stmt_empty;
    int rc = 0;
    rc = s2x_sq3_preparev( db, &st, sql, args );
    if(rc) return rc;
    rc = s2x_stmt_step( &st );
    switch(rc){
      case S2X_RC_STEP_ROW:{
        cwal_buffer buf = cwal_buffer_empty;
        void const * str = sqlite3_column_blob(st.stmt, 0);
        int const len = sqlite3_column_bytes(st.stmt,0);
        if(!str){
          *rv = NULL;
          if(rvLen) *rvLen = 0;
        }else{
          rc = cwal_buffer_append(db->se->e, &buf, str, len);
          if(!rc){
            *rv = buf.mem;
            if(rvLen) *rvLen = buf.used;
          }
        }
        break;
      }
      case S2X_RC_STEP_DONE:
        *rv = NULL;
        if(rvLen) *rvLen = 0;
        rc = 0;
        break;
      default:
        assert(S2X_RC_STEP_ERROR==rc);
        break;
    }
    s2x_stmt_finalize(&st);
    return rc;
  }
}

int s2x_sq3_get_blob( s2x_sq3 * db, void ** rv,
                     cwal_size_t * rvLen,
                     char const * sql, ... ){
  int rc;
  va_list args;
  va_start(args,sql);
  rc = s2x_sq3_get_blobv(db, rv, rvLen, sql, args);
  va_end(args);
  return rc;
}

int s2x_sq3_get_bufferv( s2x_sq3 * db, cwal_buffer * b,
                        char asBlob, char const * sql,
                        va_list args){
  if(!db || !db->dbh || !b || !sql || !*sql) return CWAL_RC_MISUSE;
  else{
    s2x_stmt st = s2x_stmt_empty;
    int rc = 0;
    rc = s2x_sq3_preparev( db, &st, sql, args );
    if(rc) return rc;
    rc = s2x_stmt_step( &st );
    switch(rc){
      case S2X_RC_STEP_ROW:{
        void const * str = asBlob
          ? sqlite3_column_blob(st.stmt, 0)
          : (void const *)sqlite3_column_text(st.stmt, 0);
        int const len = sqlite3_column_bytes(st.stmt,0);
        rc = 0;
        b->used = 0;
        rc = cwal_buffer_append( db->se->e, b, str, len );
        break;
      }
      case S2X_RC_STEP_DONE:
        rc = 0;
        break;
      default:
        assert(S2X_RC_STEP_ERROR==rc);
        break;
    }
    s2x_stmt_finalize(&st);
    return rc;
  }
}

int s2x_sq3_get_buffer( s2x_sq3 * db, cwal_buffer * b,
                       char asBlob,
                       char const * sql, ... ){
  int rc;
  va_list args;
  va_start(args,sql);
  rc = s2x_sq3_get_bufferv(db, b, asBlob, sql, args);
  va_end(args);
  return rc;
}

int32_t s2x_sq3_g_int32( s2x_sq3 * db, int32_t dflt,
                            char const * sql,
                            ... ){
  int32_t rv = dflt;
  va_list args;
  va_start(args,sql);
  s2x_sq3_get_int32v(db, &rv, sql, args);
  va_end(args);
  return rv;
}

int64_t s2x_sq3_g_int64( s2x_sq3 * db, int64_t dflt,
                            char const * sql,
                            ... ){
  int64_t rv = dflt;
  va_list args;
  va_start(args,sql);
  s2x_sq3_get_int64v(db, &rv, sql, args);
  va_end(args);
  return rv;
}

s2x_sq3_id_t s2x_sq3_g_id( s2x_sq3 * db, s2x_sq3_id_t dflt,
                         char const * sql,
                         ... ){
  s2x_sq3_id_t rv = dflt;
  va_list args;
  va_start(args,sql);
  s2x_sq3_get_idv(db, &rv, sql, args);
  va_end(args);
  return rv;
}

cwal_size_t s2x_sq3_g_size( s2x_sq3 * db, cwal_size_t dflt,
                           char const * sql,
                           ... ){
  cwal_size_t rv = dflt;
  va_list args;
  va_start(args,sql);
  s2x_sq3_get_sizev(db, &rv, sql, args);
  va_end(args);
  return rv;
}

double s2x_sq3_g_double( s2x_sq3 * db, double dflt,
                              char const * sql,
                              ... ){
  double rv = dflt;
  va_list args;
  va_start(args,sql);
  s2x_sq3_get_doublev(db, &rv, sql, args);
  va_end(args);
  return rv;
}

char * s2x_sq3_g_text( s2x_sq3 * db, cwal_size_t * len,
                      char const * sql,
                      ... ){
  char * rv = NULL;
  va_list args;
  va_start(args,sql);
  s2x_sq3_get_textv(db, &rv, len, sql, args);
  va_end(args);
  return rv;
}

void * s2x_sq3_g_blob( s2x_sq3 * db, cwal_size_t * len,
                      char const * sql,
                      ... ){
  void * rv = NULL;
  va_list args;
  va_start(args,sql);
  s2x_sq3_get_blob(db, &rv, len, sql, args);
  va_end(args);
  return rv;
}

double s2x_sq3_julian_now(s2x_sq3 * db){
  double rc = -1.0;
  if(db && db->dbh){
    /* TODO? use cached statement? So far not used often enough to
       justify it. */
    s2x_sq3_get_double( db, &rc, "SELECT julianday('now')");
  }
  return rc;
}

double s2x_sq3_string_to_julian(s2x_sq3 * db, char const * str){
  double rc = -1.0;
  if(db && db->dbh){
    /* TODO? use cached statement? So far not used often enough to
       justify it. */
    s2x_sq3_get_double( db, &rc, "SELECT julianday(%Q)",str);
  }
  return rc;
}


char s2x_sq3_existsv(s2x_sq3 * db, char const * sql, va_list args ){
  if(!db || !db->dbh || !sql) return 0;
  else if(!*sql) return 0;
  else{
    s2x_stmt st = s2x_stmt_empty;
    char rv = 0;
    if(!s2x_sq3_preparev(db, &st, sql, args)){
      rv = S2X_RC_STEP_ROW==s2x_stmt_step(&st);
    }
    s2x_stmt_finalize(&st);
    return rv;
  }

}

char s2x_sq3_exists(s2x_sq3 * db, char const * sql, ... ){
  char rc;
  va_list args;
  va_start(args,sql);
  rc = s2x_sq3_existsv(db, sql, args);
  va_end(args);
  return rc;
}

/*
   Returns non-0 if the database (which must be open) table identified
   by zTableName has a column named zColName (case-sensitive), else
   returns 0.
*/
char s2x_sq3_table_has_column( s2x_sq3 * db, char const *zTableName, char const *zColName ){
  s2x_stmt q = s2x_stmt_empty;
  int rc = 0;
  char rv = 0;
  if(!db || !zTableName || !*zTableName || !zColName || !*zColName) return 0;
  rc = s2x_sq3_prepare(db, &q, "PRAGMA table_info(%Q)", zTableName );
  if(!rc) while(S2X_RC_STEP_ROW==s2x_stmt_step(&q)){
    /* Columns: (cid, name, type, notnull, dflt_value, pk) */
    cwal_size_t colLen = 0;
    char const * zCol = s2x_stmt_g_text(&q, 1, &colLen);
    if(0==s2x_strncmp(zColName, zCol, colLen)){
      rv = 1;
      break;
    }
  }
  s2x_stmt_finalize(&q);
  return rv;
}

char * s2x_sq3_random_hex(s2x_sq3 * db, cwal_size_t n){
  if(!db || !n) return NULL;
  else{
    cwal_size_t rvLen = 0;
    char * rv = s2x_sq3_g_text(db, &rvLen,
                              "SELECT lower(hex("
                              "randomblob(%"CWAL_SIZE_T_PFMT")))",
                              (cwal_size_t)(n/2+1));
    if(rv){
      assert(rvLen>=n);
      rv[n]=0;
    }
    return rv;
  }
}


int s2x_sq3_select_slistv( s2x_sq3 * db, cwal_list * tgt,
                          char const * fmt, va_list args ){
  if(!db || !tgt || !fmt) return CWAL_RC_MISUSE;
  else if(!*fmt) return CWAL_RC_RANGE;
  else{
    int rc;
    s2x_stmt st = s2x_stmt_empty;
    cwal_size_t nlen;
    char const * n;
    char * cp;
    rc = s2x_sq3_preparev(db, &st, fmt, args);
    while( !rc && (S2X_RC_STEP_ROW==s2x_stmt_step(&st)) ){
      nlen = 0;
      n = s2x_stmt_g_text(&st, 0, &nlen);
      cp = n ? s2x_strndup(db->se->e, n, (cwal_int_t)nlen) : NULL;
      if(n && !cp) rc = CWAL_RC_OOM;
      else{
        rc = cwal_list_append(db->se->e, tgt, cp);
        if(rc && cp) cwal_free(db->se->e, cp);
      }
    }
    s2x_stmt_finalize(&st);
    return rc;
  }
}

int s2x_sq3_select_slist( s2x_sq3 * db, cwal_list * tgt,
                         char const * fmt, ... ){
  int rc;
  va_list va;
  va_start (va,fmt);
  rc = s2x_sq3_select_slistv(db, tgt, fmt, va);
  va_end(va);
  return rc;
}

void s2x_sq3_sqltrace_enable( s2x_sq3 * db, FILE * outStream ){
  if(db && db->dbh){
    if(outStream){
      sqlite3_trace_v2(db->dbh, SQLITE_TRACE_STMT,
                       s2x_db_sql_trace_v2, outStream);
    }else{
      sqlite3_trace_v2(db->dbh, 0, 0, 0);
    }
  }
}

int s2x_sq3_init( s2_engine * se,
                  cwal_error * err,
                  char const * zFilename,
                  char const * zSchema,
                  ... ){
  s2x_sq3 DB = s2x_sq3_empty;
  s2x_sq3 * db = &DB;
  char const * zSql;
  int rc;
  char inTrans = 0;
  va_list ap;
  rc = s2x_sq3_open(se, db, zFilename, 0);
  if(rc) goto end;
  rc = s2x_sq3_exec(db, "BEGIN EXCLUSIVE");
  if(rc) goto end;
  inTrans = 1;
  if(zSchema && *zSchema){
    rc = s2x_sq3_exec_multi(db, "%s", zSchema);
    if(rc) goto end;
    va_start(ap, zSchema);
    while( !rc && (zSql = va_arg(ap, const char*))!=NULL ){
      rc = s2x_sq3_exec_multi(db, "%s", zSql);
    }
    va_end(ap);
  }
  end:
  if(rc){
    if(inTrans) s2x_sq3_exec(db, "ROLLBACK");
  }else{
    if(inTrans) rc = s2x_sq3_exec(db, "COMMIT");
  }
  if(err){
    if(db->error.code){
      cwal_error_uplift(&db->error, err);
    }else if(rc){
      err->code = rc;
      err->msg.used = 0;
    }
  }
  s2x_sq3_close(db);
  return rc;
}

int s2x_stmt_each_f_dump( s2x_stmt * stmt, void * state ){
  int i;
  s2_engine * se = (stmt && stmt->db) ? stmt->db->se : NULL;
  char const * sep = "\t";
  if(!se){
    if(state){/*avoid unused param warning*/}
    return CWAL_RC_MISUSE;
  }
  if(1==stmt->rowCount){
    for( i = 0; i < stmt->colCount; ++i ){
      cwal_outputf(se->e, "%s%s", s2x_stmt_col_name(stmt, i),
            (i==stmt->colCount-1) ? "" : sep);
    }
    cwal_output(se->e, "\n", 1);
  }
  for( i = 0; i < stmt->colCount; ++i ){
    char const * val = s2x_stmt_g_text(stmt, i, NULL);
    cwal_outputf(se->e, "%s%s", val ? val : "NULL",
                 (i==stmt->colCount-1) ? "" : sep);
  }
  cwal_output(se->e, "\n", 1);
  return 0;
}

int s2x_sq3_busy_timeout(s2x_sq3 * db, int ms){
  int const rc = sqlite3_busy_timeout( db->dbh, ms );
  switch(rc){
    case 0: return rc;
    case SQLITE_NOMEM: return CWAL_RC_OOM;
    default: return S2X_RC_DB;
  }
}
#define MY_TYPEID(X) (S2X_SQ3_TYPE_IDS.X)

int s2x_sq3_extract_cwal( cwal_value * x, cwal_native ** _n, cwal_value **_vdb, s2x_sq3 ** _db ){
  cwal_native * nat = 0;
  nat = cwal_value_native_part(cwal_value_engine(x), x, MY_TYPEID(sq3));
  if(!nat) return CWAL_RC_TYPE;
  else if(_db || _vdb){
    s2x_sq3 * db = 0;
    cwal_value * vdb = cwal_native_value(nat);
    db = vdb ? (s2x_sq3*)cwal_native_get(nat, MY_TYPEID(sq3)) : 0;
    if(!db) return CWAL_RC_TYPE;
    if(_db) *_db = db;
    if(_vdb) *_vdb = vdb;
  }
  if(_n) *_n=nat;
  return 0;
}

int s2x_stmt_extract_cwal( cwal_value * x, cwal_native ** _n, cwal_value **_vst, s2x_stmt ** _st ){
  cwal_native * nat = 0;
  nat = cwal_value_native_part(cwal_value_engine(x), x, MY_TYPEID(stmt));
  if(!nat) return CWAL_RC_TYPE;
  else if(_st || _vst){
    s2x_stmt * stmt = 0;
    cwal_value * vst = cwal_native_value(nat);
    stmt = vst ? (s2x_stmt*)cwal_native_get(nat, MY_TYPEID(stmt)) : 0;
    if(!stmt) return CWAL_RC_TYPE;
    if(_st) *_st = stmt;
    if(_vst) *_vst = vst;
  }
  if(_n) *_n=nat;
  return 0;
}

int s2x_sq3_value_to_cwal( cwal_engine * e, sqlite3_value * sqv, cwal_value ** rv ){
  int const vtype = sqlite3_value_type(sqv);
  switch( vtype ){
    case SQLITE_NULL:
      *rv = cwal_value_null();
      break;
    case SQLITE_INTEGER:
      *rv = cwal_new_integer( e,
                              (cwal_int_t)sqlite3_value_int64(sqv) );
      break;
    case SQLITE_FLOAT:
      *rv = cwal_new_double( e,
                             (cwal_double_t)sqlite3_value_double(sqv) );
      break;
    case SQLITE_BLOB: {
      int rc;
      cwal_size_t slen = 0;
      void const * bl = 0;
      bl = sqlite3_value_blob(sqv);
      slen = (cwal_size_t)sqlite3_value_bytes(sqv);
      if(!bl){
        *rv = cwal_value_null();
      }else{
        cwal_buffer * buf = cwal_new_buffer(e, slen+1U);
        if(!buf) return CWAL_RC_OOM;
        rc = cwal_buffer_append( e, buf, bl, (cwal_size_t)slen );
        if(rc){
          assert(!*rv);
          cwal_value_unref(cwal_buffer_value(buf));
        }else{
          *rv = cwal_buffer_value(buf);
        }
      }
      break;
    }
    case SQLITE_TEXT: {
      cwal_size_t slen = 0;
      char const * str = 0;
      str = (char const *)sqlite3_value_text( sqv );
      slen = (cwal_size_t)sqlite3_value_bytes(sqv);
      if(!str){
        *rv = cwal_value_null();
      }else{
        *rv = cwal_new_string_value(e, str, slen);
      }
      break;
    }
    default:
      return CWAL_RC_TYPE;
  }
  return *rv ? 0 : CWAL_RC_OOM;
}


/**
  Calls one of the sqlite3_result_XXX() variants, depending on the
  data type of v (which may be NULL).
*/
void s2x_sq3_result( sqlite3_context * context, cwal_value * v ){
  cwal_type_id const vtype = v ? cwal_value_type_id(v) : CWAL_TYPE_NULL;
  switch( vtype ){
    case CWAL_TYPE_NULL:
    case CWAL_TYPE_UNDEF:
      sqlite3_result_null(context);
      break;
    case CWAL_TYPE_BOOL:
      sqlite3_result_int(context, (int)cwal_value_get_bool(v));
      break;
    case CWAL_TYPE_INTEGER:
      /* We have no way of knowing which type (32/64-bit) to bind
         here, so we'll guess. We could check the range, i guess,
         but for sqlite it makes little or no difference, anyway.
      */
      sqlite3_result_int64(context, cwal_value_get_integer(v));
      break;
    case CWAL_TYPE_DOUBLE:
      sqlite3_result_double(context, cwal_value_get_double(v));
      break;
    case CWAL_TYPE_BUFFER:
    case CWAL_TYPE_STRING: {
      cwal_size_t slen = 0;
      char const * cstr = cwal_value_get_cstr(v, &slen);
      if(!cstr){
        sqlite3_result_null(context);
        /* Will only apply to empty buffers (Strings are never
           NULL). But it's also possible that a buffer with
           length 0 has a non-NULL memory buffer. So we cannot,
           without further type inspection, clearly
           differentiate between a NULL and empty BLOB
           here. This distinction would seem to be (?) 
           unimportant for this particular use case, so
           fixing/improving it can wait.
        */
      }
      else if(CWAL_TYPE_BUFFER==vtype){
        sqlite3_result_blob(context, cstr, (int)slen, SQLITE_TRANSIENT);
      }
      else{
        sqlite3_result_text(context, cstr, (int)slen, SQLITE_TRANSIENT);
      }
      break;
    }
    default:{
      sqlite3_result_null(context);
      /*MARKER(("ERROR(?): unhandled cwal-to-sqlite result conversion type: %s\n",
        cwal_type_id_name(vtype)));*/
    }
  }
}

int s2x_stmt_bind( s2x_stmt * st, int ndx, cwal_value * v ){
  int rc;
  int const vtype = v ? cwal_value_type_id(v) : CWAL_TYPE_NULL;
  cwal_engine * e = st->db->se->e;
  if(ndx<1) {
    return cwal_exception_setf(e, CWAL_RC_RANGE,
                               "Bind index %d is invalid: indexes are 1-based.",
                               ndx);
  }
  else if(ndx > st->paramCount) {
    return cwal_exception_setf(e, CWAL_RC_RANGE,
                               "Bind index %d is out of range. Range=(1..%d).",
                               ndx, st->paramCount);
  }
  /*MARKER(("Binding %s to column #%u\n", cwal_value_type_name(v), ndx));*/
  switch( vtype ){
    case CWAL_TYPE_NULL:
    case CWAL_TYPE_UNDEF:
      rc = s2x_stmt_bind_null(st, ndx);
      break;
    case CWAL_TYPE_BOOL:
      rc = s2x_stmt_bind_int32(st, ndx, cwal_value_get_bool(v));
      break;
    case CWAL_TYPE_INTEGER:
      /* We have no way of knowing which type (32/64-bit) to bind
         here, so we'll guess. We could check the range, i guess,
         but for sqlite it makes little or no difference, anyway.
      */
      rc = s2x_stmt_bind_int64(st, ndx,
                               (int64_t)cwal_value_get_integer(v));
      break;
    case CWAL_TYPE_DOUBLE:
      rc = s2x_stmt_bind_double(st, ndx, (double)cwal_value_get_double(v));
      break;
    case CWAL_TYPE_BUFFER:
    case CWAL_TYPE_STRING: {
      cwal_size_t slen = 0;
      char const * cstr = cwal_value_get_cstr(v, &slen);
      if(!cstr){
        /* Will only apply to empty buffers (Strings are never
           NULL). But it's also possible that a buffer with
           length 0 has a non-NULL memory buffer. So we cannot,
           without further type inspection, clearly
           differentiate between a NULL and empty BLOB
           here. This distinction would seem to be (?) 
           unimportant for this particular use case, so
           fixing/improving it can wait.
        */
        rc = s2x_stmt_bind_null(st, ndx);
      }
      else if(CWAL_TYPE_BUFFER==vtype){
        rc = s2x_stmt_bind_blob(st, ndx, cstr, (int)slen, 1);
      }
      else{
        rc = s2x_stmt_bind_text(st, ndx, cstr, (int)slen, 1);
      }
      break;
    }
    default:
      return cwal_exception_setf(e, CWAL_RC_TYPE,
                                 "Unhandled data type (%s) for binding "
                                 "column %d.",
                                 cwal_value_type_name(v), ndx);
  }
  if(rc){
    cwal_size_t msgLen = 0;
    char const * msg = NULL;
    int const dbRc = s2x_sq3_err_get(st->db, &msg, &msgLen);
    rc = cwal_exception_setf(e, S2X_RC_DB,
                             "Binding column %d failed with "
                             "sqlite code #%d: %.*s", ndx,
                             dbRc, (int)msgLen, msg);
  }
  return rc;
}

/**
   Property visitor helper for binding parameters by name.
*/
static int cwal_kvp_visitor_f_bind_by_name( cwal_kvp const * kvp, void * state ){
  cwal_size_t keyLen = 0;
  cwal_value const * v = cwal_kvp_key(kvp);
  char const * key = cwal_value_get_cstr(v, &keyLen);
  s2x_stmt * stmt = (s2x_stmt *)state;
  int ndx;
  if(!key){
    if(!cwal_value_is_integer(v)){
      return 0 /* non-string property. */;
    }else{
      ndx = (int)cwal_value_get_integer(v);
    }
  }else{
    ndx = s2x_stmt_param_index(stmt, key);
    if(ndx<=0){
      return cwal_exception_setf(stmt->db->se->e, CWAL_RC_RANGE, "Parameter name '%.*s' "
                                 "does not resolve to an index. (Maybe missing "
                                 "the leading ':' or '$' part of the param name?)",
                                 (int)keyLen, key);
    }
  }
  return s2x_stmt_bind(stmt, ndx, cwal_kvp_value(kvp));
}

int s2x_stmt_bind2(s2x_stmt * st, int ndx, cwal_value * bind){
  int rc = 0;
  if(cwal_value_undefined() == bind) rc = 0;
  else if(cwal_value_is_array(bind)){
    rc = s2x_stmt_bind_values_a(st, cwal_value_get_array(bind));
  }else if(cwal_value_is_tuple(bind)){
    rc = s2x_stmt_bind_values_t(st, cwal_value_get_tuple(bind));
  }else if(cwal_value_is_buffer(bind)){
    rc = s2x_stmt_bind(st, ndx, bind);
  }else if(cwal_props_can(bind)){
    rc = cwal_props_visit_kvp(bind, cwal_kvp_visitor_f_bind_by_name, st);
  }else{
    rc = s2x_stmt_bind(st, ndx, bind);
  }
  return rc;
}

int s2x_stmt_bind_values_a(s2x_stmt * st, cwal_array const * src){
  int const n = (int)cwal_array_length_get(src);
  int i = 0;
  int rc = 0;
  for( ; !rc && (i < n); ++i ){
    rc = s2x_stmt_bind(st, i+1, cwal_array_get(src,(cwal_size_t)i) );
  }
  return rc;
}

int s2x_stmt_bind_values_t(s2x_stmt * st, cwal_tuple const * src){
  int const n = (int)cwal_tuple_length(src);
  int i = 0;
  int rc = 0;
  for( ; !rc && (i < n); ++i ){
    rc = s2x_stmt_bind(st, i+1, cwal_tuple_get(src,(uint16_t)i) );
  }
  return rc;
}

int s2x_stmt_to_value(s2x_stmt * st, int ndx, cwal_value ** rv ){
  int vtype = sqlite3_column_type(st->stmt, ndx);
  cwal_engine * e = st->db->se->e;
  assert(!*rv);
  switch( vtype ){
    case SQLITE_NULL:
      *rv = cwal_value_null();
      return 0;
    case SQLITE_INTEGER:
      *rv = cwal_new_integer( e,
                              (cwal_int_t)s2x_stmt_g_int64(st,ndx) );
      break;
    case SQLITE_FLOAT:
      *rv = cwal_new_double( e,
                             (cwal_double_t)s2x_stmt_g_double(st, ndx) );
      break;
    case SQLITE_BLOB: {
      int rc;
      cwal_size_t slen = 0;
      void const * bl = 0;
      rc = s2x_stmt_get_blob(st, ndx, &bl, &slen);
      if(rc){
        return rc;
      }else if(!bl){
        *rv = cwal_value_null();
      }else{
        cwal_value * bv;
        cwal_buffer * buf = cwal_new_buffer(e, (cwal_size_t)(slen+1));
        if(!buf) return CWAL_RC_OOM;
        bv = cwal_buffer_value(buf);
        cwal_value_ref(bv);
        rc = cwal_buffer_append( e, buf, bl, (cwal_size_t)slen );
        if(rc){
          assert(!*rv);
          cwal_value_unref(bv);
        }else{
          cwal_value_unhand(bv);
          *rv = bv;
        }
      }
      break;
    }
    case SQLITE_TEXT: {
      cwal_size_t slen = 0;
      char const * str = 0;
      str = s2x_stmt_g_text( st, ndx, &slen );
      *rv = cwal_new_string_value(e, str, (cwal_size_t) slen);
      break;
    }
    default:
      return cwal_exception_setf(e, CWAL_RC_TYPE,
                                 "Unknown db column type (%d).",
                                 vtype);
  }
  return *rv ? 0 : CWAL_RC_OOM;
}

cwal_value * s2x_stmt_row_to_object2( s2x_stmt * st, cwal_array const * colNames ){
  int colCount;
  cwal_value * objV;
  cwal_object * obj;
  cwal_value * colName;
  int i;
  int rc;
  cwal_engine * const e = st->db->se->e;
  if( ! st ) return NULL;
  colCount = st->colCount;
  if( !colCount || (colCount>(int)cwal_array_length_get(colNames)) ) {
    return NULL;
  }
  obj = cwal_new_object(e);
  if( ! obj ) return NULL;
  objV = cwal_object_value( obj );
  cwal_value_prototype_set(objV, NULL);
  for( i = 0; i < colCount; ++i ){
    cwal_value * v = NULL;
    colName = cwal_array_get( colNames, i );
    if( ! colName ) goto error;
    rc = s2x_stmt_to_value( st, i, &v );
    if( rc ) goto error;
    rc = cwal_prop_set_v( objV, colName, v );
    if( rc ){
      cwal_value_unref(v);
      goto error;
    }
  }
  return objV;
  error:
  cwal_value_unref( objV );
  return NULL;
}

cwal_value * s2x_stmt_row_to_object( s2x_stmt * st ){
  cwal_value * objV;
  cwal_object * obj;
  char const * colName;
  int i;
  int rc;
  cwal_engine * const e = st ? st->db->se->e : NULL;
  if( ! st || !st->colCount ) return NULL;
  obj = cwal_new_object(e);
  if( ! obj ) return NULL;
  objV = cwal_object_value(obj);
  cwal_value_prototype_set(objV, NULL);
  for( i = 0; i < st->colCount; ++i ){
    cwal_value * v = NULL;
    colName = s2x_stmt_col_name(st, i);
    if( ! colName ) goto error;
    rc = s2x_stmt_to_value( st, i, &v );
    if( rc ) goto error;
    rc = cwal_prop_set( objV, colName,
                        cwal_strlen(colName), v );
    if( rc ){
      cwal_value_unref(v);
      goto error;
    }
  }
  return objV;
  error:
  cwal_value_unref( objV );
  return NULL;
}

int s2x_stmt_row_to_array2( s2x_stmt * st, cwal_array * ar)
{
  int i;
  int rc = 0;
  for( i = 0; i < st->colCount; ++i ){
    cwal_value * v = NULL;
    rc = s2x_stmt_to_value( st, i, &v );
    if( rc ) break;
    cwal_value_ref(v);
    rc = cwal_array_append( ar, v );
    cwal_value_unref(v);
    if( rc ) break;
  }
  return rc;
}

cwal_value * s2x_stmt_row_to_array( s2x_stmt * st )
{
  cwal_array * ar;
  int rc = 0;
  cwal_value * av;
  cwal_engine * const e = st ? st->db->se->e : NULL;
  if( ! st || !st->colCount ) return NULL;
  ar = cwal_new_array(e);
  if( ! ar ) return NULL;
  av = cwal_array_value(ar);
  cwal_value_ref(av);
  rc = s2x_stmt_row_to_array2( st, ar );
  if(rc){
    cwal_value_unref(av);
    av = 0;
  }else{
    cwal_value_unhand(av);
  }
  return av;
}


cwal_value * s2x_stmt_row_to_tuple( s2x_stmt * st ){
  cwal_engine * const e = st ? st->db->se->e : NULL;
  cwal_tuple * tp = cwal_new_tuple(e, (uint16_t)st->colCount);
  int rc = 0;
  int i;
  cwal_value * tv = tp ? cwal_tuple_value(tp) : 0;
  if(!tp) return 0;
  cwal_value_ref(tv);
  for( i = 0; i < st->colCount; ++i ){
    cwal_value * v = NULL;
    rc = s2x_stmt_to_value( st, i, &v );
    if( rc ) break;
    cwal_value_ref(v);
    rc = cwal_tuple_set( tp, (uint16_t)i, v );
    cwal_value_unref(v);
    if( rc ) break;
  }
  if(rc){
    cwal_value_unref(tv);
    tv = 0;
  }else{
    cwal_value_unhand(tv);
  }
  return tv;
}

int s2x_sq3_toss( s2x_sq3 * db ){
  int rc;
  assert(db && db->error.code);
  rc = s2_engine_err_has(db->se);
  if(!rc){
    rc = (CWAL_RC_OOM==db->error.code)
      ? CWAL_RC_OOM
      : s2_throw_err(db->se, &db->error, 0, 0, 0);
  }
  s2x_sq3_err_reset(db);
  return rc;
}


#undef MY_TYPEID
#undef MARKER
