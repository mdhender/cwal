/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
#if !defined(NET_WANDERINGHORSE_WHCL_DB_H_INCLUDED)
#define NET_WANDERINGHORSE_WHCL_DB_H_INCLUDED
/**
  Copyright (c) 2013 D. Richard Hipp (https://www.hwaci.com/drh/)
  Copyright (c) 2016-2020 Stephan Beal (sgbeal@googlemail.com)

  This program is free software; you can redistribute it and/or
  modify it under the terms of the Simplified BSD License (also
  known as the "2-Clause License" or "FreeBSD License".)

  This program is distributed in the hope that it will be useful,
  but without any warranty; without even the implied warranty of
  merchantability or fitness for a particular purpose.

  ******************************************************************************

  This file is a port of libfossil's sqlite3 db layer to s2/cwal, for
  use as an s2 loadable module. Lots of the docs likely refer to
  fossil - ignore them.
*/

#include "libcwal.h" /* MUST come first b/c of config macros */
/*
  We don't _really_ want to include sqlite3.h at this point, but if we
  do not then we have to typedef the sqlite3 struct here and that
  breaks when client code includes both this file and sqlite3.h.
*/
#include "sqlite3.h"

#if defined(__cplusplus)
extern "C" {
#endif

typedef cwal_int_t whx_sq3_id_t;
typedef int64_t whx_sq3_time_t;

/**
   Potential TODO. Maybe not needed - v1 uses only(?) 1 hook and we
   can do that w/o hooks.
*/
typedef int (*whx_commit_hook_f)( void * state );
/** Potential TODO */
struct whx_commit_hook {
  whx_commit_hook_f hook;
  int sequence;
  void * state;
};
#define whx_commit_hook_empty_m {NULL,0,NULL}
typedef struct whx_commit_hook whx_commit_hook;
/* extern const whx_commit_hook whx_commit_hook_empty; */

/**
   Result codes used by some of the DB API.
*/
enum {
  WHX_RC_STEP_ROW = CWAL_RC_CLIENT_BEGIN2 + 1,
  WHX_RC_STEP_DONE,
  WHX_RC_STEP_ERROR,
  /** Generic cwal-compatible code for db errors. */
  WHX_RC_DB
};

/**
   Flags for use with whx_sq3_open() and friends.
*/
enum whx_sq3_open_flags {
/**
   The "no flags" value.
*/
WHX_SQ3_OPEN_F_NONE = 0,
/**
   Flag for whx_sq3_open() specifying that the db should be opened
   in read-only mode.
*/
WHX_SQ3_OPEN_F_RO = 0x01,
/**
   Flag for whx_sq3_open() specifying that the db should be opened
   in read-write mode, but should not create the db if it does
   not already exist.
*/
WHX_SQ3_OPEN_F_RW = 0x02,
/**
   Flag for whx_sq3_open() specifying that the db should be opened in
   read-write mode, creating the db if it does not already exist.
*/
WHX_SQ3_OPEN_F_CREATE = 0x04,
/**
   Shorthand for RW+CREATE flags.
*/
WHX_SQ3_OPEN_F_RWC = WHX_SQ3_OPEN_F_RW | WHX_SQ3_OPEN_F_CREATE,

/**
   Flag for whx_sq3_open() specifying that the db should enable SQL
   tracing to stdout. This logs all underlying DB operations and
   is often helpful for debugging.
*/
WHX_SQ3_OPEN_F_TRACE_SQL = 0x10
};



#if 0
/* We can't do this because it breaks when clients include both
   this header and sqlite3.h. Is there a solution which lets us
   _not_ include sqlite3.h from this file and also compiles when
   clients include both?
*/
#if !defined(SQLITE_OK)
/**
   Placeholder for sqlite3/4 type. We currently use v3 but will
   almost certainly switch to v4 at some point. Before we can do
   that we need an upgrade/migration path.
*/
typedef struct sqlite3 sqlite3;
#endif
#endif

/**
   A level of indirection to "hide" the actual db driver
   implementation from the public API. Whether or not the API
   uses/will use sqlite3 or 4 is "officially unspecified."  We
   currently use 3 because (A) it bootstraps development and
   testing by letting us use existing fossil repos for and (B) it
   reduces the number of potential problems when porting SQL-heavy
   code from the v1 tree. Clients should try not to rely on the
   underlying db driver API, but may need it for some uses
   (e.g. binding custom SQL functions).
*/
typedef sqlite3 whx_dbh_t;

typedef struct whx_sq3 whx_sq3;
typedef struct whx_stmt whx_stmt;

/**
   Db handle wrapper class. Each instance wraps a single sqlite
   database handle.

   This API is built upon sqlite3, but this abstraction is intended to
   hide that, insofar as possible, from clients so as to simplify an
   eventual port from v3 to v4. Clients should avoid relying on the
   underlying db being sqlite (or at least not rely on a specific
   version), but may want to register custom functions with the driver
   (or perform similar low-level operations) and the option is left
   open for them to access that handle via the whx_sq3::dbh member.

   FIXME: this class needs to keep a handle of all opened whx_stmt
   handles and finalize them if the db is closed before the handle.
   There are certain cases in script code (most notably triggered by
   fatal script-side errors) in which the db gets closed before any
   statements, which leaves them holding a stale DB handle, leading to
   undefined results when they are cleaned up (e.g. GC'd during s2
   stack unwinding).

   @see whx_sq3_open();
   @see whx_sq3_close();
   @see whx_stmt
*/
struct whx_sq3 {

  /**
     Underlying db driver handle.
  */
  whx_dbh_t * dbh;

  /**
     This is who manages all of this API's memory (except that which
     is managed internally by sqlite3).
  */
  cwal_engine * ec;

  /**
     Holds the file name used when opening this db. Might not refer to
     a real file (e.g. might be ":memory:" or "" (similar to
     ":memory:" but may swap to temp storage).
  */
  char * filename;

  /**
     Debugging/test counter. Closing a db with opened statements
     might assert() or trigger debug output when the db is closed.
  */
  short openStatementCount;

  /**
     A counter which we use internally to help manage out-of-order
     finalization of dbs and their handles. If whx_sq3_close() is
     called while this->openStatementCount is >0 then deathCounter
     gets set to this->openStatementCount. When whx_stmt_finalize() is
     called and stmt->db->deathCounter is >0, the finalization process
     will decrement that counter and free the whx_sq3 handle when the
     counter reaches zero. This is all made possible by
     sqlite3_close_v2(), which exists for cases such as these.
  */
  short deathCounter;

  /**
     Counter for whx_sq3_transaction_begin/end().
  */
  short beginCount;

  /**
     Internal flag for communicating rollback state through the
     call stack. If this is set to a true value,
     whx_sq3_transaction_end() calls will behave like a rollback
     regardless of the value of the 2nd argument passed to that
     function. i.e. it propagates a rollback through nested
     transactions.

     Potential TODO: instead of treating this like a boolean, store
     the error number which caused the rollback here.  We'd have to
     go fix a lot of code for that, though :/.
  */
  short doRollback;

  /**
     Internal change counter. Set when a transaction is
     started/committed.

     Maintenance note: it's an int because that's what
     sqlite3_total_changes() returns.
  */
  int priorChanges;

  /**
     List of SQL commands (char *) which should be executed prior
     to a commit. This list is cleared when the transaction counter
     drops to zero as the result of whx_sq3_transaction_end()
     or whx_sq3_rollback_force().

     TODO? Use (whx_stmt*) objects instead of strings? Depends on
     how much data we need to bind here (want to avoid an extra
     copy if we need to bind big stuff). That was implemented in
     [9d9375ac2d], but that approach prohibits multi-statement
     pre-commit triggers, so it was not trunked. It's still unknown
     whether we need multi-statement SQL in this context
     (==fossil's infrastructure).

     @see whx_sq3_before_commit()
  */
  cwal_list beforeCommit;

  /**
     An internal cache of "static" queries - those which do not
     rely on call-time state unless that state can be
     bind()ed. Holds a linked list of (whx_stmt*) instances.

     @see whx_sq3_prepare_cached()
  */
  whx_stmt * cacheHead;

  /**
     An s2 lifetime management artifact. Do not use!
  */
  cwal_value * vSelf;
  
  /**
     A marker which tells whx_sq3_close() whether or not
     whx_sq3_malloc() allocated this instance (in which case
     whx_sq3_close() will cwal_free() it) or not (in which case it
     does not free() it).
  */
  void const * allocStamp;

  /**
     For internal use in the s2 bindings as a place to keep UDFs alive
     as long as the db (or until overwritten).
  */
  cwal_value * udfStore;
};
/**
   Empty-initialized whx_sq3 structure, intended for const-copy
   initialization.
*/
#define whx_sq3_empty_m {                        \
      NULL/*dbh*/,                              \
      NULL/*el*/,                             \
      NULL/*filename*/,                         \
      0/*openStatementCount*/,                  \
      0/*deathCounter*/,                       \
      0/*beginCount*/,                          \
      0/*doRollback*/,                          \
      0/*priorChanges*/,                        \
      cwal_list_empty_m/*beforeCommit*/,         \
      NULL/*cacheHead*/,                        \
      NULL/*vSelf*/,                   \
      NULL/*allocStamp*/,                     \
      NULL/*udfStore*/                        \
      }

/**
   Empty-initialized whx_sq3 structure, intended for copy
   initialization.
*/
extern const whx_sq3 whx_sq3_empty;

/**
   If db is not NULL then this function returns its name (the one
   used to open it). The bytes are valid until the db connection is
   closed or until someone mucks with db->filename. If len is not
   NULL then *len is (on success) assigned to the length of the
   returned string, in bytes.  The string is NUL-terminated, so
   fetching the length (by passing a non-NULL 2nd parameter) is
   optional but sometimes helps improve efficiency be removing
   the need for a downstream call to whx_strlen().

   Returns NULL if !f or f has no checkout opened.
*/
char const * whx_sq3_filename(whx_sq3 const * db, cwal_size_t * len);

typedef sqlite3_stmt whx_stmt_t;
/**
   Represents a prepared statement handle.
   Intended usage:

   @code
   whx_stmt st = whx_stmt_empty;
   int rc = whx_sq3_prepare( db, &st, "..." );
   if(rc){ // Error!
     assert(!st.stmt);
     // error details have been propagated via db->el
   }else{
     // use st and eventually finalize it:
     whx_stmt_finalize( &st );
   }
   @endcode


   Script binding implementations can largely avoid exposing the
   statement handle (and its related cleanup ordering requirements)
   to script code. They need to have some mechanism for binding
   values to SQL (or implement all the escaping themselves), but
   that can be done without exposing all of the statement class if
   desired. For example, here's some hypothetical script code:

   @code
   var st = db.prepare(".... where i=:i and x=:x");
   // st is-a Statement, but we need not add script bindings for
   // the whole Statement.bind() API. We can instead simplify that
   // to something like:
   try {
   st.exec( {i: 42, x: 3} )
   // or, for a SELECT query:
   st.each({
   bind{i:42, x:3},
   rowType: 'array', // or 'object'
   callback: function(row,state,colNames){ print(row.join('\t')); },
   state: {...callback function state...}
   });
   } finally {
   st.finalize();
   // It is critical that st gets finalized before its DB, and
   // that'shard to guaranty if we leave st to the garbage collector!
   }
   // see below for another (less messy) alternative
   @endcode

   Ideally, script code should not have direct access to the
   Statement because managing lifetimes can be difficult in the
   face of flow-control changes caused by exceptions (as the above
   example demonstrates). Statements can be completely hidden from
   clients if the DB wrapper is written to support it. For example,
   in pseudo-JavaScript that might look like:

   @code
   db.exec("...where i=? AND x=?", 42, 3);
   db.each({sql:"select ... where id<?", bind:[10],
   rowType: 'array', // or 'object'
   callback: function(row,state,colNames){ print(row.join('\t')); },
   state: {...arbitrary state for the callback...}
   });
   @endcode
*/
struct whx_stmt {
  /**
     Underlying db driver-level statement handle. Clients should
     not rely on the specify concrete type if they can avoid it, to
     simplify an eventual port from sqlite3 to sqlite4.
  */
  whx_stmt_t * stmt;

  /**
     The db which prepared this statement.
  */
  whx_sq3 * db;
  
  /**
     SQL used to prepare this statement.

     We can actually get rid of this because sqlite3 provides access
     to the SQL for each statement. We currently use it for the
     whx_sq3_db_prepare_cached() APIs, though, and refactoring that
     code has a low priority.
  */
  cwal_buffer sql;

  /**
     Number of result columns in this statement. Cached when the
     statement is prepared. Is a signed type because the underlying
     API does it this way.
  */
  int colCount;

  /**
     Number of bound parameter indexes in this statement. Cached
     when the statement is prepared. Is a signed type because the
     underlying API does it this way.
  */
  int paramCount;

  /**
     The number of times this statement has fetched a row via
     whx_stmt_step().
  */
  cwal_size_t rowCount;

  /**
     Internal state flags.
  */
  int flags;

  /**
     For _internal_ use in creating linked lists. Clients _must_not_
     modify this field.
   */
  whx_stmt * next;
  
  /**
     An s2 lifetime management artifact. Do not use!
  */
  cwal_value * vSelf;
  
  /**
     A marker which tells whx_stmt_finalize() whether or not
     whx_stmt_malloc() allocated this instance (in which case
     whx_stmt_finalize() will cwal_free() it) or not (in which case
     it does not free() it).
  */
  void const * allocStamp;
};
/**
   Empty-initialized whx_stmt instance, intended for use as an
   in-struct initializer.
*/
#define whx_stmt_empty_m {                      \
    NULL/*stmt*/,                             \
    NULL/*db*/,                                 \
    cwal_buffer_empty_m/*sql*/,                \
    0/*colCount*/,                            \
    0/*paramCount*/,                          \
    0/*rowCount*/,                            \
    0/*flags*/,                               \
    NULL/*next*/,                             \
    NULL/*vSelf*/,                             \
    NULL/*allocStamp*/                        \
    }

/**
   Empty-initialized whx_stmt instance, intended for
   copy-constructing.
*/
extern const whx_stmt whx_stmt_empty;

/**
   Allocates a new, cleanly-initialized whx_stmt instance using
   cwal_malloc(). The returned pointer must eventually be passed to
   whx_stmt_finalize() to free it (whether or not it is ever passed
   to whx_sq3_prepare()).

   Returns NULL on allocation error.
*/
whx_stmt * whx_stmt_malloc(cwal_engine * e);

/**
   Prepares an SQL statement for execution. On success it returns
   0, populates tgt with the statement's state, and the caller is
   obligated to eventually pass tgt to whx_stmt_finalize(). tgt
   must have been cleanly initialized, either via allocation via
   whx_stmt_malloc() or by copy-constructing whx_stmt_empty
   resp. whx_stmt_empty_m (depending on the context).

   On error non-0 is returned and tgt is not modified. If
   preparation of the statement fails at the db level then
   CWAL_RC_DB is returned f's error state (whx_cx_err_get())
   "should" contain more details about the problem. Returns
   CWAL_RC_MISUSE if !db, !callback, or !sql. Returns
   CWAL_RC_NOT_FOUND if db is not opened. Returns CWAL_RC_RANGE if
   !*sql.

   The sql string and the following arguments get routed through
   whx_appendf(), so any formatting options supported by that
   routine may be used here. In particular, the %%q and %%Q
   formatting options are intended for use in escaping SQL for
   routines such as this one.

   Compatibility note: in sqlite, empty SQL code evaluates
   successfully but with a NULL statement. This API disallows empty
   SQL because it uses NULL as a "no statement" marker and because
   empty SQL is arguably not a query at all.

   Tips:

   - whx_stmt_col_count() can be used to determine whether a
   statement is a fetching query (whx_stmt_col_count()>0) or not
   (whx_stmt_col_count()==0) without having to know the contents
   of the query.

   - whx_sq3_prepare_cached() can be used to cache often-used or
   expensive-to-prepare queries within the context of their parent
   db handle.
*/
int whx_sq3_prepare( whx_sq3 *db, whx_stmt * tgt, char const * sql, ... );

/**
   va_list counterpart of whx_sq3_prepare().
*/
int whx_sq3_preparev( whx_sq3 *db, whx_stmt * tgt, char const * sql, va_list args );

/**
   A special-purpose variant of whx_sq3_prepare() which caches
   statements based on their SQL code. This works very much like
   whx_sq3_prepare() and friends except that it can return the same
   statement (via *st) multiple times (statements with identical
   SQL are considered equivalent for caching purposes). Clients
   need not explicitly pass the returned statement to
   whx_stmt_finalize() - the db holds these statements and will
   finalize them when it is closed. It is legal to pass them to
   finalize, in which case they will be cleaned up immediately but
   that also invalidates _all_ pointers to the shared instances.

   If client code does not call whx_stmt_finalize(), it MUST pass
   the statement pointer to whx_stmt_cached_yield(st) after is done
   with it. That makes the query available for use again with this
   routine. If a cached query is not yielded via
   whx_stmt_cached_yield() then this routine will return
   CWAL_RC_ACCESS on subsequent requests for that SQL to prevent
   that recursive (mis)use of the statement causes problems.

   This routine is intended to be used in oft-called routines
   where the cost of re-creating statements on each execution could
   be prohibitive (or at least a bummer).

   Returns 0 on success, CWAL_RC_MISUSE if any arguments are
   invalid.  On other error's db->error might be updated with more
   useful information.  See the Caveats section below for more
   details.

   Its intended usage looks like:

   @code
   whx_stmt * st = NULL;
   int rc = whx_sq3_prepare_cached(myDb, &st, "SELECT ...");
   if(rc) { assert(!st); ...error... }
   else {
   ...use it, and _be sure_ to yield it when done:...
   whx_stmt_cached_yield(st);
   }
   @endcode

   Though this function allows a formatted SQL string, caching is
   generally only useful with statements which have "static" SQL,
   i.e. no call-dependent values embedded within the SQL. It _can_,
   however, contain bind() placeholders which get reset for each
   use. Note that whx_stmt_cached_yield() resets the statement, so
   most uses of cached statements do not require that the client
   explicitly reset cached statements (doing so is harmless,
   however).

   Caveats:

   Cached queries must not be used in contexts where recursion
   might cause the same query to be returned from this function
   while it is being processed at another level in the execution
   stack. Results would be undefined. Caching is primarily intended
   for often-used routines which bind and fetch simple values, and
   not for queries which bind large inlined values or might invoke
   recursion. Because of the potential for recursive breakage, this
   function flags queries it doles out and requires that clients
   call whx_stmt_cached_yield() to un-flag them for re-use. It will
   return CWAL_RC_ACCESS if an attempt is made to (re)prepare a
   statement for which a whx_stmt_cached_yield() is pending, and
   db->error will be populated with a (long) error string
   descripting the problem and listing the SQL which caused the
   collision/misuse.


   Design note: for the recursion/parallel use case we "could"
   reimplement this to dole out a new statement (e.g. by appending
   " -- a_number" to the SQL to bypass the collision) and free it in
   whx_stmt_cached_yield(), but that (A) gets uglier than it needs
   to be and (B) is not needed unless/until we really need cached
   queries in spots which would normally break them. The whole
   recursion problem is still theoretical at this point but could
   easily affect small, often-used queries without recursion.

   @see whx_sq3_stmt_cache_clear()
   @see whx_stmt_cached_yield()
*/
int whx_sq3_prepare_cached( whx_sq3 * db, whx_stmt ** st, char const * sql, ... );

/**
   The va_list counterpart of whx_sq3_prepare_cached().
*/
int whx_sq3_prepare_cachedv( whx_sq3 * db, whx_stmt ** st, char const * sql,
                            va_list args );

/**
   "Yields" a statement which was prepared with
   whx_sq3_prepare_cached(), such that that routine can once again
   use/re-issue that statement. Statements prepared this way must
   be yielded in order to prevent that recursion causes
   difficult-to-track errors when a given cached statement is used
   concurrently in different code contexts.

   If st is not NULL then this also calls whx_stmt_reset() on the
   statement (because that simplifies usage of cached statements) and
   whx_stmt_clear_bindings() to avoid that it holds a reference to any
   now-dead values.

   Returns 0 on success, CWAL_RC_MISUSE if !st or if st does not
   appear to have been doled out from whx_sq3_prepare_cached().

   @see whx_sq3_prepare_cached()
   @see whx_sq3_stmt_cache_clear()
*/
int whx_stmt_cached_yield( whx_stmt * st );

/**
   Immediately cleans up all cached statements.  Returns the number
   of statements cleaned up. It is illegal to call this while any
   of the cached statements are actively being used (have not been
   whx_stmt_cached_yield()ed), and doing so will lead to undefined
   results if the statement(s) in question are used after this
   function completes.

   @see whx_sq3_prepare_cached()
   @see whx_stmt_cached_yield()
*/
cwal_size_t whx_sq3_stmt_cache_clear(whx_sq3 * db);

/**
   A special-purposes utility which schedules SQL to be executed
   the next time whx_sq3_transaction_end() commits a transaction for
   the given db. A commit or rollback will clear all before-commit
   SQL whether it executes them or not. This should not be used as
   a general-purpose trick, and is intended only for use in very
   limited parts of the Fossil infrastructure.

   Before-commit code is only executed if the db has made changes
   since the transaction began. If no changes are recorded
   then before-commit triggers are _not_ run. This is a historical
   behaviour which is up for debate.

   This function does not prepare the SQL, so it does not catch
   errors which happen at prepare-time. Preparation is done (if
   ever) just before the next transaction is committed.

   Returns 0 on success, non-0 on error.

   Potential TODO: instead of storing the raw SQL, prepare the
   statements here and store the statement handles. The main
   benefit would be that this routine could report preport
   preparation errors (which otherwise cause the the commit to
   fail). The down-side is that it prohibits the use of
   multi-statement pre-commit code. We have an implementation of
   this somewhere early on in the libfossil tree, but it was not
   integrated because of the inability to use multi-statement SQL
   with it.
*/
int whx_sq3_before_commit( whx_sq3 *db, char const * sql, ... );

/**
   va_list counterpart to whx_sq3_before_commit().
*/
int whx_sq3_before_commitv( whx_sq3 *db, char const * sql, va_list args );


/**
   Frees memory associated with stmt but does not free stmt unless
   it was allocated by whx_stmt_malloc() (these objects are
   normally stack-allocated, and such object must be initialized by
   copying whx_stmt_empty so that this function knows whether or
   not to cwal_free() them). Returns CWAL_RC_MISUSE if !stmt or if
   has already been finalized (but was not freed).
*/
int whx_stmt_finalize( whx_stmt * stmt );

/**
   "Steps" the given SQL cursor one time and returns one of the
   following: CWAL_RC_STEP_ROW, CWAL_RC_STEP_DONE, CWAL_RC_STEP_ERROR.
   On a db error this will update the underlying db's error state.
   This function increments stmt->rowCount by 1 if it returns
   CWAL_RC_STEP_ROW.

   Returns CWAL_RC_MISUSE if !stmt or stmt has not been prepared.

   It is only legal to call the whx_stmt_g_xxx() and
   whx_stmt_get_xxx() functions if this functon returns
   CWAL_RC_STEP_ROW. CWAL_RC_STEP_DONE is returned upon successfully
   ending iteration or if there is no iteration to perform (e.g. a
   UPDATE or INSERT).


   @see whx_stmt_reset()
   @see whx_stmt_reset2()
   @see whx_stmt_each()
*/
int whx_stmt_step( whx_stmt * stmt );

/**
   A callback interface for use with whx_stmt_each() and
   whx_sq3_each(). It will be called one time for each row fetched,
   passed the statement object and the state parameter passed to
   whx_stmt_each() resp. whx_sq3_each().  If it returns non-0 then
   iteration stops and that code is returned UNLESS it returns
   CWAL_RC_BREAK, in which case whx_stmt_each() stops iteration and
   returns 0. i.e. implementations may return CWAL_RC_BREAK to
   prematurly end iteration without causing an error.

   This callback is not called for non-fetching queries or queries
   which return no results, though it might (or might not) be
   interesting for it to do so, passing a NULL stmt for that case.

   stmt->rowCount can be used to determine how many times the
   statement has called this function. Its counting starts at 1.

   It is strictly illegal for a callback to pass stmt to
   whx_stmt_step(), whx_stmt_reset(), whx_stmt_finalize(), or any
   similar routine which modifies its state. It must only read the
   current column data (or similar metatdata, e.g. column names)
   from the statement, e.g. using whx_stmt_g_int32(),
   whx_stmt_get_text(), or similar.
*/
typedef int (*whx_stmt_each_f)( whx_stmt * stmt, void * state );

/**
   Calls the given callback one time for each result row in the
   given statement, iterating over stmt using whx_stmt_step(). It
   applies no meaning to the callbackState parameter, which gets
   passed as-is to the callback. See whx_stmt_each_f() for the
   semantics of the callback.

   Returns 0 on success. Returns CWAL_RC_MISUSE if !stmt or
   !callback.
*/
int whx_stmt_each( whx_stmt * stmt, whx_stmt_each_f callback,
                   void * callbackState );

/**
   Resets the given statement, analog to sqlite3_reset(). Should be
   called one time between whx_stmt_step() iterations when running
   multiple INSERTS, UPDATES, etc. via the same statement. If
   resetRowCounter is true then the statement's row counter
   (st->rowCount) is also reset to 0, else it is left
   unmodified. (Most use cases don't use the row counter.)

   Returns 0 on success, CWAL_RC_MISUSE if !stmt or stmt has not
   been prepared, CWAL_RC_DB if the underlying reset fails (in which
   case the error state of the stmt->db handle is updated to
   contain the error information).

   @see whx_stmt_db()
   @see whx_stmt_reset()
*/
int whx_stmt_reset2( whx_stmt * stmt, char resetRowCounter );

/**
   Equivalent to whx_stmt_reset2(stmt, 0).
*/
int whx_stmt_reset( whx_stmt * stmt );

/**
   Analog to sqlite3_clear_bindings(), this function sets all bound
   parameter values to NULL. Returns 0 on success, CWAL_RC_MISUSE
   if !stmt or it does not appear to be properly initializer,
   or WHX_RC_DB if sqlite3 reports an error.
*/
int whx_stmt_clear_bindings( whx_stmt * stmt );

/**
   Returns the db handle which prepared the given statement, or
   NULL if !stmt or stmt has not been prepared.
*/
whx_sq3 * whx_stmt_db( whx_stmt * stmt );

/**
   Returns the SQL string used to prepare the given statement, or
   NULL if !stmt or stmt has not been prepared. If len is not NULL
   then *len is set to the length of the returned string (which is
   NUL-terminated). The returned bytes are owned by stmt and are
   invalidated when it is finalized.
*/
char const * whx_stmt_sql( whx_stmt * stmt, cwal_size_t * len );

/**
   Returns the name of the given 0-based result column index, or
   NULL if !stmt, stmt is not prepared, or index is out out of
   range. The returned bytes are owned by the statement object and
   may be invalidated shortly after this is called, so the caller
   must copy the returned value if it needs to have any useful
   lifetime guarantees. It's a bit more complicated than this, but
   assume that any API calls involving the statement handle might
   invalidate the column name bytes.

   The API guarantees that the returned value is either NULL or
   NUL-terminated.

   @see whx_stmt_param_count()
   @see whx_stmt_col_count()
*/
char const * whx_stmt_col_name(whx_stmt * stmt, int index);

/**
   Returns the result column count for the given statement, or -1 if
   !stmt or it has not been prepared. Note that this value is cached
   when the statement is created. Note that non-fetching queries
   (e.g. INSERT and UPDATE) have a column count of 0. Some non-SELECT
   constructs, e.g. PRAGMA table_info(tname), behave like SELECT
   and have a positive column count.

   @see whx_stmt_param_count()
   @see whx_stmt_col_name()
*/
int whx_stmt_col_count( whx_stmt const * stmt );

/**
   Returns the bound parameter count for the given statement, or -1
   if !stmt or it has not been prepared. Note that this value is
   cached when the statement is created.

   @see whx_stmt_col_count()
   @see whx_stmt_col_name()
*/
int whx_stmt_param_count( whx_stmt const * stmt );

/**
   Returns the index of the given named parameter for the given
   statement, or -1 if !stmt or stmt is not prepared.
*/
int whx_stmt_param_index( whx_stmt * stmt, char const * param);

/**
   Binds NULL to the given 1-based parameter index.  Returns 0 on
   succcess. Sets the DB's error state on error.
*/
int whx_stmt_bind_null( whx_stmt * stmt, int index );

/**
   Equivalent to whx_stmt_bind_null_name() but binds to
   a named parameter.
*/
int whx_stmt_bind_null_name( whx_stmt * stmt, char const * param );

/**
   Binds v to the given 1-based parameter index.  Returns 0 on
   succcess. Sets the DB's error state on error.
*/
int whx_stmt_bind_int32( whx_stmt * stmt, int index, int32_t v );

/**
   Equivalent to whx_stmt_bind_int32() but binds to a named
   parameter.
*/
int whx_stmt_bind_int32_name( whx_stmt * stmt, char const * param, int32_t v );

/**
   Binds v to the given 1-based parameter index.  Returns 0 on
   succcess. Sets the DB's error state on error.
*/
int whx_stmt_bind_int64( whx_stmt * stmt, int index, int64_t v );

/**
   Equivalent to whx_stmt_bind_int64() but binds to a named
   parameter.
*/
int whx_stmt_bind_int64_name( whx_stmt * stmt, char const * param, int64_t v );

/**
   Binds v to the given 1-based parameter index.  Returns 0 on
   succcess. Sets the Fossil context's error state on error.
*/
int whx_stmt_bind_double( whx_stmt * stmt, int index, double v );

/**
   Equivalent to whx_stmt_bind_double() but binds to a named
   parameter.
*/
int whx_stmt_bind_double_name( whx_stmt * stmt, char const * param, double v );

/**
   Binds v to the given 1-based parameter index.  Returns 0 on
   succcess. Sets the DB's error state on error.
*/
int whx_stmt_bind_id( whx_stmt * stmt, int index, whx_sq3_id_t v );

/**
   Equivalent to whx_stmt_bind_id() but binds to a named
   parameter.
*/
int whx_stmt_bind_id_name( whx_stmt * stmt, char const * param, whx_sq3_id_t v );

/**
   Binds the first n bytes of v as text to the given 1-based bound
   parameter column in the given statement. If makeCopy is true then
   the binding makes an copy of the data. Set makeCopy to false ONLY
   if you KNOW that the bytes will outlive the binding.

   Returns 0 on success. On error stmt's underlying db's error state
   is updated, hopefully with a useful error message.
*/
int whx_stmt_bind_text( whx_stmt * stmt, int index,
                        char const * v, cwal_int_t n,
                        char makeCopy );

/**
   Equivalent to whx_stmt_bind_text() but binds to a named
   parameter.
*/
int whx_stmt_bind_text_name( whx_stmt * stmt, char const * param,
                             char const * v, cwal_int_t n,
                             char makeCopy );
/**
   Binds the first n bytes of v as a blob to the given 1-based bound
   parameter column in the given statement. See whx_stmt_bind_text()
   for the semantics of the makeCopy parameter and return value.
*/
int whx_stmt_bind_blob( whx_stmt * stmt, int index,
                        void const * v, cwal_size_t len,
                        char makeCopy );

/**
   Equivalent to whx_stmt_bind_blob() but binds to a named
   parameter.
*/
int whx_stmt_bind_blob_name( whx_stmt * stmt, char const * param,
                             void const * v, cwal_int_t len,
                             char makeCopy );

/**
   Gets an integer value from the given 0-based result set column,
   assigns *v to that value, and returns 0 on success.

   Returns CWAL_RC_RANGE if index is out of range for stmt.
*/
int whx_stmt_get_int32( whx_stmt * stmt, int index, int32_t * v );

/**
   Gets an integer value from the given 0-based result set column,
   assigns *v to that value, and returns 0 on success.

   Returns CWAL_RC_RANGE if index is out of range for stmt.
*/
int whx_stmt_get_int64( whx_stmt * stmt, int index, int64_t * v );

/**
   The whx_sq3_id_t counterpart of whx_stmt_get_int32(). Depending on
   the sizeof(whx_sq3_id_t), it behaves as one of whx_stmt_get_int32()
   or whx_stmt_get_int64().
*/
int whx_stmt_get_id( whx_stmt * stmt, int index, whx_sq3_id_t * v );

/**
   Convenience form of whx_stmt_get_id() which returns the value
   directly but cannot report errors. It returns -1 on error, but
   that is not unambiguously an error value.
*/
whx_sq3_id_t whx_stmt_g_id( whx_stmt * stmt, int index );

/**
   Convenience form of whx_stmt_get_int32() which returns the value
   directly but cannot report errors. It returns 0 on error, but
   that is not unambiguously an error.
*/
int32_t whx_stmt_g_int32( whx_stmt * stmt, int index );

/**
   Convenience form of whx_stmt_get_int64() which returns the value
   directly but cannot report errors. It returns 0 on error, but
   that is not unambiguously an error.
*/
int64_t whx_stmt_g_int64( whx_stmt * stmt, int index );

/**
   Convenience form of whx_stmt_get_double() which returns the value
   directly but cannot report errors. It returns 0 on error, but
   that is not unambiguously an error.
*/
double whx_stmt_g_double( whx_stmt * stmt, int index );

/**
   Convenience form of whx_stmt_get_text() which returns the value
   directly but cannot report errors. It returns NULL on error, but
   that is not unambiguously an error because it also returns NULL
   if the column contains an SQL NULL value. If outLen is not NULL
   then it is set to the byte length of the returned string.
*/
char const * whx_stmt_g_text( whx_stmt * stmt, int index, cwal_size_t * outLen );

/**
   Gets double value from the given 0-based result set column,
   assigns *v to that value, and returns 0 on success.

   Returns CWAL_RC_RANGE if index is out of range for stmt.
*/
int whx_stmt_get_double( whx_stmt * stmt, int index, double * v );

/**
   Gets a string value from the given 0-based result set column,
   assigns *out (if out is not NULL) to that value, assigns *outLen
   (if outLen is not NULL) to *out's length in bytes, and returns 0
   on success. Ownership of the string memory is unchanged - it is owned
   by the statement and the caller should immediately copy it if
   it will be needed for much longer.

   Returns CWAL_RC_RANGE if index is out of range for stmt.
*/
int whx_stmt_get_text( whx_stmt * stmt, int index, char const **out,
                       cwal_size_t * outLen );

/**
   The Blob counterpart of whx_stmt_get_text(). Identical to that
   function except that its output result (3rd paramter) type
   differs, and it fetches the data as a raw blob, without any sort
   of string interpretation.

   Returns CWAL_RC_RANGE if index is out of range for stmt.
*/
int whx_stmt_get_blob( whx_stmt * stmt, int index, void const **out, cwal_size_t * outLen );

/**
   Executes multiple SQL statements, ignoring any results they might
   collect. Returns 0 on success, non-0 on error.  On error
   db->error might be updated to report the problem.
*/
int whx_sq3_exec_multi( whx_sq3 * db, const char * sql, ...);

/**
   va_list counterpart of db_exec_multi().
*/
int whx_sq3_exec_multiv( whx_sq3 * db, const char * sql, va_list args);

/**
   Executes a single formatted SQL statement (as per cwal_printf() and
   friends), skipping over any results it may have. Returns 0 on
   success. On error db's error state may be updated.
*/
int whx_sq3_exec( whx_sq3 * db, char const * sql, ... );

/**
   va_list counterpart of fs_db_exec().
*/
int whx_sq3_execv( whx_sq3 * db, char const * sql, va_list args );

/**
   Begins a transaction on the given db. Nested transactions are
   not directly supported but the db handle keeps track of
   open/close counts, such that whx_sq3_transaction_end() will not
   actually do anything until the transaction begin/end counter
   goes to 0. Returns CWAL_RC_MISUSE if !db or the db is not
   connected, else the result of the underlying db call(s).

   Transactions are an easy way to implement "dry-run" mode for
   some types of applications. For example:

   @code
   char dryRunMode = ...;
   whx_sq3_transaction_begin(db);
   ...do your stuff...
   whx_sq3_transaction_end(db, dryRunMode ? 1 : 0);
   @endcode

   Here's a tip for propagating error codes when using
   transactions:

   @code
   ...
   if(rc) whx_sq3_transaction_end(db, 1);
   else rc = whx_sq3_transaction_end(db, 0);
   @endcode

   That ensures that we propagate rc in the face of a rollback but
   we also capture the rc for a commit (which might yet fail). Note
   that a rollback in and of itself is not an error (though it also
   might fail, that would be "highly unusual" and indicative of
   other problems), and we certainly don't want to overwrite that
   precious non-0 rc with a successful return result from a
   rollback (which would, in effect, hide the error from the
   client).
*/
int whx_sq3_transaction_begin(whx_sq3 * db);

/**
   Equivalent to whx_sq3_transaction_end(db, 0).
*/
int whx_sq3_transaction_commit(whx_sq3 * db);

/**
   Equivalent to whx_sq3_transaction_end(db, 1).
*/
int whx_sq3_transaction_rollback(whx_sq3 * db);

/**
   Forces a rollback of any pending transaction in db, regardless
   of the internal transaction begin/end counter. Returns
   CWAL_RC_MISUSE if !db or db is not opened, else returns the value
   of the underlying ROLLBACK call. This also re-sets/frees any
   transaction-related state held by db (e.g. db->beforeCommit).
   Use with care, as this mucks about with db state in a way which
   is not all that pretty and it may confuse downstream code.

   Returns 0 on success.
*/
int whx_sq3_rollback_force(whx_sq3 * db);

/**
   Decrements the transaction counter incremented by
   whx_sq3_transaction_begin() and commits or rolls back the
   transaction if the counter goes to 0.

   If doRollback is true then this rolls back (or schedules a
   rollback of) a transaction started by
   whx_sq3_transaction_begin(). If doRollback is false is commits
   (or schedules a commit).

   If db whx_sq3_transaction_begin() is used in a nested manner and
   doRollback is true for any one of the nested calls, then that
   value will be remembered, such that the downstream calls to this
   function within the same transaction will behave like a rollback
   even if they pass 0 for the second argument.

   Returns CWAL_RC_MISUSE if !db or the db is not opened, 0 if
   the transaction counter is above 0, else the result of the
   (potentially many) underlying database operations.

   Unfortunate low-level co-dependency: if db->f is not NULL and
   (db->role & WHX_SQ3_ROLE_REPO) then this function may perform
   extra repository-related post-processing on any commit, and
   checking the result code is particularly important for those
   cases.
*/
int whx_sq3_transaction_end(whx_sq3 * db, char doRollback);

/**
   If the whx_sq3_db_transaction_begin() family of routines currently
   has a transaction in place, this returns the number of transaction
   levels, else 0 is returned.
*/
int whx_sq3_db_transaction_level(whx_sq3 const * db);

/**
   Runs the given SQL query on the given db and returns non-0
   (true) if the query returns any rows, else 0 (false). Returns 0
   for any error as well.
*/
char whx_sq3_exists(whx_sq3 * db, char const * sql, ... );

/**
   va_list counterpart of whx_sq3_exists().
*/
char whx_sq3_existsv(whx_sq3 * db, char const * sql, va_list args );

/**
   Runs a fetch-style SQL query against DB and returns the first
   column of the first result row via *rv. If the query returns no
   rows, *rv is not modified. The intention is that the caller sets
   *rv to his preferred default (or sentinel) value before calling
   this.

   The format string (the sql parameter) accepts all formatting
   options supported by whx_appendf().

   Returns 0 on success. On error db's error state is updated and
   *rv is not modified.

   Returns CWAL_RC_MISUSE without side effects if !db, !rv, !sql,
   or !*sql.
*/
int whx_sq3_get_int32( whx_sq3 * db, int32_t * rv,
                      char const * sql, ... );

/**
   va_list counterpart of whx_sq3_get_int32().
*/
int whx_sq3_get_int32v( whx_sq3 * db, int32_t * rv,
                       char const * sql, va_list args);

/**
   Convenience form of whx_sq3_get_int32() which returns the value
   directly but provides no way of checking for errors. On error,
   or if no result is found, defaultValue is returned.
*/
int32_t whx_sq3_g_int32( whx_sq3 * db, int32_t defaultValue,
                            char const * sql, ... );

/**
   The int64 counterpart of whx_sq3_get_int32(). See that function
   for the semantics.
*/
int whx_sq3_get_int64( whx_sq3 * db, int64_t * rv,
                      char const * sql, ... );

/**
   va_list counterpart of whx_sq3_get_int64().
*/
int whx_sq3_get_int64v( whx_sq3 * db, int64_t * rv,
                       char const * sql, va_list args);

/**
   Convenience form of whx_sq3_get_int64() which returns the value
   directly but provides no way of checking for errors. On error,
   or if no result is found, defaultValue is returned.
*/
int64_t whx_sq3_g_int64( whx_sq3 * db, int64_t defaultValue,
                            char const * sql, ... );


/**
   The whx_sq3_id_t counterpart of whx_sq3_get_int32(). See that function
   for the semantics.
*/
int whx_sq3_get_id( whx_sq3 * db, whx_sq3_id_t * rv,
                   char const * sql, ... );

/**
   va_list counterpart of whx_sq3_get_id().
*/
int whx_sq3_get_idv( whx_sq3 * db, whx_sq3_id_t * rv,
                    char const * sql, va_list args);

/**
   Convenience form of whx_sq3_get_id() which returns the value
   directly but provides no way of checking for errors. On error,
   or if no result is found, defaultValue is returned.
*/
whx_sq3_id_t whx_sq3_g_id( whx_sq3 * db, whx_sq3_id_t defaultValue,
                      char const * sql, ... );


/**
   The cwal_size_t counterpart of whx_sq3_get_int32(). See that
   function for the semantics. If this function would fetch a
   negative value, it returns CWAL_RC_RANGE and *rv is not modified.
*/
int whx_sq3_get_size( whx_sq3 * db, cwal_size_t * rv,
                     char const * sql, ... );

/**
   va_list counterpart of whx_sq3_get_size().
*/
int whx_sq3_get_sizev( whx_sq3 * db, cwal_size_t * rv,
                      char const * sql, va_list args);

/**
   Convenience form of whx_sq3_get_size() which returns the value
   directly but provides no way of checking for errors. On error,
   or if no result is found, defaultValue is returned.
*/
cwal_size_t whx_sq3_g_size( whx_sq3 * db, cwal_size_t defaultValue,
                          char const * sql, ... );


/**
   The double counterpart of whx_sq3_get_int32(). See that function
   for the semantics.
*/
int whx_sq3_get_double( whx_sq3 * db, double * rv,
                       char const * sql, ... );

/**
   va_list counterpart of whx_sq3_get_double().
*/
int whx_sq3_get_doublev( whx_sq3 * db, double * rv,
                        char const * sql, va_list args);

/**
   Convenience form of whx_sq3_get_double() which returns the value
   directly but provides no way of checking for errors. On error,
   or if no result is found, defaultValue is returned.
*/
double whx_sq3_g_double( whx_sq3 * db, double defaultValue,
                              char const * sql, ... );

/**
   The C-string counterpart of whx_sq3_get_int32(). On success *rv
   will be set to a dynamically allocated string copied from the
   first column of the first result row. If rvLen is not NULL then
   *rvLen will be assigned the byte-length of that string. If no
   row is found, *rv is set to NULL and *rvLen (if not NULL) is set
   to 0, and 0 is returned. Note that NULL is also a legal result
   (an SQL NULL translates as a NULL string), The caller must
   eventually free the returned string value using cwal_free().
*/
int whx_sq3_get_text( whx_sq3 * db, char ** rv, cwal_size_t * rvLen,
                     char const * sql, ... );

/**
   va_list counterpart of whx_sq3_get_text().
*/
int whx_sq3_get_textv( whx_sq3 * db, char ** rv, cwal_size_t * rvLen,
                      char const * sql, va_list args );

/**
   Convenience form of whx_sq3_get_text() which returns the value
   directly but provides no way of checking for errors. On error,
   or if no result is found, NULL is returned. The returned string
   must eventually be passed to cwal_free() to free it.  If len is
   not NULL then if non-NULL is returned, *len will be assigned the
   byte-length of the returned string.
*/
char * whx_sq3_g_text( whx_sq3 * db, cwal_size_t * len,
                      char const * sql,
                      ... );

/**
   The Blob counterpart of whx_sq3_get_text(). Identical to that
   function except that its output result (2nd paramter) type
   differs, and it fetches the data as a raw blob, without any sort
   of string interpretation. The returned *rv memory must
   eventually be passed to cwal_free() to free it. If len is not
   NULL then on success *len will be set to the byte length of the
   returned blob. If no row is found, *rv is set to NULL and *rvLen
   (if not NULL) is set to 0, and 0 is returned. Note that NULL is
   also a legal result (an SQL NULL translates as a NULL string),
*/
int whx_sq3_get_blob( whx_sq3 * db, void ** rv, cwal_size_t * len,
                     char const * sql, ... );


/**
   va_list counterpart of whx_sq3_get_blob().
*/
int whx_sq3_get_blobv( whx_sq3 * db, void ** rv, cwal_size_t * stmtLen,
                      char const * sql, va_list args );

/**
   Convenience form of whx_sq3_get_blob() which returns the value
   directly but provides no way of checking for errors. On error,
   or if no result is found, NULL is returned.
*/
void * whx_sq3_g_blob( whx_sq3 * db, cwal_size_t * len,
                      char const * sql,
                      ... );
/**
   Similar to whx_sq3_get_text() and whx_sq3_get_blob(), but writes
   its result to tgt, overwriting (not appennding to) any existing
   memory it might hold.

   If asBlob is true then the underlying BLOB API is used to
   populate the buffer, else the underlying STRING/TEXT API is
   used.  For many purposes there will be no difference, but if you
   know you might have binary data, be sure to pass a true value
   for asBlob to avoid any potential encoding-related problems.
*/
int whx_sq3_get_buffer( whx_sq3 * db, cwal_buffer * tgt,
                       char asBlob,
                       char const * sql, ... );

/**
   va_list counterpart of whx_sq3_get_buffer().
*/
int whx_sq3_get_bufferv( whx_sq3 * db, cwal_buffer * tgt,
                        char asBlob,
                        char const * sql, va_list args );


/**
   Expects sql to be a SELECT-style query which (potentially)
   returns a result set. For each row in the set callback() is
   called, as described for whx_stmt_each(). Returns 0 on success.
   The callback is _not_ called for queries which return no
   rows. If clients need to know if rows were returned, they can
   add a counter to their callbackState and increment it from the
   callback.

   Returns CWAL_RC_MISUSE if !db, db is not opened, !callback,
   !sql. Returns CWAL_RC_RANGE if !*sql.
*/
int whx_sq3_each( whx_sq3 * db, whx_stmt_each_f callback,
                 void * callbackState, char const * sql, ... );

/**
   va_list counterpart to whx_sq3_each().
*/
int whx_sq3_eachv( whx_sq3 * db, whx_stmt_each_f callback,
                  void * callbackState, char const * sql, va_list args );


/**
   Returns the given Julian date value formatted as an ISO8601
   string (with a fractional seconds part if msPrecision is true,
   else without it).  Returns NULL if !db, db is not connected, j
   is less than 0, or on allocation error. The returned memory must
   eventually be freed using cwal_free().

   If localTime is true then the value is converted to the local time,
   otherwise it is not.

   @see whx_sq3_unix_to_iso8601()
   @see whx_julian_to_iso8601()
   @see whx_iso8601_to_julian()
*/
char * whx_sq3_julian_to_iso8601( whx_sq3 * db, double j,
                                 char msPrecision, char localTime );

/**
   Returns the given Julian date value formatted as an ISO8601
   string (with a fractional seconds part if msPrecision is true,
   else without it).  Returns NULL if !db, db is not connected, j
   is less than 0, or on allocation error. The returned memory must
   eventually be freed using cwal_free().

   If localTime is true then the value is converted to the local time,
   otherwise it is not.

   @see whx_sq3_julian_to_iso8601()
   @see whx_julian_to_iso8601()
   @see whx_iso8601_to_julian()
*/
char * whx_sq3_unix_to_iso8601( whx_sq3 * db, whx_sq3_time_t j, char localTime );


/**
   Returns the current time in Julian Date format. Returns a negative
   value if !db or db is not opened.
*/
double whx_sq3_julian_now(whx_sq3 * db);

/**
   Uses the given db to convert the given time string to Julian Day
   format. If it cannot be converted, a negative value is returned.
   The str parameter can be anything suitable for passing to sqlite's:

   SELECT julianday(str)

   Note that this routine will escape str for use with SQL - the
   caller must not do so.

   @see whx_julian_to_iso8601()
   @see whx_iso8601_to_julian()
*/
double whx_sq3_string_to_julian(whx_sq3 * db, char const * str);

/**
   Opens the given db file and populates db with its handle.  db
   must have been cleanly initialized by copy-initializing it from
   whx_sq3_empty (or whx_sq3_empty_m) or by allocating it using
   whx_sq3_malloc(). Failure to do so will lead to undefined
   behaviour.

   openFlags may be a mask of WHX_OPEN_F_xxx values, but not all
   are used/supported here. If WHX_OPEN_F_CREATE is _not_ set in
   openFlags and dbFile does not exist, it will return
   CWAL_RC_NOT_FOUND. The existence of WHX_OPEN_F_CREATE in the
   flags will cause this routine to try to create the file if
   needed. If conflicting flags are specified (e.g. WHX_OPEN_F_RO
   and WHX_OPEN_F_RWC) then which one takes precedence is
   unspecified and possibly unpredictable.

   As a special case, if dbFile is ":memory:" (for an in-memory
   database) or "" (empty string, for a "temporary" database) then it
   is is passed through without any filesystem-related checks and the
   openFlags are ignored.

   See this page for the differences between ":memory:" and "":

   https://www.sqlite.org/inmemorydb.html

   Returns CWAL_RC_MISUSE if !db, !dbFile, !*dbFile, or if db->dbh
   is not NULL (i.e. if it is already opened or its memory was
   default-initialized (use whx_sq3_empty to cleanly copy-initialize
   new stack-allocated instances).

   On error db->dbh will be NULL, but db->error might contain error
   details.

   Regardless of success or failure, db should be passed to
   whx_sq3_close() to free up all memory associated with it. It is
   not closed automatically by this function because doing so cleans
   up the error state, which the caller will presumably want to
   have.

   @see whx_sq3_close()
   @see whx_sq3_prepare()
   @see whx_sq3_malloc()
*/
int whx_sq3_open( cwal_engine * const ec, whx_sq3 * const db,
                  char const * dbFile, int openFlags );

/**
   Closes the given db handle and frees any resources owned by
   db.

   If db was allocated using whx_sq3_malloc() (as determined by
   examining db->allocStamp) then this routine also cwal_free()s it,
   otherwise it is assumed to either be on the stack or part of a
   larger struct and is not freed.

   If db has any pending transactions, they are rolled
   back by this function.
*/
void whx_sq3_close( whx_sq3 * const db );

/**
   If db is an opened db handle, this registers a debugging function
   with the db which traces all SQL to the given FILE handle. If the
   2nd argument is NULL then tracing is disabled.

   This mechanism is only intended for debugging and exploration of
   how the over-lying app. Tracing is often as easy way to ensure that
   a given code block is getting run. Unfortunately, this level of the
   API currently only supports output to a FILE handle, not to an
   arbitrary output routine. (Reminder to self: sqlite3_trace_v2()
   gives us the pieces we need to improve upon that, but it's a
   low-priority TODO.)

   This is a no-op if !db or db is not opened.
*/
void whx_sq3_sqltrace_enable( whx_sq3 * db, FILE * outStream );

/**
   Returns the row ID of the most recent insertion,
   or -1 if !db, db is not connected, or 0 if no inserts
   have been performed.
*/
whx_sq3_id_t whx_sq3_last_insert_id(whx_sq3 *db);

/**
   Returns non-0 (true) if the database (which must be open) table
   identified by zTableName has a column named zColName
   (case-sensitive), else returns 0.
*/
char whx_sq3_table_has_column( whx_sq3 * db, char const *zTableName,
                              char const *zColName );

  
/**
   Allocates a new whx_sq3 instance(). Returns NULL on allocation
   error. Note that whx_sq3 instances can often be used from the
   stack - allocating them dynamically is an uncommon case necessary
   for script bindings.

   Achtung: the returned value's allocStamp member is used for
   determining if whx_sq3_close() should free the value or not.  Thus
   if clients copy over this value without adjusting allocStamp back
   to its original value, the library will likely leak the instance.
   Been there, done that.
*/
whx_sq3 * whx_sq3_malloc(cwal_engine * const e);

/**
   The whx_stmt counterpart of whx_sq3_malloc(). See that function
   for when you might want to use this and a caveat involving the
   allocStamp member of the returned value. whx_stmt_finalize() will
   free statements created with this function.
*/
whx_stmt * whx_stmt_malloc(cwal_engine * const e);


/**
   ATTACHes the file zDbName to db using the databbase name
   zLabel. Returns 0 on success. Returns CWAL_RC_MISUSE if any
   argument is NULL or any string argument starts with a NUL byte,
   else it returns the result of whx_sq3_exec() which attaches the
   db. On db-level errors db's error state will be updated.

   The 3rd and 5th parameters specify the length, in bytes, of the 2nd
   and 4th parameters, respectively (not counting any trailing NUL
   byte, if any).

   If either of dbLen or labelLen is 0, cwal_strlen() is used to
   get the corresponding string to get its length.
*/
int whx_sq3_attach(whx_sq3 * db, const char *zDbName, cwal_size_t dbLen,
                   const char *zLabel, cwal_size_t labelLen);

/**
   The converse of whx_sq3_detach(). Must be passed the same arguments
   which were passed as the 1st and 3rd arguments to whx_sq3_attach().
   Returns 0 on success, CWAL_RC_MISUSE if !db, !zLabel, or !*zLabel,
   else it returns the result of the underlying whx_sq3_exec()
   call.

   The 3rd parameter specifies the length, in bytes, of the 2nd
   argument (not counting any trailing NUL byte, if any).  If the 3rd
   parameter is 0, cwal_strlen() is used to get the 2nd argument's
   length.
*/
int whx_sq3_detach(whx_sq3 * db, const char *zLabel, cwal_size_t labelLen);


/**
   Expects fmt to be a SELECT-style query. For each row in the
   query, the first column is fetched as a string and appended to
   the tgt list.

   Returns 0 on success, CWAL_RC_MISUSE if !db, !tgt, or !fmt, any
   number of potential CWAL_RC_OOM or db-related errors.

   Results rows with a NULL value (resulting from an SQL NULL) are
   added to the list as NULL entries.

   Each entry appended to the list is a (char *) which must
   be freed using cwal_free(). To easiest way to clean up
   the list and its contents is:

   @code
   cwal_list_visit_free(tgt);
   @endcode

   On error the list may be partially populated.

   Complete example:
   @code
   cwal_list li = cwal_list_empty;
   int rc = whx_sq3_select_slist(db, &li,
   "SELECT uuid FROM blob WHERE rid<20");
   if(!rc){
   cwal_size_t i;
   for(i = 0;i < li.used; ++i){
   char const * uuid = (char const *)li.list[i];
   whx_fprintf(stdout, "UUID: %s\n", uuid);
   }
   }
   cwal_list_visit_free(&li, 1);
   @endcode

   Of course cwal_list_visit() may be used to traverse the list as
   well, as long as the visitor expects (char [const]*) list
   elements.
*/
int whx_sq3_select_slist( whx_sq3 * db, cwal_list * tgt,
                         char const * fmt, ... );

/**
   The va_list counterpart of whx_sq3_select_slist().
*/
int whx_sq3_select_slistv( whx_sq3 * db, cwal_list * tgt,
                          char const * fmt, va_list args );

/**
   Returns n bytes of random lower-case hexidecimal characters
   using the given db as its data source, plus a terminating NUL
   byte. The returned memory must eventually be freed using
   cwal_free(). Returns NULL if !db, !n, or on a db-level error.
*/
char * whx_sq3_random_hex(whx_sq3 * db, cwal_size_t n);

/**
   Returns the "number of database rows that were changed or
   inserted or deleted by the most recently completed SQL statement"
   (to quote the underlying APIs). Returns 0 if !db or if db is not
   opened.


   See: http://sqlite.org/c3ref/changes.html
*/
int whx_sq3_changes_recent(whx_sq3 * db);
  
/**
   Returns "the number of row changes caused by INSERT, UPDATE or
   DELETE statements since the database connection was opened" (to
   quote the underlying APIs). Returns 0 if !db or if db is not
   opened.

   See; http://sqlite.org/c3ref/total_changes.html
*/
int whx_sq3_changes_total(whx_sq3 * db);

/**
   A thin proxy for sqlite3_busy_timeout()
   (https://sqlite.org/c3ref/busy_timeout.html), the only difference
   being that this function translates any non-0 result code from
   sqlite to one of the CWAL_RC_xxx, S2_RC_xxx, or WHX_RC_xxx
   codes. (The sqlite3 docs do not document what
   sqlite3_busy_timeout() returns.)

   Results are undefined if db is not open.
*/
int whx_sq3_busy_timeout(whx_sq3 * db, int ms);

/**
   Initializes the given database file. zFilename is the name of the
   db file. It is created if needed, but any directory components are
   not created. zSchema is the base schema to install - it may be NULL
   or the empty string. If zSchema is not NULL and does not start with
   a NUL, the following arguments may be (char const *) SQL code, each
   of which gets run against the db after the main schema is called.
   The variadic argument list MUST end with NULL (0) unless zSchema is
   NULL/empty (in which case the variadic arguments are not
   evaluated).

   Returns 0 on success.

   Errors are propagated via el's underlying cwal_engine.
*/
int whx_sq3_init( cwal_engine * const ec, char const * zFilename,
                  char const * zSchema, ... );

/**
   A whx_stmt_each_f() impl, intended primarily for debugging, which
   simply outputs row data in tabular form via whx_output(). The
   state argument is ignored. This only works if stmt was prepared
   by a whx_sq3 instance which has an associated whx_cx instance. On
   the first row, the column names are output.
*/
int whx_stmt_each_f_dump( whx_stmt * stmt, void * state );

/**
   Type IDs for type-safely mapping (void*) to cwal_native
   instances. Their types and values are irrelevant. Their values are
   statically initialized and remain constant for the life of the app.
*/
struct whx_type_ids {
  /** Type ID for whx_sq3 pointers. */
  void const * sq3;
  /** Type ID for a pointer to the whx_sq3 prototype object (not
      provided by this API - for use in/by the loadable
      module counterpart of this API).
  */
  void const * sq3_prototype;
  /** Type ID for whx_stmt pointers. */
  void const * stmt;
};
typedef struct whx_type_ids whx_type_ids;
/**
   Hold constant type IDs for various cwal/native mappings.
*/
extern const whx_type_ids WHX_SQ3_TYPE_IDS;

/**
   If self is a cwal-bound whx_sq3 db object, this function sets *n
   (if n is not NULL) to the cwal_native pointer for that object, *vdb
   (if vdb is not NULL) to the cwal_value counterpart of that pointer
   (the same as the first argument or a prototype of it), *db (if db
   is not NULL) to the whx_sq3 pointer for that object, and returns 0.

   Note that any arguments after the first may be NULL.

   Returns CWAL_RC_TYPE if no whx_sq3 can be extracted from the first
   argument.
*/
int whx_sq3_extract_cwal( cwal_value * self, cwal_native ** n, cwal_value **vdb, whx_sq3 ** db );

/**
   The whx_stmt counterpart of whx_sq3_extract_cwal().
*/
int whx_stmt_extract_cwal( cwal_value * self, cwal_native ** n, cwal_value **vst, whx_stmt ** st );

/**
   Tries to convert the given sqlite3_value to "the closest
   approximation" of its type in cwal_value form by assigning it to
   *rv. Returns 0 on success. On error *rv is not modified. Does not
   trigger an s2/cwal exception on error.
*/
int whx_sq3_value_to_cwal( cwal_engine * e, sqlite3_value * sqv, cwal_value ** rv );

/**
  Calls one of the sqlite3_result_XXX() variants, depending on the
  data type of v (which may be NULL). If v is a string or buffer then
  its bytes are immediately copied to the result, as opposed to
  pointing the result to the value's bytes.
*/
void whx_sq3_result( sqlite3_context * context, cwal_value * v );


/**
   Tries to bind v to the given parameter column (1-based) of st.

   The undefined value binds as SQL NULL.

   Returns 0 on success, triggers a script-side exception on error
   (including an out-of-bounds index).
*/
int whx_stmt_bind( whx_stmt * st, int ndx, cwal_value * v );

/**
   Similar to whx_stmt_bind(), but....

   The 4th argument may be either an Array/Tuple of values to bind, a
   container of param names (incl. ':' or '$' prefix), or a single
   value, to bind at the given index. For lists/containers, the given
   index is ignored: all indexes in the given list/container are
   bound.

   As a special case, if the 4th argument == cwal_value_undefined()
   then it is simply ignored (this is to simplify some script-side
   code).

   Returns 0 on success. On error, it "should" trigger a script-side
   exception, but it won't if the error is OOM.
*/
int whx_stmt_bind2(whx_stmt * st, int ndx, cwal_value * bind);

/**
   Iterates over the given array and passes each entry to
   whx_stmt_bind(), using 1-based array index. That is, array[0] binds
   to position 1, etc. Returns 0 if all bindings succeeds, else the
   result of the failed whx_stmt_bind() call.
*/
int whx_stmt_bind_values_a( whx_stmt * st, cwal_array const * src);

/**
   The tuple counterpart of whx_stmt_bind_values_a().
*/
int whx_stmt_bind_values_t( whx_stmt * st, cwal_tuple const * src);

/**
   Extracts 0-based result column ndx from st and returns "the closest
   approximation" of its type in cwal_value form by assigning it to
   *rv. Returns 0 on success or a CWAL_RC_xxx value on error. On error
   *rv will be unmodified. Does not trigger a cwal exception on error.

   The caller must ensure that *rv is 0 when this is called, or risk
   triggering an assert() to that effect.
*/
int whx_stmt_to_value( whx_stmt * st, int ndx, cwal_value ** rv );

/**
   Extracts all columns from st into a new cwal_object value,
   using st's column names as the keys.

   The result object has a prototype of NULL to prevent any confusion
   between its own properties and derived ones. This means that it has
   no hasOwnProperty() method!

   Returns NULL on error.
*/
cwal_value * whx_stmt_row_to_object( whx_stmt * st );

/**
   Extracts all columns from st into a new cwal_object value.
   colNames is expected to hold the same number of entries as st has
   columns, and in the same order. The entries in colNames are used as
   the object's field keys.

   The result object has a prototype of NULL to prevent any confusion
   between its own properties and derived ones. This means that it has
   no hasOwnProperty() method!

   Returns NULL on error.
*/
cwal_value * whx_stmt_row_to_object2( whx_stmt * st, cwal_array const * colNames );

/**
   Appends all result columns from st to ar, converting each value
   using whx_stmt_to_value(). Returns 0 on success
*/
int whx_stmt_row_to_array2(whx_stmt * st, cwal_array * ar);

/**
   Converts all column values from st into a new cwal_array value. Returns
   NULL on error, else an array value.
*/
cwal_value * whx_stmt_row_to_array( whx_stmt * st );


/**
   Tuple counterpart of whx_stmt_row_to_array(), returning a
   cwal_tuple-type value on success.
*/
cwal_value * whx_stmt_row_to_tuple( whx_stmt * st );


#if defined(__cplusplus)
} /*extern "C"*/
#endif
#endif
/* NET_WANDERINGHORSE_WHCL_DB_H_INCLUDED */
