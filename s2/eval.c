/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* vim: set ts=2 et sw=2 tw=80: */
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include "s2_internal.h"
#include "wh/cwal/cwal_printf.h"

#if 1
#include <stdio.h>
#define MARKER(pfexp) if(1) printf("MARKER: %s:%d():\t",__FILE__,__LINE__); if(1) printf pfexp
/**
   Dumps info about (s2_ptoken const*) TP, prefixed by the (char const *) lbl.
*/
#define s2__dump_token(lbl,TP) \
  MARKER(("%s: token type %d/%s: %.*s\n", lbl,                          \
          (TP)->ttype, s2_ttype_cstr((TP)->ttype),                      \
          (int)s2_ptoken_len(TP), s2_ptoken_begin(TP)))
#else
#define MARKER(pfexp) (void)0
#define s2__dump_token(lbl,TP) (void)0
#endif

#define s2__err(SE) (SE)->e->err


/** @internal
   Internal representation of a stack trace entry.
*/
struct s2_strace_entry {
  /**
     The next entry "up" (older) in the stack.
  */
  s2_strace_entry * up;
  /**
     The next entry "down" (newer) in the stack.
  */
  s2_strace_entry * down;
  /**
     The tokenizer active when this entry is created.
   */
  s2_ptoker * pr;
  /**
     Active token when this entry is created.
  */
  s2_ptoken pos;
};

/**
   Empty-initilized s2_strace_entry object.
*/
#define s2_strace_entry_empty_m {                \
    0/*up*/, 0/*down*/, 0/*pr*/, s2_ptoken_empty_m/*pos*/   \
      }

/**
   Empty-initilized s2_strace_entry object.
*/
static const s2_strace_entry s2_strace_entry_empty = s2_strace_entry_empty_m;


const s2_func_state s2_func_state_empty = {
0/*vSrc*/,0/*vImported*/,0/*vName*/,
0/*keyScriptName*/,
0/*next*/,
0/*line*/,0/*col*/,
0/*flags*/
};

#define s2__ukwd(SE) ((s2_ukwd*)se->ukwd)
#define s2__ukwd_key "s2_engine::ukwd"
static void s2_ukwd_free2(s2_engine * se, s2_ukwd * u){
  if(u){
    cwal_free2(se->e, u->list, u->alloced * sizeof(s2_ukwd));
    u->h = 0/* owned by s2 stash */;
    cwal_free2(se->e, u, sizeof(s2_ukwd));
  }
}

void s2_ukwd_free(s2_engine * se){
  s2_ukwd_free2(se, se->ukwd);
  se->ukwd = 0;
}


cwal_hash * s2_fstash( s2_engine * se ){
  if(!se->funcStash){
    se->funcStash = cwal_new_hash(se->e, 53);
    if(se->funcStash){
      cwal_value * v = cwal_hash_value(se->funcStash);
      cwal_value_ref(v);
      cwal_value_make_vacuum_proof(v, 1);
      cwal_value_rescope( se->e->top, v );
    }
  }
  return se->funcStash;
}

static int s2_strace_push_pos( s2_engine * se,
                               s2_ptoker * pr,
                               s2_ptoken const * srcPos,
                               s2_strace_entry * ent ){
  if(se->strace.max && se->strace.count == se->strace.max-1){
    return s2_engine_err_set(se, CWAL_RC_RANGE,
                             "Stack depth too deep. Max is %"CWAL_SIZE_T_PFMT". "
                             "Potentially caused by infinite recursion.",
                             (cwal_size_t)se->strace.max);
  }
  if(!srcPos) srcPos = &pr->token;
  /**
     Reminder to self: we could potentially check for infinite
     recursion by using a combination of strace.count limits and
     comparing srcPos to prior entries in the list. It would turn this
     into an O(N) algorithm, though, with N=stack depth. Note that
     this only catches script-originated calls, not calls made into
     Functions via native code.
  */
  ent->pos = *srcPos;
  ent->pr = pr;
  if(se->strace.tail){
    assert(!ent->down);
    se->strace.tail->down = ent;
    ent->up = se->strace.tail;
    se->strace.tail = ent;
  }else{
    assert(!se->strace.head);
    se->strace.head = se->strace.tail = ent;
  }
  ++se->strace.count;
  return 0;
}

void s2_strace_pop( s2_engine * se ){
  assert(se->strace.count);
  assert(se->strace.tail);
  if(se->strace.count){
    s2_strace_entry * x = se->strace.tail;
    assert(!x->down);
    if(x->up){
      assert(x->up->down == x);
      se->strace.tail = x->up;
      x->up->down = NULL;
      x->up = NULL;
    }else{
      se->strace.head = se->strace.tail = NULL;
    }
    --se->strace.count;
  }else{
    s2_fatal( CWAL_RC_RANGE, "internal error: "
              "s2_strace_pop() called on empty stack.");
  }
}


int s2_strace_generate( s2_engine * se, cwal_value ** rv ){
  cwal_array * ar = 0;
  int rc = 0;
  s2_strace_entry * ent = se->strace.tail;
  cwal_size_t const oldCount = se->strace.count;
  /* MARKER(("se->strace.count=%u ent=%p\n", se->strace.count, (void const *)ent)); */
  if(!ent){
    *rv = 0;
    return 0;
  }
  se->strace.count = 0
    /* Workaround for co-dependency via
       s2_add_script_props() */;
  for( ; !rc && ent; ent = ent->up ){
    /* Generate array of stack trace entries */
    cwal_value * c;
    if(!ar){
      ar = cwal_new_array(se->e);;
      if(!ar){
        rc = CWAL_RC_OOM;
        break;
      }
      cwal_value_ref(cwal_array_value(ar));
#if 0
      rc = cwal_array_reserve(ar, oldCount);
      if(rc){
        break;
      }
#endif
    }
    c = cwal_new_object_value(se->e);
    if(!c){
      rc = CWAL_RC_OOM;
      break;
    }
    cwal_value_ref(c);
    rc = cwal_array_append(ar, c);
    cwal_value_unref(c);
    if(!rc){
      s2_ptoken const et = *s2_ptoker_errtoken_get(ent->pr);
      s2_ptoker_errtoken_set(ent->pr, &ent->pos);
      rc = s2_add_script_props(se, c, ent->pr);
      s2_ptoker_errtoken_set(ent->pr, &et);
    }
  }

  se->strace.count = oldCount;
  if(rc) cwal_array_unref(ar);
  else{
    cwal_value_unhand(cwal_array_value(ar));
    *rv = cwal_array_value(ar);
  }
  return rc;
}


static s2_func_state * s2_func_state_malloc( s2_engine * se ){
  s2_func_state * rc = se->recycler.scriptFuncs.head;
  ++se->metrics.funcStateRequests;
  if(rc){
    assert(se->recycler.scriptFuncs.count>0);
    se->recycler.scriptFuncs.head = rc->next;
    --se->recycler.scriptFuncs.count;
    rc->next = 0; 
  }else{
    rc = (s2_func_state *)cwal_malloc2(se->e, sizeof(s2_func_state));
    if(rc){
      se->metrics.funcStateMemory += sizeof(s2_func_state);
      cwal_engine_adjust_client_mem(se->e, (cwal_int_t)sizeof(s2_func_state));
      ++se->metrics.funcStateAllocs;
      *rc = s2_func_state_empty;
    }
  }
  return rc;
}

static void s2_func_state_free( s2_engine * se, s2_func_state * fs ){
  /* MARKER(("Finalizing function @ %p.\n", (void const *)fs)); */
  assert(!fs->next);
  if(fs->vSrc){
    cwal_value_unref(fs->vSrc);
    fs->vSrc = 0;
    }
  if(fs->vImported){
    cwal_value_unref(fs->vImported);
    fs->vImported = 0;
  }
  if(fs->vName){
    cwal_value_unref(fs->vName);
    fs->vName = 0;
  }
  if(se->funcStash){
    /* cwal_value_unref(cwal_hash_value(se->funcStash)); */
#if 0
    /*
      We have to leave the script names in the hashtable for the time being.
      We only allocate each one once, though. */
    if(fs->keyScriptName){
      cwal_hash_remove_v( se->funcStash, fs->keyScriptName );
    }
#endif
    /* if(fs->keyName) cwal_hash_remove_v( se->funcStash, fs->keyName ); */
  }else{
    /* Corner case: cleanup of se->e during s2_engine_finalize().
       se->funcStash might be gone by then, meaning our
       pointers would be dangling. No big deal here. */
    /* assert(!fs->vSrc); */
    /* assert(!fs->keyName); */
  }
  if(fs->keyScriptName){
    cwal_value_unref(fs->keyScriptName);
    fs->keyScriptName = 0;
  }
  *fs = s2_func_state_empty;
  if(se->recycler.scriptFuncs.max>0
     && se->recycler.scriptFuncs.count < se->recycler.scriptFuncs.max
     ){
    fs->next = se->recycler.scriptFuncs.head;
    se->recycler.scriptFuncs.head = fs;
    ++se->recycler.scriptFuncs.count;
  }else{
    cwal_free2(se->e, fs, sizeof(s2_func_state));
    cwal_engine_adjust_client_mem(se->e,
                                  -((cwal_int_t)sizeof(s2_func_state)));
  }
}


void s2_engine_free_recycled_funcs( s2_engine * se ){
  int const oldMax = se->recycler.scriptFuncs.max;
  s2_func_state * fs;
  se->recycler.scriptFuncs.max = 0;
  for( ; (fs = se->recycler.scriptFuncs.head); ){
    se->recycler.scriptFuncs.head = fs->next;
    fs->next = 0;
    assert(se->recycler.scriptFuncs.count);
    --se->recycler.scriptFuncs.count;
    s2_func_state_free( se, fs );
  }
  assert(!se->recycler.scriptFuncs.count);
  se->recycler.scriptFuncs.max = oldMax;
}

/**
   cwal finalizer for Function state. m must be a (s2_func_state*).
*/
static void cwal_finalizer_f_func_state( cwal_engine * e, void * m ){
  s2_func_state *fs = (s2_func_state*)m;
  s2_engine * se = s2_engine_from_state(e);
  assert(m);
  assert(se);
  s2_func_state_free( se, fs );
}

s2_func_state * s2_func_state_for_func(cwal_function * f){
  return (s2_func_state *)cwal_function_state_get(f, &s2_func_state_empty);
}


static int s2_keyword_f_FLC( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_assert( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_breakpoint( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_builtin_vals( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_continue( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_define( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_dowhile( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_echo( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_enum( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_eval( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_exception( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_for( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_foreach( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_function( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_if( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_import( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_nameof( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_new( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_pragma( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_refcount( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_reserved( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_s2out( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_typeinfo( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_typename( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_unset( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_ukwd( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_using( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_var( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);
static int s2_keyword_f_while( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv);


static const struct S2_KEYWORDS__ {
/*
  Keep sorted on keyword name (string) so that we can binary-search it.

  When adding new keywords, update all of:

  - Add a S2_T_KeywordXXX entry for it.

  - Add an entry in this struct for it, as well as an initializer in
    the array which follows.

  - Add it to the following functions:

  -- s2_ttype_cstr()
  -- s2_ttype_keyword()
  -- s2_ptoken_keyword()

  And to s2-keyword-hasher.s2 (to generate the body of s2_ptoken_keyword()
  and test the keyword hash algo for collisions).
*/
  s2_keyword const _breakpoint;
  s2_keyword const _col;
  s2_keyword const _file;
  s2_keyword const _filedir;
  s2_keyword const _flc;
  s2_keyword const _line;
  s2_keyword const affirm;
  s2_keyword const assert_;
  s2_keyword const break_;
  s2_keyword const catch_;
  s2_keyword const class_;
  s2_keyword const const_;
  s2_keyword const continue_;
  s2_keyword const define;
  s2_keyword const defined_;
  s2_keyword const delete_;
  s2_keyword const doWhile;
  s2_keyword const echo;
  s2_keyword const enum_;
  s2_keyword const eval;
  s2_keyword const exception_;
  s2_keyword const exit_;
  s2_keyword const false_;
  s2_keyword const fatal_;
  s2_keyword const for_;
  s2_keyword const foreach_;
  s2_keyword const function_;
  s2_keyword const if_;
  s2_keyword const import;
  s2_keyword const include;
  s2_keyword const inherits;
  s2_keyword const interface_;
  s2_keyword const is_;
  s2_keyword const isa_;
  s2_keyword const nameof;
  s2_keyword const new_;
  s2_keyword const null_;
  s2_keyword const pragma;
  s2_keyword const private_;
  s2_keyword const proc_;
  s2_keyword const protected_;
  s2_keyword const public_;
  s2_keyword const refcount_;
  s2_keyword const return_;
  s2_keyword const s2out;
  s2_keyword const scope;
  s2_keyword const static_;
  s2_keyword const throw_;
  s2_keyword const true_;
  s2_keyword const try_;
  s2_keyword const typeInfo;
  s2_keyword const typeName;
  s2_keyword const undef_;
  s2_keyword const unset;
  s2_keyword const using;
  s2_keyword const var;
  s2_keyword const while_;
  s2_keyword const _sentinel_;
} S2_KWDS = {
/*{ id                      word,         wordLen,  call(), allowEOLAsEOXWhenLHS } */
  { S2_T_KeywordBREAKPOINT, "__BREAKPOINT", 12, s2_keyword_f_breakpoint, 0 },
  { S2_T_KeywordCOLUMN,     "__COLUMN",   8, s2_keyword_f_FLC,           0 },
  { S2_T_KeywordFILE,       "__FILE",     6, s2_keyword_f_FLC,           0 },
  { S2_T_KeywordFILEDIR,    "__FILEDIR",  9, s2_keyword_f_FLC,           0 },
  { S2_T_KeywordSRCPOS,     "__FLC",      5, s2_keyword_f_FLC,           0 },
  { S2_T_KeywordLINE,       "__LINE",     6, s2_keyword_f_FLC,           0 },

  { S2_T_KeywordAffirm,     "affirm",     6, s2_keyword_f_assert,        0 },
  { S2_T_KeywordAssert,     "assert",     6, s2_keyword_f_assert,        0 },
  { S2_T_KeywordBreak,      "break",      5, s2_keyword_f_eval,          0 },
  { S2_T_KeywordCatch,      "catch",      5, s2_keyword_f_eval,          0 },
  { S2_T_KeywordClass,      "class",      5, s2_keyword_f_reserved,      0 },
  { S2_T_KeywordConst,      "const",      5, s2_keyword_f_var,           0 },
  { S2_T_KeywordContinue,   "continue",   8, s2_keyword_f_continue,      0 },
  { S2_T_KeywordDefine,     "define",     6, s2_keyword_f_define,        0 },
  { S2_T_KeywordDefined,    "defined",    7, s2_keyword_f_reserved,      0 },
  { S2_T_KeywordDelete,     "delete",     6, s2_keyword_f_reserved,      0 },
  { S2_T_KeywordDo,         "do",         2, s2_keyword_f_dowhile,       1 },
  { S2_T_KeywordEcho,       "echo",       4, s2_keyword_f_echo,          0 },
  { S2_T_KeywordEnum,       "enum",       4, s2_keyword_f_enum,          0 },
  { S2_T_KeywordEval,       "eval",       4, s2_keyword_f_eval,          1 },
  { S2_T_KeywordException,  "exception",  9, s2_keyword_f_exception,     0 },
  { S2_T_KeywordExit,       "exit",       4, s2_keyword_f_eval,          0 },
  { S2_T_KeywordFalse,      "false",      5, s2_keyword_f_builtin_vals,  0 },
  { S2_T_KeywordFatal,      "fatal",      5, s2_keyword_f_eval,          0 },
  { S2_T_KeywordFor,        "for",        3, s2_keyword_f_for,           1 },
  { S2_T_KeywordForEach,    "foreach",    7, s2_keyword_f_foreach,       1 },
  { S2_T_KeywordFunction,   "function",   8, s2_keyword_f_function,      0 },
  { S2_T_KeywordIf,         "if",         2, s2_keyword_f_if,            1 },  
  { S2_T_KeywordImport,     "import",     6, s2_keyword_f_import,        0 },
  { S2_T_KeywordInclude,    "include",    7, s2_keyword_f_reserved,      0 },
  { S2_T_OpInherits,        "inherits",   8, 0 /* handled as an operator */, 0 },
  { S2_T_KeywordInterface,  "interface",  9, s2_keyword_f_reserved,      0 },
  { S2_T_KeywordIs,         "is",         2, s2_keyword_f_reserved,      0 },
  { S2_T_KeywordIsA,        "isa",        3, s2_keyword_f_reserved,      0 },
  { S2_T_KeywordNameof,     "nameof",     6, s2_keyword_f_nameof,        0 },
  { S2_T_KeywordNew,        "new",        3, s2_keyword_f_new,           0 },
  { S2_T_KeywordNull,       "null",       4, s2_keyword_f_builtin_vals,  0 },
  { S2_T_KeywordPragma,     "pragma",     6, s2_keyword_f_pragma,        0 },
  { S2_T_KeywordPrivate,    "private",    7, s2_keyword_f_reserved,      0 },  
  { S2_T_KeywordProc,       "proc",       4, s2_keyword_f_function,      0
    /*
      20171115: Interesting: while using require.s2 to import a proc
      from a file which contained only that proc (and thus evaluates
      to that proc), the allowEOLAsEOXWhenLHS handling here uncovered,
      for the first time, that proc does not like a trailing EOL when
      it's the left-most part of an expression. That usage never
      happens except in the case of import()ing or
      s2_set_from_script()'ing a proc, and was never witnessed until
      today. If we enable allowEOLAsEOXWhenLHS for procs, though,
      then we'll almost cerainly get bitten someday by something like:

      proc(){...}
      .importSymbols(...)

      Which terminates the expression after the proc and leads to
      "illegal operator '.' at start of expression" before the
      next line.

      So... for that corner case we'll just have to use semicolons
      after LHS-most procs or (if the context allows) add a return
      statement before the proc keyword. An LHS proc which is
      immediately called does not demonstrate this problem because the
      proc itself is not both the LHS and final expression component:

      proc(){...}()
     */
  },
  { S2_T_KeywordProtected,  "protected",  9, s2_keyword_f_reserved,      0 },
  { S2_T_KeywordPublic,     "public",     6, s2_keyword_f_reserved,      0 },
  { S2_T_KeywordRefcount,   "refcount",   8, s2_keyword_f_refcount,      0 },
  { S2_T_KeywordReturn,     "return",     6, s2_keyword_f_eval,          0 },
  { S2_T_KeywordS2Out,      "s2out",      5, s2_keyword_f_s2out,         0 },
  { S2_T_KeywordScope,      "scope",      5, s2_keyword_f_eval,          1 },
  { S2_T_KeywordStatic,     "static",     6, s2_keyword_f_reserved,      0 },
  { S2_T_KeywordThrow,      "throw",      5, s2_keyword_f_eval,          0 },
  { S2_T_KeywordTrue,       "true",       4, s2_keyword_f_builtin_vals,  0 },
  { S2_T_KeywordTry,        "try",        3, s2_keyword_f_reserved,      0 },
  { S2_T_KeywordTypeinfo,   "typeinfo",   8, s2_keyword_f_typeinfo,      0 },
  { S2_T_KeywordTypename,   "typename",   8, s2_keyword_f_typename,      0 },
  { S2_T_KeywordUndefined,  "undefined",  9, s2_keyword_f_builtin_vals,  0 },
  { S2_T_KeywordUnset,      "unset",      5, s2_keyword_f_unset,         0 },
  { S2_T_KeywordUsing,      "using",      5, s2_keyword_f_using,         0 },
  { S2_T_KeywordVar,        "var",        3, s2_keyword_f_var,           0 },
  { S2_T_KeywordWhile,      "while",      5, s2_keyword_f_while,         1 },

  {/*_sentinel_*/0,0,0,0,0}
};

static s2_keyword const * s2_ttype_keyword( int ttype ){
  switch( ttype ){
#define WORD(E,Member) case E: return &S2_KWDS.Member
    WORD(S2_T_KeywordBREAKPOINT,_breakpoint);
    WORD(S2_T_KeywordCOLUMN,_col);
    WORD(S2_T_KeywordFILE,_file);
    WORD(S2_T_KeywordFILEDIR,_filedir);
    WORD(S2_T_KeywordSRCPOS,_flc);
    WORD(S2_T_KeywordLINE,_line);

    WORD(S2_T_KeywordAffirm,affirm);
    WORD(S2_T_KeywordAssert,assert_);
    WORD(S2_T_KeywordBreak,break_);
    WORD(S2_T_KeywordCatch,catch_);
    WORD(S2_T_KeywordClass,class_);
    WORD(S2_T_KeywordConst,const_);
    WORD(S2_T_KeywordContinue,continue_);
    WORD(S2_T_KeywordDefine,define);
    WORD(S2_T_KeywordDefined,defined_);
    WORD(S2_T_KeywordDelete,delete_);
    WORD(S2_T_KeywordDo,doWhile);
    WORD(S2_T_KeywordEcho,echo);
    WORD(S2_T_KeywordEnum,enum_);
    WORD(S2_T_KeywordEval,eval);
    WORD(S2_T_KeywordException,exception_);
    WORD(S2_T_KeywordExit,exit_);
    WORD(S2_T_KeywordFalse,false_);
    WORD(S2_T_KeywordFatal,fatal_);
    WORD(S2_T_KeywordFor,for_);
    WORD(S2_T_KeywordForEach,foreach_);
    WORD(S2_T_KeywordFunction,function_);
    WORD(S2_T_KeywordIf,if_);
    WORD(S2_T_KeywordImport,import);
    WORD(S2_T_KeywordInclude,include);
    WORD(S2_T_OpInherits,inherits);
    WORD(S2_T_KeywordInterface,interface_);
    WORD(S2_T_KeywordIs,is_);
    WORD(S2_T_KeywordIsA,isa_);
    WORD(S2_T_KeywordNameof,nameof);
    WORD(S2_T_KeywordNew,new_);
    WORD(S2_T_KeywordNull,null_);
    WORD(S2_T_KeywordPragma,pragma);
    WORD(S2_T_KeywordPrivate,private_);
    WORD(S2_T_KeywordProc,proc_);
    WORD(S2_T_KeywordProtected,protected_);
    WORD(S2_T_KeywordPublic,public_);
    WORD(S2_T_KeywordRefcount,refcount_);
    WORD(S2_T_KeywordReturn,return_);
    WORD(S2_T_KeywordS2Out,s2out);
    WORD(S2_T_KeywordScope,scope);
    WORD(S2_T_KeywordThrow,throw_);
    WORD(S2_T_KeywordTrue,true_);
    WORD(S2_T_KeywordTry,try_);
    WORD(S2_T_KeywordTypeinfo,typeInfo);
    WORD(S2_T_KeywordTypename,typeName);
    WORD(S2_T_KeywordUndefined,undef_);
    WORD(S2_T_KeywordUnset,unset);
    WORD(S2_T_KeywordUsing,using);
    WORD(S2_T_KeywordVar,var);
    WORD(S2_T_KeywordWhile,while_);

#undef WORD
    default:
      return 0;
  }
}

#define s2__keyword_perfect_hash( PTOKEN ) \
  s2_hash_keyword(s2_ptoken_begin(PTOKEN), s2_ptoken_len(PTOKEN))
uint32_t s2_hash_keyword( char const * input, cwal_size_t len ){
  cwal_size_t i = 0;
  uint64_t h = 0;
  char const * pos = input;
  for( ; i < len; ++i, ++pos ){
    /*All of these variations have worked out so far...
      h = (h << 1) + 100*i + i*pt->begin[i] - i*35;
      h = (h << 1) + 100*i + i*pt->begin[i] - i*35;
      h = (h << 1) + 45*i + pt->begin[i] - 35;

      Note that the magic 35==ASCII '$'-1, the lowest-numbered
      character allowed in an s2 keyword.

      ************************************************************
      WARNING: this algo MUST be kept in sync with the one in
      s2-keyword-hasher.s2, as that script generates the C code
      for our keyword/typeinfo/pragma bits.
      ************************************************************
    */
    if(*pos > 'z' || *pos < '$') return 0;
    h = (h << 1) + (i+1) * (*pos - 35/*==>ASCII '$'-1*/);
    while(h > (uint64_t)0x7fffffff)
      h = h>>1
        /* With a uint64_t hash, trim hash to within 32 bits because
           we need to be able to use these values in switch/case, and
           64 bits are not portable for that. We *have* to use 64-bit
           integers for the calculation because they're also
           calculated in script-space, where we use (by and large)
           64-bit builds.
        */;
  }
  return (uint32_t)h;
}


/**
   If pt's [begin,end) range corresponds to a keyword, its entry from
   S2_KWDS is returned, else NULL is returned.

   This is an O(1) search, requiring, at most, generation of 1 hash
   code and (on a hash match) 1 string comparison.
*/
static s2_keyword const * s2_ptoken_keyword( s2_ptoken const * pt ){
  const cwal_size_t tlen = s2_ptoken_len(pt);
  if(tlen > sizeof("__BREAKPOINT"/*must be the longest keyword!*/)-1) return NULL;
  switch(s2__keyword_perfect_hash(pt)){
#define W(X,M) return tlen==(cwal_size_t)sizeof(X)-1 &&     \
      0==cwal_compare_cstr(s2_ptoken_begin(pt), tlen, X, sizeof(X)-1)   \
      ? &S2_KWDS.M : NULL
    /* Generated by s2-keyword-hasher.s2 (or equivalent): */

    case 0x0609ce: W("__BREAKPOINT",_breakpoint);
    case 0x0061bc: W("__COLUMN",_col);
    case 0x00170e: W("__FILE",_file);
    case 0x00c013: W("__FILEDIR",_filedir);
    case 0x000b0c: W("__FLC",_flc);
    case 0x0017b2: W("__LINE",_line);
    case 0x001f9a: W("affirm",affirm);
    case 0x00225c: W("assert",assert_);
    case 0x000f50: W("break",break_);
    case 0x000f05: W("catch",catch_);
    case 0x000f88: W("class",class_);
    case 0x001059: W("const",const_);
    case 0x008ee4: W("continue",continue_);
    case 0x001f82: W("define",define);
    case 0x0040cb: W("defined",defined_);
    case 0x00200e: W("delete",delete_);
    case 0x00011a: W("do",doWhile);
    case 0x0006de: W("echo",echo);
    case 0x00077c: W("enum",enum_);
    case 0x000740: W("eval",eval);
    case 0x011e4b: W("exception",exception_);
    case 0x0007a0: W("exit",exit_);
    case 0x000f46: W("false",false_);
    case 0x000f39: W("fatal",fatal_);
    case 0x000329: W("for",for_);
    case 0x00448b: W("foreach",foreach_);
    case 0x009058: W("function",function_);
    case 0x000112: W("if",if_);
    case 0x0022f4: W("import",import);
    case 0x0044a2: W("include",include);
    case 0x008cb6: W("inherits",inherits);
    case 0x01211a: W("interface",interface_);
    case 0x00012c: W("is",is_);
    case 0x000312: W("isa",isa_);
    case 0x0020ba: W("nameof",nameof);
    case 0x000330: W("new",new_);
    case 0x0007c2: W("null",null_);
    case 0x0021e8: W("pragma",pragma);
    case 0x0048f2: W("private",private_);
    case 0x0007a8: W("proc",proc_);
    case 0x012d65: W("protected",protected_);
    case 0x002294: W("public",public_);
    case 0x008bd2: W("refcount",refcount_);
    case 0x0023b0: W("return",return_);
    case 0x000da5: W("s2out",s2out);
    case 0x001042: W("scope",scope);
    case 0x00233c: W("static",static_);
    case 0x001118: W("throw",throw_);
    case 0x0007f4: W("true",true_);
    case 0x000382: W("try",try_);
    case 0x0098e2: W("typeinfo",typeInfo);
    case 0x009884: W("typename",typeName);
    case 0x011f6d: W("undefined",undef_);
    case 0x001135: W("unset",unset);
    case 0x001114: W("using",using);
    case 0x000331: W("var",var);
    case 0x00106a: W("while",while_);

    default: break;
#undef W
  }
  return NULL;
}


/**
   Comparison for bsearch() which compares keywords
   by name.
*/
static int cmp_ukwd_kw(void const * lhs, void const * rhs){
  s2_keyword const * l = (s2_keyword const *)lhs;
  s2_keyword const * r = (s2_keyword const *)rhs;
  return cwal_compare_cstr(l->word, l->wordLen,
                           r->word, r->wordLen);
}

/**
   A variant of s2_ptoken_keyword() which first calls that function,
   and if it returns NULL then this function looks for user-defined
   keywords, returning one if found, else returning NULL.
*/
static s2_keyword const *
s2_ptoken_keyword2( s2_engine * se, s2_ptoken const * pt ){
  s2_keyword const * rc = s2_ptoken_keyword(pt);
  if(!rc){
    s2_keyword dummy;
    cwal_size_t tLen = 0;
    s2_ukwd * const uk = s2__ukwd(se);
    if(!uk || !uk->count) return 0;
    dummy.word = s2_ptoken_cstr(pt, &tLen);
    if(tLen>(unsigned short)-1/*overflow*/) return 0;
    dummy.wordLen = (unsigned short)tLen;
    ++se->metrics.ukwdLookups;
    if(1==uk->count){
      /* Fast-track it! */
      rc = 0==cmp_ukwd_kw(&uk->list[0], &dummy)
        ? &uk->list[0]
        : 0;
    }else{
      rc = (s2_keyword const*)bsearch(&dummy, uk->list, uk->count,
                                      sizeof(s2_keyword), cmp_ukwd_kw);
    }
    if(rc) ++se->metrics.ukwdHits;
  }
  return rc;
}



#if 0
/**
   If ttype matches a keyword's token type, ttype is returned, else 0
   is returned. Note that this returns non-0 for S2_T_OpInherits: the
   "inherits" keyword is partially a keyword, partially an operator.
 */
int s2_ttype_is_keyword( int ttype );
int s2_ttype_is_keyword( int ttype ){
  return s2_ttype_keyword(ttype) ? ttype : 0;
}
#endif

/**
   Internal-only flags for s2_eval_expr() and friends.
*/
enum s2_eval_flags3_t {
/*
   Maintenance reminder: flags must be greater than the highest flag
   in s2_eval_flags.
*/
/**
   Tells s2_eval_expr() to treat the _first_ unresolved identifier as
   the undefined value instead of an error. Used by 'typename' and
   typeinfo(name ...).
*/
S2_EVAL_UNKNOWN_IDENTIFIER_AS_UNDEFINED = 1 << S2_EVAL_flag_bits,
/**
   Tells s2_eval_expr() not to skip the first EOL token.
*/
S2_EVAL_NO_SKIP_FIRST_EOL = 2 << S2_EVAL_flag_bits,
/**
   Unused/untested: tells s2_eval_expr() to sweep up before
   evaluation.
*/
S2_EVAL_PRE_SWEEP = 4 << S2_EVAL_flag_bits,
/**
   Tells s2_eval_expr() that an empty parens group
   is legal. Only used to support allow return(),
   so it's a candidate for removal.
*/
S2_EVAL_EMPTY_GROUP_OK = 8 << S2_EVAL_flag_bits,
/**
   Experimental, doesn't work/do anything useful.
*/
S2_EVAL_KEEP_STACK = 0x10 << S2_EVAL_flag_bits,
/**
   To assist in the 'new' keyword. Tells s2_eval_expr_impl() to stop
   at a call-like operation, as new() handles the arguments itself.
*/
S2_EVAL_STOP_AT_CALL = 0x20 << S2_EVAL_flag_bits,
/**
   Experimental: tells s2_eval_expr_impl() to not clear
   s2_dotop_state() before returning (which it normally does unless a
   fromLhsOp argument is passed to it). Used by (unset x.y) so that it
   can get ahold of the 'this' part of the expression.
*/
S2_EVAL_RETAIN_DOTOP_STATE = 0x40 << S2_EVAL_flag_bits,
/**
   Tells eval that a trailing semicolon is not allowed. This is
   primarily to allow [array] and {object} literals to catch
   semicolons after the value parts of their inner expressions.

   e.g. without this then:

   [1;,2] evals to [1,2] because the semicolon legally terminates the
   1. Similarly, {a:1;, b:2;} is legal without this flag.

   Whether or not this is really a fix is arguable, as a semicolon
   legally ends any expression. It was added more for pedandicness'
   sake than to fix a problem. (It was only noticed by accident one
   day that a trailing semicolon in an array literal was not being
   caught as a syntax error.)
*/
S2_EVAL_EOX_SEMICOLON_NOT_ALLOWED = 0x80 << S2_EVAL_flag_bits
};


static int s2_eval_expr_impl( s2_engine * se,
                              s2_ptoker * st,
                              s2_op const * fromLhsOp,
                              int evalFlags,
                              cwal_value ** rv);

/**
   An eval-level helper to check for interruption, which should trump
   any result of just-performed evaluation.

   If rc is not 0...

   - if rv then *rv is set to 0.

   - potential todo: if *rv then *rv is passed to cwal_refunref(),
   which may or may not clean it up immediately.

   If rc is 0, it rescopes *rv (if both rv and *rv are not NULL) to sc
   (if not NULL).

   Always returns rc except when se->flags.interrupted trumps it,
   in which case that non-0 code is returned.
*/
static int s2_rv_maybe_accept( s2_engine * se, cwal_scope * sc,
                                int rc, cwal_value ** rv ){
  rc = s2_check_interrupted(se, rc);
  switch(rc){
    case 0:
      assert(se->e->current);
      if(rv && *rv && sc) cwal_value_rescope(sc, *rv);
      break;
    default:
      break;
  }
  if(rc && rv){
#if 0
    if(*rv){
      /* This can theoretically (if all *rv-setters are well-behaved)
         only happen when s2_check_interrupted() trumps an otherwise
         successful operation. This block is not triggering in test
         code, and would be difficult to trigger for testing, so it's
         currently disabled. */
      assert(!"untested");
      cwal_refunref(*rv);
    }
#endif
    *rv = 0;
  }
  return rc;
}


int s2_err_ammend_flc(s2_engine * se, s2_ptoker const * st) /* Declared in s2.c */;

/**
   Internal helper for ammending stack-machine-level errors with
   file/line/column info. The 3rd argument is the error code of the op
   which just failed.

   Preconditions:

   - A stack-processing op must just have failed.

   - rc must be non-0.

   Preferably, se->err.msg is not empty.

   If !pr, se->currentScript is used. It asserts() that at least one
   of those is non-NULL.

   Returns, on success, se->err.code, and some other non-0 value if
   ammending the FLC info led to a more serious error (e.g. out of
   memory or an interrupt). It will only return 0 if it is called when
   se->err.code is 0, and in that case it will assert() in debug
   builds.
*/
static int s2_ammend_op_err(s2_engine * se, s2_ptoker const * pr, int rc){
  int const rcInterrupt = s2_check_interrupted(se,0); 
  assert(pr || se->currentScript);
  if(rcInterrupt) return rcInterrupt;
  switch(rc){
    case CWAL_RC_ASSERT:
    case CWAL_RC_EXCEPTION:
    case CWAL_RC_EXIT:
    case CWAL_RC_FATAL:
    case CWAL_RC_INTERRUPTED:
    case CWAL_RC_OOM:
      break;
    case CWAL_SCR_SYNTAX: /* Not QUITE the condition i want, but it
                             keeps existing tests running for the time
                             being. */
      assert(s2__err(se).code);
      rc = s2_err_ammend_flc(se, pr ? pr : se->currentScript);
      break;
    default:
      /* Translate it to an exception... */
      assert(s2__err(se).code);
      /* MARKER(("opErrPos=%p\n", (void const *)se->opErrPos)); */
      rc = s2_throw_err_ptoker(se, pr ? pr : se->currentScript);
      break;
  }
  se->opErrPos = 0
    /* Avoid a stale pointer in some rare cases (namely s2sh REPL loop).
       Stale pointer led to an assert() in s2_t10n's line-counting code
       via this s2sh session:

       var e = enum e{a}
       e#'a'                    // (1) op error here
       e = enum e{#a}
       catch e = enum e{#a}
       catch { e = enum e{#a} } // syntax error here referenced (1).
     */;
  return rc;

}


void s2_dotop_state( s2_engine * se, cwal_value * self,
                     cwal_value * lhs, cwal_value * key ){
    
#if 0
  /* Leave this around in case we have to go ref hunting
     sometime. */
    se->dotOp.self = self;
    se->dotOp.lhs = lhs;
    se->dotOp.key = key;
#else
    /* Reminder: the validity checks here point to, essentially,
       misuse in s2, where we've not cleaned up this state before it
       goes stale.

       A potential future problem is vacuuming-up of these pointers,
       which case we can't solve without a per-s2_scope
       array/container to hold these and make them vacuum-proof.
    */
    cwal_value * oldSelf = se->dotOp.self;
    cwal_value * oldLhs = se->dotOp.lhs;
    cwal_value * oldKey = se->dotOp.key;
    /**
       Assert that all of them appear to still be valid references
       (because it's easy to mess that up). These assertions are not
       guaranteed to trigger in all error cases, but they catch the
       most common one that we've prematurely unref'd a value and he
       have a pointer to its cwal-side recycling bin.
    */
    /* MARKER(("Setting se->dotOpXXX...\n")); */
    if(oldSelf){
        assert( cwal_value_scope(oldSelf)
                || cwal_value_is_builtin(oldSelf) );
    }
    if(oldLhs){
        assert( cwal_value_scope(oldLhs)
                || cwal_value_is_builtin(oldLhs));
    }
    if(oldKey){
        assert( cwal_value_scope(oldKey)
                || cwal_value_is_builtin(oldKey) );
    }
    /**
       Because any of self/lhs/key can refer to or contain/own any
       other, as well as be the same instance of oldSelf/oldLhs/oldKey
       (in any combination!), we have to ref them all before we unref
       any of them.
    */
    if(self) cwal_value_ref(self);
    if(lhs) cwal_value_ref(lhs);
    if(key) cwal_value_ref(key);

    se->dotOp.self = self;
    se->dotOp.lhs = lhs;
    se->dotOp.key = key;

    if(oldSelf) cwal_value_unref(oldSelf);
    if(oldLhs) cwal_value_unref(oldLhs);
    if(oldKey) cwal_value_unref(oldKey);
    /* MARKER(("Done setting se->dotOpXXX\n")); */
#endif
}

/**
   Internal helper for s2_eval_expr().

   Possibly processes pending operators in se's stack, depending on op
   and its precedence in relation to the operator (if any) to the left
   (i.e. in s2's operator stack). Returns 0 on success (which includes
   it doing nothing of note).

   Specifically: if se->st has a pending operator (and operand(s))
   with a higher priority than op, or the same priority but the
   operator is left-associative, then that pending operator is
   processed. This repeats, if needed, to resolve all pending LHS ops
   until se->st is out of ops or we hit an op with a lower precedence
   than the given op (or equal precedence but not left-associative).
*/
static int s2_eval_lhs_ops(s2_engine * se, s2_ptoker const * pr, s2_op const * op){
  s2_stoken * topOpTok = s2_engine_peek_op(se);
  s2_op const * topOp = topOpTok ? s2_stoken_op(topOpTok) : 0;
  int rc = s2_check_interrupted(se, 0);
  assert(op);
  if(rc) return rc;
  else if(topOp
     && topOp->placement!=0
     && topOp->arity>0
     && (s2_ttype_is_assignment(op->id) || s2_ttype_is_assignment_combo(op->id))
     ){
    return s2_err_ptoker( se, pr, CWAL_SCR_SYNTAX,
                          "Invalid operator '%s' preceeding '%s'.",
                          topOp->sym, op->sym);
    /* This is admittedly a workaround for the X.Y=Z family of ops,
       which do not get a chance to set se->dotLhsOp and se->dotLhsKey
       before the ++ is run. It turns out that JavaScript disallows
       such ops on the LHS of an assignment, too.
     */
  }
  while(topOp &&
        ((op->prec < topOp->prec)
         || (op->assoc<0 && (op->prec==topOp->prec)))
        ){
    if(se->flags.traceTokenStack){
      MARKER(("Processing ge-precedent op '%s' to "
              "the left of '%s'.\n",
              topOp->sym, op->sym));
      s2_dump_val(se->dotOp.lhs,"se->dotOp.lhs");
      s2_dump_val(se->dotOp.key,"se->dotOp.key");
    }
    rc = s2_process_top(se);
    if(rc){
      rc = s2_ammend_op_err(se, 0, rc);
      assert(rc);
      break;
    }
    assert(se->st.vals.size>0);
    topOpTok = s2_engine_peek_op(se);
    topOp = topOpTok ? s2_stoken_op(topOpTok) : 0;
  }
  return rc;
}

/**
   Evaluates the contents of pr->token, treating it like a sub-parser.

   If asExpr is true it uses s2_eval_expr_impl() to process one
   expression, otherwise it uses s2_eval_ptoker() to parse it all as a
   collection of expressions.
*/
static int s2_eval_current_token( s2_engine * se, s2_ptoker * pr,
                                  char asExpr,
                                  int evalFlags,
                                  cwal_value **rv ){
  int rc = s2_check_interrupted(se, 0);
  s2_ptoker_errtoken_set(pr, 0);
  /* assert(!name || nameLen>0); */
  if(rc) return rc;
  else if(!s2_ptoken_has_content(&pr->token)){
    /* we know it's empty, so don't bother.*/
    if(rv) *rv = 0
             /* not the undefined value, so that the caller can
                differentiate empty expressions/bodies (which might
                not be legal in a given context). */;
    return 0;
  }else{
    s2_ptoker sub = s2_ptoker_empty;
    int rc = s2_ptoker_sub_from_toker(pr, &sub);
    assert(sub.parent == pr);
    /* 20200902: breaks stuff: assert(pr->e); */
    assert(sub.e == pr->e);
    if(rc){
      s2_ptoker_errtoken_set(pr, &pr->token) /* TODO: remove this (and test it). */;
      goto end;
    }
    else if(asExpr){
      rc = s2_eval_expr_impl( se, &sub, 0, evalFlags, rv );
    }
    else{
      s2_subexpr_savestate save = s2_subexpr_savestate_empty_m;
      s2_engine_subexpr_save(se, &save)
                      /* Saves/resets ternary level */;
      rc = s2_eval_ptoker( se, &sub,
                           evalFlags ? evalFlags : S2_EVALP_F_PROPAGATE_RETURN,
                           rv );
      s2_engine_subexpr_restore(se, &save);
    }
    if(rc) s2_ptoker_errtoken_set(pr, s2_ptoker_errtoken_get(&sub));
    pr->capture = sub.capture;
    end:
    s2_ptoker_finalize(&sub);
    return rc;
  }
}

/**
   Intended to be called at the end of a subexpression if:

   a) that subexpression generated an error which _might_ be one of
   CWAL_RC_BREAK/RETURN/CONTINUE.

   b) wants to report that as an error, using the tokenizer's current
   location information.

   Alternately, they can let it propagate and hope that it's handled
   higher up.

   The 2nd parameter is the tokenizer in which (or through which) the
   error was triggered.

   The 3rd parameter is the result code of the subexpression which
   failed.  If it is one of CWAL_RC_BREAK/RETURN/CONTINUE then this
   function clears any propagating value (if needed) and triggers a
   tokenizer error. All other values of rc have no side-effects.

   The 4th parameter is a string snippet used for the error report.
   It should be a brief description of the context, suitable for
   appending to a string in the form "Unhandled 'break' in ...".

   The "new" rc is returned, which will be either rc itself or, on
   allocation error while setting the error state, CWAL_RC_OOM.
*/
static int s2_check_brc( s2_engine * se,
                         s2_ptoker * pr,
                         int rc,
                         char const * contextDescr){
  s2_keyword const * kword = 0;
  switch(rc){
    case CWAL_RC_RETURN:
      kword = s2_ttype_keyword(S2_T_KeywordReturn);
      assert(kword);
      s2_propagating_set(se, 0);
      /* s2_engine_err_reset(se); */
      break;
    case CWAL_RC_BREAK:
      kword = s2_ttype_keyword(S2_T_KeywordBreak);
      assert(kword);
      s2_propagating_set(se, 0);
      /* s2_engine_err_reset(se); */
      break;
    case CWAL_RC_CONTINUE:
      kword = s2_ttype_keyword(S2_T_KeywordContinue);
      assert(kword);
      /* s2_engine_err_reset(se); */
      break;
    default: break;
  }
  if(kword){
    rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                       /* if we pass on the same rc, error reporting
                          won't DTRT. */
                       "Unhandled '%s' in %s.",
                       kword->word, contextDescr);
  }
  return rc;
}

/**
   Like s2_eval_current_token(se, pr, 0, 0, rv), but pushes a scope and
   sets a local "this" variable to the self value. It is intended to
   be called only for the context of the code block in:

   new X() {this block}

   pr->token MUST of type S2_T_SquigglyBlock: this function assert()s
   it (only to simplify skip-mode handling, though we could extend it
   to handle other block-level constructs as well).

   This function treats script-propagated return/break/continue
   conditions as an error.

   ACHTUNG: self must not be a temporary value: it MUST have a ref
   or this will kill it. Exception: in skip-mode, self is not
   evaluated.
*/
static int s2_eval_current_token_with_this( s2_engine * se, s2_ptoker * pr,
                                            cwal_value * self,
                                            cwal_value **rv ){
  assert( S2_T_SquigglyBlock==pr->token.ttype );
  if(se->skipLevel){
    if(rv) *rv = cwal_value_undefined();
    return 0;
  }else{
    cwal_scope sc = cwal_scope_empty;
    cwal_value * xrv = 0;
    int rc = cwal_scope_push2(se->e, &sc);
    if(rc) return rc;
    rc = s2_var_decl_v(se, se->cache.keyThis, self, 0);
    if(!rc){
      rc = s2_eval_current_token(se, pr, 0,
                                 0, rv ? &xrv : NULL);
    }
    cwal_scope_pop2(se->e, rc ? 0 : xrv);
    if(!rc && rv) *rv = xrv
                    /* will be NULL on an empty expr except in
                       skip-mode */;
    if(rc){
      rc = s2_check_brc( se, pr, rc,
                         "'new' post-ctor script block" );
    }
    return rc;
  }
}


int s2_eval_expr( s2_engine * se, s2_ptoker * st,
                  int evalFlags,
                  cwal_value ** rv){
  return s2_eval_expr_impl(se, st, 0, evalFlags, rv);
}

/**
   Expects pr to be a comma-separated list of expressions, until its
   EOF. Empty expressions and a stray trailing comma are not
   allowed. Evaluates each token and appends it to dest. Returns 0 on
   success. Some errors are thrown as exceptions, but syntax errors
   are not and propagated errors might not be exceptions.

   If allowAtExpansion is true then it allows a minor syntax expansion:

   Any _array value_ (including deriving from Array) in the list may
   start with the @ symbol. Its slot in the array is replaced with the
   entries from the array value. e.g.

   print(@[1,2,@[3]]) ==> print(1,2,3)

   If a non-array comes after a @, an exception is triggered. An empty
   array causes no slots to be filled.
*/
static int s2_eval_to_array( s2_engine * se, s2_ptoker * pr, cwal_array * dest,
                             int allowAtExpansion){
  int rc = s2_check_interrupted(se, 0);
  s2_op const * opComma = s2_ttype_op(S2_T_Comma);
  s2_ptoken prev = s2_ptoken_empty;
  char const * errMsg = 0;
  assert(!se->skipLevel);
  /* s2_dotop_state(se, 0, 0, 0); */
  while( !rc ){
    s2_ptoken next = s2_ptoken_empty;
    cwal_value * v = 0;
    char hadAtPrefix = 0;
    /* if((int)'@' == pr->token; */
    if(allowAtExpansion){
#if 0
      rc = s2_ptoker_next_token_skip_junk(pr)
        /* ^^^ cheaper than s2_next_token() */;
      /**
         20190820... it turns out that the cheap option breaks when we do:

         func(a, b, c,
         @blah)

         with a newline. Hmm. Interestingly, i only found the solution
         so quickly because of the comment above that
         s2_ptoker_next_token_skip_junk() is "cheaper than
         s2_next_token()". Had that comment not been there, i'd have
         been searching for years.
       */
#else
      rc = s2_next_token( se, pr, 0, 0 );
#endif
      if(rc) break;
      else if(S2_T_At==pr->token.ttype){
        hadAtPrefix = 1;
      }else{
        s2_ptoken const tmp = pr->token;
        s2_ptoker_putback(pr) /* rewind to just before non-junk
                                 token */;
        s2_ptoker_next_token_set(pr, &tmp)
          /* this one has a relatively large effect (~20% of the 10k
             nextToken hits in the current tests) */;
      }
    }
    if( (rc = s2_eval_expr_impl( se, pr, opComma,
                                 S2_EVAL_EOX_SEMICOLON_NOT_ALLOWED,
                                 &v)) ){
      break;
    }
    else if(!v){
      if(s2_ptoker_is_eof(pr)){
        if(S2_T_Comma==prev.ttype){
          s2_ptoker_errtoken_set(pr, &prev);
          errMsg = "Unexpected, comma at end, of list,";
          goto bad_comma;
        }
        else break;
      }
      else if(/*(S2_T_Comma==pr->token.ttype)
                &&*/ (!prev.ttype || (S2_T_Comma == pr->token.ttype))
              ){
        errMsg = "Unexpected, comma in, list.";
        goto bad_comma;
      }
      else{
        rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                           "Unexpected empty expression result.");
        break;
      }
    }else{ /* v === non-NULL result from eval */
      if(hadAtPrefix){
        /* Expand arrays/tuples prefixed with '@' into the destination array... */
        cwal_size_t i, n;
        cwal_array * av = 0;
        cwal_tuple * tp = 0;
        cwal_value_ref(v);
        av = cwal_value_array_part(se->e, v);
        if(!av) tp = cwal_value_get_tuple(v);
        if(!av && !tp){
          cwal_value_unref(v);
          return s2_throw_ptoker(se, pr, CWAL_RC_TYPE,
                                 "Illegal (non-array/tuple) value following '@'.");
        }
        n = av ? cwal_array_length_get(av) : cwal_tuple_length(tp);
        for( i = 0; !rc && i < n; ++i ){
          rc = av
            ? cwal_array_append(dest, cwal_array_get(av, i))
            : cwal_array_append(dest, cwal_tuple_get(tp, (uint16_t)i));
        }
        cwal_value_unref(v);
      }else{
        cwal_value_ref(v);
        rc = cwal_array_append(dest, v);
        cwal_value_unref(v);
      }
      if(rc) break;
      else if( (rc = s2_next_token( se, pr, 0, &next )) ) break;
      else if(s2_ttype_is_eof(next.ttype)){
        if(S2_T_Comma==pr->token.ttype){
          /* s2_ptoker_errtoken_set(pr, &next); */
          errMsg = "Unexpected, comma at end, of list,";
          goto bad_comma;
        }
        else{
          /*???*/ /*pr->nextToken = next;*/
          break;
        }
      }
      else if(S2_T_Comma != next.ttype){
        errMsg = "Expecting ',' after list element.";
        goto bad_comma;
      }else{
        prev = next;
        s2_ptoker_token_set(pr, &next) /* consume comma */;
      }
    }
    rc = s2_check_interrupted(se, rc);
  }
  return rc;
  bad_comma:
  assert(errMsg);
  return s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX, "%s", errMsg);
}

/**
   Parser for [array, literals] and [#tuple literals]. Requires
   pr->token.ttype to be S2_T_BraceGroup. In skip mode, *rv is set to
   the undefined value and 0 is returned, else the [...] block is
   parsed as a comma-separated list of expressions and (on success)
   *rv is set to a new Array or Tuple value.

   TODO: the tuple impl is rather inefficient because it first creates
   an array. We've got to do that (or traverse the tokens twice) so
   that we have the size of the tuple (required for construction). We
   could think about adding a tuple factory to cwal which transfers
   list memory from an array to a tuple.
*/
static int s2_eval_array_literal( s2_engine * se, s2_ptoker * pr, cwal_value ** rv ){
  s2_ptoker sub = s2_ptoker_empty;
  int rc = s2_check_interrupted(se, 0);
  assert(S2_T_BraceGroup==pr->token.ttype);
  if(rc) return rc;
  else if(se->skipLevel>0){
    *rv = cwal_value_undefined();
    return 0;
  }else{
    cwal_array * ar = 0;
    cwal_value * av = 0;
    cwal_scope scope = cwal_scope_empty;
    int gotTuple = 0;
    if(!s2_ptoken_has_content(&pr->token)){
      return (*rv = cwal_new_array_value(se->e)) ? 0 : CWAL_RC_OOM;
    }
    else if((rc = cwal_scope_push2(se->e, &scope))) return rc;
    rc = s2_ptoker_init_v2( se->e, &sub, s2_ptoken_adjbegin(&pr->token),
                            (int)s2_ptoken_len2(&pr->token), 0 );
    if(rc) goto end;
    sub.parent = pr;
    gotTuple = s2_ptoker_next_is_ttype(se, &sub, S2_NEXT_NO_POSTPROCESS, S2_T_OpHash, 1);
    if((rc = s2_check_interrupted(se, 0))) goto end;
    else if(!(ar = cwal_new_array(se->e))){
      rc = CWAL_RC_OOM;
      goto end;
    }
    av = cwal_array_value(ar);
    cwal_value_ref(av);
    rc = s2_eval_to_array( se, &sub, ar, 1 );
    if(!rc && gotTuple){
      cwal_size_t const nA = cwal_array_length_get(ar);
      uint16_t const nT = (uint16_t)nA;
      cwal_tuple * tp = 0;
      if((cwal_size_t)nT != nA){
        s2_ptoker_errtoken_set(pr, &sub.token);
        rc = s2_throw_ptoker(se, pr, CWAL_RC_RANGE, "Too many entries for a tuple.");
      }else{
        tp = cwal_new_tuple(se->e, nT);
        if(!tp){
          rc = CWAL_RC_OOM;
        }else{
          uint16_t i = 0;
          for( ; i < nT && !rc; ++i ){
            rc = cwal_tuple_set(tp, i, cwal_array_get(ar, i));
            assert(!rc && "No known error conditions here. All memory has been allocated.");
          }
          ar = 0;
          cwal_value_unref(av);
          av = cwal_tuple_value(tp);
          cwal_value_ref(av);
        }
      }
    }
    rc = s2_check_interrupted(se, rc);
    if(rc) rc = s2_check_brc(se, &sub, rc,
                             gotTuple
                             ? "tuple literal"
                             : "array literal");
    if(!rc){
      assert(av);
      cwal_value_unhand(av);
      *rv = av;
    }else if(av){
      cwal_value_unref(av);
    }
    end:
    s2_ptoker_finalize(&sub);
    cwal_scope_pop2(se->e, rc ? 0 : *rv);
    return rc;    
  }
}

/** Internal helper for s2_eval_to_object() */
static int s2_eval_object_prop_set( s2_engine * se, s2_ptoker * pr,
                                    cwal_value * dest,
                                    cwal_value * key, cwal_value * val,
                                    cwal_flags16_t flags ){
  int rc;
  cwal_function * f = 0;
  if(0==cwal_value_compare(key, se->cache.keyCtorNew) &&
     (f = cwal_value_function_part(se->e, val))){
    /* If the key is '__new' and v is call()able, set it as
       hidden/const, simply for consistency across classes (as
       opposed to it being hidden/const in C-created cases but not
       script-created). We should arguably do this in
       s2_set_with_flags_v(), but that code is complex enough as
       it is :/. Maybe someday.

       2020-02-20: with the addition of {@anObject} property copying
       syntax, the flagging of constructors as hidden *might* be
       considered a misbehaviour, since {@x} will not copy a ctor
       property to the target object. Feature or bug?
    */
    rc = s2_ctor_method_set(se, dest, f);
  }else{
    /* use s2_set_v() for the 'prototype' and object-mode-hash
       handling, as well as se->err error reporting. */
    rc = s2_set_with_flags_v(se, dest, key, val, flags);
  }
  return s2_handle_set_result(se, pr, rc);
}

/** State for use with cwal_kvp_visitor_props_copy_s2() */
typedef struct {
  s2_engine * se;
  s2_ptoker * ptoker;
  cwal_value * store;
} S2CopyPropsState;

/** Internal helper for s2_eval_to_object() */
static int cwal_kvp_visitor_props_copy_s2( cwal_kvp const * kvp,
                                           void * state ){
  S2CopyPropsState * const sps = (S2CopyPropsState*)state;
  return s2_eval_object_prop_set(sps->se, sps->ptoker, sps->store,
                                 cwal_kvp_key(kvp), cwal_kvp_value(kvp),
                                 cwal_kvp_flags(kvp));
}


/**
   Expects pr to contain, until EOF, JavaScript-style object key/value pairs:

   k1: v1, k2: v2 ...

   Returns 0 on success, throws or propagates an error on error.

   The resulting container is written to *rv on success. On error
   *rv is not modified.

   If propCount is not 0 then it is set to the number of properties
   parsed on success.

   If inputHasHashMarker is not 0 then:

   - *inputHasHashMarker is set to 0 before starting.

   - the syntax is extended a little bit: it allows, at the start
   (only) one # symbol, *inputHasHashMarker to be incremented by
   one. If one is seen then this functon creates a cwal_hash,
   otherwise it creates a cwal_object. If a # is encountered while
   inputHasHashMarker is NULL then it is a syntax error.

   Extensions to conventional JS-style syntax:

   1) { [expression-as-a-key]: value }, where the expression's value
   becomes the key.

   2) { @otherContainer } is essentially the same as JS's spread
   syntax, importing the iterable (non-hidden) properties from
   @otherContainer into the literal. This syntax is not yet permitted
   for hashes because of potential semantic ambiguities in handling of
   hash vs. object properties.

*/
static int s2_eval_to_object( s2_engine * se, s2_ptoker * pr, cwal_value ** rv,
                              cwal_size_t * propCount,
                              int *inputHasHashMarker){
  s2_ptoken prev = s2_ptoken_empty;
  s2_op const * opComma = s2_ttype_op(S2_T_Comma);
  cwal_size_t count = 0;
  char seenHash = 0;
  cwal_value * store = 0;
  int rc = s2_check_interrupted(se, 0);
  cwal_value * v = 0;
  cwal_value * vKey = 0;
  assert(!se->skipLevel);
  assert(opComma);
  if(inputHasHashMarker) *inputHasHashMarker = 0;
  while( !rc && !s2_ptoker_is_eof(pr)){
    s2_ptoken ident = s2_ptoken_empty;
    s2_ptoken startPos = pr->token;
    cwal_flags16_t fSetFlags = 0 /* flags for cwal_prop_set_with_flags_v()
                                    and friends. */;
    char gotAtSign = 0 /* true if we get a @-style key */;
    assert(!v);
    assert(!vKey);
    /*pr->flags |= S2_T10N_F_IDENTIFIER_DASHES;
      ^^^ potentially interesting. */
    rc = s2_next_token(se, pr, 0, 0);
    /*pr->flags &= ~S2_T10N_F_IDENTIFIER_DASHES;*/
    if( rc ) break;
    else if((S2_T_Comma == pr->token.ttype)
            && (!prev.ttype || (S2_T_Comma == prev.ttype))
            ){
      goto bad_comma;
    }
    else if(s2_ptoker_is_eof(pr)){
      if(S2_T_Comma == prev.ttype){
        s2_ptoker_errtoken_set(pr, &prev);
        goto bad_comma;
      }
      else break;
    }else if(inputHasHashMarker && !seenHash
             && !count && (int)'#'==pr->token.ttype){
      ++seenHash;
      ++*inputHasHashMarker;
      continue;
    }else if((int)'#'==pr->token.ttype){
      rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                         "Extra '#' in input.");
      break;
    }
#if 0
    MARKER(("OBJ KEY: %s %.*s\n", s2_ttype_cstr(pr->token.ttype),
            (int)s2_ptoken_len(&pr->token),
            s2_ptoken_begin(&pr->token)));
#endif
    else if(S2_T_BraceGroup==pr->token.ttype){
      /* 20191117: check for {[key expr]: value}
         a.k.a. Expression-as-a-Key (EaaK, pronounced like "eek").
      */
      s2_ptoker sub = s2_ptoker_empty;
      rc = s2_ptoker_sub_from_toker(pr, &sub);
      if(rc){
        s2_ptoker_finalize(&sub);
        break;
      }
      rc = s2_eval_ptoker( se, &sub, 0, &vKey );
      s2_ptoker_finalize(&sub);
      if(rc){
        assert(!vKey);
        break;
      }
      else if(!vKey){
        rc = s2_err_ptoker(se, pr, CWAL_RC_MISUSE,
                           "Unexpected NULL eval result while processing "
                           "[...] object literal key.");
        break;
      }
      cwal_value_ref(vKey);
      /*s2_dump_val(vKey, "vKey");*/
    }
    else if(S2_T_At == pr->token.ttype){
      gotAtSign = 1;
    }
    else if(!s2_ttype_is_object_keyable(pr->token.ttype)){
      s2_ptoker_errtoken_set(pr, &startPos);
      rc = s2_throw_ptoker(se, pr, CWAL_SCR_SYNTAX,
                           "Expecting identifier or 'simple' "
                           "value as object key.");
      break;
    }
    ident = pr->token;
    /*MARKER(("ident =%.*s\n", (int)s2_ptoken_len(&pr->token), s2_ptoken_begin(&pr->token)));*/
    if(!gotAtSign){
      rc = s2_next_token(se, pr, 0, 0);
      if(rc) break;
      else if(S2_T_OpColonEqual == pr->token.ttype){
        fSetFlags = CWAL_VAR_F_CONST;
      }else if(S2_T_Colon != pr->token.ttype){
        /* check for {standaloneIdentifier} ... */
        if(vKey){
          rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                             "{[object key]} syntax requires a "
                             "following colon.");
          break;
        }
        else if(S2_T_Identifier==ident.ttype
                && (S2_T_Comma == pr->token.ttype
                    || s2_ptoker_is_eof(pr/* virtual EOF at end of object: {a:1,...,b} */))
                ){
          /* JS-like {a,b} ==> {a: a, b: b} */
          s2_ptoker_putback(pr) /* comma/eof will be handled downstream */;
          v = s2_var_get(se, -1, s2_ptoken_begin(&ident), s2_ptoken_len(&ident));
          if(!v){
            rc = s2_throw_ptoker(se, pr, CWAL_RC_NOT_FOUND,
                                 "Could not resolve identifier '%.*s'.",
                                 (int)s2_ptoken_len(&ident),
                                 s2_ptoken_begin(&ident));
            break;
          }
        }else{
          rc = s2_throw_ptoker(se, pr, CWAL_SCR_SYNTAX,
                               "Expecting ':' after object key.");
          break;
        }
      }
    }
    if(!v && (rc = s2_eval_expr_impl( se, pr, opComma,
                                      S2_EVAL_EOX_SEMICOLON_NOT_ALLOWED,
                                      &v)) ){
      break;
    }
    else if(!v){
      rc = s2_throw_ptoker(se, pr, CWAL_SCR_SYNTAX,
                           "An empty expression is not allowed here.");
      break;
    }else if( gotAtSign ){
      if(!cwal_props_can(v) || s2_value_is_enum(v)){
        /**
          The "problem" with enums is that they can use either object
          or hash storage internally, so we would need to
          differentiate between the two in the @ expansion.  We don't
          currently do that, but maybe someday will.
        */
        rc = s2_throw_ptoker(se, pr, CWAL_RC_TYPE,
                             "Value after @ must be a non-enum "
                             "container type.");
        break;
      }
      assert(!vKey);
      vKey = v /* simplifies handling below */;
      cwal_value_ref(vKey);
    }
    cwal_value_ref(v);
    {
      s2_ptoken next = s2_ptoken_empty;
      /*s2_dump_val(v, "object entry value");*/
      if( (rc = s2_next_token( se, pr, 0, &next )) ) break;
      else if(!s2_ttype_is_eof(next.ttype) && S2_T_Comma != next.ttype){
        rc = s2_throw_ptoker(se, pr, CWAL_SCR_SYNTAX,
                               "Got token type %s, but expected ',' or "
                               "end-of-object after object entry.",
                               s2_ttype_cstr(next.ttype));
        break;
      }

      if(!store){
        /* Create the property storage: Object or Hash */
        store = seenHash
          ? cwal_new_hash_value(se->e, 7)
          : cwal_new_object_value(se->e);
        if(!store){
          rc = CWAL_RC_OOM;
          break;
        }else{
          cwal_value_make_vacuum_proof(store, 1);
          cwal_value_ref(store);
          if(seenHash){
            s2_hash_dot_like_object(store, 1
                                    /* Needed for remainder of the loop
                                       so that s2_set_v() will DTRT.
                                       Might get overwritten afterwards*/);
          }
        }
      }

      /* And finally... insert the key/value... */
      if( !vKey ){
        assert(!gotAtSign);
        if((rc = s2_ptoken_create_value(se, pr, &ident, &vKey))) break;
        assert(vKey);
        /* s2_dump_val(vKey, "object key"); */
        cwal_value_ref(vKey);
      }
      if( gotAtSign ){
        if(seenHash){
          rc = s2_throw_ptoker(se, pr, CWAL_RC_TYPE,
                               "@ property expansion is not currently "
                               "permitted for hash literals.");
          break;
        }else{
          S2CopyPropsState sps;
          assert(vKey == v);
          sps.se = se;
          sps.ptoker = pr;
          sps.store = store;
          rc = cwal_props_visit_kvp( v, cwal_kvp_visitor_props_copy_s2,
                                     &sps );
        }
      }else{
        rc = s2_eval_object_prop_set(se, pr, store, vKey, v, fSetFlags);
      }
      cwal_value_unref(vKey);
      vKey = 0;
      cwal_value_unref(v);
      v = 0;
      if(rc) break;
      ++count;
      prev = next;
      s2_ptoker_token_set(pr, &next) /* consume comma */;
    }
    assert(!rc);
    rc = s2_check_interrupted(se, rc);
  }/* loop over object body */

  if(!rc && seenHash){
    /* Resize the hashtable if it doesn't appear adequately sized... */
    cwal_hash * h = cwal_value_get_hash(store);
    if(!h){
      assert(!count);
      cwal_value_unref(store);
      store = cwal_new_hash_value(se->e, 5);
      if(!store){
        rc = CWAL_RC_OOM;
      }else{
        cwal_value_ref(store);
      }
    }else{
      rc = cwal_hash_grow_if_loaded( h, 0.75 );
    }
  }

  end:
  cwal_value_unref(v);
  cwal_value_unref(vKey);
  if(store){
    cwal_value_make_vacuum_proof(store, 0);
  }
  if(rc){
    cwal_value_unref(store);
    rc = s2_check_brc( se, pr, rc, seenHash
                       ? "hash literal"
                       : "object literal" );
  }else{
    if(propCount) *propCount = count;
    if(!store){
      assert(!count);
      store = seenHash
        ? cwal_new_hash_value(se->e, 5)
        : cwal_new_object_value(se->e);
      if(!store){
        rc = CWAL_RC_OOM;
      }else{
        cwal_value_ref(store);
      }
    }
    if(store){
      assert(!rc);
      cwal_container_client_flags_set(store, seenHash>1
                                      ? S2_VAL_F_DOT_LIKE_OBJECT
                                      : 0);
      cwal_value_unhand(store);
      *rv = store;
    }else{
      assert(rc);
    }
  }
  return rc;
  bad_comma:
  rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                     "Unexpected comma in %s literal.",
                     seenHash ? "hash" : "object");
  goto end;
}

/**
   Evaluator for object literal blocks. Asserts that
   S2_T_SquigglyBlock ==pr->token.ttype.

   On success, *rv will hold the parsed object.

   Returns 0 on success, of course.
*/
static int s2_eval_object_literal( s2_engine * se, s2_ptoker * pr,
                                   cwal_value ** rv ){
  int rc = s2_check_interrupted(se, 0);
  assert( S2_T_SquigglyBlock == pr->token.ttype );
  if(rc) return rc;
  else if(se->skipLevel>0){
    *rv = cwal_value_undefined();
    return 0;
  }else{
    cwal_value * ov = 0;
    cwal_scope scope = cwal_scope_empty;
    if(!s2_ptoken_has_content(&pr->token)){
      return (*rv = cwal_new_object_value(se->e)) ? 0 : CWAL_RC_OOM;
    }
    else if((rc = cwal_scope_push2(se->e, &scope))) return rc;
    else{
      s2_ptoker sub = s2_ptoker_empty;
      cwal_size_t propCount = 0;
      int doHash = 0;
      rc = s2_ptoker_sub_from_toker(pr, &sub);
      if(!rc){
        rc = s2_eval_to_object( se, &sub, &ov, &propCount, &doHash );
      }
      s2_ptoker_finalize(&sub);
    }
    if(!rc){
      assert(ov);
      *rv = ov;
    }
    cwal_scope_pop2(se->e, rc ? 0 : *rv);
    return rc;
  }
}


/**
   Iternal impl for ternary op: (IF ? THEN : ELSE). Expects the top of
   the value stack to hold the LHS value (the IF part). On success it
   replaces that value with the result of either its THEN or ELSE
   parts. On success, pr->token will be set up so that its next token
   will be the one after the ELSE expression. If rv is not 0 then the
   result is also placed in *rv.
*/
static int s2_eval_ternary( s2_engine * se, s2_ptoker * pr,
                            cwal_value **rv){
  int rc;
  cwal_value * lhs /* the IF part of (IF ? THEN : ELSE) */;
  cwal_value * rhs1 = 0 /* the THEN part */,
    * rhs2 = 0 /* the ELSE part */;
  cwal_value ** rhs = 0 /* bool(lhs) ? &rhs1 : &rhs2 */;
  char buul;
  s2_ptoken pos = pr->token;
  s2_ptoken const origin = pos;
  s2_op const * fromLhsOp = s2_ttype_op(S2_T_Question);
  int const oldTernaryLevel = se->ternaryLevel;
  assert(fromLhsOp);
  assert(S2_T_Question==pr->token.ttype);

  rc = s2_eval_lhs_ops(se, pr, s2_ttype_op(S2_T_Question));
  if(rc) return rc;
  lhs = s2_engine_peek_value(se);
  if(!lhs){
    return s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                                "Expecting value before X?Y:Z op");
  }
  buul = cwal_value_get_bool(lhs);
  assert(cwal_value_is_builtin(lhs)
         || cwal_value_refcount(lhs)/*expecting eval holder ref*/);
  cwal_value_ref(lhs);
  rhs = buul ? &rhs1 : &rhs2;
  ++se->ternaryLevel;

  /* Eval the THEN part... */
  rc = s2_eval_expr_impl(se, pr, fromLhsOp/*0?*/,
                         buul ? 0 : S2_EVAL_SKIP,
                         &rhs1);
  /*MARKER(("Got rhs1 rc=%d: [[[%.*s]]\n", rc,
    (int)(s2_ptoken_begin(&pr->capture.end)-s2_ptoken_end(&pr->capture.begin)),
    s2_ptoken_begin(&pr->capture.begin)));*/
  cwal_value_ref(rhs1);
  if(rc) goto end;
  rc = s2_next_token(se, pr, 0, 0);
  if( !rc && (S2_T_Colon != pr->token.ttype) ){
    rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                       "Expecting ':' after the "
                       "Y part of ternary expression "
                       "(X ? Y : Z)");
  }
  if(rc) goto end;

  /* Eval the ELSE part... */
  pos = pr->token;
  rc = s2_eval_expr_impl(se, pr, fromLhsOp,
                         buul ? S2_EVAL_SKIP : 0,
                         &rhs2);
  cwal_value_ref(rhs2);
  /*MARKER(("Got rhs2 rc=%d: [[[%.*s]]\n", rc,
    (int)(s2_ptoken_begin(&pr->capture.end)-s2_ptoken_begin(&pr->capture.begin)),
    s2_ptoken_begin(&pr->capture.begin)));*/
  end:
  if(!rc){
    assert(s2_engine_peek_value(se)==lhs);
    s2_engine_pop_token(se, 0) /* ==> lhs (still in the eval holder) */;
    assert(se->ternaryLevel == oldTernaryLevel+1);
  }
  se->ternaryLevel = oldTernaryLevel;
  assert(cwal_value_is_builtin(lhs)
         || cwal_value_refcount(lhs)>1/*expecting eval holder ref and ours*/);
  cwal_value_ref(*rhs);
  cwal_value_unref(lhs);
  cwal_value_unref(rhs1);
  cwal_value_unref(rhs2);
  rc = s2_check_interrupted(se, rc);
  if(!rc && (!rhs1 || !rhs2)){
    static char const * emptyMarker = "<EMPTY>";
    s2_ptoker_errtoken_set(pr, &pos);
    rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                       "Unexpected empty expression "
                       "in (X ? %s : %s)",
                       rhs1 ? "Y" : emptyMarker,
                       rhs2 ? "Z" : emptyMarker)
      /* Reminder: rhs1/rhs2 pointers are semantically invalid now
         (unref'd above), but their addresses are still usable
         here */;
  }
  if(rc){
    cwal_value_unref(*rhs);
  }else{
    if(rv) *rv = *rhs;
    cwal_value_unhand(*rhs);
    /* s2_dump_val(*rhs,"Ternary result"); */
    s2_engine_push_val(se, *rhs)->srcPos = origin
      /* Trivia: that push cannot fail as long as token recycling is
         enabled, because the pop (above) just freed one up for us.

         The main eval loop will stuff that *rhs into the eval holder.
      */;
  }
  return rc;
}

#if 0
static int s2_next_is_assignment( s2_engine * se, s2_ptoker * pr ){
  s2_ptoken tgt = s2_ptoken_empty;
  int rc;
  s2_next_token( se, pr, 0, &tgt );
  if(!(rc = s2_ttype_is_assignment(tgt.ttype))){
    rc = s2_ttype_is_assignment_combo(tgt.ttype);
  }
  if(!rc) pr->nextToken = tgt;
  return rc;
}
#endif

static int s2_next_wants_identifier( s2_engine * se, s2_ptoker * pr ){
  s2_ptoken tgt = s2_ptoken_empty;
  int rc;
  rc = s2_next_token( se, pr, S2_NEXT_NO_POSTPROCESS, &tgt );
  if(rc) return 0;
  s2_ptoker_next_token_set(pr, &tgt)
    /* This pr->nextToken adjustment appears to have the
       single-biggest effect in triggering the s2_ptoker_next_token()
       nextToken optimization: ~75% of the 10k hits in the current
       tests. */;
  /* MARKER(("Next-wants-identifier? ttype=%s\n", s2_ttype_cstr(tgt.ttype))); */
  switch(tgt.ttype){
    case S2_T_OpIncr:
    case S2_T_OpDecr:
      return tgt.ttype;
  }
  if( (rc = s2_ttype_is_assignment(tgt.ttype)) ) return rc;
  else if((rc = s2_ttype_is_assignment_combo(tgt.ttype))) return rc;
  else rc = s2_ttype_is_identifier_prefix(tgt.ttype);
  return rc;
}

/**
   Returns ttype if it refers to a "dereferencing" operator,
   else returns 0.

   If this returns non-0:

   - assignment operators translate the stack into a ternary
   operation.

   - In skip mode, a call operator after a dereferenced
   value is assumed to be valid, and skipped.

   - It is assumed (and may be assert()ed) that the operator sets 
   s2_dotop_state( se, lhs, lhs, rhs ) when run, so that assignments 
   can work and 'this' can get bound if the following operation is
   a call() on the lhs[rhs] result.
   
   i.e. the token type must be, or behave just like, the dot operator,
   S2_T_OpDot.

*/
static int s2_ttype_is_deref( int ttype ){
  switch(ttype){
#if 0
    case S2_T_OpArrow:
    case S2_T_OpDotDot:
#endif
#if 0
    case S2_T_OpHash:
#endif
    case S2_T_OpDot:
    /*case S2_T_OpColon2:*/
    /*case S2_T_OpDotPrototype:*/
      return ttype;
    default:
      return 0;
  }
}

/**
  If op->id is of a type which should treat its RHS identifier as a 
  property key, rather than immediately resolving the identifier, 
  op->id is returned, else 0 is returned.
*/
static int s2_op_rhs_as_propkey( s2_op const * op ){
  switch(op ? op->id : 0){
    /*case S2_T_OpDotDot:*/
    case S2_T_OpColon2:
    case S2_T_OpDot:
      return op->id;
    default:
      return 0;
  }
}

/**
   Returns ttype if it is a dot-op-like id or (special case) parens
   group, else returns 0.
*/
static int s2_ttype_is_dotish( int ttype ){
  switch(ttype){
    case S2_T_OpArrow:
    case S2_T_OpDotDot:
    case S2_T_OpColon2:
    case S2_T_OpHash:
    case S2_T_OpDot:
    /*case S2_T_OpDotPrototype:*/
    case S2_T_BraceGroup:
    case S2_T_BraceOpen:
    case S2_T_ParenGroup:
    case S2_T_ParenOpen:
      /* 20170323: ParenGroup/Open added so that:

         ++f().x

         can work.
       */
      return ttype;
    default:
      return 0;
  }
}

/**
   Returns the token type id of the next token in pr is a dot-op-like
   token, else returns 0.
*/
static int s2_next_is_dotish( s2_engine * se, s2_ptoker * pr ){
  s2_ptoken tgt = s2_ptoken_empty;
  int const rc = s2_next_token( se, pr, S2_NEXT_NO_POSTPROCESS, &tgt )
    ? 0
    : s2_ttype_is_dotish(tgt.ttype);
  if(!rc){
    s2_ptoker_next_token_set(pr, &tgt);
  }
  return rc;
}

/**
   Internal helper for s2_eval_expr_impl(). Requires pr->token to be
   a S2_T_ParenGroup and prevOp to be the operator associated with
   the previous expression token (may be 0). This function inspects
   prevOp and se's stack to determine if this paren group looks
   like it should be function call arguments.

   Returns 0 if the parens do not look like a function call, else
   non-0. Assigns *rc to 0 on success, non-0 if it detects a syntax
   error. If *rc is non-0 upon returning, evaluation must stop and
   the error must be propagated.

   If allowNonCallableContainer then this function returns true if the
   LHS is a Container type, regardless of whether or not it is
   callable. This is intended for use with the 'new' keyword.

   It is only okay to call s2_eval_fcall() if this returns non-0 and
   sets *rc to 0, and that call (if made) must be made before doing any
   further tokenization or stack processing.

   BUG:

   var f = proc(){return o} using {o:{a:1}};
   f().a += 1 // OK
   f().a++ // OK
   ++f().a // not OK: Exception: Non-function value type 'string' before a call() operator.

   Where the 'string' is really the identifier 'f'.

   That was fixed by adding S2_T_ParenGroup to
   s2_ttype_is_dotish(). Probably not the best fix, but the simplest
   and most expedient (and doesn't cause any regressions, curiously
   enough).
*/
static char s2_looks_like_fcall( s2_engine * se, s2_ptoker * pr,
                                 int allowNonCallableContainer,
                                 s2_op const * prevOp, int * rc ){
  assert(S2_T_ParenGroup==pr->token.ttype);
  *rc = 0;
  if(prevOp) return 0;
  else{
    if( se->skipLevel>0 ){
      return s2_engine_peek_value(se) ? 1 : 0;
    }else{
      cwal_value * v = s2_engine_peek_value(se);
      s2_stoken const * opT = v ? s2_engine_peek_op(se) : 0;
      if(!v){
        return 0;
      }else if(opT && s2_ttype_is_dotish(opT->ttype)){
        return 1;
        /* We'll assume a function is possible and fail
           after evaluating the top op if it isn't one. */
      }
      else if(se->dotOp.lhs && se->dotOp.key){
        /* So that (obj.prop)(...) can work */
        return 1;
      }else if(allowNonCallableContainer
               ? !cwal_props_can(v)
               : !cwal_value_function_part(se->e, v)){
        /* s2_dump_val(v, "non-function before call()"); */
        *rc = s2_throw_ptoker(se, pr, CWAL_RC_TYPE,
                              "Non-function value type '%s' "
                              "before a call() operator.",
                              cwal_value_type_name(v));
        return 0;
      }else{
        return 1;
      }
    }
  }
}

/**
   Internal helper for s2_eval_expr_impl(). Requires that s2_looks_like_fcall()
   has just succeeded, else results are undefined.

   Treats the current S2_T_ParenGroup token as function call arguments and
   calls the function, which may be either of these forms on the stack:

   FUNC
   or
   OBJ.FUNC

   In the former form, the 'this' value for the call() will be the
   function itself.

   Remember that OBJ[PROP] gets translated at parse time to OBJ.PROP.
 */
static int s2_eval_fcall( s2_engine * se, s2_ptoker * pr, cwal_value ** rv ){
  int rc = 0;
  s2_stoken const * opTok;
  s2_op const * op;
  s2_ptoken const origin = pr->token;
  cwal_value * fv = 0;
  cwal_value * xrv = 0;
  cwal_value * vSelf = 0;
  cwal_scope scope = cwal_scope_empty;
  s2_strace_entry strace = s2_strace_entry_empty;
  cwal_function * theFunc;
  s2_func_state * fst = 0;
  char reachedTheCall = 0
    /* will be set to 1 if we reach the actual call() part. This
       distinction is needed to handle stray continue/break
       properly. */;
  assert(S2_T_ParenGroup==pr->token.ttype);
  if(!se->skipLevel){
    rc = s2_scope_push_with_flags(se, &scope, S2_SCOPE_F_IS_FUNC_CALL);
    if(rc) return rc;
  }

  /* Check if this is a FUNC() or OBJ.FUNC() call... */
  opTok = s2_engine_peek_op(se);
  op = opTok ? s2_ttype_op( opTok->ttype ) : 0;

  /*
    To consider: if dot is not a dot op but se->dotOp.lhs
    is set, then some LHS (possibly a (subexpr)) possibly
    has given us a 'this' to work with. It's not yet clear
    whether we can really know if that's the proper 'this',
    though.

    (obj.func)(...) // OK

    obj.prop.func(...) // OK b/c the stack will never have more than 1 dot op;

    obj.prop, func(...) // not OK

    What if we just reset se->dotOp.lhs on each new operator?

    obj.prop['x'] ==> obj.prop.x ==> OK

    obj.prop + 1 (...) // invalid - not-a Function ==> OK

    obj[obj.prop](...) // [...]==>dot op ==> OK
  */
  if(op &&
#if 1
     s2_ttype_is_dotish(op->id)
#else
     s2_ttype_is_deref(op->id)
#endif
     ){
    /* If a dot-like operator is pending, run it
       to get the function and 'this'. */
    rc = s2_process_top(se);
    switch(rc){
      case 0:
        break;
      case CWAL_RC_EXCEPTION:
        goto end;
        break;
      default:
        assert(s2__err(se).code);
        rc = s2_ammend_op_err(se, pr, rc);
        assert(rc);
        goto end;
    }
  }
  if(se->dotOp.lhs){
    /*
      To allow (obj.func)(...) to work...

      Reminder to self: we should be able(?) to do this in the
      assignment ops as well, so that (x.y)=3 could work. The problem,
      it seems, with that, is that (A) we'd end up having to special-case
      clearing of the dotop state for (...) groups and (B) we _might_
      end up leaking over dotop state like this:

      var x = (y.z);

      On the assignment, "this" _might_ (not certain) get picked up as
      'y' and se->dotOp.key as 'z'. No, it won't - the stack machine
      will only push 2 ops, and 'this' will get cleared at the end of
      that expression. Hmmm.
    */
    /* MARKER(("Possibly stealing a 'this'\n")); */
    assert(!fv);
    assert(!vSelf);
    vSelf = se->dotOp.self;
    /* s2_dump_val(vSelf, "se->dotOpThis"); */
    if(vSelf) cwal_value_ref(vSelf);
    fv = s2_engine_pop_value(se) /* presumably a dot-op result */;
    assert(fv);
    cwal_value_ref( fv );
  }else{
    fv = s2_engine_pop_value(se) /* hopefully a function */;
    cwal_value_ref( fv );
  }
  s2_dotop_state(se, 0, 0, 0);

  /* In skip mode, we're done. We had to get the values off the stack,
     though, so we couldn't do this first. */
  if(se->skipLevel>0){
    xrv = cwal_value_undefined();
    goto end;
  }

  rc = s2_strace_push_pos(se, pr, &origin, &strace )
    /* we have to do this fairly late so that the
       the (still pending) call doesn't confuse the
       the trace. */
    ;
  if(rc) goto end;

  if(!fv || !(theFunc = cwal_value_function_part(se->e, fv))) {
    rc = s2_throw_ptoker(se, pr, CWAL_RC_TYPE,
                         "Non-function (%s) LHS before "
                         "a call() operation.",
                         fv ? cwal_value_type_name(fv)
                         : "<NULL>");
    goto end;
  }
  /* MARKER(("Function call args: %.*s\n", (int)s2_ptoken_len(&pr->token),
     s2_ptoken_begin(&pr->token))); */
  /* s2_dump_val(vSelf,"call() vSelf"); */
  /* s2_dump_val(fv,"call() func"); */

  fst = s2_func_state_for_func(theFunc);
  {
    /* Process the args and call() the func... */
    s2_ptoker sub = s2_ptoker_empty;
    cwal_value * arV = 0;
    cwal_array * oldArgV;
    s2_func_state const * oldScriptFunc = se->currentScriptFunc;
    cwal_array * ar = cwal_new_array(se->e);
    if(!ar){
      rc = CWAL_RC_OOM;
      goto end;
    }
    arV = cwal_array_value(ar);
    cwal_value_ref(arV);
    cwal_value_make_vacuum_proof(arV, 1);
    rc = s2_ptoker_sub_from_toker(pr, &sub);
    if(rc){
      cwal_value_unref(arV);
      arV = 0;
      ar = 0;
      goto end;
    }
    assert(pr==sub.parent);
    /* sub.name = "call(args)"; */
    rc = s2_eval_to_array( se, &sub, ar, 1 );
    s2_ptoker_finalize(&sub);
    if(rc){
      cwal_value_unref(arV);
      arV = 0;
      ar = 0;
      goto end;
    }
    oldArgV = se->callArgV;
    se->callArgV = 0;
    se->currentScriptFunc = 0 /* if this is a script-side function call,
                                 s2_callback_f(), which is about to trigger,
                                 will set this to the corresponding function. */;
    s2_ptoker_errtoken_set(pr, &origin) /* just in case, location for error reporting */;
    if(0){
      MARKER(("Function call()...\n"));
      s2_dump_val(vSelf ? vSelf : fv, "vSelf ? vSelf : fv");
      s2_dump_val(cwal_array_value(ar), "argv");
    }

    reachedTheCall = 1;
    /* Reminder to self:
       cwal makes fv sweep/vacuum-safe for the life of the call,
       if it was not already.
     */
    if(fst
       && (fst->flags & S2_FUNCSTATE_F_EMPTY_BODY)
       && (fst->flags & S2_FUNCSTATE_F_EMPTY_PARAMS)
       ){
      /* No function body and no possibility of default parameter
         values triggering side effects, so we don't need to call() it
         (which involves setting up imported symbols, "argv", and
         "this").  We need to ensure that se->callArgV is 0, though,
         as that would normally be taken care of via the pre- and/or
         post-call() hooks.

         It's hypothetically possible that default parameter values
         want to trigger side-effects via imported symbols (including
         "this" and the function's call()-local name
         (fst->vName)). For that to work, we need to go through the
         whole call() process if the body is empty but the params list
         is not. It's hard to envision a real use case for that, but
         the language allows for such constructs, so let's behave
         properly if someone actually constructs such a weird thing.
      */
      assert(!se->callArgV);
    }else{
      se->callArgV = ar /* needed by internals we're about to pass
                           through via the call() */;
      rc =  cwal_function_call_array( &scope, theFunc,
                                      vSelf ? vSelf : fv, &xrv, ar )
      /* Still wavering on this point: when called without a propery
         access (vSelf===0), should the function's "this" be the
         undefined value or itself? i don't want "this" propagating in
         from an older scope.

         One argument for passing fv instead of undefined:

         var obj = { prototype: proc(x){ this.x = argv.x } };

         obj(1);

         'this' is undefined in that call.

         Another argument: it seems that i already wrote a chapter on
         how to use/abuse it in the docs.
       */;
    }
    se->currentScriptFunc = oldScriptFunc;
    /* assert(!se->callArgV && "Gets unset via the post-call() hook");
    
       interesting problem: on certain types of errors, first made
       possible with the introduction of interceptors, the pre()
       hook might not be called, and thus the post-hook won't. */
    if(se->callArgV){
      assert(rc && "Only expecting this in certain error cases.");
      se->callArgV = 0;
      /* s2_dotop_state( se, 0, 0, 0 ); necessary!!! ? */
    }
    se->callArgV = oldArgV;
    /* assert(0==oldArgV); */
    cwal_value_make_vacuum_proof(arV, 0);
    if(!rc && xrv){
      /* we have to ref xrv before destroying arV for the case that xrv==arV
         or arV contains (perhaps indirectly) xrv. */
        cwal_value_ref(xrv);
    }
    cwal_value_unref(arV);
  }
  end:
  rc = s2_check_interrupted(se, rc);
  switch(rc){
    case 0:
      break;
    case CWAL_RC_EXCEPTION:
      rc = s2_exception_add_script_props(se, pr)
        /* Needed to decorate calls to native functions. */;
      if(rc){
        /* lower-level error, e.g. CWAL_RC_OOM, while adding script
           location state. */
        break;
      }
      rc = CWAL_RC_EXCEPTION;
      CWAL_SWITCH_FALL_THROUGH;
    /* case CWAL_SCR_SYNTAX: */
    case CWAL_RC_OOM:
    case CWAL_RC_EXIT:
    case CWAL_RC_FATAL:
    case CWAL_RC_INTERRUPTED:
    case CWAL_RC_ASSERT:
      /* do we want assertion errors to translate to exceptions
         if they go up the call stack? Much later: no.*/
      break;
    case CWAL_RC_BREAK:
    case CWAL_RC_CONTINUE:
      /*
        If a break/continue are triggered while handling the
        arguments, that's okay: let them propagate so they can be
        caught by upstream code. If they passed through the call()
        itself, that's not okay: stop them from propagating further.
      */
      if(!reachedTheCall) break;
      CWAL_SWITCH_FALL_THROUGH;
    default:
      if(s2__err(se).code){
        /* Likely a syntax-related error pending */
        /*MARKER(("[rc=%s] se->err.msg=%s\n", cwal_rc_cstr(se->err.code), (char *)se->err.msg.mem));*/
        rc = s2_throw_err(se, 0, 0, 0, 0);
      }else{
        if(cwal_exception_get(se->e)){
          /* s2_exception_add_script_props(se, pr); why is this not needed? */
          rc = CWAL_RC_EXCEPTION /* keep propagating it and
                                    assume/hope that rc is encoded in
                                    exception.code. */;
        }else{
          rc = s2_throw_ptoker(se, pr, rc,
                               "Function call returned non-exception "
                               "error code #%d (%s).",
                               rc, cwal_rc_cstr(rc));
        }
      }
      break;
  }
  if(strace.pr){
    s2_strace_pop(se);
  }
  if(!rc && xrv){
      assert(cwal_value_refcount(xrv)>0 /* we ref'd it above */
             || cwal_value_is_builtin(xrv));
  }
  if(vSelf) cwal_value_unref(vSelf);
  if(fv) cwal_value_unref(fv);
  if(!rv){
    cwal_value_unref(xrv);
    xrv = 0;
  }
  if(rc){
    cwal_value_unref(xrv);
    xrv = 0;
  }else{
    cwal_value_unhand(xrv);
    if(rv) *rv = xrv ? xrv : cwal_value_undefined();
    else{
      assert(!xrv && "was zeroed out above.");
    }
  }
  if(scope.parent){
    assert(&scope == se->scopes.current->cwalScope);
    cwal_scope_pop2(se->e, rc ? 0 : xrv);
  }
  return rc;
}

static int s2_eval_is_eox(s2_engine const * se,
                          s2_ptoker const * pr){
  int rc;
  assert(pr);
  rc = s2_ttype_is_eox(pr->token.ttype);
#if 1
  if(!rc && se->ternaryLevel>0
     && S2_T_Colon==pr->_nextToken.ttype ){
    /*
      huge kludge for:

      0 ? foreach(@[1]=>k,v) eval x : 1
    */
    rc = S2_T_Colon;
  }
#endif
  return rc;
}

/**
   If true, s2_eval_expr_impl() internally uses a local
   array to keep references active and vacuum-proofed.

   HOLY COW... when recycling is disabled, this causes alloc counts
   and total (not peak) memory to skyrocket (nearly 60% increase in
   the 20160131 unit tests). With recycling on alloc counts go up by
   roughly 3% but peak memory goes up anywhere from 1k to 7k (as the
   recycling bins accumulate) in those same unit tests.

   After changing this, MAKE SURE to run all the valgrind tests with
   various combinations of recycling and string interning!!!

   Interestingly... if we'd done the right thing at the start of s2's
   development and told the s2_stoken_stack to hold/release refs, we'd
   probably have no problems with refs wrt string interning and how
   s2_engine::dotOpXXX are managed. At the time, i had too much faith
   in temp values because i didn't fully understand the implications
   of some of the interactions :/. There, i've admitted it. IIRC, we
   also didn't yet have cwal_value_unhand(), which is what makes some
   of the current ref handling possible/sane.

   20171112 re-enabling EVAL_USE_HOLDER to work around a serious bug:

   ./s2sh --S --a -v --R crash.s2

   with this crash.s2:

   if((var d=s2.getenv('HOME')) && (d = s2.fs.realpath(d+'/fossil'))) 0;

   It's crashing in the && operator, and it seems to be a lifetime
   problem related to the 'd' value (possibly getting hosed via the
   2nd assignment?). Running s2sh with recycling ON (-R instead of
   --R) hides the problem but potentially causes Weird Broken Stuff to
   happen later (as is being witnessed in the CGI plugin, which was
   where this was initially triggered). This is the first serious
   lifetime-related bug we've seen the core in a couple years :`(. A
   decent solution, aside from the eval holder, is as yet unknown :`(.

   Other formulations and permutations:

   (Forewarning: these are often dependent on the exact recycling
   options used and the internal state of the recycling system(s)!)

   var d; if((d = s2.getenv('HOME')) && (d = s2.fs.realpath(d+'/fossil'))) 0;
   // ^^^ crashes

   var d = s2.getenv('HOME'); if((d) && (d = s2.fs.realpath(d+'/fossil'))) 0;
   // ^^^ works

   (var d = 'a b c d e f g') && (d = d + 'h i j k') // crashes
   (var a = 30) && (a = a + 10) // works
   (var a = '30') && (a = a + '10') // crashes
   (var d = 'a b c d e f g') && (d = 'h i j k') // crashes
   (var a = 'x') && (a = 'x') // crashes
   (var a = 'x') + (a = 'y') // crashes
   (var a = 'x'), (a = 'y'), a // works
   (var a = 'x'), (a = a + 'y'), a // crashes
   (var a = 30), (a = a + 20), a // works

   So... the common element appears to be assigning to the same string
   value on the LHS and RHS of a binary operator.

   With the "eval holder" enabled, all of the above work as expected
   because the holder keeps refs to all of the values created during
   those expressions, unref'ing them at the very end of the expression
   (taking care to not kill the expression's final result value, if
   it's needed).

   The problem can (at times) be triggered both with and without
   string interning and/or the chunk recycler, which rules out the
   easy solution of getting rid of string interning.

   As of 20171112, don't disable this until/unless a resolution
   for the above problem is implemented.

   2020-02-20: the above story disregards the fact that even if the
   stack machine held refs to everything, we'd still need the eval
   holder to ensure vacuum-safety of in-progress expressions. It is a
   seemingly unavoidable feature of the engine.
*/
#define EVAL_USE_HOLDER 1

/**
   Internal helper for s2_eval_expr_impl(), which performs
   a sub-eval on keywords.

   Returns 0 on success.

   *nextTokenFlags will be set to 0 or to S2_NEXT_NO_SKIP_EOL, which
   must be respected in the next iteration of the main eval loop,
   treating an immediately-subsequent EOL as an EOX.

   *tVal will be set to the result of the keyword.

   st will be advanced to the last of the keyword's tokens.

   fromPt must be the currently-eval'ing token, and this function
   may change its ttype.

   fromLhsOp needs to be the operation taking place on the LHS, if
   any. If not NULL, then EOL is _never_ treated like an EOX. This is
   almost always null, but some few callers will want to pass
   s2_ttype_op(S2_T_Comma) so that their (sub)expression stops eval'ing
   at the first comma.
*/
static int s2_eval_keyword_call( s2_keyword const * kword,
                                 s2_ptoken * fromPt,
                                 s2_op const * fromLhsOp,
                                 s2_engine * se, s2_ptoker * st,
                                 cwal_value ** tVal,
                                 int * nextTokenFlags ){
  int rc;
  assert(kword->call);
  fromPt->ttype = st->token.ttype = kword->id;
  rc = kword->call(kword, se, st, tVal);
  s2_dotop_state( se, 0, 0, 0 );
  rc = s2_check_interrupted(se, rc);
  *nextTokenFlags = 0;
  if(rc) return rc;
  else if(!tVal){
    s2_ptoker_errtoken_set(st, fromPt);
    return s2_err_ptoker(se, st, CWAL_RC_ASSERT,
                         "Keyword '%s' eval'd to <NULL>, "
                         "which is currently verboten",
                         kword->word);
    /* Reminder to self: maybe we could abuse a NULL keyword
       result value for keywords which should expand to nothing,
       e.g. __sweep and __BREAKPOINT.
    */
  }
  else if((S2_T_SquigglyBlock==st->token.ttype
           || S2_T_Heredoc==st->token.ttype
           /* ^^ heredocs handling here is historic */)
          && !fromLhsOp && !se->st.vals.size
          /* ^^ keyword is the only thing on the stack */){
    /* If the keyword ends in a {script} and its the only
       expression (so far), then possibly treat a trailing
       newline as EOX. This is primarily so that
       scope/if/while/for/etc do not require a semicolon,
       but it might bleed into some other potentially
       unexpected cases.
    */
    if(kword->allowEOLAsEOXWhenLHS){
      *nextTokenFlags = S2_NEXT_NO_SKIP_EOL;
    }else{
      /* In order to avoid breaking certain cases, we have
         to check the next token. Heuristic: if the next
         token is an infix or postfix op, assume that it's
         to be applied to this result and treat EOL as
         normal, otherwise treat EOL as an implicit EOX.
           
         e.g.:
           
         print(catch{...}
         .message);
           
         If it fails tokenizing, ignore it - we'll catch
         it again in a moment.
      */
      s2_op const * opCheck = 0;
      s2_ptoken check = s2_ptoken_empty;
#if 0
      /* why doesn't this work the same? */
      s2_ptoker stMod = *st;
      do{
        rc = s2_ptoker_next_token_skip_junk(&stMod);
      }while(!rc && s2_ttype_is_eol(stMod.token.ttype));
      if(!rc){
        check = stMod.token;
      }
#else
      rc = s2_next_token( se, st, 0, &check );
#endif
      if(!rc
         && S2_T_ParenGroup!=check.ttype
         /* avoid that: proc (){}
            (1,2,3)
            unduly fails! This might come around and
            bite us, though. */
         && (!(opCheck = s2_ttype_op(check.ttype))
             || (opCheck->placement<0 /* prefix op */))
         ){
        *nextTokenFlags = S2_NEXT_NO_SKIP_EOL;
        /* MARKER(("Squiggly workaround for %s\n", kword->word)); */
        s2_ptoker_next_token_set(st, &check);
      }
    }
  }
  assert(!rc);
  return rc;
}

/**
   Internal impl of s2_eval_expr().

   If fromLhsOp is not 0, this call is assumed to be the RHS of that
   operator. Upon encountering an operator of that precedence or less,
   that operator is put back into the tokenizer and evaluation
   finishes. This is used in implementing short-circuit logic.

   TODO: break this beast down into smaller chunks! It's over 700 lines
   long!
*/
int s2_eval_expr_impl( s2_engine * se,
                       s2_ptoker * st,
                       s2_op const * fromLhsOp,
                       int evalFlags,
                       cwal_value ** rv){
  s2_ptoken pt = s2_ptoken_empty;
  s2_ptoken prevTok = s2_ptoken_empty;
  s2_ptoken const pOrigin = st->token;
  s2_ptoken const pbOrigin = *s2_ptoker_putback_get(st);
  int tlen = 0;
  int const oldSkipLevel = se->skipLevel;
  s2_op const * op = 0;
  s2_op const * prevOp = 0;
  s2_estack priorStack = s2_estack_empty;
  s2_ptoken captureBegin = s2_ptoken_empty
    /* starting point of capture (first non-junk token) */,
    captureEnd = s2_ptoken_empty
    /* one-after-the-end point of capture. It turns out that using a
       range of [captureBegin,captureEnd) simplifies handling notably
       over [captureBegin,captureEnd]. This is typically an
       expression's EOX, but will be the same as captureBegin for an
       empty expression (including one comprised solely of an EOX
       token, in which case both captureBegin and captureEnd will
       point to that EOX token). */;
  char const consumeIt = (evalFlags & S2_EVAL_NO_CONSUME) ? 0 : 1;
  char const evalIt = (evalFlags & S2_EVAL_SKIP) ? 0 : 1;
  s2_keyword const * kword = 0 /* keyword for the token, if any */;
  s2_ptoker const * oldScript = se->currentScript;
  char const ownStack =
#if 1
    (S2_EVAL_KEEP_STACK & evalFlags) ? 0 : 1
#else
    /*
      Bug: currently leads to extra stack items when short-circuit
      logic and sub-parses activate. Caused by how we unwind the
      stack (in full) at the end.
    */
    (fromLhsOp ? 0 : 1)
#endif
    ;
  int totalValCount = 0 /* just for some error checks */;
  int nextTokenFlags = /* The s2_next_token() flags for the very next token */
    (evalFlags & S2_EVAL_NO_SKIP_FIRST_EOL/* fromLhsOp && S2_T_RHSEvalNoEOL!=fromLhsOp->id */)
    ? S2_NEXT_NO_SKIP_EOL
    : 0;
  cwal_scope scope = cwal_scope_empty /* cwal stack, if (flags&S2_EVAL_PUSH_SCOPE) */;
#if EVAL_USE_HOLDER
  cwal_array * holder = 0 /* holds pending expression values to keep
                             them referenced and vacuum-safe. */;
  cwal_size_t holderLen = 0 /* length of
                               se->scopes.current->evalHolder at the
                               time this function is called */;
#endif
#ifdef DEBUG
  /* Just for sanity checking */
  int const oldValCount = se->st.vals.size;
  int const oldOpCount = se->st.ops.size;
#endif
  int rc = s2_check_interrupted(se, 0);

  if( rc ) return rc;
  else if(!fromLhsOp
          /* Explicitly NOT honoring !(S2_EVAL_RETAIN_DOTOP_STATE &
             evalFlags) at the start of eval, only at the end. */
          ) s2_dotop_state(se, 0, 0, 0);
  if(S2_EVAL_PUSH_SCOPE & evalFlags){
    MARKER(("Oh, this scope-pushing bit in eval_expr_impl IS used.\n"));
    assert(!"This isn't used, is it?");
    rc = cwal_scope_push2(se->e, &scope);
    if(rc) return rc;
    assert(scope.parent);
    assert(0==se->scopes.current->sguard.sweep);
    assert(0==se->scopes.current->sguard.vacuum);
    /* se->scopes.current->sguard.sweep = 0; */
    /* REMINDER: we cannot blindly disable se->sguard->vacuum because it
       can nuke non-script-visible values in use by the client. Been there,
       done that.
    */
  }

  if(++se->metrics.subexpDepth > se->metrics.peakSubexpDepth){
    se->metrics.peakSubexpDepth = se->metrics.subexpDepth;
  }

  s2_ptoker_errtoken_set(st, 0);

  /* if(rv) *rv = 0; */
  if((S2_EVAL_PRE_SWEEP & evalFlags)
     && !fromLhsOp
     && !se->st.vals.size
     && !se->st.ops.size
     ){
    /* MARKER(("SWEEPING from eval_expr_impl\n")); */
    /* We cannot know if a vacuum is safe from here :/,
     and it certainly isn't if s2_eval_ptoker() is waiting
     on a pending EOX. */
    assert(!"unused/untested");
    ++se->scopes.current->sguard.vacuum;
    s2_engine_sweep(se);
    --se->scopes.current->sguard.vacuum;
  }
  if(!scope.parent){
    /* reminder to self: needed only when no scope has been pushed by
       this routine. If this routine pushed a scope (that feature is
       currently unused/untested) then there cannot be pending LHS
       expressions which could be affected by a sweep in the
       newly-pushed scope.
    */
    ++se->scopes.current->sguard.sweep;
  }

  se->currentScript = st;

  s2_engine_err_reset(se);
  if(!evalIt) ++se->skipLevel;
  if(ownStack){
    s2_engine_stack_swap(se, &priorStack);
  }


#if EVAL_USE_HOLDER
#  define eval_hold_this_value(ARRAY, VALUE) (cwal_value_is_builtin(VALUE) \
                                    ? 0                                 \
                                    : cwal_array_append((ARRAY), (VALUE)))
#  define eval_hold(V) if( holder && (V) \
                           && (rc = eval_hold_this_value(holder, (V)))) goto end
  /*
    Create se->scopes.current->evalHolder to hold refs to our
    being-eval'd values. As an optimization, if we're in skip mode,
    don't bother... and let's hope that this function is never called
    in such a way that skip mode is disabled by a downstream sub-eval
    (because this optimization will hose us if that happens). That
    case shouldn't be possible, though.
  */
  if(!se->skipLevel && !(holder = se->scopes.current->evalHolder)){
    holder = cwal_new_array(se->e);
    if(!holder){
      rc = CWAL_RC_OOM;
      goto end;
    }else{
      cwal_value * hv = cwal_array_value(holder);
      cwal_value_ref(hv);
#if 0
      /* array_reserve() costs us inordinate amounts of memory
         when recycling is disabled. Tried values 0, 10, 15, 20,
         and 0 is the clear winner.*/
      rc = cwal_array_reserve(holder, 0/*guess!*/);
      if(rc){
        cwal_value_unref(hv);
        holder = 0;
        goto end;
      }
#endif
      se->scopes.current->evalHolder = holder;
      cwal_value_make_vacuum_proof(hv,1);
      cwal_value_rescope(se->scopes.current->cwalScope, hv)
        /* in case a cwal-level API has pushed a scope without us
           knowing */;
    }
  }else if(holder){
    holderLen = cwal_array_length_get(holder)
      /* Get the older (pre-eval) length so that we can trim holder
         back to that length when we're done, releasing any refs this
         invocation of eval() is holding. */;
  }
#else
#  define eval_hold(V)
#endif
/* ^^^^ EVAL_USE_HOLDER */
  
  pt = st->token;

  /*
    Adapted from:

    https://en.wikipedia.org/wiki/Stack_(data_structure)#Expression_evaluation_and_syntax_parsing

    https://en.wikipedia.org/wiki/Shunting-yard_algorithm

    https://en.wikipedia.org/wiki/Operator-precedence_parser

    For later reference (but not used here): Pratt Parsing:
    https://en.wikipedia.org/wiki/Pratt_parser
    https://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
  */
  for( ; !rc &&
         (
          (prevTok.ttype
           && !op
           && s2_ttype_is_eox(st->token.ttype)
           /*
             ^^^^ translation: if we had a previous token which was
             not an operator and we're currently at an EOX, then stop
             processing without an error (though we may trigger one in
             an up-coming step).

             Sub-tokenizations can end on an EOX and still produce a
             value. i would love to be able to do without this check
             here (and it might be a side-effect of older EOL rules?).

             This case appears once keywords and short-circuiting
             enter this mix (i.e. sub-parses/sub-evals).

             Note that we allow EOX's through in some cases just so
             that we can do some more error checking/reporting.
           */)
          ? 0
          : (0 == (rc = s2_next_token(se, st, nextTokenFlags, 0)))
          )
         ;
       prevOp = op, prevTok = pt
       ){
    cwal_value * tVal = 0 /* Value for the token, if any */;
    int doBreak = 0 /* set to non-0 to force a non-error multi-level
                       break in certain places below. */;
    if((rc=s2_check_interrupted(se, rc))) break;
    if(!s2_ptoken_begin(&captureBegin)){
      captureBegin = st->token;
    }

    if(s2_ttype_is_eox(st->token.ttype)){
      if(/*!fromLhsOp &&  <--- 20171205: why did we do that?
           having that triggers an assert() via: new X( y. )
           (note the stray '.'). */
         prevOp
         && ((prevOp->placement<0 && prevOp->arity>0)/*prefix op*/
             ||(prevOp->placement==0 && prevOp->arity>1)/*infix op*/)
         ){
        /* infix/prefix operator preceeding EOX */
        /* s2_ptoker_errtoken_set(st, s2_ptoken_begin(&prevTok) ? &prevTok : &st->token); */
        rc = s2_err_ptoker(se, st, CWAL_SCR_SYNTAX,
                                  "Invalid operator '%s' before "
                                  "end of expression",
                                  prevOp->sym);
      }
      else if(s2_ttype_is_eof(pt.ttype)
              /* yes, checking previous token, not current one */){
        if(prevOp
           && prevOp->arity>0
           && prevOp->placement<=0
           ){
          rc = s2_err_ptoker( se, st, CWAL_SCR_SYNTAX,
                              "Non-nullary operator '%s' "
                              "before EOF", prevOp->sym);
        }else if(fromLhsOp){
          rc = s2_err_ptoker( se, st, CWAL_SCR_SYNTAX,
                              "Expecting RHS but got EOF");
        }
      }
      else if(S2_T_EOX == st->token.ttype
              && (evalFlags & S2_EVAL_EOX_SEMICOLON_NOT_ALLOWED)){
        /* Bugfix: catch these as syntax errors:

           [1,2,3;]
           [1,2,3;,4]
           {a:b, c:d ;}

           Without this check, those semicolons are silently ignored
           because they're the tail end of the values' expressions
           (and thus officially end the expressions). That's arguably
           not a bug, given the eval engine's nature, but we'll go
           ahead and flag it as an error just to be pedantic (even
           though it means adding more code just to catch this)..
        */
        rc = s2_err_ptoker( se, st, CWAL_SCR_SYNTAX,
                            "Unexpected semicolon.");
      }
      /*
        Would like to do this but is currently endlessly looping
        in s2_eval_ptoker():

        s2_ptoker_putback(st);
      */
      break;
    }else if(kword
             && kword->allowEOLAsEOXWhenLHS
             && (S2_NEXT_NO_SKIP_EOL & nextTokenFlags)
             && s2_ttype_is_eol(st->token.ttype)
             ){
      /* Special case for keywords with a {script} operand.
         Treat this EOL as EOX */
      break;
    }
    nextTokenFlags = 0;
    kword = 0;
    pt = st->token;
    /*captureEnd = pt; Reminder to self: setting captureEnd here does
      not work with break/continue/return handling (and similar): it
      captures the keyword but not the optional expression after it
      (which arrives back in this loop via error handling). For most
      cases we have to wait until after this loop to set
      captureEnd. */
    tlen = s2_ptoken_len(&pt);
    assert(tlen>0);
    if(se->flags.traceTokenStack){
      MARKER(("Token: fromLhsOp=%s type=%s [%.*s]\n",
              fromLhsOp ? fromLhsOp->sym : "none",
              s2_ttype_cstr(pt.ttype), tlen, s2_ptoken_begin(&pt)));
    }
    switch(pt.ttype){ /* Some basic sanity checks */
      case S2_T_SquigglyOpen:
      case S2_T_HeredocStart:
      case S2_T_BraceOpen:
      case S2_T_ParenOpen:
        assert(!"Cannot happen - gets handled by s2_next_token()");
        /* But in case it does, fall through... */
        CWAL_SWITCH_FALL_THROUGH;
      case S2_T_SquigglyClose:
      case S2_T_BraceClose:
      case S2_T_ParenClose:
        rc = s2_err_ptoker(se, st, CWAL_SCR_SYNTAX,
                           "Mismatched '%.*s' token",
                           tlen, s2_ptoken_begin(&pt));
        break;
      case S2_T_At:
        rc = s2_err_ptoker(se, st, CWAL_SCR_SYNTAX,
                           "'@' is not allowed here.");
        break;
      case S2_T_Question:{
        cwal_value * lhs;
        if(fromLhsOp && (S2_T_OpAnd==fromLhsOp->id
                         || S2_T_OpOr==fromLhsOp->id
                         || S2_T_OpOr3==fromLhsOp->id
                         || S2_T_OpElvis==fromLhsOp->id)
           ){
          /*
            Workaround to avoid short-circuiting the RHS of a ternary
            when its LHS is a short-circuiting logical op which is
            skipping over its own RHS:
             
            0 && 1 ? a : b;
            1 || 0 ? a : b;

            We put back this token and come back to it after the
            logical op resolves.

            This is a bit of a hack, admittedly, but i haven't got a
            better approach for the time being.
          */
          captureEnd = st->token;
          s2_ptoker_putback(st);
          doBreak = S2_T_Question;
          break;
        }
        lhs = prevOp && prevOp->placement<=0 ? 0 : s2_engine_peek_value(se);
        if(!lhs){
          rc = s2_err_ptoker( se, st, CWAL_SCR_SYNTAX,
                              "Unexpected LHS (%s%s) for %s operator.",
                              prevOp ? prevOp->sym
                              : (lhs ? cwal_value_type_name(lhs)
                                 : "<NULL>"),
                              prevOp ? " operation" : (lhs ? " value" : ""),
                              s2_ttype_op( S2_T_Question )->sym);
          break;
        }
        lhs = 0;
        rc = s2_eval_ternary(se, st, &lhs);
        if(rc) break;
        else{
          op = 0
            /* 20180507: avoid confusion in the next iteration.

               var a, b;
               a++ ? 1 : 2; b

               ==> Unexpected token after postfix operator: (OpIncrPost) ==> (Identifier)

               That triggers after the ternary expr completes, but
               adding a second semicolon after the ternary expr fixes
               it.

               Other postfix ops can also trigger it.
            */;
          assert(lhs);
          eval_hold(lhs);
          continue;
        }
      }
      case S2_T_Identifier:{
        /* If an Identifier is the RHS of a dot operator, change its
           type to PropertyKey so that identifier expansion is not
           done for this property access. The raw token bytes
           will be converted to a string value later on in this
           routine. Implications:

           obj.prop ==> Identifier DOT LiteralString

           obj.'prop' ==> effectively the same thing

           obj.(prop) ==> Identifier DOT ( Identifier )

           Seems reasonable to me.
        */
#if 0
        s2_stoken const * topOpTok = s2_engine_peek_op(se);
        if(topOpTok && (S2_T_OpDot==topOpTok->ttype)){
          pt.ttype = S2_T_PropertyKey;
        }
#else
        if(prevOp && s2_op_rhs_as_propkey(prevOp)){
          pt.ttype = S2_T_PropertyKey;
        }
#endif
        break;
      }
      case S2_T_OpIncr:
      case S2_T_OpDecr:
        /* Convert -- and ++ to either prefix or postfix form, depending
           on the state of the stack... */
        if(prevOp || !se->st.vals.size){/* xxxx misdiagnosis as post-op on mis-continued lines */
          pt.ttype = (S2_T_OpIncr==pt.ttype)
            ? S2_T_OpIncrPre
            : S2_T_OpDecrPre;
        }else{
          pt.ttype = (S2_T_OpIncr==pt.ttype)
            ? S2_T_OpIncrPost
              : S2_T_OpDecrPost;
        }
#if 0
        op = s2_ttype_op(pt.ttype);
        assert(op);
        MARKER(("Changing token '%s' to %s form.\n", op->sym,
                op->placement>0 ? "postfix" : "prefix"));
#endif
        break;
      default:
        break;
    }
    rc = s2_check_interrupted(se,rc);
    if(rc || doBreak) break;

    op = s2_ttype_op( pt.ttype );
#if 0
    if(op && !fromLhsOp){
      s2_dotop_state( se, 0, 0, 0 )
        /* If these get stale, Very Bad Things happen in the cwal core
             (assertions, if we're lucky). */;
      MARKER(("op=%s, dotop state?\n",op->sym));
      /* se->dotOpId = 0 */ /* keep Other Bad Things from happening */;
      /* Reminder: this means that assignment ops cannot tell
         which dot-like op triggered them. */
    }
#else
    /*
      We need a better heuristic, as the above makes this fail:

      x.y++; // fine
      x.y++ >= 2; // "Invalid LHS..." from the incrdecr op because dotop state is cleared.
    */
    if(op && !fromLhsOp){
      if(!prevOp){
        /* MARKER(("op=%s, prevOp=%s, resetting dotop state\n",op->sym, prevOp ? prevOp->sym : "<NULL>")); */
        s2_dotop_state( se, 0, 0, 0 );
      }else{
        /* MARKER(("op=%s, prevOp=%s, keeping dotop state\n",op->sym, prevOp->sym)); */
      }
    }
#endif

#if 0
    if(fromLhsOp){
      MARKER(("fromLhsOp=%s, pt.type=%s\n", s2_ttype_cstr(fromLhsOp->id), s2_ttype_cstr(pt.ttype)));
    }
#endif

    if(S2_T_Colon==pt.ttype){
      if(se->ternaryLevel>0){
        /* s2_eval_ternary() will deal with it */
        captureEnd = st->token;
        s2_ptoker_putback(st)
          /* This putback causes certain grief for foreach(). */;
        s2_ptoker_next_token_set(st, &pt) /* part of a kludge in s2_eval_is_eox().
                                             Also incidentally useful here. */;
      }else{
        rc = s2_err_ptoker(se, st, CWAL_SCR_SYNTAX,
                           "Unexpected ':' token (no %s op in progress).",
                           s2_ttype_op(S2_T_Question)->sym);
      }
      break;
    }else if(fromLhsOp
             /* This eval_expr() call is fetching the RHS of a pending
                expression. */
             && op
             && (op->prec < fromLhsOp->prec
                 || (op->prec==fromLhsOp->prec
                     /* So (a=b=c=d) can work. */
                     && op->assoc<0))){
      /* This invocation was catching an RHS, but we
         can stop now. */
      /*MARKER(("This '%s' is where a sub(ish)-eval for RHS "
              "'%s' skipping would break off.\n",
              op->sym, fromLhsOp->sym));*/
      captureEnd = st->token;
      s2_ptoker_putback(st);
      break;
    }

    /* Fiddle with consecutive ops for some cases... */
    if(op && (prevOp || !s2_engine_peek_token(se))){
      /* Leading operator or two consecutive operators. */
      switch(op->id){
        case S2_T_OpPlus:
        case S2_T_OpMinus:
            /* See if op can be made into a unary op. */
          if(
             !prevOp || s2_ttype_may_precede_unary(prevOp->id)
             ){
            /* MARKER(("Changing token '%s' to unary form.\n", op->sym)); */
            pt.ttype = (S2_T_OpPlus==op->id)
              ? S2_T_OpPlusUnary
              : S2_T_OpMinusUnary;
            op = s2_ttype_op(pt.ttype);
          }
          break;
        case S2_T_OpHash:
            if(prevOp && S2_T_OpDot==prevOp->id){
                /* Convert the dot op in X.# to S2_T_OpDotLength. */
                s2_stoken * topTok = s2_engine_peek_op(se);
                assert(S2_T_OpDot==topTok->ttype);
                assert(topTok->ttype == prevOp->id);
                topTok->ttype = S2_T_OpDotLength;
                op = s2_ttype_op(topTok->ttype)
                    /* so that prevOp gets set for the next iteration */;
                assert(op);
                continue;
            }
            break;
        default:
          break;
      }
      if(!rc
         && op
         && prevOp && prevOp->arity>0 && prevOp->placement<=0
         && op->placement>=0 /* && op->assoc<=0 */ && op->arity!=0
         /* e.g. OpPlus ==> OpMultiply */){
        /* consecutive non-nullary operators */
        if(op->assoc<=0){
          rc = s2_err_ptoker(se, st, CWAL_SCR_SYNTAX,
                                    "Unexpected consecutive operators: "
                                    "'%s' ==> '%s'",
                                    prevOp->sym, op->sym);
        }
      }
    }/*op vs prevOp check*/

    if(rc) break;
    else if(!op){
      /* A non-operator ("value") token */
      kword = s2_ptoken_keyword2(se, &pt);
      if(prevOp
         && (!kword || (kword && kword->call/*non-operator-like keyword*/))
         && prevOp->placement>0/*postfix*/ && prevOp->arity>0/*non-nullary*/){
        /* e.g. a++a */
        rc =  s2_err_ptoker(se, st, CWAL_SCR_SYNTAX,
                            "Unexpected token after postfix operator: "
                           "(%s) ==> (%s)",
                            s2_ttype_cstr(prevOp->id),
                            s2_ttype_cstr(pt.ttype));
        break;
      }
      if(!prevOp
         && /*prevTok.ttype*/ s2_engine_peek_value(se)
         && (S2_T_BraceGroup!=pt.ttype) /* for obj[prop] access */
         && (S2_T_ParenGroup!=pt.ttype) /* so func() can work */
         && (!kword/* || kword->call*/ /* for 'inherits' and potential
                                       future operator-like keywords */)
         ){
#if 0
        /* who is setting se->opErrPos to this (correct) location? */
        if(se->opErrPos){
          MARKER(("opErrPos=[%.30s...]\n", se->opErrPos));
          st->errPos = se->opErrPos;
        }else{
          se->opErrPos = st->errPos =  s2_ptoken_begin(&pt);
        }
#else
        /* se->opErrPos = 0; resetting this here causes 'catch' to misreport
         line numbers in some cases! */
        s2_ptoker_errtoken_set(st, &pt);
        se->opErrPos = s2_ptoken_begin(&pt)
          /* se->opErrPos trumps st->errPos in s2_err_ptoker()
             and is currently (for unknown reasons) being relied
             upon by a containing 'catch' for proper behaviour.
             That's a bug. */;
#endif
        rc = s2_err_ptoker(se, st, CWAL_SCR_SYNTAX,
                           "Unexpected consecutive non-operators: "
                           "#%d (%s) ==> #%d (%s)" /* " [%.*s]" */,
                           prevTok.ttype,
                           s2_ttype_cstr(prevTok.ttype),
                           pt.ttype, s2_ttype_cstr(pt.ttype)
                           /* tlen, s2_ptoken_begin(&pt) */
                           );
        break;
      }
    }

    /* Some (more) checks and pre-processing... */
    switch(pt.ttype){
      case S2_T_OpOr:
      case S2_T_OpOr3:
      case S2_T_OpElvis:
      case S2_T_OpAnd:{
        /*
          For short-circuit logic we need both this loop and the
          stack to be in skip mode, but we also have to know when
          to back out of skip mode.

          Evaluate RHS expression in skip mode when:

          - op==OpOr or OpOr3 and topVal is truthy,
          - op==OpElvis and topVal !== undefined
          - op==OpAnd and topVal is falsy
        */
        cwal_value * lhs = 0;
        char buul = 0;
        char shortIt = 0;
        int const theOpId = pt.ttype;
        rc = s2_eval_lhs_ops(se, st, op);
        if( rc ) break;
        lhs = s2_engine_peek_value(se);
        if(!lhs) break /* will error out below */;
        else if(se->skipLevel>0) break;
        if(S2_T_OpElvis==pt.ttype){
          shortIt = (cwal_value_undefined() == lhs) ? 0 : 1;
        }else{
          buul = cwal_value_get_bool(lhs);
          if(!buul){
            if(pt.ttype==S2_T_OpAnd){
              shortIt = 1;
            }
          }else{
            if(pt.ttype!=S2_T_OpAnd){
              assert(S2_T_OpOr==pt.ttype
                     || S2_T_OpOr3==pt.ttype);
              shortIt = 1;
            }
          }
        }
        if(shortIt){
          /*
            short-circuit the RHS by calling back into this function
            and passing it the current op as a cut-off precedence.
          */
          cwal_value * rhs = 0;
          /* int const oldOpSize = se->st.ops.size; */
          /* MARKER(("op %s should short circuit RHS here.\n", op->sym)); */
          rc = s2_eval_expr_impl(se, st, op,
                                 S2_EVAL_SKIP /* | S2_EVAL_KEEP_STACK */,
                                 &rhs)
            /* This causes the following to parse incorrectly:

               0&&1 ? 2 : 3; ===> false instead of 3

               _Effectively_ parses as:

               0 && (1 ? 2 : 3)

               Because of the sub-eval. It actually parses right, but the
               skipLevel is still set after the &&, so the end effect is that
               the whole ?: gets skipped.

               Contrast with:

               (0&&1) ? 2 : 3; ===> 3

               which works because of the expression boundary there.

               Anyway: worked around up above in the S2_T_Question
               handling.
            */
            ;
          /* MARKER(("after %d op: skipLevel=%d\n", pt.ttype, se->skipLevel)); */
          assert(lhs == s2_engine_peek_token(se)->value);
          if(rc){
            /*
              Reminder: lhs is now an errant value, possibly a temp.
              It's potentially orphaned for the time being.
              (Later: except that it's currently in the eval-holder.)
            */
            break;
          }
          /*MARKER(("rhs-eval rc=%s. capture=%.*s\n", cwal_rc_cstr(rc),
            (int)(s2_ptoken_begin(&captureEnd)-s2_ptoken_begin(&captureBegin)),
            s2_ptoken_begin(&captureBegin)
            ));*/
          /*if( (rc = s2_check_interrupted(se, rc)) ) break;
            else */
          if(!rhs){
            s2_ptoker_errtoken_set(st, &pt);
            rc = s2_err_ptoker(se, st, CWAL_SCR_SYNTAX,
                               "Empty RHS for logic operator");
          }else{
            s2_stoken * topTok = s2_engine_pop_token(se, 1) /* the short-circuited value */;
            assert(topTok->value);
            assert(lhs == topTok->value);
            topTok->value = 0;
            s2_stoken_free(se, topTok, 1);
            assert(cwal_value_undefined()==rhs
                   && "Violation of current API convention for skip-mode evals");
            s2_engine_push_val(se, (S2_T_OpOr3==theOpId || S2_T_OpElvis==theOpId)
                               ? lhs
                               : cwal_new_bool(buul)
                               )->srcPos = pt
              /* that allocation cannot fail b/c it will
                 recycle the popped one unless someone foolishly
                 disables the stack token recycler. */;
            /* s2_dump_val(lhs, "new LHS"); */
            /* assert(lhs == s2_engine_peek_value(se)); */
            op = 0 /* so prevOp does not expose our trickery to the next
                      iteration */;
            if(S2_T_OpOr3 != pt.ttype && S2_T_OpElvis != pt.ttype){
              /*
                Clean this up:
                s2> x = 0; for( var i = 0; i < 100; (i++, x++ || 1, ++i) );
                MARKER: s2.c:296:s2_engine_sweep():	Swept up 48 value(s) in sweep mode
              */
              cwal_refunref(lhs);
            }else{
              eval_hold(lhs);
            }
            continue;
          }
        }/*if(shortIt)*/
        break;
      }/* end &&, ||, ||| */
      case S2_T_Identifier:{
        /*
          Identifiers on the RHS of a dot are re-tagged (above) as
          PropertyKey, which will cause them not to be seen as
          identifiers or keywords here.

          Try a keyword...
        */
        if(kword){
          if(!kword->call){
            /* Transform keywords which get treated as operators
               into those operators... */
            s2_op const * kOp = s2_ttype_op(kword->id);
            assert(kOp);
            if(/* check for !inherits (two tokens) and transform
                  them into a single operator, !inherits. */
               S2_T_OpInherits == kOp->id
               && prevOp
               && S2_T_OpNot==prevOp->id){
              assert(s2_engine_peek_op(se)
                     && s2_engine_peek_op(se)->ttype == prevOp->id);
              s2_engine_pop_op(se,0);
              pt.ttype = S2_T_OpNotInherits;
              kOp = s2_ttype_op(pt.ttype);
              assert(kOp);
              assert(S2_T_OpNotInherits == kOp->id);
              prevOp = 0 /* avoid dowstream confusion */;
            }else{
              pt.ttype = kword->id;
            }
            op = kOp;
          }/* end operator-like words*/
          else{
            rc = s2_eval_keyword_call(kword, &pt, fromLhsOp,
                                      se, st, &tVal, &nextTokenFlags);
          }
        }/* end keywords */
        else if(se->skipLevel>0){
          /*
            In skip mode, we are obligated to resolve as much as we
            optimally can, and we resolve everything to undefined in
            that case.
          */
          tVal = cwal_value_undefined();
        }else{
          /* Non-keyword identifier, not in skip mode, so resolve
             it... */
          s2_stoken const * opT = s2_engine_peek_op(se);
          s2_op const * topOp = opT ? s2_ttype_op(opT->ttype) : 0;
          assert(opT ? !!topOp : 1);
          tVal = s2_var_get(se, -1, s2_ptoken_begin(&pt), (cwal_size_t)tlen);
          if(!tVal){
            if(S2_EVAL_UNKNOWN_IDENTIFIER_AS_UNDEFINED & evalFlags){
              evalFlags &= ~S2_EVAL_UNKNOWN_IDENTIFIER_AS_UNDEFINED;
              tVal = cwal_value_undefined();
            }else{
              rc = s2_throw_ptoker(se, st, CWAL_RC_NOT_FOUND,
                                   "Could not resolve identifier "
                                   "'%.*s'",
                                   tlen, s2_ptoken_begin(&pt));
            }
          }
          else if(topOp && s2_ttype_is_identifier_prefix(topOp->id)){
            /* Previous token was prefix ++/-- (or similar) */
            if(s2_next_is_dotish(se, st)){
              /* we'll put the resolved tVal in the stack. */
              pt.ttype = S2_T_Value;
            }else{
              /* put the unresolved identifier on the stack as a string-type
                 value. */
              rc = s2_ptoken_create_value(se, st, &pt, &tVal);
              if(!tVal){
                assert(rc);
                /* rc = CWAL_RC_OOM; */
              }
            }
          }
          else if(s2_next_wants_identifier(se, st)){
            /* Required so that assignment gets the identifier's string value,
               but we let it get validated above so that we get precise
               error location info. We just hope that pending ops don't
               invalidate it (potential corner case?).
            */
            tVal = 0;
            rc = s2_ptoken_create_value(se, st, &pt, &tVal);
            /*
              Potential problem here: tVal might return to a shared/stashed
              string and we cannot add a ref here. If we later assume it's
              new and simply unref it, we will hose a valid Value being used
              somewhere else (e.g. in se->stash).
            */
            if(!rc){
              assert(tVal);
            }
          }else{
            pt.ttype = S2_T_Value /* So as to not confuse
                                     the result with its identifier */;
          }
        }
        break;
      }/*end Identifier*/
      case S2_T_SquigglyBlock:{
        rc = s2_eval_object_literal( se, st, &tVal );
        if( !rc ){
          assert(tVal);
          pt.ttype = S2_T_Value;
        }
        break;
      }
#if 0
        /*
          The DotPrototype op (..). While the code below works just
          fine, after having used this in scripts i find myself deleting
          it and typing ".prototype" to improve the readability. i'm
          going to disable the .. op for now and reserve it for something
          more useful (don't yet know what, except that numeric ranges
          come to mind).
         */
      case S2_T_OpDotDot:{
        /*
          Transform this token to:

          DOT "prototype"

          We do this here, instead of at the operator level, because
          this approach (A) required much less code and (B) works out
          of the box with assignment:

          x.. = bar;

          and, by extension, the ++/-- ops, without a lot of extra
          fiddling in the operator code.
        */
        if(prevOp && prevOp->arity>0 && prevOp->placement<=0){
          rc = s2_err_ptoker(se, st, CWAL_SCR_SYNTAX,
                             "Invalid operator '%s' preceeding '%s'.",
                             prevOp->sym, op->sym);
        }else if(0==(rc = s2_eval_lhs_ops(se, st, s2_ttype_op(S2_T_OpDot)))){
          s2_stoken * synthTok = s2_engine_push_ttype( se, S2_T_OpDot );
          if(!synthTok) rc = CWAL_RC_OOM;
          else{
            synthTok->srcPos = pt;
            pt.ttype = S2_T_Identifier;
            tVal = se->skipLevel ? cwal_value_undefined() : se->cache.keyPrototype;
            prevOp = 0;
            op = 0;
            /* ^^^^ that setup will cause "prototype" to be pushed
               onto the value stack as the RHS. The injected dot op
               will do the LHS checking. It turns out that .prototype
               is valid for the majority of expression results.
            */
          }
        }
        break;
      }
#endif
/* S2_T_OpDotDot */
      case S2_T_BraceGroup:{
        /*
          Here's how we do VALUE[...] ...

          Sub-parse the [...] exactly like for a (...) block, but also
          (on success) push a Dot operator onto the stack. That will
          resolve to (VALUE DOT RESULT), which is actually exactly
          what we want.
        */
        cwal_value * lhs;
        s2_stoken const * topOpTok = s2_engine_peek_op(se);
        s2_op const * topOp = s2_stoken_op(topOpTok);
        if(!prevOp /* prev part (if any) was a value */
           && topOp && s2_ttype_is_deref(topOp->id) /* last op is a pending dot */
           ){
          /* Need to process any pending (single) LHS dot operator
             to support chaining. */
          /* MARKER(("Processing pending(?) LHS dot op\n")); */
          rc = s2_process_top(se);
          if( rc ){
            rc = s2_ammend_op_err(se, st, rc);
            assert(rc);
          }
          topOpTok = 0;
        }
        if(rc) break;
        lhs = s2_engine_peek_value(se);
#if 0
        /* Reminder to self: string interning can bite us here (again)
           with missing refs on lhs unless we eval_hold() lhs. Because
           this is a more general problem, that hold() was added to
           s2_process_op_impl() on 20171115. A case which can (but
           doesn't always) trigger a crash without this eval_hold()
           is: 'abc'[2][0][0][0][0][0]
        */
        eval_hold(lhs);
#endif
        if(prevOp || !totalValCount){
          /* Treat as array literal... */
          rc = s2_eval_array_literal( se, st, &tVal );
          if( !rc ){
            pt.ttype = S2_T_Value;
          }
          break /* avoid ParenGroup fall-through below */;
        }
        else if(!se->skipLevel
                && (!lhs
                    || prevOp
                    || (/* !prevOp && */
                        !cwal_value_is_unique(lhs) /* for EnumEntry['value'] */ &&
                        !cwal_value_container_part(se->e, lhs)))
                ){
          /* Syntax error */
          if(prevOp) s2_ptoker_errtoken_set(st, &prevTok);
          rc = s2_throw_ptoker( se, st, CWAL_RC_TYPE,
                                "Invalid LHS (%s %s) "
                                "preceeding [...] block.",
                                s2_ttype_cstr(prevTok.ttype),
                                prevOp
                                ? prevOp->sym
                                : (lhs
                                   ? cwal_value_type_name(lhs)
                                   : "<NULL>")
                                );
        }
        if(rc) break;
        CWAL_SWITCH_FALL_THROUGH;
      }
      case S2_T_ParenGroup:{
        /*
          We expand these into Value tokens by doing a recursive eval
          on the byte range enclosed by the group. Call arguments for
          functions and 'new' are evaluated separately by
          s2_eval_fcall() resp. s2_keyword_f_new(), and arguments to
          function-like keywords are handled by the respective
          keywords. Thus some uses of parens never land here.
        */
        int const braceType = pt.ttype;
        char const braceOpen = *s2_ptoken_begin(&pt);
        char const braceClose = *(s2_ptoken_end(&pt)-1);
        char emptySet = 1;
        assert(S2_T_ParenGroup==pt.ttype || S2_T_BraceGroup==pt.ttype);
        assert(S2_T_ParenGroup==pt.ttype
               ? ('('==braceOpen && ')'==braceClose)
               : ('['==braceOpen && ']'==braceClose));
        assert(s2_ptoken_adjbegin(&pt));
        assert(s2_ptoken_adjend(&pt));
        assert(s2_ptoken_adjend(&pt) >= s2_ptoken_adjbegin(&pt));
        if(S2_T_ParenGroup==pt.ttype){
          doBreak = 0;
          if(s2_looks_like_fcall(se, st,
                                 (S2_EVAL_STOP_AT_CALL & evalFlags),
                                 prevOp, &rc) && !rc){
            if(S2_EVAL_STOP_AT_CALL & evalFlags){
              doBreak = S2_T_ParenGroup;
            }else{
              /* MARKER(("Looks like func call? rc=%s prevOp=%s\n",
                 cwal_rc_cstr(rc), prevOp?prevOp->sym:"<NULL>")); */
              rc = s2_eval_fcall(se, st, &tVal);
            }
            break;
          }
          else if(rc) break;
        }
        if(s2_ptoken_adjend(&pt) > s2_ptoken_adjbegin(&pt)){
          /* In order to know whether it's really empty, and fail
             consistently across both skipping and non-skipping mode,
             we have to parse the subexpr regardless of skip level
             :/.

             2021-06-24: we need to push a scope for this to avoid:

             var o = {}; o[var x = 'hi'] = 1; assert 'hi'===x;

             But we want standalone (...) to run in the current
             scope. We have script code which relies on that:

             1 && (var y = 3);
             assert 3 === y;
          */
          cwal_scope scope = cwal_scope_empty;
          assert(s2_ptoken_begin(&pt) == s2_ptoken_begin(&st->token));
          s2_ptoker_token_set(st, &pt) /* consume it */;
          if(!se->skipLevel){
            char const isParens = S2_T_ParenGroup==pt.ttype ? 1 : 0;
            s2_stoken const * topOpTok = isParens ? s2_engine_peek_op(se) : 0;
            s2_op const * topOp = topOpTok ? s2_stoken_op(topOpTok) : 0;
            if(S2_T_BraceGroup==pt.ttype
               || (topOp && s2_ttype_is_deref(topOp->id)
                   /* last op is a pending dot */)){
               rc = cwal_scope_push2(se->e, &scope);
            }
          }
          if(!rc){
            rc = s2_eval_current_token(se, st, 0, 0, &tVal)
              /* Reminder to self: passing true as the 3rd arg
                 breaks this arguable feature: (a;b).
              */;
          }
          if(scope.level){
            cwal_scope_pop2(se->e, rc ? 0 : tVal);
            if(rc) tVal = 0 /*was possibly cleaned up by scope pop*/;
          }
          if(!rc){
            emptySet = tVal ? 0 : 1;
            /* s2_dump_val(tVal, "post-() tVal"); */
          }
        }else{
          emptySet = 1;
        }
        if(rc) break;
        else if(emptySet){
          assert(!tVal);
          if(se->skipLevel
             || (!prevOp
                 && S2_T_BraceGroup==braceType
                 && cwal_value_array_part(se->e, s2_engine_peek_value(se)))){
            /*
              PHP-style array-append operator:

              If LHS val is-a array and braceOpen=='[' then push a
              an ArrayAppend op and assert that the next token
              is an assignment op. Alternately, we could just assume
              an assignment op and elide one if it appears. But we'll
              go ahead and be strict for now.

              Minor bug (20160106):

              var v = []; v.blah = 1;
              assert v.blah === (v[] = v.blah);

              The parens should not be required. What happens is
              that this:

              assert v.blah === v[] = v.blah;

              thinks the LHS of the []= op is a boolean, i.e. it
              sees:

              (v.blah === v)[] = v.blah

              20160107: turns out this applies to all(?) assignments
              on the RHS of a binary comparison or logical op.

              Oh, wait... JavaScript fails in the exact same way. :-D
              Now i remember commenting about this elsewhere long
              ago.
            */
            s2_ptoken next = s2_ptoken_empty;
            op = s2_ttype_op( pt.ttype = S2_T_ArrayAppend );
            s2_next_token( se, st,
                           /* this flag is only safe so long as the
                              up-coming logic using 'next' is
                              compatible. */
                           S2_NEXT_NO_POSTPROCESS,
                           &next );
            if((rc=s2_check_interrupted(se,rc))){
                /* next-token might have set
                   error state we which want to ignore...
                   except for an interruption. */;
              break;
            }
            if(s2_ttype_is_assignment(next.ttype)){
              s2_ptoker_token_set(st, &next) /* consume/skip it */;
              /* Fall through and push this op */
            }else{
              s2_ptoker_errtoken_set(st, &pt);
              rc = s2_err_ptoker(se, st, CWAL_SCR_SYNTAX,
                                 "Missing assignment op "
                                 "after '%s' operator.",
                                 op->sym);
              break;
            }
          }else if(S2_T_ParenGroup==pt.ttype
                   && !se->st.vals.size
                   && !se->st.ops.size
                   && (evalFlags & S2_EVAL_EMPTY_GROUP_OK)){
            /* We're doing this for the sake of return(). */
            evalFlags &= ~S2_EVAL_EMPTY_GROUP_OK;
            tVal = cwal_value_undefined();
          }else{
            /*
              We (generally) disallow empty () and [] groups because
              they would otherwise be ambiguous with (undefined). We
              do indeed special-case them in places.
            */
            rc = s2_err_ptoker(se, st, CWAL_SCR_SYNTAX,
                                 "Empty '%c%c' block is not "
                                 "allowed here.",
                                 braceOpen, braceClose);
            break;
          }
        }else{
          assert(!op);
          assert(tVal || se->skipLevel);
          pt.ttype = S2_T_Value;
          /* FALL THROUGH and be picked up as a VALUE via tVal */
          if(S2_T_BraceGroup==braceType){
            /*
              OBJ[KEY] is transformed to (OBJ DOT KEY)...
            */
            s2_stoken * topOpTok = s2_engine_push_ttype( se, S2_T_OpDot );
            if(!topOpTok){
              rc = CWAL_RC_OOM;
            }
            else topOpTok->srcPos = pt;
            /* MARKER(("Added DOT OP for [] group.\n")); */
          }
        }
        /*
          Hypothetical problem with _not_ having a '(' operator token in the
          stack:

          pending LHS ops _might_ not be evaluated in the proper order.

          Something to consider doing here:

          op = s2_ttype_op(S2_T_ParenOpen)

          Then, in the if(op) block below, recognize that particular
          token and do _NOT_ push it onto the stack. Instead, set op=0
          and jump into the is-a-Value block.

          Then again, parens are supposed to have amongst the highest
          precedence, so there can be no pending LHS operators to the
          left with with >= precedence, can there? Pending LHS groups,
          OTOH, which have == precedence have already been evaluated
          left-to-right, so the effect would seem to be the same.
        */
        break;
      }/*end parens/brace groups*/
      case S2_T_OpAssign:
      case S2_T_OpPlusAssign:
      case S2_T_OpMinusAssign:
      case S2_T_OpXOrAssign:
      case S2_T_OpAndAssign:
      case S2_T_OpOrAssign:
      case S2_T_OpMultiplyAssign:
      case S2_T_OpDivideAssign:
      case S2_T_OpModuloAssign:
      case S2_T_OpShiftLeftAssign:
      case S2_T_OpShiftRightAssign:
      case S2_T_OpColonEqual
        /* op := is a special case: only supports ternary, not binary */:{
        /**
           The binary assign works without us assisting it here, but in
           the case of a property assignment, we need to adjust the
           operator from the binary assignment (ident=expr) to the
           ternary assignment (obj DOT prop = expr), which we translate
           to (obj prop .= expr), where '.=' is the member assignment
           operator.

           Reminder to self: for compound assignments, e.g. +=

           we can translate (A+=B) to (A = A + B) and (A.B+=C)
           to (A.B = A.B+C). (But we don't do it that way.)

           Prefix/postfix incr/decr can be done similarly:
           --A ==> (var tmp=A; A = A-1; tmp)
           Except that we want overloaded ops to know if
           they're being called in unary form. Hmm.
        */
        s2_stoken * topOpTok = s2_engine_peek_op(se);
        if(prevOp /* && prevOp->arity>0 && prevOp->placement<=0 */
           /* How about postfix ++/--? Can we reasonably chain those
              with assignments?*/
           ){
          s2_ptoker_errtoken_set(st, &pt);
          rc = s2_err_ptoker(se, st, CWAL_SCR_SYNTAX,
                             "Invalid adjacent operators: "
                             "'%s' ==> '%s'", prevOp->sym,
                             op->sym);
        }

        if(topOpTok && s2_ttype_is_deref(topOpTok->ttype)){
          /*
            Assume the stack has (lhs,rhs) operands for the dot
            operator, then replace the dot op with a ternary
            assignment op: X.Y = Z.
          */
          int newOp = 0;
          switch(pt.ttype){
#define CASE(T) case T: newOp = T##3; break
            CASE(S2_T_OpAssign);
            CASE(S2_T_OpPlusAssign);
            CASE(S2_T_OpMinusAssign);
            CASE(S2_T_OpXOrAssign);
            CASE(S2_T_OpAndAssign);
            CASE(S2_T_OpOrAssign);
            CASE(S2_T_OpMultiplyAssign);
            CASE(S2_T_OpDivideAssign);
            CASE(S2_T_OpModuloAssign);
            CASE(S2_T_OpShiftLeftAssign);
            CASE(S2_T_OpShiftRightAssign);
#undef CASE
            case S2_T_OpColonEqual: newOp = S2_T_OpAssignConst3; break;
            default:
              assert(!"Missing operator mapping.");
              s2_fatal(CWAL_RC_FATAL, "Missing operator mapping: %s",
                       s2_ttype_cstr(pt.ttype));
          }
          assert(newOp);
          op = s2_ttype_op( pt.ttype = newOp );
          assert(op);
          s2_engine_pop_op(se,0)/*==topOpTok, a.k.a. OpDot */;
          /* Continue on and allow new op to be pushed to the stack */
        }else if(S2_T_OpColonEqual == pt.ttype){
          rc = s2_err_ptoker(se, st, CWAL_SCR_SYNTAX,
                             "The const-assign operator only works in "
                             "ternary form (X.Y:=Z).");
        }
        break;
      }/* /Assignment */
      case S2_T_LiteralInt:
      case S2_T_LiteralIntDec:
      case S2_T_LiteralIntHex:
      case S2_T_LiteralIntOct:
      case S2_T_LiteralIntBin:
      case S2_T_LiteralDouble:
      case S2_T_LiteralStringDQ:
      case S2_T_LiteralStringSQ:
      case S2_T_LiteralString:
      case S2_T_Heredoc:
      case S2_T_PropertyKey:
      case S2_T_Value:
      /* These will be picked up as Values below. */
        break;
      default:
          /*
            20171130: discovered by accident that a backtick character
            in a script was getting parsed as a string value (via
            s2_ptoken_create_value()). It was slipping through eval as
            a value token. s2_next_token() tags their ttype with their
            ASCII value, many of which overlap with various S2_T_OpXXX
            and such. It is only at this late point in the evaluation
            that we can really take note of them.
          */
        if(!op){
          rc = s2_err_ptoker(se, st, CWAL_SCR_SYNTAX,
                             "Unhandled token type %d (%s): %.*s",
                             pt.ttype, s2_ttype_cstr(pt.ttype),
                             (int)s2_ptoken_len(&pt), s2_ptoken_begin(&pt));
        }
        break;
    }/*switch(pt.ttype)*/

    /************************************************************/
    /**
       We're done with the handling of the token. We now either have a
       value (via tVal) or an operator (via op) to put onto the
       stack...
    */
    /************************************************************/
    if(rc || doBreak) break;
    else if(op){
      if(/* Script starts with an invalid operator.  "Invalid"
            basically means any which do not look like they can
            legally be used at the start of an expression. */
         op->arity>0
         && op->placement>=0
         /* && !fromLhsOp */
         && se->st.vals.size < op->arity-1 /*no values the op can work with*/
         ){
        rc = s2_err_ptoker( se, st, CWAL_SCR_SYNTAX,
                            "Illegal operator '%s' at start "
                            "of expression", op->sym);
      }else{
        rc = s2_eval_lhs_ops(se, st, op);
        if(!rc){
          s2_stoken * topOpTok = s2_engine_push_ttype(se, pt.ttype);
          if(!topOpTok){
            rc = CWAL_RC_OOM;
          }
          else{
            topOpTok->srcPos = pt;
            if(op->placement==0 && op->arity>1){
              /*
                Reminder: i do not really want EOL-skipping for
                prefix/postfix ops.  Prefix should cause a syntax
                error if they exist at EOL and postfix already has its
                operand on the stack.
              */
            }
          }
        }
      }
    }/* /if(op) */
    else{/* a Value token */
      if(!tVal){
        if(se->skipLevel>0) tVal = cwal_value_undefined();
        else{
          rc = s2_ptoken_create_value(se, st, &pt, &tVal);
          if(!rc){
            assert(tVal);
          }
        }
      }
      if(!rc){
        s2_stoken * vtok;
        assert(tVal);
        if((se->flags.traceTokenStack>0) && (S2_T_Identifier==pt.ttype)){
          MARKER(("Identifier token: %.*s\n", tlen, s2_ptoken_begin(&pt)));
        }
        vtok = s2_engine_push_tv(se, pt.ttype, tVal);
        if(!vtok){
          rc = CWAL_RC_OOM;
        }else{
          eval_hold(tVal);
          ++totalValCount;
          vtok->srcPos = pt;
          if(se->skipLevel>0){
#if 1
            if(cwal_value_undefined()!=tVal){
              s2_dump_val(tVal, "what is this???");
              assert(!"current internal convention for "
                     "skip-mode evaluation was violated");
              s2_fatal( CWAL_RC_ASSERT, "Internal convntion for "
                        "skip-mode evaluation was violated." );

            }
#endif
          }
        }
      }/* /if(!rc) */
    }/* /if op else value */
  }/* /foreach token */

  if(rc && !s2_ptoker_errtoken_has(st)){
    s2_ptoker_errtoken_set(st, (s2_ptoken_begin(&pt)
                                && (s2_ptoken_begin(&pt) != s2_ptoken_begin(&pt)))
                           ? &pt : &prevTok);
  }else if(!rc){
    assert(s2_ptoken_begin(&captureBegin));
  }
  if(!s2_ptoken_begin(&captureBegin)) captureBegin = st->token;
  if(!s2_ptoken_begin(&captureEnd)){
    captureEnd = st->token;
    /*
      20200110: Capturing historically elides the eox token, but
      before today we captured the expression's contents as a simple
      [begin,end) byte range. For EOX-eliding to work with the
      two-token capture approach, we have 2 options:

      1) We need to massage the capture end if (and only if) st->token
      is-a EOX.

      2) We define the capture end token as the one-after-the-end,
      rather than the end token.

      It turns out that 2 is easier to implement here, easier to deal
      with downstream, and ever-so-slightly more efficient (a handful
      fewer CPU instructions).

      When capturing a completely empty expression with an EOX,
      e.g. ";", captureBegin will unavoidably point to that semicolon,
      which goes against our age-old policy of *not* including the
      semicolon in the capture, but captureEnd will *also* point to
      that token, so well-behaved code will not include the ';' in any
      handling.
    */
  }
  assert(s2_ptoken_begin(&captureEnd) >= s2_ptoken_begin(&captureBegin)
         && s2_ptoken_end(&captureEnd) >= s2_ptoken_end(&captureBegin));
  st->capture.begin = captureBegin;
  st->capture.end = captureEnd;
  /*MARKER(("captured: <<<%.*s>>>\n", (int)(captureEnd.begin - captureBegin.begin),
    captureBegin.begin));*/
  /* MARKER(("Stack sizes: op=%d, token=%d\n", se->st.ops.size, se->st.vals.size)); */
  if(rc) goto end;

  /*
    Now process what's left of the stack...
  */
  if(ownStack){
    while(!rc && se->st.ops.size){
      rc = s2_process_top(se);
    }
    if(rc){
      rc = s2_ammend_op_err(se, st, rc);
      assert(rc);
      goto end;
    }
  }
  /* MARKER(("Stack sizes: op=%d, token=%d\n", se->st.ops.size, se->st.vals.size)); */
  assert(!rc);
  if(ownStack && (1 != se->st.vals.size)){ /* too many or too few items in stack */
    if(!se->st.vals.size
       && s2_ttype_is_eox(st->token.ttype)
       /* ==> empty expression */
       ){
      /*MARKER(("Truly empty expression? totalValCount=%d %.*s\n",
        totalValCount, (int)(capEnd - capBegin), capBegin));*/
#if 0
      if(!totalValCount){
        /* A truly empty expression with no expected value part
           before the EOX. */
        /* if(evalIt) */xrv = 0;
      } 
#endif
      /*else{
          
          Leave previous expression value, if any, because we
          currently need it for:

          X; ==> X

          but it also means that (X;;; ==> X), which we really
          don't want. Catching that has to be done from the code
          calling this - it needs to remember seeing (or not)
          multiple EOX tokens.
        }*/
    }else if((se->st.vals.size > 0) && totalValCount){
      s2_ptoker_errtoken_set(st, &pt);
      rc = s2_err_ptoker(se, st, CWAL_SCR_SYNTAX,
                         "Unexpected stack size "
                         "(%d) after expression. "
                         "Normally caused by a missing ';' "
                         "in the previous line.",
                         se->st.vals.size);
    }
  }/* /stack size error checking. */

  if(!rc){
    cwal_value * xrv = s2_engine_pop_value(se);
    /* assert(xrv); */
    if(se->skipLevel>0){
#if 1
      /* s2_dump_val(xrv, "what is this"); */
      assert((!xrv || (xrv == cwal_value_undefined()))
             && "current internal convention for "
             "skip-mode evaluation was violated");
#endif
    }
    if(se->flags.traceTokenStack>0 && !se->skipLevel){
      s2_dump_val(xrv, "eval_expr result");
    }
    if(rv) *rv = xrv;
    else cwal_refunref(xrv);
  }

  end:
#undef eval_hold
#undef eval_hold_this_value
  if(rc){
    rc= 1 ? rc : 0 /* put breakpoint here (the ?: is to avoid
                      assignment-to-self warning from clang).*/;
  }
  /* Clean up... */

  if(rc && !s2__err(se).code && st->errMsg
     && (CWAL_RC_EXCEPTION!=rc)){
    /* Error from the tokenizer. */
    rc = s2_err_ptoker(se, st, rc, 0);
  }

  rc = s2_rv_maybe_accept(se, scope.parent /* 0 is okay */, rc, rv);

#if EVAL_USE_HOLDER
  if(holder){
    assert(!se->skipLevel);
    /* --se->sguard->vacuum; */
    assert(1 == cwal_value_refcount(cwal_array_value(holder)));
    if(!rc && rv) cwal_value_ref(*rv);
    cwal_array_length_set( holder, holderLen )
      /* truncate to its old length, potentially freeing any refs this
         eval() added */;
    holder = 0;
    if(!rc && rv) cwal_value_unhand(*rv);
  }
#endif
/* ^^^^ EVAL_USE_HOLDER */

  if(/* !fromLhsOp && */ !(S2_EVAL_RETAIN_DOTOP_STATE & evalFlags)){
    /* We must clear these to avoid picking them up after they're
       stale.  However, they's needed by, e.g. (unset x.y).

       Reminder: this ref/unhand of *rv is critical to avoid a crash
       caused by too many unrefs here:

       scope { {## a: 3}.a }

       where the temporary LHS hash is in se->dotOp.self and and the
       result value of the expression will get destroyed by it when
       s2_dotop_state() resets it. Thus we need to ref it. Right here:
    */
    if(rv && *rv) cwal_value_ref(*rv);
    s2_dotop_state( se, 0, 0, 0 );
    if(rv && *rv) cwal_value_unhand(*rv);
  }
  if(!evalIt){
    assert(se->skipLevel==1+oldSkipLevel);
    se->skipLevel = oldSkipLevel;
  }
  if(ownStack){
    s2_engine_stack_swap(se, &priorStack);
    s2_estack_clear(se, &priorStack, 1);
#ifdef DEBUG
    assert(oldOpCount == se->st.ops.size);
    assert(oldValCount == se->st.vals.size);
#endif
  }
  if(!consumeIt){
    s2_ptoker_token_set( st, &pbOrigin );
    s2_ptoker_putback_set(st, &pbOrigin);
  }else{
    s2_ptoker_putback_set(st, &pOrigin)
      /* yes, pOrigin (not pbOrigin) - we want to be able to putback
         the whole expression */;
  }
  --se->metrics.subexpDepth;

  se->currentScript = oldScript;
  if(S2_EVAL_PUSH_SCOPE & evalFlags){
    assert(scope.parent);
    assert(s2_scope_current(se)->cwalScope==&scope);
    cwal_scope_pop2(se->e, rc ? 0 : (rv ? *rv : 0));
  }else{
    if(!scope.parent){
      --se->scopes.current->sguard.sweep;
    }
  }
  return rc;
}

#undef EVAL_USE_HOLDER

int s2_eval_buffer( s2_engine * se,
                    char newScope,
                    char const * name,
                    cwal_buffer const * buf,
                    cwal_value **rv ){
  return s2_eval_cstr( se, newScope, name,
                       buf->used ? (char const *)buf->mem : "",
                       (int)buf->used,
                       rv );
}

int s2_eval_cstr( s2_engine * se,
                  char newScope,
                  char const * name,
                  char const * src, int srcLen,
                  cwal_value **rv ){
  s2_ptoker pt = s2_ptoker_empty;
  int rc = s2_ptoker_init_v2( se->e, &pt, src, srcLen, 0 );
  if(!rc){
    cwal_scope SC = cwal_scope_empty;
    if(!newScope || !(rc=cwal_scope_push2(se->e, &SC))){
      cwal_value * xrv = 0;
      pt.name = (name && *name) ? name : 0;
      rc = s2_eval_ptoker( se, &pt, 0/*TODO: flags*/, rv ? &xrv : 0 );
      if(!rc && rv) *rv = xrv;
      else cwal_refunref(xrv);
      if(SC.parent){
        cwal_scope_pop2(se->e, rc ? 0 : (rv ? *rv : 0));
      }
    }
  }
  s2_ptoker_finalize( &pt );
  return rc;
}

int s2_eval_cstr_with_var( s2_engine * se,
                           char const * varName,
                           cwal_value * varValue,
                           char const * scriptName,
                           char const * src, int srcLen,
                           cwal_value **rv ){
  int rc;
  cwal_scope sc = cwal_scope_empty;
  rc = cwal_scope_push2(se->e, &sc);
  if(rc) return rc;
  cwal_value_ref(varValue);
  rc = cwal_var_decl(se->e, 0, varName, cwal_strlen(varName), varValue, 0);
  if(!rc){
    rc = s2_eval_cstr(se, 0, scriptName, src, srcLen, rv );
  }
  cwal_scope_pop2(se->e, rc ? (rv ? *rv : 0) : 0);
  cwal_value_unhand(varValue);
  return rc;
}


int s2_eval_ptoker( s2_engine * se, s2_ptoker * pr, int e2Flags, cwal_value **rv ){
  int const oldTStackSize = se->st.vals.size;
  cwal_value * xrv = 0 /* pending result value */;
  int hardEoxCount = 0;
  int const srcLen = (int)(s2_ptoker_end(pr) - s2_ptoker_begin(pr));
  s2_ptoker const * oldScript = se->currentScript;
  char xrvWasVacSafe = 0 /* whether not not xrv was vacuum-safe (if not, we need to make it so). */;
  int rc = s2_check_interrupted(se,0);
  if(rc) return rc;
  else if(srcLen<0){
    return s2_engine_err_set(se, CWAL_RC_MISUSE, "Invalid s2_ptoker range.");
  }else if(!srcLen || !*s2_ptoker_begin(pr)){
    if(rv) *rv = 0;
    return 0;
  }

  /*
    We have one notable problem in the sweepup mechanism with regard
    to iterating over expressions and keeping the most recent result:
    vacuuming can steal the result value from us. So we either have to
    make the value itself vacuum-proof (which doesn't work for PODs
    and would have unwanted side-effects in some cases) or we can
    disallow vacuuming so long as we have a pending expression.
    Bummer.

    As of 20160228, cwal can make non-container instances
    vacuum-proof, so we use that to make the pending result safe from
    vacuuming (and a ref point to keep it safe from sweep-up) while
    not unduly extending its lifetime because (A) each new result
    overwrites it, potentially cleaning it up immediately, and (B) at
    the end of the block we set the references straight (in a kosher
    manner) for the return via *rv.
  */
  s2_engine_err_reset(se);
  se->currentScript = pr;
#if 0
  if(pr->name){
    MARKER(("Running script [%s]\n", pr->name))
      /* for helping in finding a script name bug in interactive
         mode. */
      ;
  }
#endif
  for( ; !rc ; ){
    int isEof, isEox;
    cwal_value * vrx = 0;
    s2_engine_sweep(se);

    rc = s2_eval_expr_impl(se, pr, 0, 0, rv ? &vrx : 0);
    if(rc) break;
#if 0
    MARKER(("Ran expr: value=%p, capture=[[[%.*s]]]\n",
            (void const *)vrx, 
            (int)(s2_ptoken_begin(&pr->capture.end)
                  - s2_ptoken_begin(&pr->capture.begin)),
            s2_ptoken_begin(&pr->capture.begin)));
#endif
    isEof = s2_ptoker_is_eof(pr);
    isEox = isEof ? isEof : s2_ttype_is_eox(pr->token.ttype);
    if(vrx){
      hardEoxCount = 0 /* tells us (later) to keep xrv at the next EOX. */;
      assert(rv && "Else all of this is a waste of time...");
      /* Swap out pending result... */
      cwal_value_ref(vrx);
      if(xrv){
        if(!xrvWasVacSafe){
          assert(cwal_value_is_vacuum_proof(xrv));
          cwal_value_make_vacuum_proof(xrv, 0);
        }
        assert(cwal_value_refcount(xrv) || cwal_value_is_builtin(xrv) /* will fail on too many unrefs */);
        cwal_value_unref(xrv);
      }
      /* Make vrx the new pending result... */
      xrv = vrx;
      vrx = 0;
      xrvWasVacSafe = xrv ? cwal_value_is_vacuum_proof(xrv) : 0;
      if(xrv){ /* formerly known as vrx */
        assert(cwal_value_refcount(xrv) || cwal_value_is_builtin(xrv) /* we ref'd it above */);
        if(!xrvWasVacSafe) cwal_value_make_vacuum_proof(xrv, 1);
        /* cwal_unique_wrapped_set(holder, xrv); */
        /* s2_dump_val(xrv, "s2_eval_expr result"); */
      }
    }
    if(!isEof && isEox){
      /*
        Reminder; "3 ;" ends in an EOX but has a value. It evals
        differently than "3 ; ;", which evals to two expressions, the
        second one empty.

        We accept one ';' as "keep previous value", but a series of
        semicolons, possibly separated by other empty expressions,
        evals to NULL. We pass this NULL back to the caller so that
        they can differentiate between empty expression [as the final
        result] and the undefined value (which would be the
        alternative result). Splitting hairs, either way, in this
        context (but the distinction is important in/via
        s2_eval_expr_impl()).
      */
      if(rv && ++hardEoxCount>1 && xrv){
        assert(cwal_value_refcount(xrv) || cwal_value_is_builtin(xrv) /* we ref'd it above */);
        assert(cwal_value_is_vacuum_proof(xrv));
        if(!xrvWasVacSafe) cwal_value_make_vacuum_proof(xrv, 0);
        cwal_value_unref(xrv);
        xrv = 0;
      }
    }else if(!isEox){
      hardEoxCount = 0;
    }
    if(isEof) break;
  }/* for-each expression */
  /* MARKER(("Stack sizes: op=%d, token=%d\n", se->pr->ops.size, se->pr->vals.size)); */
  if(!rc){
    if(oldTStackSize != se->st.vals.size){
      /*MARKER(("Unexpected stack sizes: op=%d, token=%d\n",
        se->pr->ops.size, se->pr->vals.size));*/
      rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                         "Unexpected stack size in s2_eval_ptoker(): %d",
                         se->st.vals.size);
    }else{
      rc = s2_check_interrupted(se, rc);
    }
  }

  if(xrv && !xrvWasVacSafe){
    assert(cwal_value_is_vacuum_proof(xrv));
    cwal_value_make_vacuum_proof(xrv, 0);
  }
  /* s2_dump_val(xrv, "s2_eval_ptoker result"); */
  /* s2_dump_val(xrv, "s2_eval_ptoker result"); */
  if(!pr->parent
     && CWAL_RC_RETURN==rc
     && !(e2Flags & S2_EVALP_F_PROPAGATE_RETURN)){
    /* Top-level parser shall accept a RETURN as a non-error, and
       stuff it in *rv. Unless e2Flags has the S2_EVALP_F_PROPAGATE_RETURN
       bit set.

       We do not handle EXIT here because this routine will be used to
       import files from other files, without having the parent
       sources as pr->parent, and exit() needs to work cross-file
       whereas we can justify RETURN stopping at a file boundary
       (PHP-style).
    */
    cwal_value * rr = s2_propagating_take(se);
    assert(rr);
    cwal_value_ref(rr);
    cwal_value_unref(xrv);
    cwal_value_unhand(rr);
    xrv = rr;
    rc = 0;
    /* s2_engine_err_reset(se); */
  }else if(xrv){
    assert(rv);
    if(!rc) cwal_value_unhand(xrv)
              /* we pass xrv on down below */;
    else{
      cwal_value_unref(xrv);
      xrv = 0;
    }
  }
  
  if(!rc && rv){
    *rv  = xrv;
    assert((!xrv || cwal_value_scope(xrv) || cwal_value_is_builtin(xrv))
           && "Seems like we cleaned up xrv too early.");
    if(se->flags.traceTokenStack){
      s2_dump_val(*rv, "s2_eval_ptoker result");
    }
  }
  se->currentScript = oldScript;
  return rc;
}

int s2_keyword_f_breakpoint( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv){
  int rc;
  S2_UNUSED_ARG kw;
  S2_UNUSED_ARG se;
  S2_UNUSED_ARG pr;
  *rv = cwal_value_undefined();
  rc = 0 /* place breakpoint here */;
  /**
     TODO:

     add an interactive breakpoint callback hook, which the client can
     optionally start running when the hook is called. e.g. s2sh might
     (only in interactive mode) switch (back) to interactive mode in
     the callback, returning here when it's done. We'd need to install
     some convenience symbols here:

     __SCOPE = vars
  */
  return rc;
}

int s2_keyword_f_builtin_vals( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv){
  if(se->skipLevel){
    *rv = cwal_value_undefined();
    return 0;
  }else{
    switch(pr->token.ttype){
      case S2_T_KeywordUndefined: *rv = cwal_value_undefined(); return 0;
      case S2_T_KeywordNull: *rv = cwal_value_null(); return 0;
      case S2_T_KeywordTrue: *rv = cwal_value_true(); return 0;
      case S2_T_KeywordFalse: *rv = cwal_value_false(); return 0;
      default:
        assert(!"Invalid keyword mapping");
        S2_UNUSED_ARG kw;
        s2_fatal(CWAL_RC_RANGE, "Invalid keyword mapping in s2_keyword_f_builtin_vals()")
          /* does not return, but your compiler doesn't know that, so... */;
        return CWAL_RC_ERROR;
    }
  }
}

int s2_keyword_f_FLC( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv){
  s2_linecol_t line = 0, col = 0;
  char const * script = 0;
  cwal_size_t scriptLen = 0;
  if(se->skipLevel>0){
    *rv = cwal_value_undefined();
    return 0;
  }
  /* s2_ptoker_err_info( pr, &script, s2_ptoken_begin(&pr->token), &line, &col ); */
  switch(kw->id){
    case S2_T_KeywordFILE:
      script = s2_ptoker_name_first(pr, &scriptLen);
      *rv = script
        ? cwal_new_string_value(se->e, script, scriptLen)
        : cwal_value_undefined();
      break;
    case S2_T_KeywordFILEDIR:{
      char const * sepPos;
      cwal_size_t len;
      script = s2_ptoker_name_first(pr, &scriptLen);
      sepPos = scriptLen ? s2_last_path_sep(script, scriptLen) : 0;
      len = sepPos ? (cwal_size_t)(sepPos - script) : 0;
      assert(len <= scriptLen);
      *rv = len
        ? cwal_new_string_value(se->e, script, len)
        : (sepPos /* root dir */
           ? cwal_new_string_value(se->e, sepPos, 1)
           : cwal_new_string_value(se->e, 0, 0));
      break;
    }
    case S2_T_KeywordLINE:
      s2_ptoker_count_lines( pr, s2_ptoken_begin(&pr->token), &line, 0);
      *rv = cwal_new_integer(se->e, (cwal_int_t)line);
      break;
    case S2_T_KeywordCOLUMN:
      s2_ptoker_count_lines( pr, s2_ptoken_begin(&pr->token), 0, &col);
      *rv = cwal_new_integer(se->e, (cwal_int_t)col);
      break;
    case S2_T_KeywordSRCPOS:
      s2_ptoker_count_lines( pr, s2_ptoken_begin(&pr->token), &line, &col);
      script = s2_ptoker_name_first(pr, &scriptLen);
      *rv = (line>0)
        ? cwal_string_value(script
                            ? cwal_new_stringf(se->e, "%.*s:%d:%d",
                                               (int)scriptLen, script,
                                               line, col)
                            : cwal_new_stringf(se->e, "unnamed script:%d:%d",
                                               line, col)
                            )
        : cwal_value_undefined();
      break;
    default:
      assert(!"Invalid operator mapping");
      return CWAL_RC_ERROR;
  }
  return *rv ? 0 : CWAL_RC_OOM;
}


int s2_keyword_f_exception( s2_keyword const * kw, s2_engine * se,
                            s2_ptoker * pr, cwal_value **rv){
  int rc;
  cwal_array * args = 0;
  cwal_value * argsV = 0;
  cwal_size_t argc = 0;
  s2_ptoker sub = s2_ptoker_empty;
  s2_ptoken const origin = pr->token;
  rc = s2_next_token( se, pr, 0, 0 );
  if(rc) return rc;
  else if(pr->token.ttype != S2_T_ParenGroup){
    /* 20191228: if the exception keyword is followed by anything
       other than a'(', it resolves to the exception prototype. */
    s2_ptoker_putback(pr);
    *rv = se->skipLevel
      ? cwal_value_undefined()
      : cwal_prototype_base_get( se->e, CWAL_TYPE_EXCEPTION );
    return 0;
  }
  /*MARKER(("exception: %.*s\n",
    (int)s2_ptoken_len(&pr->token), s2_ptoken_begin(&pr->token))); */
  *rv = cwal_value_undefined();
  if(rc || se->skipLevel>0) goto end;

  args = cwal_new_array(se->e);
  if(!args){
    rc = CWAL_RC_OOM;
    goto end;
  }
  argsV = cwal_array_value(args);
  cwal_value_ref(argsV);
  cwal_value_make_vacuum_proof(argsV, 1);

  rc = s2_ptoker_sub_from_toker(pr, &sub);
  if(!rc) rc = s2_eval_to_array(se, &sub, args, 0);
  s2_ptoker_finalize(&sub);
  if(rc) goto end;
  argc = cwal_array_length_get(args);
  /* s2_dump_val(argsV,"argsV"); */
  if(argc==1 || argc==2){
    /* 
       (ARG) is equivalent to what we get with (throw ARG), namely
       that the 'message' property === ARG.

       (ARG1, ARG2) sets the 'code' property to ARG1 and the
       'message' property to ARG2.
    */
    int exceptionCode = CWAL_RC_EXCEPTION;
    if(argc>1){
      /* Try to parse result code from exception(CODE,message)...
         Accept an integer or string in the form "CWAL_RC_name"
         resp. "S2_RC_name".
      */
      cwal_value * arg0 = cwal_array_get(args, 0);
      cwal_value *hv = s2_stash_get(se, "RcHash")
        /* Optimization: if the stashed RcHash (set up in s2.c) is
           available, check it first. This avoids having to allocate
           x-strings which we know are already in that hash. It also
           incidentally supports a reverse mapping, such that passing in
           the string 'CWAL_RC_OOM' will return its integer value.
        */;
      *rv = 0;
      if(hv){
        cwal_hash * h = cwal_value_get_hash(hv);
        assert(h);
        *rv = cwal_hash_search_v(h, arg0);
        if(*rv){
          if(!cwal_value_is_integer(*rv)){
            *rv = cwal_hash_search_v(h, *rv) /* reverse mapping */;
            assert(*rv);
            assert(cwal_value_is_integer(*rv));
          }
          exceptionCode = (int)cwal_value_get_integer(*rv);
        }
      }
      if(!*rv){
        /* Try argument as a string (enum entry name) or integer (enum
           entry value)... */
        cwal_size_t nameLen = 0;
        char const * codeName = cwal_value_get_cstr(arg0, &nameLen);
        if(codeName && s2_cstr_to_rc(codeName, (cwal_int_t)nameLen, &exceptionCode)){
          if(! (*rv = cwal_new_integer(se->e, (cwal_int_t)exceptionCode)) ){
            rc = CWAL_RC_OOM;
          }
        }else{
          exceptionCode = (int)cwal_value_get_integer(arg0);
        }
      }
      *rv = 0;
    }
    switch(exceptionCode){
      case 0:
      case CWAL_RC_OOM: /* don't allow OOM passed in here to crash the script engine */
        exceptionCode = CWAL_RC_EXCEPTION;
        break;
    }
    s2_ptoker_errtoken_set(pr, &origin);
    rc = s2_throw_value( se, pr, exceptionCode,
                         cwal_array_get(args, argc==1 ? 0 : 1));
    if(CWAL_RC_EXCEPTION==rc){
      /* Success! Now take away the newly-populated exception. */
      cwal_value * exv = cwal_exception_get(se->e);
      assert(exv);
      /* s2_dump_val(exv,"exv"); */
      assert(cwal_value_exception_part(se->e,exv));
      cwal_value_ref(exv);
      cwal_exception_set(se->e, 0);
      cwal_value_unhand(exv);
      s2_engine_err_reset(se);
      *rv = exv;
      rc = 0;
    }
  }else{
    rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                       "%s(...) requires (Message) or "
                       "(int|string Code, Message) arguments.",
                       kw->word);
  }

  end:
  if(argsV){
    cwal_value_make_vacuum_proof(argsV, 0);
    cwal_value_unref(argsV);
  }
  return rc;
}

int s2_keyword_f_eval( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv){
  int rc = 0;
  cwal_scope scope_ = cwal_scope_empty;
  cwal_scope * scope = 0 /* gets set if we push a scope */;
  cwal_value * xrv = 0 /* internal result value */;
  int sourceType = 0 /* logical token type of the part after the keyword */;
  int modifier = 0 /* set if the -> or => modifier is specified */;
  int phase = 0 /* evaluation phase: 1st or 2nd (where we eval string content fetched in phase 1) */;
  s2_ptoken const origin = pr->token;
  s2_ptoken exprPos = s2_ptoken_empty;
  int evalFlags = 0;
  int scopeFlags = S2_SCOPE_F_NONE;
  int isBlock = 0;
  switch(kw->id){
    /* These push a scope... */
    case S2_T_KeywordCatch:
    case S2_T_KeywordScope:
      if(!se->skipLevel) scope = &scope_;
      break;
    /* These do not push a scope, or do so at their own level... */
    case S2_T_KeywordEval:
    case S2_T_KeywordFatal:
    case S2_T_KeywordThrow:
      break;
    case S2_T_KeywordExit:
    case S2_T_KeywordBreak:
    case S2_T_KeywordReturn:
#if 0
      /* i don't like this inconsistency */
      evalFlags = S2_EVAL_EMPTY_GROUP_OK;
#endif
      break;
    /* And these are just plain wrong... */
    default:
      assert(!"Invalid keyword mapping!");
      return s2_err_ptoker(se, pr, CWAL_RC_FATAL,
                           "Invalid keyword mapping.");
  }

  /*
    Check if the following expression is a {squiggly} or not,
    or whether it uses one of the -> or => modifiers...
  */
  {
    s2_ptoker next = *pr;
    next_token: /* modifiers cause us to goto here */
    rc = s2_next_token(se, &next, 0, 0);
    if(rc) return rc;
    else{
      switch(next.token.ttype){
        case S2_T_SquigglyBlock:
          sourceType = next.token.ttype;
          s2_ptoker_token_set(pr, &next.token)/*consume it*/;
          break;
        case S2_T_OpArrow2:
          switch(kw->id){
            case S2_T_KeywordBreak:
            case S2_T_KeywordEval:
            case S2_T_KeywordExit:
            case S2_T_KeywordFatal:
            case S2_T_KeywordReturn:
              break;
            default:
              return s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                                   "The => modifier can only be used "
                                   "with these keywords: "
                                   "break, eval, exit, fatal, return");
          }
          CWAL_SWITCH_FALL_THROUGH;
        case S2_T_OpArrow:
          if(!modifier){
            modifier = next.token.ttype;
            s2_ptoker_token_set(pr, &next.token) /* consume it */;
            goto next_token;
          }else{
            return s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                                 "Unexpected extra modifier token '%.*s'.",
                                 (int)s2_ptoken_len(&next.token),
                                 s2_ptoken_begin(&next.token));
          }
          break;
        default:
          s2_ptoker_next_token_set(pr, &next.token)
            /* hits fairly often: just over 400 out of 10457
               next-token requests in the current UNIT.s2
               amalgamation. */;
          break;
      }
    }
  }

  isBlock = (sourceType == S2_T_SquigglyBlock) ? 1 : 0
    /* treat heredocs as strings for eval purposes */;
  if(!isBlock){
    /*
      Consider: (1 ? catch 2 : 3)

      Without this flag, the ':' after 'catch 2' is seen as a
      standalone token with no '?'  counterpart (causing a syntax
      error). Thus we have to set a flag to tell downstream code that
      the ':' ends that (sub)expression.
    */
    scopeFlags = S2_SCOPE_F_KEEP_TERNARY_LEVEL;
  }

  if(scope){
    rc = s2_scope_push_with_flags(se, scope, scopeFlags);
    if(rc) return rc;
    assert(scope->parent);
  }

  exprPos = pr->token;
  /* As of here, don't use 'return': use goto end. */

  if(isBlock){
    if(se->skipLevel>0){
      /* Skip it! */
      goto end;
    }
    else if(S2_T_OpArrow2 == modifier){
      /* eval=>{block} result = the block body as a string. */
      char const * beg;
      cwal_size_t len;
      s2_ptoken const * block = &pr->token;
      assert(S2_T_SquigglyBlock==block->ttype);
      assert(s2_ptoken_adjbegin(block));
      assert(s2_ptoken_adjend(block) &&
             s2_ptoken_end(block) >= s2_ptoken_begin(block)+2);
      /* Should we keep leading/trailing spaces or not? */
#if 0
      /* Keep spaces... */
      beg = s2_ptoken_begin(block) + 1 /* skip opening '{' */;
      len = (cwal_size_t)s2_ptoken_len(block)-2
        /* skip trailing '}' */;
#else
      /* Strip spaces... */
      beg = s2_ptoken_cstr2( block, &len );
#endif
      xrv = cwal_new_string_value(se->e, beg, len);
      if(!xrv) rc = CWAL_RC_OOM;
      else cwal_value_ref(xrv);
    }
    else{
      /* Eval the block... */
      switch(kw->id){
        case S2_T_KeywordBreak:
        case S2_T_KeywordExit:
        case S2_T_KeywordFatal:
        case S2_T_KeywordReturn:
        case S2_T_KeywordThrow:
          /* $keyword {} treats {} as an Object. This is incidentally
             aligns with $keyword [...] seeing an array literal.  We
             eval it as a normal expression, as opposed to an Object
             literal, to pick up object literals and any following
             operators. */
          s2_ptoker_next_token_set(pr, &pr->token) /* avoids re-tokenization effort... */;
          /* MARKER(("pr->nextToken = pr->token\n")); */
          rc = s2_eval_expr_impl(se, pr, 0, 0, &xrv);
          /*
            This leads to a syntactic inconsistency between
            eval/scope/catch and the other variants:

            eval {1},2; // two separate expressions: (eval==>1), literal 2
            return {a:1},2; // one expresion (object, 2) ==> 2
          */
          cwal_value_ref(xrv);
          break;
        default:
          /* Treat {} as a script block */
          rc = s2_eval_current_token( se, pr, 0, 0, &xrv );
          cwal_value_ref(xrv);
          break;
      }
      /* s2_dump_val(xrv, "EVAL result"); */
      if(!rc && !xrv) xrv = cwal_value_undefined();
    }
  }else{
    /* Not a block construct. Behave mostly as if the keyword wasn't
       there (except that we ignore LHS operators). */
    s2_op const * pseudoOp =
#if 1
      0
#else
      (sourceType == S2_T_Heredoc)
      ? s2_ttype_op(S2_T_Comma)
      : 0
      /* So that we stop eval'ing the RHS at a comma or higher prec,
         AND because the special RHSEval.

         But doing that breaks:

         return 1, 2, 3;

         And fixing that causes some broken (wrong script name)
         error messages in code broken by this.

         Consider:

         eval {1+2}, 3

         because of the {block}, that "probably really" should be
         (1+2), 3, as this is the only context where we allow a
         {block} like this.
      */
#endif
      ;
    char const isArrow2 = (S2_T_OpArrow2 == modifier) ? 1 : 0;
    /* MARKER(("%s'ing around %.10s ...\n", kw->word,
       s2_ptoken_begin(&exprPos))); */

    if(isArrow2){
        ++se->skipLevel
            /* so that we capture the full text of the expression
               without "really" executing it. */;
    }
    rc = s2_eval_expr_impl( se, pr, pseudoOp, evalFlags, &xrv );
    cwal_value_ref(xrv);
    if(isArrow2) --se->skipLevel;
    if(isArrow2 && !rc){
      /* Capture the expression's text as a string. For consistency
         with other eval contexts, treat an empty expression (as
         opposed to an empty script string) as an error. */
      assert(0==xrv || cwal_value_undefined()==xrv);
      /* Capture the text of the expression as the result. */
      if(se->skipLevel){
        xrv = cwal_value_undefined();
      }else{
        cwal_size_t capLen = 0;
        char const * cap = s2_ptoker_capture_cstr(pr, &capLen);
        assert(cap);
        /*MARKER(("OpArrow2 capture: <<<%.*s>>>\n",(int)capLen, cap));*/
        while(capLen>0 && s2_is_space(*cap)){
          /* Skip leading spaces for this specific case because
             "it just looks funny" otherwise. */
          /* Leading space hypothetically cannot happen with the
             two-token capture approach, but better safe than sorry
             (it's conceivable, but seems unlikely, that a newline
             could slip through this way). */
          ++cap;
          --capLen;
        }
        while(capLen>0 && s2_is_space(cap[capLen-1])){
          /* Skip trailing spaces for consistency with the {block} form.*/
          --capLen;
        }
        if(!capLen){
          rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                             "Empty expression is not allowed here.");
          goto end;
        }
        assert(capLen);
        xrv = cwal_new_string_value(se->e, cap, capLen);
        if(!xrv) rc = CWAL_RC_OOM;
        else cwal_value_ref(xrv);
      }
    }
    /* s2_dump_val(xrv, "EVAL result"); */
    if(!rc && !xrv){
      assert(!isArrow2);
      switch(kw->id){
        /* These are allowed to have empty expressions... */
        case S2_T_KeywordReturn:
        case S2_T_KeywordExit:
        case S2_T_KeywordBreak:
        case S2_T_KeywordFatal:
          xrv = cwal_value_undefined();
          break;
        /* The rest are not... */
        default:{
          char const * errMsg;
          switch(kw->id){
            case S2_T_KeywordThrow:
              errMsg = "%s requires a non-empty expression operand.";
              break;
            default:
              errMsg = "%s requires a non-empty expression or {script} operand.";
          }
          s2_ptoker_errtoken_set(pr, &origin);
          rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                             errMsg, kw->word);
        }
      }
    }
  }
  ++phase;

  if(!rc
     && !se->skipLevel
     && S2_T_OpArrow == modifier /* got -> modifier */
     && cwal_value_is_string(xrv)
     /* ^^^ We don't want this upcoming cwal_value_get_cstr() to get
        source code from a Buffer because (partly) of potentially
        fatal corner-cases if that Buffer is modified during/via the
        eval. */
     ){
    /* second-pass evaluation: eval -> expr */
    cwal_size_t vlen = 0;
    char const * vstr = cwal_value_get_cstr(xrv, &vlen);
    if(vstr){
      s2_ptoker sub = s2_ptoker_empty;
      assert(cwal_value_refcount(xrv) || cwal_value_is_builtin(xrv));
      rc = s2_ptoker_init_v2( se->e, &sub, vstr, (int)vlen, 0 );
      if(!rc){
        /* We have to ref and vacuum-guard to keep this follow-up eval
           from vacuuming xrv out from under us. Thanks again,
           valgrind. We also have to ref xrv (thanks, cwal
           assertions).
        */
        cwal_value * xrv2 = 0;
        ++se->scopes.current->sguard.vacuum;
        s2_ptoker_count_lines( pr, s2_ptoken_end(&exprPos),
                               &sub.lineOffset,
                               &sub.colOffset );
        sub.name = s2_ptoker_name_first(pr, 0)
          /* Needed when functions are created in this code. It kinda
             (well... totally) confuses the __FLC-related bits,
             though, as well as exceptions and parse errors.
          */;
        /* sub.parent = pr; wrong: sub is dynamic text */
        rc = s2_eval_ptoker(se, &sub,
                            S2_EVALP_F_PROPAGATE_RETURN,
                            &xrv2)
          /* ..._RETURN flag needed so that (eval -> 'return 1')
             behaves just like (eval -> return 1) does.*/
          ;
        --se->scopes.current->sguard.vacuum;
        cwal_value_ref(xrv2);
        cwal_value_unref(xrv);
        xrv = xrv2;
      }
      s2_ptoker_finalize(&sub);
      ++phase;
    }
  }/* end second-pass eval */
  end:
  if(se->skipLevel>0){
    if(xrv){
      assert(cwal_value_undefined() == xrv);
    }else{
      xrv = cwal_value_undefined();
    }
  }else{
    switch(kw->id){
      case S2_T_KeywordBreak:
      case S2_T_KeywordExit:
      case S2_T_KeywordFatal:
      case S2_T_KeywordReturn:
        if(!rc){
          if(xrv){
            assert(cwal_value_refcount(xrv) || cwal_value_is_builtin(xrv));
          }else{
            xrv = cwal_value_undefined();
          }
          s2_propagating_set(se, xrv);
          cwal_value_unref(xrv) /* ^^^ took a reference */;
          xrv = 0; 
          switch(kw->id){
            case S2_T_KeywordExit: rc = CWAL_RC_EXIT; break;
            case S2_T_KeywordFatal: rc = CWAL_RC_FATAL; break;
            case S2_T_KeywordReturn: rc = CWAL_RC_RETURN; break;
            case S2_T_KeywordBreak: rc = CWAL_RC_BREAK; break;
            default:
              s2_fatal(CWAL_RC_ASSERT,
                       "Cannot happen: invalid nested switch() values.");
          }
          /*
            But we need to keep the error location...
          */
          {
            s2_ptoken err = origin;
            s2_ptoken_begin_set(&err, s2_ptoken_end(&origin));
            s2_ptoken_end_set(&err, s2_ptoken_end(&err));
            s2_ptoker_errtoken_set(pr, &err);
            rc = s2_err_ptoker(se, pr, rc, 0);
          }
        }
        break;
      case S2_T_KeywordThrow:
        if(!rc){
          if(xrv){
            assert(cwal_value_refcount(xrv) || cwal_value_is_builtin(xrv));
          }else{
            xrv = cwal_value_undefined();
          }
          s2_ptoker_errtoken_set(pr, &origin);
          rc = s2_throw_value( se, pr, CWAL_RC_EXCEPTION, xrv);
            /* rc will be CWAL_RC_EXCEPTION or something more serious
               (CWAL_RC_OOM) */;
          assert(rc);
          cwal_value_unref(xrv) /* throwing took a refrence to it. */;
          xrv = 0;
        }
        break;
      case S2_T_KeywordCatch:
        cwal_value_unref(xrv)
          /* discard any would-propagate result */;
        xrv = 0;
        switch(rc){ /* Convert certain errors to exceptions... */
          case 0:
            xrv = cwal_value_undefined();
            break;
          case CWAL_SCR_SYNTAX:
            /* MARKER(("pr->errPos=[%.30s...]\n", pr->errPos ? pr->errPos : "<NULL>")); */
            /* MARKER(("se->opErrPos=[%.30s...]\n", se->opErrPos ? se->opErrPos : "<NULL>")); */
            if(!s2__err(se).code){
              break /* we have nothing to report */;
            }else if(phase < 2 && S2_T_SquigglyBlock != sourceType){
              /* Don't convert errors which came from EXPR input
                 unless the error came from its contents (via the
                 -> modifier), because we cannot know from here if the
                 EXPR part itself has a syntax error.
              */
              break;
            }
            else if( CWAL_RC_EXCEPTION !=
                     (rc = s2_throw_err_ptoker(se, pr))){
              /* a more serious error */
              break;
            }
            /*
              Else fall through and treat it like the exception we
              just threw. We know that the error is somewhere
              contained in the _contents_, but the contents are
              (ostensibly) syntactically legal. In the EXPR form, the
              error might be somewhere in the top of the expr (we
              don't know), so we have to pass those on as potentially
              fatal to the current script.
            */
            CWAL_SWITCH_FALL_THROUGH;
          case CWAL_RC_EXCEPTION:
            xrv = cwal_exception_get(se->e);
            assert(xrv &&
                   "Downstream code failed to call cwal_exception_set().");
            /* assert(0==cwal_value_refcount(xrv) && "This is only temporary while debugging something"); */
            /* s2_dump_val(xrv,"exception before rescope"); */
            if(scope){
              assert(scope->parent);
              cwal_value_rescope(scope->parent, xrv);
            }
            cwal_value_ref(xrv);
            cwal_exception_set(se->e, 0);
            cwal_value_unhand(xrv);
            /* s2_dump_val(xrv,"exception after set-exception 0"); */
            assert((cwal_value_scope(xrv) || cwal_value_is_builtin(xrv))
                   && "Premature cleanup on isle 7.");
            assert(!cwal_exception_get(se->e));
            rc = 0
              /* we will propagate xrv as the non-error result. */;
            break;
          default:
            break;
        }
        break;
        /* end S2_T_KeywordCatch */
      default:
        /* Propagate anything else... */
        if(!rc){
          /* we'll allow the NULL==>undefined translation here for
             sanity's sake of sanity: eval -> "". An empty string is a
             valid script, but results in NULL instead of the
             undefined value, and keywords are generally not supposed
             to set *rv to NULL.
          */
          if(S2_T_OpArrow == modifier && !xrv){
            xrv = cwal_value_undefined();
          }
        }
        rc = s2_rv_maybe_accept(se, 0, rc, 0);
        break;
    }
  }
  if(rc || !rv){
    cwal_value_unref(xrv);
    xrv = 0;
  }else{
    if(rv) *rv = xrv ? xrv : cwal_value_undefined();
    cwal_value_unhand(xrv);
  }
  if(scope){
    /* We pushed a scope */
    assert(se->scopes.current->cwalScope == scope);
    assert(s2_scope_current(se)->cwalScope == scope);
    cwal_scope_pop2(se->e, rc ? 0 : xrv);
    assert(!scope->e);
    assert(!scope->props);
  }
  return rc;
}

int s2_keyword_f_reserved( s2_keyword const * kw, s2_engine * se,
                           s2_ptoker * pr, cwal_value **rv){
  if(se->skipLevel>0){
    /* Workaround: so that if/else/etc blocks which traverse these
       don't cry. But then they cry about token ordering. Hmm.
    */
    *rv = cwal_value_undefined();
    return 0;
  }else{
    return s2_err_ptoker( se, pr,
                          CWAL_SCR_SYNTAX
                          /* need for ^^^^ 'catch' to intercept this */,
                          "'%s' is a reserved keyword.",
                          kw->word);
  }
}

static int s2_keyword_f_assert( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv){
  int rc = 0;
  s2_ptoken const pos = pr->token;
  cwal_value * xrv = 0;
  assert(S2_T_KeywordAssert==pos.ttype
         || S2_T_KeywordAffirm==pos.ttype);
  rc = s2_eval_expr_impl(se, pr, 0, 0, &xrv);
  if(rc){
    if(kw){/*avoid unused param warning*/}
    assert(!xrv && "expecting NULL result value on error.");
    return rc;
  }
  else if(!xrv){
    s2_ptoker_errtoken_set(pr, &pos);
    return s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                                "Unexpected empty expression");
  }
  else if(se->skipLevel>0){
    assert(cwal_value_undefined()==xrv);
    *rv = cwal_value_undefined();
    return 0;
  }else{
    cwal_size_t capLen = 0;
    char const * capStr = s2_ptoker_capture_cstr(pr, &capLen);
    char const isAssert = (S2_T_KeywordAssert==pos.ttype) ? 1 : 0;
    char buul;
    cwal_value_ref(xrv);
    buul = cwal_value_get_bool(xrv);
    cwal_value_unref(xrv);
    xrv = 0;
    if(!buul){
      if(se->flags.traceAssertions > 1){
        cwal_outputf(se->e, "%s FAILED: %.*s\n",
                     isAssert ? "ASSERTION" : "AFFIRMATION",
                     (int)capLen, capStr);
      }
      s2_ptoker_errtoken_set(pr, &pos);
      return isAssert
        ? s2_err_ptoker(se, pr, CWAL_RC_ASSERT,
                        "Assertion failed: %.*s",
                        (int)capLen, capStr)
        : s2_throw_ptoker(se, pr, CWAL_RC_ASSERT,
                          "Affirmation failed: %.*s",
                          (int)capLen, capStr);
    }else{
      if(isAssert) ++se->metrics.assertionCount;
      if(se->flags.traceAssertions>1
         || (isAssert && se->flags.traceAssertions>0)){
#if 0
        /* testing token-level line/column count.
           These are off. */
        cwal_size_t slen = 0;
        char const * script = s2_ptoker_name_first(pr, &slen);
        int l = 0, c = 0;
        /* s2_ptoken_adjusted_lc(pr, &ast, &l, &c); */
        s2_ptoker_count_lines2( pr, &pr->capture.begin, &l, &c );
        cwal_outputf(se->e, "%.*s:%d:%d: %s passed: %.*s\n",
          (int)slen, script, l, c,
          isAssert ? "Assertion" : "Affirmation",
          (int)capLen, capStr);
#else
        cwal_outputf(se->e, "%s passed: %.*s\n",
          isAssert ? "Assertion" : "Affirmation",
          (int)capLen, capStr);
#endif
      }
      *rv = cwal_value_true();
      return 0;
    }
  }
}

int s2_keyword_f_typename( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv){
  int rc;
  s2_ptoken const origin = pr->token;
  cwal_value * xrv = 0;
  assert(S2_T_KeywordTypename==pr->token.ttype);
  /*
    TODO:

    - peek and see if the next value is-a identifier. If so, use its string value
    to resolve
  */
  /* Behave as if the keyword wasn't there. */
  rc = s2_eval_expr_impl( se, pr, s2_ttype_op(S2_T_Comma),
                          S2_EVAL_UNKNOWN_IDENTIFIER_AS_UNDEFINED,
                          &xrv );
  /* s2_dump_val(xrv, "TYPENAME result"); */
  if(rc) return rc;
  else if(!xrv){
    s2_ptoker_errtoken_set(pr, &origin);
    rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                       "%s requires a non-empty expression.",
                       kw->word);
  }else if(se->skipLevel>0){
    assert(cwal_value_undefined()==xrv);
    *rv = cwal_value_undefined();
  }else{
    cwal_value * vTn;
    cwal_value_ref(xrv);
    vTn = cwal_prop_get_v(xrv, se->cache.keyTypename)
      /* If interested, see the notes in s2_keyword_f_typeinfo(),
         about TYPEINFO_NAME, for why we first look for this
         property. */;
    if(!vTn){
      cwal_size_t tlen = 0;
      char const * tn;
      tn = cwal_value_type_name2( xrv, &tlen );
      if(!tn){
        assert(!"Can't really happen, can it?");
        tn = cwal_type_id_name(CWAL_TYPE_UNDEF);
        tlen = cwal_strlen(tn);
      }
      vTn = cwal_new_string_value(se->e, tn, tlen);
      if(!vTn) rc = CWAL_RC_OOM;
    }
    cwal_value_ref(vTn);
    cwal_value_unref(xrv);
    if(rc){
      assert(NULL == vTn);
      cwal_value_unref(vTn) /* noop, but for symmetry */;
    }else{
      assert(NULL != vTn);
      *rv = vTn;
      cwal_value_unhand(vTn);
    }
  }
  return rc;
}

int s2_keyword_f_refcount( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv){
#if 1
  S2_UNUSED_ARG(rv);
  return s2_err_ptoker( se, pr, CWAL_SCR_SYNTAX,
                        "'%s' is so gauche. Use pragma(refcount ...) "
                        "instead!", kw->word );
#else
  int rc;
  cwal_value * xrv = 0;
  s2_ptoken const origin = pr->token;
  rc = s2_eval_expr_impl( se, pr, s2_ttype_op(S2_T_Comma),
                          /* S2_EVAL_NO_SKIP_FIRST_EOL */ 0, &xrv );
  if(rc){
    assert(!xrv);
  }else{
    if(!xrv){
      s2_ptoker_errtoken_set(pr, &origin);
      rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                                "%s requires an RHS expression",
                                kw->word);
    }else if(se->skipLevel>0){
      assert(cwal_value_undefined()==xrv);
      *rv = cwal_value_undefined();
    }else{
      *rv = cwal_new_integer(se->e, (cwal_int_t)cwal_value_refcount(xrv));
      cwal_refunref(xrv);
      rc = *rv ? 0 : CWAL_RC_OOM;
    }
  }
  return rc;
#endif
}

int s2_keyword_f_nameof( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv){
  s2_ptoken next = s2_ptoken_empty;
  int rc;
  assert(S2_T_KeywordNameof==pr->token.ttype);
  rc = s2_next_token( se, pr, 0, &next );
  if(rc) return rc;
  else if(S2_T_Identifier != next.ttype){
    return s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                                "%s requires an IDENTIFIER argument",
                                kw->word);
  }else if(se->skipLevel>0){
    s2_ptoker_token_set(pr, &next);
    *rv = cwal_value_undefined();
    return 0;
  }else{
    cwal_value * xrv = 0;
    cwal_size_t tlen = s2_ptoken_len(&next);
    assert(s2_ptoken_begin(&next)<s2_ptoken_end(&next));
    rc = s2_get( se, 0, s2_ptoken_begin(&next), tlen, &xrv );
    if(rc) return rc;
    else if(!xrv){
      s2_ptoker_errtoken_set(pr, &next);
      return s2_err_ptoker(se, pr, CWAL_RC_NOT_FOUND,
                           "Cannot resolve identifier '%.*s'",
                           (int)tlen, s2_ptoken_begin(&next));
    }else{
      assert((S2_T_KeywordNameof==pr->token.ttype)
             && "lookahead broken?");
      s2_ptoker_token_set(pr, &next);
      *rv = cwal_new_string_value(se->e, s2_ptoken_begin(&next), tlen);
      return *rv ? 0 : CWAL_RC_OOM;
    }
  }
}  

/**
   Internal helper for collecting lists of declared variables, namely
   var/const and function parameter lists.

   Evaluates a comma-separated list of IDENTIFIER [= EXPR] tokens
   from pr until EOX, declaring each one in the current scope.
   If isConst, the vars are declared const.

   If argv is not 0 then it is assumed to be the arguments array
   passed to a function call, and it is populated with any default
   values not accounted for by that array (exception: in skip mode it
   is not modified). Default parameter values are not processed for
   slots filled by argv. Contrariwise, if the var list contains more
   entries than argv, any extras are appended to argv.  If rv is not
   0, the last-evaluated value is put in *rv.

*/
static int s2_keyword_f_var_impl( s2_keyword const * kw, s2_engine * se,
                                  s2_ptoker * pr, cwal_value **rv,
                                  char isConst,
                                  cwal_array * argv){
  s2_ptoken next = s2_ptoken_empty;
  s2_ptoken ident = s2_ptoken_empty;
  cwal_value * v;
  int rc;
  char gotComma;
  cwal_size_t argPos = 0;
  uint16_t declFlags;
  s2_op const * pseudoOp = s2_ttype_op(S2_T_Comma);
  cwal_size_t argc = argv ? cwal_array_length_get(argv): 0;

  assert(pseudoOp);
  next_part:
  v = 0;
  declFlags = isConst ? CWAL_VAR_F_CONST : 0;
  rc = s2_next_token( se, pr, 0, 0 );
  if(rc) return rc;
  else if(S2_T_Identifier != pr->token.ttype){
    if(argv && s2_ttype_is_eof(pr->token.ttype)){
      /* empty function argument list */
      return 0;
    }else{
      return s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                           "'%s' list requires an identifier argument, "
                           "but got token type %d with contents [%.*s].",
                           kw->word, pr->token.ttype,
                           (int)s2_ptoken_len(&pr->token),
                           s2_ptoken_begin(&pr->token));
    }
  }
  gotComma = 0;
  ident = pr->token;
  if(!se->skipLevel){
    if(s2_ptoken_keyword2(se, &ident)){
      /* Confirm that it's not a keyword, as the user would not be
         able to use the var/param properly. Throw vs error:
         s2_err_ptoker() is ostensibly fatal to a script, but
         subscripts will, on error propagation, convert this to
         an exception, so a sub-/imported script won't kill a
         top-level script this way. Hmmm... if this is an Error
         then it's difficult to test (our current unit tests
         will abort when they hit it). We'll make this an
         Exception for the time being:
      */
      return s2_throw_ptoker(se, pr, CWAL_RC_ALREADY_EXISTS,
                             "Cannot use keyword '%.*s' "
                             "as a %s name.",
                             (int)s2_ptoken_len(&ident),
                             s2_ptoken_begin(&ident),
                             argv ? "parameter" : "variable");
      
    }else if(cwal_scope_search(/*&se->currentScope->scope
                                 ^^^^
                                 cwal_function_call() pushes a scope
                                 which s2 doesn't have an s2_scope
                                 for, so we need to look in
                                 se->e->current instead, else an
                                 overloaded operator's declared vars
                                 can report a collision where there is
                                 really none. Been there, done that.

                                 20191117: that "shouldn't" be an
                                 issue since the cwal API added the
                                 scope-push/pop hooks, as the scopes
                                 are now in sync. This setup continues
                                 to work, though, so we'll leave it as
                                 is.
                               */
                                se->e->current,
                                0, s2_ptoken_begin(&ident),
                                s2_ptoken_len(&ident),
                                0)){
      /* Check for dupe symbols before evaluating the RHS. This
         "need" was uncovered by the sqlite3 module. It's a
         small performance hit, though. */
      return s2_throw_ptoker(se, pr, CWAL_RC_ALREADY_EXISTS,
                             "Symbol '%.*s' is already declared "
                             "in the current scope.",
                             (int)s2_ptoken_len(&ident),
                             s2_ptoken_begin(&ident));
    }
  }
  rc = s2_next_token( se, pr, 0, &next);
  if(rc) return rc;
  switch(next.ttype){
    case S2_T_Comma:
      gotComma = 1;
      CWAL_SWITCH_FALL_THROUGH;
    case S2_T_EOF:
    case S2_T_EOX:
      v = isConst ? 0 : cwal_value_undefined();
      s2_ptoker_token_set( pr, &next ) /* consume it */;
      break;
    case S2_T_OpColonEqual:
      if(argv){
        return s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                             "The := operator is not permitted on function "
                             "parameters because of syntactic inconsistencies "
                             "with regards to parameters with resp. without "
                             "default values.");
      }
      declFlags = CWAL_VAR_F_CONST;
      CWAL_SWITCH_FALL_THROUGH;
    case S2_T_OpAssign:
      s2_ptoker_token_set(pr, &next);
      if(argv && argPos<argc){
        /* Do not process default values for argument positions
           already filled by argv. The var decl below will use argv's
           value for this position.
        */
        rc = s2_eval_expr_impl(se, pr, pseudoOp, S2_EVAL_SKIP, 0);
        if(!rc) v = cwal_value_undefined();
      }else{
        rc = s2_eval_expr_impl(se, pr, pseudoOp, 0, &v);
      }
#if 0
      if(rc){
        /* If we disallow return/break/continue here, then we cannot
           do:

           const ex = catch { ... return foo };

           and i think that should work.
        */
        return s2_check_brc(se, pr, rc,
                            isConst
                            ? "const declaration"
                            : "var declaration");
      }
      else
#else
      if(rc){
        assert(!v && "Internal API misuse.");
        return rc;
      }
      else
#endif
      if(!v /* && !s2_ptoker_is_eof(pr) */){
        if(!isConst && S2_T_OpColonEqual == next.ttype){
          return s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                               "%s declaration with := requires a value.",
                               kw->word, (int)s2_ptoken_len(&ident),
                               s2_ptoken_begin(&ident));
        }else{
          return s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                               "Empty expression is not permitted "
                               "after %s assignment.",
                               argv
                               ? "parameter"
                               : (isConst?"const":"var"));
        }
      }
      break;
    default:
      return s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                           "Unexpected token '%.*s' in %s decl '%.*s'.",
                           (int)s2_ptoken_len(&next),
                           s2_ptoken_begin(&next),
                           argv
                             ? "parameter"
                             : (isConst?"const":"var"),
                           (int)s2_ptoken_len(&ident),
                           s2_ptoken_begin(&ident));
  }
  if(!v){
    if(isConst){
      return s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                           "%s decl '%.*s' requires a value.",
                           kw->word, (int)s2_ptoken_len(&ident),
                           s2_ptoken_begin(&ident));
    }
    v = cwal_value_undefined();
  }

  if(!se->skipLevel){
    cwal_size_t const tlen = s2_ptoken_len(&ident);
    /* s2_dump_val(v, "var decl before"); */
    if(argv && argPos<argc){
      /* Value at this index is already in argv. */
      v = cwal_array_get(argv, argPos);
    }
    rc = s2_var_decl(se, s2_ptoken_begin(&ident), tlen, v, declFlags);
    if(CWAL_RC_ALREADY_EXISTS==rc){
      assert(!"CWAL_RC_ALREADY_EXISTS is checked earlier now.");
      s2_ptoker_errtoken_set(pr, &ident);
      return s2_throw_ptoker(se, pr, rc,
                            "Symbol '%.*s' is already declared "
                            "in the current scope.",
                             (int)tlen, s2_ptoken_begin(&ident));
    }
    else if(rc){
      return rc;
    }
    /* MARKER(("Declared %s '%.*s'\n", kw->word, (int)tlen,
       s2_ptoken_begin(&ident))); */
    /* s2_dump_val(v, "var decl after"); */
    ++argPos;
  }
  if(!s2_ptoker_is_eof(pr) && !gotComma){
    /* Check for a follow-up token */
    if(s2_ptoker_next_is_ttype(se, pr, S2_NEXT_NO_POSTPROCESS, S2_T_Comma, 1)){
      gotComma = 1;
    }
  }
  assert(!rc);
  rc = s2_check_interrupted(se, rc);
  if(!rc){
    if(gotComma) goto next_part;
    assert(v);
    if(rv) *rv = v;
  }
  return rc;
}


int s2_keyword_f_var( s2_keyword const * kw, s2_engine * se,
                      s2_ptoker * pr, cwal_value **rv){
  char const isConst = (S2_T_KeywordConst==pr->token.ttype) ? 1 : 0;
  return s2_keyword_f_var_impl( kw, se, pr, rv, isConst, 0 );
}

int s2_keyword_f_unset( s2_keyword const * kw, s2_engine * se,
                      s2_ptoker * pr, cwal_value **rv){
  s2_ptoken next = s2_ptoken_empty;
  s2_ptoken origin = s2_ptoken_empty;
  s2_ptoken ident = s2_ptoken_empty;
  int rc;
  char gotComma;
  int identLen;
  assert(S2_T_KeywordUnset==pr->token.ttype);
  next_part:
  origin = pr->token;
  rc = s2_next_token( se, pr, 0, 0 );
  if(rc) return rc;
  else if(S2_T_Identifier != pr->token.ttype){
    return s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                                "%s requires an IDENTIFIER argument",
                                kw->word);
  }
  gotComma = 0;
  ident = pr->token;
  identLen = (int)s2_ptoken_len(&ident);
  /* Check next token first to avoid unsetting the obj part of obj.prop. */
  rc = s2_next_token( se, pr, 0, &next);
  if(rc) return rc;
  switch(next.ttype){
    case S2_T_Comma:
      gotComma = 1;
      s2_ptoker_token_set( pr, &next ) /* consume it */;
      CWAL_SWITCH_FALL_THROUGH;
    case S2_T_EOF: /* e.g. typing "unset x" in s2sh */
    case S2_T_EOX:{
      if(!se->skipLevel){
        cwal_kvp * kvp = cwal_scope_search_kvp(se->e->current, 0,
                                               s2_ptoken_begin(&ident),
                                               (cwal_size_t)identLen,
                                               0);
        if(!kvp){
          return s2_throw_ptoker(se, pr, CWAL_RC_NOT_FOUND,
                                 "%s could not resolve identifier '%.*s' "
                                 "in the local scope.",
                                 kw->word, identLen, s2_ptoken_begin(&ident));
        }else if(CWAL_VAR_F_CONST & cwal_kvp_flags(kvp)){
          return s2_throw_ptoker(se, pr, CWAL_RC_NOT_FOUND,
                                 "Cannot %s const '%.*s'.",
                                 kw->word, identLen, s2_ptoken_begin(&ident));
        }
        rc = s2_handle_set_result(se, pr,
                                  s2_set( se, 0, s2_ptoken_begin(&ident),
                                          (cwal_size_t)identLen, 0));
        /* MARKER(("Unsetting ident [%.*s] rc=%d\n",
           (int)s2_ptoken_len(&ident), s2_ptoken_begin(&ident), rc)); */
        if(rc && !cwal_exception_get(se->e)){
          rc = s2_throw_err_ptoker( se, pr );
        }
        if(rc) return rc;
      }
      if(gotComma) goto next_part;
      break;
    }
#if 0
    case S2_T_OpArrow:
#endif
    case S2_T_OpDot:
    case S2_T_BraceGroup:{
      s2_op const * pseudoOp = s2_ttype_op(S2_T_Comma);
      assert(pseudoOp);
      /* MARKER(("Unsetting a property?\n")); */
      /*
        Treat this as a property unset:

        OpDot | BraceGroup: eval expr with Comma
        precedence; get se->dotOp.lhs and se->dotOp.key;

        That's full of weird corner cases, though:

        unset a.b = c.d

        20191117: that ends up unsetting (without error) c.d because
        se->dotOp refers, at the point where the unset is resolved, to
        c.d instead of a.b. Hmmm. Maybe we need a new
        S2_EVAL_FOR_UNSET flag, which only allows identifiers and
        dot-op, throwing if any other operators are seen
        access... except that in order to know whether we're doing a
        dot-op, we first have to eval the LHS of that dot op
        (e.g. unset (someFuncCall()).foo is/needs to be legal). Hmmm.
        The only solution i currently see is to skip-mode the first
        eval phase, store its source position, then check for a
        dot. If it's a dot, go back and eval that LHS (whatever it
        is), else check if the LHS is an identifier, and fail if it's
        not. Hmmm.

        So far such a case has never happened in practice, so priority
        for "fixing" it is low, but it's still an unsightly behaviour.
      */
      s2_ptoker_token_set(pr, &origin);
      rc = s2_eval_expr_impl(se, pr, pseudoOp,
                             S2_EVAL_RETAIN_DOTOP_STATE, 0);
      if(rc) return rc;
      else if(!se->skipLevel){
        if(!se->dotOp.lhs || !se->dotOp.key){
          s2_ptoker_errtoken_set(pr, &ident);
          return s2_throw_ptoker( se, pr, CWAL_SCR_SYNTAX,
                                  "Illegal RHS for %s operation.",
                                  kw->word);
        }
        cwal_value_ref(se->dotOp.lhs);
        cwal_value_ref(se->dotOp.key);
        /* Do we want to support: unset hash # key? */
        rc = s2_handle_set_result(se, pr,
                                  s2_set_v( se, se->dotOp.lhs, se->dotOp.key, 0 ));
        cwal_value_unhand(se->dotOp.lhs);
        cwal_value_unhand(se->dotOp.key);
        s2_dotop_state(se, 0, 0, 0);
        if(rc) return rc;
      }
      if(s2_ptoker_next_is_ttype(se, pr, S2_NEXT_NO_POSTPROCESS, S2_T_Comma, 1)){
        goto next_part;
      }
      break;
    }
    default:
      s2_ptoker_errtoken_set(pr, &next);
      return s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                           "Unexpected token type %s in %s.",
                           s2_ttype_cstr(next.ttype),
                           kw->word
                           /*(int)s2_ptoken_len(&ident),
                             s2_ptoken_begin(&ident)*/);
  }
  assert(0==rc);
  *rv = cwal_value_undefined();
  return 0;
}

int s2_keyword_f_if( s2_keyword const * kw, s2_engine * se,
                      s2_ptoker * pr, cwal_value **rv){
  int rc = 0;
  s2_ptoken next = s2_ptoken_empty;
  s2_ptoken tCondition = s2_ptoken_empty;
  s2_ptoken tBody = s2_ptoken_empty;
  cwal_scope _SCOPE = cwal_scope_empty;
  cwal_scope * scope = 0;
  cwal_value * xrv = 0;
  char buul = 0;
  char finalElse = 0;
  char hasTrued = 0;
  int runCount = 0;
  char bodyIsExpr = 0;
  next_if:
  ++runCount;
  /* Get the condition part... */
  rc = s2_next_token(se, pr, 0, 0);
  if(S2_T_ParenGroup != pr->token.ttype){
    rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                       "Expecting (...) after '%s'.",
                       kw->word);
    goto end;
  }
  tCondition = pr->token;

  /* Get the body part... */
  rc = s2_next_token(se, pr, 0, 0);
  if(rc) goto end;

  capture_body:
  bodyIsExpr = 0;
  tBody = pr->token;
  while(S2_T_SquigglyBlock != tBody.ttype
        && S2_T_Heredoc/*historical behaviour*/ != tBody.ttype){
    if(s2_ttype_is_eox(tBody.ttype)){
      /* special case: empty body. */
      rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                         "Empty non-block expression is not allowed.");
      goto end;
    }
    else{
      /* Check for an expression...

         Remember that tCondition.end is a bit screwy because it's based
         on the s2_ptoken::adjEnd of its input token.
      */
      if(!finalElse){
        s2_ptoker_token_set(pr, &tCondition);
      }/* else the token is already placed where it needs to be */
      /* MARKER(("pr->token=%.*s...\n",10, s2_ptoken_begin(&pr->token))); */
      if((rc = s2_eval_expr_impl(se, pr, 0, S2_EVAL_SKIP, 0))){
        goto end;
      }else if(s2_eval_is_eox(se, pr)
              /* is an expression ending with an EOX */){
        /* FIXME: won't work as-is with #compiled tokens */
        tBody = pr->capture.begin;
        s2_ptoken_end_set(&tBody, s2_ptoken_begin(&pr->capture.end));
        /*MARKER(("CAPTURE: %.*s\n", (int)s2_ptoken_len(&tBody),
          s2_ptoken_begin(&tBody)));*/
        bodyIsExpr = 1;
        break;
      }else{
        MARKER(("???\n"));
        assert(!"what happens here?");
      }
    }
    rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                       "Expecting {...} or a single "
                       "expression after %s(...).",
                       kw->word);
    goto end;
  }
  if(finalElse){
    goto the_body;
  }

  if(1==runCount && !se->skipLevel){
    assert(!scope);
    scope = &_SCOPE;
    rc = cwal_scope_push2(se->e, scope);
    if(rc) return rc;
  }

  /* Eval the condition... */
  s2_ptoker_token_set(pr, &tCondition);
  /* Reminder: we need to eval-sub, even in skip mode,
     for the corner case of an empty (). */
  rc = s2_eval_current_token( se, pr, 0, 0, &xrv );
  if(rc) goto end;
  else if(!xrv){
    rc = s2_err_ptoker( se, pr, CWAL_SCR_SYNTAX,
                        "Empty '%s' condition is not allowed.",
                        kw->word);
    goto end;
  }
  buul = (se->skipLevel>0) ? 0 : cwal_value_get_bool(xrv);
  xrv = 0;
  the_body:
  /*MARKER(("tBody: %.*s\n", (int)s2_ptoken_len(&tBody),
    s2_ptoken_begin(&tBody)));*/

  /* assert(S2_T_SquigglyBlock==pr->token.ttype || bodyIsExpr); */
  if(!finalElse && (!buul || hasTrued || se->skipLevel>0)){
    s2_ptoker_token_set(pr, &tBody);
    if(bodyIsExpr){
      s2_next_token(se, pr, 0, 0)
        /* Slurp up the semicolon */;
      assert(s2_ttype_is_eox(pr->token.ttype));
    }
    goto check_else;
  }

  /*
    Arguably slightly broken, but really not:

    1 ? if(1){ 1; } else 1 : 1
    ----------------------^^^

    ==> Unexpected ':' token (no X?Y:Z op in progress).

    The problem is one of scope. The 'else' is happening in a
    different scope than the '?', and ternary state explicitly does
    not span scopes (it's saved/reset/restored when pushing/popping
    scopes). That construct can be resolved by using a {block} or
    explicitly ending the else's expression with a semicolon.
  */
  s2_ptoker_token_set(pr, &tBody);
  rc = s2_eval_current_token(se, pr, bodyIsExpr, 0, 0);
  if(!rc && bodyIsExpr){
    /*
      var a = 0;
      if(0) throw __FLC; else if(1) a = 1; else throw __FLC;
      -------------------------------------^^^^

      Could not resolve identifier 'else'

      tBody = "a = 1". rc from this eval is 0.
      capture after eval of the 2nd 'if' is "a = 1".
      pr is not at EOX afterwards: current token is S2_T_INVALID
      (because tBody is from the capture, not a valid token type,
      and pr is not advanced by s2_eval_current_token()).

      The problem is that the next token in pr, in that case, is an
      EOX, which s2_eval_current_token() is not getting/consuming, so
      we are seeing is as the end of this keyword's work and we're
      propagating back up to the eval loop, which then picks up the
      'else' and thinks it's an unknown identifier.

      As a workaround, we'll check the next token and, if it's an EOX,
      consume it so that the next/upcoming check of an 'else' part can
      DTRT. If it's not an EOX, we'll put it back and let the next part
      deal with it.
    */
    rc = s2_next_token(se, pr, 0, &next);
    if(!rc){
      if(s2_ttype_is_eox(next.ttype)) s2_ptoker_token_set(pr, &next) /* consume it */;
      else s2_ptoker_next_token_set(pr, &next) /* give it back for the next caller */;
    }
  }
  if(rc) goto end;
  else if(!hasTrued && !finalElse){
    hasTrued = 1;
    ++se->skipLevel;
  }
 
  check_else:
  if(finalElse){
    goto end;
  }
  next = s2_ptoken_empty;
  rc = s2_next_token(se, pr, 0, &next);

  /*MARKER(("bodyIsExpr=%d\n", bodyIsExpr));*/
  /*MARKER(("next: <<<%.*s>>>\n", (int)s2_ptoken_len(&next),
    s2_ptoken_begin(&next)));*/
#define TOK_IS_ELSE(TOKP) (4==(int)(s2_ptoken_len(TOKP))            \
                           && 0==memcmp(s2_ptoken_begin(TOKP),"else",4))
  bodyIsExpr = 0;
  /*MARKER(("after IF: <<<%.*s>>>\n", (int)s2_ptoken_len(&next),
    s2_ptoken_begin(&next)));*/
  if(!rc && TOK_IS_ELSE(&next)){
    s2_keyword const * ifCheck;
    s2_ptoken const theElse = next;
    s2_ptoker_token_set(pr, &next) /*consume it*/;

    next = s2_ptoken_empty;
    rc = s2_next_token(se, pr, 0, &next);
    if(rc) goto end;
    else if((ifCheck=s2_ptoken_keyword(&next))
            && (S2_T_KeywordIf==ifCheck->id)){
      s2_ptoker_token_set(pr, &next) /*consume it*/;
      goto next_if;
    }else{
      /*MARKER(("final else: %s <<<%.*s>>>\n",
        s2_ttype_cstr(next.ttype),
        (int)s2_ptoken_len(&next), s2_ptoken_begin(&next)));*/
      if(S2_T_SquigglyBlock==next.ttype
         || S2_T_Heredoc==next.ttype){
        s2_ptoker_token_set(pr, &next) /* make sure we've got the right token type */;
      }else{
        s2_ptoker_token_set(pr, &theElse);
      }
      finalElse = 1;
      goto capture_body;
    }
  }
#undef TOK_IS_ELSE 
  end:
  if(hasTrued) --se->skipLevel;
  if(scope){
    assert(scope->parent);
    cwal_scope_pop(se->e);
  }
  if(!rc){
    *rv = se->skipLevel
      ? cwal_value_undefined()
      : (hasTrued
         ? cwal_value_true()
         : cwal_value_false())
      ;
  }
  /*MARKER(("tokenizer at: %.*s\n",
    (int)(s2_ptoker_end(pr) - s2_ptoken_end(&pr->token)),
s2_ptoken_end(&pr->token)));*/
  return rc;
}

int s2_keyword_f_continue( s2_keyword const * kw, s2_engine * se,
                           s2_ptoker * pr, cwal_value **rv){
  if(se->skipLevel>0){
    *rv = cwal_value_undefined();
    if(kw){/*avoid unused param warning*/}
    return 0;
  }else{
    return s2_err_ptoker(se, pr, CWAL_RC_CONTINUE, 0);
  }
}

/**
   Internal code consolidator for do/while/for/foreach loops.

   kw is the keyword on whose behalf this is working.

   Preconditions: pr's must be set up such that the next call to
   s2_next_token() (which this function makes) will fetch what we
   believe/hope is the loop body. This function determines whether
   it's a {block} or a single expression and sets *tBody to contain
   the block/expression contents.

   *bodyIsExpr is assigned 0 if tBody represents a {block}, else it's
   set to non-0.

   All loop types except for foreach() allow an empty body but require
   that non-{} expression bodies be explicitly EOX-terminated or end
   on an implicit EOX (e.g. the end of a block construct).

   Returns 0 on success. Any error must be propagated back to the
   loop's caller and the state of pr, tBody, and bodyIsExpr is
   unspecified.
*/
static int s2_keyword_loop_get_body( s2_keyword const * kw, s2_engine * se,
                                     s2_ptoker * pr, s2_ptoken * tBody,
                                     char * bodyIsExpr){
  /*s2_ptoken const origin = pr->token;*/
  char const allowEmptyBody = S2_T_KeywordForEach==kw->id ? 0 : 1;
  int rc = s2_next_token(se, pr, 0, 0);
  if(rc) return rc;
  *tBody = pr->token;
  if(S2_T_SquigglyBlock == tBody->ttype
     || S2_T_Heredoc/*historical behaviour*/ == tBody->ttype){
    if(!allowEmptyBody && !s2_ptoken_has_content(tBody)){
      goto empty_body;
    }else{
      *bodyIsExpr = 0;
      return 0;
    }
  }
  else if(s2_ttype_is_eox(tBody->ttype)){
    /* special case: empty body. */
    if(allowEmptyBody){
      /* all but foreach() allow an empty body but require that it ends with
         an explicit EOX. */
      *bodyIsExpr = 1;
      return 0;
    }else{
      goto empty_body;
      /* foreach() disallows an empty body because it'd do nothing
         at all except iterate (no visible side-effects). */
    }
  }else{
    /* Check for an expression... */
    /* MARKER(("pr->token=[%.*s]\n",(int)s2_ptoken_len(&pr->token),
       s2_ptoken_begin(&pr->token))); */
    s2_ptoker_next_token_set(pr, tBody);
    if((rc = s2_eval_expr_impl(se, pr, 0, S2_EVAL_SKIP, 0))) return rc;
    else if(s2_eval_is_eox(se, pr)
            /* is an expression ending with an EOX */){
      *tBody = pr->capture.begin;
      s2_ptoken_end_set(tBody, s2_ptoken_begin(&pr->capture.end))
        /* 20200107 FIXME: this can't work with #compiled tokens. */;
      *bodyIsExpr = 1;
#if 0
      {
        cwal_size_t n = 0;
        char const * str = s2_ptoken_cstr(tBody, &n);
        MARKER(("expr tBody = %.*s\n", (int)n, str));
        str = s2_ptoker_capture_cstr(pr, &n);
        MARKER(("pr->capture = %.*s\n", (int)n, str));
      }
#endif
      return 0;
    }
    return s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                         "Expecting {...} or a single "
                         "expression after %s(...).",
                         kw->word);
  }
  empty_body:
  return s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                       "%s may not have an empty body.",
                       kw->word);
}


int s2_keyword_f_while( s2_keyword const * kw, s2_engine * se,
                      s2_ptoker * pr, cwal_value **rv){
  int rc = 0;
  s2_ptoken tCondition = s2_ptoken_empty;
  s2_ptoken tBody = s2_ptoken_empty;
  cwal_scope scope = cwal_scope_empty;
  cwal_value * xrv = 0;
  char bodyIsExpr = 0;

  /* Get the condition part... */
  rc = s2_next_token(se, pr, 0, 0);
  if(rc) return rc;
  else if(S2_T_ParenGroup!=pr->token.ttype){
    return s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                         "Expecting (...) after '%s'.",
                         kw->word);
  }
  tCondition = pr->token;

  /* Get the body part... */
  rc = s2_keyword_loop_get_body(kw, se, pr, &tBody, &bodyIsExpr);
  /* Run the tBody while the tCondition evaluates to truthy... */
  while(!rc){
    char buul = 0;
    if(scope.parent){
      cwal_scope_pop(se->e);
      assert(!scope.parent);
    }
    rc = cwal_scope_push2(se->e, &scope);
    if(rc) break;
    assert(scope.parent);
    s2_ptoker_token_set(pr, &tCondition);
    assert(!xrv);
    rc = s2_eval_current_token( se, pr, 0, 0, &xrv );
    if(rc) break;
    else if(!xrv){
      rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                         "Invalid empty expression in %s().",
                         kw->word);
      break;
    }
    buul = cwal_value_get_bool(xrv);
    /* ref()/unref() combo kills temporaries, regardless of
       their owning scope, but leaves others effectively
       untouched. This is the same hack the for() loop uses -
       see explanation in s2_keyword_f_for(). */
    cwal_refunref(xrv);
    xrv = 0;
    if(!buul){
      s2_ptoker_token_set(pr, &tBody)
        /* consume the body. err... you know what i mean. */;
      break;
    }
    else{
      int doBreak = 0;
      s2_ptoker_token_set(pr, &tBody) /* set body up for eval */;
      if(se->skipLevel>0){
        /* we only need to get this far to ensure
           syntax rules are met. */
        break;
      }
      s2_ptoker_token_set(pr, &tBody);
      rc = s2_eval_current_token(se, pr, bodyIsExpr, 0, 0);
      switch(rc){
        case CWAL_RC_BREAK:
          doBreak = rc;
          xrv = s2_propagating_take(se);
          assert(xrv);
          assert(scope.parent);
          cwal_value_rescope(scope.parent, xrv);
          rc = 0;
          cwal_value_ref(xrv) /* required when xrv is created in tCondition! */;
          s2_engine_err_reset(se);
          break;
        case CWAL_RC_CONTINUE:
          rc = 0;
          s2_engine_err_reset(se);
          break;
        default:
          break;
      }
      if(doBreak) break;
    }/* end body eval */
  }/* end while(!rc) */
  
  if(scope.parent){
    cwal_scope_pop2(se->e, (rc && !se->skipLevel) ? 0 : xrv);
    assert(!scope.parent);
  }
  if(rc){
    cwal_value_unref(xrv);
  }else{
    cwal_value_unhand(xrv);
    *rv = se->skipLevel
      ? cwal_value_undefined()
      : (xrv ? xrv : cwal_value_undefined());
  }
  return rc;
}

int s2_keyword_f_dowhile( s2_keyword const * kw, s2_engine * se,
                          s2_ptoker * pr, cwal_value **rv){
  int rc = 0;
  s2_ptoken tCondition = s2_ptoken_empty;
  s2_ptoken tBody = s2_ptoken_empty;
  /* s2_ptoken const tOrigin = pr->token; */
  cwal_scope scope = cwal_scope_empty;
  cwal_value * xrv = 0;
  char bodyIsExpr = 0;
  /* Get the body part... */
  rc = s2_keyword_loop_get_body(kw, se, pr, &tBody, &bodyIsExpr);
  if(rc) return rc;
  /* Get the while(condition) parts... */
  rc = s2_next_token(se, pr, 0, 0);
  if(rc) return rc;
  else{
    s2_keyword const * kWhile = s2_ptoken_keyword(&pr->token);
    if(!kWhile){
      return s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                           "Expecting while(...) after '%s' body.",
                           kw->word);
    }
  }
  /* Get the (condition) part... */
  rc = s2_next_token(se, pr, 0, 0);
  if(rc) return rc;
  else if(S2_T_ParenGroup != pr->token.ttype){
    rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                       "Expecting (...) after %s...while.",
                       kw->word);
  }
  tCondition = pr->token;
  if(se->skipLevel){
    *rv = cwal_value_undefined();
    return 0;
  }
  /* Run tBody while tCondition evaluates to truthy... */
  do{
    char doBreak = 0;
    if(scope.parent){
      cwal_scope_pop(se->e);
      assert(!scope.parent);
    }
    rc = cwal_scope_push2(se->e, &scope);
    if(rc) break;
    assert(scope.parent);

    /* Run the body... */
    xrv = 0;
    s2_ptoker_token_set(pr, &tBody) /* set body up for eval */;
    rc = s2_eval_current_token(se, pr, bodyIsExpr, 0, 0);
    switch(rc){
      case CWAL_RC_BREAK:
        xrv = s2_propagating_take(se);
        assert(xrv && "Expecting 'break' keyword to set this.");
        assert(scope.parent);
        cwal_value_ref(xrv);
        doBreak = 1;
        CWAL_SWITCH_FALL_THROUGH;
      case CWAL_RC_CONTINUE:
        rc = 0;
        s2_engine_err_reset(se);
        break;
      default:
        break /* other, more serious, error,
                 or a 'return' or similar */;
    }
    if(rc || doBreak) break;
    s2_ptoker_token_set(pr, &tCondition);
    xrv = 0;
    rc = s2_eval_current_token( se, pr, 0, 0, &xrv );
    if(rc){
      xrv = 0 /* just in case, so we don't misbehave below */;
      break;
    }else if(!xrv){
      rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                         "Invalid empty expression in %s...while().",
                         kw->word);
      break;
    }else if(!cwal_value_get_bool(xrv)){
      /* Condition is falsy, end the loop. */
      xrv = 0;
      break;
    }else{
      /* Condition is truthy, so continue. ref/unref combo cleans up
         temps, regardless of owning scope, while not cleaning up
         non-temps.*/
      cwal_refunref(xrv);
      xrv = 0;
    }
  }while(!rc);
  
  if(scope.parent){
    cwal_scope_pop2(se->e, rc ? 0 : xrv);
    assert(!scope.parent);
  }

  if(rc){
    cwal_value_unref(xrv);
  }else{
    cwal_value_unhand(xrv);
    s2_ptoker_token_set(pr, &tCondition) /* consume the final (condition) part */;
    *rv = se->skipLevel
      ? cwal_value_undefined()
      : (xrv ? xrv : cwal_value_undefined());
  }
  return rc;
}


int s2_keyword_f_for( s2_keyword const * kw, s2_engine * se,
                      s2_ptoker * pr, cwal_value **rv){
  int rc = 0;
  s2_ptoken tCondition = s2_ptoken_empty;
  s2_ptoken tPre = s2_ptoken_empty;
  s2_ptoken tPost = s2_ptoken_empty;
  s2_ptoken tBody = s2_ptoken_empty;
  s2_ptoker prParens = s2_ptoker_empty;
  cwal_scope scopeOut = cwal_scope_empty;
  cwal_scope scopeIn = cwal_scope_empty;
  cwal_value * xrv = 0;
  char bodyIsExpr = 0;
  unsigned int runCount = 0;
  /* Get the condition part... */
  rc = s2_next_token(se, pr, 0, 0);
  if(rc) return rc;
  else if(S2_T_ParenGroup!=pr->token.ttype){
    return s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                         "Expecting (...) after '%s'.",
                         kw->word);
  }
  rc = s2_ptoker_sub_from_toker(pr, &prParens);
  if(rc) return rc;

  /* Get the body part... */
  rc = s2_keyword_loop_get_body( kw, se, pr, &tBody, &bodyIsExpr );
  if(rc) goto end;

  /* Capture the initial expr of (x;y;z) */
  tPre = prParens.token;
  rc = s2_eval_expr_impl(se, &prParens, 0, S2_EVAL_SKIP, 0);
  if(rc) goto end;
  if( S2_T_EOX != prParens.token.ttype){
    s2_ptoker_errtoken_set(pr, &prParens.token);
    rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                       "Expecting ';' at start of %s(...), "
                       "but got %s.",
                       kw->word,
                       s2_ttype_cstr(prParens.token.ttype));
    goto end;
  }
  /* reminder to self: tPre, being assigned before prParens' first token
     is fetched, has no 'end'. We probably don't need it. */

  /* Capture the condition expr of (x;y;z) */
  tCondition = prParens.token;
  rc = s2_eval_expr_impl(se, &prParens, 0, S2_EVAL_SKIP, 0);
  if(rc) goto end;
  if( S2_T_EOX != prParens.token.ttype){
    s2_ptoker_errtoken_set(pr, &prParens.token);
    rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                       "Expecting ';' after condition part "
                       "of %s(...), but got %s.",
                       kw->word,
                       s2_ttype_cstr(prParens.token.ttype));
    goto end;
  }

  /* Capture the post-loop expr of (x;y;z) and make sure it's sound */
  tPost = prParens.token;
  rc = s2_eval_expr_impl(se, &prParens, 0, S2_EVAL_SKIP, 0);
  if(rc) goto end;
  if( !s2_ttype_is_eof(prParens.token.ttype)){
    s2_ptoker_errtoken_set(pr, &prParens.token);
    rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                       "Extra junk after post-loop part of "
                       "of %s(...): got a %s.",
                       kw->word,
                       s2_ttype_cstr(prParens.token.ttype));
    goto end;
  }

  /* Now we know that the ( pre; condition; post ) part is well-formed */

  if(se->skipLevel>0){
    goto end;
  }

  /*
    We need two scopes here if we want to support var decls
    in both the prefix part and loop body. The first scope
    opens just before the (...) is run, the second is pushed
    before each loop iteration, before the condition is run,
    and popped after the post-loop is run.
  */
  /* oldRefCount = s2_scope_refs_count(se); */
  rc = cwal_scope_push2(se->e, &scopeOut);
  if(rc) goto end;
  /* No RETURNs as of here... */

  /* Run the prefix part */
  s2_ptoker_token_set( &prParens, &tPre );
  rc = s2_eval_expr_impl( se, &prParens, 0, 0, 0 );
  if(rc) goto end;

  /* Run the tBody while the tCondition evaluates
     to truthy... */
  while( !rc ){
    /* cwal_value * postRv = 0 */ /* a cleanup hack */
    cwal_value * condRv = 0;
    char condBuul;
    if(scopeIn.parent){
      /* Inner scope needs to be re-initialized on each
         iter.
      */
      cwal_scope_pop(se->e);
      assert(!scopeIn.parent);
    }
    assert(&scopeOut == se->scopes.current->cwalScope);
    if(0==(++runCount % 5 /* # arbitrarily chosen, the point being
                             only to not sweep on every iteration
                             (would be overkill).*/)){
      s2_engine_sweep(se)
        /* Outer scope: we need this to clean up overwritten loop vars
           declared in this scope, though i'm not sure why refcounting
           isn't doing it for us? Simple loop/metrics tests imply
           that it's not happening, anyway, or that refs are being
           held elsewhere. Specifically, it's easy to see with:

           var y = 100;
           for(var x = 0; x<y; ++x);
           s2.dumpMetrics();

           for various values of y. The metrics report more allocs than
           expected, starting when the recycling bin size for integers
           is (not incidentally!) y-1.

           Oh, i am pretty sure it leads back to: the ++ op (and
           similar ops) holds a ref to all the operands because
           overloads can do funny things with them. When it's done, it
           unhand()s them, which makes them available for sweep but
           does not destroy them outright.

           Later on... after having looked into that as the cause, i
           do see an extra reference whose origin i have not yet
           located.

           20160206: i don't believe that's the case anymore.
        */;
    }
    rc = cwal_scope_push2(se->e, &scopeIn);
    if(rc) break;
    assert(scopeIn.parent)

    /* Check the condition part */;
    prParens.token = tCondition;
    rc = s2_eval_expr_impl( se, &prParens, 0, 0, &condRv );
    if(rc) break;
    condBuul = condRv /* Treat empty expr as true here */
      ? cwal_value_get_bool(condRv)
      : 1;
    if(condRv){
      cwal_refunref(condRv);
      condRv = 0;
    }
    if(!condBuul) break;

    /* Run the body ... */;
    xrv = 0;
    s2_ptoker_token_set(pr, &tBody);
    rc = s2_eval_current_token(se, pr, bodyIsExpr, 0, 0);
    if(rc){
      if(CWAL_RC_BREAK==rc){
        /* fall through */
      }
      else if(CWAL_RC_CONTINUE==rc){
        rc = 0;
        s2_engine_err_reset(se);
      }
      else break;
    }
    /*
      Run the post part. By an accident of design, this runs while the
      per-iteration scope is active, meaning it can reference vars
      declared there. Feature or bug?
    */
    if(!rc){
      prParens.token = tPost;
      rc = s2_eval_expr_impl( se, &prParens, 0, 0, 0 /* &postRv */ );
    }
    if(CWAL_RC_BREAK==rc){
      /*
        There's little reason not to allow the tPost part to 'break'
        out.  Unconventional, yes, but so are keywords which resolve
        to values.
       */
        xrv = s2_propagating_take(se);
        assert(xrv && "else misuse of CWAL_RC_BREAK.");
        assert(scopeOut.parent);
        cwal_value_rescope(scopeOut.parent, xrv);
        rc = 0;
        cwal_value_ref(xrv);
        s2_engine_err_reset(se);
        break;
    }
#if 0
    /* 20160206: doesn't seem to be needed anymore. Tested with the
       example case described below and everything was cleaned up
       optimally. */
    if( postRv ){
      /* What this does: keeps lower-scope temporaries from
         surviving until the end of the loop:

         Without this hack:

         s2> var a=[100]
         result: array@0x104ca20[scope=#1@0x7fffdb851260 ref#=1] ==> [100]
         s2> for(a.0 = 100; a.0 > 0; a.0-- );
         MARKER: s2.c:264:s2_engine_sweep():	Swept up 99 value(s) in vacuum mode

         With this hack: no post-loop sweepup of those temps because
         they're cleaned up here. This only works (has a benefit) for
         the common case (a single var gets modified), not for a
         series of them. The comma op has a similar hack which cleans up
         the LHS side(s) of a comma-separated list of expressions.
      */
      cwal_refunref(postRv);
    }
#endif
  }

  end:
  if(scopeIn.parent){
    assert(&scopeIn == se->scopes.current->cwalScope);
    cwal_scope_pop(se->e);
    assert(!scopeIn.parent);
  }
  if(scopeOut.parent){
    assert(&scopeOut == se->scopes.current->cwalScope);
    cwal_scope_pop(se->e);
    assert(!scopeOut.parent);
  }
  if(!rc){
    s2_ptoker_token_set(pr, &tBody);
    cwal_value_unhand(xrv);
    *rv = se->skipLevel
      ? cwal_value_undefined()
      : (xrv ? xrv : cwal_value_undefined());
  }else{
    cwal_value_unref(xrv);
  }
  s2_ptoker_finalize( &prParens );
  return rc;
}


int s2_eval_filename( s2_engine * se, char pushScope,
                      char const * fname,
                      cwal_int_t fnlen,
                      cwal_value ** rv ){
  int rc;
  cwal_buffer buf = cwal_buffer_empty;
  cwal_value * xrv = 0;
  cwal_scope _SCOPE = cwal_scope_empty;
  cwal_scope * scope = pushScope ? &_SCOPE : 0;
  cwal_size_t const nFname = (fnlen<0) ? cwal_strlen(fname) : (cwal_size_t)fnlen;
  if(!se || !fname) return CWAL_RC_MISUSE;
  else if( !*fname || !nFname) return CWAL_RC_RANGE;
  if(rv) *rv = 0;
#if defined(S2_OS_UNIX)
  if(nFname>1 || '-'!=*fname
     /* ^^^ upcoming call will interpret that as stdin, so skip stat() */){
    /*
      Check if fname is a directory. Trying to eval a dir entry will
      lead to (in my experience) confusing CWAL_RC_OOM error (though
      other weird errors are also possible). We only have code for
      doing this check on platforms which support stat(). We could
      arguably also fail for other device types, e.g. BLOCK, but we'll
      let those stay for now because they haven't caused any grief in
      practice.
    */
    s2_fstat_t fst = s2_fstat_t_empty;
    rc = s2_fstat( fname, nFname, &fst, 1 );
    if(CWAL_RC_UNSUPPORTED==rc){
      /* Assume that stat() is not available in this build, so go
         ahead and try to read the given filename without stat()'ing
         it first. This is needed for non-stat()-aware builds to be
         able to eval any external scripts.*/
      rc = 0;
      /* fall through... */
    }
    else if(rc){
      return s2_engine_err_set(se, rc, "stat(\"%.*s\") failed.",
                               (int)nFname, fname);
    }
    else if(S2_FSTAT_TYPE_DIR == fst.type){
      return s2_engine_err_set(se, CWAL_RC_TYPE,
                               "Cannot eval a directory.");
    }
  }
#endif
  rc = cwal_buffer_fill_from_filename2( se->e, &buf, fname, nFname );
  if(rc){
    rc = s2_engine_err_set(se, rc, "%s: %s",
                           buf.used ? "Could not open" : "Could not read",
                           fname);
  }else{
    if(scope){
      rc = cwal_scope_push2(se->e, scope);
      if(!rc){
        assert(0==se->scopes.current->sguard.sweep);
        assert(0==se->scopes.current->sguard.vacuum);
      }
    }
    if(!rc){
      s2_ptoker pr = s2_ptoker_empty;
      rc = s2_ptoker_init_v2( se->e, &pr, (char const *)buf.mem,
                              (cwal_int_t)buf.used, 0 );
      if(!rc){
        pr.name = fname;
        rc = s2_eval_ptoker( se, &pr, 0, rv ? &xrv : 0 );
      }
      s2_ptoker_finalize(&pr);
      switch(rc){
        case CWAL_RC_INTERRUPTED:
        case CWAL_RC_EXCEPTION:
        case CWAL_RC_OOM:
        case CWAL_RC_EXIT:
        case CWAL_RC_FATAL:
        case CWAL_RC_ASSERT:
          break;
        case CWAL_RC_RETURN:
          assert(!"This cannot happen anymore: s2_eval_ptoker() will catch this.");
          rc = 0;
          xrv = s2_propagating_take(se);
          /* s2_engine_err_reset(se); */
          CWAL_SWITCH_FALL_THROUGH;
        case 0:
          if(scope && xrv && rv) cwal_value_rescope(scope->parent, xrv);
          if(rv) *rv = xrv;
          break;
        default:
          if(s2__err(se).code){
            /* Convert non-exception errors to exceptions so that
               eval'ing a sub-script does not kill the top-level
               script.
            */
            rc = s2_throw_err(se, 0, 0, 0, 0);
          }
          break;
      }
    }
    if(scope && scope->parent){
      cwal_scope_pop2(se->e, rc ? 0 : (rv ? *rv : 0));
    }
  }
  cwal_buffer_clear(se->e, &buf);
  /* MARKER(("se->err.script=%.*s\n", (int)se->err.script.used, (char const *)se->err.script.mem)); */

  if(!rc && rv){
    assert((!*rv || cwal_value_scope(*rv) || cwal_value_is_builtin(*rv))
           && "Seems like we've cleaned up a value too early.");
  }
  return rc;
}

/**
   The cwal_callback_f() impl used by script-side functions
   created by s2_keyword_f_function().
*/
static int s2_callback_f( cwal_callback_args const * args, cwal_value ** rv ){
  int rc = 0;
  s2_func_state * fst = s2_func_state_for_func(args->callee)
    /*(s2_func_state *)cwal_args_state(args, &s2_func_state_empty)*/
    ;
  s2_ptoker _pr = s2_ptoker_empty;
  s2_ptoker * pr = &_pr;
  s2_ptoken tBody = s2_ptoken_empty;
  s2_ptoken tParams = s2_ptoken_empty ;
  s2_engine * se = fst ? s2_engine_from_args(args) : 0;
  cwal_array * cbArgv = se ? se->callArgV : 0
    /* The array form of args->argv */
    ;
  char const * src = 0;
  cwal_size_t srcLen = 0;
  s2_ptoker const * oldScript = se->currentScript;
  s2_func_state const * oldScriptFunc = se->currentScriptFunc;
  se->callArgV = 0
    /* Necessary so that this array does not "leak" into certain
       nested call constructs. Triggered by calling an overloaded
       operator inside a script function - the _first_ set of call
       args were wrong (leaked in from a higher call).
    */;

  assert(fst);
  assert(se);
  assert(se->funcStash);

  if((fst->flags & S2_FUNCSTATE_F_EMPTY_PARAMS)
     && (fst->flags & S2_FUNCSTATE_F_EMPTY_BODY)){
    /* We have neither a body nor params, so let's
       go the easy route.

       One might think that we could do this if the body is empty but
       the parameter list is not, but default param values can have
       side-effects, so we need to evaluate those. We "could" do a
       quick scan for a '=', as side effects cannot (legally) happen
       without an assignment to a default param value. If one is not
       found, we could take the empty-params route. A
       micro-optimization, in any case.
    */
    /* assert(!fst->lastCallReturned); */
    assert(!fst->keyScriptName);
    assert(!fst->vSrc);
    *rv = cwal_value_undefined();
    return 0;
  }
  assert(cbArgv || (fst->flags & S2_FUNCSTATE_F_EMPTY_PARAMS));

  assert(fst->keyScriptName);
  assert(cwal_value_scope(fst->keyScriptName)||cwal_value_is_builtin(fst->keyScriptName));

  assert(fst->vSrc);
  assert(cwal_value_scope(fst->vSrc) || cwal_value_is_builtin(fst->vSrc));

  /* As of here, no 'return' - use goto end. */
  src = cwal_value_get_cstr(fst->vSrc, &srcLen);
  assert(src);
  assert(srcLen>=8);

  rc = s2_ptoker_init_v2( se->e, pr, src, (cwal_int_t)srcLen, 0 );
  if(rc) goto end;

  se->currentScriptFunc = fst;
  se->currentScript = pr;
  pr->name = cwal_value_get_cstr(fst->keyScriptName, 0);
  pr->lineOffset = fst->line;
  pr->colOffset = fst->col;
  /* MARKER(("line=%d, col=%d\n", pr->lineOffset, pr->colOffset)); */
  /* s2_dump_val(fst->keyScriptName, "fst->keyScriptName"); */
  /* MARKER(("Calling func from script [%s]\n", pr->name)); */

  /* Skip over keyword */
  rc = s2_next_token( se, pr, 0, 0 );
  /* assert(!rc && "Should have failed at decl time!"); */
  if(rc) goto end /* we know the tokens are valid, but interruption
                     can trigger this */;

  /* Get the params */
  rc = s2_next_token( se, pr, 0, 0 );
  if(rc) goto end;
  else if(S2_T_Identifier==pr->token.ttype){ /* The name part */
    rc = s2_next_token( se, pr, 0, 0 );
    if(rc) goto end;
  }
  tParams = pr->token;
  assert(S2_T_ParenGroup==tParams.ttype);

  /* The body... */
  rc = s2_next_token( se, pr, 0, 0 );
  if(rc) goto end
           /* likely an interruption error, not tokenization (which we
              know works because it was parsed at decl-time). Yes, it
              happened when testing s2_interrupt().
           */;
  tBody = pr->token;
  if(S2_T_Identifier==tBody.ttype){
    /* Skip over using(...) resp. using{...} */
    assert((5U == s2_ptoken_len(&tBody)) && "Not 'using'?");
    rc = s2_next_token(se, pr, 0, 0);
    if(rc) goto end;
    if(S2_FUNCSTATE_F_NO_USING_DECL & fst->flags){
      assert(S2_T_OpDot==pr->token.ttype);
      rc = s2_next_token(se, pr, 0, 0);
      if(rc) goto end;
    }
    assert(S2_T_ParenGroup==pr->token.ttype
           || S2_T_SquigglyBlock==pr->token.ttype);
    rc = s2_next_token(se, pr, 0, 0);
    if(rc) goto end;
    tBody = pr->token;
  }
  assert(S2_T_SquigglyBlock==tBody.ttype);

  assert(s2_ptoker_begin(pr) < s2_ptoken_begin(&tParams));
  assert(s2_ptoker_begin(pr) < s2_ptoken_begin(&tBody));
  assert(s2_ptoker_end(pr) > s2_ptoken_end(&tParams));
  assert(s2_ptoker_end(pr) >= s2_ptoken_end(&tBody));
  /* fst->lastCallReturned = 0; */
  if(!(fst->flags & S2_FUNCSTATE_F_EMPTY_PARAMS)){
    /* Collect parameter list */
    s2_ptoker sub = s2_ptoker_empty;
    s2_ptoker_token_set(pr, &tParams);
    rc = s2_ptoker_sub_from_toker(pr, &sub);
    /* MARKER(("PARAMS: %.*s\n", (int)s2_ptoken_len(&tParams),
       s2_ptoken_begin(&tParams))); */
    if(!rc){
      rc = s2_keyword_f_var_impl( s2_ttype_keyword(S2_T_KeywordFunction),
                                  se, &sub, 0, 0, cbArgv );
    }
    s2_ptoker_finalize(&sub);
    /* s2_dump_val(cwal_array_value(cbArgv), "func->argv"); */
    if(rc) goto end;
  }
  /* MARKER(("CALLING:\n%.*s\n", (int)srcLen, cwal_string_cstr(srcStr))); */
  if(!(fst->flags & S2_FUNCSTATE_F_EMPTY_BODY)){
    assert(!rc);
    s2_ptoker_token_set(pr, &tBody);
    rc = s2_eval_current_token( se, pr, 0, 0, NULL );
  }
  switch(rc){
    case 0:
      /* function ended without an explicit return. */
      *rv = cwal_value_undefined();
      break;
    case CWAL_RC_RETURN:
      rc = 0;
      *rv = s2_propagating_take(se);
      s2_engine_err_reset(se);
      assert(*rv && "s2_propagating_set() must be used with CWAL_RC_RETURN");
      if(!*rv) rc = s2_err_ptoker(se, pr, CWAL_RC_ASSERT,
                                  "internal misuse: only return CWAL_RC_RETURN "
                                  "in conjunction with s2_propagating_set().");
      break;
    case CWAL_RC_ASSERT:
    case CWAL_RC_CANNOT_HAPPEN:
    case CWAL_RC_EXCEPTION:
    case CWAL_RC_EXIT:
    case CWAL_RC_FATAL:
    case CWAL_RC_INTERRUPTED:
    case CWAL_RC_OOM:
    case CWAL_SCR_SYNTAX:
      break;
    case CWAL_RC_BREAK:
      /* Cosmetic: improve error messages for break/continue...  We
         really need this handling in a higher-level place, but to get
         a good stack trace we have to not BREAK/CONTINUE here.
      */
      assert(s2_propagating_get(se)
             && "s2_propagating_set() must be used with CWAL_RC_BREAK");
      s2_propagating_set(se, 0);
      CWAL_SWITCH_FALL_THROUGH /* ... to 'continue' */;
    case CWAL_RC_CONTINUE:
      rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                         "Unhandled '%s' outside of a loop.",
                         (CWAL_RC_BREAK==rc) ? "break" : "continue");
      break;
    default:
      if(s2__err(se).code == rc){
        /*MARKER(("se->err.code=%s: %.*s\n", cwal_rc_cstr(se->err.code),
          (int)se->err.msg.used, (char *)se->err.msg.mem));*/
        /* Leave it! */
        /* rc = s2_err_ptoker(se, pr, rc, 0, 0); */
      }else{
        rc = s2_throw_ptoker(se, pr, rc,
                             "Script function call returned "
                             "non-exception error code #%d (%s).",
                             rc, cwal_rc_cstr(rc));
      }
      break;
  }
  end:
    /* s2_dump_val(*rv, "call result"); */
  s2_ptoker_finalize( pr );
  se->currentScriptFunc = oldScriptFunc;
  se->currentScript = oldScript;
  return rc;
}

/**
   cwal_callback_hook_pre_f() impl which installs the following
   vars in argv->scope:

   this
   argv
   funcNameIfSetInFuncDecl

   and symbols imported via Function.importSymbols() or the
   'using' keyword.

   It does not set up named parameters - those are set up before the
   Function is call()ed, and this callback is triggered from
   cwal_function_call_in_scope().

   s2_engine_from_state(args) must return a (s2_engine*) or an
   assertion may be triggered (or a NULL pointer deref).
*/
int s2_callback_hook_pre(cwal_callback_args const * args,
                         void * stateUnused){
  s2_func_state * fs = args->callee
    ? s2_func_state_for_func(args->callee)
    : NULL;
  S2_UNUSED_ARG stateUnused;
  /* MARKER(("PRE-FUNC HOOK fstate=%p argc=%d!\n", (void const *)fs, (int)args->argc)); */
  if(
     !fs /* It's a native function */
     || (/* A function with neither body content nor params. */
         fs->flags & S2_FUNCSTATE_F_EMPTY_BODY
         && fs->flags & S2_FUNCSTATE_F_EMPTY_PARAMS)
     ){
    s2_engine * se = s2_engine_from_state(args->engine);
    assert(se);
    se->callArgV = 0 /* Make sure this doesn't propagate through to a script
                        func that this native function calls. Been
                        there, debugged that! */;
    /* MARKER(("Non-script or empty function. Not injecting argv/this.\n")); */
    s2_dotop_state( se, 0, 0, 0 ) /* necessary!!! */;
    return 0;
  }
  else{
    /* Set up argv, callee name, "this", and imported symbols. */
    int rc = 0;
    cwal_array * ar = 0;
    cwal_value * av = 0;
    cwal_value * calleeV = 0;
    cwal_scope * s = args->scope;
    s2_engine * se = s2_engine_from_args(args);
    assert(fs && "Currently always true here.");
    assert(se);
    calleeV = cwal_function_value(args->callee);
    assert(calleeV);
    s2_dotop_state( se, 0, 0, 0 ) /* necessary!!! */;

    assert(!(/* empty function */
             fs->flags & S2_FUNCSTATE_F_EMPTY_BODY
             &&
             fs->flags & S2_FUNCSTATE_F_EMPTY_PARAMS)
           && "optimizations should have skipped this call() overhead"
           );
    ar = se->callArgV;
    /* s2_dump_val(cwal_array_value(ar),"ar"); */
    if(ar){
      /* This call is EITHER:

         1) coming from a script-parsed call() point
         about to call into s2_callback_f().

         2) from a new T() call.

         In either case, we'll re-use this argv array.
      */
      av = cwal_array_value(ar);
      assert(cwal_value_refcount(av) && "Expecting a ref from our caller's end.");
      cwal_value_ref(av);
    }else{
      /* This is probably being called from C code, so we need to inject
         an argv array. */
      int i = (int)args->argc;
      /* MARKER(("Script func called from C code? argc=%d\n", i)); */
      ar = cwal_new_array(args->engine);
      if(!ar){
        return CWAL_RC_OOM;
      }
      av = cwal_array_value(ar);
      cwal_value_ref(av);
      for( ; !rc && i > 0; --i ){
        /* insert in reverse order as a reallocation optimization */
        /* s2_dump_val(args->argv[i-1],"args->argv[i-1]"); */
        rc = cwal_array_set(ar, (cwal_size_t)i-1, args->argv[i-1]);
      }
      if(rc){
        cwal_value_unref(av);
        return rc;
      }
      se->callArgV = ar /* i don't like this, but currently needed */;
    }
    if(0){
      MARKER(("Function call() argc=%d...\n", (int)args->argc));
      s2_dump_val(args->self, "args->self");
      s2_dump_val(av, "argv");
    }

    /* s2_dump_val(cwal_array_value(ar),"args array"); */
    rc = s2_var_set_v(se, 0, se->cache.keyArgv, av);
    cwal_value_unref(av);
    if(rc) return rc;

    /* Inject function's name (if any)... */
    if( fs->vName
        &&
        (rc = cwal_scope_chain_set_with_flags_v(s, 0, fs->vName, calleeV,
                                                CWAL_VAR_F_CONST)) ){
      return rc;
    }
    /* Inject "this"... */
    rc = s2_var_set_v( se, 0, se->cache.keyThis, args->self
                       ? args->self
                       : cwal_value_undefined())
      /* Should we use var_decl instead and make it const?  So
         far no need, and i have a vague memory of that approach
         causing grief when that topic came up before.
      */;
    if(rc) return rc;
#if S2_TRY_INTERCEPTORS
    else if(args->propertyHolder){
      /* s2_dump_val(args->propertyHolder, "interceptee"); */
      rc = s2_var_set_v( se, 0, se->cache.keyInterceptee, args->propertyHolder );
      if(rc) return rc;
    }
#endif
    if(fs
       && fs->vImported
       && !(S2_FUNCSTATE_F_NO_USING_DECL & fs->flags)){
      /* Import imported properties (using/Function.importSymbols())
         into the current scope.
      */
      rc = cwal_scope_import_props( s, fs->vImported );
    }
    return rc;
  }
}

int s2_callback_hook_post(cwal_callback_args const * args,
                          void * state,
                          int fRc, cwal_value * rv){
  int rc = 0;
  s2_engine * se = s2_engine_from_state(args->engine);
  /* s2_func_state * fs = (s2_func_state *)cwal_args_callee_state(args, &s2_func_state_empty); */
  if(state || fRc || rv){/*avoid unused param warning*/}
  assert(se);
  /* MARKER(("Callback post-hook\n")); */
  /* s2_dump_val(cwal_array_value(se->callArgV), "se->callArgV"); */
  se->callArgV = 0
    /* required for certain calling combinations of native vs script funcs
       to work. */;
  /**
     Reminder to self:

     We can potentially use this callback to communicate certain
     non-zero results back up the call chain, e.g. CWAL_RC_OOM could
     be stuffed into a dedicated error code propagation slot (the plan
     is to consolidate return/exit/etc. with the
     s2_engine::flags::interrupted for that purpose).
  */
#if 0
  if(!fs) return 0;
  if(fRc && fs){
    /* Script function: if it threw an exception with no location info,
       let's ammend the location info now where we have the input
       script source. */
    cwal_value * ex = cwal_exception_get(args->engine):
    assert(ex);
    if(!cwal_prop_get(ex, "line", 4)){
      rc = s2_ammend_script_func_exception(se, fs, ex);
    }
  }
#endif
  return rc;
}
    

/**
   Internal helper to populate s2_func_state on behalf of a function
   declation.
*/
static int s2_function_setup_srcinfo( s2_engine * se, s2_ptoker const * pr,
                                      s2_ptoken const * tOrigin,
                                      s2_ptoken const * tName,
                                      char const * endPos,
                                      s2_func_state * fst,
                                      cwal_value * func ){
  int tlen = (int)(endPos - s2_ptoken_begin(tOrigin));
  int rc = 0;
  cwal_hash * h = s2_fstash(se);
  cwal_value * v = 0;
  cwal_size_t nameLen = 0;
  char const * scriptName = s2_ptoker_name_first(pr, &nameLen);
  assert(endPos >= s2_ptoken_begin(tOrigin)+8
         && endPos <= s2_ptoker_end(pr)
         /* 8 == strlen("proc(){}"), shortest possible function
            decl */);
  if(!h){
    return CWAL_RC_OOM;
  }
  assert(tlen >=8 /*proc(){}*/);
  /* MARKER(("setting up srcinfo for:\n%.*s\n", tlen,
     s2_ptoken_begin(tOrigin))); */

  /* Calculate script/line/column info, as we need those for
     error reporting. */
  {
    s2_ptoker_count_lines( pr, s2_ptoken_begin(tOrigin),
                           &fst->line, &fst->col );
  }
  /* currenly used as the mechanism for holding a func's name. */
  if(s2_ptoken_begin(tName)){
    assert(s2_ptoken_end(tName) > s2_ptoken_begin(tName));
    assert(!fst->vName);
    rc = s2_ptoken_create_value( se, pr, tName, &fst->vName );
    if(rc) return rc;
    cwal_value_ref(fst->vName);
    s2_value_to_lhs_scope(func, fst->vName);
  }
  if(fst->vImported
     || !(fst->flags & S2_FUNCSTATE_F_EMPTY_PARAMS
          && fst->flags & S2_FUNCSTATE_F_EMPTY_BODY)
     ){
    /* For "empty" functions (no non-junk tokens),
       we don't really need any of these string bits,
       as they're only used for error reporting...
    */
    v = cwal_new_string_value(se->e, s2_ptoken_begin(tOrigin),
                              (cwal_size_t)tlen);
    if(!v){
      return CWAL_RC_OOM;
    }
    fst->vSrc = v;
    cwal_value_ref(fst->vSrc);
    s2_value_to_lhs_scope( func, fst->vSrc );
    if(scriptName && *scriptName){
      v = cwal_hash_search( h, scriptName, nameLen );
      if(v){
        ++se->metrics.totalReusedFuncStash;
      }else{
        v = cwal_new_string_value(se->e, scriptName, nameLen);
        if(!v){
          return CWAL_RC_OOM;
        }
        cwal_value_ref(v);
        rc = cwal_hash_insert_v( h, v, v, 0 );
        if(!rc) rc = cwal_hash_grow_if_loaded(h, 0.8);
        cwal_value_unref(v);
        if(rc) return rc;
        /* hashtable now holds a ref to v (or v is a builtin)  */;
      }
      fst->keyScriptName = v;
      cwal_value_ref(fst->keyScriptName);
      /* s2_dump_val(v, "fst->keyScriptName"); */
    }
  }
  return 0;
}

/**
   A cwal_value_rescoper_f() intended for use with
   cwal_function instances holding s2_func_state native
   data.
*/
static int cwal_value_rescoper_f_func_state(cwal_scope * s,
                                            cwal_value * v){
  cwal_function * f = cwal_value_get_function(v);
  s2_func_state * fst =  s2_func_state_for_func(f);
  assert(f);
  assert(fst);
  if(fst->vSrc) cwal_value_rescope(s, fst->vSrc);
  if(fst->vImported) cwal_value_rescope(s, fst->vImported);
  if(fst->vName) cwal_value_rescope(s, fst->vName);
  if(fst->keyScriptName) cwal_value_rescope(s, fst->keyScriptName);
  return 0;
}


/**
   Internal helper to process the using(...) part of: proc()
   using(...) {}, resp. the {object} part of ... using {object} ...

   It stashes imported properties into fst->vImported.

   pr_->token must be the (...) or {object} token. fv must be the
   Function Value and fst must be fv's state.

   Returns 0 on success, and all that.

   Ownership/lifetime of se, fv, and fst are not modified.

   If passed an object literal, it imports all properties of
   that literal into the function. If passed (...) then:

   1) Any identifiers in that list are resolved immediately and
   that identifier's name/value are imported into the function.

   2) Any object literals are treated as mentioned above.


   This handling leads to a descrepancy with importSymbols(), which
   is best demonstrated with an example:

   const with = proc(obj, func){return proc(){return func()}.importSymbols(obj)()};
   assert 3 === with({a: 1, b:2}, proc(){return a+b});

   The difference is that 'using' currently (20171112) has no syntax
   for saying "import the properties of this non-literal object".

   One easy-to-implement approach would be to extend the (...) syntax
   just a small bit:

   using (->nonLiteralObject1, nonLiteralObject2)

   such that the first one would, due to the -> modifier, import the
   symbols of that object, whereas the second one would be imported
   as-is.
*/
static int s2_function_using( s2_engine * se, s2_ptoker const * pr_,
                              cwal_value * fv, s2_func_state * fst ){
  s2_ptoken next = s2_ptoken_empty;
  cwal_value * v = 0;
  int rc;
  char gotComma;
  cwal_size_t argPos = 0;
  s2_ptoker sub = s2_ptoker_empty;
  s2_ptoker * pr = &sub /* simplifies some copy/paste reuse of code :/ */;
  s2_ptoken const tUsing = pr_->token;
  cwal_value * importHolder = 0;
  s2_op const * commaOp = s2_ttype_op(S2_T_Comma);
  /* if(!importHolder) return CWAL_RC_OOM; */
  assert(cwal_value_is_function(fv));
  assert(commaOp);
  if(S2_T_SquigglyBlock==tUsing.ttype){
    /* using {object} ... */
    sub = *pr_;
    rc = s2_eval_object_literal( se, pr, &v );
    if(rc){
      assert(!v);
    }else{
      assert(v);
      cwal_value_ref(v);
      if(!cwal_value_is_object(v)){
        rc = s2_throw_ptoker(se, pr, CWAL_RC_TYPE,
                             "Expecting an Object literal but got a %s.",
                             cwal_value_type_name(v));
        cwal_value_unref(v);
      }
      else if(!fst->vImported){
        fst->vImported = v /* fst now owns the ref point */;
        cwal_value_prototype_set(v, NULL);
        s2_value_to_lhs_scope(fv, v);
      }else{
        rc = cwal_props_copy(v, fst->vImported);
        cwal_value_unref(v);
      }
      v = 0;
    }
    goto end;
  }/* end of {object}*/
  /* Else using (...) ... */
  assert(S2_T_ParenGroup == tUsing.ttype);
  assert(s2_ptoken_len(&tUsing) >= 3
         && "(...) emptiness was checked earlier");
  importHolder = s2_func_import_props(se, fv, fst);
  if(!importHolder) return CWAL_RC_OOM;
  rc = s2_ptoker_sub_from_toker(pr_, &sub);
  if(rc) return rc;
  next_part:
  gotComma = 0;
  v = 0;
  rc = s2_next_token( se, pr, 0, 0 );
  if(rc) goto end;
  else if(S2_T_Identifier==pr->token.ttype){
    if(!se->skipLevel){
      s2_ptoken const ident = pr->token;
      if(s2_ptoken_keyword2(se, &ident)){
        /* confirm that it's not a keyword, as the user would not
           be able to use such an imported var.
        */
        rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                           "Cannot use keyword '%.*s' "
                           "as a 'using' identifier.",
                           (int)s2_ptoken_len(&ident),
                           s2_ptoken_begin(&ident));
        goto end;
      }
      s2_get(se, 0, s2_ptoken_begin(&ident), s2_ptoken_len(&ident), &v);
      if(!v){
        rc = s2_err_ptoker(se, pr, CWAL_RC_NOT_FOUND,
                           "Cannot resolve identifier '%.*s'.",
                           (int)s2_ptoken_len(&ident),
                           s2_ptoken_begin(&ident));
        goto end;
      }
      /* Import identifier's value... */
      cwal_value_ref( v );
      rc = cwal_prop_set( importHolder, s2_ptoken_begin(&ident),
                          s2_ptoken_len(&ident), v);
      cwal_value_unref( v );
      if(rc) goto end;
    }
  }else{
    /* See if we got an Object we can import props from... */
    s2_ptoker_putback(pr) /* for pending eval... */;
    rc = s2_eval_expr_impl(se, pr, commaOp, 0, &v);
    if(rc){
      assert(!v);
      goto end;
    }else if(!se->skipLevel){
      if(!v){
        rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                           "An empty expression is not allowed here.");
        goto end;
      }else if(!cwal_props_can(v)){
        rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                           "Unexpected '%s' value in 'using' list. "
                           "Expecting identifier or object.",
                           cwal_value_type_name(v));
        cwal_refunref(v);
        goto end;
      }else{
        cwal_value_ref(v);
        rc = cwal_props_copy(v, importHolder);
        cwal_value_unref(v);
        if(rc) goto end;
      }
    }else{
      assert(cwal_value_undefined()==v
             && "Violation of current skip-mode conventions.");
    }
  }
  /* Check for trailing comma or EOX */
  rc = s2_next_token( se, pr, 0, &next);
  if(rc) goto end;
  switch(next.ttype){
    case S2_T_Comma:
      gotComma = 1;
      CWAL_SWITCH_FALL_THROUGH;
    case S2_T_EOF:
    case S2_T_EOX:
      s2_ptoker_token_set(pr, &next) /* consume it */;
      break;
    default:
      s2_ptoker_errtoken_set(pr, &next);
      rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                         "Unexpected token '%.*s' in 'using' list.",
                         (int)s2_ptoken_len(&next),
                         s2_ptoken_begin(&next));
      goto end;
  }
  assert(!rc);
  if(!se->skipLevel){
    ++argPos;
  }
  if(!s2_ptoker_is_eof(pr) && !gotComma){
    /* Check for a follow-up token */
    if(s2_ptoker_next_is_ttype(se, pr, S2_NEXT_NO_POSTPROCESS, S2_T_Comma, 1)){
      gotComma = 1;
    }
  }
  assert(!rc);
  rc = s2_check_interrupted(se, rc);
  if(!rc && gotComma) goto next_part;
  end:
  s2_ptoker_finalize(&sub);
  return rc;
}

/**
   Part of s2_keyword_f_function():

   Requires that curToken->ttype==S2_T_Identifier and be the token at
   the X part of either (proc() X) or (proc(){} X). stage must be -1
   for former case and 1 for the latter. Both of these conditions are
   assert()ed.

   On error, non-0 is returned and s2_keyword_f_function() must
   propagated it.

   On success 0 is returned.

   If -1==stage:

   - An error is triggered if curToken is NOT the identifier "using"...

   - else *tUsing is set to the "using" clause's body: (...) or
   {...}.

   If 1==stage:

   - If curToken is an identifier other than "using", this function
   has no side effects and 0 is returned: this indicates that whatever
   follows the function body is not part of the function definition
   (it might be an error, but not one which we have enough information
   to diagnose from here). Neither tUsing, fsFlags, nor pr->token are
   modified in that case. If curToken is the "using" identifier then,
   on success, *tUsing is set to the "using" clause's body token:
   (...) or {...}.

   Both cases, on success:

   - If the "using" clause was followed immediately by a dot,
   *fsFlags gets OR'd with S2_FUNCSTATE_F_NO_USING_DECL.

   - pr->token is set to the last-consumed token (the body part of
   the "using" clause).
 */
static int s2_keyword_f_function_using(s2_keyword const * kw,
                                       s2_engine * se, s2_ptoker * pr,
                                       int stage, s2_ptoken const * curToken,
                                       s2_ptoken * tUsing,
                                       int * fsFlags){
  s2_ptoken check = s2_ptoken_empty;
  int rc = 0;
  int gotDot = 0;
  assert(S2_T_Identifier==curToken->ttype);
  assert(-1==stage || 1==stage);
  if(5 != s2_ptoken_len(curToken)
     || 0 != cwal_compare_cstr("using", 5, s2_ptoken_begin(curToken), 5)){
    if(1==stage){
      /* proc(){body} <curToken>: we do not/cannot know, from here, if
         this is legal, so we do not consume this token, leaving it
         for downstream to deal with. */
      return 0;
    }else{
      s2_ptoker_errtoken_set(pr, curToken);
      return s2_err_ptoker( se, pr, CWAL_SCR_SYNTAX,
                            "Unexpected token (type %s) after %s(...)%s.",
                            s2_ttype_cstr(curToken->ttype), kw->word,
                            1==stage ? "{...}" : ""
                            );
    }
  }
  /* Grab the (...) resp. {...} part... */
  s2_ptoker_token_set(pr, curToken)/*consume it*/;
  rc = s2_next_token( se, pr, 0, &check );
  if(rc) return rc;

  if(S2_T_OpDot == check.ttype){
    /* proc() using <PERIOD> ... indicates that we should not
       declare using/imported symbols as local symbols at
       call()-time. They can instead be accessed via the using()
       keyword. */
    /*fFlags |= S2_FUNCSTATE_F_NO_USING_DECL;*/
    s2_ptoker_token_set(pr, &check)/*consume it*/;
    rc = s2_next_token(se, pr, 0, &check);
    if(rc) return rc;
    ++gotDot;
  }

  /* Expecting a (...) or {...} part... */
  if(S2_T_ParenGroup != check.ttype
     && S2_T_SquigglyBlock != check.ttype){
    s2_ptoker_errtoken_set(pr, &check);
    return s2_err_ptoker( se, pr, CWAL_SCR_SYNTAX,
                          "Expecting (...) or {object} after 'using%s', "
                          "but got token type %s. Sub-parse stage: %d.",
                          gotDot ? "." : "",
                          s2_ttype_cstr(check.ttype), stage);
  }
  *tUsing = check /* the (...) or {...} part */;
  s2_ptoker_token_set(pr, &check)/*consume it*/;
  if(gotDot) *fsFlags |= S2_FUNCSTATE_F_NO_USING_DECL;
  return 0;
}

int s2_keyword_f_function( s2_keyword const * kw, s2_engine * se,
                           s2_ptoker * pr, cwal_value **rv){
  s2_ptoken tParams = s2_ptoken_empty /* function params list */;
  s2_ptoken tBody = s2_ptoken_empty /* function body */;
  s2_ptoken tName = s2_ptoken_empty /* function name */;
  s2_ptoken tUsing = s2_ptoken_empty /* the (...) part of: using (...) */;
  s2_ptoken tTail = s2_ptoken_empty /* either tBody or tUsing, whichever appears last */;
  s2_ptoken const tOrigin = pr->token;
  int rc;
  cwal_function * cf = 0;
  cwal_value * vf = 0;
  s2_func_state * fst = 0;
  int fFlags = 0;
  assert(S2_T_KeywordFunction==pr->token.ttype
         || S2_T_KeywordProc==pr->token.ttype);
  rc = s2_next_token( se, pr, 0, &tParams );
  if(rc) goto end;
  if(S2_T_Identifier==tParams.ttype){
    tName = tParams;
    s2_ptoker_token_set(pr, &tParams);
    rc = s2_next_token( se, pr, 0, &tParams );
    if(rc) goto end;
  }
  if(S2_T_ParenGroup != tParams.ttype){
    rc = s2_err_ptoker( se, pr, CWAL_SCR_SYNTAX,
                        "Expecting [identifier](...) after '%s', "
                        "but got token type %s.",
                        kw->word,
                        s2_ttype_cstr(tParams.ttype));
    goto end;
  }
  s2_ptoker_token_set(pr, &tParams);
  if(!s2_ptoken_has_content(&pr->token)){
    fFlags |= S2_FUNCSTATE_F_EMPTY_PARAMS;
  }
  /*
    TODO? - skip-eval the params and body for syntactic correctness
    here.  This would also let us count the arity (for potential use
    in function dispatching).

    TODO: consolidate the two checks for the 'using' modifier.
  */

  rc = s2_next_token( se, pr, 0, &tBody );
  if(rc) goto end;
  if(S2_T_Identifier == tBody.ttype){
    /* check for using(...) _before_ function body... */
    assert(!s2_ptoken_begin(&tUsing));
    rc = s2_keyword_f_function_using(kw, se, pr, -1,
                                     &tBody, &tUsing, &fFlags);
    if(rc) goto end;
    assert(s2_ptoken_begin(&tUsing));
    rc = s2_next_token( se, pr, 0, &tBody );
    if(rc) goto end;
  }

  /* Get/check the body... */
  if(S2_T_SquigglyBlock != tBody.ttype){
    s2_ptoker_errtoken_set(pr, &tBody);
    rc = s2_err_ptoker( se, pr, CWAL_SCR_SYNTAX,
                        "Expecting {...} after %s(...), "
                        "but got token type %s.",
                        kw->word,
                        s2_ttype_cstr(tBody.ttype));
    goto end;
  }else if(!s2_ptoken_has_content(&tBody)){
    fFlags |= S2_FUNCSTATE_F_EMPTY_BODY;
  }
  s2_ptoker_token_set(pr, &tBody) /* consume it */;
  if(!s2_ptoken_begin(&tUsing)){
    /* Check for using(...) _after_ the body.  */
    s2_ptoken check = s2_ptoken_empty;
    rc = s2_next_token(se, pr, 0, &check);
    if(rc) goto end;
    else if(S2_T_Identifier==check.ttype){
      rc = s2_keyword_f_function_using(kw, se, pr, 1,
                                       &check, &tUsing, &fFlags);
      if(rc) goto end;
    }else{/* Roll it back, let downstream deal with it. */
      assert(s2_ptoken_begin(&pr->token) == s2_ptoken_begin(&tBody));
      /*s2_ptoker_token_set(pr, &tBody);*/
      s2_ptoker_next_token_set(pr, &check);
    }
  }
  assert(!rc);
  
  if(S2_T_ParenGroup==tUsing.ttype
     && !s2_ptoken_has_content(&tUsing)){
    s2_ptoker_errtoken_set(pr, &tUsing);
    rc = s2_err_ptoker( se, pr, CWAL_SCR_SYNTAX,
                        "using (...) may not be empty.");
    /* We explicitly disallow using(<EMPTY>) but we do allow
       using{<EMPTY OBJECT>} because (A) that {} is parsed at a
       different level and (B) it turns out, with the addition of the
       using() keyword, to be potentially useful to have an empty
       imports object installed.
    */
    goto end;
  }

  tTail = (s2_ptoken_begin(&tUsing)
           && s2_ptoken_begin(&tUsing) > s2_ptoken_begin(&tBody))
    ? tUsing : tBody;

  if(se->skipLevel){
    *rv = cwal_value_undefined();
    rc = 0;
    s2_ptoker_token_set(pr, &tTail);
    goto end;
  }

  /*MARKER(("Got func:\n%.*s\n",
          (int)(s2_ptoken_end(&tBody) - s2_ptoken_begin(&tOrigin)),
          s2_ptoken_begin(&tOrigin)));*/
  *rv = cwal_value_undefined();

  fst = s2_func_state_malloc(se);
  if(!fst){
    rc = CWAL_RC_OOM;
    goto end;
  }
  cf = cwal_new_function( se->e, s2_callback_f, fst,
                          cwal_finalizer_f_func_state,
                          &s2_func_state_empty);
  if(!cf){
    s2_func_state_free( se, fst );
    rc = CWAL_RC_OOM;
    goto end;
  }
  /* As of here fst belongs to vf. */
  cwal_function_set_rescoper( cf, cwal_value_rescoper_f_func_state );
  fst->flags = fFlags;
  vf = cwal_function_value(cf);
  cwal_value_ref(vf);
  if(S2_T_INVALID != tUsing.ttype){
    s2_ptoken const tmp = pr->token;
    assert(S2_T_ParenGroup==tUsing.ttype
           || S2_T_SquigglyBlock == tUsing.ttype);
    s2_ptoker_token_set(pr, &tUsing);
    rc = s2_function_using( se, pr, vf, fst );
    s2_ptoker_token_set(pr, &tmp);
    if(rc) goto end;
  }
  assert(!rc);
  rc = s2_function_setup_srcinfo( se, pr, &tOrigin,
                                  &tName, s2_ptoken_end(&tTail),
                                  fst, vf );
  if(rc) goto end;

  *rv = vf;
  assert(s2_ptoken_begin(&tTail));
  s2_ptoker_token_set(pr, &tTail);
  goto end;

  end:
  if(vf){
    if(rc) cwal_value_unref(vf);
    else{
      assert(*rv == vf);
      cwal_value_unhand(vf);
    }
  }
  return rc;
}

int s2_ctor_apply( s2_engine * se, cwal_value * operand,
                   cwal_function * ctor, cwal_array * args,
                   cwal_value **rv ){
  cwal_value * newThis;
  int rc;
  cwal_scope sc = cwal_scope_empty;
  *rv = 0;
  if(!se || !operand || !rv) return CWAL_RC_MISUSE;
  else if(!ctor){
    rc = s2_ctor_fetch( se, NULL, operand, &ctor, 1 );
    if(rc) return rc;
  }
  rc = cwal_scope_push2(se->e, &sc);
  if(rc) return rc;
  if(args) cwal_value_ref(cwal_array_value(args));
  newThis = cwal_new_object_value(se->e);
  if(!newThis){
    rc = CWAL_RC_OOM;
    goto end;
  }
  cwal_value_ref(newThis);
  rc = s2_set_v( se, newThis, se->cache.keyPrototype, operand)
    /* instead of cwal_value_prototype_set(newThis, operand) so that
       we get consistent error reporting (via s2_set_v(), as opposed
       to an unadorned error code via the lower-level function). */;
  if(!rc){
    cwal_value * ctorResult = 0 /* the ctor result */;
    cwal_array * oldArgV = se->callArgV
      /* not _actually_ sure this is needed, but there might
         be a corner case or two without it. */;
    uint16_t const vFlags
      = cwal_container_client_flags_set( newThis, S2_VAL_F_IS_NEWING );
    se->callArgV = args;
    rc = args
      ? cwal_function_call_array(&sc, ctor, newThis, &ctorResult, args)
      : cwal_function_call_in_scope(&sc, ctor, newThis, &ctorResult,
                                    0, NULL)
      ;
    cwal_container_client_flags_set( newThis, vFlags );
    assert(!se->callArgV && "callArgV Gets unset via the call() hook(s)");
    se->callArgV = oldArgV;
    if(!rc && ctorResult
       && newThis != ctorResult
       && cwal_value_undefined() != ctorResult){
      /*
        If the ctor returns a non-undefined value, use that as the
        result of the construction.  It's up to the caller to set
        the prototype in that case, but he can fetch is via the
        ctor's args->self. This approach is needed when the client
        wants/needs to return a type other than Object.
      */
      /* s2_dump_val(ctorResult,"ctorResult"); */
      /* s2_dump_val(newThis,"newThis"); */
      cwal_value_ref(ctorResult);
      cwal_value_unref(newThis);
      newThis = ctorResult;
    }
  }
  end:
  assert(se->scopes.current->cwalScope == &sc);
  cwal_scope_pop2(se->e, newThis);
  assert(se->scopes.current->cwalScope != &sc);
  if(rc){
    cwal_value_unref(newThis);
  }else{
    assert(newThis);
    *rv = newThis;
    cwal_value_unhand(newThis);
  }
  if(args) cwal_value_unref(cwal_array_value(args));
  return rc;
}

/**
   Impl for the "new" keyword:

   new Operand(...args...) {optional post-ctor init block}

   Evaluates to the result of the Operand's constructor call (a new
   Object by default unless that ctor returns a non-undefined
   value). In skip mode, it always evaluates to the undefined value.

   On success pr will be set up such that the next token fetched will
   be the one right after the (...args...) or {post-ctor block}.
*/
int s2_keyword_f_new( s2_keyword const * kw, s2_engine * se,
                      s2_ptoker * pr, cwal_value **rv){
  int rc;
  cwal_size_t operandLen = 0;
  char const * operandStr = 0;
  cwal_value * operand = 0;
  cwal_function * ctor = 0;
  cwal_array * args = 0;
  cwal_value * vargs = 0;
  s2_ptoker ptArgs = s2_ptoker_empty;
  s2_ptoken tTail = s2_ptoken_empty;
  s2_ptoken tWithThis = s2_ptoken_empty;
  s2_strace_entry strace = s2_strace_entry_empty;
  s2_ptoken const origin = pr->token;
  cwal_value * xrv = 0;
  assert(rv);
  rc = s2_strace_push_pos(se, pr, &origin, &strace )
    /* treat new() like a function for stack trace purposes
       or exceptions may contain far-off location info.
    */;
  if(rc) return rc /* after this, no more 'return', only goto end */;
#if 0
  /* Reminder to self:

     1) please leave more descriptive comments to self in the future.

     2) this will not work with the #compiled token layer.
  */
  while(s2_is_space(*s2_ptoken_end(&pr->token))){
    /* Cosmetic workaround! */
    s2_ptoken_end_set(&pr->token, s2_ptoken_end(&pr->token)+1);
  }
#endif

  rc = s2_eval_expr_impl(se, pr, s2_ttype_op(S2_T_Comma),
                         S2_EVAL_STOP_AT_CALL,
                         &operand);
  if(rc) goto end;
  else if(operand) cwal_value_ref(operand);
  if(!operand || S2_T_ParenGroup!=pr->token.ttype){
    rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                       "Expecting EXPR(...) after '%s'.",
                       kw->word);
    goto end;
  }
  tTail = pr->token;
  operandStr = s2_ptoker_capture_cstr(pr, &operandLen);
#if 0
  MARKER(("operand=%.*s\n", (int)operandLen, operandStr));
#endif
  if(se->skipLevel>0){
    xrv = cwal_value_undefined();
    goto check_tail;
  }else if(!cwal_props_can(operand)){
    /* s2_dump_val(operand,"new operand"); */
#if 0
    if(cwal_value_is_tuple(operand)){
      /* we just happen to know this has a ctor, so
         fall through and try it... */
    }
    else
#endif
    {
      rc = s2_throw_ptoker(se, pr, CWAL_RC_EXCEPTION,
                           "'%s' expects a container, "
                           "but '%.*s' resolves to type '%s'.",
                           kw->word, (int)operandLen, operandStr,
                           cwal_value_type_name(operand));
      goto end;
    }
  }
  rc = s2_ctor_fetch(se, pr, operand, &ctor, -1);
  if(rc) goto end;
  assert(ctor);
#if 0
  MARKER(("new'ing=%.*s\n", (int)operandLen, operandStr));
  MARKER(("Capture=%.*s\n", (int)(s2_ptoken_begin(&pr->capture.end)
                                  -s2_ptoken_begin(&pr->capture.begin)),
          s2_ptoken_begin(&pr->capture.begin)));
  MARKER(("Call bits=%.*s\n", (int)s2_ptoken_len(&pr->token),
          s2_ptoken_begin(&pr->token)));
  s2_dump_val(operand, "new() prototype");
#endif

  /* Eval the call params... */
  assert(S2_T_ParenGroup==pr->token.ttype);
  args = cwal_new_array(se->e);
  if(!args){
    rc = CWAL_RC_OOM;
    goto end;
  }
  vargs = cwal_array_value(args);
  assert(vargs);
  cwal_value_ref(vargs);
  cwal_value_make_vacuum_proof(vargs, 1);
  /* cwal_value_make_vacuum_proof(operand, 1); */
  /* cwal_value_ref(operand); */
  rc = s2_ptoker_sub_from_toker(pr, &ptArgs);
  if(!rc){
    rc = s2_eval_to_array( se, &ptArgs, args, 1 );
    /* cwal_value_unhand(operand); */
    /* cwal_value_make_vacuum_proof(operand, 0); */
    if(!rc){
      /* s2_dump_val(vargs,"call args"); */
      rc = s2_ctor_apply( se, operand, ctor, args, &xrv );
      /* s2_dump_val(xrv, "result from 'new'"); */
    }
  }
  s2_ptoker_finalize( &ptArgs );

  check_tail:
  if(rc){
    assert(!xrv);
  }else{
    if(xrv) cwal_value_ref(xrv);
    /* Check for a {body} part after the T(...), and treat it like
       an inlined extension to the ctor like in Java: new X() {{ ... }}.
    */
    s2_next_token(se, pr, 0, &tWithThis);
    if(S2_T_SquigglyBlock==tWithThis.ttype){
      tTail = tWithThis;
      s2_ptoker_token_set(pr, &tTail);
      /*MARKER(("Got post-new() body: %.*s\n",
        (int)s2_ptoken_len(&tTail), s2_ptoken_begin(&tTail)));*/
      if(!se->skipLevel){
        rc = s2_eval_current_token_with_this(se, pr, xrv, NULL);
      }
    }else{
      s2_ptoker_next_token_set(pr, &tWithThis);
    }
  }
  end:
  switch(rc){
    case CWAL_RC_EXCEPTION:
      s2_exception_add_script_props(se, pr)
        /* Needed to decorate calls to native functions. */;
      break;
    default:
      break;
  }
  if(strace.pr){
    s2_strace_pop(se);
  }
  if(operand){
    cwal_value_unref(operand);
    operand = 0;
  }
  if(vargs){
    cwal_value_make_vacuum_proof(vargs, 0);
    cwal_value_unref(vargs);
  }
  if(!rc){
    s2_ptoker_token_set(pr, &tTail);
    *rv = xrv;
    cwal_value_unhand(xrv);
  }else if(xrv){
    cwal_value_unref(xrv);
  }
  return rc;
}

static int s2_enum_is_legal_id( int ttype ){
  switch(ttype){
    case S2_T_Identifier:
    case S2_T_LiteralStringSQ:
    case S2_T_LiteralStringDQ:
    case S2_T_LiteralString:
#if 0
    /* reminder to self:

       The reason numbers as keys in enums is not a good idea is
       because lookups may or may not be type-strict, depending on
       whether the enum uses an object or hash for storage. Consider:

       var e = enum {1: 'one'};
       e::1; // fine
       e::'1'; // doh: will match for an object but not a hash

       So... we disallow them solely to eliminate that corner case :/.
    */
    case S2_T_LiteralIntBin:
    case S2_T_LiteralIntDec:
    case S2_T_LiteralIntHex:
    case S2_T_LiteralIntOct:
    case S2_T_LiteralDouble:
#endif
      return ttype;
    default:
      return 0;
  }
}

int s2_keyword_f_echo( s2_keyword const * kw, s2_engine * se,
                       s2_ptoker * pr, cwal_value **rv){
  return s2_keyword_f_reserved(kw, se, pr, rv);
}

int s2_keyword_f_enum( s2_keyword const * kw, s2_engine * se,
                       s2_ptoker * pr, cwal_value **rv){
  /* TODO: reimplement this using the s2_enum_builder API.
     The glaring problem is that the builder wants to know, in advance,
     whether we want a hash or object for property storage.

     2020-02-21: enums are now always hashes, so that's not an issue
     anymore.
 */
  /* Also TODO: this function is a mess. Clean it up! */
  int rc;
  s2_ptoken tName = s2_ptoken_empty;
  s2_ptoker ptBody = s2_ptoker_empty;
  s2_ptoker const * oldScript = se->currentScript;
  cwal_value * store = 0;
  cwal_value * key;
  cwal_size_t entryCount = 0, loopCount = 0;
  cwal_value * enumProto = s2_prototype_enum(se);
  int doBreak = 0;
  cwal_hash * hashStore = 0;
  int gotCustomProto = 0 /* If a custom prototype property is applied, we
                            need a couple special cases. */;
  if(!enumProto) return CWAL_RC_OOM;
  rc = s2_next_token(se, pr, 0, 0);
  if(rc) return rc;
  else if(S2_T_Identifier==pr->token.ttype){
    tName = pr->token;
    rc = s2_next_token(se, pr, 0, 0);
    if(rc) return rc;
  }

  if(S2_T_SquigglyBlock != pr->token.ttype){
    return s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                         "Expecting {...} after '%s'.",
                         kw->word);
  }
  if(se->skipLevel>0){
    *rv = cwal_value_undefined();
    return 0;
  }
  if(!s2_ptoken_has_content(&pr->token)){
    return s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                         "Empty %s body is not permitted.",
                         kw->word);
  }

  rc = s2_ptoker_sub_from_toker(pr, &ptBody);
  /* As of here, NO RETURNs */
  if(rc) goto end;

#if 0  
  MARKER(("tName=%.*s\n", (int)nameLen, s2_ptoken_begin(&tName)));
  MARKER(("Body=%.*s\n", (int)s2_ptoken_len(&ptBody),
          s2_ptoken_begin(&ptBody)));
#endif

  assert(ptBody.parent == pr);
  se->currentScript = &ptBody;
  while(!rc && !doBreak){
    s2_ptoken tId = s2_ptoken_empty;
    s2_ptoken tValue = s2_ptoken_empty;
    cwal_value * uv = 0;
    cwal_value * wrappedVal = 0;
    char wrappedIsId = 0 /* true if the wrapped value is the
                            string form of the ID token, so that
                            we can know to re-use that string
                            in the entry-name-to-entry mapping. */;
    char isPrototype = 0 /* true if an entry's key is "prototype" */;
    key = 0;
    rc = s2_next_token(se, &ptBody, 0, 0);
    if(rc) goto loop_end;
    else if(1==++loopCount
            && S2_T_OpHash==ptBody.token.ttype){
      rc = s2_err_ptoker(se, &ptBody, CWAL_SCR_SYNTAX,
                         "The '#' modifier for enums is no longer "
                         "needed nor permitted.");
      goto loop_end;
    }else if(!s2_enum_is_legal_id(ptBody.token.ttype)){
      rc = s2_err_ptoker(se, &ptBody, CWAL_SCR_SYNTAX,
                         "Unexpected token type [%s] in %s body.",
                         s2_ttype_cstr(ptBody.token.ttype),
                         kw->word);
       goto loop_end;
    }
    tId = ptBody.token;
    if(9U==s2_ptoken_len(&tId)
       && 0==cwal_compare_cstr("prototype", 9,
                               s2_ptoken_begin(&tId), 9)){
      isPrototype = 1;
    }
    /* Check next token before continuing to alloc this value, to
       accommodate our switch-to-hash bits... */
    rc = s2_next_token(se, &ptBody, 0, &tValue);
    if(rc)  goto loop_end;
    s2_ptoker_token_set(&ptBody, &tValue) /* consume it */;
    doBreak = s2_ptoker_is_eof(&ptBody)
      /* Reminder: we want to fall through when doBreak
         is true so that store can be upgraded to a hash
         if needed.
      */;
    if(!doBreak){
      if(S2_T_Colon == tValue.ttype){ /* Entry: VALUE */
        if( (rc = s2_eval_expr_impl( se, &ptBody,
                                     s2_ttype_op(S2_T_Comma), 0,
                                     &wrappedVal)) ){
          assert(!wrappedVal);
          /*cwal_value_unref(wrappedVal);*/
          goto loop_end;
        }
        cwal_value_ref(wrappedVal);
        /* s2_dump_val(wrappedVal, "enum_entry = (value)"); */
        rc = s2_next_token(se, &ptBody, 0, 0);
        if(rc) goto loop_end;
        doBreak = s2_ptoker_is_eof(&ptBody);
      }
      else if(S2_T_Comma != tValue.ttype){
        rc = s2_err_ptoker(se, &ptBody, CWAL_SCR_SYNTAX,
                           "Expecting comma in %s body, "
                           "but got token type %s.",
                           kw->word,
                           s2_ttype_cstr(tValue.ttype));
        goto loop_end;
      }
    }
    assert(!rc);
    if(!wrappedVal){
      /* treat an entry with no value as having its name as its
         value. That seems, in practice, to be more useful than the
         undefined value. */
      assert(!wrappedIsId);
      if(isPrototype){
        rc = s2_err_ptoker(se, &ptBody, CWAL_SCR_SYNTAX,
                           "%s: prototype property cannot be applied "
                           "without a value.",
                           kw->word);
        goto loop_end;
      }
      rc = s2_ptoken_create_value(se, pr, &tId, &wrappedVal);
      if(!wrappedVal){
        rc = CWAL_RC_OOM;
        goto loop_end;
      }
      cwal_value_ref(wrappedVal);
      wrappedIsId = 1;
    }
    if(!store){
      /* Create the property storage. */
      assert(!entryCount);
      hashStore = cwal_new_hash(se->e, 7);
      if(!hashStore){
        rc = CWAL_RC_OOM;
        goto loop_end;
      }else{
        store = cwal_hash_value(hashStore);
        s2_hash_dot_like_object(store, 1
                                /* Needed for remainder of the loop,
                                   so that s2_set_v() will DTRT.
                                   Will get overwritten afterwards*/);
        cwal_value_ref(store);
        cwal_value_make_vacuum_proof(store,1);
      }
    }
    assert(!key);
    if(wrappedIsId){
      assert(!isPrototype);
      key = wrappedVal;
    }else if(!isPrototype){
      rc = s2_ptoken_create_value(se, pr, &tId, &key);
      if(rc){
        assert(!key);
        goto loop_end;
      }
    }
    if(isPrototype){
      assert(!key);
      assert(wrappedVal);
      ++gotCustomProto;
      rc = s2_set_v( se, store, se->cache.keyPrototype, wrappedVal)
        /* ^^^^^^^^ for its non-trivial handling of prototype setting. */
        ;
      if(rc) goto loop_end;
    }else{
      cwal_value_ref(key);
      uv = cwal_new_unique(se->e, 0);
      if(!uv){
        cwal_value_unref(key);
        rc = CWAL_RC_OOM;
        goto loop_end;
      }
      cwal_unique_wrapped_set( uv, wrappedVal );
      cwal_value_ref(uv);
      rc = s2_set_with_flags_v( se, store, key, uv, CWAL_VAR_F_CONST );
      cwal_value_unref(uv);
      cwal_value_unref(wrappedVal);
      wrappedVal = 0;
      ++entryCount;
      if(rc){
        cwal_size_t keylen = s2_ptoken_len(&tId);
        char const * ckey = s2_ptoken_begin(&tId);
        cwal_value_unref(key);
        key = 0;
        s2_ptoker_token_set( &ptBody, &tId );
        rc = s2_err_ptoker(se, &ptBody, rc,
                           "Assignment to enum entry '%.*s' failed with "
                           "code %d (%s).",
                           (int)keylen, ckey, rc, cwal_rc_cstr(rc));
        goto loop_end;
      }else{
        rc = s2_set_with_flags_v( se, store, uv, key,
                                  CWAL_VAR_F_CONST
                                  | CWAL_VAR_F_HIDDEN );
        cwal_value_unref(key);
        key = 0;
        if(rc) goto loop_end;
      }
    }
    loop_end:
    if(wrappedVal){
      cwal_value_unref(wrappedVal);
    }
    key = 0;
  }

  end:
  s2_ptoker_finalize( &ptBody );
  if(!rc && !entryCount){
    rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                       "%s requires at least one entry.",
                       kw->word);
  }
  
  if(!rc){
    assert(hashStore);
    rc = cwal_hash_grow_if_loaded( hashStore, 0.85 );
  }

  if(!rc && s2_ptoken_begin(&tName)){
    /* Set typename... */
    cwal_size_t const nameLen = s2_ptoken_len(&tName);
    assert(nameLen);
    key = cwal_new_string_value(se->e, s2_ptoken_begin(&tName), nameLen);
    if(!key) rc = CWAL_RC_OOM;
    else{
      cwal_value_ref(key);
      rc = cwal_prop_set_with_flags_v( store, se->cache.keyTypename, key,
                                       CWAL_VAR_F_CONST | CWAL_VAR_F_HIDDEN )
        /* Note that this is an object-level property, not hash entry. */;
      cwal_value_unref(key);
      key = 0;
    }
  }

  se->currentScript = oldScript;
  if(store){
      cwal_value_make_vacuum_proof(store,0);
  }
  if(rc){
    if(store){
      cwal_value_unref(store);
    }
  }else{
    if(!gotCustomProto){
      cwal_value_prototype_set(store, enumProto);
    }
    cwal_container_flags_set(store,
                             CWAL_CONTAINER_DISALLOW_NEW_PROPERTIES
                             | CWAL_CONTAINER_DISALLOW_PROP_SET
                             /* TODO:???
                                | CWAL_CONTAINER_DISALLOW_PROTOTYPE_SET */
                             );
    cwal_container_client_flags_set(store, S2_VAL_F_ENUM);
    cwal_value_unhand(store);
    *rv = store;
  }
  return rc;
}

/**
   Tag type IDs for typeinfo(TAG ...).

   ACHTUNG: their order MUST match the entries of the s2TypeInfoWords
   array (defined below) because we reference those by index with these
   entries.
*/
enum s2_typeinfo_words {
  /* Sentinel value. Must be 0. */
  TYPEINFO_NONE = 0,
  /** True if operand can be used as an operand for the "new" keyword. */
  TYPEINFO_CANNEW,
  /** True if operand is or has an array in its prototype chain. */
  TYPEINFO_HASARRAY,
  /** True if operand is or has an buffer in its prototype chain. */
  TYPEINFO_HASBUFFER,
  /** True if operand is or has an enum in its prototype chain. */
  TYPEINFO_HASENUM,
  /** True if operand is or has an exception in its prototype chain. */
  TYPEINFO_HASEXCEPTION,
  /** True if operand is or has a hash in its prototype chain. */
  TYPEINFO_HASHASH,
  /** True if operand is or has a native in its prototype chain. */
  TYPEINFO_HASNATIVE,
  /** True if operand is or has an Object in its prototype chain. */
  TYPEINFO_HASOBJECT,
  /** True if operand has a prototype. */
  TYPEINFO_HASPROTOYPE,
  /** True if operand is an array. */
  TYPEINFO_ISARRAY,
  /** True if operand is a bool. */
  TYPEINFO_ISBOOL,
  /** True if operand is a buffer. */
  TYPEINFO_ISBUFFER,
  /** True if operand is callable (is a function or has a function
      in its prototype chain). */
  TYPEINFO_ISCALLABLE,
  /** True if operand is a container (can hold its own properties). */
  TYPEINFO_ISCONTAINER,
  /** True if operand is the name of a declared (currently in-scope) variable/const. */
  TYPEINFO_ISDECLARED,
  /** True if operand is legal for use with the dot operator. */
  TYPEINFO_ISDEREFABLE,
    /** True if operand is a double. */
  TYPEINFO_ISDOUBLE,
  /** True if operand is an enum. */
  TYPEINFO_ISENUM,
  /** True if operand is an exception. */
  TYPEINFO_ISEXCEPTION,
  /** True if operand is a function. */
  TYPEINFO_ISFUNCTION,
  /** True if operand is a hash. */
  TYPEINFO_ISHASH,
  /** True if operand is an integer . */
  TYPEINFO_ISINT,
  /** True if cwal_value_is_iterating_props(operand) OR cwal_value_is_iterating_list(operand). */
  TYPEINFO_ISITERATING,
  /** True if cwal_value_is_iterating_list(operand). */
  TYPEINFO_ISITERATINGLIST,
  /** True if cwal_value_is_iterating_props(operand). */
  TYPEINFO_ISITERATINGPROPS,
  /** True if operand is a tuple or an array. */
  TYPEINFO_ISLIST,
  /** True if operand is a var/const declared in the local scope. */
  TYPEINFO_ISLOCAL,
  /** True if operand is a native. */
  TYPEINFO_ISNATIVE,
  /** True if the current "this" is being initalized via the 'new' keyword. */
  TYPEINFO_ISNEWING,
  /** True if operand is an integer or a double. */
  TYPEINFO_ISNUMBER,
  /**
     True if operand is an integer, a double, a boolean,
     or a numeric-format string (one parseable by
     NumberPrototype.parseNumber()).
  */
  TYPEINFO_ISNUMERIC,
  /** True if operand is an Object */
  TYPEINFO_ISOBJECT,
  /** True if operand is a string. */
  TYPEINFO_ISSTRING,
  /** True if operand is a Tuple */
  TYPEINFO_ISTUPLE,
  /** True if operand is a Unique-type value. */
  TYPEINFO_ISUNIQUE,
  /** True if cwal_value_may_iterate(operand) OR cwal_value_may_iterate_list(operand). */
  TYPEINFO_MAYITERATE,
  /** True if cwal_value_may_iterate_list(operand). */
  TYPEINFO_MAYITERATELIST,
  /** True if cwal_value_may_iterate(operand). */
  TYPEINFO_MAYITERATEPROPS,
  /** Evaluates to the type's name (as as the typename
      keyword). */
  TYPEINFO_NAME,
  /** Evaluates to the operand's refcount. */
  TYPEINFO_REFCOUNT
};
enum s2_typeinfo_arg_type {
TYPEINFO_ARG_IDENTIFIER = -1,
TYPEINFO_ARG_NONE = 0,
TYPEINFO_ARG_EXPR = 1
};
typedef struct {
  enum s2_typeinfo_words type;
  /**
     Argument type.
  */
  enum s2_typeinfo_arg_type argType;
} s2_typeinfo_word;
static const s2_typeinfo_word s2TypeInfoWords[] = {
  /*
   ACHTUNG: their order MUST match the entries of
   the s2_typeinfo_words enum because we reference them
   by index that way!
  */
  { TYPEINFO_NONE,              TYPEINFO_ARG_NONE},
  { TYPEINFO_CANNEW,            TYPEINFO_ARG_EXPR},
  { TYPEINFO_HASARRAY,          TYPEINFO_ARG_EXPR},
  { TYPEINFO_HASBUFFER,         TYPEINFO_ARG_EXPR},
  { TYPEINFO_HASENUM,           TYPEINFO_ARG_EXPR},
  { TYPEINFO_HASEXCEPTION,      TYPEINFO_ARG_EXPR},
  { TYPEINFO_HASHASH,           TYPEINFO_ARG_EXPR},
  { TYPEINFO_HASNATIVE,         TYPEINFO_ARG_EXPR},
  { TYPEINFO_HASOBJECT,         TYPEINFO_ARG_EXPR},
  { TYPEINFO_HASPROTOYPE,       TYPEINFO_ARG_EXPR},
  { TYPEINFO_ISARRAY,           TYPEINFO_ARG_EXPR},
  { TYPEINFO_ISBOOL,            TYPEINFO_ARG_EXPR},
  { TYPEINFO_ISBUFFER,          TYPEINFO_ARG_EXPR},
  { TYPEINFO_ISCALLABLE,        TYPEINFO_ARG_EXPR},
  { TYPEINFO_ISCONTAINER,       TYPEINFO_ARG_EXPR},
  { TYPEINFO_ISDECLARED,        TYPEINFO_ARG_IDENTIFIER},
  { TYPEINFO_ISDEREFABLE,       TYPEINFO_ARG_EXPR},
  { TYPEINFO_ISDOUBLE,          TYPEINFO_ARG_EXPR},
  { TYPEINFO_ISENUM,            TYPEINFO_ARG_EXPR},
  { TYPEINFO_ISEXCEPTION,       TYPEINFO_ARG_EXPR},
  { TYPEINFO_ISFUNCTION,        TYPEINFO_ARG_EXPR},
  { TYPEINFO_ISHASH,            TYPEINFO_ARG_EXPR},
  { TYPEINFO_ISINT,             TYPEINFO_ARG_EXPR},
  { TYPEINFO_ISITERATING,       TYPEINFO_ARG_EXPR},
  { TYPEINFO_ISITERATINGLIST,   TYPEINFO_ARG_EXPR},
  { TYPEINFO_ISITERATINGPROPS,  TYPEINFO_ARG_EXPR},
  { TYPEINFO_ISLIST,            TYPEINFO_ARG_EXPR},
  { TYPEINFO_ISLOCAL,           TYPEINFO_ARG_IDENTIFIER},
  { TYPEINFO_ISNATIVE,          TYPEINFO_ARG_EXPR},
  { TYPEINFO_ISNEWING,          TYPEINFO_ARG_EXPR
    /*special case: isnewing requires argType TYPEINFO_ARG_EXPR but
      optionally accepts no argument. */},
  { TYPEINFO_ISNUMBER,          TYPEINFO_ARG_EXPR},
  { TYPEINFO_ISNUMERIC,         TYPEINFO_ARG_EXPR},
  { TYPEINFO_ISOBJECT,          TYPEINFO_ARG_EXPR},
  { TYPEINFO_ISSTRING,          TYPEINFO_ARG_EXPR},
  { TYPEINFO_ISTUPLE,           TYPEINFO_ARG_EXPR},
  { TYPEINFO_ISUNIQUE,          TYPEINFO_ARG_EXPR},
  { TYPEINFO_MAYITERATE,        TYPEINFO_ARG_EXPR},
  { TYPEINFO_MAYITERATELIST,    TYPEINFO_ARG_EXPR},
  { TYPEINFO_MAYITERATEPROPS,   TYPEINFO_ARG_EXPR},
  { TYPEINFO_NAME,              TYPEINFO_ARG_EXPR},
  { TYPEINFO_REFCOUNT,          TYPEINFO_ARG_EXPR}
};

/**
   If the given ptoken resolves to a typeinfo(WORD) identifier, its
   entry is returned, else NULL is returned. This is an O(1) search.
   See s2_ptoken_keyword() for more details.
 */
static s2_typeinfo_word const * s2_ptoken_typeinfo( s2_ptoken const * pt ){
  cwal_size_t const tlen = s2_ptoken_len(pt);
  s2_typeinfo_word const * rc;
#define W(X,E) rc = tlen==(cwal_size_t)sizeof(X)-1 &&               \
    0==cwal_compare_cstr(s2_ptoken_begin(pt), tlen, X, sizeof(X)-1) \
    ? &s2TypeInfoWords[E] : NULL;                                   \
  assert(rc ? E==rc->type : 1); return rc
  switch(s2__keyword_perfect_hash(pt)){
    /* Generated by s2-keyword-hasher.s2 (or equivalent): */

    case 0x00002004: W("cannew",TYPEINFO_CANNEW);
    case 0x00003a10: W("can-new",TYPEINFO_CANNEW);
    case 0x000088d4: W("hasarray",TYPEINFO_HASARRAY);
    case 0x0000f5ba: W("has-array",TYPEINFO_HASARRAY);
    case 0x000112eb: W("hasbuffer",TYPEINFO_HASBUFFER);
    case 0x0001ece2: W("has-buffer",TYPEINFO_HASBUFFER);
    case 0x000043ba: W("hasenum",TYPEINFO_HASENUM);
    case 0x00007a24: W("has-enum",TYPEINFO_HASENUM);
    case 0x0008c084: W("hasexception",TYPEINFO_HASEXCEPTION);
    case 0x000f9697: W("has-exception",TYPEINFO_HASEXCEPTION);
    case 0x000042db: W("hashash",TYPEINFO_HASHASH);
    case 0x00007920: W("has-hash",TYPEINFO_HASHASH);
    case 0x0001163a: W("hasnative",TYPEINFO_HASNATIVE);
    case 0x0001f102: W("has-native",TYPEINFO_HASNATIVE);
    case 0x00011411: W("hasobject",TYPEINFO_HASOBJECT);
    case 0x0001ee92: W("has-object",TYPEINFO_HASOBJECT);
    case 0x0008fe4e: W("hasprototype",TYPEINFO_HASPROTOYPE);
    case 0x000fe16a: W("has-prototype",TYPEINFO_HASPROTOYPE);
    case 0x0000466e: W("isarray",TYPEINFO_ISARRAY);
    case 0x00007814: W("is-array",TYPEINFO_ISARRAY);
    case 0x00002216: W("isbool",TYPEINFO_ISBOOL);
    case 0x00003abf: W("is-bool",TYPEINFO_ISBOOL);
    case 0x00008df4: W("isbuffer",TYPEINFO_ISBUFFER);
    case 0x0000f16b: W("is-buffer",TYPEINFO_ISBUFFER);
    case 0x00023026: W("iscallable",TYPEINFO_ISCALLABLE);
    case 0x0003bb16: W("is-callable",TYPEINFO_ISCALLABLE);
    case 0x00048a39: W("iscontainer",TYPEINFO_ISCONTAINER);
    case 0x0007a928: W("is-container",TYPEINFO_ISCONTAINER);
    case 0x0002317e: W("isdeclared",TYPEINFO_ISDECLARED);
    case 0x0003bcff: W("is-declared",TYPEINFO_ISDECLARED);
    case 0x00047176: W("isderefable",TYPEINFO_ISDEREFABLE);
    case 0x00078b66: W("is-derefable",TYPEINFO_ISDEREFABLE);
    case 0x00008f26: W("isdouble",TYPEINFO_ISDOUBLE);
    case 0x0000f2e6: W("is-double",TYPEINFO_ISDOUBLE);
    case 0x00002290: W("isenum",TYPEINFO_ISENUM);
    case 0x00003b5a: W("is-enum",TYPEINFO_ISENUM);
    case 0x00049271: W("isexception",TYPEINFO_ISEXCEPTION);
    case 0x0007b484: W("is-exception",TYPEINFO_ISEXCEPTION);
    case 0x00024c1e: W("isfunction",TYPEINFO_ISFUNCTION);
    case 0x0003de01: W("is-function",TYPEINFO_ISFUNCTION);
    case 0x000021d6: W("ishash",TYPEINFO_ISHASH);
    case 0x00003a7b: W("is-hash",TYPEINFO_ISHASH);
    case 0x00012407: W("isinteger",TYPEINFO_ISINT);
    case 0x0001ecea: W("is-integer",TYPEINFO_ISINT);
    case 0x00049bc0: W("isiterating",TYPEINFO_ISITERATING);
    case 0x0007c0fa: W("is-iterating",TYPEINFO_ISITERATING);
    case 0x0049f317: W("isiteratinglist",TYPEINFO_ISITERATINGLIST);
    case 0x00f86719: W("is-iterating-list",TYPEINFO_ISITERATINGLIST);
    case 0x0093f07e: W("isiteratingprops",TYPEINFO_ISITERATINGPROPS);
    case 0x01f0da02: W("is-iterating-props",TYPEINFO_ISITERATINGPROPS);
    case 0x000022fe: W("islist",TYPEINFO_ISLIST);
    case 0x00003bef: W("is-list",TYPEINFO_ISLIST);
    case 0x00004697: W("islocal",TYPEINFO_ISLOCAL);
    case 0x0000788c: W("is-local",TYPEINFO_ISLOCAL);
    case 0x00009072: W("isnative",TYPEINFO_ISNATIVE);
    case 0x0000f4ba: W("is-native",TYPEINFO_ISNATIVE);
    case 0x0000918a: W("isnewing",TYPEINFO_ISNEWING);
    case 0x0000f61c: W("is-newing",TYPEINFO_ISNEWING);
    case 0x0000932c: W("isnumber",TYPEINFO_ISNUMBER);
    case 0x0000f84b: W("is-number",TYPEINFO_ISNUMBER);
    case 0x00012a04: W("isnumeric",TYPEINFO_ISNUMERIC);
    case 0x0001f4bc: W("is-numeric",TYPEINFO_ISNUMERIC);
    case 0x00008e90: W("isobject",TYPEINFO_ISOBJECT);
    case 0x0000f291: W("is-object",TYPEINFO_ISOBJECT);
    case 0x00009662: W("isstring",TYPEINFO_ISSTRING);
    case 0x0000fc5c: W("is-string",TYPEINFO_ISSTRING);
    case 0x00004a2e: W("istuple",TYPEINFO_ISTUPLE);
    case 0x00007d16: W("is-tuple",TYPEINFO_ISTUPLE);
    case 0x0000954c: W("isunique",TYPEINFO_ISUNIQUE);
    case 0x0000fb0a: W("is-unique",TYPEINFO_ISUNIQUE);
    case 0x000243ae: W("mayiterate",TYPEINFO_MAYITERATE);
    case 0x00040cc2: W("may-iterate",TYPEINFO_MAYITERATE);
    case 0x00246da6: W("mayiteratelist",TYPEINFO_MAYITERATELIST);
    case 0x0081db28: W("may-iterate-list",TYPEINFO_MAYITERATELIST);
    case 0x0048e4dc: W("mayiterateprops",TYPEINFO_MAYITERATEPROPS);
    case 0x0103c160: W("may-iterate-props",TYPEINFO_MAYITERATEPROPS);
    case 0x0000070c: W("name",TYPEINFO_NAME);
    case 0x00008bd2: W("refcount",TYPEINFO_REFCOUNT);

    default: break;
  }
#undef W
  return NULL;
}

int s2_keyword_f_typeinfo( s2_keyword const * kw, s2_engine * se,
                           s2_ptoker * pr, cwal_value **rv){
  s2_ptoken tIdent = s2_ptoken_empty;
  s2_ptoker prBody = s2_ptoker_empty;
  s2_ptoker const * oldScript = se->currentScript;
  cwal_size_t idLen = 0;
  cwal_value * xrv = 0;
  int rc;
  int buul = -1 /* <0=unknown, 0=false, >0=true */;
  s2_typeinfo_word const * word = 0;
  *rv = 0;
  assert(pr->e);
  /**
     Ideas:

     typeinfo(FLAG EXPR)

     We use (...) to avoid potential precedence confusion for use
     cases such as:

     typeinfo iscallable X || throw "need a callable type"

     i.e. does the || belong the RHS of the typeinfo or not? We punt
     on that problem by adding the parenthesis, making it unambiguous.

     FLAG is one of the words defined in s2TypeInfoWords above.
  */
  rc = s2_next_token( se, pr, 0, 0 );
  if(rc) goto end;

  if(S2_T_ParenGroup!=pr->token.ttype){
    rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                       "Expecting (IDENTIFIER ...) after '%s'.",
                       kw->word);
    goto end;
  }

  if(se->skipLevel>0){
    /**
       Reminder to self: someday we should arguably continue going
       in skip mode so that we can validate that the contents of (...)
       are syntactically valid.
    */
    *rv = cwal_value_undefined();
    goto end;
  }

  rc = s2_ptoker_sub_from_toker(pr, &prBody);
  assert(prBody.e == pr->e);
  if(!rc){
    prBody.flags |= S2_T10N_F_IDENTIFIER_DASHES;
    rc = s2_next_token(se, &prBody, 0, 0);
    prBody.flags &= ~S2_T10N_F_IDENTIFIER_DASHES;
  }
  if(rc) goto end;
  else if(S2_T_Identifier!=prBody.token.ttype){
    rc = s2_err_ptoker(se, &prBody, CWAL_SCR_SYNTAX,
                       "Expected typeinfo tag in %s(TAG ...).",
                       kw->word);
    goto end;
  }

  se->currentScript = &prBody;
  tIdent = prBody.token;
  idLen = s2_ptoken_len(&tIdent);
  assert(idLen > 0);
  /* MARKER(("typeinfo tag: %.*s\n", (int)idLen, s2_ptoken_begin(&tIdent))); */

  /*
    Look for sub-keyword match...
  */
  word = s2_ptoken_typeinfo(&tIdent);

  assert(!rc);
  if(!word){
    /* Note that in skip mode we've skipped over the whole (...), so
       this won't be triggered in skip mode (an exception to the
       rule/guideline regarding "blatant syntax errors").  Whether or
       not that needs "fixing" (such that this error could be
       triggered in skip mode) is as yet undecided.
    */
    rc = s2_err_ptoker(se, &prBody, CWAL_SCR_SYNTAX,
                       "Unknown idenitifier '%.*s' in %s().",
                       (int)idLen, s2_ptoken_begin(&tIdent),
                       kw->word);
    goto end;
  }

  switch(word->argType){
    case TYPEINFO_ARG_IDENTIFIER:
        /* typeinfo(tag IDENTIFIER) */
      rc = s2_next_token(se, &prBody, 0, 0);
      if(!rc && S2_T_Identifier != prBody.token.ttype){
          rc = s2_err_ptoker(se, &prBody, CWAL_SCR_SYNTAX,
                             "The %s(%.*s ...) tag requires an IDENTIFIER.",
                             kw->word, (int)idLen, s2_ptoken_begin(&tIdent));
      }
      if(rc) goto end;
      switch(word->type){
        case TYPEINFO_ISLOCAL:
        case TYPEINFO_ISDECLARED:
          xrv = s2_var_get(se, TYPEINFO_ISLOCAL==word->type ? 0 : -1,
                           s2_ptoken_begin(&prBody.token),
                           s2_ptoken_len(&prBody.token));
          buul = xrv ? 1 : 0;
          xrv = 0;
          break;
        default:
            s2_fatal(CWAL_RC_ASSERT,
                     "Missing typeinfo() handler entry for "
                     "TYPEINFO_ARG_IDENTIFIER!");
            break;
      }
      break;
    case TYPEINFO_ARG_EXPR:
        /* typeinfo(tag EXPR) */
      rc = s2_eval_expr_impl(se, &prBody, 0,
                             (TYPEINFO_NAME==word->type)
                             ? S2_EVAL_UNKNOWN_IDENTIFIER_AS_UNDEFINED
                             : 0
                             , &xrv);
      if(rc){
        assert(!xrv);
        goto end;
      }else if(!xrv && word->type==TYPEINFO_ISNEWING){
        /* Special case: typeinfo(isnewing) ==> typeinfo(isnewing this) */
        xrv = s2_var_get_v( se, -1, se->cache.keyThis );
      }else if(!xrv){
        rc = s2_err_ptoker(se, &prBody, CWAL_SCR_SYNTAX,
                           "Expecting non-empty expression after %s(%.*s ...).",
                           kw->word, (int)idLen, s2_ptoken_begin(&tIdent));
        goto end;
      }
      cwal_value_ref(xrv);
      break;
    case TYPEINFO_ARG_NONE:
        /* typeinfo(tag) */
        ++se->skipLevel;
        rc = s2_eval_expr_impl(se, &prBody, 0, 0, &xrv);
        --se->skipLevel;
        if(rc){
            assert(!xrv);
            goto end;
        }else if(xrv){
            assert(cwal_value_undefined() == xrv && "b/c of skipLevel");
            cwal_value_ref(xrv);
            rc = s2_err_ptoker(se, &prBody, CWAL_SCR_SYNTAX,
                               "Got unexpected expression in %s(%.*s <HERE>).",
                               kw->word, (int)idLen, s2_ptoken_begin(&tIdent));
            goto end;
        }
        break;
  }
  assert(!rc && "Expecting that non-0 RC did 'goto end'");

  switch(word->type){

#define TYPEWORD(WORD,PREDICATE) \
    case WORD: buul = PREDICATE(xrv); break

    TYPEWORD(TYPEINFO_ISARRAY,cwal_value_is_array);
    TYPEWORD(TYPEINFO_ISBOOL,cwal_value_is_bool);
    TYPEWORD(TYPEINFO_ISBUFFER,cwal_value_is_buffer);
    TYPEWORD(TYPEINFO_ISDOUBLE,cwal_value_is_double);
    TYPEWORD(TYPEINFO_ISENUM,s2_value_is_enum);
    TYPEWORD(TYPEINFO_ISEXCEPTION,cwal_value_is_exception);
    TYPEWORD(TYPEINFO_ISFUNCTION,cwal_value_is_function);
    TYPEWORD(TYPEINFO_ISINT,cwal_value_is_integer);
    TYPEWORD(TYPEINFO_ISITERATINGLIST,cwal_value_is_iterating_list);
    TYPEWORD(TYPEINFO_ISITERATINGPROPS,cwal_value_is_iterating_props);
    TYPEWORD(TYPEINFO_ISNATIVE,cwal_value_is_native);
    TYPEWORD(TYPEINFO_ISNEWING,s2_value_is_newing);
    TYPEWORD(TYPEINFO_ISOBJECT,cwal_value_is_object);
    TYPEWORD(TYPEINFO_ISSTRING,cwal_value_is_string);
    TYPEWORD(TYPEINFO_ISTUPLE,cwal_value_is_tuple);
    TYPEWORD(TYPEINFO_ISUNIQUE,cwal_value_is_unique);
    TYPEWORD(TYPEINFO_MAYITERATELIST,cwal_value_may_iterate_list);
    TYPEWORD(TYPEINFO_MAYITERATEPROPS,cwal_value_may_iterate);
#undef TYPEWORD

    case TYPEINFO_CANNEW:
      buul = s2_value_is_newable(se, xrv);
      break;

    case TYPEINFO_HASARRAY:
      buul = !!cwal_value_array_part(se->e, xrv);
      break;

    case TYPEINFO_HASBUFFER:
      buul = !!cwal_value_buffer_part(se->e, xrv);
      break;

    case TYPEINFO_HASENUM:
      buul = !!s2_value_enum_part(se, xrv);
      break;

    case TYPEINFO_HASEXCEPTION:
      buul = !!cwal_value_exception_part(se->e, xrv);
      break;

    case TYPEINFO_HASHASH:
      buul = s2_value_is_enum(xrv)
        ? 0 : !!cwal_value_hash_part(se->e, xrv);
      break;

    case TYPEINFO_HASNATIVE:
      buul = !!cwal_value_native_part(se->e, xrv, 0);
      break;

    case TYPEINFO_HASOBJECT:
      buul = !!cwal_value_object_part(se->e, xrv);
      break;

    case TYPEINFO_HASPROTOYPE:
      buul = !!cwal_value_prototype_get(se->e, xrv);
      break;

    case TYPEINFO_ISCALLABLE:
      buul = !!cwal_value_function_part(se->e, xrv);
      break;

    case TYPEINFO_ISCONTAINER:
      buul = cwal_props_can(xrv);
      break;

    case TYPEINFO_ISHASH:
      buul = cwal_value_is_hash(xrv) && !s2_value_is_enum(xrv);
      break;

    case TYPEINFO_ISITERATING:
      buul = cwal_value_is_iterating_props(xrv) || cwal_value_is_iterating_list(xrv);
      break;

    case TYPEINFO_ISLIST:
      buul = cwal_value_is_array(xrv) || cwal_value_is_tuple(xrv);
      break;

    case TYPEINFO_ISLOCAL:
    case TYPEINFO_ISDECLARED:
      /* handled above */
      assert(buul >= 0);
      break;

    case TYPEINFO_ISNUMBER:
      buul = cwal_value_is_integer(xrv)
        || cwal_value_is_double(xrv)
        /* reminder: don't use cwal_value_is_number() here b/c that
           one considers booleans to be numeric. */;
      break;

    case TYPEINFO_ISNUMERIC:
      if(!(buul = cwal_value_is_number(xrv))
         && cwal_value_is_string(xrv)){
      /* check for numeric-format strings a-la
         0.prototype.parseNumber(). */
        cwal_size_t slen = 0;
        char const * src = cwal_value_get_cstr(xrv, &slen);
        buul = s2_cstr_parse_double(src, slen, 0)
            || s2_cstr_parse_int(src, slen, 0);
      }
      break;

    case TYPEINFO_MAYITERATE:
      buul = cwal_value_may_iterate(xrv) || cwal_value_may_iterate_list(xrv);
      break;

    case TYPEINFO_ISDEREFABLE:
      switch(cwal_value_type_id(xrv)){
        case CWAL_TYPE_UNDEF:
        case CWAL_TYPE_NULL:
        case CWAL_TYPE_BOOL:
          buul = 0;
          break;
        default:
          buul = 1;
          break;
      }
      break;

    case TYPEINFO_NAME:{
      /* This dual-pronged approach isn't strictly necessary, but it
         keeps us from effectively strdup()'ing typenames on each call
         if xrv has/inherits a __typename property. The cwal type-name
         API does not deal in non-string types, so it cannot pass us
         back the underlying cwal_value for a given __typename
         property. Maybe that API needs to be changed?
      */
      cwal_value * vTn = cwal_prop_get_v(xrv, se->cache.keyTypename);
      if(!vTn){
        cwal_size_t nlen = 0;
        char const * name = cwal_value_type_name2(xrv, &nlen);
        vTn = nlen
          ? cwal_new_string_value(se->e, name, nlen)
          : cwal_value_undefined();
        if(!vTn){
          rc = CWAL_RC_OOM;
          goto end;
        }
      }
      *rv = vTn;
      break;
    }
    case TYPEINFO_REFCOUNT:
      *rv = cwal_new_integer(se->e,
                             (cwal_int_t)cwal_value_refcount(xrv));
      if(!*rv){
        rc = CWAL_RC_OOM;
        goto end;
      }
      break;
      
    case TYPEINFO_NONE:
      s2_fatal(CWAL_RC_ASSERT,
               "This is not possible: unhandled case in typeinfo handler.");
      break;
  }

  if(rc) goto end;
  else if(buul>=0){
    *rv = cwal_new_bool(buul ? 1 : 0);
  }else{
    assert(*rv);
  }

  end:
  s2_ptoker_finalize(&prBody);
  se->currentScript = oldScript;
  if(xrv){
    if(!rc && *rv == xrv) cwal_value_unhand(xrv);
    else cwal_value_unref(xrv);
  }
  return rc;
}

/**
   Internal helper for s2_foreach_iterate(), processing one entry
   from the result set.

   body holds the whole for-each body. If bodyIsExp then body is
   assumed to be a single expression, otherwise it's assumed to
   be a block of expressions. If keyName is not NULL then it is the
   NAME part of:

   foreach(X => NAME, value)

   and the key parameter must be the associated property key or
   array index.

   If valName is not NULL, it is the VALUE part of:

   foreach(X => name, VALUE)

   The val param holds the associated property value or array
   entry.

   Combinations of NULL/non-NULL which are legal depends on the
   operand's type, but it is never legal for both to be NULL.

   Returns 0 on success.
*/
static int s2_foreach_one( s2_engine * se, s2_ptoker const * body,
                           char bodyIsExpr,
                           cwal_value * keyName, cwal_value * key,
                           cwal_value * valName, cwal_value * val ){
  s2_ptoker pr = *body;
  int rc = 0;
  cwal_scope scope = cwal_scope_empty;
  rc = cwal_scope_push2(se->e, &scope);
  if(rc) return rc;
  assert(keyName ? !!key : !key);
  assert(valName ? !!val : !!keyName);
  if(keyName && (rc = s2_var_decl_v( se, keyName, key, 0 ))){
    goto end;
  }
  if(valName && (rc = s2_var_decl_v( se, valName, val, 0 ))){
    goto end;
  }
  assert(body->e);
  assert(pr.e);
  /* FIXME: won't work with #compiled tokens: */
  s2_ptoken_begin_set(&pr.token, s2_ptoker_begin(body));
  s2_ptoken_end_set(&pr.token, s2_ptoker_end(body));
  rc = s2_eval_current_token(se, &pr, bodyIsExpr, 0, 0);
  rc = s2_check_interrupted(se, rc);
  switch(rc){
    /* case CWAL_RC_BREAK: is handled higher up */
    case CWAL_RC_CONTINUE:
      rc = 0;
      s2_engine_err_reset(se);
      break;
    default:
      break;
  }

  end:
  cwal_scope_pop(se->e);
  return rc;
}

/**
   State used by s2_foreach_iterate().
*/
struct s2_foreach_state {
  s2_engine * se;
  s2_ptoker * body;
  char bodyIsExpr;
  char isEnum;
  cwal_value * keyName;
  cwal_value * valName;
  cwal_value * breakRC;
  cwal_size_t index;
};
typedef struct s2_foreach_state s2_foreach_state;
static const s2_foreach_state s2_foreach_state_empty = {
0,0,0,0,0,0,0,0U
};

/* cwal_kvp_visitor_f() impl for s2_foreach_iterate(). */
static int s2_foreach_kvp_visitor( cwal_kvp const * kvp, void * state ){
  s2_foreach_state * fst = (s2_foreach_state*)state;
  int rc = 0;
  cwal_value * val = cwal_kvp_value(kvp);
  assert(fst && fst->se);
  assert(fst->keyName);
  if(fst->isEnum && !cwal_value_is_unique(val)){
    /* for enums, only iterate over the string=>unique mappings, not
       the reverse mappings nor the properties like enumEntryCount. Seems
       reasonable enough. */
  }
#if S2_TRY_INTERCEPTORS
  else if(!!s2_value_is_interceptor(val)){
    /* Don't iterate over interceptors. */
  }
#endif
  else{
    rc = s2_foreach_one( fst->se, fst->body, fst->bodyIsExpr,
                         fst->keyName, cwal_kvp_key(kvp),
                         fst->valName, fst->valName ? val : 0 );
    if(CWAL_RC_BREAK==rc){
      fst->breakRC = s2_propagating_take(fst->se);
      assert(fst->breakRC && "misuse of CWAL_RC_BREAK");
      cwal_value_ref(fst->breakRC);
      /* s2_engine_err_reset(fst->se); */
      /* s2_dump_val(fst->breakRC,"breakRC"); */
    }
  }
  return rc;
}

/* cwal_array_visitor_f() impl for s2_foreach_iterate(). */
static int s2_foreach_array_visitor( cwal_array * a, cwal_value * v,
                                     cwal_size_t index, void * state ){
  s2_foreach_state * fst = (s2_foreach_state*)state;
  int rc;
  cwal_engine * e;
  cwal_value * vIndex;
  assert(fst && fst->se);
  e = fst->se->e;
  vIndex = fst->keyName ? cwal_new_integer(e, (cwal_int_t)index) : 0;
  if(fst->keyName && !vIndex){
    if(a){/*avoid unused param warning*/}
    return CWAL_RC_OOM;
  }
  cwal_value_ref(vIndex);
  rc = s2_foreach_one( fst->se, fst->body, fst->bodyIsExpr,
                       fst->keyName, fst->keyName ? vIndex : 0,
                       fst->valName, v ? v : cwal_value_undefined() );
  cwal_value_unref(vIndex);
  if(CWAL_RC_BREAK==rc){
    fst->breakRC = s2_propagating_take(fst->se);
    assert(fst->breakRC && "misuse of CWAL_RC_BREAK");
    cwal_value_ref(fst->breakRC);
    /* s2_engine_err_reset(fst->se); */
  }
  return rc;
}

/* cwal_value_visitor_f() impl for s2_foreach_iterate(), for
   visiting tuples. */
static int s2_foreach_tuple_visitor( cwal_value * v, void * state ){
  s2_foreach_state * fst = (s2_foreach_state*)state;
  int rc;
  cwal_engine * e;
  cwal_value * vIndex;
  assert(fst && fst->se);
  e = fst->se->e;
  vIndex = fst->keyName ? cwal_new_integer(e, (cwal_int_t)fst->index++) : 0;
  if(fst->keyName && !vIndex){
    return CWAL_RC_OOM;
  }
  if(vIndex) cwal_value_ref(vIndex);
  rc = s2_foreach_one( fst->se, fst->body, fst->bodyIsExpr,
                       fst->keyName, fst->keyName ? vIndex : 0,
                       fst->valName, v ? v : cwal_value_undefined() );
  if(vIndex) cwal_value_unref(vIndex);
  if(CWAL_RC_BREAK==rc){
    fst->breakRC = s2_propagating_take(fst->se);
    assert(fst->breakRC && "misuse of CWAL_RC_BREAK");
    cwal_value_ref(fst->breakRC);
    /* s2_engine_err_reset(fst->se); */
  }
  return rc;
}

/**
   foreach impl for strings. vStr must be the X part of
   foreach(X=>...) and it must be of type string (that gets
   asserted). Unlike most places, a Buffer will not act like a string
   here (foreach will use the Object property path for Buffers).

   Note that iteration using this approach is far more efficient than
   iterating over its length, e.g.

   for(var i = 0, n = aString.#; i<n; ++i ){... aString[i] ...}

   because that approach has to (for non-ASCII strings) re-traverse
   the whole string to find the i'th character.

   Returns 0 on success.
*/
static int s2_foreach_string_char( cwal_value * vStr, s2_foreach_state * fst ){
  int rc = 0;
  cwal_midsize_t slen = 0;
  cwal_string * str = cwal_value_get_string(vStr);
  char unsigned const * cstr = (char unsigned const *)cwal_string_cstr2( str, &slen );
  char unsigned const * pos = cstr;
  char const isAscii = cwal_string_is_ascii(str);
  cwal_engine * const e = fst->se->e;
  cwal_value * vIndex = 0;
  cwal_value * vVal = 0;
  char unsigned const * const eof = pos + slen;
  assert(str);
  assert(cstr);
  for( ; !rc && pos < eof && fst->index < slen; ++fst->index ){
    unsigned char const * cend = pos;
    if(isAscii){
      pos = cstr + fst->index;
      cend = pos + 1;
    }else{
      cwal_utf8_read_char(pos, eof, &cend);
      if(!(cend-pos)) break /*???*/;
    }
    assert(cend <= eof);
    vVal = cwal_new_string_value(e, (char const*)pos, (cwal_size_t)(cend-pos));
    if(!vVal){
      rc = CWAL_RC_OOM;
      break;
    }
    cwal_value_ref(vVal);
    if(fst->keyName){
      vIndex = cwal_new_integer(e, (cwal_int_t)fst->index);
      if(!vIndex){
        cwal_value_unref(vVal);
        rc = CWAL_RC_OOM;
        break;
      }
      cwal_value_ref(vIndex);
    }
    rc = s2_foreach_one( fst->se, fst->body, fst->bodyIsExpr,
                         fst->keyName, fst->keyName ? vIndex : 0,
                         fst->valName, vVal );
    if(vIndex) cwal_value_unref(vIndex);
    cwal_value_unref(vVal);
    pos = cend;
  }
  if(CWAL_RC_BREAK==rc){
    fst->breakRC = s2_propagating_take(fst->se);
    assert(fst->breakRC && "misuse of CWAL_RC_BREAK");
    cwal_value_ref(fst->breakRC);
    /* s2_engine_err_reset(fst->se); */
  }
  return rc;
}

/**
   Part of the foreach() loop mechanism. This part loops over the
   XXX part of foreach(...) XXX;

   arguments:

   kw: the keyword this function is working for (used for error
   reporting).

   body: the token holding the complete body of the loop.

   bodyIsExpr: if true, body is assumed to contain only a single
   expression, else is it assumed to be a {script block}.

   container: the container or string to iterate over.

   opMode: what to iterate over: 0 = object props, enum entries, or
   tuple entries. S2_T_At = array and tuple entries. S2_T_OpHash =
   hash entries. S2_T_LiteralString = a string (iterate over its
   characters), noting that it need not be a literal (we just use this
   token type ID because it's convenient to do so). Anything else is
   illegal and may trigger an assert().

   keyName: the symbol name of the key part of foreach(X=>key,val).

   valName: the symbol name of the val part of foreach(X=>key,val).

   rv: on success, the result value is put here (rescoped if needed)
   if it's not NULL.
*/
static int s2_foreach_iterate( s2_keyword const * kw, s2_engine * se,
                               s2_ptoker * body, char bodyIsExpr,
                               cwal_value * container, int opMode,
                               cwal_value * keyName, cwal_value * valName,
                               cwal_value ** rv){
  int rc = 0;
  s2_foreach_state fst = s2_foreach_state_empty;
  assert(0==opMode
         || S2_T_At==opMode
         || S2_T_OpHash==opMode
         || S2_T_LiteralString==opMode);
  fst.se = se;
  fst.body = body;
  fst.bodyIsExpr = bodyIsExpr;
  fst.keyName = keyName;
  fst.valName = valName;
  assert(!se->skipLevel);
  /* s2_dump_val(container,"container"); */
  /* s2_dump_val(keyName,"keyName"); */
  /* s2_dump_val(valName,"valName"); */

  /* Special case: for enums, when no mode is specified, choose its
     enum hash entries, but filter so that only the "forward"
     mappings, not its value-to-key reverse mappings, are
     traversed... */
  if( !opMode && s2_value_is_enum(container)){
    opMode = S2_T_OpHash;
    fst.isEnum = 1;
    assert(cwal_value_is_hash(container) && "Enums are always hashes since 2020-02-21.");
  }

  if(rv) *rv = cwal_value_undefined();
  switch(opMode){
    case S2_T_OpHash: /* Hash entries */
      assert(cwal_value_get_hash(container));
      CWAL_SWITCH_FALL_THROUGH;
    case 0: /* Object properties */{
      assert(cwal_props_can(container));
      rc = 0==opMode
        ? cwal_props_visit_kvp( container, s2_foreach_kvp_visitor,
                                &fst )
        : cwal_hash_visit_kvp( cwal_value_get_hash(container),
                               s2_foreach_kvp_visitor, &fst )
        ;
      break;
    }/* Objects/Hashes */
    case S2_T_At: /* Array/Tuple entries */{
      cwal_array * ar = cwal_value_array_part(se->e, container);
      cwal_tuple * tp = ar ? 0 : cwal_value_get_tuple(container);
      if(ar){
        rc = cwal_array_visit2( ar, s2_foreach_array_visitor, &fst );
      }else{
        assert(tp);
        rc = cwal_tuple_visit(tp, s2_foreach_tuple_visitor, &fst);
      }
      break;
    }
    case S2_T_LiteralString:
      rc = s2_foreach_string_char( container, &fst );
      break;
    default:
      if(kw){/*avoid unused param warning*/}
      assert(!"Unknown opMode!");
      rc = CWAL_RC_CANNOT_HAPPEN;
      break;
  }
  /* Reminder to self (20180620): there's seeminly a race condition
     between the break- and interruption handling here, in that a
     Break can potentially hide/trump a poorly-timed interrupt. We
     check for interruption in the top-level foreach impl. */
  if(CWAL_RC_BREAK==rc){
    rc = 0;
    assert(fst.breakRC && "should have been set by iterator");
    if(fst.breakRC){
      assert((cwal_value_is_builtin(fst.breakRC)
              || cwal_value_refcount(fst.breakRC))
             && "We're expecting the ref we held.");
      if(rv) {
        *rv = fst.breakRC;
        cwal_value_unhand(fst.breakRC);
      }else{
        cwal_value_unref(fst.breakRC);
      }
    }
  }else{
    if(fst.breakRC/* hypothetically possible on an interrupt */){
      assert((cwal_value_is_builtin(fst.breakRC)
              || cwal_value_refcount(fst.breakRC))
             && "We're expecting the ref we held.");
      cwal_value_unref(fst.breakRC);
    }
  }
  return rc;
}


/**
   foreach(X => v)
   foreach(X => k, v)

   Where X may be:

   container: iterate over non-hidden properties (never inherited properties).

   @array: iterate over array entries.

   #hash: iterate over hash entries.

   enum: iterate over (key=>enumEntry) pairs.

   tuple: iterate over tuple entries. May optionally, for consistency
   with @array, be prefixed with @, but the meaning is the same.

   string: iterate over each character.

   If 1 operand is passed after =>, then its interpretation depends on
   the data type:

   - Arrays, tuples, strings: the value of the element
   - Everything else: the property key

   If 2 operands are passed after =>, they are the property key/index
   and value, in that order.

   The value to the left of the => is evaluated only once, so it may,
   e.g. be the result of a function call.

   Reminder to self: results are undefined if passed a non-NULL final
   argument which points to uninitialized memory. i.e. *rv must, when
   this is called, either be 0 or point to a value. In practice,
   though, it must never point to a value, as we will (on success)
   overwrite it without unreferencing it. We never, in day-to-day
   work, leave such values uninitialized, but if this routine crashes
   near the end, it may be an indication of such.
*/
int s2_keyword_f_foreach( s2_keyword const * kw, s2_engine * se,
                          s2_ptoker * pr, cwal_value **rv){
  int rc = 0;
  s2_ptoker prParens = s2_ptoker_empty /* the whole (X => ...) part */;
  s2_ptoker prBody = s2_ptoker_empty /* the ... part of (X => ...) */;
  s2_ptoken tBody = s2_ptoken_empty /* body block/expression */;
  s2_ptoken tKey = s2_ptoken_empty /* (x => KEY, value) */;
  s2_ptoken tVal = s2_ptoken_empty /* (x => VALUE) (X => key, VALUE) */;
  cwal_value * operand = 0/*the value at the X part of foreach(X=>...)*/;
  char bodyIsExpr = 0 /* true if the loop body is a standalone expr,
                         false if it's a block. */;
  int operandMode = 0
    /*
      0 = object props, enum entries, or tuple entries

      S2_T_At = array and tuple entries

      S2_T_OpHash = hash entries

      S2_T_LiteralString = chars of a string
    */;
  
  rc = s2_next_token(se, pr, 0, 0);
  if(rc) return rc;
  else if(S2_T_ParenGroup!=pr->token.ttype){
    return s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                         "Expecting (...) after '%s'.",
                         kw->word);
  }
  rc = s2_ptoker_sub_from_toker(pr, &prParens);
  if(!rc){
    rc = s2_keyword_loop_get_body(kw, se, pr, &tBody, &bodyIsExpr);
  }
  if(rc) goto end;

  if(se->skipLevel){
    if(rv) *rv = cwal_value_undefined();
    goto end;
  }
  else{
    /* Parse the (X => ...) part */
    s2_ptoker prEntry = prParens
      /* (X => ...) part */
      /* This will break if/when we add #compiled tokens */
      ;
    s2_ptoken tmpTok = s2_ptoken_empty;
    s2_op const * opArrow = s2_ttype_op(S2_T_OpArrow2);
    assert(opArrow);
    /* Check for a leading '@' or '#' ... */
    rc = s2_next_token(se, &prEntry, 0, 0);
    if(rc){
      /*rc = s2_err_ptoker(se, &prEntry, rc, "Tokenization error.");*/
      goto end;
    }
    switch(prEntry.token.ttype){
      case S2_T_At:
      case S2_T_OpHash:
        operandMode = prEntry.token.ttype;
        break;
      default:
        s2_ptoker_next_token_set(&prEntry, &prEntry.token);
        break;
    }

    rc = s2_eval_expr_impl(se, &prEntry, opArrow, 0, &operand);
    if(operand) cwal_value_ref(operand);
    if(rc){
      assert(!operand && "but... how?");
      /*
        20181119: let's allow a break in the operand expression, for
        the sake of:

        foreach((blah ||| break)=>x){...}

        Yeah, it's a corner case, but i just tried to do it in a script
        and it didn't work, so... here it is.
      */
      if(CWAL_RC_BREAK==rc){
        *rv = s2_propagating_take(se);
        assert(*rv && "Else internal misuse of CWAL_RC_BREAK.");
        rc = 0;
      }
      goto end;
    }
    else if(!operand){
      rc = s2_err_ptoker(se, &prEntry, CWAL_SCR_SYNTAX,
                         "Empty expression not allowed in the X part of "
                         "%s(X=>...).", kw->word);
      goto end;
    }
    tmpTok = prEntry.capture.begin;
    /* s2_dump_val(operand,"operand"); */
    /*MARKER(("(X => ...) operandMode=%s, X=<<<%.*s>>>\n",
            s2_ttype_cstr(operandMode),
            (int)s2_ptoken_len(&tmpTok), s2_ptoken_begin(&tmpTok)));*/
    rc = s2_next_token(se, &prEntry, 0, 0);
    if(rc) goto end;
    /* Confirm operand type is okay... */
    switch(operandMode){
      case 0: /* default mode and mode for Object properties */{
        if(cwal_value_is_tuple(operand)){
          /* Force iteration over entries */
          operandMode = S2_T_At;
        }
        else if(cwal_value_is_string(operand)){
          operandMode = S2_T_LiteralString;
        }
        else if(!cwal_props_can(operand)){
          s2_ptoker_errtoken_set(&prEntry, &tmpTok);
          rc = s2_err_ptoker(se, &prEntry, CWAL_RC_TYPE,
                             "Expecting a string or a value capable of "
                             "holding properties.");
        }
        else if(!cwal_props_has_any(operand)
                && !s2_value_is_enum(operand)
                ){
          /* Simply checking for no properties isn't sufficient in the
             case of hashed enums unless the enum has the
             enumEntryCount property (which was removed 20171204). */

          /* Nothing to do! */
          /* *rv set is important, else the eval loop
             ignores what we parsed and takes "foreach" (the string)
             as the next token! */
          *rv = cwal_value_undefined();
          goto end;
        }
        break;
      }
      case S2_T_At: /* Array and tuple entries */{
        cwal_array * ar = cwal_value_array_part(se->e, operand);
        cwal_tuple * tp = ar ? 0 : cwal_value_get_tuple(operand);
        if(!ar && !tp){
          s2_ptoker_errtoken_set(&prEntry, &tmpTok);
          rc = s2_err_ptoker(se, &prEntry, CWAL_RC_TYPE,
                             "Expecting an Array or Tuple value.");
        }else if(0==
                 (ar ? cwal_array_length_get(ar) : cwal_tuple_length(tp))){
          /* Nothing to do! */
          *rv = cwal_value_undefined();
          goto end;
        }
        break;
      }
      case S2_T_OpHash: /* Hash entries */{
        cwal_hash * h = cwal_value_hash_part(se->e, operand);
        if(!h){
          s2_ptoker_errtoken_set(&prEntry, &tmpTok);
          rc = s2_err_ptoker(se, &prEntry, CWAL_RC_TYPE,
                             "Expecting a Hash value.");
        }else if(!cwal_hash_entry_count(h)){
          /* Nothing to do! */
          *rv = cwal_value_undefined();
          goto end;
        }
        break;
      }
      default:
        rc = CWAL_RC_ASSERT;
        assert(!"Wrong opMode!");
    }
    if(rc) goto end;

    if(S2_T_OpArrow2 != prEntry.token.ttype){
      rc = s2_err_ptoker(se, &prEntry, CWAL_SCR_SYNTAX,
                         "Expecting => after X in "
                         "%s(X => ...).",
                         kw->word);
      goto end;
    }
    rc = s2_next_token(se, &prEntry, 0, 0);
    if(rc) goto end;
    else if(S2_T_Identifier != prEntry.token.ttype){
      rc = s2_err_ptoker(se, &prEntry, CWAL_SCR_SYNTAX,
                         "Expecting identifier after => "
                         "in %s(X => ...).",
                         kw->word);
      goto end;
    }else{
      tKey = prEntry.token;
    }
    /* Check for a comma then a value identifier... */
    rc = s2_next_token(se, &prEntry, 0, 0);
    if(rc) goto end;
    else if(S2_T_Comma == prEntry.token.ttype){
      rc = s2_next_token(se, &prEntry, 0, 0);
      if(rc) goto end;
      else if(S2_T_Identifier != prEntry.token.ttype){
        rc = s2_err_ptoker(se, &prEntry, CWAL_SCR_SYNTAX,
                           "Expecting identifier after comma "
                           "in %s(X => Y,...).",
                           kw->word);
        goto end;
      }else{
        tVal = prEntry.token;
      }
    }else if(!s2_ttype_is_eox(prEntry.token.ttype)){
      rc = s2_err_ptoker(se, &prEntry, CWAL_SCR_SYNTAX,
                         "Unexpected token after Y in "
                         "%s(X => Y...).",
                         kw->word);
      goto end;
    }else{
      if(S2_T_At==operandMode || S2_T_LiteralString==operandMode){
      /* (@X=>V) and ("string"=>V) pass the index's value, not the
         index, to each loop iteration. */
        tVal = tKey;
        tKey = s2_ptoken_empty;
      }
    }
  }

  /*MARKER(("tBody (isExpr=%d, opMode=%s): <<<%.*s>>>\n", bodyIsExpr,
          s2_ttype_cstr(operandMode), (int)s2_ptoken_len(&tBody),
          s2_ptoken_begin(&tBody)));*/
  /*MARKER(("tKey: <<<%.*s>>>\n", (int)s2_ptoken_len(&tKey), s2_ptoken_begin(&tKey)));*/
  /*MARKER(("tVal: <<<%.*s>>>\n", (int)s2_ptoken_len(&tVal), s2_ptoken_begin(&tVal)));*/

  /* And now, finally, create the key/value name parts and iterate
     over the body... */
  assert(!rc);
  if(!rc){
    cwal_value * key = 0;
    cwal_value * val = 0;
    cwal_value * xrv = 0;
    cwal_scope scope = cwal_scope_empty;
    assert(tVal.ttype || tKey.ttype);
    rc = cwal_scope_push2(se->e, &scope);
    if(rc) goto end;
    rc = s2_ptoker_sub_from_token(pr, &tBody, &prBody)
      /* 20200107 FIXME: this won't work with #compiled tokens when
         tBody is non-{} expression, nor can
         s2_ptoker_sub_from_token() accommodate underlying compiled
         tokens, whether or not tBody is a {} block. */;
    assert(!rc && "Cannot fail!");
    assert(pr->e);
    prBody.parent = pr;
    prBody.e = pr->e;
    if(tKey.ttype){
      rc = s2_ptoken_create_value(se, pr, &tKey, &key);
      if(rc){
        assert(!key);
        goto pop_scope;
      }
      assert(key);
      cwal_value_ref(key);
    }
    if(tVal.ttype){
      rc = s2_ptoken_create_value(se, pr, &tVal, &val);
      if(rc){
        if(key) cwal_value_unref(key);
        goto pop_scope;
      }
      assert(val);
      cwal_value_ref(val);
    }

    /* s2_dump_val(key,"foreach key"); */
    /* s2_dump_val(val,"foreach value name"); */
    ++se->scopes.current->sguard.vacuum /* protect key/val */;
    rc = s2_foreach_iterate( kw, se, &prBody, bodyIsExpr,
                             operand, operandMode,
                             key, val, &xrv);
    --se->scopes.current->sguard.vacuum;
    if(xrv) cwal_value_ref(xrv);
    if(key) cwal_value_unref(key);
    cwal_value_unref(val);
    val = 0;
    /* prBody.errPos = 0; */
    pop_scope:
    assert( &scope == se->scopes.current->cwalScope );
    cwal_scope_pop2( se->e, rc ? 0 : xrv );
    rc = s2_check_interrupted(se, rc);
    if(!rc && rv){
      *rv = xrv ? xrv : cwal_value_undefined();
      if(xrv) cwal_value_unhand(xrv);
    }else{
      if(xrv) cwal_value_unref(xrv);
    }
  }

  end:
  if(!rc){
    s2_ptoker_token_set(pr, &tBody)
      /* 20200107 FIXME: this won't work with #compiled tokens when
         tBody is non-{} expression. */;
    rc = s2_check_interrupted(se, rc);
  }
  if(operand){
    if(!rc && rv) cwal_value_ref(*rv);
    cwal_value_unref(operand);
    if(!rc && rv) cwal_value_unhand(*rv);
  }
  s2_ptoker_finalize( &prParens );
  return rc;
}

/**
   The using() function-like keyword is used for accessing
   "using"/importSymbols()-injected state.
*/
int s2_keyword_f_using( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv){
  int rc;
  cwal_value * imports = 0;
  cwal_value * xrv = 0;
  assert(S2_T_KeywordUsing == kw->id);
  rc = s2_next_token(se, pr, 0, 0);
  if(rc) return rc;
  if(S2_T_ParenGroup!=pr->token.ttype){
    if(se->currentScriptFunc){
      /* Accept "using" as shorthand for "using()" inside a script
         function body. */
      s2_ptoker_putback(pr);
    }else{
      return s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                           "Expecting (...) after %s keyword.",
                           kw->word);
    }
  }
  if(se->skipLevel>0){
    *rv = cwal_value_undefined();
    return 0;
  }
  if(S2_T_ParenGroup==pr->token.ttype){
    rc = s2_eval_current_token(se, pr, 0,
                               0/* S2_EVAL_PUSH_SCOPE seems like overkill,
                                   as the operand will almost always be empty
                                   or an identifier/property access, rather than
                                   a complex expression. Note that the empty-parens
                                   case gets optimized out (no eval, no scope),
                                   either way.*/,
                               &xrv);
    if(rc){
      assert(!xrv);
      return rc;
    }
  }else{
    assert(se->currentScriptFunc /* tested above */);
  }
  if(!xrv){
    /* using() is valid in the current script-side func to refer
       to its own imports. */
    if(!se->currentScriptFunc){
      rc = s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                         "%s() is only valid in the body of a "
                         "script-implemented function.",
                         kw->word);
    }else{
      imports = se->currentScriptFunc->vImported;
      cwal_value_ref(imports);
    }
  }else{
    /* using(expr), where expr must be a script-side function. */
    cwal_function * f = cwal_value_get_function(xrv);
    s2_func_state * fst = f ? s2_func_state_for_func(f) : 0;
    cwal_value_ref(xrv);
    if(!fst){
      rc = s2_throw_ptoker(se, pr, CWAL_RC_TYPE,
                           "Expression passed to %s(...) is not a "
                           "script-defined function.",
                           kw->word)
        /* Note that this throws an exception, but using(<EMPTY>) in
           the wrong place is a syntax error. Feature or bug? */;
    }else{
      imports = fst->vImported;
      cwal_value_ref(imports);
    }
    cwal_value_unref(xrv);
    xrv = 0;
  }
  if(imports){
    assert(!rc);
#if 0
    cwal_value_rescope(se->scopes.current->cwalScope, imports)
      /* i don't think this is strictly necessary. i haven't (yet?)
         been able to work out a case where the imports would end up
         in a wrong scope without this. If the imports are accessed by
         an older scope, they'll get rescoped as soon as a local symbol
         references them, they're propagated, or similar.*/;
#endif
    cwal_value_unhand(imports);
    *rv = imports;
  }else if(!rc){
    *rv = cwal_value_undefined();
  }
  return rc;
}

/**
   The s2out keyword.
*/
int s2_keyword_f_s2out( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv){
  int rc = 0;
  cwal_value * v = se->skipLevel>0 ? cwal_value_undefined() : se->cache.s2out;
  if(!v){
    /*
      Initialize it on the first call...

      The API does not guaranty that s2.io.output ever gets installed,
      so we cannot simply reuse that object here (though we may at
      some point try that before falling back to creating a new
      object). Alternately, we could reverse that: initilialize
      s2.output to point to this object.
    */
    cwal_scope sc = cwal_scope_empty
      /*
        Reminder to self: we use a scope here in order to cleanly
        handle the 1-in-a-bazillion chance that the s2out.operator<<
        prop set succeeds but s2_stash_set() fails (out of memory). In
        that case we'd leave a cyclic structure laying around, which
        the scope will clean up for us.
      */;
    S2_UNUSED_ARG pr;
    rc = cwal_scope_push2(se->e, &sc);
    if(rc) return rc;
    v = cwal_new_function_value(se->e, s2_cb_write, 0, 0, 0);
    if(v){
      cwal_value_prototype_set(v, 0);
      /*
        We want s2out to be callable like s2out(...)  and
        s2out<<arg<<arg<<...; The easiest and most efficient way to do
        that is...
      */
      rc = cwal_prop_set_with_flags(
             v, "operator<<", 10, v,
             CWAL_VAR_F_CONST
             /* const is not strictly needed, but that's okay */
           );
      if(!rc){
        rc = s2_stash_set(se, kw->word, v )
          /* Moves it to the top scope and implicitly makes it
             vacuum-proof. */;
      }
    }else{
      rc = CWAL_RC_OOM;
    }
    cwal_scope_pop2( se->e, rc ? 0 : v );
    if(rc){
      v = 0 /* was destroyed by the popped scope */;
    }else{
      /* v is held by/in se->stash. */
      se->cache.s2out = v;
      /* We'll disallow new properties on this object for sanity's
         sake. We can reconsider this later if the need arises. One
         side-effect of this is that we cannot reasonably make
         s2.io.output an alias for this object because there's no good
         justification for locking that object this way. */
      s2_seal_container(v, 1);
    }
  }
  if(!rc) *rv = v;
  return rc;
}

/**
   The cwal_callback_f() part of the import keyword.
*/
static int s2_cb_keyword_import( cwal_callback_args const * args, cwal_value **rv ){
  int rc = 0;
  s2_engine * const se = s2_engine_from_args(args);
  cwal_value * const vPf = cwal_prop_get(cwal_function_value(args->callee),
                                         "path", 4);
  s2_pf * const pf = s2_value_pf(vPf);
  if((rc=s2_cb_disable_check(args, S2_DISABLE_FS_STAT | S2_DISABLE_FS_READ))){
    return rc;
  }
  else if(!pf){
    rc = cwal_cb_throw(args, CWAL_RC_ASSERT,
                     "The import.path property has gone missing!");
  }
  else if(1==args->argc && cwal_value_is_bool(args->argv[0])){
    /* import(bool): set args->self[se->cache.keyImportFlag] */;
    s2_seal_container(args->self, 0);
    rc = cwal_prop_set_v(args->self, se->cache.keyImportFlag,
                         args->argv[0]);
    s2_seal_container(args->self, 1);
    if(!rc) *rv = args->self;
  }else if(!args->argc || args->argc>2 ||
           (2==args->argc && (!cwal_value_is_bool(args->argv[0])))
           /* Non-stringy argv[1] case is handled below. */
           ){
    misuse:
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting ([bool doPathSearch,] string filename) "
                       "or (bool doPathSearch) arguments(s).");
  }else{
    /*
      import([bool doPathSearch,] string filename), where doPathSearch
      defaults to args->self[se->cache.keyImportFlag].
    */
    char const doPfSearch = args->argc>1
      ? cwal_value_get_bool(args->argv[0])
      : cwal_value_get_bool(cwal_prop_get_v(args->self,
                                            se->cache.keyImportFlag));
    cwal_value * const vFName = args->argv[args->argc>1 ? 1 : 0];
    cwal_size_t fLen;
    char const * fname = cwal_value_get_cstr(vFName, &fLen);
#define FMT_NOT_FOUND "File not found: %.*s", (int)fLen, fname
    if(!fname){
      goto misuse;
    }
    else if(doPfSearch){
      char const * fn = s2_pf_search(pf, fname, fLen, &fLen,
                                     S2_PF_SEARCH_FILES);
      if(fn) fname = fn;
      if(!fn){
        rc = cwal_cb_throw(args, CWAL_RC_NOT_FOUND, FMT_NOT_FOUND);
        if(CWAL_RC_EXCEPTION==rc){
          /* Amend the exception with:
             exception.notFound = 
             {filename:name, path:array, extensions:array} */
          int rc2;
          cwal_value * obj;
          cwal_value * const ex = cwal_exception_get(args->engine);
          assert(ex && "But we *just* threw one?");
          obj = cwal_new_object_value(args->engine)
            /* Reminder to self: we're running in a callback scope, so
               cleanup on error is not _strictly_ necessary here. */;
          if(obj){
            cwal_value_ref(obj);
            rc2 = cwal_prop_set(obj, "filename", 8, vFName);
            if(!rc2) rc2 = cwal_prop_set(obj, "path", 4,
                                         cwal_array_value(s2_pf_dirs(pf)));
            if(!rc2) rc2 = cwal_prop_set(obj, "extensions", 10,
                                         cwal_array_value(s2_pf_exts(pf)));
            if(!rc2){
              rc2 = cwal_prop_set(ex, "notFound", 8, obj);
            }
            cwal_value_unref(obj);
          }else{
            rc2 = CWAL_RC_OOM;
          }
          if(rc2){
            rc = rc2;
          }
        }
      }
    }else if(!s2_file_is_accessible(fname, 0)){
      rc = cwal_cb_throw(args, CWAL_RC_NOT_FOUND, FMT_NOT_FOUND);
    }
#undef FMT_NOT_FOUND
    if(!rc){
      rc = s2_eval_filename(se, 1, fname, fLen, rv);
    }
  }
  return rc;
}

/**
   The import keyword impl.
*/
int s2_keyword_f_import( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv){
  int rc = 0;
  cwal_value * v = se->skipLevel>0 ? cwal_value_undefined() : se->cache.import;
  if(!v){
    /*
      Initialize it on the first call...
    */
    cwal_scope sc = cwal_scope_empty
      /*
        Reminder to self: we use a scope here in order to simplify
        cleanup if something goes wrong.
      */;
    s2_pf * pf = 0;
    cwal_value * vImport = 0;
    S2_UNUSED_ARG pr;
    rc = cwal_scope_push2(se->e, &sc);
    if(rc) return rc;
    vImport = cwal_new_function_value(se->e, s2_cb_keyword_import, 0, 0, 0);
    if(!vImport){
      rc = CWAL_RC_OOM;
      goto end;
    }
    pf = s2_pf_new(se);
    rc = pf
      ? cwal_prop_set_with_flags(vImport, "path", 4,
                                 s2_pf_value(pf), CWAL_VAR_F_CONST)
      : CWAL_RC_OOM;
    if(!rc){
      rc = cwal_prop_set_v(vImport, se->cache.keyImportFlag,
                           cwal_value_true());
    }
    if(rc) goto end;
    else{/* Set path/extensions from environment, if available... */
      struct {
        char const * evar;
        char const * dflt;
        int (*adder)(s2_pf *, char const *, cwal_size_t);
        cwal_array * (*getter)(s2_pf*);
      } envs[] = {
        {"S2_IMPORT_PATH", NULL, s2_pf_dir_add, s2_pf_dirs
         /* Potential TODO: if s2_home_get() returns non-NULL
            and S2_IMPORT_PATH is not set, add S2_HOME/lib(?)
            (S2_HOME/import?) to the defeault import path. */
        },
        {"S2_IMPORT_EXTENSIONS", ".s2", s2_pf_ext_add, s2_pf_exts}
      };
      s2_path_toker pt = s2_path_toker_empty;
      char const * entry = 0;
      char const * env;
      cwal_size_t entryLen = 0;
      size_t i = 0;
      assert(pf);
      for(; !rc && i < sizeof(envs)/sizeof(envs[0]); ++i){
        env = getenv(envs[i].evar);
        if(!env){
          /* Note that empty env vars evaluate to "", not NULL.  We
             rely on that here to enable overriding of the default
             S2_IMPORT_EXTENSIONS value with an empty list. */
          env = envs[i].dflt;
        }
        if(env && *env){
          s2_path_toker_init(&pt, env, -1);
          while(!rc && !s2_path_toker_next(&pt, &entry, &entryLen)){
            rc = envs[i].adder(pf, entry, entryLen);
          }
        }
        if(!rc && !envs[i].getter(pf)){
          /* If there was no env var, or it had no entries, the
             current pf array member will be NULL. Calling the getter
             initializes that member if needed, and we want that for
             this use case. */
          rc = CWAL_RC_OOM;
          break;
        }
      }
      if(rc) goto end;
    }
    assert(!rc);
    rc = s2_stash_set(se, kw->word, vImport)
      /* Moves it to the top scope and implicitly makes it
         vacuum-proof. */;
    end:
    cwal_scope_pop( se->e )
      /* Remember that:
         1) on success, vImport is in the stash (so it's alive) and
         2) this pop also cleans up pf on error.
      */;
    if(rc){
       /* vImport was destroyed by the popped scope */
    }else{
      /* vImport is held by/in se->stash. */
      v = se->cache.import = vImport;
      s2_seal_container(vImport, 1);
    }
  }/* end of first-call initialization */
  if(!rc) *rv = v;
  return rc;
}


/**
   Tag type IDs for pragma(TAG ...).

   ACHTUNG: their order MUST match the entries of the s2PragmaWords
   array (defined below) because we reference those by index with
   these entries.
*/
enum s2_pragma_type {
  /* Sentinel value. Must be 0. */
  PRAGMA_NONE = 0,
  /**
     Get/set s2_engine::flags::exceptionStackTrace.

  */
  PRAGMA_EXCEPTION_STACKTRACE,
  /** Evaluates to the operand's refcount. */
  PRAGMA_REFCOUNT,
  /** Get/set s2_engine::sweepInterval */
  PRAGMA_SWEEP_INTERVAL,
  /** Get/set s2_engine::vacuumInterval */
  PRAGMA_VACUUM_INTERVAL,
  /** Get/set s2_engine::flags::traceSweeps */
  PRAGMA_TRACE_SWEEP,
  /** Get/set s2_engine::flags::traceAssertions */
  PRAGMA_TRACE_ASSERT,
  /** Get/set s2_engine::flags::traceTokenStack */
  PRAGMA_TRACE_TOKEN_STACK,
  /** Get various build-time flags/options. */
  PRAGMA_BUILD_OPT,
  /** Fetch integer values for CWAL_RC_.../S2_RC_...
      names. */
  PRAGMA_RC
};
/**
   Modes of operation for pragmas:
 */
enum s2_pragma_operand_mode {
/* pragma(TAG) */
PRAGMA_OPERAND_NONE = 0,
/* pragma(TAG EXPR) */
PRAGMA_OPERAND_EXPR,
/* pragma(TAG) or pragma(TAG EXPR) */
PRAGMA_OPERAND_OPT_EXPR,
/* pragma(TAG IDENTIFIER) */
PRAGMA_OPERAND_IDENTIFIER,
/* pragma(TAG) or pragma(TAG IDENTIFIER) */
PRAGMA_OPERAND_OPT_IDENTIFIER
};

/**
   Internal-only callback type for individual pragma() handlers.
*/
typedef struct s2_pragma_word s2_pragma_word;

/**
   Callback for pragma() handlers. The 2nd parameter is the
   pragma an whose behalf the handler is being called. The third
   is NULL unless...

   If the pragma has an `opMode` of PRAGMA_OPERAND_IDENTIFIER then the
   3rd parameter is the token which contains the identifier. If opMode
   is PRAGMA_OPERAND_OPT_IDENTIFIER then the 3rd parameter is NULL if
   no identifier was provided. In either case, the 4th parameter
   will have a value of NULL.

   If opMode is PRAGMA_OPERAND_EXPR then the 4th parameter is the
   non-NULL result of that expression. If opMode is
   PRAGMA_OPERAND_OPT_EXPR, the 4th parameter may be NULL, indicating
   that no expression was provided (an empty expression is not
   allowed, and this callback will never be called in that case).

   The pragma must write its result to the final parameter and return
   0 on success or a CWAL_RC_xxx or S2_RC_xxx value on error. Any
   non-0 result other than CWAL_RC_EXCEPTION will likely be fatal to
   the current script.
*/
typedef int (*s2_pragma_f)(s2_engine *, struct s2_pragma_word const *,
                           s2_ptoken const * tIdent,
                           cwal_value * ev, cwal_value **rv);
/**
   pragma(...) state.
*/
struct s2_pragma_word {
  enum s2_pragma_type type;
  /** Name of the pragma. */
  char const * word;
  /** Length of this->word, in bytes. */
  cwal_size_t wordLen;
  enum s2_pragma_operand_mode opMode;
  s2_pragma_f call;
};

#define PRAGMA_F_DECL(P) static int s2_pragma_f_##P\
  (s2_engine *, struct s2_pragma_word const *, \
   s2_ptoken const *, cwal_value *, cwal_value **)
PRAGMA_F_DECL(refcount);
PRAGMA_F_DECL(stacktrace);
PRAGMA_F_DECL(svinterval);
PRAGMA_F_DECL(trace_sweep);
PRAGMA_F_DECL(trace_assert);
PRAGMA_F_DECL(trace_token_stack);
PRAGMA_F_DECL(build_opt);
PRAGMA_F_DECL(rc);
#undef PRAGMA_F_DECL

static const struct s2_pragma_word s2PragmaWords[] = {
  /*
   ACHTUNG: their order **MUST** match the entries of
   the s2_pragma_word enum because we reference them
   by index that way!
  */
{ PRAGMA_NONE,                  "", 0,
    PRAGMA_OPERAND_NONE, 0},
{ PRAGMA_EXCEPTION_STACKTRACE,  "exception-stacktrace", 20,
    PRAGMA_OPERAND_OPT_EXPR, s2_pragma_f_stacktrace},
{ PRAGMA_REFCOUNT,              "refcount", 8,
    PRAGMA_OPERAND_EXPR, s2_pragma_f_refcount},
{ PRAGMA_SWEEP_INTERVAL,        "sweep-interval", 14,
    PRAGMA_OPERAND_OPT_EXPR, s2_pragma_f_svinterval},
{ PRAGMA_VACUUM_INTERVAL,       "vacuum-interval", 15,
    PRAGMA_OPERAND_OPT_EXPR, s2_pragma_f_svinterval},
{ PRAGMA_TRACE_SWEEP,           "trace-sweep", 11,
    PRAGMA_OPERAND_OPT_EXPR, s2_pragma_f_trace_sweep},
{ PRAGMA_TRACE_ASSERT,           "trace-assert", 12,
    PRAGMA_OPERAND_OPT_EXPR, s2_pragma_f_trace_assert},
{ PRAGMA_TRACE_TOKEN_STACK,      "trace-token-stack", 17,
    PRAGMA_OPERAND_OPT_EXPR, s2_pragma_f_trace_token_stack},
{ PRAGMA_BUILD_OPT,              "build-opt", 9,
    PRAGMA_OPERAND_IDENTIFIER, s2_pragma_f_build_opt},
{ PRAGMA_RC,                     "rc", 2,
    PRAGMA_OPERAND_IDENTIFIER, s2_pragma_f_rc}
};

int s2_pragma_f_refcount(s2_engine *se, s2_pragma_word const *pw,
                         s2_ptoken const * tIdent,
                         cwal_value * ev, cwal_value **rv){
  cwal_value * c;
  S2_UNUSED_ARG(pw);
  S2_UNUSED_ARG(tIdent);
  assert(!tIdent);
  assert(ev);
  c = cwal_new_integer(se->e, (cwal_int_t)cwal_value_refcount(ev));
  if(c) *rv = c;
  return c ? 0 : CWAL_RC_OOM;
}

int s2_pragma_f_svinterval(s2_engine *se, s2_pragma_word const *pw,
                           s2_ptoken const * tIdent,
                           cwal_value * ev, cwal_value **rv){
  int getVal = 0;
  int * setVal = 0;
  S2_UNUSED_ARG(tIdent);
  assert(!tIdent);
  switch(pw->type){
    case PRAGMA_SWEEP_INTERVAL:
      if(ev) setVal = &se->sweepInterval;
      getVal = se->sweepInterval;
      break;
    case PRAGMA_VACUUM_INTERVAL:
      if(ev) setVal = &se->vacuumInterval;
      getVal = se->vacuumInterval;
      break;
    default:
      s2_fatal( CWAL_RC_ASSERT, "Not possible: invalid pragma type.");
  }
  if(setVal){
    cwal_int_t const x = cwal_value_get_integer(ev);
    if(x<0){
      return cwal_exception_setf(se->e, CWAL_RC_RANGE,
                                 "pragma(%s) does not allow negative values.",
                                 pw->word);
    }
    *setVal = (int)x;
  }
  *rv = cwal_new_integer(se->e, (cwal_int_t)getVal);
  return *rv ? 0 : CWAL_RC_OOM;
}


/**
   Impl. of pragmas which get/set an int-type flag. A pointer to that
   flag's origin is passed as the 2nd argument. The other parameters
   are as for the s2_pragma_f() typedef.

   If asBool is true, the *what value and result are treated as a
   booleans, rather than as integers.
*/
static int s2_pragma_f_int_impl(s2_engine *se,
                                int * what,
                                bool asBool,
                                s2_pragma_word const *pw,
                                s2_ptoken const * tIdent,
                                cwal_value const * ev, cwal_value **rv){
  int const getVal = *what;
  S2_UNUSED_ARG(tIdent);
  assert(!tIdent);
  if(ev){
    cwal_int_t const x = asBool
      ? cwal_value_get_bool(ev)
      : cwal_value_get_integer(ev);
    if(x<0){
      return cwal_exception_setf(se->e, CWAL_RC_RANGE,
                                 "pragma(%s) does not allow negative values.",
                                 pw->word);
    }
    *what = (int)(asBool ? (x ? 1 : 0) : x);
  }
  if(asBool){
    *rv = getVal ? cwal_value_true() : cwal_value_false();
  }else{
    *rv = cwal_new_integer(se->e, (cwal_int_t)getVal);
  }
  return *rv ? 0 : CWAL_RC_OOM;
}

int s2_pragma_f_stacktrace(s2_engine *se, s2_pragma_word const *pw,
                           s2_ptoken const * tIdent,
                           cwal_value * ev, cwal_value **rv){
  return s2_pragma_f_int_impl(se, &se->flags.exceptionStackTrace, 1,
                              pw, tIdent, ev, rv);
}


int s2_pragma_f_trace_sweep(s2_engine *se, s2_pragma_word const *pw,
                            s2_ptoken const * tIdent,
                            cwal_value * ev, cwal_value **rv){
  return s2_pragma_f_int_impl(se, &se->flags.traceSweeps, 0,
                              pw, tIdent, ev, rv);
}

int s2_pragma_f_trace_assert(s2_engine *se, s2_pragma_word const *pw,
                            s2_ptoken const * tIdent,
                            cwal_value * ev, cwal_value **rv){
  return s2_pragma_f_int_impl(se, &se->flags.traceAssertions, 0,
                              pw, tIdent, ev, rv);
}

int s2_pragma_f_trace_token_stack(s2_engine *se, s2_pragma_word const *pw,
                                  s2_ptoken const * tIdent,
                                  cwal_value * ev, cwal_value **rv){
  return s2_pragma_f_int_impl(se, &se->flags.traceTokenStack, 0,
                              pw, tIdent, ev, rv);
}

static int s2_pragma_f_rc(s2_engine *se, s2_pragma_word const *pw,
                          s2_ptoken const * tIdent,
                          cwal_value * ev, cwal_value **rv){
  int code = 0;
  S2_UNUSED_ARG(pw);
  S2_UNUSED_ARG(ev);
  assert(tIdent);
  assert(!ev);
  cwal_size_t nIden = 0;
  char const * iden = s2_ptoken_cstr(tIdent, &nIden);
  if(s2_cstr_to_rc(iden, (cwal_int_t)nIden, &code)){
    *rv = cwal_new_integer(se->e, (cwal_int_t)code);
    return *rv ? 0 : CWAL_RC_OOM;
  }
  return s2_throw_ptoker(se, se->currentScript, CWAL_RC_NOT_FOUND,
                         "Unknown pragma(rc) identifier: %.*s",
                         (int)nIden, iden);
}

/**
   pragma(build-opt IDENTIFIER)

   TODO?: Make the identifier optional, in which case return an object
   containing all of the known entries. We have that readily available
   in cwal_build_info_object() but the property keys it uses differ
   from the identifiers used by this pragma.
*/
int s2_pragma_f_build_opt(s2_engine *se, s2_pragma_word const *pw,
                          s2_ptoken const * tIdent,
                          cwal_value * ev, cwal_value **rv){
  cwal_value * v = 0;
  S2_UNUSED_ARG(pw);
  S2_UNUSED_ARG(ev);
  assert(tIdent);
  assert(!ev);
  switch(s2__keyword_perfect_hash(tIdent)){
  /* First integer values... */
#define W(X) v = cwal_new_integer(se->e, (cwal_int_t)X); \
  if(!v) return CWAL_RC_OOM;                             \
  break
    case 0x028b1cb7: W(CWAL_OBASE_ISA_HASH);
    case 0x0052e906: W(CWAL_SIZE_T_BITS);
#undef W
  /* Then string values... */
#define W(X) v = cwal_new_string_value(se->e, X, cwal_strlen(X));   \
  if(!v) return CWAL_RC_OOM;                                      \
  break
    case 0x0295ed40: W(CWAL_VERSION_STRING);
    case 0x000281a8: W(CWAL_CFLAGS);
    case 0x000a2338: W(CWAL_CPPFLAGS);
  /* Then values which may or may not be #define'd... */
#undef W
    case 0x000007e8:
#if defined(DEBUG)
      v = cwal_value_true();
#else
      v = cwal_value_false();
#endif
      break;
    case 0x09740cb5:
#if defined(S2_AMALGAMATION_BUILD)
      v = cwal_value_true();
#else
      v = cwal_value_undefined();
#endif
      break;
    case 0x00014d8e:
#if defined(S2_OS_UNIX)
      v = cwal_value_true();
#else
      v = cwal_value_undefined();
#endif
      break;
    case 0x000a7660:
#if defined(S2_OS_WINDOWS)
    v = cwal_value_true();
#else
    v = cwal_value_undefined();
#endif
  }
  if(!v){
    cwal_size_t nIden = 0;
    char const * iden = s2_ptoken_cstr(tIdent, &nIden);
    return s2_throw_ptoker(se, se->currentScript, CWAL_RC_MISUSE,
                           "Unknown pragma build-opt identifier: %.*s",
                           (int)nIden, iden);
  }
  *rv = v;
  return 0;
}


/**
   If the given ptoken resolves to a pragma(WORD) identifier, its
   entry is returned, else NULL is returned. This is an O(1) search.
   See s2_ptoken_keyword() for more details.
 */
static s2_pragma_word const * s2_ptoken_pragma( s2_ptoken const * pt ){
  cwal_size_t const tlen = s2_ptoken_len(pt);
  s2_pragma_word const * rc;
#define W(X,E) rc = tlen==(cwal_size_t)sizeof(X)-1 &&                   \
    0==cwal_compare_cstr(s2_ptoken_begin(pt), tlen, X, sizeof(X)-1)     \
    ? &s2PragmaWords[E] : NULL;                                         \
  assert(rc ? E==rc->type : 1); return rc
  switch(s2__keyword_perfect_hash(pt)){
    /* Generated by s2-keyword-hasher.s2 (or equivalent): */

    case 0x00011029: W("build-opt",PRAGMA_BUILD_OPT);
    case 0x09022910: W("exception-stacktrace",PRAGMA_EXCEPTION_STACKTRACE);
    case 0x0000011e: W("rc",PRAGMA_RC);
    case 0x00008bd2: W("refcount",PRAGMA_REFCOUNT);
    case 0x00245262: W("sweep-interval",PRAGMA_SWEEP_INTERVAL);
    case 0x0008b1a6: W("trace-assert",PRAGMA_TRACE_ASSERT);
    case 0x00045dbf: W("trace-sweep",PRAGMA_TRACE_SWEEP);
    case 0x0117cb00: W("trace-token-stack",PRAGMA_TRACE_TOKEN_STACK);
    case 0x004732eb: W("vacuum-interval",PRAGMA_VACUUM_INTERVAL);

    default: break;
  }
#undef W
  return NULL;
}

int s2_keyword_f_pragma( s2_keyword const * kw, s2_engine * se,
                         s2_ptoker * pr, cwal_value **rv){
  int rc;
  s2_pragma_word const * ptag = 0;
  s2_ptoker prBody = s2_ptoker_empty;
  s2_ptoken tIdent = s2_ptoken_empty;
  s2_ptoken tPTag = s2_ptoken_empty;
  s2_ptoken const * pIdent = 0;
  s2_ptoker const * oldScript = se->currentScript;
  cwal_size_t pTagLen = 0;
  cwal_value * xrv = 0 /* pragma(tag EXPR) result for EXPR */;
  cwal_value * crv = 0 /* result of the pragma */;
  cwal_value * vHolder = 0;
  /**
     Ideas:

     pragma(blah)

     There's all sorts of stuff we could potentially do with (blah),
     potentially requiring different syntaxes, e.g. (x=y), (x), (x y).

     e.g.:

     pragma(stacktrace false)

     To enable/disable collection of stack traces (a major cause of
     line-counting, which is slow). For most testing purposes, stack
     traces are not needed - they're needed primarily in app-level
     code.

     It would seem we need both getter and setter forms for some
     constructs.
  */
  rc = s2_next_token( se, pr, 0, 0 );
  if(rc) return rc;
  else if(S2_T_ParenGroup!=pr->token.ttype){
    return s2_err_ptoker(se, pr, CWAL_SCR_SYNTAX,
                         "Expecting (...) after '%s'.",
                         kw->word);
  }
  if(se->skipLevel>0){
    *rv = cwal_value_undefined();
    return 0;
  }
  
  rc = s2_ptoker_sub_from_toker(pr, &prBody);
  if(!rc){
    prBody.flags |= S2_T10N_F_IDENTIFIER_DASHES;
    rc = s2_next_token(se, &prBody, 0, 0);
    prBody.flags &= ~S2_T10N_F_IDENTIFIER_DASHES;
  }
  if(rc) goto end;
  else if(S2_T_Identifier!=prBody.token.ttype){
    rc = s2_err_ptoker(se, &prBody, CWAL_SCR_SYNTAX,
                       "Expected pragma command in %s(...).",
                       kw->word);
    goto end;
  }

  se->currentScript = &prBody;
  tPTag = prBody.token;
  pTagLen = s2_ptoken_len(&tPTag);
  assert(pTagLen > 0);
  ptag = s2_ptoken_pragma(&tPTag);
  if(!ptag){
    rc = s2_err_ptoker(se, &prBody, CWAL_SCR_SYNTAX,
                       "Unknown pragma in %s(%.*s ...).",
                       kw->word, (int)pTagLen, s2_ptoken_begin(&tPTag));
    goto end;
  }
  assert(ptag->word && *ptag->word);
  assert(ptag->call);

  rc = s2_next_token(se, &prBody, 0, 0);
  if(rc) goto end;
  
  switch(ptag->opMode){
    case PRAGMA_OPERAND_NONE:
      /* pragma(tag) */
      if(!s2_ptoker_is_eof(&prBody)){
        rc = s2_err_ptoker(se, &prBody, CWAL_SCR_SYNTAX,
                           "Unexpected token in pragma %s(%.*s ...).",
                           kw->word, (int)pTagLen, s2_ptoken_begin(&tPTag));
        break;
      }
      break;
    case PRAGMA_OPERAND_OPT_EXPR:
      /* pragma(tag) or pragma(tag EXPR) */
      if(s2_ptoker_is_eof(&prBody)){
        break;
      }
      CWAL_SWITCH_FALL_THROUGH;
    case PRAGMA_OPERAND_EXPR:{
      /* pragma(tag EXPR) */
      s2_ptoker_putback(&prBody);
      rc = s2_eval_expr_impl(se, &prBody, 0, 0, &xrv);
      if(rc) goto end;
      else if(!xrv){
        rc = s2_err_ptoker(se, &prBody, CWAL_SCR_SYNTAX,
                           "Empty expression is not permitted here.");
        goto end;
      }
      break;
    }
    case PRAGMA_OPERAND_OPT_IDENTIFIER:
      /* pragma(tag) or pragma(tag IDENTIFIER) */
      if(s2_ptoker_is_eof(&prBody)){
        break;
      }
      CWAL_SWITCH_FALL_THROUGH;
    case PRAGMA_OPERAND_IDENTIFIER:
      /* pragma(tag IDENTIFIER) */
      if(S2_T_Identifier != prBody.token.ttype){
        rc = s2_err_ptoker(se, &prBody, CWAL_SCR_SYNTAX,
                           "The %s(%.*s ...) pragma requires an "
                           "IDENTIFIER argument",
                           kw->word, pTagLen, s2_ptoken_begin(&tPTag));
        goto end;
      }
      tIdent = prBody.token;
      pIdent = &tIdent;
      break;
    default:
      s2_fatal( CWAL_RC_ASSERT, "Cannot happen: unknown pragma operand type.");
  }

  if(xrv){
    vHolder = cwal_new_unique(se->e, xrv);
    if(!vHolder){
      rc = CWAL_RC_OOM;
      cwal_refunref(xrv);
      xrv = 0;
      goto end;
    }
    cwal_value_ref(vHolder);
    cwal_value_make_vacuum_proof(vHolder, 1);
  }
  rc = ptag->call(se, ptag, pIdent, xrv, &crv);

  end:
  s2_ptoker_finalize(&prBody);
  if(crv){
    cwal_value_ref(crv) /* needed in case crv == xrv */;
    if(rc) cwal_value_unref(crv);
    else *rv = crv;
    cwal_value_unref(vHolder);
    if(!rc) cwal_value_unhand(crv);
    crv = 0;
  }else{
    if(!rc){
      *rv = cwal_value_undefined();
    }
    cwal_value_unref(vHolder);
  }
  se->currentScript = oldScript;
  return rc;
}

int s2_keyword_f_ukwd( s2_keyword const * kw, s2_engine * se, s2_ptoker * pr, cwal_value **rv){
  int rc = 0;
  s2_ukwd * uk = 0;
  if(se->skipLevel>0){
    *rv = cwal_value_undefined();
    return 0;
  }
  uk = s2__ukwd(se);
  S2_UNUSED_ARG(pr);
  if(uk && uk->h){
    cwal_value * v;
    v = cwal_hash_search(uk->h, kw->word, kw->wordLen);
    if(v){
      *rv = v;
    }else{
      rc = cwal_exception_setf(se->e, CWAL_RC_NOT_FOUND,
                               "Could not find %.*s keyword object.",
                               (int)kw->wordLen, kw->word);
    }
  }
  return rc;
}

/**
   If pt's [begin,end) range corresponds to a keyword, its entry from
   S2_KWDS is returned, else NULL is returned.

   This is an O(1) search, requiring, at most, generation of 1 hash
   code and (on a hash match) 1 string comparison.
*/
static s2_keyword const * s2__cstr_keyword( char const * name,
                                            cwal_size_t nameLen){
  s2_ptoken pt = s2_ptoken_empty;
  s2_ptoken_begin_set(&pt, name);
  s2_ptoken_end_set(&pt, name + nameLen);
  return s2_ptoken_keyword(&pt)/*NOT keyword2()!*/;  
}

#if 0
static s2_keyword const * s2__value_keyword( s2_engine * se, cwal_value * v ){
  cwal_size_t nameLen = 0;
  char const * name = cwal_value_get_cstr(v, &nameLen);
  if(name){
      s2_ptoken pt = s2_ptoken_empty;
      s2_ptoken_begin_set(pt, name);
      s2_ptoken_end_set(&pt, name + nameLen);
      return s2_ptoken_keyword2(se, &pt);
  }else{
    return 0;
  }
}
#endif

static int s2_cstr_is_identifier(char const * z, cwal_size_t len){
  char const * tail = 0;
  if(!s2_read_identifier(z, z+len, &tail)) return 0;
  return tail==z+len;
}

/**
   Creates a UKWD entry for the given key/value.

   See the public API docs for s2_define_ukwd() for the details, with
   the addition that CWAL_RC_TYPE is returned if k is not a stringable
   type.
*/
static int s2_define_ukwd_v(s2_engine * se, cwal_value * k, cwal_value * v){
  s2_ukwd * uk = s2__ukwd(se);
  s2_keyword * kw = 0;
  int rc;
  cwal_size_t nameLen = 0;
  char const * name;
  cwal_size_t const maxNameLen = 100
    /*This upper limit is essentially arbitrary, but we *must* reject
      anything longer than an unsigned short because kw->wordLen will
      hold the length. Also, long names have longer comparison times,
      and we frequently check the UKWD list during evaluation, so
      keeping them shorter is a good thing.*/;
  assert(v);
  if(cwal_value_undefined()==v){
    return s2_engine_err_set(se, CWAL_RC_UNSUPPORTED,
                             "The undefined value is not legal for use as "
                             "a define() value.");
  }
  name = cwal_value_get_cstr(k, &nameLen);
  if(!name){
    return s2_engine_err_set(se, CWAL_RC_TYPE,
                             "define() requires a string key.");
  }else if(!nameLen){
    return s2_engine_err_set(se, CWAL_RC_RANGE,
                             "define() key name may not be empty");
  }else if(nameLen > maxNameLen){
    return s2_engine_err_set(se, CWAL_RC_RANGE,
                             "define() key name is too long (max length=%d).",
                             (int)maxNameLen);
  }else if(!s2_cstr_is_identifier(name, nameLen)){
    return s2_engine_err_set(se, CWAL_SCR_SYNTAX,
                             "define() key must be a legal identifier.");
  }else if(s2__cstr_keyword(name, nameLen)){
    return s2_engine_err_set(se, CWAL_RC_ACCESS,
                             "define() may not override a built-in "
                             "keyword.");
  }
  else if(!uk){
    uk = cwal_malloc( se->e, sizeof(s2_ukwd));
    if(!uk) return s2_engine_err_set(se, CWAL_RC_OOM, 0);
    memset(uk, 0, sizeof(s2_ukwd));
    se->ukwd = uk;
  }
  if(!uk->h){
    cwal_value * hv;
    uk->h = cwal_new_hash(se->e, 13);
    if(!uk->h) return s2_engine_err_set(se, CWAL_RC_OOM, 0);
    hv = cwal_hash_value(uk->h);
    cwal_value_ref(hv);
    rc = s2_stash_set_v(se, hv, hv)
        /* To give the hash permanent lifetime */;
    cwal_value_unref(hv);
    if(rc){
      uk->h = 0;
      return rc;
    }
  }
  if(uk->count<=uk->alloced){
    unsigned const n = uk->alloced ? uk->alloced*3/2 : 10;
    void * m = cwal_realloc(se->e, uk->list, n * sizeof(s2_keyword));
    if(!m) return s2_engine_err_set(se, CWAL_RC_OOM, 0);
    uk->list = (s2_keyword *)m;
    uk->alloced = n;
  }
  {
    rc = cwal_hash_insert_with_flags_v(uk->h, k, v, 0, CWAL_VAR_F_CONST);
    if(rc){
      return CWAL_RC_ALREADY_EXISTS==rc
        ? s2_engine_err_set(se, rc, "define() key already exists.")
        : s2_engine_err_set(se, rc, "Hash insertion of define() key "
                            "failed: %s",
                            cwal_rc_cstr(rc))
        ;
    }
    kw = &uk->list[uk->count];
    kw->wordLen = (unsigned short)nameLen;
    kw->word = name
      /* We know those bytes now live forever in uk->h,
         so there's no lifetime issue. */;
    ++uk->count;
    kw->id = S2_T_KeywordUKWD;
    kw->word = name;
    kw->call = s2_keyword_f_ukwd;
    kw->allowEOLAsEOXWhenLHS = 0;
    qsort( uk->list, uk->count, sizeof(s2_keyword), cmp_ukwd_kw );
    cwal_hash_grow_if_loaded(uk->h, 0.7)/*ignore error - not fatal*/;
    return 0;
  }
}

int s2_define_ukwd(s2_engine * se, char const * name,
                   cwal_int_t nameLen, cwal_value * v){
  int rc;
  cwal_value * const k =
    cwal_new_string_value(se->e, name,
                          nameLen>0
                          ? (cwal_midsize_t)nameLen
                          : cwal_strlen(name));
  if(k){
    cwal_value_ref(k);
    rc = s2_define_ukwd_v(se, k, v);
    cwal_value_unref(k);
  }else{
    rc = s2_engine_err_set(se, CWAL_RC_OOM, 0);
  }
  return rc;
}

/**
   cwal_callback_f() impl which wraps s2_define_ukwd(). Script-side
   signature:


   mixed define(string name[, mixed value])

   If called with one argument, it returns the defined value for the
   given key (or the undefined value), else it defines the given
   key/value pair.

   It returns its 2nd argument.
*/
static int s2_cb_define_ukwd( cwal_callback_args const * args, cwal_value **rv ){
  s2_engine * se = s2_engine_from_args(args);
  if((!args->argc || args->argc>2)
     && !cwal_value_is_string(args->argv[0])){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting (string name[, value]) arguments.");
  }
  if(1==args->argc){
    s2_ukwd * uk = s2__ukwd(se);
    if(uk){
      cwal_value * v = cwal_hash_search_v(uk->h, args->argv[0]);
      *rv = v ? v : cwal_value_undefined();
    }else{
      *rv = cwal_value_undefined();
    }
    return 0;
  }else{
    int rc = s2_define_ukwd_v(se, args->argv[0], args->argv[1]);
    if(rc){
      if(s2__err(se).code == rc){
        rc = s2_throw_err(se, 0, 0, 0, 0);
      }
    }
    else{
      *rv = args->argv[1];
    }
    return rc;
  }
}

int s2_keyword_f_define( s2_keyword const * kw, s2_engine * se,
                         s2_ptoker * pr, cwal_value **rv){
  int rc = 0;
  cwal_value * v = se->skipLevel>0
    ? cwal_value_undefined()
    : se->cache.define;
  if(!v){
    cwal_value * vDef = 0;
    S2_UNUSED_ARG pr;
    vDef = cwal_new_function_value(se->e, s2_cb_define_ukwd, 0, 0, 0);
    if(!vDef){
      return CWAL_RC_OOM;
    }
    cwal_value_ref(vDef);
    rc = s2_stash_set(se, kw->word, vDef)
      /* Moves it to the top scope and implicitly makes it
         vacuum-proof. */;
    cwal_value_unref(vDef);
    if(!rc){
      /* vDef is held by/in se->stash. */
      v = se->cache.define = vDef;
      s2_seal_container(vDef, 1);
    }
  }/* end of first-call initialization */
  if(!rc) *rv = v;
  return rc;
}
#undef s2__dump_token
#undef s2__ukwd
#undef s2__ukwd_key
#undef s2__keyword_perfect_hash
#undef s2__err
#undef MARKER
