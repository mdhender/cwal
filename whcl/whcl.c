/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
/*
  License: same as cwal. See cwal.h resp. libcwal.h for details.
*/
#include "internal.h"

#include <assert.h>
#include <string.h>
#include <stdlib.h>

#if !defined(WHCL_HAVE_SIGACTION)
#  define WHCL_HAVE_SIGACTION 0
#endif
#define WHCL_USE_SIGNALS WHCL_HAVE_SIGACTION
#if WHCL_USE_SIGNALS
#  include <signal.h> /* sigaction(), if our feature macros are set right */
#endif

#include <stdio.h>
#if 1
#define MARKER(pfexpr) if(1) printf("MARKER: %s:%d:\t",__FILE__,__LINE__); if(1) printf pfexpr
#else
#define MARKER if(0) printf
#endif

const whcl_engine whcl_engine_empty = whcl_engine_empty_m;
const whcl_init_opt whcl_init_opt_empty = whcl_init_opt_empty_m;
const whcl__sweep_guard whcl__sweep_guard_empty = whcl__sweep_guard_empty_m;
static const whcl_scope whcl__scope_empty = {
  /*cs*/cwal_scope_empty_m,
  NULL/*props*/,NULL/*evalHolder*/,NULL/*dotHolder*/,
  0U/*flags*/,0U/*level*/
};
const whcl__stoken whcl__stoken_empty = whcl__stoken_empty_m;
const whcl__stoken_stack whcl__stoken_stack_empty = whcl__stoken_stack_empty_m;
const whcl__estack whcl__estack_empty = whcl__estack_empty_m;
const whcl__strace_entry whcl__strace_entry_empty = whcl__strace_entry_empty_m;
const whcl_path_toker whcl_path_toker_empty = whcl_path_toker_empty_m;

#if WHCL_USE_SIGNALS
static whcl_engine * whclInterruptable = 0;
#endif

/**
   cwal_engine_vtab::hook::scope_push() hook. state must be
   a (whcl_engine*). 
*/
static int whcl__scope_hook_push( cwal_scope * const s, void * state );
/**
   cwal_engine_vtab::hook::scope_pop() hook. state must be
   a (whcl_engine*). 
*/
static void whcl__scope_hook_pop( cwal_scope const * s, void * state );

#if 0
/* Our "command" dispatching system "misinteracts" with how we used to
   use the pre/post callback hooks, so that code has been moved
   elsewhere for the time being (and may well stay there). */
static int whcl__callback_hook_pre(cwal_callback_args const * args,
                                  void * state);
static int whcl__callback_hook_post(cwal_callback_args const * args,
                                   void * state, int fRc, cwal_value * rv);
#endif

/**
   If ch is a valid character for an WHCL variable name then ch is
   returned, else 0 is returned. If isFirstChar is true then
   digits are excluded, else they are permitted.

   In WHCL, valid variable names are composed of:

   - ASCII letters, underscores, and decimal digits, but may not start
   with a decimal digit.

   - Any UTF-8 character outside of the ASCII range.
*/
static int whcl__is_var_char(int ch, bool isFirstChar);

/**
   Fetches the next token from t. On success, returns 0 and sets
   t->token to the new token state. On error returns non-0 and sets
   t->errMsg to a description of the problem.

   This updates t->currentLine and t->currentCol, noting that those
   only work so long as t is scanned linearly, with no use of putback
   tokens or some such.
*/
static int whcl_stoken_provider_f_whcl(whcl_script * const ct, tok1_izer * const pt,
                                    void * state);

/**
   If the given token array (containing nTok tokens) conforms to bic's
   minArgs/maxArgs, returns 0, else an exception or error (still
   undecided which) is triggered and that result is returned.
*/
static int whcl__bic_check_argc(whcl_engine * const el,
                                whcl__bic const * const bic,
                                whcl_script * const ct,
                                whcl_stoken const * const * const tokens,
                                uint16_t nTok);

/**
   If ttype is one of the values which represents a builtin command,
   that command's definition is returned, else NULL is returned. This
   is an O(1) operation.
*/
static whcl__bic const * whcl__ttype_get_bic(int ttype);

static cwal_value * whcl__vsym_v_proxy(whcl_engine * const el,
                                       cwal_value * self,
                                       cwal_value const * const key,
                                       cwal_value **foundIn);

/** Fetches the various dotop members and returns the number of them
    which were fetched and are non-NULL. Any argument after the
    first may be NULL to skip that element. */
static unsigned char whcl__dotop_get( whcl_engine * const el,
                                      cwal_value ** lhs,
                                      cwal_value ** key,
                                      cwal_value ** holder ){
  cwal_tuple const * const tu = el->scopes.current->dotHolder;
  unsigned char rc = 0;
  if(lhs && (*lhs = tu ? cwal_tuple_get(tu, 0) : NULL)) ++rc;
  if(key && (*key = tu ? cwal_tuple_get(tu, 1): NULL)) ++rc;
  if(holder && (*holder = tu ? cwal_tuple_get(tu, 2) : NULL)) ++rc;
  return rc;
}

/**
   If the string s matches a builtin command name, its definition is returned,
   else NULL is returned. This is an O(1) op, requiring only a hash of s
   and a single lookup.
*/
static whcl__bic const * whcl__bic_search(char const *s, uint16_t n);

#define EVH el->scopes.current->evalHolder
cwal_size_t whcl__holder_len(whcl_engine const * const el){
  return EVH
    ? cwal_array_length_get(EVH)
    : 0;
}
void whcl__holder_truncate(whcl_engine * const el, cwal_size_t len,
                           cwal_value * const keepThis){
  if(EVH){
#if 0
    /* Just for helping in tracking down errant value lifetime
       issues. */
    if(el || len || keepThis){/*unused*/}
#else
    assert(cwal_array_length_get(EVH) >= len);
    if(keepThis) cwal_ref(keepThis);
    cwal_array_length_set(EVH, len);
    if(keepThis) cwal_unhand(keepThis);
#endif
  }
}
#undef EVH

static char const * whcl__rc_cstr(int rc){
  switch(rc){
#define CASE(X) case X: return #X
    CASE(WHCL_RC_END_EACH_ITERATION);
    CASE(WHCL_RC_NO_COMMANDS);
#undef CASE
    default:
      return NULL;
  }
}

static void whsh_static_init(void){
  static int rcFallback = 0;
  if(!rcFallback && 1==++rcFallback){
    /* yes, there's a small race condition here, but it's not tragic
       except in a 1-in-a-bazillion case where X instances happen to
       successfully race here, where X is the static limit of
       cwal_rc_cstr_f fallbacks. */
    cwal_rc_cstr_fallback(whcl__rc_cstr);
  }
}

static char const * whcl__type_name_proxy( cwal_value const * v,
                                           cwal_size_t * len ){
  cwal_value const * tn = cwal_prop_get(v, "__typename", 10);
  return tn ? cwal_value_get_cstr(tn, len) : NULL;
}

int whcl_engine_init( whcl_engine * const el, cwal_engine * const ec,
                      whcl_init_opt const * opt){
  int rc = 0;

  whsh_static_init();
  if(opt){/*currently unused*/}
  assert(!el->ec || (el->ec == ec));
  *el = whcl_engine_empty;
  el->ec = ec;
  
  assert(el->ec->current);
  assert(el->ec->current==&el->ec->topScope);

  rc = cwal_engine_client_state_set(ec, el, &whcl_engine_empty, NULL);
  if(rc) goto end;
  cwal_scope_pop(el->ec) /* we need to take over the top scope */;
  if(!whcl__scope_push(el, 0)){WHCL__WARN_OOM; rc = CWAL_RC_OOM; goto end;}
  assert(NULL!=el->scopes.blocks);
  /* Don't install the scope push/pop hooks until after we've replaced
     the top scope with our own. */
  ec->vtab->hook.scope_push = whcl__scope_hook_push;
  ec->vtab->hook.scope_pop = whcl__scope_hook_pop;
  ec->vtab->hook.scope_state = el;
#if 0
  {
    cwal_callback_hook hook = {el, whcl__callback_hook_pre, whcl__callback_hook_post};
    cwal_callback_hook_set(ec, &hook);
  }
#endif
  assert(1 == el->scopes.level);
  el->scopes.topCwal = &el->scopes.current->cs;
  assert(NULL != el->scopes.topCwal);
  cwal_engine_type_name_proxy( ec, whcl__type_name_proxy );
  { /* Set up el->cache.stash... */
    el->cache.stash = cwal_new_hash(ec, 47);
    if(!el->cache.stash){WHCL__WARN_OOM;rc = CWAL_RC_OOM;goto end;}
    cwal_value * const sv = cwal_hash_value(el->cache.stash);
    cwal_ref(sv);
    cwal_value_make_vacuum_proof(sv, true);
  }
  {
    /* We need some strings as keys in a few places, so put a copy in
       the stash.

       Reminder: it/they're owned by the stash.
    */
#define STASHVAL(MEMBER,VAL)                    \
  do{                                                               \
    if(!(el->cache.MEMBER = VAL)) {                                 \
      WHCL__WARN_OOM;                                                \
      rc = CWAL_RC_OOM;                                              \
    }else{                                                           \
      cwal_ref(el->cache.MEMBER);                                     \
      rc = whcl_stash_set_v( el, el->cache.MEMBER, el->cache.MEMBER ); \
      cwal_unref(el->cache.MEMBER);                                   \
      if(rc) el->cache.MEMBER = NULL;                                 \
    }                                                                 \
  } while(0); if(rc) goto end
#define STASHKEY(MEMBER,STR)                                            \
    STASHVAL(MEMBER,cwal_new_string_value(ec, (STR), (cwal_size_t)(sizeof(STR)-1)))

    STASHKEY(keyPrototype,"__prototype");
    STASHKEY(keyArgv,"argv");
    STASHKEY(keyCommand,"__command");
    STASHKEY(keyLine,"line");
    STASHKEY(keyColumn,"column");
    STASHKEY(keyScript,"script");
    STASHKEY(keyStackTrace,"stackTrace");
    STASHKEY(keyUsing,"using");
    STASHKEY(keyTypename,"__typename");
    STASHKEY(keyThis,"this");
    STASHKEY(keyNewCtor,"__new");
    STASHVAL(scriptNames,cwal_new_object_value(el->ec));
    STASHVAL(vWhcl,cwal_new_object_value(el->ec));
#undef STASHKEY
#undef STASHVAL
  }
  assert(0==rc);
  rc = whcl_install_command_cb(el, el->cache.vWhcl);
  if(0==rc){
    cwal_value * v = cwal_new_string_value(ec, "whcl", 4);
    if(!v){ WHCL__WARN_OOM; rc = CWAL_RC_OOM; goto end; }
    cwal_ref(v);
    rc = whcl_install_typename_v(el, el->cache.vWhcl, v); 
    cwal_unref(v);
    if(rc) goto end;
    v = cwal_new_object_value(el->ec);
    if(!v){ WHCL__WARN_OOM; rc = CWAL_RC_OOM; goto end; }
    cwal_ref(v);
    cwal_value_prototype_set(v, NULL)
      /* Keep whcl.client from inheriting any methods. The client is
         free to replace the prototype if they like. */;
    rc = whcl__install_into_whcl(el, "client", v);
    cwal_unref(v);
    if(rc) goto end;
  }
  end:
  if(CWAL_RC_OOM==rc){
    WHCL__WARN_OOM;
  }
  return rc;
}

void whcl_engine_finalize( whcl_engine * const el){
  cwal_engine * const ec = el->ec;
  //MARKER(("%s()\n", __func__));
#if WHCL_USE_SIGNALS
  if(whclInterruptable==el) whclInterruptable = NULL;
#endif
  if(!ec){
    assert(NULL==el->scopes.blocks);
    return;
  }
  cwal_exception_set(ec, NULL);
  cwal_propagating_set(ec, NULL);
  cwal_buffer_clear(ec, &el->escBuf);
  whcl__stoken_stack_clear(el, &el->recycler.stok, false);
  whcl__estack_clear(el, &el->estack, false);
  el->recycler.scriptFuncs.max = 0;
  while(el->recycler.scriptFuncs.head){
    assert(el->recycler.scriptFuncs.count);
    whcl__func_free(el, el->recycler.scriptFuncs.head, false);
  }
  assert(NULL==el->recycler.scriptFuncs.head);
  assert(0==el->recycler.scriptFuncs.count);
  cwal_list_reserve(ec, &el->ob, 0);
  cwal_list_reserve(ec, &el->with.holder, 0);
  extern void whcl__modules_close( whcl_engine * const)/*in mod.c*/;
  whcl__modules_close(el);
  while(el->scopes.current) whcl_scope_pop(el, NULL, NULL);
  for(uint16_t i = 0; i < el->scopes.blockCount; ++i){
    memset(el->scopes.blocks[i], 0,
           sizeof(whcl_scope) * whcl__scope_block_size);
    cwal_free(el->ec, el->scopes.blocks[i]);
  }
  cwal_free(el->ec, el->scopes.blocks);
  cwal_engine_destroy(ec);
  *el = whcl_engine_empty;
}

/**
   Replaces el->with.current with the given value and returns the previous
   value.
*/
static cwal_value * whcl__replace_with(whcl_engine * const el, cwal_value * const v){
  cwal_value * const x = el->with.current;
  el->with.current = v;
  return x;
}

int whcl_stash_set_v( whcl_engine * const el, cwal_value * const key, cwal_value * const v ){
  int rc = 0;
  if(!key || !v) return CWAL_RC_MISUSE;
  rc = cwal_hash_insert_v(el->cache.stash, key, v, true);
  if(!rc) rc = cwal_hash_grow_if_loaded(el->cache.stash, 0.75);
  return rc;
}

int whcl_stash_set( whcl_engine * const el, char const * const key,
                    cwal_value * const v ){
  int rc;
  cwal_value * const kv = cwal_new_string_value2(el->ec, key, -1);
  if(kv){
    cwal_value_ref(kv);
    rc = whcl_stash_set_v(el, kv, v);
    cwal_value_unref(kv);
  }else{
    rc = CWAL_RC_OOM;
    WHCL__WARN_OOM;
  }
  return rc;
}

cwal_value * whcl_stash_get2( whcl_engine * const el, char const * const key,
                              cwal_size_t keyLen){
  return (key && *key)
    ? cwal_hash_search(el->cache.stash, key, keyLen )
    : NULL;
}

cwal_kvp * whcl_stash_get2_kvp( whcl_engine * const el, char const * const key,
                                cwal_size_t keyLen){
  return (key && *key)
    ? cwal_hash_search_kvp( el->cache.stash, key, keyLen )
    : NULL;
}

cwal_value * whcl_stash_get( whcl_engine * const el, char const * const key ){
  return whcl_stash_get2(el, key, cwal_strlen(key));
}

cwal_value * whcl_stash_get_v( whcl_engine * const el,
                               cwal_value const * const key ){
  return key ? cwal_hash_search_v( el->cache.stash, key ) : NULL;
}

cwal_engine * whcl_engine_cwal(whcl_engine * const el){
  return el->ec;
}


whcl_engine * whcl_engine_from_state( cwal_engine * const e ){
  return (whcl_engine *) cwal_engine_client_state_get( e, &whcl_engine_empty );
}

whcl_engine * whcl_engine_from_args( cwal_callback_args const * const args ){
  return (whcl_engine *) cwal_engine_client_state_get( args->engine, &whcl_engine_empty );
}

/**
   If NDX is within range of (whcl_engine*)EL's allocated scope
   blocks, this evals to the corresponding (whcl_scope*), else it
   evals to NULL. Be aware that each argument may be evaluated more
   than once.
*/
#define whcl__scope_at(EL,NDX)                                          \
  (((((uint16_t)NDX)) && (((uint16_t)NDX)) <=                           \
    ((EL)->scopes.blockCount * whcl__scope_block_size))                 \
   ? (&(EL)->scopes.blocks                                              \
      [((((uint16_t)NDX))-1) / whcl__scope_block_size]                  \
      [((uint16_t)NDX) % whcl__scope_block_size])                       \
   : (whcl_scope*)NULL)

/**
   Internal impl of whcl_scope_for_level(). If LVL is within range of
   (whcl_engine*)EL's current scope level, this evals to the
   corresponding (whcl_scope*), else it evals to NULL. Be aware that
   each argument may be evaluated more than once.
*/
#define whcl__scope_for_level(EL,LVL)     \
  (((LVL) && (LVL)<=((EL)->scopes.level)) \
   ? whcl__scope_at(EL,LVL)               \
   : (whcl_scope*)NULL)

whcl_scope * whcl_scope_for_level( whcl_engine const * const el,
                                   uint16_t level ){
  return whcl__scope_for_level(el, level);
}

#define whcl__scope_current(EL)                     \
  whcl__scope_for_level((EL), (EL)->scopes.level)

whcl_scope * whcl_scope_current( whcl_engine const * const el ){
  return whcl__scope_current(el);
}

whcl_scope * whcl_scope_parent( whcl_engine const * const el,
                                 whcl_scope * s ){
  if(!s) s = el->scopes.current;
  return s ? whcl__scope_for_level(el, el->scopes.level-1) : NULL;
}

uint16_t whcl_scope_level(whcl_scope const * const s){
  return s->level;
}

#define whcl__rescopeS(S,v)                    \
  cwal_value_rescope(&S->cs, v)
#define whcl__rescopeE(E,v)                    \
  whcl__rescopeS(E->scopes.current, v)

cwal_value * whcl_scope_props(whcl_engine * const el,
                              whcl_scope * const sc_){
  whcl_scope * const sc = sc_ ? sc_ : whcl__scope_current(el);
  if(!sc->props){
    sc->props = cwal_new_object_value(el->ec);
    if(!sc->props){WHCL__WARN_OOM; return NULL;}
    whcl__rescopeS(sc, sc->props);
    cwal_value_make_vacuum_proof(sc->props, true);
    cwal_value_prototype_set(sc->props, NULL);
    cwal_ref(sc->props);
  }
  return sc->props;
}

int whcl__scope_import_props(whcl_engine * const el, whcl_scope * scel,
                             cwal_value * const src){
  if(!scel) scel = whcl__scope_current(el);
  cwal_value * const p = whcl_scope_props(el, scel);
  int const rc = p ? cwal_props_copy(src, p) : CWAL_RC_OOM;
  return rc
    ? whcl_err_set(el, rc, "Copying properties failed with code %s",
                   cwal_rc_cstr(rc))
    : 0;
}

void whcl_scope_pop(whcl_engine * const el,
                    whcl_scope * const sc_,
                    cwal_value * const propagate){
  assert(el->scopes.level);
  if(!el->scopes.level){
    assert(!"Don't call pop without a matching push!");
    return;
  }
  if(sc_){
    assert(sc_ == el->scopes.current
           && "Else scope push/pop mismatch.");
  }
  whcl_scope * const sc = sc_ ? sc_ : el->scopes.current;
  assert(el->scopes.level == sc->level
         && "Else internal scope level mismanagement.");
#if 1
  whcl_scope * const sanity = el->scopes.current;
  if(&sanity->cs != &sc->cs){
    whcl__fatal(CWAL_RC_ASSERT,
                "Scope mismatch: el->scopes.current=%d, sc->cs=%d",
                (int)sanity->level, (int)sc->cs.level);
    /*does not return*/
  }
  cwal_scope * const sanity2 = &el->scopes.current->cs;
  if(sanity2 != el->ec->current){
    whcl__fatal(CWAL_RC_ASSERT,
                "Scope mismatch: el->ec->current=%d, sc->cs=%d",
                (int)sanity2->level, (int)sc->cs.level);
    /*does not return*/
  }
  assert(el->ec->current == &sc->cs);
#endif
  if(propagate){
    assert(el->scopes.level>1);
    cwal_ref(propagate);
  }
  if(sc->props){
    cwal_value_make_vacuum_proof(sc->props, false);
    assert(cwal_value_refcount(sc->props)>=1);
    cwal_unref(sc->props);
  }
  if(sc->dotHolder){
    cwal_value * const v = cwal_tuple_value(sc->dotHolder);
    cwal_value_make_vacuum_proof(v, false);
    assert(cwal_value_refcount(v)>=1);
    cwal_unref(v);
  }
  if(propagate){
    cwal_unhand(propagate);
  }
  --el->scopes.level;
  cwal_scope_pop2(el->ec, el->scopes.level ? propagate : NULL);
  *sc = whcl__scope_empty;
  el->scopes.current = whcl__scope_for_level(el, el->scopes.level);
}

/**
   Ensures that el->scopes.list has enough space for pushing at least
   one new scope. Returns 0 on success, CWAL_RC_OOM on allocation error.
*/
static int whcl__scopes_reserve( whcl_engine * const el ){
  if(whcl__scope_at(el, el->scopes.level+1)){
    return 0;
  }
  uint16_t const nb = el->scopes.blockCount + 1;
  whcl_scope ** blocks =
    (whcl_scope**)cwal_realloc(el->ec, el->scopes.blocks,
                               nb * whcl__scope_block_size
                               * sizeof(whcl_scope*));
  if(!blocks){ WHCL__WARN_OOM; return CWAL_RC_OOM; }
  whcl_scope * block = (whcl_scope*)cwal_malloc(el->ec, whcl__scope_block_size
                                                * sizeof(whcl_scope));
  if(!block){
    cwal_free2(el->ec, blocks, nb * whcl__scope_block_size);
    WHCL__WARN_OOM; return CWAL_RC_OOM;
  }
  el->scopes.blocks = blocks;
  blocks[el->scopes.blockCount] = block;
  ++el->scopes.blockCount;
  memset(block, 0, whcl__scope_block_size * sizeof(whcl_scope));
  return 0;
}

whcl_scope * whcl__scope_push(whcl_engine * const el, cwal_flags16_t flags ){
  el->scopes.nextFlags = 0;
  if(whcl__scopes_reserve(el)) return NULL;
  assert(el->scopes.blockCount>0);
  if(el->scopes.current){
    assert(el->scopes.level == el->scopes.current->level);
  }
  if(el->scopes.maxLevel < ++el->scopes.level){
    el->scopes.maxLevel = el->scopes.level;
  }
  whcl_scope * const els = whcl__scope_current(el);
  *els = whcl__scope_empty;
  els->flags = flags;
  els->level = el->scopes.level;
  el->scopes.current = els;
  cwal_scope * cs = &els->cs;
  cwal_scope_push(el->ec, &cs) /* cannot fail */;
#if 0
  MARKER(("pushed whcl_scope #%d/%d\n",
          (int)el->scopes.level, (int)els->level));
#endif
  return els;
}

whcl_scope * whcl_scope_push(whcl_engine * const el){
  return whcl__scope_push(el, el->scopes.nextFlags);
}

whcl_scope * whcl__scope_push2(whcl_engine * const el){
  return whcl__scope_push(el, el->scopes.current->flags
                          & WHCL__SCOPE_F_PUSH2);
}

int whcl__scope_hook_push( cwal_scope * s, void * state ){
  //MARKER(("Pushing cwal scope #%d\n", (int)s->level));
  //whcl_engine * const el = (whcl_engine *)state;
  if(s || state){/*unused*/}
  //whcl_engine * const el = (whcl_engine *)state;
  //return whcl_scope_push(el, 0) ? 0 : CWAL_RC_OOM;
  return 0;
}

void whcl__scope_hook_pop( cwal_scope const * s, void * state ){
  //MARKER(("Popping cwal scope #%d\n", (int)s->level));
  whcl_engine * const el = (whcl_engine *)state;
  if(s){/*unused*/}
  //assert(el->scopes.level); assert(el->scopes.current);
  if(1 == s->level){
    /* Final scope is popping: cwal_engine is shutting down. */
    /* The following values are owned by this cwal scope scope and
       were (likely) already destroyed via the whcl_scope_pop() which
       (likely) triggered this. */
    el->cache.stash = NULL;
    el->with.current = NULL;
  }
}

/** @internal 

    Expects cachedKey to be an entry from whcl_engine::cache or NULL. ckey/keyLen describe
    an input string to compare against the final argument. This function returns
    true (non-0) if the final argument compares string-equivalent to (ckey,keyLen)
    OR is the same pointer address as the 2nd argument, else it returns false.
*/
static bool whcl_v_is_cached_string( cwal_value const * const cachedKey,
                                     char const * const ckey, cwal_size_t keyLen,
                                     cwal_value const * const key ){
  if(cachedKey == key) return true;
  else{
      cwal_size_t xkeyLen = 0;
      char const * xkey = cwal_value_get_cstr(key, &xkeyLen);
      return (xkeyLen==keyLen && *ckey==*xkey
              && 0==memcmp(ckey, xkey, keyLen));
  }
}

#if 0
bool whcl__v_is_command_string( whcl_engine const * const el, cwal_value const * const key ){
  return whcl_v_is_cached_string( el->cache.keyValue, "__command", 9, key );
}
#endif

static bool whcl__v_is_prototype_string( whcl_engine const * const el,
                                         cwal_value const * const key ){
  return whcl_v_is_cached_string( el->cache.keyPrototype, "__prototype", 11, key );
}

int whcl__sweep_impl( whcl_engine * const el, enum WhclSweepModes initialBroomMode,
                      bool force ){
  int rc = 0;
  whcl_scope * const sc = whcl__scope_current(el);
  assert(el && el->ec);
  assert(sc);
  /*MARKER(("SWEEP RUN #%d? sweepTick=%d, s-guard=%d, v-guard=%d\n",
          el->sweepTotal, el->sweeper.guard.sweepTick,
          el->sweeper.guard.sweep, el->sweeper.guard.vacuum));*/
  if(!sc){
    whcl__fatal(CWAL_RC_MISUSE, "Someone called whcl_engine_sweep() "
                "without whcl_scope_push().");
    return CWAL_RC_MISUSE /* not reached */;
  }
#if defined(DEBUG)
  switch(initialBroomMode){
    case SweepMode_Default:
    case SweepMode_Sweep:
    case SweepMode_Vacuum:
    case SweepMode_VacuumRecursive:
    case SweepMode_SweepRecursive:
      break;
    default:
      assert(!"Invalid sweep mode!");
      return CWAL_RC_MISUSE;
  }
#endif

  if(!force && (el->sweeper.guard.sweep>0 || el->sweeper.sweepInterval<=0)) return 0;
  else if(!force && (SweepMode_Default==initialBroomMode
                     && (++el->sweeper.guard.sweepTick
                         != el->sweeper.sweepInterval))){
    return 0;
  }else{
    enum WhclSweepModes sweepMode =
      (SweepMode_Default==initialBroomMode)
      ? SweepMode_Sweep : initialBroomMode;
    int valsSwept = 0;
    el->sweeper.guard.sweepTick = 0;
    ++el->sweeper.sweepTotal;
    /* See if we can/should use sweep or vacuum... */
    if(SweepMode_Default==initialBroomMode
       && el->sweeper.vacuumInterval>0
       && !el->sweeper.guard.vacuum
       && (0 == (el->sweeper.sweepTotal
                 % el->sweeper.vacuumInterval))
       ){
      //MARKER(("Switching from sweep to vacuum for cleanup #%u/%u\n",
      //        el->sweeper.sweepTotal, el->sweeper.vacuumTotal));
#if 0
      /* An experiment in recursive vacuuming, which is known to lead
         to premature cleanup of _some_ values _sometimes_.  It _will_
         eventually fail. Best case, that means assert() failures in
         the cwal core. Worst case, undefined behavior of the grandest
         sort. */
      sweepMode = (0==(el->sweeper.sweepTotal
                       % (el->sweeper.vacuumInterval
                          * el->sweeper.vacuumInterval))
                   ? SweepMode_VacuumRecursive
                   : SweepMode_Vacuum);
#else
      sweepMode = SweepMode_Vacuum;
#endif
    }
    if(valsSwept){/*written but never read outside of debugging*/}
    //MARKER(("SWEEP RUN #%d mode=%d\n", el->sweeper.sweepTotal, sweepMode));
    switch(sweepMode){
      default:
        assert(!"Invalid sweep mode!");
        return CWAL_RC_MISUSE;
      case SweepMode_Default:
      case SweepMode_Sweep:
        valsSwept = (int)cwal_engine_sweep(el->ec);
        break;
      case SweepMode_SweepRecursive:
        valsSwept = (int)cwal_engine_sweep2(el->ec, 1);
        break;
      case SweepMode_Vacuum:
        ++el->sweeper.vacuumTotal;
        if(1){
#if 0
          extern void cwal_engine_vacuum_v2( cwal_engine * const e );
          cwal_engine_vacuum_v2(el->ec)
            /* As-yet failed experiment */;
#else
          rc = cwal_engine_vacuum(el->ec, NULL);
#endif
          rc = 0;
        }else{
          int vcount = 0;
          rc = cwal_engine_vacuum(el->ec, &vcount);
          if(vcount){
            MARKER(("Vacuumed up %d values.\n", vcount));
          }
        }
        assert(!rc && "Vacuum \"cannot fail\".");
        break;
      case SweepMode_VacuumRecursive:{
        whcl__fatal(CWAL_RC_CANNOT_HAPPEN,
                    "Never run with vac mode "
                    "SweepMode_VacuumRecursive.");
        rc = 0;
      }
    }
    return rc;
  }
}

void whcl_engine_sweep( whcl_engine * const el ){
  whcl__sweep_impl(el, SweepMode_Default, false);
}

int whcl__is_var_char(int ch, bool isFirstChar){
  switch(ch){
    case '_':
    case 'a': case 'b': case 'c': case 'd': case 'e':
    case 'f': case 'g': case 'h': case 'i': case 'j':
    case 'k': case 'l': case 'm': case 'n': case 'o':
    case 'p': case 'q': case 'r': case 's': case 't':
    case 'u': case 'v': case 'w': case 'x': case 'y':
    case 'z':
    case 'A': case 'B': case 'C': case 'D': case 'E':
    case 'F': case 'G': case 'H': case 'I': case 'J':
    case 'K': case 'L': case 'M': case 'N': case 'O':
    case 'P': case 'Q': case 'R': case 'S': case 'T':
    case 'U': case 'V': case 'W': case 'X': case 'Y':
    case 'Z':
      return ch;
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
    case '-':
      return isFirstChar ? 0 : ch;
    default:
      return (0x80 & ch || ch>255) ? ch : 0;
  }
}


int whcl_t_is_eox(int ch){
  switch(ch){
    case TOK1_T_EOL:
    case TOK1_T_EOX:
    case TOK1_T_EOF:
    case TOK1_T_Semicolon:
      return ch;
    default:
      return 0;
  }
}

int whcl__ttype_is_operator(int ch){
  switch(ch){
    case TOK1_T_OpNot:
    case TOK1_T_OpModulo:
    case TOK1_T_OpModuloAssign:
    case TOK1_T_OpModuloAssign3:
    case TOK1_T_OpAndBitwise:
    case TOK1_T_OpAnd:
    case TOK1_T_OpAndAssign:
    case TOK1_T_OpAndAssign3:
    case TOK1_T_OpMultiply:
    case TOK1_T_OpMultiplyAssign:
    case TOK1_T_OpMultiplyAssign3:
    case TOK1_T_OpPlus:
    case TOK1_T_OpPlusUnary:
    case TOK1_T_OpPlusAssign:
    case TOK1_T_OpPlusAssign3:
    case TOK1_T_OpIncr:
    case TOK1_T_OpIncrPre:
    case TOK1_T_OpIncrPost:
    //?case TOK1_T_Comma:
    case TOK1_T_OpMinus:
    case TOK1_T_OpMinusUnary:
    case TOK1_T_OpMinusAssign:
    case TOK1_T_OpMinusAssign3:
    case TOK1_T_OpDecr:
    case TOK1_T_OpDecrPre:
    case TOK1_T_OpDecrPost:
    case TOK1_T_OpDot:
    case TOK1_T_OpArrow:
    case TOK1_T_OpArrow2:
    case TOK1_T_OpDivide:
    case TOK1_T_OpDivideAssign:
    case TOK1_T_OpDivideAssign3:
    case TOK1_T_Colon:
    case TOK1_T_OpColon2:
    case TOK1_T_OpColonEqual:
    case TOK1_T_OpCmpLT:
    case TOK1_T_OpCmpLE:
    case TOK1_T_OpShiftLeft:
    case TOK1_T_OpShiftLeftAssign:
    case TOK1_T_OpShiftLeftAssign3:
    case TOK1_T_OpAssign:
    case TOK1_T_OpAssign3:
    case TOK1_T_OpCmpEq:
    case TOK1_T_OpCmpNotEq:
    case TOK1_T_OpCmpEqStrict:
    case TOK1_T_OpCmpNotEqStrict:
    case TOK1_T_OpInherits:
    case TOK1_T_OpNotInherits:
    case TOK1_T_OpContains:
    case TOK1_T_OpNotContains:
    case TOK1_T_OpAssignConst3:
    case TOK1_T_OpCmpGT:
    case TOK1_T_OpCmpGE:
    case TOK1_T_OpShiftRight:
    case TOK1_T_OpShiftRightAssign:
    case TOK1_T_OpShiftRightAssign3:
    case TOK1_T_Question:
    case TOK1_T_QDot:
    case TOK1_T_OpXOr:
    case TOK1_T_OpXOrAssign:
    case TOK1_T_OpXOrAssign3:
    case TOK1_T_OpOrBitwise:
    case TOK1_T_OpOr:
    case TOK1_T_OpOr3:
    case TOK1_T_OpElvis:
    case TOK1_T_OpOrAssign:
    case TOK1_T_OpOrAssign3:
    case TOK1_T_OpNegateBitwise:
      return ch;
    default:
      return 0;
  }
}

/**
   Given the FIRST BYTE of a UTF8 character, this function returns the
   byte length of that character (1-4).
*/
static inline unsigned char whcl__chlen(int ch){
  switch(ch<=127 ? 0 : 0xF0 & ch){
    case 0xF0: return 4;
    case 0xE0: return 3;
    case 0xC0: return 2;
    default: return 1;
  }
}

int whcl__read_identifier2( char const * zPos,
                          char const * zEnd,
                          char const ** zIdEnd,
                          uint32_t flags ){
  unsigned char const * start = (unsigned char const *) zPos;
  unsigned char const * pos = start;
  unsigned char const * end = (unsigned char const *) zEnd;
  unsigned char const * endChar = pos;
  int ch;
  int rc = 0;
  int rcNonDash = 0;
  bool const allowDash = (WHCL__SCRIPT_F_IDENTIFIER_DASHES & flags);
  bool const allowDash2 = WHCL__SCRIPT_F_IDENTIFIER_DASHES2
    ==(WHCL__SCRIPT_F_IDENTIFIER_DASHES2 & flags);
  assert(zEnd>zPos);
  for( ; pos < end; ){
    ch = cwal_utf8_read_char( pos, end, &endChar );
    if(endChar == pos) break;
    /*MARKER(("%s(): read char: %.*s (0x%04x len=%d) is-var-char? %d\n",
            __func__, (int)(endChar-pos), pos, ch, whcl__chlen(*pos),
            whcl__is_var_char(ch, 1) ));*/
    if(whcl__is_var_char(ch, pos==start)){
      ++rcNonDash;
      ++rc;
    }else if('-'==ch && ((allowDash && pos>start)
                         || (allowDash2 && pos==start))){
      ++rc;
    }else{
      break;
    }
    pos = endChar;
  }
  if(rcNonDash){
    *zIdEnd = zPos + (pos - start);
  }else{
    rc = 0;
  }
  return rc;
}

int whcl__read_identifier( char const * zPos,
                        char const * zEnd,
                        char const ** zIdEnd ){
  return whcl__read_identifier2( zPos, zEnd, zIdEnd, 0 );
}

int whcl__compile_buffer( whcl_engine * const el, whcl_script * const ct,
                          cwal_buffer const * const buf,
                          uint32_t flags ){
  return whcl__compile( el, ct, buf->mem ? (char const *)buf->mem : "",
                      buf->mem ? (cwal_int_t)buf->used : 0,
                      flags );
}

/**
   Perform post-compilation processing on a freshly-compiled
   ct chain. On the first call, the final argument must be 1, but
   recursive use will pass different values.
*/
static int whcl__compile_post(whcl_engine * const el, whcl_script * const ct,
                              whcl_stoken_id pos);

static int whcl__maybe_tweak_identifier(whcl_engine * const el,
                                        whcl_script * const ct,
                                        whcl_stoken * const tok){
  assert(TOK1_T_Identifier==tok->ttype);
  int rc = 0;
#if 0
  cwal_midsize_t n = 0;
  char const * s = whcl_stoken_cstr(ct, tok, &n, false);
#else
  if(ct || tok){/*unused*/}
#endif
  if(el){/*unused*/}
  return rc;
}

int whcl__compile_post(whcl_engine * const el, whcl_script * const ct,
                       whcl_stoken_id pos){
  whcl_stoken * tPrev = NULL;
  whcl_stoken * tok = whcl__script_at_nc(ct, pos);
  int rc = 0;
  whcl_stoken_id nextId = 0;
#if 0
  static whcl_stoken_id prevId = 0;
  static whcl_script const * prevScript = NULL;
  if(prevId == pos && prevScript == ct){
    whcl__fatal(CWAL_RC_ERROR, "Loop in compilation.");
  }
  prevId = pos; prevScript = ct;
#endif
  for(; 0==rc && tok && !whcl_stoken_is_eof(tok);
      tPrev = tok, tok = whcl__script_at_nc(ct, nextId)){
    //whcl__dump_stok(ct, tok, "post-compile?");
    whcl__stoken_set(ct, tok);
    if(tPrev && whcl__ttype_op2(tPrev->ttype)
       && whcl__ttype_op2(tok->ttype)
       && whcl_stokens_touch(tPrev, tok) ){
      //whcl__script_errtoken_set(ct, tok);
      /* This breaks !! but i can live with that. */
      return whcl__script_throw(ct, CWAL_SCR_SYNTAX,
                                "Operators must be separated by "
                                "at least one space to avoid certain "
                                "ambiguities.");
    }
    nextId = tok->nextId;
    if(tPrev && TOK1_T_At==tPrev->ttype && whcl_stokens_touch(tPrev, tok)){
      /* Restructure @token so that the @ contains the RHS token. If the RHS
         is an identifier, change it to an identifier deref, to avoid that we
         have to type out @$x when @x will suffice. */
      tPrev->nextId = tok->nextId;
      tok->nextId = whcl__script_eof_id(ct);
      tPrev->innerId = tok->id;
      tPrev->ttype = TOK1_T_AtExpand;
      if(TOK1_T_Identifier == tok->ttype && !tok->ttype2){
        tok->ttype = TOK1_T_IdentifierDeref;
      }
      /* Fall through to recurse into tok if needed */
    }
    if(tok->innerId && tok->id!=tok->innerId){
      whcl_script sub = whcl__script_empty;
      rc = whcl__script_sub_from_group(ct, &sub, true);
      if(0==rc) rc = whcl__compile_post(el, &sub, tok->innerId);
      whcl__script_finalize(&sub);
      if(rc) return rc;
    }
    if(tok->subscriptId && tok->subscriptId!=tok->id){
      whcl_script sub = whcl__script_empty;
      rc = whcl__script_sub_from_group(ct, &sub, false);
      if(0==rc) rc = whcl__compile_post(el, &sub, tok->subscriptId);
      whcl__script_finalize(&sub);
      if(rc) return rc;
    }
    switch(tok->ttype){
      case TOK1_T_EOL:
      case TOK1_T_EOX:
        continue;
      case TOK1_T_Identifier:
        if(tok->ttype2){
          whcl__dump_stok(ct, tok, "ttype2?");
          assert(!tok->ttype2);
        }
        if(tPrev
           && (TOK1_T_OpDecr==tPrev->ttype
               || TOK1_T_OpMinus==tPrev->ttype)
           && whcl_stokens_touch(tPrev, tok)){
          /* Convert -IDENT and --IDENT to a flag token.  As of this
            writing, -IDENT is handled at the lower tokenization
            level, but "the plan" is to _maybe_ change that and stuff
            the IDENT into the minus-sign's innerId part so that
            downstream code does not have to concern itself with the -
            or -- prefix. */
          assert(!tPrev->innerId && !tok->innerId &&
                 !tPrev->subscriptId && !tok->subscriptId);
          tPrev->ttype = TOK1_T_WHCLFlag;
          tPrev->length += tok->length;
          tPrev->innerLength = tPrev->length;
          nextId = tPrev->nextId = tok->nextId;
          for(char const * b = whcl_stoken_begin(ct, tPrev);
              '-'==*b; ++b){
            ++tPrev->innerOffset;
            --tPrev->innerLength;
          }          
        }else {
          rc = whcl__maybe_tweak_identifier(el, ct, tok);
          nextId = tok->nextId;
        }
        continue;
      default: continue;
    }
  }
  return rc;
}

int whcl__compile( whcl_engine * const el, whcl_script * const ct,
                   char const * const src, cwal_int_t len,
                   uint32_t flags ){
  if(!ct->ec) whcl__script_init(el->ec, ct);
  whcl_script * const oldScript = el->ct;
  int rc = whcl__script_compile( ct, src, len,
                                 flags | WHCL__SCRIPT_F_IDENTIFIER_DASHES2,
                                 whcl_stoken_provider_f_whcl, el );
  if(0==rc){
    rc = whcl__compile_post(el, ct, 1);
    if(0==rc) whcl__script_rewind(ct);
  }
  el->ct = oldScript;
  return rc;
}

int whcl_compile_buffer_take(whcl_engine * const el, whcl_script ** tgt,
                             char const * const name,
                             cwal_buffer * const src){
  whcl_script * const sc = whcl__script_alloc(el->ec);
  if(!sc){
    WHCL__WARN_OOM;
    return CWAL_RC_OOM;
  }
  sc->name = name;
  whcl_script * const oldScript = el->ct;
  int rc = whcl__compile(el, sc, (char const *)src->mem,
                         (cwal_int_t)src->used, 0);
  el->ct = oldScript;
  if(rc) whcl_script_free(sc);
  else{
    if(name && *name){
      rc = whcl_script_name_set(sc, name, -1);
      if(rc) goto oom;
    }else{
      sc->name = "";
    }
    cwal_buffer tmp = cwal_buffer_empty;
    cwal_buffer_swap_mem(src, &tmp);
    if(tmp.mem){
      sc->begin = sc->ownSrc = (char *)tmp.mem;
      sc->end = sc->begin + tmp.used;
    }else{
      sc->begin = sc->end = "";
      cwal_buffer_clear(el->ec, &tmp);
    }
    *tgt = sc;
  }
  return rc;
  oom:
  assert(sc);
  assert(sc->ec);
  whcl_script_free(sc);
  WHCL__WARN_OOM;
  return rc ? rc : CWAL_RC_OOM;
}

int whcl_compile( whcl_engine * const el, whcl_script ** tgt,
                         char const * const name,
                         char const * const src, cwal_int_t len){
  cwal_buffer b = cwal_buffer_empty;
  if(len<0) len = (cwal_int_t)cwal_strlen(src);
  int rc = cwal_buffer_append(el->ec, &b, src, (cwal_size_t)len);
  if(rc){
    WHCL__WARN_OOM;
    assert(NULL==b.mem);
    return rc;
  }
  rc = whcl_compile_buffer_take(el, tgt, name, &b);
  cwal_buffer_clear(el->ec, &b);
  return rc;
}       

int whcl_compile_buffer( whcl_engine * const el, whcl_script ** tgt,
                         char const * scriptName,
                         cwal_buffer const * const buf ){
  return whcl_compile(el, tgt, scriptName, (char const *)buf->mem,
                      (cwal_int_t)buf->used);
}

int whcl_compile_file( whcl_engine * const el, whcl_script ** tgt,
                       char const * filename,
                       char const * scriptName ){
  cwal_buffer b = cwal_buffer_empty;
  int rc = cwal_buffer_fill_from_filename(el->ec, &b, filename);
  if(rc){
    rc = whcl_err_set(el, rc, "Reading file failed with code %s: %s",
                      cwal_rc_cstr(rc), filename);
  }else{
    rc = whcl_compile_buffer_take(el, tgt,
                                  scriptName ? scriptName : filename,
                                  &b);
  }
  cwal_buffer_clear(el->ec, &b);
  return rc;    
}

#if 0
int whcl__callback_hook_pre(cwal_callback_args const * args,
                            void * state){
  if(args || state){/*unused*/}
  return 0;
}

int whcl__callback_hook_post(cwal_callback_args const * args,
                            void * state,
                            int fRc, cwal_value * rv){
  if(args || rv || fRc || state){/*unused*/}
  return 0;
}
#endif

void whcl__annotate_exception(whcl_engine * const el,
                              cwal_value * ex){
  if(el->ct){
    if(!ex) ex = cwal_exception_get(el->ec);
    if(ex) whcl__add_script_props(el, ex, el->ct);
  }
}

void whcl_err_reset(whcl_engine * const el){
  el->flags.interrupted = 0;
  cwal_engine_error_reset(el->ec);
  cwal_exception_set(el->ec, NULL);
}

int whcl_err_has(whcl_engine const * const el, bool alsoCheckException){
  int rc = el->flags.interrupted;
  if(!rc){
    rc = cwal_engine_error_get(el->ec, NULL, NULL);
    if(!rc && alsoCheckException){
      rc = cwal_exception_get(el->ec)
        ? CWAL_RC_EXCEPTION : 0;
    }
  }
  return rc;
}

int whcl_err_get(whcl_engine const * const el, char const ** msg,
                 cwal_size_t * msgLen ){
  return cwal_engine_error_get(el->ec, msg, msgLen);
}


int whcl_err_set( whcl_engine * const el, int code, char const * fmt, ... ){
  int rc;
  va_list args;
  va_start(args,fmt);
  if(el->ct && (el->ct->errToken || el->ct->token)){
    //whcl__dump_stok(el->ct, whcl__script_errtoken_get(el->ct), "errtoken?");
    rc = whcl__script_errv(el->ct, code, fmt, args );
  }else{
    rc = cwal_error_setv(el->ec, NULL, code, fmt, args);
  }
  va_end(args);
  return rc;
}

int whcl_err_throw( whcl_engine * const el, int code, char const * fmt, ... ){
  int rc;
  va_list args;
  va_start(args,fmt);
  cwal_engine_error_reset(el->ec);
  if(el->ct){
    if(!el->ct->errToken) el->ct->errToken = el->ct->token;
    rc = whcl__throwv(el, el->ct, NULL, code, fmt, args);
  }else{
    rc = cwal_exception_setfv(el->ec, code ? code : CWAL_RC_EXCEPTION,
                             fmt, args);
  }
  va_end(args);
  return rc;
}

int whcl__errv( whcl_engine * const el, whcl_script * ct,
                     whcl_stoken const * const t,
                     int code, char const * fmt, va_list args ){
  if(!ct) ct = el->ct;
  assert(ct);
  if(t) whcl__script_errtoken_set(ct, t);
  int const rc = whcl__script_errv(ct, code ? code : CWAL_RC_ERROR, fmt, args );
  return rc;
}

int whcl__err( whcl_engine * const el, whcl_script * const ct,
                      whcl_stoken const * const t,
                      int code, char const * fmt, ... ){
  int rc;
  va_list args;
  va_start(args,fmt);
  rc = whcl__errv(el, ct, t, code, fmt, args);
  va_end(args);
  return rc;
}

int whcl__throwv( whcl_engine * const el, whcl_script * ct, whcl_stoken const * const t,
                  int code, char const * fmt, va_list args ){
  if(!ct) ct = el->ct;
  assert(ct);
  if(!code) code = CWAL_RC_EXCEPTION;
  if(t) whcl__script_errtoken_set(ct, t);
  int const rc = whcl__script_throwv(ct, code, fmt, args);
  if(CWAL_RC_OOM!=rc) whcl__add_script_props(el, cwal_exception_get(el->ec), ct);
  return rc;
}

int whcl__throw( whcl_engine * const el, whcl_script * ct, whcl_stoken const * const t,
                      int code, char const * fmt, ... ){
  int rc;
  va_list args;
  va_start(args,fmt);
  rc = whcl__throwv(el, ct, t, code, fmt, args);
  va_end(args);
  return rc;
}

int whcl__err_oom(whcl_engine * const el){
  return whcl_err_set(el, CWAL_RC_OOM, NULL);
}

int whcl__check_interrupted( whcl_engine * const el, int rc ){
  switch(rc){
    case CWAL_RC_ASSERT:
    case CWAL_RC_CANNOT_HAPPEN:
    case CWAL_RC_EXIT:
    case CWAL_RC_FATAL:
    case CWAL_RC_INTERRUPTED:
    case CWAL_RC_OOM:
      return rc;
    default:
        return el->ec->fatalCode
          ? (el->flags.interrupted = el->ec->fatalCode)
          : (el->flags.interrupted
             ? el->flags.interrupted
             : rc);
  }
}


#if 0
int whcl_err_toss(whcl_engine * const el){
  return cwal_error_throw(el->ec, NULL, NULL, 0, 0);
}
#endif

/** tok1_next_token_f() impl */
static int whcl__next_token_base(tok1_izer * const t, void * state){
  /**
     Some of what follows is historical, brought over from three
     previous cwal-based languages. In particular, the tok2
     tokenization model requires that a tok1_izer be traversed
     linearly, no jumping back and forth between tokens or using the
     putback token.
  */
  int rc = 0;
  tok1_en * const tok = &t->token;
  char const * const startPos = tok1_en_end(&t->token)
    ? tok1_en_end(&t->token) /* for the 2nd and subsequent calls */
    : tok1_en_begin(&t->token) /* for the first run through this function */;
  char const * curpos = startPos;
  if(state){/*unused*/}
  assert(curpos);
  assert(curpos >= tok1_izer_begin(t));
  assert(curpos <= tok1_izer_end(t));
  t->_pbToken = t->token;
  tok1_en_adjbegin_set(&t->token, 0);
  tok1_en_adjend_set(&t->token, 0);
  t->errMsg = 0;
  t->_errToken = tok1_en_empty;
  tok->ttype2 = TOK1_T_INVALID;

  if(!t->currentLine) t->currentLine = 1;
  tok->line = t->currentLine;
  tok->column = t->currentCol;

#define BUMP(X) for(int bpos=0; IN_BOUNDS && bpos<X; ++bpos) \
  {curpos+=whcl__chlen(*curpos); ++t->currentCol; } (void)0
#define RETURN_ERR(RC,MSG) tok->ttype = TOK1_T_TokErr; t->errMsg = MSG; rc=RC; goto end
#define NEXT_LINE  ++t->currentLine; t->currentCol = 0
#define IN_BOUNDS (curpos<t->end)
#define CHECK_OPS (void)0 /* a failed experiment which might return in another form */
  if( !IN_BOUNDS ) {
    tok->ttype = TOK1_T_EOF;
    tok1_en_begin_set(&t->token, tok1_izer_end(t));
    tok1_en_end_set(&t->token, tok1_izer_end(t));
    return 0;
  }

  if(!t->parent && curpos == tok1_izer_begin(t)){
    /* Check for some things which can only appear at the start of a
       script. The if() condition above isn't quite 100% accurate,
       considering how we use sub-tokenizers at times, but it's
       pretty close.
    */
    if('#'==*curpos && '!'==*(curpos+1)){
      /* Workaround: strip shebang line from start of scripts. */
      for( ; ++curpos < tok1_izer_end(t)
             && *curpos
             && ('\n' != *curpos); ++t->currentCol){}
      ++curpos /* skip NL */;
      if( !IN_BOUNDS ) {
        tok->ttype = TOK1_T_EOF;
        tok1_en_begin_set(&t->token, tok1_izer_end(t));
        tok1_en_end_set(&t->token, tok1_izer_end(t));
        return 0;
      }
      tok1_en_end_set(&t->token, curpos);
      t->token.ttype = TOK1_T_Shebang;
      ++t->currentLine;
      t->currentCol = 0;
      return 0;
    }else{
      /* Check for a UTF-8 BOM. */
      unsigned char const * ccp = (unsigned char const*)curpos;
      if(0xEF==*ccp && 0xBB==ccp[1] && 0xBF==ccp[2]){
        curpos += 3;
        tok1_en_end_set(&t->token, curpos);
        t->token.ttype = TOK1_T_UTFBOM;
        t->currentCol += 3;
        return 0;
      }
    }
  }

  tok->ttype = TOK1_T_INVALID;
  tok1_en_begin_set(&t->token, curpos);
  switch(IN_BOUNDS ? *curpos : 0){
    case 0:
      tok->ttype = TOK1_T_EOF;
      /*This is a necessary exception to the bump-on-consume
        rule.*/
      break;
    case '\\': /* Treat \<NEWLINE> as a continuation of a line */
      /* TODO/FIXME: we want to be able to backslash-escape
         \# and similar sequences. */
      if(('\n'==*(curpos+1)) || (('\r'==*(curpos+1)) && ('\n'==*(curpos+2)))){
        tok->ttype = TOK1_T_Whitespace;
        if('\r'==*(curpos+1)) { BUMP(3); }
        else { BUMP(2); }
        NEXT_LINE;
      }else{
        RETURN_ERR(CWAL_SCR_SYNTAX,"Unexpected backslash.");
      }
      break;
    case '\r':
      if('\n'==*(curpos+1)){
        tok->ttype = TOK1_T_EOL;
        BUMP(2);
        NEXT_LINE;
      }
      else{
        tok->ttype = TOK1_T_CR;
        BUMP(1);
      }
      break;
    case '\n':
      tok->ttype = TOK1_T_EOL/*NL*/;
      BUMP(1);
      NEXT_LINE;
      break;
    case ' ':
    case '\t':
    case '\v':
    case '\f':
      tok->ttype = *curpos /*TOK1_T_Blank*/;
      BUMP(1);
      while( curpos<tok1_izer_end(t)
             && *curpos && tok1_is_blank(*curpos) ){
        tok->ttype = TOK1_T_Blank;
        BUMP(1);
      }
      break;
    case ':':
      /* We need this only for heredoc processing to work */
      tok->ttype = TOK1_T_Colon; BUMP(1); break;
    case ';':
      tok->ttype = TOK1_T_EOX; BUMP(1); break;
    case '@':
      tok->ttype = TOK1_T_At; BUMP(1); break;
    case '/':
      if(curpos[1]=='*'){
        BUMP(2);
        tok->ttype = TOK1_T_CommentC;
        do{
          while( IN_BOUNDS && *curpos && ('*' != *curpos) ){
            if('\n'==*curpos){
              NEXT_LINE;
            }
            BUMP(1);
          }
          if(!IN_BOUNDS){
            RETURN_ERR(CWAL_SCR_SYNTAX,
                       "Reached (virtual) EOF while looking for "
                       "C-style comment closer.");
          }
          BUMP(1); /* skip '*' */
        } while( IN_BOUNDS && *curpos && ('/' != *curpos));
        assert(IN_BOUNDS);
        if( IN_BOUNDS && *curpos != '/' ){
          RETURN_ERR(CWAL_SCR_SYNTAX,"End of C-style comment not found."); 
        }else{
          BUMP(1); /* get that last slash */
        }
      }else if('/' == curpos[1]){
        /* It seems reasonable to treat these as EOL tokens. If we
           don't, we end up with 2 tokens: a comment followed by a
           newline. The one outlier case is when a comment ends at
           EOF, without a newline. No harm done there, though. */
        tok->ttype = 0 ? TOK1_T_EOL : TOK1_T_CommentCpp;
        BUMP(2);
        while(IN_BOUNDS && '\n'!=*curpos){
          BUMP(1);
        }
      }else{
        BUMP(1);
        tok->ttype = TOK1_T_OpDivide;
      }
      break;
    case '#':{ // maybe a TCL-style comment
      /* For compatibility with TCL, primarily for the sake of getting
         nice results with editors' TCL syntax highlighting(!), only
         consider this # a comment if it is the first token on the
         current line or follows one of a small subset of token
         types. Otherwise treat is as a plain old token, which will
         be picked up as a string for most purposes. */
      bool lineIsClear = true;
      for( char const * z = curpos-1; z>=t->begin; --z ){
        switch(tok1_is_blank(*z) ? ' ' : *z){
          case ' ': case '\t': continue;
          case ';': case '\n': break;
          default: lineIsClear = false; break;
        }
        break;
      }
      if(lineIsClear){
        tok->ttype = 0 ? TOK1_T_EOL : TOK1_T_CommentTCL;
        BUMP(1);
        while(IN_BOUNDS && '\n'!=*curpos){ BUMP(1); }
      }else{
        tok->ttype = TOK1_T_OpHash;
        BUMP(1);
      }
      break;
    }
    case '$': /* Variable dereference */{
      int nChar = 0;
      BUMP(1);
#if 0
      if('('==*curpos){
        /* 2022-03-02: $(call block) syntax was phased out but we'll
           keep this code for potential reuse with different
           semantics. */
        tok->ttype = TOK1_T_CallBlockOpen;
        BUMP(1);
      }else
#endif
      {
        while(IN_BOUNDS && whcl__is_var_char(*curpos, nChar==0)){
          BUMP(1);
          ++nChar;
        }
        if(0==nChar){
          //tok->ttype = TOK1_T_Identifier;
          goto malformed_var_deref;
        }
        else if(IN_BOUNDS && !(tok1_is_space(*curpos)
                               || whcl_t_is_eox(*curpos)
                               || ')'==*curpos
                               || '['==*curpos
                               || ']'==*curpos
                               || '}'==*curpos
                               || whcl__ttype_is_operator(*curpos)
                               )){
          goto malformed_var_deref;
        }else{
          tok->ttype = TOK1_T_IdentifierDeref;
        }
      }
      break;
      malformed_var_deref:
      RETURN_ERR(CWAL_SCR_SYNTAX,"Malformed variable dereference.");
    }
    case '"':
    case '\'': /* read string literal */{
      char const quote = *curpos;
      char const * begin = curpos;
      tok->ttype = TOK1_T_QuotedString;
      tok->ttype2 = ('"' == *curpos)
        ? TOK1_T_QuotedStringDQ
        : TOK1_T_QuotedStringSQ;
      BUMP(1)/*leading quote*/;
      while(IN_BOUNDS && *curpos && (*curpos != quote)){
        /*
          BUG: our counting of t->currentCol (via BUMP())
          is off for non-ASCII characters. Need to loop over
          this as UTF.
        */
        if( (*curpos == '\\') ){
          /* consider next char to be escaped, but keep escape char */
          BUMP(1);
          if(*curpos == 0){
            RETURN_ERR(CWAL_SCR_SYNTAX,
                       "Unexpected EOF while tokenizing "
                       "backslash-escaped char in string literal.");
          }
          BUMP(1);
          continue;
        }
        else if('\n'==*curpos){
          BUMP(1);
          NEXT_LINE;
        }
        else {
          BUMP(1);
        }
      }
      if(!IN_BOUNDS || *curpos != quote){
        RETURN_ERR(CWAL_SCR_SYNTAX,
                   "Unexpected end of string literal.");
      }else{
        BUMP(1)/*trailing quote*/;
      }
      tok->_adjBegin = begin+1;
      tok->_adjEnd = curpos-1;
      break;
    } /* end literal string */
    case '0': /* 0 or hex or octal or binary literals */
    try_number0:
      BUMP(1);
      if(tok1_izer_end(t) <= curpos){
        /* special case: 0 at the end of input. */
        tok->ttype = TOK1_T_LiteralNumber;
        tok->ttype2 = TOK1_T_LiteralIntDec;
        break;
      }
      switch (*curpos)/* try hex or octal or binary */{
        case 'x':
        case 'X':{/** hex digit. */
          int digitCount = 0;
          BUMP(1);
          while(curpos<tok1_izer_end(t) && *curpos){
            if('_'==*curpos){
              BUMP(1);
              continue /* separator */;
            }else if(tok1_is_xdigit(*curpos)){
              ++digitCount;
              BUMP(1);
              continue;
            }
            break;
          }
          /*
            20200828: bug: the following syntax errors are not
            reporting error positions. e.g.

            catch { 0x }
            catch { 0x3_ }

            But only *sometimes*!?!?

            i'm imagining this, certainly.
          */
          if(!digitCount){
            RETURN_ERR(CWAL_SCR_SYNTAX,
                       "No digits in hexidecimal int literal.");
          }else{
            tok->ttype = TOK1_T_LiteralNumber;
            tok->ttype2 = TOK1_T_LiteralIntHex;
            if('_' == curpos[-1] /* last char was a separator */
               || tok1_is_alnum(*curpos)
               ){
              BUMP(1); /* make sure it shows up in the error string. */
              RETURN_ERR(CWAL_SCR_SYNTAX,
                         "Malformed hexidecimal int literal.");
            }
          }
          break;
        }
        case 'o':{/* try octal... */
          int digitCount = 0;
          BUMP(1);
          while(curpos<tok1_izer_end(t) && *curpos){
            if('_'==*curpos){
              BUMP(1);
              continue /* separator */;
            }else if(tok1_is_octaldigit(*curpos)){
              ++digitCount;
              BUMP(1);
              continue;
            }
            break;
          }
          if(!digitCount){
            RETURN_ERR(CWAL_SCR_SYNTAX,
                       "No digits in octal int literal.");
          }else{
            tok->ttype = TOK1_T_LiteralNumber;
            tok->ttype2 = TOK1_T_LiteralIntOct;
            if('_' == curpos[-1] /* last char was a separator */
               || tok1_is_alnum(*curpos)
               ){
              BUMP(1); /* make sure it shows up in the error string. */
              RETURN_ERR(CWAL_SCR_SYNTAX,
                         "Malformed octal int literal.");
            }
          }
          break;
        }
        case 'b':{
          /* try binary... */
          int digitCount = 0;
          BUMP(1);
          while(*curpos){
            if(*curpos=='0' || *curpos=='1'){
              ++digitCount;
              BUMP(1);
              continue;
            }else if('_'==*curpos){
              BUMP(1);
              continue /* separator */;
            }
            break;
          }
          if(!digitCount){
            RETURN_ERR(CWAL_SCR_SYNTAX,
                       "No digits in binary int literal.");
          }
          /*else if(digitCount > CWAL_INT_T_BITS){
            RETURN_ERR(CWAL_SCR_SYNTAX,
            "Binary value is too large for this build.");
            }*/
          else{
            tok->ttype = TOK1_T_LiteralNumber;
            tok->ttype2 = TOK1_T_LiteralIntBin;
            if('_' == curpos[-1] /* last char was a separator */
               || tok1_is_alnum(*curpos)
               ){
              BUMP(1); /* make sure it shows up in the error string. */
              RETURN_ERR(CWAL_SCR_SYNTAX,
                         "Malformed binary int literal.");
            }
          }
          break;
        }/*binary*/
        default:
          if( *curpos && (
                          tok1_is_alnum(*curpos)
                          || (*curpos == '_'))){
            /* reject 12334x where x is alphanum or _ */
            BUMP(1); /* make sure it shows up in the error string. */
            RETURN_ERR(CWAL_SCR_SYNTAX,
                       "Malformed numeric literal starts "
                       "with '0' but is neither octal nor "
                       "hex nor binary.");
          }
          if('.'==*curpos){
            BUMP(1);
            while( tok1_is_digit(*curpos) ){
              BUMP(1);
            }
            if('.'==*(curpos-1)){
              RETURN_ERR(CWAL_SCR_SYNTAX,
                         "Mis-terminated floating point value.");
            }
            tok->ttype = TOK1_T_LiteralNumber;
            tok->ttype2 = TOK1_T_LiteralDouble;
          }
          else {
            tok->ttype = TOK1_T_LiteralNumber;
            tok->ttype2 = TOK1_T_LiteralIntDec;
            /* A literal 0. This is okay. */
          }
          break;
      }
      break;
      /* end numbers starting with '0' */
    case '1': case '2': case '3':
    case '4': case '5': case '6':
    case '7': case '8': case '9': /* integer or double literal. */
    try_number19:
    {
      int gotSep = 0;
      /*
        Reminder to self: The 0x, 0o, and 0b formats all support '_'
        characters as "visual separators" in their numeric literals.
        Decimals values "should" do the same but...  our downstream
        code for parsing doubles does not handle those characters,
        thus we prohibit the '_' separators in floating-point values
        here. At _this_ point in the tokenization we don't yet know if
        we're reading an integer or double, so we have to remember
        whether we hit a '_' and error out if it turns out we're
        parsing a double. Not elegant, but that's okay.
      */
      BUMP(1);
      while(IN_BOUNDS && *curpos){
        /* integer or first part of a double. */
        if('_'==*curpos){
          ++gotSep;
          BUMP(1);
          continue;
        }else if(tok1_is_digit(*curpos)){
          BUMP(1);
          continue;
        }
        break;
      }
      if( IN_BOUNDS
          && ('.' == *curpos) && tok1_is_digit(*(curpos+1)) ){
        /* double number */
        if(gotSep){
          RETURN_ERR(CWAL_SCR_SYNTAX,
                     "'_' separators are not legal in floating-point literals.");
        }
        tok->ttype = TOK1_T_LiteralNumber;
        tok->ttype2 = TOK1_T_LiteralDouble;
        BUMP(1);
        while(IN_BOUNDS && *curpos && tok1_is_digit(*curpos)){
          BUMP(1);
        }
      }
      else {
        tok->ttype = TOK1_T_LiteralNumber;
        tok->ttype2 = TOK1_T_LiteralIntDec;
      }
      if( (curpos[-1] == '_'
           /* disallow trailing separator for symmetry
              with 0x/0o/0b literals. */)
          || (IN_BOUNDS
              && *curpos
              && (tok1_is_alnum(*curpos)
                  || (*curpos == '_')))
          ) {
        BUMP(1); /* make sure it shows up in the error string. */
        RETURN_ERR(CWAL_SCR_SYNTAX,
                   "Malformed numeric literal.");
      }
      break;
    } /* 1..9 */
    case '&': /* & or && or &= */
      CHECK_OPS;
      tok->ttype = TOK1_T_OpAndBitwise;
      BUMP(1);
      switch(IN_BOUNDS ? *curpos : 0){
        case '&':
          tok->ttype = TOK1_T_OpAnd;
          BUMP(1);
          break;
        case '=':
          tok->ttype = TOK1_T_OpAndAssign;
          BUMP(1);
          break;
      }
      break;
    case '|': /* | or || or |= or ||| */
      CHECK_OPS;
      tok->ttype = TOK1_T_OpOrBitwise;
      BUMP(1);
      switch(IN_BOUNDS ? *curpos : 0){
        case '|':
          tok->ttype = TOK1_T_OpOr;
          BUMP(1);
          if( IN_BOUNDS && '|' == *curpos ){
            tok->ttype = TOK1_T_OpOr3;
            BUMP(1);
          }
          break;
        case '=':
          tok->ttype = TOK1_T_OpOrAssign;
          BUMP(1);
          break;
      }
      break;
    case '?': /* ? or ?: */
      CHECK_OPS;
      tok->ttype = TOK1_T_Question;
      BUMP(1);
      if( IN_BOUNDS && ':' == *curpos ){
        tok->ttype = TOK1_T_OpElvis;
        BUMP(1);
      }
      break;
    case '^': /* ^ or ^= */
      CHECK_OPS;
      tok->ttype = TOK1_T_OpXOr;
      BUMP(1);
      if( '=' == *curpos ){
        tok->ttype = TOK1_T_OpXOrAssign;
        BUMP(1);
      }
      break;
    case '+': /* + or ++ or += */{
      CHECK_OPS;
      tok->ttype = TOK1_T_OpPlus;
      BUMP(1);
      switch(IN_BOUNDS ? *curpos : 0){
        case '0': goto try_number0;
        case '1': case '2': case '3': case '4': case '5':
        case '6': case '7': case '8': case '9': goto try_number19;
        case '+': /* ++ */
          tok->ttype = TOK1_T_OpIncr;
          BUMP(1);
          break;
        case '=': /* += */
          tok->ttype = TOK1_T_OpPlusAssign;
          BUMP(1);
          break;
        default: break;
      }
      break;
    }
    case '*': /* * or *= */
      CHECK_OPS;
      tok->ttype = TOK1_T_OpMultiply;
      BUMP(1);
      switch(IN_BOUNDS ? *curpos : 0){
        case '=':
          tok->ttype = TOK1_T_OpMultiplyAssign;
          BUMP(1);
          break;
        case '/':
          tok->ttype = TOK1_T_INVALID;
          RETURN_ERR(CWAL_SCR_SYNTAX,
                     "Comment closer (*/) not inside a comment.");
          break;
      }
      break;
    case '-': /* - or -- or -= */{
      if(whcl__is_var_char(curpos[1], true)) goto try_identifier;
      CHECK_OPS;
      tok->ttype = TOK1_T_OpMinus;
      BUMP(1);
      switch( IN_BOUNDS ? *curpos : 0 ){
        case '0': goto try_number0;
        case '1': case '2': case '3': case '4': case '5':
        case '6': case '7': case '8': case '9': goto try_number19;
        case '-': /* -- */
          tok->ttype = TOK1_T_OpDecr;
          BUMP(1);
          break;
        case '=': /* -= */
          tok->ttype = TOK1_T_OpMinusAssign;
          BUMP(1);
          break;
        case '>': /* -> */
          tok->ttype = TOK1_T_OpArrow;
          BUMP(1);
          break;
        default: break;
      }
      break;
    }
    case '<': /* LT or << or <= or <<= or <<< */{
      if(!('<'==curpos[1] && '<'==curpos[2])){
        CHECK_OPS;
      }
      tok->ttype = TOK1_T_OpCmpLT;
      BUMP(1);
      switch( IN_BOUNDS ? *curpos : 0 ){
        case '<': /* tok= << */ {
          tok->ttype = TOK1_T_OpShiftLeft;
          BUMP(1);
          switch(IN_BOUNDS ? *curpos : 0){
            case '=': /* <<= */
              tok->ttype = TOK1_T_OpShiftLeftAssign;
              BUMP(1);
              break;
            case '<': /* <<< */
              tok->ttype = TOK1_T_HeredocStart;
              BUMP(1);
              break;
            default: break;
          }
          break;
        }
        case '=': /* tok= <= */
          tok->ttype = TOK1_T_OpCmpLE;
          BUMP(1);
          break;
        default: break;
      }
      break;
    }
    case '>': /* GT or >> or >= or >>= */{
      CHECK_OPS;
      tok->ttype = TOK1_T_OpCmpGT;
      BUMP(1);
      switch(IN_BOUNDS ? *curpos : 0){
        case '>': /* >> */
          tok->ttype = TOK1_T_OpShiftRight;
          BUMP(1);
          if( '=' == *curpos ){/* >>= */
            tok->ttype = TOK1_T_OpShiftRightAssign;
            BUMP(1);
          }
          break;
        case '=': /* >= */
          tok->ttype = TOK1_T_OpCmpGE;
          BUMP(1);
          break;
        default: break;
      }
      break;
    }
    case '=': /* = or == or =~ or => */
      CHECK_OPS;
      tok->ttype = TOK1_T_OpAssign;
      BUMP(1);
      switch( IN_BOUNDS ? *curpos : 0 ){
        case '~': /* -- */
          tok->ttype = TOK1_T_OpContains;
          BUMP(1);
          break;
        case '=':
          tok->ttype = TOK1_T_OpCmpEq;
          BUMP(1);
          break;
        case '>':
          tok->ttype = TOK1_T_OpArrow2;
          BUMP(1);
          break;
        default: break;
      }
      break;
    case '%': /* % or %= */
      CHECK_OPS;
      tok->ttype = TOK1_T_OpModulo /* *curpos */;
      BUMP(1);
      if( '=' == *curpos ){
        tok->ttype = TOK1_T_OpModuloAssign;
        BUMP(1);
      }
      break;
    case '!': /* ! or != or != or !~ */
      CHECK_OPS;
      tok->ttype = TOK1_T_OpNot;
      BUMP(1);
      tok->ttype2 = 1;
      switch(IN_BOUNDS ? *curpos : 0){
        case 0: break;
#if 0
          /* The idea is that we set the number of consecutive
             `!` so that the eval engine can fast-track them.
             However, the operator-handler level doesn't have
             access to the token in question. "Leaking"
             these tokens into the operator code is something i've
             avoided, but this would be worth it. */
        case '!':
          while(IN_BOUNDS && '!' == *curpos){
            ++tok->ttype2;
            BUMP(1);
          }
          break;
#endif
        case '~':
          tok->ttype = TOK1_T_OpNotContains;
          tok->ttype2 = 0;
          BUMP(1);
          break;
        case '=':
          tok->ttype = TOK1_T_OpCmpNotEq;
          tok->ttype2 = 0;
          BUMP(1);
          break;
      }
      break;
    case ',':
      /* ^^^ we process this one for the sake of {...script-like
         strings...}  which contain SQL or some such. */
      CHECK_OPS;
      tok->ttype = TOK1_T_Comma;
      BUMP(1);
      break;
    case '.': /* Runs of dots. We store the number of dots in
                 tok->ttype2. We permit any number of dots primarily
                 for the sake of nesting `with` blocks. */
      CHECK_OPS;
      tok->ttype = TOK1_T_OpDot;
      BUMP(1);
      tok->ttype2 = 1;
      while( IN_BOUNDS && '.' == *curpos ){
        BUMP(1);
        ++tok->ttype2;
      }
      break;
    case '~':
      CHECK_OPS;
      tok->ttype = TOK1_T_OpNegateBitwise;
      BUMP(1);
      break;
    case '{':
      tok->ttype = TOK1_T_SquigglyOpen; BUMP(1);
      if(IN_BOUNDS && '{'==*curpos && '{'==curpos[1]){
        /* {{{ ...heredoc... }}} is provided as an alternate heredoc
           syntax because it plays better with existing syntax
           highlighting and auto-indention modes. */
        tok->ttype = TOK1_T_HeredocStart2;
        BUMP(2);
      }     
      break;
    case '}': tok->ttype = TOK1_T_SquigglyClose; BUMP(1); break;
    case '[': tok->ttype = TOK1_T_BraceOpen; BUMP(1); break;
    case ']': tok->ttype = TOK1_T_BraceClose; BUMP(1); break;
    case '(': tok->ttype = TOK1_T_ParenOpen; BUMP(1); break;
    case ')': tok->ttype = TOK1_T_ParenClose; BUMP(1); break;
    default: break;
  }/* end of switch(*curpos) */

  try_identifier:
  if(0==rc
     && (curpos == startPos)
     && (TOK1_T_EOF != tok->ttype) ){
    /* Try to parse an identifier... */
    int const ch1 = *curpos;
    int const idChars = whcl__read_identifier2( curpos, tok1_izer_end(t),
                                                &curpos, t->flags );
    //MARKER(("idChars=%d, t->flags=%08x\n", idChars, t->flags));
    if(idChars &&
       (!IN_BOUNDS || !whcl__is_var_char(*curpos, false))){
      /* identifier or -flag string */
      t->currentCol += idChars;
      if('-'==ch1){
        tok->ttype = TOK1_T_WHCLFlag;
        tok->_adjBegin = tok->_begin;
        for(char const * b = startPos; '-'==*b; ++b){
          ++tok->_adjBegin;
        }
        tok1_en_adjend_set(&t->token, (curpos > tok1_izer_end(t))
                           ? tok1_izer_end(t) : curpos);
      }else{
        tok->ttype = TOK1_T_Identifier;
      }
    }else {
      curpos = startPos;
      assert(TOK1_T_INVALID==tok->ttype);
    }
  }
  if(TOK1_T_INVALID == tok->ttype){
    tok->ttype = TOK1_T_TokErr;
    /* MARKER(("byte=%02x\n", (unsigned char)*curpos)); */
    t->errMsg = "Don't know how to tokenize this.";
    rc = CWAL_SCR_SYNTAX;
  }
  tok1_en_end_set(&t->token, (curpos > tok1_izer_end(t))
                     ? tok1_izer_end(t) : curpos);
#undef BUMP
#undef RETURN_ERR
#undef NEXT_LINE
#undef IN_BOUNDS
#undef CHECK_OPS
  end:
  return rc;
}

int whcl_stoken_provider_f_whcl(whcl_script * const ct, tok1_izer * const t,
                               void * state){
  //whcl_engine const * const el = (whcl_engine const *)state;
  tok1_en * tok = &t->token;
  int rc;
  if(state){/*unused*/}
  rc = whcl__next_token_base(t, state);
  assert(t->ec);
  if(ct){/*unused*/}
  switch(rc ? 0 : tok->ttype){
    case TOK1_T_HeredocStart:
    case TOK1_T_HeredocStart2:
      rc = tok1_slurp_heredoc(t, whcl__next_token_base, state, NULL);
      break;
    case TOK1_T_IdentifierDeref:
    case TOK1_T_Identifier:{
      cwal_size_t tLen = 0;
      char const * tStr = tok1_en_cstr(tok, &tLen, false);
      whcl__bic const * bic;
      assert(tLen < (cwal_size_t)((uint16_t)-1));
      if('$'==*tStr && tLen>1){
        --tLen; ++tStr; // strip '$'
      }
      assert(!tok->ttype2);
      if((bic = whcl__bic_search(tStr, (uint16_t)tLen))){
        tok->ttype = TOK1_T_BIC;
        tok->ttype2 = bic->ttype;
      }else if((rc = whcl__biv_search(tStr, (uint16_t)tLen))){
        tok->ttype = TOK1_T_BIV;
        tok->ttype2 = rc;
        //MARKER(("Remapped Identifier to BIV %s\n", tok1_t_cstr(rc)));
        rc = 0;
      }
      break;
    }
    case 0:
      assert(0!=rc);
      t->_errToken = t->token;
      assert(t->errMsg);
      rc = tok1_err_toker(t, rc, "%s", t->errMsg);
      break;
    default:
      break;
  }
  return rc;
}

static cwal_tuple * whcl__dotop_holder(whcl_engine * const el){
  if(!el->scopes.current->dotHolder){
    /* Set up the "dotop holder" */
    cwal_tuple * const tu = cwal_new_tuple(el->ec, 3);
    if(!tu){
      WHCL__WARN_OOM;
      return NULL;
    }
    cwal_value * const tv = cwal_tuple_value(tu);
    cwal_ref(tv);
    cwal_value_make_vacuum_proof(tv, true);
    cwal_value_rescope(&el->scopes.current->cs, tv);
    el->scopes.current->dotHolder = tu;
  }
  return el->scopes.current->dotHolder;
}

void whcl__dotop_set( whcl_engine * const el, cwal_value * const lhs,
                      cwal_value * const key,
                      cwal_value * const holder ){
  /* Reminder: the validity checks here point to, essentially,
     internal misuse, where we've not cleaned up this state before it
     goes stale.

     A potential future problem is vacuuming-up of these pointers,
     which we can work around by ensuring that these values are in the
     evalHolder.
  */
  cwal_tuple * const tu = whcl__dotop_holder(el);
  if(!tu){
    whcl__fatal(CWAL_RC_OOM,
                "FIXME: changing this function to return a "
                "result code will be a relatively invasive change.")
      /*does not return*/;
  }
  cwal_value * const oldLhs = cwal_tuple_get(tu, 0);
  cwal_value * const oldKey = cwal_tuple_get(tu, 1);
  cwal_value * const oldHolder = cwal_tuple_get(tu, 2);
  /**
     Assert that all of them appear to still be valid references
     (because it's easy to mess that up). These assertions are not
     guaranteed to trigger in all error cases, but they catch the
     most common one that we've prematurely unref'd a value and he
     have a pointer to its cwal-side recycling bin.
  */
  /* MARKER(("Setting scope->dot...\n")); */
  if(oldHolder){
    assert( cwal_value_scope(oldHolder)
            || cwal_value_is_builtin(oldHolder) );
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
     Because any of holder/lhs/key can refer to or contain/own any
     other, as well as be the same instance of oldHolder/oldLhs/oldKey
     (in any combination!), we have to ref them all before we unref
     any of them.
  */
  if(holder) cwal_ref(holder);
  if(lhs) cwal_ref(lhs);
  if(key) cwal_ref(key);

  cwal_tuple_set(tu, 0, lhs);
  cwal_tuple_set(tu, 1, key);
  cwal_tuple_set(tu, 2, holder);

  if(oldHolder) cwal_unref(oldHolder);
  if(oldLhs) cwal_unref(oldLhs);
  if(oldKey) cwal_unref(oldKey);
  /* MARKER(("Done setting scope->dot...\n")); */
}

whcl_stoken const * whcl__args_hasflag( whcl__args const * const args,
                                       uint16_t ndx ){
  return ndx<args->argc && TOK1_T_WHCLFlag==args->argv[ndx]->ttype
    ? args->argv[ndx] : NULL;
}

bool whcl__args_next_matches(whcl__args const * const args,
                             uint16_t * ndx,
                             char const * flag ){
  return (*ndx<args->argc
          && whcl_stoken_matches(args->ct, args->argv[*ndx], flag, -1))
    ? (++*ndx, true) : false;
}

int whcl__args_ncheck( whcl__args const * const args,
                       uint16_t ndx, uint16_t minNeeded ){
  return (args->argc - ndx < minNeeded)
    ? whcl__script_throw(args->ct, CWAL_RC_MISUSE,
                     "Too few arguments for %s.",
                     args->commandName)
    : 0;
}

#if 0
bool wchl__is_callable_ref(whcl_engine * const el,
                           cwhcl_script * const ct,
                           whcl_stoken const * const tok,
                           whcl__bic const ** bic,
                           cwal__value ** fv){
}
#endif

/** cwal_value_visitor_f() impl which requires state to be a
    (cwal_array*) and appends v to that array. */
static int cwal_value_visitor_f_array_append( cwal_value * v, void * state ){
  return cwal_array_append((cwal_array *)state, v);
}

/**
   "Expands" xv into the target array: if xv is a list type, each of its
   elements is appended to tgt, else xv is appended to tgt.
*/
static int whcl__expand_value_to_array(cwal_array * const tgt, cwal_value * const xv){
  assert(xv);
  switch(cwal_value_type_id(xv)){
    case CWAL_TYPE_ARRAY:
      return cwal_array_visit(cwal_value_get_array(xv),
                              cwal_value_visitor_f_array_append,
                              tgt);
    case CWAL_TYPE_TUPLE:
      return cwal_tuple_visit(cwal_value_get_tuple(xv),
                              cwal_value_visitor_f_array_append,
                              tgt);
    default:
      return cwal_array_append(tgt, xv);
  }
}

/**
   If xv is a list type, each element of that list is visited by the
   given visitor function (visitor(theValue, state), else visitor(xv,
   state) is called.
*/
static int whcl__expand_value_to_visitor(cwal_value * const xv,
                                         cwal_value_visitor_f visitor,
                                         void * state){
  assert(xv);
  switch(cwal_value_type_id(xv)){
    case CWAL_TYPE_ARRAY:
      return cwal_array_visit(cwal_value_get_array(xv), visitor, state);
    case CWAL_TYPE_TUPLE:
      return cwal_tuple_visit(cwal_value_get_tuple(xv), visitor, state);
    default:
      return visitor(xv, state);
  }
}


int whcl__read_new_object(whcl_engine * const el,
                          whcl_script * ct,
                          whcl_stoken const * tok,
                          uint32_t rdobjFlags,
                          cwal_value ** rv){
  int rc = 0;
  cwal_size_t const holdLen = whcl__holder_len(el);
  cwal_value * ov = NULL;
  whcl_script sub = whcl__script_empty;
  whcl_script * const oldScript = el->ct;
  whcl_scope * scel = NULL;
  bool const fromScope = (WHCL__READ_OBJ_F_SCOPE & rdobjFlags);
  bool const useThis =
    (WHCL__READ_OBJ_F_THIS & rdobjFlags) == WHCL__READ_OBJ_F_THIS;
  whcl__stoken_set(ct, tok);
  el->ct = ct;
  while(fromScope){
    /* Eval {...} as a scope and make the result of this
       function be that scope's properties container
       (its vars). This is not only about 1/3rd as much
       code as the other approach, but it's much more
       powerful. */
    whcl__script_next_token2(ct,&tok);
    if(TOK1_T_SquigglyGroup != tok->ttype){
      rc = whcl_err_throw(el, CWAL_RC_MISUSE,
                          "Expecting a {...} block.");
      break;
    }
    if(!(scel = whcl__scope_push2(el))){
      WHCL__WARN_OOM; rc = CWAL_RC_OOM; break;
    }
    ov = whcl_scope_props(el, scel)
      /* Reminder to self: ov has a refcount point and is vacuum-proof
         by virtue of being the current scope's properties object. */;
    if(!ov){ WHCL__WARN_OOM; rc = CWAL_RC_OOM; break; }
    cwal_value_prototype_set(ov, whcl_prototype_object(el))
      /* Scope property objects have no prototype by default */;
    if(useThis) rc = whcl_set_this(el, ov);
    if(0==rc) rc = whcl__eval_sub2(el, false, ct, tok, 0, NULL);
    if(useThis) whcl_set_this(el, NULL);
    break;
  }
  if(fromScope){
    goto end;
  }

  /* Process the object as a list of key/value pairs... */
  ov = cwal_new_object_value(el->ec);
  if(!ov){ rc = whcl__err_oom(el); goto end;}
  rc = whcl__holder_push(el, ov);
  if(rc) goto end;
  
#define next_t \
  (&sub==ct ? whcl__next_token_no_eol(ct, &tok) \
   : whcl__script_next_token2(ct,&tok))
  for(int i = 0; true; ++i ){
    cwal_value * key = NULL;
    cwal_value * val = NULL;
    rc = next_t;
    if(!rc && 0==i){/* first iteration specifics... */
      if(!(scel = whcl__scope_push2(el))){
        rc = CWAL_RC_OOM;
        goto end;
      }
      if(TOK1_T_SquigglyGroup==tok->ttype){
        /* Optionally accept key/val pairs wrapped in {...} */
        rc = whcl__script_sub_from_group(ct, &sub, true);
        assert(0==rc);
        //assert(sub.nextToken);
        ct = &sub;
        rc = next_t;
        el->ct = &sub;
        //whcl__dump_stok(ct, tok, "first inner object-read token");
      }
    }
    if(rc || whcl_t_is_eox(tok->ttype)) break;
    cwal_size_t const preKeyLen = whcl__holder_len(el);
    //whcl__dump_stok(ct, tok, "key");
    switch(tok->ttype){
      case TOK1_T_BraceGroup:
      case TOK1_T_IdentifierDeref:
      case TOK1_T_ParenGroup:
        rc = whcl__eval_token(el, ct, tok, 0, &key);
        break;
      case TOK1_T_BIV:
      case TOK1_T_Identifier:
        if(tok->subscriptId) goto not_a_key;
        CWAL_SWITCH_FALL_THROUGH;
      case TOK1_T_BIC:
      case TOK1_T_LiteralNumber:
      case TOK1_T_QuotedString:
      case TOK1_T_SquigglyGroup:
        rc = whcl__create_value2(el, ct, tok, true, &key);
        break;
      default:
        not_a_key:
        rc = whcl__throw(el, ct, tok, CWAL_RC_TYPE,
                         "Invalid token type (%s) for object key.",
                         tok->subscriptId
                         ? "property access" : tok1_t_cstr(tok->ttype));
        break;
    }
    if(rc || (rc = whcl__holder_push(el, key))) break;
    if((rc = next_t)) break;
    //whcl__dump_stok(ct, tok, "val");
#undef next_t
    switch(whcl_t_is_eox(tok->ttype) ? 0 : tok->ttype){
      case 0:
        rc = whcl_err_throw(el, CWAL_SCR_SYNTAX,
                            "Objects require matching key/value pairs.");
        break;
      case TOK1_T_SquigglyGroup: {
        whcl_stoken const * k2 = whcl__script_at(ct, tok->id - 1);
        assert(k2);
        rc = whcl__read_new_object(el, ct, k2, 0, &val);
        whcl__stoken_set(ct, tok);
        break;
      }
      case TOK1_T_ParenGroup: {
        whcl_stoken const * k2 = whcl__script_at(ct, tok->id - 1);
        assert(k2);
        rc = whcl__read_new_array(el, ct, k2, &val);
        whcl__stoken_set(ct, tok);
        break;
      }
      default:
        rc = whcl__eval_token(el, ct, tok, 0, &val);
        break;
    }
    if(rc) break;
    cwal_ref(key); cwal_ref(val);
    whcl__script_errtoken_set(ct, tok);
    //whcl__dump_stok(ct, tok, "about to set");
    rc = whcl_set_v(el, ov, key, val ? val : cwal_value_undefined());
    //whcl__dump_stok(ct, tok, "just setted");
    whcl__script_errtoken_set(ct, NULL);
    cwal_unref(key); cwal_unref(val);
    whcl__holder_truncate(el, preKeyLen, NULL);
    if(rc) break;
  }
  whcl__script_finalize(&sub);
  end:
  el->ct = oldScript;
  cwal_value * const propagate = (rc || !rv) ? NULL : (*rv=ov);
  if(scel) whcl_scope_pop(el, scel, propagate);
  whcl__holder_truncate(el, holdLen, propagate);
  return rc;
}

int whcl__read_new_array(whcl_engine * const el, whcl_script * ct,
                         whcl_stoken const * tok, cwal_value ** rv){
  int rc = 0;
  cwal_size_t const holdLen = whcl__holder_len(el);
  cwal_array * const ar = cwal_new_array(el->ec);
  cwal_value * const av = cwal_array_value(ar);
  if(!ar) return whcl__err_oom(el);
  rc = whcl__holder_push(el, av);
  if(rc) return rc;
  whcl_script * const oldScript = el->ct;
  whcl_script sub = whcl__script_empty;
  whcl_scope * scel = NULL;
  whcl__stoken_set(ct, tok);
  el->ct = ct;
#define next_t \
  (&sub==ct ? whcl__next_token_no_eol(ct, &tok) \
   : whcl__script_next_token2(ct,&tok))
  for(int i = 0; true; ++i ){
    cwal_value * val = NULL;
    rc = next_t;
    if(0==rc && 0==i){
      if(!(scel = whcl__scope_push2(el))){rc = CWAL_RC_OOM; goto end;}
      else if(TOK1_T_ParenGroup==tok->ttype){
        /* Optionally accept key/val pairs wrapped in (...)
           Note that we cannot use {...} for this purpose because
           that would lead to an ambiguity: is the {...} intended to
           be a plain array entry or a list of values? */
        rc = whcl__script_sub_from_group(ct, &sub, true);
        assert(0==rc);
        ct = &sub;
        rc = next_t;
      }
    }
    if(rc || whcl_t_is_eox(tok->ttype)) break;
    switch(tok->ttype){
      case TOK1_T_SquigglyGroup: {
        whcl_stoken const * k2 = whcl__script_at(ct, tok->id - 1);
        assert(k2);
        rc = whcl__read_new_object(el, ct, k2, 0, &val);
        whcl__stoken_set(ct, tok);
        break;
      }
      case TOK1_T_ParenGroup: {
        whcl_stoken const * k2 = whcl__script_at(ct, tok->id - 1);
        assert(k2);
        rc = whcl__read_new_array(el, ct, k2, &val);
        whcl__stoken_set(ct, tok);
        break;
      }
      default:
        rc = whcl__eval_token(el, ct, tok,
                              WHCL__EVAL_F_EXPANDO_OKAY, &val);
        break;
    }
    if(rc) break;
    cwal_ref(val);
    rc = whcl__expand_value_to_array(ar, val);
    cwal_unref(val);
    if(rc) break;
#undef next_t
  }
  end:
  el->ct =oldScript;
  cwal_value * const propagate = (rc || !rv) ? NULL : (*rv=av);
  if(scel) whcl_scope_pop(el, scel, propagate);
  whcl__script_finalize(&sub);
  whcl__holder_truncate(el, holdLen, propagate);
  return rc;
}

/**
   Confirms that the given token array (which must contain at least nToks
   valid tokens followed by a NULL pointer) meets the basic arguments
   requirements for bic then calls bic->f(). Returns 0 on success
   and any number of potential non-0 codes on error.
*/
static int whcl__bic_call(whcl_engine * const el, whcl__bic const * const bic,
                          bool isSubcall,
                          whcl_script * const ct, whcl_stoken const * const * tokens,
                          uint16_t nToks, cwal_value ** rv){
  int rc = whcl__bic_check_argc(el, bic, ct, tokens, nToks);
  if(0==rc){
    whcl__args const whar = {bic->name, el, ct, tokens, nToks, isSubcall};
    whcl__strace_entry strace = whcl__strace_entry_empty;
    if(0==(rc = whcl__strace_push(el, ct, tokens[0], &strace))){
      rc = bic->f( bic, &whar, rv );
      /* Reminder: unlike non-builtins, builtins run in the current
         scope unless they explicitly push their own. This is... kinda
         bad, but not really. Some builtins _need_ to use the current
         scope to work (most notably `decl` and `info`). WHCL/cwal has
         no "upvar" capability like TCL does, so some commands simply
         must run in the current scope. */
      if(rc) whcl__annotate_exception(el, NULL);
      whcl__strace_pop(el);
    }
  }
  return rc;
}

/**
   A helper for checking whether a given builtin command's argument
   token refers to another BIC. If the 2nd argument refers to a BIC,
   this function calls whcl__bic_call() as a sub-call, passing on all
   relevant arguments, assigns the result of that call to `*theRc` and
   returns true. If the 2nd argument is not a reference to a BIC then
   `*theRc` is set to 0 and false is returned.

   If tokBicNdx is out of bounds for args->argv, `*theRc` is set to 0
   and false is returned.
*/
static bool whcl__check_bic_call(whcl__args const * const args,
                                 uint16_t tokBicNdx,
                                 cwal_value ** rv,
                                 int * theRc){
  whcl_stoken const * const tokMaybeBic = tokBicNdx<args->argc
    ? args->argv[tokBicNdx] : NULL;
  if(tokMaybeBic){
    whcl__bic const * const subBic =
      whcl__ttype_get_bic(tokMaybeBic->ttype2);
    *theRc = subBic
      ? whcl__bic_call(args->el, subBic, true, args->ct,
                       args->argv + tokBicNdx, args->argc-tokBicNdx,
                       rv)
      : 0;
    return !!subBic;
  }else{
    *theRc = 0;
    return false;
  }
}


/**
   Processes nTok tokens from the given token array, calls f with
   vSelf as its "this", and assigns `*rv` to the result of that
   call. Returns 0 on success or propagates any number of possible
   codes on error.

   It is the caller's responsibility to ensure that f and vSelf are in
   the eval holder, as this routine will eval the given tokens and may
   trigger a sweep or vacuum.

   This function runs the argument processing in the current scope
   and the function in a new scope.
*/
static int whcl__call_cwal_f(whcl_engine * const el,
                             whcl_script * const ct,
                             cwal_function * const f,
                             cwal_value * const vSelf,
                             whcl_stoken const * const * tokens,
                             uint16_t nTok,
                             cwal_value **rv){
  int rc = 0;
  cwal_array * aargv = NULL;
  cwal_value * vargv[nTok];
  //cwal_scope scope = cwal_scope_empty;
  //rc = cwal_scope_push2(el->ec, &scope);
  //if(rc) return rc;
  whcl_scope * scel = whcl__scope_push(el, 0);
  if(!scel){ /*cwal_scope_pop(el->ec);*/ return CWAL_RC_OOM; }
  for(uint16_t i = 0; 0==rc && i<nTok; ++i){
    cwal_value * xrv = NULL;
    bool tokIsExpando = false;
    whcl_stoken const * const tok = tokens[i];
    if( TOK1_T_AtExpand == tok->ttype ){
      tokIsExpando = true;
      if(!aargv){
        /* Encountered first @expando: Switch into func-args-as-array
           mode (which is less efficient, which is the reason we don't
           use it by default). */
        aargv = cwal_new_array(el->ec);
        if(!aargv){WHCL__WARN_OOM; rc = CWAL_RC_OOM; break;}
        rc = whcl__holder_push(el, cwal_array_value(aargv));
        if(rc) break;
        for(uint16_t j = 0; 0==rc && j < i; ++j){
          rc = cwal_array_append(aargv, vargv[j]);
        }
        if(rc) break;
      }
    }
    rc = whcl__eval_token(el, ct, tok, WHCL__EVAL_F_EXPANDO_OKAY, &xrv);
    if(rc){
      assert(NULL==xrv);
    }else if(aargv){
      if(tokIsExpando){
        cwal_ref(xrv);
        rc = whcl__expand_value_to_array(aargv, xrv);
        cwal_unref(xrv);
      }else{
        rc = cwal_array_append(aargv, xrv)
          /* aargv is in the eval holder, so xrv is now also
             vacuum-proof. */;
      }
    }else{
      assert(!tokIsExpando);
      rc = whcl__holder_push(el, xrv);
      vargv[i] = xrv;
    }
  }
  if(0==rc){
    assert(scel == el->scopes.current);
    scel->flags |= WHCL__SCOPE_F_CALL;
    //MARKER(("test-xsym. func@%p Flags=0x%04x\n",
    //        (void*)cwal_function_value(f),
    //        cwal_container_client_flags_get(cwal_function_value(f))));
    if(WHCL_CONTAINER_F_XSYM &
       cwal_container_client_flags_get(cwal_function_value(f))){
      /* Reminder to self: ^^^ this check does not work when f is a
         __command function which is going to dispatch the next
         argument as a function. See whcl_function_forward() and
         whcl_function_call() for the workarounds.
      
         We "could" argue that native functions should not block
         symbol lookup on the grounds that they generally do not look
         up symbols except indirectly, while running passed-on
         functions or eval'able code blocks, both of which (especially
         the latter) potentially benefit from unblocked symbol
         lookups.  This discrepancy bothers me, though. So... we'll
         use a container flag slot for this marker and apply it
         equally to script- and non-script functions. */
      scel->flags |= WHCL__SCOPE_F_XSYM;
    }
    cwal_value * const vWith = whcl__replace_with(el, NULL);
    rc = aargv
      ? cwal_function_call_array(NULL, f, vSelf, rv, aargv)
      : cwal_function_call(f, vSelf, rv, nTok, vargv);
    whcl__replace_with(el, vWith);
  }
  cwal_value * const propagate = rc||!rv ? NULL : *rv;
  //if(scope.parent) cwal_scope_pop2(el->ec, propagate);
  whcl_scope_pop(el, scel, propagate);
  return rc;
}

bool whcl__token_has_prop(whcl_script const * const ct,
                               whcl_stoken const * const tok){
  switch(tok->ttype){
    case TOK1_T_PropAccessWith: return true;
    default:
      return tok->subscriptId &&
        TOK1_T_PropAccess == whcl__script_at(ct, tok->subscriptId)->ttype;
  }
}

int whcl__ttype_legal_prop_lhs(int ttype){
  switch(ttype){
    case TOK1_T_IdentifierDeref:
    case TOK1_T_Identifier:
    case TOK1_T_QuotedString:
    case TOK1_T_LiteralNumber:
    case TOK1_T_BIV:
      //case TOK1_T_ParenGroup:
    case TOK1_T_BraceGroup:
    case TOK1_T_CallBlock:
      return ttype;
  }
  return 0;
}

bool whcl__t_legal_prop_lhs(whcl_stoken const * const tok){
  return tok && !tok->subscriptId
    && whcl__ttype_legal_prop_lhs(tok->ttype);
}

/**
   Requires tok to be one of (Identifier, BIV, IdentifierDeref). It looks
   up a value for its symbol. On success, sets *rv to that value and
   returns 0. On error, throws.

   For consistency with the other eval APIs, it permits a NULL rv
   but in that case it will simply skip over processing of
   BIV tokens because processing them requires a non-NULL rv.

   It will resolve vars/identifiers in these 3 forms: X, $X, and $X[Y]
   but it only resolves the X part, not nested components such as
   $X[Y] or $X[Y][Z][...]. The latter happens in
   whcl__eval_token(). Whether or not it would would make sense to
   move that particular bit of code into this function is still
   unclear. It requires diving into tok->subscriptId, rather than
   consuming further from tok's level of the chain, so there would
   seem to be no technical complications or API incompatibilities in
   doing so, but it currently feels "unmodular."  Time will tell.
*/
static int whcl__varident_lookup(whcl_engine * const el,
                                 whcl_script * const ct,
                                 whcl_stoken const * const tok,
                                 cwal_value **rv){
  bool dollarSign = 0;
  switch(tok->ttype){
    case TOK1_T_BIV:
      return rv ? whcl__create_value(el, ct, tok, rv) : 0;
    case TOK1_T_IdentifierDeref:
      dollarSign = '$'==*whcl_stoken_begin(ct, tok);
      CWAL_SWITCH_FALL_THROUGH;
    case TOK1_T_Identifier:{
      int rc;
      cwal_midsize_t tlen = 0;
      char const * cstr = whcl_stoken_cstr(ct, tok, &tlen, false);
      if(dollarSign){ // skip leading '$'
        ++cstr;
        --tlen;
      }
      whcl__script_errtoken_set(ct, tok);
      //whcl__dump_stok(ct, tok, "looking up sym");
      rc = whcl_lookup_vsym(el, NULL, cstr, (cwal_int_t)tlen,
                            true, rv);
      if(!rc) whcl__script_errtoken_set(ct, NULL);
      return rc;
    }
    default:
      whcl__script_errtoken_set(ct, tok);
      return whcl__script_throw(ct, CWAL_RC_TYPE,
                            "Invalid token type (%s) for %s()",
                            tok1_t_cstr(tok->ttype),
                            __func__);
  }
}


/** A dummy/placeholder whcl__bic_f() implementation. */
static int whcl__bic_f_dummy(whcl__bic const * const bic,
                             whcl__args const * const args,
                             cwal_value **rv){
  if(rv) *rv = cwal_value_undefined();
  if(args){/*unused*/}
  MARKER(("NOT YET IMPLEMENTED BUILTIN: %s\n", bic->name));
#if 1
  return whcl_err_throw(args->el, CWAL_RC_NOT_FOUND,
                        "Just testing exception annotation.");
#else
  return 0;
#endif
}

/** Handler for the array command */
static int whcl__bic_f_array(whcl__bic const * const bic,
                             whcl__args const * const args,
                             cwal_value **rv){
  int rc = 0;
  if(bic){/*unused*/}
  if(args->argc>2 && TOK1_T_ParenGroup==args->argv[1]->ttype){
    goto err_usage;
  }
  rc = whcl__read_new_array(args->el, args->ct, args->argv[0], rv);
  return rc;
  err_usage:
  return whcl__throw(args->el, args->ct, args->argv[1],
                          CWAL_SCR_SYNTAX,
                          "Expecting (...) or key/value pair arguments.");
}

/** Handler for the assert/affirm commands */
static int whcl__bic_f_affert(whcl__bic const * const bic,
                               whcl__args const * const args,
                               cwal_value **rv){
  int rc = 0;
  cwal_value * xrv = NULL;
  bool const trace =
    TOK1_T_BIC_assert==bic->ttype
    ? args->el->flags.traceAssert
    : args->el->flags.traceAffirm;

  whcl_stoken const * const tBody = args->argv[1];
  if(2==args->argc && TOK1_T_SquigglyGroup == tBody->ttype){
    rc = whcl__eval_token(args->el, args->ct, tBody,
                          WHCL__EVAL_F_SQUIGGLY_AS_SCOPE, &xrv);
  }else{
    whcl__stoken_set(args->ct, args->argv[0]);//tBody);//
    rc = whcl__eval_expr(args->el, 0, &xrv);
  }
  if(0==rc){
    whcl_stoken const * const tok = tBody;
    whcl_stoken const * const tokEnd = args->argv[args->argc-1];
    whcl_stoken const * const tokEndNext = whcl_stoken_sibling(args->ct, tokEnd);
    char const * const begin = whcl_stoken_begin(args->ct, tok);
    char const * const end = tokEndNext
      ? whcl_stoken_begin(args->ct, tokEndNext)
      /* ^^^ so that the string covers the whole range of a block
         construct or $var[key]. */
      : whcl_stoken_end(args->ct, tokEnd);
    bool const ok = cwal_value_get_bool(xrv);
    //whcl__dump_val(xrv, "xrv");
    cwal_refunref(xrv);
    if(trace){
      cwal_outputf(args->el->ec, "%s %s @ [%s]:%d,%d: %.*s\n",
                   TOK1_T_BIC_assert==bic->ttype
                   ? "Assertion" : "Affirmation",
                   ok ? "passed" : "failed",
                   args->ct->name,
                   (int)tok->line, (int)tok->column,
                   (int)(end-begin), begin);
    }
    if(ok){
      if(rv) *rv = cwal_value_true();
    }else{
      rc = (TOK1_T_BIC_assert==bic->ttype)
        ? whcl__err(args->el, args->ct, args->argv[1], CWAL_RC_ASSERT,
                         "Assertion failed: %.*s", (int)(end-begin), begin)
        : whcl__throw(args->el, args->ct, args->argv[1], CWAL_RC_EXCEPTION,
                           "Affirmation failed: %.*s", (int)(end-begin), begin);
    }
  }
  return rc;
}

/** Handler for the break, continue, return commands */
static int whcl__bic_f_brc(whcl__bic const * const bic,
                           whcl__args const * const args,
                           cwal_value **rv){
  int rc = 0;
  cwal_value * xrv = NULL;
  switch(bic->ttype){
    case TOK1_T_BIC_continue:
      if(!(WHCL__SCOPE_F_LOOP & args->el->scopes.current->flags)){
        goto invalid_bc;
      }
      return CWAL_RC_CONTINUE;
    case TOK1_T_BIC_break: // return|break [value]
      if(!(WHCL__SCOPE_F_LOOP & args->el->scopes.current->flags)){
        goto invalid_bc;
      }
      CWAL_SWITCH_FALL_THROUGH;
    case TOK1_T_BIC_return:
      if(!whcl__check_bic_call(args, 1, &xrv, &rc) && 0==rc){
        if(args->argc>2){
          whcl__script_errtoken_set(args->ct, args->argv[2]);
          rc = whcl_err_throw(args->el, CWAL_RC_MISUSE,
                              "Extra arguments after %s.", bic->name);
        }else if(args->argc>1){
          rc = whcl__eval_token(args->el, args->ct, args->argv[1],
                                0, &xrv);
        }
      }
      if(rc) return rc;
      *rv = cwal_value_undefined();
      cwal_propagating_set(args->el->ec,
                           xrv ? xrv : cwal_value_undefined());
      return TOK1_T_BIC_return==bic->ttype
        ? CWAL_RC_RETURN : CWAL_RC_BREAK;
    default:
      whcl__fatal(CWAL_RC_CANNOT_HAPPEN, "Invalid builtin mapping: %s",
                  bic->name);
      return 0/*not reached*/;
  }
  invalid_bc:
  return whcl_err_throw(args->el, CWAL_SCR_SYNTAX,
                        "'%s' may only be used in a loop body.",
                        bic->name);
}

/** Handler for the 'catch' command. */
static int whcl__bic_f_catch(whcl__bic const * const bic,
                             whcl__args const * const args,
                             cwal_value **rv){
  int rc = 0;
  cwal_value * xrv = NULL;
  cwal_value * lhs = NULL;
  cwal_value * key = NULL;
  whcl_stoken const * tArg = NULL;
  whcl_stoken const * tVar = NULL;
  whcl_stoken const * tBody = NULL;
  cwal_size_t const holderLen = whcl__holder_len(args->el);
  whcl_scope * scel = NULL;
  uint16_t argNdx = 1;
  bool noScope = false;
  while( (tArg = whcl__args_hasflag(args, argNdx)) ){
    ++argNdx;
    if(whcl_stoken_matches(args->ct, tArg, "-noscope", 8)){
      noScope = true;
      continue;
    }else{
      whcl__script_errtoken_set(args->ct, tArg);
      rc = whcl_err_throw(args->el, CWAL_RC_MISUSE,
                          "Invalid flag for %s: %.*s",
                          bic->name, (int)tArg->length,
                          whcl_stoken_cstr(args->ct, tArg, NULL, false));
      goto end;
    }
  }
  uint16_t const argc = args->argc - argNdx;
  if(argc<1 || argc>2){
    whcl__script_errtoken_set(args->ct, args->argv[0]);
    return whcl_err_set(args->el, CWAL_SCR_SYNTAX,
                      "Expecting: %s %s", bic->name,
                      bic->usage);
  }else if(1==argc){
    tBody = args->argv[argNdx];
  }else{
    tVar = args->argv[argNdx++];
    tBody = args->argv[argNdx++];
    switch(whcl__token_has_prop(args->ct, tVar)
           ? 0 : tVar->ttype){
      case TOK1_T_Identifier:
        rc = whcl__create_value(args->el, args->ct, tVar, &key);
        if(rc || (rc = whcl__holder_push(args->el, key))) goto end;
        else if(whcl_var_search_v(args->el, NULL, false, key, NULL)){
          whcl__script_errtoken_set(args->ct, tVar);
          rc = whcl_err_set(args->el, CWAL_RC_ALREADY_EXISTS,
                            "Variable '%.*s' is already declared in "
                            "the current scope.", (int)tVar->length,
                            whcl_stoken_cstr(args->ct, tVar, NULL, false));
          goto end;
        }
        break;
      case 0:
        rc = whcl__eval_token(args->el, args->ct, tVar, 0, NULL);
        if(rc) goto end;
        whcl__dotop_get(args->el, &lhs, &key, NULL);
        assert(lhs && key);
        if((rc = whcl__holder_push(args->el, lhs))
           || (rc = whcl__holder_push(args->el, key))) goto end;
        break;
      default:
        whcl__script_errtoken_set(args->ct, tVar);
        goto err_syntax;
    }
  }
  if(TOK1_T_SquigglyGroup!=tBody->ttype){
    whcl__script_errtoken_set(args->ct, tBody);
    goto err_syntax;
  }
  if(!noScope && !(scel = whcl__scope_push2(args->el))){
    rc = CWAL_RC_OOM; goto end;
  }
  whcl__stoken_set(args->ct, tBody);
  rc = whcl__eval_sub(args->el, false, args->ct, 0, NULL);
  //MARKER(("rc=%s\n", cwal_rc_cstr(rc)));
  switch(rc){
    /* A hopefully-complete list of error conditions we absolutely
       cannot and must not catch. */
    case CWAL_RC_ASSERT:
    case CWAL_RC_BREAK:
    case CWAL_RC_CANNOT_HAPPEN:
    case CWAL_RC_CONTINUE:
    case CWAL_RC_EXIT:
    case CWAL_RC_FATAL:
    case CWAL_RC_OOM:
    case CWAL_RC_INTERRUPTED:
      //MARKER(("Not uplifting catch code %s\n", cwal_rc_cstr(rc)));
    case 0:
      break;
    case CWAL_RC_EXCEPTION:
      handle_exception:
      xrv = cwal_exception_take(args->el->ec);
      assert(xrv && "Else violation of CWAL_RC_EXCEPTION semantics.");
      //whcl__dump_val(xrv, "exception");
      rc = 0;
      break;
    case CWAL_SCR_SYNTAX:
    default:
      /* Transform non-exception errors with this code to
         exceptions. This behavior is adopted from s2, where it's
         worked out fairly well for us. In s2 we only transform
         CWAL_SCR_SYNTAX, but CWAL_RC_NOT_FOUND is another one which
         can sensibly be caught _when_ it crops up from a failed
         symbol lookup. */
      if(whcl_err_has(args->el, false)){
        xrv = cwal_error_exception(args->el->ec, NULL, NULL, 0, 0);
        if(xrv){
          /* We're expecting that the error already has at least
             script/line/column info, but we may need to add the stack
             trace to it, noting that this will trace only to _this_
             point in the call stack. */
          cwal_exception_set(args->el->ec, xrv);
          whcl__exception_add_stacktrace(args->el, xrv);
          //whcl__dump_val(xrv, "Error changed to exception.");
          goto handle_exception;
        }else{
          WHCL__WARN_OOM;
          rc = CWAL_RC_OOM;
        }
      }else{
        rc = cwal_exception_setf(args->el->ec, rc,
                                 "Non-exception %s error with no "
                                 "additional information.",
                                 cwal_rc_cstr(rc));
        if(0==rc) goto handle_exception;
      }
      break;
  }
  end:
  if(scel) whcl_scope_pop(args->el, scel, rc ? NULL : xrv);
  if(0==rc){
    if(lhs){
      rc = whcl_set_v(args->el, lhs, key,
                      xrv ? xrv : cwal_value_undefined());
    }else if(key){
      rc = whcl_scope_set_v(args->el, NULL, false, key,
                            xrv ? xrv : cwal_value_undefined());
    }
    if(0==rc && xrv && rv){
      whcl__holder_truncate(args->el, holderLen, xrv);
      *rv = xrv;
    }else{
      whcl__holder_truncate(args->el, holderLen, NULL);
      if(rv) *rv = cwal_value_undefined();
    }
  }else{
    whcl__holder_truncate(args->el, holderLen, NULL);
  }
  return rc;
  err_syntax:
  rc = whcl_err_set(args->el,
                      CWAL_SCR_SYNTAX,
                      "Expecting [varname|$var[key]] {...} "
                      "after '%s'.", bic->name);
  goto end;
}

/**
   Handler for the concat command. Reminder to self:
   we allow the single-argument case, though seemingly senseless,
   to simplify converting non-strings to strings.
*/
static int whcl__bic_f_concat(whcl__bic const * const bic,
                              whcl__args const * const args,
                              cwal_value **rv){
  int rc = 0;
  uint16_t ndx = 1;
  cwal_buffer buf = cwal_buffer_empty
    /* We can't use args->el->escBuf without a bunch of hoop-jumping
       because any given argument might recursively invoke this. */
    ;
  if(bic){/*unused*/}
  for(uint16_t i = ndx; 0==rc && i < args->argc; ++i){
    cwal_value * xv = NULL;
    //whcl__dump_stok(args->ct, args->argv[i],"concat ARG");
    rc = whcl__eval_token(args->el, args->ct, args->argv[i], 0, &xv);
    if(0==rc){
      if(xv){
        cwal_ref(xv);
        rc = whcl_value_to_buffer(args->el, &buf, xv);
        cwal_unref(xv);
      }else{
        MARKER(("WARNING: token evaluated to NULL.\n"));
        whcl__dump_stok(args->ct, args->argv[i], "eval'd to NULL");
      }
    }else if(xv){
      cwal_refunref(xv);
    }
  
  }
  if(0==rc){
    *rv = cwal_buffer_to_zstring_value(args->el->ec, &buf);
  }else{
    cwal_buffer_clear(args->el->ec, &buf);
  }
  return rc;
}

/** Handler for the increment/decrement commands. */
static int whcl__bic_f_crement(whcl__bic const * const bic,
                               whcl__args const * const args,
                               cwal_value **rv){
  int rc = 0;
  cwal_size_t const holdLen = whcl__holder_len(args->el);
  whcl_stoken const * tArg;
  cwal_value * value = NULL;
  cwal_value * lhs = NULL;
  cwal_value * key = NULL;
  cwal_value * crv = NULL;
  cwal_value * newV = NULL;
  whcl_scope * foundIn = NULL;
  uint16_t ndx = 1;
  assert(args->argc>1 && args->argc<4);
  if(args->argc - ndx > 1){
    tArg = args->argv[ndx+1];
    rc = whcl__eval_token(args->el, args->ct, tArg, 0, &crv);
    if(rc || (rc = whcl__holder_push(args->el, crv))) goto end;
  }
  tArg = args->argv[ndx];
  switch(whcl__token_has_prop(args->ct, tArg)
         ? 0 : tArg->ttype){
    case 0:
      rc = whcl__eval_token(args->el, args->ct, tArg, 0, &value);
      if(rc) goto end;
      whcl__dotop_get(args->el, &lhs, &key, NULL);
      assert(lhs && key);
      if((rc = whcl__holder_push(args->el, lhs))
         || (rc = whcl__holder_push(args->el, key))) goto end;
      if(!value) value = cwal_new_integer(args->el->ec, 0);
      break;
    case TOK1_T_Identifier:
      rc = whcl__create_value(args->el, args->ct, tArg, &key);
      if(rc || (rc = whcl__holder_push(args->el, key))) goto end;
      assert(key);
      value = whcl_var_search_v(args->el, NULL, true, key, &foundIn);
      if((rc = whcl_err_has(args->el, true))){
        goto end;
      }else if(!value){
        rc = whcl_err_throw(args->el, CWAL_RC_NOT_FOUND,
                            "Cannot resolve var '%.*s'.",
                            (int)tArg->length,
                            whcl_stoken_cstr(args->ct, tArg, NULL, false));
        goto end;
      }
      assert(foundIn);
      break;
    default:
      whcl__script_errtoken_set(args->ct, tArg);
      rc = whcl_err_throw(args->el, CWAL_RC_TYPE,
                          "Invalid token type (%s) for '%s' command key.",
                          tok1_t_cstr(tArg->ttype),
                          bic->name);
      goto end;
  }
  assert(0==rc);
  if(!crv){
      crv = cwal_new_integer(args->el->ec, 1)/*does not allocate/cannot fail*/;
  }
  rc = whcl__values_addsub(args->el, TOK1_T_BIC_incr==bic->ttype,
                           value, crv, &newV);
  value = NULL;
  if(rc) goto end;
  else if(lhs){ // object property
    assert(lhs && key);
    rc = whcl_set_v(args->el, lhs, key, newV);
  }else{ // scope var
    assert(foundIn);
    rc = whcl_scope_set_v(args->el, foundIn, false, key, newV);
  }
  if(0==rc && rv){
    *rv = newV;
    whcl__holder_truncate(args->el, holdLen, newV);
  }
  end:
  if(lhs) whcl__dotop_set(args->el, NULL, NULL, NULL);
  whcl__holder_truncate(args->el, holdLen, NULL);
  return rc;
}

/**
   Helper for `decl`:

   decl [-const] {
     key val
     key val...
   }

   Resolves to `undefined`.

   TODO:

   - Add support for `set` as well. That requires a few changes,
   e.g. the ability to have x.y as a key. The 4th arg must be true
   for decl and false for set.

   - Consider adding support for commands in the value positions,
   consuming until EOL/EOF.

   Returns blah blah blah and ownership is blah blah blah. You know
   the drill.
*/
static int whcl__decl_read_object(whcl_engine * const el,
                                  whcl_script * ct,
                                  whcl_stoken const * tok,
                                  bool isDecl, bool isConst){
  int rc = 0;
  cwal_size_t const holdLen = whcl__holder_len(el);
  whcl_script * const oldScript = el->ct;
  whcl_scope * const scel = whcl__scope_current(el);
  cwal_value * const props = whcl_scope_props(el, scel);
  whcl_script sub = whcl__script_empty;
  assert(TOK1_T_SquigglyGroup == tok->ttype);
  if(!props) return CWAL_RC_OOM;
  whcl__stoken_set(ct, tok);
  rc = whcl__script_sub_from_group(ct, &sub, true);
  if(rc) return rc;
  el->ct = ct;
  ct = &sub;
  /* Process the object as a list of key/value pairs... */
#define next_t whcl__next_token_no_eol(ct, &tok)
  for(int i = 0; true; ++i ){
    cwal_value * key = NULL;
    cwal_value * val = NULL;
    whcl_stoken const * tKey;
    rc = next_t;
    if(rc || whcl_t_is_eox(tok->ttype)) break;
    cwal_size_t const preKeyLen = whcl__holder_len(el);
    switch(tok->ttype){
#if 0
      /* TODO: allow for `set` but not `decl`: */
      case TOK1_T_BraceGroup:
      case TOK1_T_IdentifierDeref:
      case TOK1_T_ParenGroup:
        rc = whcl__eval_token(el, ct, tok, 0, &key);
        break;
      case TOK1_T_LiteralNumber:
      case TOK1_T_QuotedString:
      case TOK1_T_SquigglyGroup:
#endif
      case TOK1_T_BIV:
      case TOK1_T_Identifier:
        if(tok->subscriptId) goto not_a_key;
        CWAL_SWITCH_FALL_THROUGH;
      case TOK1_T_BIC:
        rc = whcl__create_value2(el, ct, tok, true, &key);
        break;
      /* TODO: handle keys with subscriptId (property access)
         for isDecl==false (SET but not DECL). */
      default:
        not_a_key:
        rc = whcl__throw(el, ct, tok, CWAL_RC_TYPE,
                         "Invalid token type (%s) for a `%s` key.",
                         tok->subscriptId
                         ? "property access" : tok1_t_cstr(tok->ttype),
                         isDecl ? "decl" : "set");
        break;
    }
    if(rc || (rc = whcl__holder_push(el, key))) break;
    cwal_type_id const keyTypeID = cwal_value_type_id(key);
    switch(keyTypeID){
      case CWAL_TYPE_STRING: break;
      default:
        rc = whcl__throw(el, ct, tok, CWAL_RC_TYPE,
                         "Invalid type (%s) for var name.",
                         cwal_type_id_name(keyTypeID));
        break;
    }
    tKey = tok;
    if(rc || (rc = next_t)) break;
#undef next_t
    switch(whcl_t_is_eox(tok->ttype) ? 0 : tok->ttype){
      case 0:
        rc = whcl_err_throw(el, CWAL_SCR_SYNTAX,
                            "Expecting matching key/value pairs.");
        /* We "could" simply default to undefined for the final value
           when isDecl==true but that seems likely to lead to silent
           misuse and downstream confusion. */
        break;
      default:
        rc = whcl__eval_token(el, ct, tok, 0, &val);
        if(0==rc && !val) val = cwal_value_undefined();
        break;
    }
    if(rc || (rc = whcl__holder_push(el, val))) break;
    whcl__script_errtoken_set(ct, tok);
    if(isDecl && whcl_get_v(el, props, key)){
      cwal_size_t nKey = 0;
      char const * zKey = cwal_value_get_cstr(key, &nKey);
      rc = whcl__throw(el, ct, tKey, CWAL_RC_ALREADY_EXISTS,
                       "Already declared in this scope: %.*s",
                       (int)nKey, zKey);
      break;
    }
    cwal_ref(key); cwal_ref(val);
    rc = whcl_set_with_flags_v(el, props, key,
                               val ? val : cwal_value_undefined(),
                               isConst ? CWAL_VAR_F_CONST : 0);
    cwal_unref(key); cwal_unref(val);
    whcl__holder_truncate(el, preKeyLen, NULL);
    if(rc) break;
  }
  whcl__script_finalize(&sub);
  el->ct = oldScript;
  whcl__holder_truncate(el, holdLen, NULL);
  return rc;
}

/** Handler for the decl command:

    - `decl [-const] name [value]|builtin command`

    Noting that:

    - `-const` requires a value.

    Result value is that of the declared value (defaulting
    to the undefined value).
 */
static int whcl__bic_f_decl(whcl__bic const * const bic,
                            whcl__args const * const args,
                            cwal_value **rv){
  int rc = 0;
  uint16_t ndx = 1;
  cwal_value * xv = NULL;
  whcl_engine * const el = args->el;
  cwal_size_t const holdLen = whcl__holder_len(el);
  bool doConst = TOK1_T_BIC_const==bic->ttype;
  whcl_stoken const * tArg;
  whcl_stoken const * tName;
  while( (tArg = whcl__args_hasflag(args, ndx)) ){
    ++ndx;
    if(whcl_stoken_matches(args->ct, tArg, "-const", 6)){
      if(doConst){
        return whcl_err_set(el, CWAL_RC_MISUSE,
                            "Cannot apply const twice.");
      }
      doConst = true;
      continue;
    }else{
      rc = whcl_err_set(el, CWAL_RC_MISUSE,
                        "Invalid flag for %s: %.*s",
                        bic->name, (int)tArg->length,
                        whcl_stoken_cstr(args->ct, tArg, NULL, false));
      goto end;
    }
    /* i wonder if it might be interesting to add -unique and create
       a unique value with an optional wrapped value. */
  }
  if((rc = whcl__args_ncheck(args, ndx, 1 + doConst))) goto end;
  tName = args->argv[ndx++];
  if(TOK1_T_SquigglyGroup==tName->ttype){
    if(args->argv[ndx]){
      rc = whcl__throw(el, args->ct, args->argv[ndx], CWAL_SCR_SYNTAX,
                       "Extra arguments after {...}.");
      goto end;
    }
    rc = whcl__decl_read_object(args->el, args->ct, tName, true, doConst);
    goto end;
  }  
  whcl__stoken_set(args->ct, tName);
  if(whcl__ttype_is_biv(tName->ttype)){
    //whcl__biv_search_t(args->ct, tName)){
    rc = whcl__throw(el, args->ct, tName, CWAL_RC_ALREADY_EXISTS,
                     "Variable name collides with a "
                     "builtin value: %.*s",
                     (int)tName->length,
                     whcl_stoken_cstr(args->ct, tName, NULL, false));
    goto end;
  }else if(TOK1_T_Identifier!=tName->ttype){
    rc = whcl__throw(el, args->ct, tName, CWAL_RC_MISUSE,
                     "Invalid variable name: %.*s",
                     (int)tName->length,
                     whcl_stoken_cstr(args->ct, tName, NULL, false));
    goto end;
  }
  if(ndx==args->argc) xv = cwal_value_undefined();
  /* If we have a BIC, call it. This is primarily a convenience
     to enable: decl x proc {} ...

     Instead of: decl x [proc ...]

     But also for the object/array/new builtins.
  */
  else if(!whcl__check_bic_call(args, ndx, &xv, &rc) && 0==rc){
    if(ndx < args->argc-1){
      //whcl__dump_stok(args->ct, args->argv[ndx], "extra token?");
      rc = whcl_err_throw(el, CWAL_SCR_SYNTAX,
                          "Too many arguments to '%s'.", bic->name);
    }else{
      rc = whcl__eval_token(el, args->ct, args->argv[ndx], 0, &xv);
    }
  }
  if(rc) goto end;
  else if(!xv){
    /* Can happen on, e.g.: decl x obj[NonExistentProperty] */
    xv = cwal_value_undefined();
  }
  //whcl__dump_stok(args->ct, tName, "decl name");
  //whcl__dump_stok(args->ct, args->argv[ndx], "decl val");
  rc = whcl_var_decl(el, NULL, doConst,
                     whcl_stoken_cstr(args->ct, tName, NULL, false),
                     tName->length, xv);
  end:
  if(0==rc && rv){
    whcl__holder_truncate(el, holdLen, (*rv = xv));
  }else{
    if(xv) cwal_refunref(xv);
    whcl__holder_truncate(el, holdLen, NULL);
  }
  return rc;
}

/** Handler for the 'do' loop command. */
static int whcl__bic_f_dowhile(whcl__bic const * const bic,
                               whcl__args const * const args,
                               cwal_value **rv){
  int rc = 0;
  bool buul = true;
  cwal_value * vBool = NULL;
  cwal_value * xrv = NULL;
  whcl_stoken const * tBody = args->argv[1];
  whcl_stoken const * tWhile = whcl_stoken_sibling(args->ct, tBody);
  whcl_stoken const * tCond = whcl_stoken_sibling(args->ct, tWhile);
  whcl_scope * scel = NULL;
  uint32_t evalFlags = 0;
  if(!tCond ||
     !tBody || TOK1_T_SquigglyGroup != tBody->ttype ||
     !tWhile || TOK1_T_BIC_while!=tWhile->ttype2){
    err_syntax:
    rc = whcl_err_throw(args->el, CWAL_SCR_SYNTAX,
                        "Expecting {CODE} while {EXPR} after '%s'.",
                        bic->name);
    goto end;
  }
  switch(tCond->ttype){
    case TOK1_T_ParenGroup:
    case TOK1_T_SquigglyGroup: evalFlags = WHCL__EVAL_F_SUB_EXPR;
      break;
    case TOK1_T_BraceGroup: evalFlags = WHCL__EVAL_F_SUB_CALLBLOCK;
      break;
    default: goto err_syntax;
  }
  while(buul){
    if(scel){
      whcl_scope_pop(args->el, scel, NULL);
      scel = NULL;
    }
    if(!(scel = whcl__scope_push(args->el, WHCL__SCOPE_F_LOOP))){
      rc = CWAL_RC_OOM; break;
    }
    whcl__stoken_set(args->ct, tBody);
    rc = whcl__eval_sub(args->el, false, args->ct, 0, NULL);
    switch(rc){
      case CWAL_RC_CONTINUE: rc = 0;
        CWAL_SWITCH_FALL_THROUGH;
      case 0: break;
      case CWAL_RC_BREAK:
        xrv = cwal_propagating_take(args->el->ec);
        assert(xrv && "Else violation of CWAL_RC_BREAK semantics.");
        rc = 0;
        goto end;
      default:
        goto end;
    }
    assert(0==rc);
    if(rc) break;
    whcl__stoken_set(args->ct, tCond);
    rc = whcl__eval_sub(args->el, false, args->ct, evalFlags, &vBool);
    if(rc) break;
    buul = cwal_value_get_bool(vBool);
    cwal_refunref(vBool);
    vBool = NULL;
  }
  end:
  if(scel) whcl_scope_pop(args->el, scel, rc ? NULL : xrv);
  if(0==rc && rv){
    *rv = xrv ? xrv : cwal_value_undefined();
  }
  return rc;
}

/** Helper for expanding @expandos in the `echo` builtin. */
struct WhclEchoState {
  whcl_engine * el;
  bool addSpaces;
  uint16_t ndx;
};
typedef struct WhclEchoState WhclEchoState;
static int cwal_value_visitor_f_echo( cwal_value * v, void * state ){
  WhclEchoState * const wes = (WhclEchoState*)state;
  int rc = 0;
  if(wes->addSpaces && wes->ndx++>0){
    if( (rc = cwal_output(wes->el->ec, " ", 1)) ) return rc;
  }
  return whcl_value_output(wes->el, v);
}

/** Handler for the echo command */
static int whcl__bic_f_echo(whcl__bic const * const bic,
                            whcl__args const * const args,
                            cwal_value **rv){
  int rc = 0;
  bool addNL = true;
  uint16_t ndx = 1;
  whcl_stoken const * tArg;
  WhclEchoState wes;
  wes.el = args->el;
  wes.addSpaces = true;
  while( (tArg = whcl__args_hasflag(args, ndx)) ){
    ++ndx;
    if(whcl_stoken_matches(args->ct, tArg, "-n", 2)){
      addNL = false;
    }else if(whcl_stoken_matches(args->ct, tArg, "-s", 2)){
      wes.addSpaces = false;
    }else{
      whcl__script_errtoken_set(args->ct, tArg);
      return whcl_err_throw(args->el, CWAL_RC_MISUSE,
                            "Invalid flag for %s: %.*s",
                            bic->name, (int)tArg->length,
                            whcl_stoken_cstr(args->ct, tArg, NULL, false));
    }
  }
  for(uint16_t i = ndx; 0==rc && i < args->argc; ++i){
    cwal_value * xv = NULL;
    //whcl__dump_stok(args->ct, args->argv[i],"echo ARG");
    whcl_stoken const * const tok = args->argv[i];
    rc = whcl__eval_token(args->el, args->ct, tok,
                          WHCL__EVAL_F_EXPANDO_OKAY, &xv);
    if(0==rc){
      if(wes.addSpaces && i>ndx){
        rc = cwal_output(args->el->ec, " ", 1);
      }
      if(rc) cwal_refunref(xv);
      else if(xv){
        cwal_ref(xv);
        if(TOK1_T_AtExpand==tok->ttype){
          wes.ndx = 0;
          rc = whcl__expand_value_to_visitor(xv,
                                             cwal_value_visitor_f_echo,
                                             &wes);
        }else{
          rc = whcl_value_output(args->el, xv);
        }
        cwal_unref(xv);
      }else{
        xv = cwal_value_undefined();
        //MARKER(("WARNING: token evaluated to NULL.\n"));
        //whcl__dump_stok(args->ct, args->argv[i], "eval'd to NULL");
      }
    }
  }
  if(0==rc && addNL){
    rc = cwal_output(args->el->ec, "\n", 1);
  }
  if(0==rc && rv) *rv = cwal_value_undefined();
  return rc;
}


/**
   Returns a synthetic script name based on the given script and token
   position, intended for use as a second-pass `eval` script
   name. Returns NULL on OOM.
*/
static char * whcl__eval_name(whcl_engine * const el,
                              whcl_script const * const ct,
                              whcl_stoken const * const tok){
  char const * sName = whcl_script_name_get(ct, NULL);
  return cwal_printf_cstr(el->ec, "[eval@%s:%d:%d]",
                          sName ? sName : "(unnamed)",
                          (int)tok->line, (int)tok->column);
}

/** Handler for the `eval` and `__debug` commands. */
static int whcl__bic_f_eval(whcl__bic const * const bic,
                             whcl__args const * const args,
                             cwal_value **rv){
  int rc = 0;
  uint16_t ndx = 1;
  bool const isDebug = bic->ttype == TOK1_T_BIC___debug;
  bool doScope = isDebug;
  bool singlePass = true;
  char passPhase = 1 /* 1=1st pass, 2=2nd */;
  cwal_value * xrv = NULL;
  whcl_stoken const * tArg = NULL;
  cwal_size_t const holderLen = whcl__holder_len(args->el);
  whcl_scope * scel = NULL;
  static const uint32_t evalFlags = WHCL__EVAL_F_SCOPE_INHERIT_FLAGS;
  while( TOK1_T_BIC___debug!=bic->ttype &&
         (tArg = whcl__args_hasflag(args, ndx)) ){
    ++ndx;
    if(whcl_stoken_matches(args->ct, tArg, "-scope", 6)){
      doScope = true;
      continue;
    }else{
      whcl__script_errtoken_set(args->ct, tArg);
      rc = whcl_err_throw(args->el, CWAL_RC_MISUSE,
                          "Invalid flag for %s: %.*s",
                          bic->name, (int)tArg->length,
                          whcl_stoken_cstr(args->ct, tArg, NULL, false));
      goto end;
    }
  }
  tArg = args->argv[ndx++];
  if(!tArg) goto invalid_token;
  if(!isDebug && TOK1_T_OpArrow == tArg->ttype){
    singlePass = false;
    tArg = args->argv[ndx++];
  }
  if(isDebug){
    if(!tArg || TOK1_T_SquigglyGroup!=tArg->ttype){
      rc = whcl__script_throw(args->ct, CWAL_SCR_SYNTAX,
                          "Expecting {...} after %s.", bic->name);
      goto end;
    }else if(!args->el->flags.enableDebugBlock){
      goto end;
    }
  }
  if(!rv) goto end;
  switch(tArg ? tArg->ttype : 0){
    case TOK1_T_SquigglyGroup:
      rc = whcl__eval_sub2(args->el, doScope, args->ct,
                           tArg, evalFlags, &xrv);
      if(0==rc && !singlePass){
        passPhase = 2;
        goto pass_phase_bit;
      }
      break;
    case 0:
      invalid_token:
      whcl__script_errtoken_set(args->ct, tArg ? tArg : args->argv[0]);
      rc = whcl__script_throw(args->ct, CWAL_SCR_SYNTAX,
                          "Invalid token type (%s). Expecting %s %s",
                          tArg ? tok1_t_cstr(tArg->ttype) : "NULL",
                          bic->name, bic->usage);
      break;
    default: pass_phase_bit: {
      whcl_script sub = whcl__script_empty;
      whcl_script * scrEval =
        NULL /*script to eval: &sub or a whcl_script argument */;
      char * subName = NULL;
      char const * subNameOrig = NULL;
      if(doScope
         && !(scel = whcl__scope_push2(args->el))){
        rc = CWAL_RC_OOM; break;
      }
      do{
        cwal_value * vEval = NULL;
        if(1==passPhase){
          rc = whcl__eval_token(args->el, args->ct, tArg, evalFlags, &vEval);
        }else{
          assert(2==passPhase);
          //whcl__dump_val(xrv, "pass 2 from {...}");
          vEval = xrv;
          xrv = NULL;
        }
        if(0==rc && vEval) rc = whcl__holder_push(args->el, vEval);
        if(rc) break;
        if( singlePass || !vEval || !cwal_value_get_bool(vEval)){
          xrv = vEval ? vEval : cwal_value_undefined();
          break;
        }
        if((scrEval = whcl__value_get_script(args->el, vEval))){
          subName = whcl__eval_name(args->el, args->ct, tArg);
          whcl__script_rewind(scrEval);
          if(subName){
            subNameOrig = scrEval->name;
            scrEval->name = subName;
          }
        }else if(!cwal_value_is_string(vEval)){
          xrv = vEval ? vEval : cwal_value_undefined();
          break;
        }else{
          scrEval = &sub;
          cwal_size_t srcLen = 0;
          char const * src = NULL;
          src = cwal_value_get_cstr(vEval, &srcLen);
          if(!srcLen){
            xrv = cwal_value_undefined();
            break;
          }
          whcl__script_init(args->el->ec, &sub);
          subName = whcl__eval_name(args->el, args->ct, tArg);
          sub.name = subName ? subName : "eval";
          rc = whcl__compile(args->el, &sub, src, (cwal_int_t)srcLen, 0);
#if 0
          /* Reminder to self: renumbering the script for the
             second-pass case seems to cause more confusion than
             not doing so. */
          if(0==rc){
            //whcl__dump_stok(args->ct, tArg, "renumbering");
            whcl_script_renumber(&sub, tArg->line, tArg->column);
          }
#endif
        }
        if(0==rc){
          //MARKER(("Evaling whcl_script:\n%s\n", scrEval->begin));
          rc = whcl__eval_script(args->el, false, scrEval, evalFlags, &xrv);
          //MARKER(("rc=%s\n", cwal_rc_cstr(rc)));
          if(0==rc && xrv) rc = whcl__holder_push(args->el, xrv);
        }
      }while(false);
      if(subNameOrig) scrEval->name = subNameOrig;
      whcl__script_finalize(&sub);
      cwal_free(args->el->ec, subName);
      break;
    }      
  }
  end:
  if(isDebug) xrv = NULL /* the eval holder will kill it */;
  cwal_value * const propagate = (rc || !rv) ? NULL : xrv;
  if(scel) whcl_scope_pop(args->el, scel, propagate);
  if(0==rc && rv){
    whcl__holder_truncate(args->el, holderLen, propagate);
    *rv = propagate ? propagate : cwal_value_undefined();
  }else{
    whcl__holder_truncate(args->el, holderLen, NULL);
  }
  return rc;
}


/** Handler for the exception command. This shares a lot of
    logic with the throw builtin but combining them leads to
    ugly code. */
static int whcl__bic_f_exception(whcl__bic const * const bic,
                                 whcl__args const * const args,
                                 cwal_value **rv){
  int rc = 0;
  cwal_value * xrv = NULL;
  cwal_value * rcCode = NULL;
  int rcEx = 0;
  cwal_size_t const holderLen = whcl__holder_len(args->el);
  if(2==args->argc){
    if(!whcl__check_bic_call(args, 1, &xrv, &rc) && 0==rc){
      rc = whcl__eval_token(args->el, args->ct, args->argv[1], 0, &xrv);
    }
  }else{
    assert(args->argc>1);
    rc = whcl__eval_token(args->el, args->ct, args->argv[1], 0, &rcCode);
    if(rc || (rc = whcl__holder_push(args->el, rcCode))){
      goto end;
    }
    if(!whcl__check_bic_call(args, 2, &xrv, &rc) && 0==rc){
      if(args->argc>3){
        whcl__script_errtoken_set(args->ct, args->argv[2]);
        rc = whcl__script_throw(args->ct, CWAL_RC_MISUSE,
                            "Too many arguments for %s.", bic->name);
      }else{
        rc = whcl__eval_token(args->el, args->ct, args->argv[2],
                              0, &xrv);
      }
    }
  }
  if(rc || (rc = whcl__holder_push(args->el, xrv))){
    goto end;
  }
  assert(xrv);
  if(rcCode){
    cwal_size_t nStr = 0;
    char const * str = cwal_value_get_cstr(rcCode, &nStr);
    if(str) whcl_cstr_to_rc(str, (cwal_int_t)nStr, &rcEx);
    else rcEx = cwal_value_get_integer(rcCode);
  }
  assert(xrv);
  xrv = cwal_new_exception_value(args->el->ec,
                                 rcEx ? rcEx : CWAL_RC_EXCEPTION,
                                 xrv ? xrv : cwal_value_undefined());
  if(!xrv){
    WHCL__WARN_OOM;
    rc = CWAL_RC_OOM;
  }else{
    whcl__annotate_exception(args->el, xrv);
    *rv = xrv;
  }
  end:
  whcl__holder_truncate(args->el, holderLen, rc ? NULL : xrv);
  return rc;
}

/** Handler for the expr command. */
static int whcl__bic_f_expr(whcl__bic const * const bic,
                            whcl__args const * const args,
                            cwal_value **rv){
  if(bic){/*unused*/}
  assert(args->argc>1);
  whcl__stoken_set(args->ct, args->argv[0]);
  return whcl__eval_expr(args->el, 0, rv);
}


/** Handler for the 'if' command. */
static int whcl__bic_f_if(whcl__bic const * const bic,
                          whcl__args const * const args,
                          cwal_value **rv){
  int rc = 0;
  whcl_stoken const * tArg;
  bool buul = false;
  cwal_value * vBool = NULL;
  uint16_t ndx = 1;
  whcl_scope * scel = NULL;
  uint32_t evalFlags = 0;
  assert(args->argc>1);
  if(!(scel = whcl__scope_push2(args->el))) return CWAL_RC_OOM;
  next_if:
  tArg = args->argv[ndx++];
  switch(tArg ? tArg->ttype : 0){
    case TOK1_T_ParenGroup:
    case TOK1_T_SquigglyGroup: evalFlags = WHCL__EVAL_F_SUB_EXPR;
      break;
    case TOK1_T_BraceGroup: evalFlags = WHCL__EVAL_F_SUB_CALLBLOCK;
      break;
    default:
      rc = whcl_err_throw(args->el, CWAL_SCR_SYNTAX,
                          "Expecting {...} after '%s'.",
                          bic->name);
      goto end;
  }
  // if {CONDITION} {body}
  whcl__stoken_set(args->ct, tArg);
  rc = whcl__eval_sub(args->el, true, args->ct, evalFlags, &vBool);
  if(rc) goto end;
  buul = cwal_value_get_bool(vBool);
  cwal_refunref(vBool);
  vBool = NULL;
  tArg = args->argv[ndx++];
  if(!tArg){
    if(ndx>3) goto end /* final else clause */;
    else if(TOK1_T_SquigglyGroup != tArg->ttype){
      whcl__script_errtoken_set(args->ct, tArg);
      rc = whcl_err_throw(args->el, CWAL_SCR_SYNTAX,
                          "Expecting {...} after '%s {...}'.",
                          bic->name);
      goto end;
    }
  }
  if(buul){
    final_body: ;
    // (if {condition} {BODY}) or (else {BODY})
    whcl__stoken_set(args->ct, tArg);
    rc = whcl__eval_sub(args->el, true, args->ct,
                        WHCL__EVAL_F_SCOPE_INHERIT_FLAGS,
                        NULL);
    goto end;
  }else{ // Check for an else clause...
    tArg = args->argv[ndx++];
    if(!tArg) goto end;
    else if(TOK1_T_Identifier!=tArg->ttype
            || 0 != whcl_stoken_strcmp(args->ct, tArg, "else", 4, false)){
      whcl__script_errtoken_set(args->ct, tArg);
      rc = whcl_err_set(args->el, CWAL_SCR_SYNTAX,
                        "Expecting 'else' after '%s'.", bic->name);
      goto end;
    }
    // Check what follows the else...
    tArg = args->argv[ndx++];
    switch(tArg ? tArg->ttype : 0){
      case TOK1_T_SquigglyGroup: goto final_body;
      case TOK1_T_BIC:
        if(TOK1_T_BIC_if==tArg->ttype2){
          goto next_if;
        }
        CWAL_SWITCH_FALL_THROUGH;
      default:
        whcl__script_errtoken_set(args->ct, tArg);
        rc = whcl_err_set(args->el, CWAL_SCR_SYNTAX,
                          "Expecting '%s' or {...} after 'else'.",
                          bic->name);
        goto end;
    }
  }
  end:
  whcl_scope_pop(args->el, scel, NULL);
  if(0==rc && rv) *rv = cwal_new_bool(buul);
  return rc;
}

/** Handler for the 'for' command. */
static int whcl__bic_f_for(whcl__bic const * const bic,
                             whcl__args const * const args,
                             cwal_value **rv){
  int rc = 0;
  bool buul;
  cwal_value * vBool = NULL;
  cwal_value * xrv = NULL;
  whcl_scope * scel1 = NULL;
  whcl_scope * scel2 = NULL;
  whcl_stoken const * tArg = NULL;
  whcl_stoken const * tPre = args->argv[1];
  whcl_stoken const * tCond = whcl_stoken_sibling(args->ct, tPre);
  whcl_stoken const * tPost = whcl_stoken_sibling(args->ct, tCond);
  whcl_stoken const * tBody = whcl_stoken_sibling(args->ct, tPost);
  //cwal_size_t const holderLen = whcl__holder_len(args->el);
  assert(5==args->argc);
  for(uint16_t i = 1; i <5; ++i){
    tArg = args->argv[i];
    if(!tArg || TOK1_T_SquigglyGroup != tArg->ttype){
      whcl__script_errtoken_set(args->ct, tArg ? tArg : args->argv[i-1]);
      return whcl__script_throw(args->ct, CWAL_SCR_SYNTAX,
                            "Expecting: %s %s", bic->name,
                            bic->usage);
    }
  }
  if(args->argv[5]){
    whcl__script_errtoken_set(args->ct, args->argv[5]);
    return whcl__script_throw(args->ct, CWAL_SCR_SYNTAX,
                          "Extra tokens after for loop construct.");
  }
  if(!(scel1 = whcl__scope_push( args->el, WHCL__SCOPE_F_LOOP ))){
    rc = CWAL_RC_OOM; goto end;
  }
  // for {PRE} {condition} {post} {body}
  rc = whcl__eval_sub2(args->el, false, args->ct, tPre, 0, NULL);
  if(rc){
    goto end;
  }
  while(1){
    // Reset the inner scope for each iteration.
    if(scel2){
      assert(scel2 == args->el->scopes.current);
      whcl_scope_pop(args->el, scel2, NULL);
      //whcl__holder_truncate(args->el, holderLen, NULL);
    }
    if(!(scel2 = whcl__scope_push2(args->el))){
      rc = CWAL_RC_OOM;
      break;
    }
    // for {pre} {CONDITION} {post} {body}
    rc = whcl__eval_sub2(args->el, false, args->ct, tCond,
                         WHCL__EVAL_F_SUB_EXPR, &vBool);
    if(rc) goto loop_end;
    buul = cwal_value_get_bool(vBool);
    cwal_refunref(vBool);
    vBool = NULL;
    if(!buul) goto loop_end;
    // for {pre} {condition} {post} {BODY}
    rc = whcl__eval_sub2(args->el, false, args->ct, tBody, 0, NULL);
    if(rc) goto loop_end;
    // for {pre} {condition} {POST} {body}
    rc = whcl__eval_sub2(args->el, false, args->ct, tPost, 0, NULL);
    if(0==rc) continue;
    loop_end:
    whcl_scope_pop(args->el, scel2, NULL);
    switch(rc){
      case CWAL_RC_CONTINUE: rc = 0; continue;
      case CWAL_RC_BREAK:
        xrv = cwal_propagating_take(args->el->ec);
        assert(xrv && "Else violation of CWAL_RC_BREAK semantics.");
        rc = 0;
        break;
      default:
        break;
    }
    break;
  }
  end:
  if(rc || !rv){
    cwal_refunref(xrv);
    xrv = NULL;
  }
  //whcl__holder_truncate(args->el, holderLen, xrv);
  if(scel1) whcl_scope_pop(args->el, scel1, xrv);
  if(0==rc && rv) *rv = xrv ? xrv : cwal_value_undefined();
  return rc;
}

bool whcl_is_newing(cwal_value const * const v){
  return v && 0!=(cwal_container_client_flags_get(v)
                  & WHCL__VAL_F_IS_NEWING);
}

/**
   Helper for the `new` builtin: requires v to be the ostensible
   constructor operand to `new`. If operand is-a function then *func
   is set to that function and `*proto` is set to its prototype, else
   it falls back to see if operand has a method named `__new`. If so,
   `*func` is set to that method and `*proto` is set to operand. If neither
   is the case, `*func` and `*proto` are set to NULL.
*/
static void whcl__new_find_ctor(whcl_engine * const el, cwal_value * const operand,
                                cwal_function ** func, cwal_value ** proto){

  *func = NULL;
  *proto = NULL;
  cwal_function * f = cwal_value_get_function(operand);
  if(f){
    *func = f;
    *proto = cwal_value_prototype_get(el->ec, operand);
  }else{
    cwal_value * xrv = NULL;
    whcl_lookup_vsym_v(el, operand, el->cache.keyNewCtor, false, &xrv);
    if(xrv && cwal_value_is_function(xrv)){
      *func = cwal_value_get_function(xrv);
      *proto = operand;
    }
  }
}

/** BIC handler for the "new" command. */
static int whcl__bic_f_new(whcl__bic const * const bic,
                           whcl__args const * const args,
                           cwal_value **rv){
  if(rv) *rv = cwal_value_undefined();
  assert(args->argc>1);
  whcl_stoken const * tArg;
  whcl_stoken const * tBody = NULL;
  cwal_value * vCtor = NULL;
  cwal_value * vThis = NULL;
  cwal_value * vProto = NULL;
  cwal_value * vRc = NULL;
  cwal_size_t const holderLen = whcl__holder_len(args->el);
  int rc;
  uint16_t argNdx = 1;
  bool doPost = false;
  while( (tArg = whcl__args_hasflag(args, argNdx)) ){
    argNdx++;
#if 0
    if(8==tArg->length
       && whcl_stoken_matches(args->ct, tArg, "-no-post", tArg->length)){
      doPost = false;
    }else
#endif
    if(5==tArg->length
       && whcl_stoken_matches(args->ct, tArg, "-post", tArg->length)){
      doPost = true;
    }else{
      whcl__script_errtoken_set(args->ct, tArg);
      return whcl_err_set(args->el, CWAL_SCR_SYNTAX,
                          "Invalid flag for %s. Expecting: %s",
                          bic->name, bic->usage);
    }
  }
  tArg = args->argv[argNdx++];
  switch(tArg->subscriptId ? 0 : tArg->ttype){
    //   ^^^^^^^^ to differentiate IDENT from IDENT[...]
    case TOK1_T_Identifier:
      rc = whcl__varident_lookup(args->el, args->ct, tArg, &vCtor);
      break;
    default:
      rc = whcl__eval_token(args->el, args->ct, tArg, 0, &vCtor);
      break;
  }
  if(rc || (rc = whcl__holder_push(args->el, vCtor))) goto end;
  cwal_function * fCtor = NULL;
  whcl__new_find_ctor(args->el, vCtor, &fCtor, &vProto);
  if(!fCtor){
    whcl__script_errtoken_set(args->ct, tArg);
    rc = whcl__script_throw(args->ct, CWAL_RC_TYPE,
                        "'%s' expects a function or an object which "
                        "inherits a __new method.", bic->name);
    goto end;
  }
  assert(vProto);
  tBody = doPost ? args->argv[args->argc-1] : NULL;
  if(tBody && TOK1_T_SquigglyGroup!=tBody->ttype){
    tBody = NULL;
  }
  vThis = cwal_new_object_value(args->el->ec);
  if(!vThis){ WHCL__WARN_OOM; rc = CWAL_RC_OOM; goto end; }
  else if((rc = whcl__holder_push(args->el, vThis))) goto end;
  rc = cwal_value_prototype_set(vThis, vProto)
    /* Start out with this prototype, but the ctor may swap it out
       (needed for unusual cases). */;
  if(rc){
    whcl__script_errtoken_set(args->ct, tArg);
    rc = whcl__script_err(args->ct, rc, "Error %s assigning prototype.",
                      cwal_rc_cstr(rc));
    goto end;
  }
  cwal_flags16_t const ccFlags = cwal_container_client_flags_get(vThis);
  cwal_container_client_flags_set(vThis, ccFlags | WHCL__VAL_F_IS_NEWING);
  uint16_t const argOffset = argNdx;
  assert(tBody ? argOffset < args->argc : argOffset <= args->argc);
  rc = whcl__call_cwal_f(args->el, args->ct, fCtor, vThis,
                         args->argv + argOffset,
                         args->argc - argOffset - (tBody ? 1 : 0),
                         &vRc);
  cwal_container_client_flags_set(vThis, ccFlags);
  if(rc) goto end;
  else if(vRc && cwal_value_undefined()!=vRc){
    /* If the ctor returns anything other than `undefined`,
       that becomes our "this". That mechanism allows ctors
       to return types we cannot account for here, namely
       natives. */
    vThis = vRc;
    if((rc = whcl__holder_push(args->el, vRc))) goto end;
  }
  if(tBody && whcl_stoken_has_inner_content(args->ct, tBody)){
    whcl_scope * const scel = whcl__scope_push2(args->el);
    if(!scel){ rc = CWAL_RC_OOM; goto end; }
    rc = whcl_set_this(args->el, vThis);
    if(0==rc){
      whcl__stoken_set(args->ct, tBody);
      rc = whcl__eval_sub(args->el, false, args->ct, 0, NULL);
    }
    assert(scel == args->el->scopes.current);
    whcl_scope_pop(args->el, scel, NULL);
    switch(rc){
      case 0: break;
      case CWAL_RC_RETURN:
        /* We catch `return` only for convience. If we don't
           do this, a `return` could well exit the current script,
           which would feel quite wrong for this particular syntatic
           construct. However, we disregard the result because
           vThis is the result of `new`. */
        cwal_propagating_set(args->el->ec, NULL);
        rc = 0;
        break;
    }
  }    
  end:
  if(rc || !rv) whcl__holder_truncate(args->el, holderLen, NULL);
  else{
    assert(vThis);
    whcl__holder_truncate(args->el, holderLen, vThis);
    *rv = vThis;
  }
  return rc;
}


/** Handler for the object command. */
static int whcl__bic_f_object(whcl__bic const * const bic,
                              whcl__args const * const args,
                              cwal_value **rv){
  int rc = 0;
  whcl_stoken const *tArg;
  uint16_t ndx = 1;
  uint32_t rdobjFlags = 0;
  while( (tArg = whcl__args_hasflag(args, ndx)) ){
    ++ndx;
    if(whcl_stoken_matches(args->ct, tArg, "-scope", 6)){
      rdobjFlags |= WHCL__READ_OBJ_F_SCOPE;
    }else if(whcl_stoken_matches(args->ct, tArg, "-this", 5)){
      rdobjFlags |= WHCL__READ_OBJ_F_THIS;
    }else{
      return whcl_err_throw(args->el, CWAL_RC_MISUSE,
                            "Invalid flag for %s: %.*s",
                            bic->name, (int)tArg->length,
                            whcl_stoken_cstr(args->ct, tArg, NULL, false));
    }
  }
  tArg = args->argv[ndx-1];
  rc = whcl__read_new_object(args->el, args->ct, tArg, rdobjFlags, rv);
  return rc;
}

/** Handler for the alias command. */
static int whcl__bic_f_alias(whcl__bic const * const bic,
                             whcl__args const * const args,
                             cwal_value **rv){
  int rc = 0;
  uint16_t ndx = 1;
  whcl_stoken const * tArg = NULL;
  whcl_stoken const * t1 = NULL;
  whcl_stoken const * tTarget = NULL;
  whcl_stoken const * tProp = NULL;
  whcl_stoken const * tFunc = NULL;
  cwal_value * vTarget = NULL;
  cwal_value * vProp = NULL;
  cwal_value * vFunc = NULL;
  cwal_value * vRef = NULL;
  cwal_size_t const holderLen = whcl__holder_len(args->el);
  cwal_propref_e refType = CWAL_PROPREF_AUTO;
  uint32_t const evalFlags = 0
    /*WHCL__EVAL_F_SCOPE_INHERIT_FLAGS*/;
  cwal_flags16_t refFlags = 0;
  while( (tArg = whcl__args_hasflag(args, ndx)) ){
    ++ndx;
    if(whcl_stoken_matches(args->ct, tArg, "-ro", 3)){
      refFlags |= CWAL_PROPREF_F_READONLY;
      continue;
    }else if(whcl_stoken_matches(args->ct, tArg, "-weak", 5)){
      refFlags |= CWAL_PROPREF_F_WEAK;
      continue;
    }else{
      whcl__script_errtoken_set(args->ct, tArg);
      rc = whcl_err_throw(args->el, CWAL_RC_MISUSE,
                          "Invalid flag for %s: %.*s",
                          bic->name, (int)tArg->length,
                          whcl_stoken_cstr(args->ct, tArg, NULL, false));
      goto end;
    }
  }
  tArg = args->argv[ndx];
  assert(ndx <= args->argc);
  uint16_t const argc = args->argc - ndx;
  assert(argc>0);
  t1 = tArg;
  if(argc) tFunc = args->argv[++ndx];
  if(whcl__token_has_prop(args->ct, t1)){
    /* container.property */
    tTarget = t1;
    assert(!tProp);
    cwal_value * xrv = NULL;
    rc = whcl__eval_token(args->el, args->ct, tTarget, evalFlags, &xrv);
    if(rc) goto end;// || (rc = whcl__holder_push(args->el, xrv))) goto end;
    whcl__dotop_get(args->el, &vTarget, &vProp, NULL);
    assert(vTarget)
      /*keeping in mind that with X[Y][Z], X[Y] is now vTarget.*/;
    if(!cwal_prop_key_can(vProp)){
      cwal_refunref(xrv);
      goto bad_key;
    }
#if 0
    else if(!xrv){
      /* Corner case: if the property does not exist, trying to deref
         it later will come up as "property not found" because the
         propref indirection happens far below a level where we can see it.
         For this case, we'll set the property to `undefined`.

         decl o object
         decl ref [alias o.x]
         set ref 2 ;# will fail with "cannot resolve var"
      */
      assert(vProp);
      rc = whcl_set_v(args->el, vTarget, vProp, cwal_value_undefined());
      if(rc) goto end;
    }
#endif
    bool const wasUndef = xrv && (cwal_value_undefined()==xrv);
    if(xrv) cwal_refunref(xrv);
    xrv = NULL;
    assert(vProp);
    if(rc || (rc = whcl__holder_push(args->el, vProp))) goto end;
    if(wasUndef && cwal_value_is_array(vTarget)
       && cwal_value_is_integer(vProp)){
      cwal_int_t const i = cwal_value_get_integer(vProp);
      if(i<0 || (i>=0xefff)){
        rc = whcl_err_throw(args->el, CWAL_RC_RANGE,
                            "Index to an array entry alias "
                            "must have a realistic value.");
        goto end;
      }
#if 0
      /* If we reference a non-existent array entry, we have the same
         problem as described above for missing object properties, so
         we'll fill those with undefined if they're not set. .*/
      cwal_array * const ar = cwal_value_get_array(vTarget);
      if(!cwal_array_get(ar, (cwal_midsize_t)i)){
        rc = cwal_array_set(ar, (cwal_midsize_t)i, cwal_value_undefined());
        if(rc){WHCL__WARN_OOM; goto end;}
      }
#endif
      refType = CWAL_PROPREF_AUTO;
    }
#if 0
    if(1){
      whcl__dump_stok(args->ct, tTarget, "tTarget");
      whcl__dump_val(vProp,"vProp");
      whcl__dump_val(xrv,"xrv");
    }
#endif
  }else{ /* varname */
    whcl_scope * foundIn = NULL;
    cwal_value * xrv = NULL;
    tProp = t1;
    if(TOK1_T_Identifier != tProp->ttype){
      //whcl__dump_stok(args->ct, tProp, "tProp");
      goto bad_args;
    }
    rc = whcl__eval_token(args->el, args->ct, tProp, evalFlags, &xrv);
    if(rc || (rc = whcl__holder_push(args->el, xrv))) goto end;
    else if(!cwal_value_is_string(xrv)){
      goto bad_key;
    }
    vProp = xrv; xrv = NULL;
    if(!whcl_var_search_v(args->el, NULL, true, vProp, &foundIn)){
      rc = whcl_err_throw(args->el, CWAL_RC_NOT_FOUND,
                          "Cannot resolve variable %.*s.",
                          (int)tProp->length,
                          whcl_stoken_cstr(args->ct, tProp, NULL, false));
      goto end;
    }
    vTarget = whcl_scope_props(args->el, foundIn);
    assert(vTarget && "We just found a property in this.");
    if((rc = whcl__holder_push(args->el, vTarget))) goto end;
  }
  assert(!vRef);
  assert(vTarget);
  assert(vProp);
  assert(0==rc);
  if(tFunc){ /* The optional function argument... */
    if(!whcl__check_bic_call(args, ndx, &vFunc, &rc)){
      rc = whcl__eval_token(args->el, args->ct, tFunc, evalFlags, &vFunc);
    }
    if(rc ||
       (vFunc && (rc = whcl__holder_push(args->el, vFunc)))) goto end;
    else if(!vFunc || !cwal_value_is_function(vFunc)){
      rc = whcl__throw(args->el, args->ct, tFunc, CWAL_RC_TYPE,
                              "Expecting a Function-type value for %s.",
                              bic->name);
      goto end;
    }
    refType = CWAL_PROPREF_INTERCEPTOR;
  }
  //whcl__dump_val(vTarget, "vTarget"); whcl__dump_val(vProp, "vProp");
  cwal_propref * p = NULL;
  rc = cwal_new_propref2(refType, refFlags, vTarget, vProp, vFunc, &p);
  if(rc) goto end;
  vRef = cwal_propref_value(p);
  if((rc = whcl__holder_push(args->el, vRef))) goto end;
  assert(vTarget == cwal_propref_container(cwal_value_get_propref(vRef)));
  assert(vProp == cwal_propref_key(cwal_value_get_propref(vRef)));
  end:
  if(0==rc){
    assert(vRef);
    *rv = vRef;
  }
  whcl__holder_truncate(args->el, holderLen, rc ? NULL : vRef);
  return rc;
  bad_args:
  rc = whcl__throw(args->el, args->ct, tArg, CWAL_RC_MISUSE,
                   "Invalid args for %s. Expecting: %s",
                   bic->name, bic->usage);
  goto end;
  bad_key:
  rc = whcl__throw(args->el, args->ct, tProp, CWAL_RC_MISUSE,
                   "This value cannot be used as a property key.",
                   bic->name, bic->usage);
  goto end;
}

/** Handler for the set command. */
static int whcl__bic_f_set(whcl__bic const * const bic,
                           whcl__args const * const args,
                           cwal_value **rv){
  int rc = 0;
  cwal_size_t const holdLen = whcl__holder_len(args->el);
  uint16_t ndx = 1;
  bool doConst = false;
  whcl_stoken const * tArg;
  cwal_value * lhs = NULL;
  cwal_value * key = NULL;
  cwal_value * value = NULL;
  uint16_t foundInLevel = 0
    /* The level of the scope the var was found in. We cannot hold
       the scope itself because following events might cause it
       to get realloced and moved out from under us! This actually
       happened via require.whcl test code. */;
  assert(args->argc>=3);
  while( (tArg = whcl__args_hasflag(args, ndx)) ){
    ++ndx;
    if(whcl_stoken_matches(args->ct, tArg, "-const", 6)){
      doConst = true;
      continue;
    }else{
      rc = whcl_err_set(args->el, CWAL_RC_MISUSE,
                        "Invalid flag for %s: %.*s",
                        bic->name, (int)tArg->length,
                        whcl_stoken_cstr(args->ct, tArg, NULL, false));
      goto end;
    }
  }
  if((rc = whcl__args_ncheck(args, ndx, 2))) goto end;
  tArg = args->argv[ndx++];
  switch(whcl__token_has_prop(args->ct, tArg)
         ? 0 : tArg->ttype){
    case 0:
      rc = whcl__eval_token(args->el, args->ct, tArg, 0, NULL);
      if(rc) goto end;
      whcl__dotop_get(args->el, &lhs, &key, NULL);
      assert(lhs && key);
      if((rc = whcl__holder_push(args->el, lhs))
         || (rc = whcl__holder_push(args->el, key))) goto end;
      break;
    case TOK1_T_Identifier:
    case TOK1_T_QuotedString:{
      if(doConst) goto no_const;
      rc = whcl__eval_token(args->el, args->ct, tArg, 0, &key);
      if(rc || (rc=whcl__holder_push(args->el, key))) goto end;
      whcl_scope * foundIn = NULL;
      value = whcl_var_search_v(args->el, NULL, true, key, &foundIn);
      if((rc = whcl_err_has(args->el, true))){
        goto end;
      }else if(!value){
        rc = whcl_err_set(args->el, CWAL_RC_NOT_FOUND,
                            "Cannot resolve var '%.*s'.",
                            (int)tArg->length,
                            whcl_stoken_cstr(args->ct, tArg, NULL, false));
        goto end;
      }
      assert(foundIn);
      foundInLevel = foundIn->level;
      value = NULL;
      break;
    }
    default:
      whcl__script_errtoken_set(args->ct, tArg);
      return whcl_err_set(args->el, CWAL_SCR_SYNTAX,
                            "Invalid token type (%s) for '%s' command key.",
                            tok1_t_cstr(tArg->ttype), bic->name);
      no_const:
      whcl__script_errtoken_set(args->ct, tArg);
      return whcl_err_set(args->el, CWAL_SCR_SYNTAX,
                            "set -const is only permitted when "
                            "setting object properties.", bic->name);
  }
  tArg = args->argv[ndx];
  if(!whcl__check_bic_call(args, ndx, &value, &rc) && 0==rc){
    if(args->argc > ndx+1){
      //MARKER(("args->argc=%d ndx=%d\n", (int)args->argc, (int)ndx));
      rc = whcl_err_set(args->el, CWAL_SCR_SYNTAX,
                        "Too many arguments to '%s'.", bic->name);
    }else{
      rc = whcl__eval_token(args->el, args->ct, tArg, 0, &value);
    }
  }
  if(rc
     || (rc = whcl__holder_push(args->el, value ? value
                                : (value=cwal_value_undefined())))){
    goto end;
  }
  if(lhs){ // object property
    assert(lhs && key);
    rc = whcl_set_with_flags_v(args->el, lhs, key, value,
                               doConst ? CWAL_VAR_F_CONST : 0);
  }else{ // scope var
    assert(foundInLevel);
    rc = whcl_scope_set_v(args->el,
                          whcl__scope_for_level(args->el, foundInLevel),
                          false, key, value);
  }
  if(0==rc && rv){
    whcl__holder_truncate(args->el, holdLen, value);
    *rv = value;
  }
  end:
  if(lhs) whcl__dotop_set(args->el, NULL, NULL, NULL);
  whcl__holder_truncate(args->el, holdLen, NULL);
  return rc;
}

/** Handler for the exit, throw, and exception commands. */
static int whcl__bic_f_threxit(whcl__bic const * const bic,
                               whcl__args const * const args,
                               cwal_value **rv){
  int rc = 0;
  cwal_value * xrv = NULL;
  switch(bic->ttype){
    case TOK1_T_BIC_throw:{ // throw value
      assert(args->argc>=2);
      if(!whcl__check_bic_call(args, 1, &xrv, &rc) && 0==rc){
        if(args->argc>2){
          whcl__script_errtoken_set(args->ct, args->argv[2]);
          rc = whcl__script_throw(args->ct, CWAL_RC_MISUSE,
                              "Too many arguments for %s.", bic->name);
        }else{
          rc = whcl__eval_token(args->el, args->ct, args->argv[1],
                                0, &xrv);
        }
      }
      if(rc) return rc;
      assert(xrv && "Expecting RHS to have given us a value.");
      if(!cwal_value_is_exception(xrv)){
        cwal_value * exv =
          cwal_new_exception_value(args->el->ec, CWAL_RC_EXCEPTION, xrv);
        if(!exv){
          MARKER(("OOM allocating exception.\n"));
          WHCL__WARN_OOM;
          rc = CWAL_RC_OOM;
        }else{
          whcl_script * const currentScript = args->el->ct;
          args->el->ct = args->ct;
          whcl__script_errtoken_set(args->ct, args->argv[0]);
          whcl__annotate_exception(args->el, exv);
          args->el->ct = currentScript;
          xrv = exv;
        }
      }
      if(0==rc){
        assert(xrv);
        *rv = cwal_value_undefined();
        rc = cwal_exception_set(args->el->ec, xrv);
      }
      return rc;
    }
    case TOK1_T_BIC_exit: // exit [value]
      if(!whcl__check_bic_call(args, 1, &xrv, &rc) && 0==rc){
        if(args->argc>2){
          whcl__script_errtoken_set(args->ct, args->argv[2]);
          return whcl__script_throw(args->ct, CWAL_RC_MISUSE,
                                "Too many arguments to %s.", bic->name);
        }else if(2==args->argc){
          rc = whcl__eval_token(args->el, args->ct, args->argv[1],
                                0, &xrv);
          if(rc) return rc;
        }
      }
      *rv = cwal_value_undefined();
      cwal_propagating_set(args->el->ec,
                           xrv ? xrv : cwal_value_undefined());
      return CWAL_RC_EXIT;
    default:
      whcl__fatal(CWAL_RC_CANNOT_HAPPEN, "Invalid builtin mapping: %s",
                  bic->name);
      return 0/*not reached*/;
  } 
}

/** Handler for the unset command. */
static int whcl__bic_f_unset(whcl__bic const * const bic,
                           whcl__args const * const args,
                           cwal_value **rv){
  int rc = 0;
  cwal_size_t const holdLen = whcl__holder_len(args->el);
  whcl_stoken const * tArg;
  uint16_t ndx = 1;
  assert(args->argc>1);
  while( (tArg = args->argv[ndx++]) ){
    cwal_value * lhs = NULL;
    cwal_value * key = NULL;
    whcl_scope * foundIn = NULL;
    switch(whcl__token_has_prop(args->ct, tArg)
           ? 0 : tArg->ttype){
      case 0:
        rc = whcl__eval_token(args->el, args->ct, tArg, 0, NULL);
        if(rc) goto end;
        whcl__dotop_get(args->el, &lhs, &key, NULL);
        assert(lhs && key);
      break;
      case TOK1_T_Identifier:{
        cwal_value * value = NULL;
        rc = whcl__create_value(args->el, args->ct, tArg, &key);
        if(rc || (rc=whcl__holder_push(args->el, key))) goto end;
        value = whcl_var_search_v(args->el, NULL, false, key, &foundIn);
        if(!value){
          // This might be being over-pedantic.
          rc = whcl_err_throw(args->el, CWAL_RC_NOT_FOUND,
                              "Cannot resolve var '%.*s'.",
                              (int)tArg->length,
                              whcl_stoken_cstr(args->ct, tArg, NULL, false));
          goto end;
        }
        value = NULL;
        break;
      }
      default:
        whcl__script_errtoken_set(args->ct, tArg);
        rc = whcl__script_throw(args->ct, CWAL_RC_TYPE,
                            "Invalid token type (%s) for '%s' command key.",
                            tok1_t_cstr(tArg->ttype),
                            bic->name);
        goto end;
    }
    assert(0==rc);
    rc = foundIn
      ? whcl_scope_set_v(args->el, foundIn, false, key, NULL)
      : whcl_set_v(args->el, lhs, key, NULL);
    if(lhs) whcl__dotop_set(args->el, NULL, NULL, NULL);
  }
  *rv = cwal_value_undefined();
  end:
  whcl__holder_truncate(args->el, holdLen, NULL);
  return rc;
}

/** Handler for the 'while' command. */
static int whcl__bic_f_while(whcl__bic const * const bic,
                             whcl__args const * const args,
                             cwal_value **rv){
  int rc = 0;
  bool buul = false;
  cwal_value * vBool = NULL;
  cwal_value * xrv = NULL;
  whcl_stoken const * tCond = args->argv[1];
  whcl_stoken const * tBody = whcl_stoken_sibling(args->ct, tCond);
  whcl_scope * scel = NULL;
  uint32_t evalFlags = 0;
  if(!tCond || !tBody || TOK1_T_SquigglyGroup != tBody->ttype){
    err_syntax:
    rc = whcl_err_throw(args->el, CWAL_SCR_SYNTAX,
                        "Expecting {EXPR} {CODE} after '%s'.",
                        bic->name);
    goto end;
  }
  switch(tCond->ttype){
    case TOK1_T_ParenGroup:
    case TOK1_T_SquigglyGroup: evalFlags = WHCL__EVAL_F_SUB_EXPR;
      break;
    case TOK1_T_BraceGroup: evalFlags = WHCL__EVAL_F_SUB_CALLBLOCK;
      break;
    default: goto err_syntax;
  }
  next_try:
  if(scel) whcl_scope_pop(args->el, scel, NULL);
  scel = whcl__scope_push(args->el, WHCL__SCOPE_F_LOOP);
  if(!scel){ rc = CWAL_RC_OOM; goto end; }
  whcl__stoken_set(args->ct, tCond);
  rc = whcl__eval_sub(args->el, false, args->ct, evalFlags, &vBool);
  if(rc) goto end;
  buul = cwal_value_get_bool(vBool);
  cwal_refunref(vBool);
  vBool = NULL;
  if(buul){
    whcl__stoken_set(args->ct, tBody);
    rc = whcl__eval_sub(args->el, false, args->ct, 0, NULL);
    switch(rc){
      case 0:
      case CWAL_RC_CONTINUE: rc = 0; goto next_try;
      case CWAL_RC_BREAK:
        xrv = cwal_propagating_take(args->el->ec);
        assert(xrv && "Else violation of CWAL_RC_BREAK semantics.");
        rc = 0;
        goto end;
      default:
        goto end;
    }
  }
  end:
  if(scel) whcl_scope_pop(args->el, scel, rc ? NULL : xrv);
  if(0==rc && rv) *rv = xrv ? xrv : cwal_value_undefined();
  return rc;
}

static int whcl__bic_f_with(whcl__bic const * const bic,
                            whcl__args const * const args,
                            cwal_value **rv){
  int rc = 0;
  uint16_t argNdx = 1;
  whcl_engine * const el = args->el;
  cwal_value * const oldWith = el->with.current;
  cwal_value * wSelf = NULL;
  cwal_size_t const holderLen = whcl__holder_len(args->el);
  cwal_list * const withHolder = &el->with.holder;
  cwal_size_t const withLen = withHolder ? withHolder->count : 0;
  whcl_stoken  const * tVal = NULL;
  whcl_stoken  const * tBody = NULL;
  whcl_scope * scel = NULL
    /* We have to always run in a new scope for the ...X property
       access to work, otherwise multiple `with` blocks can be active
       in a single scope and we can no longer map which with goes to
       which scope while still respecting that `with` blocks cannot
       cross lexical boundaries. */;
  assert(3==args->argc);

  tVal = args->argv[argNdx++];
  tBody = args->argv[argNdx++];
  assert(tBody && tVal);

  if(TOK1_T_SquigglyGroup!=tBody->ttype){
    whcl__script_errtoken_set(args->ct, tBody);
    return whcl_err_set(el, CWAL_SCR_SYNTAX,
                        "Invalid argument to '%s'. Expecting: %s",
                        bic->name, bic->usage);
  }
  scel = whcl__scope_push2(el);
  if(!scel) {WHCL__WARN_OOM; return CWAL_RC_OOM;}
  rc = whcl__eval_token(el, args->ct, tVal, 0, &wSelf);
  if(rc || (rc = whcl__holder_push(el, wSelf))) goto end;
  rc = cwal_list_append(el->ec, withHolder, wSelf);
  if(rc){WHCL__WARN_OOM; goto end;}
  el->with.current = wSelf;
  assert(0==(el->scopes.current->flags & WHCL__SCOPE_F_WITH));
  //bool const wasWith = (el->scopes.current->flags & WHCL__SCOPE_F_WITH);
  //if(!wasWith)
  el->scopes.current->flags |= WHCL__SCOPE_F_WITH;
  rc = whcl__eval_token(el, args->ct, tBody,
                        WHCL__EVAL_F_SQUIGGLY_AS_CODE,
                        NULL);
  //if(!wasWith)
  el->scopes.current->flags &= ~WHCL__SCOPE_F_WITH;
  end:
  if(rc || !rv) wSelf = NULL;
  if(el->with.current != oldWith){
    if(0==rc && wSelf) cwal_ref(wSelf);
    for(cwal_size_t i = withLen; i < withHolder->count; ++i){
      withHolder->list[i] = NULL;
    }
    withHolder->count = withLen;
    el->with.current = oldWith;
    if(0==rc && wSelf) cwal_unhand(wSelf);
  }
  if(scel) whcl_scope_pop(el, scel, wSelf);
  whcl__holder_truncate(el, holderLen, wSelf);
  if(wSelf) *rv = wSelf;
  return rc;
}

/**
   Builtin commands, a.k.a. keywords. Maintenance notes:

   - whcl__bic_search() and whcl__ttype_get_bic() must be
     key in sync. Use kwhasher.c to generate the hashes for those
     functions.
   - Each entry needs a TOK1_T_BIC_xxxx entry, which also requires
     updating tok1_t_cstr().
*/
static const struct {
  whcl__bic const affirm;
  whcl__bic const alias;
  whcl__bic const array;
  whcl__bic const assert_;
  whcl__bic const break_;
  whcl__bic const catch_;
  whcl__bic const concat;
  whcl__bic const const_;
  whcl__bic const continue_;
  whcl__bic const debug_;
  whcl__bic const decl;
  whcl__bic const decr;
  whcl__bic const define;
  whcl__bic const do_;
  whcl__bic const echo;
  whcl__bic const eval;
  whcl__bic const exception;
  whcl__bic const exit;
  whcl__bic const expr;
  whcl__bic const for_;
  whcl__bic const foreach;
  whcl__bic const if_;
  whcl__bic const incr;
  whcl__bic const info;
  whcl__bic const new_;
  whcl__bic const object;
  whcl__bic const pragma;
  whcl__bic const proc;
  whcl__bic const return_;
  whcl__bic const set;
  whcl__bic const throw_;
  whcl__bic const unset;
  whcl__bic const while_;
  whcl__bic const with;
#if 0
  whcl__bic const add;
  whcl__bic const subtract;
  whcl__bic const mult;
  whcl__bic const divide;
#endif
} whcl__builtins = {
#define BIC(N) whcl__bic_f_## N
#define DUMMY BIC(dummy)
#define T(X) TOK1_T_BIC_##X
/* These are kept in alphabetical order for readability but
   that's not a technical requirement. */
  {"affirm",   T(affirm),    1, -1,  BIC(affert), "[value...]"},
  {"alias",    T(alias),     1, 4,   BIC(alias),
               "[-ro] [-weak] varName|container[property] "
               "[interceptor-function]"},
  {"array",    T(array),     0, -1,  BIC(array), "[value...]"},
  {"assert",   T(assert),    1, -1,  BIC(affert), "[value...]"},
  {"break",    T(break),     0, -1,  BIC(brc),
               "[value|builtin command args...]"},
  {"catch",    T(catch),     1, 3,   BIC(catch),
   "[-noscope] [varname] {body}"},
  {"concat",   T(concat),    1, -1,  BIC(concat), "[args...]"},
  {"const",    T(const),     2, -1,  BIC(decl),
               "varname value...|builtin-command args..."},
  {"continue", T(continue),  0, 0,   BIC(brc), ""},
  {"__debug",  T(__debug),   1, 1,   BIC(eval), "{code block}"},
  {"decl",     T(decl),      1, -1,  BIC(decl),
               "[-const] varname [value...|builtin-command args...] "
               "| {varName value var2 value...}"
  },
  {"decr",     T(decr),      1, 2,   BIC(crement),
               "varname|deref[key] [by-value]"},
  {"define",   T(define),    2, 2,   DUMMY, "identifier value"},
  {"do",       T(do),        3, 3,   BIC(dowhile),
               "{BODY} while {EXPR}"},
  {"echo",     T(echo),      0, -1,  BIC(echo), "[-n] [args...]"},
  {"eval",     T(eval),      1, 3,   BIC(eval),
               "[-scope] [->] value|{code}|builtin-command args..."},
  {"exception",T(exception), 1, -1,  BIC(exception),
               "[integer code] message-value|builtin-command args..."},
  {"exit",     T(exit),      0, 1,   BIC(threxit), "[value]"},
  {"expr",     T(expr),      1, -1,  BIC(expr), "args..."},
  {"for",      T(for),       4, 4,   BIC(for),
               "{PRE} {TEST} {POST} {CODE}"},
  {"foreach",  T(foreach),   3, 5,   BIC(foreach),
               "[-props] [indexVarName] valueVarname value {CODE}"},
  {"if",       T(if),        2, -1, BIC(if),
               "{TEST} {CODE} [elif {TEST} {CODE}|else {CODE}...]"},
  {"incr",     T(incr),      1, 2,   BIC(crement),
               "varname|deref[key] [by-value]"},
  {"info",     T(info),      1, 2,   BIC(info), "info-cmd [arg]"},
  {"new",      T(new),       1, -1,  BIC(new),
               "[-post] arg... [{post-construction code}]"},
  {"object",   T(object),    0, -1,  BIC(object),
               "key value... | {[key value...]}"},
  {"pragma",   T(pragma),    1, -1,  BIC(pragma), "subcommand [args...]"},
  {"proc",     T(proc),      2, 7,   BIC(proc),
               "[-global|-anon|-local] [name] {args} {body} "
               "[using [-scope] {...}]"},
  {"return",   T(return),    0, -1,  BIC(brc),
               "[value|builtin-command args...]"},
  {"set",      T(set),       2, -1,  BIC(set),
               "-const IDENTIFIER|$deref[key] "
               "value|builtin-command args..."},
  {"throw",    T(throw),     1, -1,  BIC(threxit),
               "value|builtin-command args..."},
  {"unset",    T(unset),     1, -1,  BIC(unset),
               "IDENTIFIER|$deref(key)..."},
  {"while",    T(while),     2, 2,   BIC(while), "{EXPR} {BODY}"},
  {"with" ,    T(with),     2, 2,    BIC(with), "value {BODY}"}
#undef T
#undef DUMMY
#undef BIC
};

whcl__bic const * whcl__bic_search(char const *s, uint16_t n){
  static const uint16_t maxLen = 9/*length of longest builtin name*/;
#if !defined(NDEBUG)
  static bool once = false;
  if(!once){
    /* Sanity checks on the whcl__builtins array and enum */
    once = true;
    assert(0==strcmp("affirm", whcl__builtins.affirm.name));
    assert(0==strcmp("array", whcl__builtins.array.name));
    assert(0==strcmp("with", whcl__builtins.with.name));
  }
#endif
  //MARKER(("h=0x%08x w=%.*s\n", whcl__hash_keyword(s, n), (int)n, s));
  switch(n>maxLen ? 0 : whcl__hash_keyword(s, n)){
#define THEN(K,W) return (sizeof(W)-1==n && 0==memcmp(s,W,n))   \
    ? &whcl__builtins.K : NULL
    /* Values generated by kwhasher.c */
    case 0x006a337f: THEN(affirm,"affirm");
    case 0x0035161e: THEN(alias,"alias");
    case 0x00351c4e: THEN(array,"array");
    case 0x006a44d0: THEN(assert_,"assert");
    case 0x0035a10e: THEN(break_,"break");
    case 0x00362bfe: THEN(catch_,"catch");
    case 0x006c6640: THEN(concat,"concat");
    case 0x003634ee: THEN(const_,"const");
    case 0x01b1e85d: THEN(continue_,"continue");
    case 0x00cffbfc: THEN(debug_,"__debug");
    case 0x001b574c: THEN(decl,"decl");
    case 0x001b5812: THEN(decr,"decr");
    case 0x006d7577: THEN(define,"define");
    case 0x0006d355: THEN(do_,"do");
    case 0x001b9e03: THEN(echo,"echo");
    case 0x037522de: THEN(exception,"exception");
    case 0x001ba270: THEN(exit,"exit");
    case 0x001b9fb0: THEN(eval,"eval");
    case 0x001ba362: THEN(expr,"expr");
    case 0x000df0c6: THEN(for_,"for");
    case 0x00df6292: THEN(foreach,"foreach");
    case 0x00072a36: THEN(if_,"if");
    case 0x001cb6ae: THEN(incr,"incr");
    case 0x001cb6cf: THEN(info,"info");
    case 0x000f0798: THEN(new_,"new");
    case 0x00797348: THEN(object,"object");
    case 0x007a8fbb: THEN(pragma,"pragma");
    case 0x001ea02f: THEN(proc,"proc");
    case 0x007cce16: THEN(return_,"return");
    case 0x000fb5de: THEN(set,"set");
    case 0x003f7902: THEN(throw_,"throw");
    case 0x004003f2: THEN(unset,"unset");
    case 0x004114ec: THEN(while_,"while");
    case 0x002088d4: THEN(with,"with");

#undef THEN
  }
  return NULL;
}

whcl__bic const * whcl__ttype_get_bic(int ttype){
  switch(ttype){
#define CASE2(X,Y) case TOK1_T_BIC_##X: return &whcl__builtins.Y
#define CASE(X) CASE2(X,X)
    CASE(affirm);
    CASE(alias);
    CASE(array);
    CASE2(assert,assert_);
    CASE2(break,break_);
    CASE2(catch,catch_);
    CASE(concat);
    CASE2(const,const_);
    CASE2(continue,continue_);
    CASE2(__debug,debug_);
    CASE(decl);
    CASE(decr);
    CASE(define);
    CASE2(do,do_);
    CASE(echo);
    CASE(eval);
    CASE(exception);
    CASE(exit);
    CASE(expr);
    CASE2(for,for_);
    CASE(foreach);
    CASE2(if,if_);
    CASE(incr);
    CASE(info);
    CASE2(new,new_);
    CASE(object);
    CASE(pragma);
    CASE(proc);
    CASE2(return,return_);
    CASE(set);
    CASE2(throw,throw_);
    CASE(unset);
    CASE2(while,while_);
    CASE(with);
#undef CASE
#undef CASE2
  }
  return NULL;
}

int whcl__biv_search(char const *s, uint16_t n){
  static const uint16_t maxLen = 9/*length of longest builtin name*/;
  switch(n>maxLen ? 0 : whcl__hash_keyword(s, n)){
#define THEN(K,W) return (sizeof(W)-1==n && 0==memcmp(s,W,n))   \
    ? TOK1_T_BIV_##K : 0
    /* Values generated by kwhasher.c */

    case 0x001fb8b9: THEN(true,"true");
    case 0x0037cfba: THEN(false,"false");
    case 0x001e15b8: THEN(null,"null");
    case 0x001fb6bf: THEN(this,"this");
    case 0x04005eaa: THEN(undefined,"undefined");
    case 0x00400250: THEN(using,"using");
    case 0x00208640: THEN(whcl,"whcl");
    case 0x019f2ce6: THEN(__COLUMN,"__COLUMN");
    case 0x0067c8d7: THEN(__LINE,"__LINE");
    case 0x0067c407: THEN(__FILE,"__FILE");
    case 0x033e2ff2: THEN(__FILEDIR,"__FILEDIR");
    case 0x0033e0b4: THEN(__FLC,"__FLC");
    
#undef THEN
  }
  return 0;
}

int whcl__biv_search_t(whcl_script const * const ct,
                       whcl_stoken const * const t){
  return whcl__biv_search(whcl_stoken_cstr(ct, t, NULL, false),
                          (uint16_t)t->length);
}


static int whcl__t_is_definitely_not_command(int ttype){
  int rc = tok1_t_is_junk(ttype);
  if(0==rc) rc = tok1_t_is_space(ttype);
  return rc ? rc : whcl_t_is_eox(ttype);
}

int whcl__holder_push(whcl_engine * const el, cwal_value * const v){
#define EVH el->scopes.current->evalHolder
  //if(!v) return 0;
  int rc;
  cwal_ref(v);
  if(!EVH){
    EVH = cwal_new_array(el->ec);
    if(!EVH){
      cwal_unref(v);
      WHCL__WARN_OOM;
      return CWAL_RC_OOM;
    }
    cwal_value * const av = cwal_array_value(EVH);
    cwal_value_make_vacuum_proof(av, true);
    cwal_ref(av);
    assert(cwal_value_is_vacuum_proof(av));
    whcl__rescopeE(el, av);
  }
  rc = cwal_array_append(EVH, v);
  cwal_unref(v);
  return rc;
#undef EVH
}

int whcl__ttype_is_biv(int ttype){
  switch(ttype){
    case TOK1_T_BIV:
    case TOK1_T_BIV_true:   case TOK1_T_BIV_false:
    case TOK1_T_BIV_null:   case TOK1_T_BIV_undefined:
    case TOK1_T_BIV_this:   case TOK1_T_BIV_using:
    case TOK1_T_BIV_whcl:   case TOK1_T_BIV___COLUMN:
    case TOK1_T_BIV___FILE: case TOK1_T_BIV___FILEDIR:
    case TOK1_T_BIV___LINE: case TOK1_T_BIV___FLC:
      return ttype;
    default: return 0;
  }
}

int whcl__ttype_is_bic(int ttype){
  switch(ttype){
    case TOK1_T_BIC:
    case TOK1_T_BIC_affirm: case TOK1_T_BIC_alias:
    case TOK1_T_BIC_array:
    case TOK1_T_BIC_assert: case TOK1_T_BIC_break:
    case TOK1_T_BIC_catch: case TOK1_T_BIC_concat:
    case TOK1_T_BIC_continue: case TOK1_T_BIC___debug:
    case TOK1_T_BIC_decl: case TOK1_T_BIC_decr:
    case TOK1_T_BIC_define: case TOK1_T_BIC_do:
    case TOK1_T_BIC_echo: case TOK1_T_BIC_eval:
    case TOK1_T_BIC_exit: case TOK1_T_BIC_expr:
    case TOK1_T_BIC_for: case TOK1_T_BIC_foreach:
    case TOK1_T_BIC_if: case TOK1_T_BIC_incr:
    case TOK1_T_BIC_new:
    case TOK1_T_BIC_object: case TOK1_T_BIC_pragma:
    case TOK1_T_BIC_proc: 
    case TOK1_T_BIC_return:
    case TOK1_T_BIC_set: case TOK1_T_BIC_throw:
    case TOK1_T_BIC_unset: case TOK1_T_BIC_while:
    case TOK1_T_BIC_with:
      return ttype;
    default: return 0;
  }
}

/**
   Requires that tok be a TOK1_T_PropAccessWith token.  This function
   resolves to the current "with" value associated with its LHS,
   taking care not to cross function call boundaries. On success, sets
   `*rv` to the `with` container and returns 0. On error it returns
   non-0 and updates the engine's error state.
*/
static int whcl__get_with(whcl_engine * const el,
                          whcl_script * const ct,
                          whcl_stoken const * const tok,
                          cwal_value ** rv){
  *rv = NULL;
  assert(TOK1_T_PropAccessWith==tok->ttype);
  if(!el->with.current){
    return whcl__err(el, ct, tok, CWAL_SCR_SYNTAX,
                     "The .propertyName syntax is only "
                     "legal when a 'with' is active.");
  }
  whcl_scope const * sc = el->scopes.current;
  uint16_t sLevel = sc->level;
  uint16_t levelsUp = 0;
  /** Starting at the current scope, walk up until we find one with
      WHCL__SCOPE_F_WITH. That's our first-level with. A scope with
      the WHCL__SCOPE_F_CALL blocks lookup, regardless of
      WHCL__SCOPE_F_XSYM. */
  while(sc){
    while(sc && 0==(WHCL__SCOPE_F_WITH & sc->flags)){
      if(WHCL__SCOPE_F_CALL & sc->flags){
        return whcl__err(el, ct, tok, CWAL_SCR_SYNTAX,
                         "A function call boundary blocks lookup of "
                         "'with' properties.");
      }
      --sLevel;
      sc = whcl__scope_at(el, sLevel);
    }
    if(!sc) break;
    if(++levelsUp == tok->ttype2){
      cwal_size_t const n = el->with.holder.count;
      assert(n>=levelsUp);
      cwal_value * const w =
        (cwal_value*)el->with.holder.list[n - levelsUp];
      *rv = w;
      return 0;
    }
    --sLevel;
    sc = whcl__scope_at(el, sLevel);
  }
  return whcl__err(el, ct, tok, CWAL_SCR_SYNTAX,
                   "Too many dots for the current 'with' level.");
}

int whcl__eval_token(whcl_engine * const el, whcl_script * const ct,
                     whcl_stoken const * const tok, uint32_t flags,
                     cwal_value ** rv){
  enum goBackTo { GBT_ERR = -1, GBT_NONE = 0, GBT_SUBSCRIPT = 1 };
  int rc = 0;
  whcl_stoken const * const tCurrent = whcl__script_token(ct);
  cwal_size_t const holderLen = whcl__holder_len(el);
  cwal_value * lhs = NULL;
  enum goBackTo gogo = GBT_NONE;
  whcl__stoken_set(ct, tok);
  //whcl_err_reset(el);
  whcl__dotop_set(el, NULL, NULL, NULL);
  switch(el->skipLevel>0 ? TOK1_T_VTab
         : (whcl__token_has_prop(ct, tok)
            ? 0 : tok->ttype)){
    case TOK1_T_VTab: /* Skip-mode */
      if(rv) *rv = cwal_value_undefined();
      rc = 0;
      break;
    case TOK1_T_EOF:
    case TOK1_T_EOX:
    case TOK1_T_EOL:
      rc = whcl_err_set(el, CWAL_RC_MISUSE,
                        "Don't try to eval EOX tokens.");
      break;
    case TOK1_T_BIV:
    case TOK1_T_IdentifierDeref:
      /* Noting that the X[Y] variants of these are handled in the
         case 0 block. */
      rc = whcl__varident_lookup(el, ct, tok, rv);
      break;
    case 0:{
      /** $var[key1][key2][...keyN]. The [key] part(s) is/are
          structured as a chain "inside" $var:

          $var --> ...
            \
            [key1] --> [key2] --> [...keyN] --> EOF

         We have to keep resolving LHS[key] pairs until
         we hit the end of that inner chain.
      */
      assert(tok->subscriptId || (TOK1_T_PropAccessWith==tok->ttype));
      /* Step 1: resolve the X part of the X[Y] construct... */
      switch(tok->ttype){
        case TOK1_T_CallBlock:
        case TOK1_T_BraceGroup:
          gogo = GBT_SUBSCRIPT;
          goto gogo_callblock;
        case TOK1_T_QuotedString:
        case TOK1_T_LiteralNumber:
          rc = whcl__create_value(el, ct, tok, &lhs);
          break;
        case TOK1_T_PropAccessWith:
          rc = whcl__get_with(el, ct, tok, &lhs);
          break;
        default:
          if(!whcl__ttype_legal_prop_lhs(tok->ttype)){
            whcl__dump_stok(ct, tok, "Internal error: should not have subscriptId");
            assert(!"Token re-organization did something wrong.");
            whcl__fatal(CWAL_RC_CANNOT_HAPPEN, "Internal error: "
                        "we gave a subscriptId to a %s-type token.",
                        tok1_t_cstr(tok->ttype));
          }
          rc = whcl__varident_lookup(el, ct, tok, &lhs);
      }
      gogo_subscript:
      assert(GBT_NONE == gogo);
      if(rc || (rc = whcl__holder_push(el, lhs))) break;
      /* Steps 2 to N: iteratively resolve the Y/Z/etc parts of the
         X[Y][Z][...] construct. The final result of this is the
         right-most LHS and the right-most property key, which we
         stuff into whcl__dotop_set() so that the being-processed
         command can do something with those. */
      cwal_value * key = NULL;
      whcl_stoken const * sub = NULL;
      bool doWith = (TOK1_T_PropAccessWith == tok->ttype)
        /* The first step of handling a PropertyAccessWith token
           requires slightly different treatment because it's
           structured differently. */;
      sub = whcl__script_at(ct, doWith
                            ? tok->innerId : tok->subscriptId);
      assert(sub);
      while( sub && !whcl_stoken_is_eof(sub) ){
        if(!doWith && TOK1_T_PropAccess!=sub->ttype){
          whcl__dump_stok(ct, sub, "non-property?");
          if(1 || TOK1_T_BraceGroup!=sub->ttype){
            assert(doWith ? 1 : (TOK1_T_PropAccess==sub->ttype
                                 && "Else we flagged a token->ttype incorrectly."));
            whcl__fatal(CWAL_RC_ERROR,
                        "Compilation (mis-tagged token) or eval error? You decide.");
          }
        }
        whcl__stoken_set(ct, sub);
        rc = doWith
          ? whcl__eval_token(el, ct, sub, WHCL__EVAL_F_DEREF2_SUB, &key)
          : whcl__eval_sub(el, false, ct, WHCL__EVAL_F_DEREF2_SUB, &key);
        if(rc || (rc = whcl__holder_push(el, key))) break;
        //whcl__dump_val(key, "Key?");
        //whcl__dump_val(lhs, "lhs");
        sub = doWith
          ? (whcl__script_at(ct, tok->subscriptId
                             ? tok->subscriptId
                             : whcl__script_eof_id(ct)))
          : whcl_stoken_sibling(ct, sub);
        doWith = false;
        if(sub && !whcl_stoken_is_eof(sub)){
          /* Look up lhs[key], making that result the new lhs for the
             next iteration of this loop... */
          cwal_value * v = NULL;
          whcl__script_errtoken_set(ct, sub);
          rc = whcl_lookup_vsym_v(el, lhs, key, false, &v);
          if(rc) break;
          else if(v && (rc = whcl__holder_push(el, v))) break;
          else if(!v) v = cwal_value_undefined();
          whcl__script_errtoken_set(ct, NULL);
          lhs = v;
          key = NULL/*not leaked - is in the eval holder*/;
          //whcl__dump_val(lhs, "lhs");
        }
      }
      if(rc) break;
      else if(rv){
        /* We only bother doing the final lookup if the caller passed
           us an output result pointer. */
        rc = whcl_lookup_vsym_v(el, lhs, key, false, rv);
        if(rc) break;
#if 0
        else if(!*rv) *rv = cwal_value_undefined();
#endif
      }
      whcl__dotop_set(el, lhs, key, NULL)
        /* This is how we communicate the LHS[KEY] parts to the
           current in-progress command. */;
      break;
    }/*whcl__token_has_prop()*/
    case TOK1_T_CallBlock:
      whcl__dump_stok(ct, tok,
            "WARNING: $(...) has been phased out. Use [...] instead.");
      CWAL_SWITCH_FALL_THROUGH;
    case TOK1_T_BraceGroup:
      gogo_callblock:
      rc = whcl__eval_sub(el, false, ct, WHCL__EVAL_F_SUB_CALLBLOCK,
                          gogo ? &lhs : rv);
      break;
    case TOK1_T_SquigglyGroup:
      rc = (WHCL__EVAL_F_SQUIGGLY_AS_CODE & flags)
        ? whcl__eval_sub(el,
                         WHCL__EVAL_F_SQUIGGLY_AS_SCOPE
                         ==(WHCL__EVAL_F_SQUIGGLY_AS_SCOPE & flags),
                         ct, 0, rv)
        : whcl__create_value(el, ct, tok, rv);
      break;
    case TOK1_T_ParenGroup:
      rc = whcl__eval_sub(el, false, ct, WHCL__EVAL_F_SUB_EXPR, rv);
      break;
    case TOK1_T_AtExpand:
      assert(tok->innerId);
      if(WHCL__EVAL_F_EXPANDO_OKAY & flags){
        whcl_stoken const * const sub = whcl__script_at(ct, tok->innerId);
        assert(sub);
        rc = whcl__eval_token(el, ct, sub,
                              WHCL__EVAL_F_SQUIGGLY_AS_SCOPE, rv);
      }else{
        rc = whcl_err_set(el, CWAL_SCR_SYNTAX,
                          "@rray expansion is not permitted in "
                          "this context.");
      }
      break;
    default:
      rc = whcl__create_value(el, ct, tok, rv);
      break;
  }
  switch(rc ? GBT_ERR : gogo){
    case GBT_ERR: break;
    case GBT_NONE: break;
    case GBT_SUBSCRIPT:
      if(!lhs) lhs = cwal_value_undefined();
      gogo = GBT_NONE;
      goto gogo_subscript;
  }
  whcl__stoken_set(ct, tCurrent);
  whcl__holder_truncate(el, holderLen, rv && 0==rc ? *rv : NULL);
  return whcl__check_interrupted(el, rc);
}

/**
   Finds the "next" token which appears to be the start of a command
   (without traversing into block constructs) and returns it, noting
   that it will return an EOF token at the end of input and NULL if ct
   has no tokens. If advanceFirst is true then whcl__script_next_token2()
   is called once before checking for a command, otherwise the current
   token is considered a potential candidate.

   The returned token will be ct's "current" token.
*/
static whcl_stoken const * whcl__to_next_command(whcl_script * const ct, bool advanceFirst){
  whcl_stoken const * tok;
  if(advanceFirst){
    if(0!=whcl__script_next_token2(ct, &tok)) return NULL;
  }else{
    tok = whcl__script_token(ct);
    if(!tok && 0!=whcl__script_next_token2(ct, &tok)) return NULL;
  }
  while(tok && (!whcl_stoken_is_eof(tok)
                && whcl__t_is_definitely_not_command(tok->ttype))){
    whcl__script_next_token2(ct, &tok);
  }
  return tok;
}

/**
   If v is-a Function or has a Function-type property called
   el->cache.keyCommand (checking recursively up its prototype chain)
   then that function is returned, else NULL is returned. `*isACommand`
   is set to false for the first case, true for the 2nd case, and not
   modified if NULL is returned.
*/
static cwal_function * whcl__value_callable(whcl_engine * const el,
                                            cwal_value * const v,
                                            bool * isACommand){
#if 1
  cwal_function * f = cwal_value_function_part(el->ec, v);
  if(f) *isACommand = false;
  else{
    cwal_value * const x =
      whcl__vsym_v_proxy(el, v, el->cache.keyCommand, NULL);
    if(x && (f = cwal_value_function_part(el->ec, x))) *isACommand = true;
  }
#else
  // Same as ^^^ but swap the check order. This approach is
  // semantically broken, but keep it here for the time being as a
  // reminder not to try this again.
  cwal_value * const x =
    whcl__vsym_v_proxy(el, v, el->cache.keyCommand, NULL);
  cwal_function * f = x ? cwal_value_get_function(x) : NULL;
  if(f) *isACommand = true;
  else{
    if((f = cwal_value_get_function(v))) *isACommand = false;
  }
#endif
  return f;
}

/**
   Expects to be passed a token from the start of a line.  If that
   token resolves to a cwal-native command then `*rv` is set to the
   resolved value and `*rf` is set to the command function (which may
   differ from `*rv` and is expected to be used as the "this" for the
   upcoming call). If it doesn't appear to be a command, and exception
   is thrown. It does _not_ resolve BICs, only Functions.

   For the semantics of the isACommand argument see
   whcl__value_callable().

   If whcl__token_has_prop(ct,tok) and this function succeeds then
   whcl__dotop_get() will briefly hold the and key used for the
   lookup. For any other token type, that state will be cleared before
   the lookup is performed.
*/
static int whcl__cmd_check_callable(whcl_engine * const el,
                                    whcl_script * const ct,
                                    whcl_stoken const * const tok,
                                    cwal_value ** rv,
                                    cwal_function ** rf,
                                    bool * isACommand){
  int rc = 0;
  cwal_midsize_t tlen = 0;
  char const * cstr = whcl_stoken_cstr(ct, tok, &tlen, false);
  cwal_value * v = NULL;
  cwal_function * f = NULL;
  if(tok->innerId || tok->subscriptId){
    // Kludge to capture the whole token for those which
    // contain other tokens.
    tlen = whcl__script_at(ct, tok->nextId)->begin - tok->begin;
    /* ^^^ This length is not actually correct, but it's close enough
       for now:

       j.sort [proc {l r} { return [l.name.compare r.name]}]

       ==> Symbol 'j.sort ' does not resolve to a command.

       Note the extra space, caused because tlen is based on
       tok->nextId and tok->nextId is the [proc...] block. That
       behavior is correct, but that it catches any number of
       filtered-out tokens between tok and tok->nextId is, in this
       case, unfortunate. We cannot simply use tok->length here
       because then the error report would be that 'j' does not
       resolve to a command, which would be misleading enough to be
       called wrong in this context (even though it's true, it's not
       the reason for that particular error case).
    */
  }
  switch(whcl__token_has_prop(ct, tok)
         ? 0 : tok->ttype){
    case TOK1_T_BIV:
    case TOK1_T_Identifier:
      rc = whcl__varident_lookup(el, ct, tok, &v);
      goto handle_v;
    case TOK1_T_IdentifierDeref:
    case 0:{
      rc = whcl__eval_token(el, ct, tok, 0, &v);
      handle_v:
      if(rc) break;
      f = v ? whcl__value_callable(el, v, isACommand) : NULL;
      if(f){
        *rv = v;
        *rf = f;
      }else{
        rc = whcl__throw(el, ct, tok, CWAL_RC_TYPE,
                              "Symbol '%.*s' does not resolve to a command.",
                              (int)tlen, cstr);
      }
      break;
    }
    default:
      rc = whcl__throw(el, ct, tok, CWAL_RC_NOT_FOUND,
                            "Expecting a command "
                            "(identifier or $varDeref) "
                            "but got: %.*s",
                            (int)tlen, cstr);
      break;
  }
  return rc;
}

/**
   Expects to be called when ct's current token is a COMMAND. `*nToks`
   must be the number of entries in the toks array. This function
   collects all tokens until (but not including) EOX into toks,
   starting with ct's current token. Returns 0 on success and only
   fails if the line has more than `*nToks` tokens. Upon return
   `*nToks` is modified to the number entries put into the array.
*/
static int whcl__collect_command(whcl_script * const ct,
                                 whcl_stoken const ** toks,
                                 uint16_t * nToks){
  uint16_t i = 0;
  whcl_stoken const * tok = 0;
  toks[i++] = tok = whcl__script_token(ct);
  //assert(*nToks>=24);
  assert(*nToks);
  assert(tok);
  while(0==whcl__script_next_token2(ct, &tok)
        && !whcl_t_is_eox(tok->ttype)){
    if(i==*nToks){
      int const rc =
        whcl__script_err(ct, CWAL_RC_RANGE,
                    "Not enough stack space was reserved "
                     "for command argument tokens.",
                    (int)*nToks);
      return rc;
    }
    toks[i++] = tok;
  }
  *nToks = i;
  return 0;
}

int whcl__bic_check_argc(whcl_engine * const el,
                         whcl__bic const * const bic,
                         whcl_script * const ct,
                         whcl_stoken const * const * const tokens,
                         uint16_t nTok){
  cwal_midsize_t cmdLen = 0;
  char const * cmdName = whcl_stoken_cstr(ct, tokens[0], &cmdLen, false);
  int16_t const argc = nTok-1;
  assert(nTok>0);
  if(0==bic->argcMin && 0==bic->argcMax && argc){
    return whcl__throw(el, ct, tokens[0], CWAL_RC_MISUSE,
                            "Builtin '%.*s' does not accept any arguments.",
                            (int)cmdLen, cmdName);
  }else if((bic->argcMin>0 && argc<bic->argcMin)
           || (bic->argcMax>0 && argc>bic->argcMax)){
#if 0
    MARKER(("bic=%s argc=%d, nTok=%d, minArgs=%d, maxArgs=%d\n", bic->name,
            (int)argc, (int)nTok, (int)bic->argcMin, (int)bic->argcMax));
    for(uint16_t i = 0; i < nTok; ++i ){
      whcl__dump_stok(ct, tokens[i], "arg");
    }
#endif
    return whcl__throw(el, ct, tokens[0], CWAL_RC_MISUSE,
                            "Invalid argument count for '%.*s'. "
                            "Expecting: %s",
                            (int)cmdLen, cmdName, bic->usage);
  }
  return 0;
}

/** @internal
  This function looks for the "next" COMMAND line in ct (using el's
  current whcl_script instance if ct is NULL), where "next" means:

  1) The current one if the current token looks like it might be the
     start of a command else...
  2) Read the tokenizer (without recursing into block constructs!) until
     it hits the start of a line, then treat that as a command.

  If a command is found it is called, the result code of doing so
  is returned, and the cwal_value result (if any) is assigned
  to `*rv`. `rv` may be NULL, in which case any result value will
  be discarded before this call returns.

  If `isCallBlock` is true then this processing assumes that it is
  handling a `[...]` block. In that case the isSubcall flag will be
  set on the BIC call and it will fail if ct contains more tokens than
  a single call. Recall that the compilation phase strips all EOLs out
  of `[...]` and `(...)` groups, so any contents are inherently a
  single command unless a user adds a `;` in there somewhere.

  If there are no more commands to run then WHCL_RC_NO_COMMANDS is
  returned. That is not an error, just an indication that there's
  nothing more to do.
*/
static int whcl__process_next_command(whcl_engine * const el, whcl_script * ct,
                                      bool isCallBlock, cwal_value **rv);
int whcl__process_next_command(whcl_engine * const el, whcl_script * ct,
                               bool isCallBlock, cwal_value **rv){
  int rc = 0;
  whcl_stoken const * tok;
  cwal_size_t const holderLen = whcl__holder_len(el);
  if(!ct) ct = el->ct;
  tok = whcl__to_next_command(ct, false);
  if(whcl_stoken_is_eof(tok)){
    rc = WHCL_RC_NO_COMMANDS;
    if(rv) *rv = NULL;
    goto end;
  }else if(isCallBlock && whcl_t_is_eox(tok->ttype)){
    rc = whcl_err_set(el, CWAL_SCR_SYNTAX,
                      "Invalid ';' in [...] block.");
    goto end;
  }
  assert(tok == whcl__script_token(ct));
  enum { MaxTokens = 64 };
  whcl_stoken const * tokens[MaxTokens] = {0};
  uint16_t nToks = MaxTokens-1;
  memset(tokens, 0, sizeof(tokens));
  assert(NULL==tokens[MaxTokens-1]);
  rc = whcl__collect_command(ct, tokens, &nToks);
  if(rc) goto end;
  else if(0==nToks){
    assert(!"Cannot happen, can it?");
    rc = WHCL_RC_NO_COMMANDS;
    goto end;
  }else if(el->skipLevel>0){
    if(rv) *rv = cwal_value_undefined();
    goto end;
  }
  assert(NULL==tokens[nToks]);
  whcl_stoken const * const ctPos = whcl__script_token(ct)
    /* Evaluation may well change ct's current token, so
       we need to restore the evaluation point after processing
       the command. */;
  tok = tokens[0];
  whcl__bic const * const bic
    = whcl__ttype_get_bic(tok->ttype2);
  if(bic){
    rc = whcl__bic_call(el, bic, isCallBlock, ct, tokens, nToks, rv);
  }else{
    cwal_value * v = NULL;
    cwal_function * f = NULL;
    whcl__strace_entry strace = whcl__strace_entry_empty;
    bool isACommand = false;
    rc = whcl__cmd_check_callable(el, ct, tok, &v, &f, &isACommand);
    if(0==rc && 0==(rc = whcl__strace_push(el, ct, tokens[0], &strace))){
      cwal_value * const oldUsing = el->fcall.currentUsing;
      assert(v && f);
      el->fcall.currentUsing = NULL;
      cwal_value * vThis = NULL;
      if(!isACommand) whcl__dotop_get(el, &vThis, NULL, NULL);
      /* vThis this is needed for this syntax to work:

         decl s "..."
         echo $s[to-json]

         But it breaks (in some contexts):

         echo $s to-json

         The latter is currently the calling convention for the
         various classes, but maybe the former should be. $x[y] is
         more generic whereas ($x y) requires specific class-side
         dispatch code (namely, a command function which dispatches
         based on its first argument). The latter is more readable and
         easier to write.

         One breaking example:

         catch x { ... }
         assert $($x[message] index-of ...)

         In that case vThis is the exception object and we'd end up
         calling $x's inherited __command method, passing it
         "index-of".

         The workaround is to distinguish between a "Command" and
         non-command methods. A non-command method is one which is-a
         function. A Command method is one named el->cache.keyCommand,
         possibly fetched from v's prototype. In the former case we
         pass on vThis (if set, which it will be for $x[y] property
         access). In the latter we don't.

         That permits both calls syntaxes to work. Whew. Disaster
         averted.
      */
      //whcl__dump_stok(ct, tokens[0], "token[0]");
      //whcl__dump_stok(ct, tokens[nToks-1], "token[last]");
      rc = whcl__holder_push(el, v);
      if(0==rc) rc = whcl__holder_push(el, cwal_function_value(f));
      if(0==rc && vThis && vThis!=v) rc = whcl__holder_push(el, vThis);
      if(0==rc){
        rc = whcl__call_cwal_f(el, ct, f, vThis ? vThis : v,
                               tokens+1, nToks-1, rv);
      }
      el->fcall.currentUsing = oldUsing;
      if(rc) whcl__annotate_exception(el, NULL);
      whcl__strace_pop(el);
    }
  }
  if(rc) goto end;   
#if 0
  cwal_midsize_t tokLen = 0;
  char const * zTok;
  zTok = whcl_stoken_cstr(ct, tok, &tokLen, false);
  printf("COMMAND @%s:%d,%d (builtin=0x%p) %s %.*s\n",
         bic->name, (int)tok->line, (int)tok->column, (void*)bic,
         tok1_t_cstr(tok->ttype), (int)tokLen, zTok);
  for(uint16_t i = 1; 0==rc && i < nToks; ++i){
    tok = tokens[i];
    zTok = whcl_stoken_cstr(ct, tok, &tokLen, false);
    printf("  Arg #%d %s = %.*s\n", (int)i, tok1_t_cstr(tok->ttype),
           (int)tokLen, zTok);
  }
#endif
  if(0==rc) whcl__stoken_set(ct, ctPos);
  else rc = whcl__check_interrupted(el, rc);
  end:
  whcl__holder_truncate(el, holderLen, rv && 0==rc ? *rv : NULL);
  return rc;
}

char const * whcl__last_path_sep(char const * str, cwal_int_t slen ){
  if(slen<0) slen = (cwal_int_t)cwal_strlen(str);
  unsigned char const * pos = (unsigned char const *)str + slen;
  while( pos > (unsigned char const *)str ){
    if('/'==*pos || '\\'==*pos){
      return (char const *)pos;
    }
    --pos;
  }
  return NULL;
}

int whcl__create_value( whcl_engine * const el, whcl_script * const ct,
                        whcl_stoken const * const t, cwal_value ** rv ){
  return whcl__create_value2(el, ct, t, false, rv);
}

int whcl__create_value2( whcl_engine * const el, whcl_script * const ct,
                         whcl_stoken const * const t, bool bivAsString,
                         cwal_value ** rv ){
  switch(el->skipLevel>0 ? 0 : t->ttype){
    case 0:
      *rv = cwal_value_undefined();
      return 0;
    case TOK1_T_BIV:
      if(bivAsString) break;
      switch(t->ttype2){
        case TOK1_T_BIV_true: *rv = cwal_value_true(); return 0;
        case TOK1_T_BIV_false: *rv = cwal_value_false(); return 0;
        case TOK1_T_BIV_null: *rv = cwal_value_null(); return 0;
        case TOK1_T_BIV_this:
          *rv = whcl_var_search_v(el, NULL, true, el->cache.keyThis, NULL);
          if(!*rv) *rv = cwal_value_undefined();
          return 0;
        case TOK1_T_BIV_undefined: *rv = cwal_value_undefined(); return 0;
        case TOK1_T_BIV_using:
          *rv = el->fcall.currentUsing
            ? el->fcall.currentUsing : cwal_value_undefined();
          //whcl__dump_stok(ct, t, "using"); whcl__dump_val(*rv, "using");
          return 0;
        case TOK1_T_BIV_whcl: *rv = el->cache.vWhcl; return 0;
        case TOK1_T_BIV___COLUMN:
        case TOK1_T_BIV___LINE: {
          whcl_linecol_t const n = (TOK1_T_BIV___COLUMN==t->ttype2)
            ? t->column : t->line;
          cwal_value * const x
            = cwal_string_value(cwal_new_stringf(el->ec,"%d",n));
          if(x){
            *rv = x;
            return 0;
          }else{
            WHCL__WARN_OOM;
            return CWAL_RC_OOM;
          }
        }
        case TOK1_T_BIV___FILE:{
          cwal_midsize_t nName = 0;
          char const * fname = whcl_script_name_get(ct, &nName);
          cwal_value * const x
            = cwal_new_string_value(el->ec,
                                    fname ? fname : "(unnamed)",
                                    fname ? nName : 9);
          if(x){
            *rv = x;
            return 0;
          }else{
            WHCL__WARN_OOM;
            return CWAL_RC_OOM;
          }
        }
        case TOK1_T_BIV___FILEDIR:{
          cwal_midsize_t nName = 0;
          char const * fname = whcl_script_name_get(ct, &nName);
          if(!fname){
            *rv = cwal_value_undefined();
            return 0;
          }
          char const * sep = whcl__last_path_sep(fname, (cwal_int_t)nName);
          assert(sep ? sep >= fname : 1);
          nName = sep ? (cwal_size_t)(sep - fname) :  0;
          cwal_value * const x = nName
            ? cwal_new_string_value(el->ec, fname, nName)
            : (sep /* root dir */
               ? cwal_new_string_value(el->ec, sep, 1)
               : cwal_new_string_value(el->ec, "", 0));
          if(x){
            *rv = x;
            return 0;
          }else{
            WHCL__WARN_OOM;
            return CWAL_RC_OOM;
          }
        }
        case TOK1_T_BIV___FLC:{
          cwal_value * const x
            = cwal_string_value(cwal_new_stringf(el->ec,"%s:%d,%d",
                                                 ct->name ? ct->name : "(unnamed)",
                                                 (int)t->line, (int)t->column));
          if(x){
            *rv = x;
            return 0;
          }else{
            WHCL__WARN_OOM;
            return CWAL_RC_OOM;
          }
        }
        default:
          whcl__fatal(CWAL_RC_CANNOT_HAPPEN,"Unmapped TOK1_T_BIV subtype: %s",
                      tok1_t_cstr(t->ttype2));
      }/*BIVs switch(t->ttype2)*/
    default: break;
  }
  return whcl__script_create_value(ct, t, &el->escBuf, rv);
}

cwal_json_output_opt const * whcl_json_output_opt(bool formatted){
  static cwal_json_output_opt oF = cwal_json_output_opt_empty_m;
  static cwal_json_output_opt oU = cwal_json_output_opt_empty_m;
  if(NULL==oF.override.f){
    oF.cyclesAsStrings = 1;
    oF.functionsAsObjects = 0;
    oF.addNewline = 1;
    oF.indentSingleMemberValues = 0;
    oF.indent = 0;
    oF.indentString.str = "  ";
    oF.indentString.len = cwal_strlen(oF.indentString.str);
    oF.override.f = oU.override.f = cwal_json_override_f_whcl;
  }
  return formatted ? &oF : &oU;
}

void whcl__dump_value( cwal_value * const v, char const * msg,
                       char const * file, char const * func, int line ){
  cwal_scope const * sc = cwal_value_scope(v);
  FILE * const out = stdout /* Reminder: we cannot use cwal_output() because
                         v might be a built-in const without a cwal_engine
                         instance. */;
  if(v){
    assert((sc || cwal_value_is_builtin(v))
           && "Seems like we've cleaned up too early.");
  }
  if(file && func && line>0){
    fprintf(out,"%s:%d:%s(): ", file, line, func);
  }
  fprintf(out,"%s%s%s@%p[scope=#%d ref#=%d] "
          "==> ",
          msg ? msg : "", msg?": ":"",
          cwal_value_type_name(v),
          (void const *)v,
          (int)(sc ? sc->level : 0),
          (int)cwal_value_refcount(v)
          );
  switch( v ? cwal_value_type_id(v) : -1 ){
    case -1:
      fwrite("<NULL>\n", 7, 1, out);
      break;
    case CWAL_TYPE_BUFFER:
    case CWAL_TYPE_FUNCTION:
    case CWAL_TYPE_HASH:
    case CWAL_TYPE_NATIVE:
    case CWAL_TYPE_PROPREF:
    case CWAL_TYPE_UNIQUE:
      fprintf(out, "%s@%p\n",
              cwal_value_type_name(v), (void const*)v);
      break;
    case CWAL_TYPE_UNDEF:
      fwrite("undefined\n", 10, 1, out);
      break;
    default:
      cwal_json_output_FILE( v, out, whcl_json_output_opt(true) );
      break;
  }
}

/**
   If any non-EOF tokens are found after ct->token then this function
   triggers a syntax error and returns that error result, else it
   returns 0. If allowEox is true then an EOX after ct->token is
   permitted (but tokens beyond that one are not examined, even though
   they may technically be trailing junk).
*/
int whcl__trailing_junk_check(whcl_script * const ct, bool allowEox){
  int rc = 0;
  whcl_stoken const * const tok = whcl__script_token(ct);
  whcl_stoken const * const next = tok ? whcl__script_at(ct, tok->nextId) : NULL;
  if(next && !(allowEox
              ? whcl_t_is_eox(next->ttype)
               : whcl_stoken_is_eof(next))){
    //whcl__dump_stok(ct, next, "Junk?");
    whcl__script_errtoken_set(ct, next);
    rc = whcl__script_err(ct, CWAL_SCR_SYNTAX,
                          "Unexpected trailing junk tokens found.");
  }
  return rc;
}

int whcl__eval_sub( whcl_engine * const el, bool pushScope,
                    whcl_script * const ct, uint32_t flags,
                    cwal_value **rv ){
  if(el->skipLevel>0){
      if(rv) *rv = cwal_value_undefined();
      return 0;
  }
  whcl_script sub = whcl__script_empty;
  int rc = whcl__script_sub_from_group(ct, &sub, true);
  if(0==rc){
    if( WHCL__EVAL_F_SUB_EXPR & flags ){
      whcl_script * const oldT = el->ct;
      el->ct = &sub;
      rc = whcl__eval_expr(el, WHCL__EXPR_F_IGNORE_EOL
                           | WHCL__EXPR_F_EOX_NO_SEMICOLON,
                           rv);
      el->ct = oldT;
    }else{
      rc = whcl__eval_script(el, pushScope, &sub, flags, rv);
    }
  }
  whcl__script_finalize(&sub);
  return rc;
}

int whcl__eval_sub2( whcl_engine * const el, bool pushScope,
                     whcl_script * const ct,
                     whcl_stoken const * const tok,
                     uint32_t flags,
                     cwal_value **rv ){
  whcl_stoken const * const oldTok = whcl__script_token(ct);
  whcl__stoken_set(ct, tok);
  int const rc = whcl__eval_sub(el, pushScope, ct, flags, rv);
  whcl__stoken_set(ct, oldTok);
  return rc;
}


int whcl__eval_script( whcl_engine * const el, bool pushScope,
                      whcl_script * const ct, uint32_t flags,
                      cwal_value **rv ){
  int rc = 0;
  cwal_value * xrv = NULL;
  cwal_value * vHolder = NULL;
  whcl_scope * scel = NULL;
  cwal_size_t const holderLen = whcl__holder_len(el);
  whcl_script * const ctOld = el->ct;
  if(el->skipLevel>0){
    if(rv) *rv = cwal_value_undefined();
    return 0;
  }
  if(pushScope){
    scel = (WHCL__EVAL_F_SCOPE_INHERIT_FLAGS & flags)
      ? whcl__scope_push2(el)
      : whcl__scope_push(el, 0);
    if(!scel) { rc = CWAL_RC_OOM; return rc; }
  }
  //whcl__script_rewind(ct);
  //MARKER(("xsym check. scope flags=0x%04x\n",
  //        (int)scel->flags));
  el->ct = ct;
  vHolder = cwal_new_unique(el->ec, NULL);
  if(!vHolder){
    WHCL__WARN_OOM;
    rc = CWAL_RC_OOM;
    goto end;
  }
  rc = whcl__holder_push(el, vHolder);
  //whcl__dump_val(vHolder, "vHolder pushed into eval holder");
  bool justKeepLooping = true;
#define CHECK_VHOLDER_REF 0
  while(0==rc && justKeepLooping){
#if CHECK_VHOLDER_REF
    if(1!=cwal_value_refcount(vHolder)) goto vholder_misref;
#endif
    cwal_value * xrv2 = NULL;
    if(scel){ assert(el->scopes.current == scel); }
    if(WHCL__EVAL_F_EVAL_TOKEN & flags){
      whcl_stoken const * tok = 0;
      if( (rc = whcl__script_next_token2(ct, &tok)) ) break;
      rc = whcl__eval_token(el, ct, tok, flags, &xrv2);
      if(0==rc && (WHCL__EVAL_F_REJECT_TRAILING_JUNK & flags)){
        rc = whcl__trailing_junk_check(ct, false);
      }
      justKeepLooping = false;
    }else{
#if 1
      /* Reminder to self: this sweep was responsible for finding (via
         crashing) a mismanaged value in the proc parser :). */
      whcl_engine_sweep(el)
        /* Before calls, instead of after, because we keep the result
           of the last-run command around briefly on purpose. */;
#endif
      rc =
        whcl__process_next_command(el, ct,
                                   (WHCL__EVAL_F_SUB_CALLBLOCK & flags)
                                   == WHCL__EVAL_F_SUB_CALLBLOCK,
                                   &xrv2);
    }
    if(scel){ assert(el->scopes.current == scel); }
#if CHECK_VHOLDER_REF
    if(1!=cwal_value_refcount(vHolder)) goto vholder_misref;
#endif
    rc = whcl__check_interrupted(el, rc);
    if(0==rc){
      cwal_unique_wrapped_set(vHolder, xrv2);
      xrv = xrv2;
    }
  }
  if(0==rc || WHCL_RC_NO_COMMANDS==rc){
    rc = 0;
    cwal_value_ref(xrv);
    cwal_unique_wrapped_set(vHolder, NULL);
  }else{
    xrv = NULL /* managed by vHolder */;
  }
#if CHECK_VHOLDER_REF
  if(1!=cwal_value_refcount(vHolder)) goto vholder_misref;
#endif
  end:
  assert(rc ? NULL==xrv : 1);
  if(scel) whcl_scope_pop(el, scel, xrv);
  whcl__holder_truncate(el, holderLen, xrv);
  if(xrv){
    if(rv && 0==rc){
      *rv = cwal_unhand(xrv);
    }
    else{
      cwal_unref(xrv);
      xrv = NULL;
    }
  }
  el->ct = ctOld;
  return rc;
#if CHECK_VHOLDER_REF
  vholder_misref:
  whcl__dump_val(vHolder, "vHolder");
  whcl__fatal(CWAL_RC_ASSERT, "Memory corruption: unexpected refcount=%d\n",
              (int)1==cwal_value_refcount(vHolder));
  assert(1==cwal_value_refcount(vHolder));
  return CWAL_RC_ASSERT/*not reached*/;
#endif
#undef CHECK_VHOLDER_REF
}

int whcl_script_eval(whcl_script * const sc,
                     uint32_t flags,
                     cwal_value ** rv){
  whcl_engine * const el = whcl_engine_from_state(sc->ec);
  assert(el);
  return whcl__eval_script(el, 0!=(flags & WHCL_SCRIPT_EVAL_SCOPE),
                           whcl__script_rewind(sc),
                           WHCL__EVAL_F_SCOPE_INHERIT_FLAGS,
                           rv);
}

int whcl_eval_cstr( whcl_engine * const el,
                    bool pushScope,
                    char const * scriptName,
                    char const * src,
                    cwal_int_t len,
                    cwal_value ** rv ){
  int rc;
  whcl_script * const ctOld = el->ct;
  whcl_script ct = whcl__script_empty;
  whcl__script_init(el->ec, &ct);
  ct.name = scriptName ? scriptName : "whcl_eval_cstr()";
  el->ct = &ct;
  rc = whcl__compile(el, &ct, src, len, 0);
  assert(ct.name);
  if(0==rc) rc = whcl__eval_script(el, pushScope, &ct, 0, rv);
  assert(scriptName == ct.name);
  whcl__script_finalize(&ct);
  el->ct = ctOld;
  return rc;
}

int whcl_eval_buffer_take( whcl_engine * const el,
                           bool pushScope,
                           char const * scriptName,
                           cwal_buffer * const src,
                           cwal_value ** rv ){
  int rc;
  whcl_script * ct = NULL;
  if(!scriptName) scriptName = "whcl_eval_buffer_take()";
  rc = whcl_compile_buffer_take(el, &ct, scriptName, src);
  if(0==rc){
    assert(!src->mem);
    rc = whcl__eval_script(el, pushScope, ct, 0, rv);
    whcl_check_for_return(el, &rc, rv);
  }
  if(ct) whcl_script_free(ct);
  return rc;
}


int whcl_eval_cstr_with_var( whcl_engine * const el,
                             char const * varName,
                             cwal_value * const varValue,
                             char const * scriptName,
                             char const * src, cwal_int_t srcLen,
                             cwal_value **rv ){
  int rc;
  whcl_scope * scel = NULL;
  cwal_size_t const holderLen = whcl__holder_len(el);
  cwal_ref(varValue);
  if((rc = whcl__holder_push(el, varValue))){
    cwal_unhand(varValue);
    return rc;
  }  
  if(!(scel = whcl__scope_push2(el))) return CWAL_RC_OOM;
  rc = whcl_var_decl(el, NULL, false, varName, 
                     (cwal_int_t)cwal_strlen(varName),varValue);
  if(0==rc){
    rc = whcl_eval_cstr(el, false, scriptName, src, srcLen, rv );
  }
  whcl_scope_pop(el, scel, rc||!rv ? NULL : *rv);
  whcl__holder_truncate(el, holderLen, NULL);
  cwal_unhand(varValue);
  return rc;
}

int whcl_eval_buffer( whcl_engine * const el,
                      bool pushScope,
                      char const * scriptName,
                      cwal_buffer const * const buf,
                      cwal_value **rv ){
  return whcl_eval_cstr( el, pushScope, scriptName,
                           buf->mem ? (char const *)buf->mem : "",
                           buf->mem ? (cwal_int_t)buf->used : 0,
                           rv );
}

bool whcl_check_for_return(whcl_engine * const el,
                           int * const rc,
                           cwal_value **rv){
  if(CWAL_RC_RETURN==*rc){
    *rv = cwal_propagating_take(el->ec);
    *rc = 0;
    return true;
  }
  return false;
}

int whcl_eval_file( whcl_engine * const el,
                    bool pushScope,
                    char const * scriptName,
                    cwal_value ** rv ){
  int rc;
  cwal_buffer b = cwal_buffer_empty;

  rc = cwal_buffer_fill_from_filename(el->ec, &b, scriptName);
  if(0==rc){
    rc = whcl_eval_buffer_take(el, pushScope, scriptName, &b, rv);
  }
  cwal_buffer_clear(el->ec, &b);
  return rc;
}


cwal_value * whcl_value_unwrap( cwal_value * const v ){
  return cwal_value_is_unique(v)
    ? cwal_unique_wrapped_get(v)
    : v;
}

cwal_value const * whcl_value_unwrap_c( cwal_value const * const v ){
  return cwal_value_is_unique(v)
    ? cwal_unique_wrapped_get(v)
    : v;
}

void whcl__fatalv( char const * file, int line,
                   char const * func,
                   int code, char const * fmt, va_list vargs ){
  static bool inFatal = false;
  if(inFatal){
    /* This can only happen if the fsl_appendv() bits
       call this AND trigger it via fsl_fprintf() below,
       neither of which is currently the case.
    */
    assert(!"fsl__fatal() called recursively.");
    abort();
  }else{
    inFatal = true;
    fprintf(stderr, "%s():%s:%d FATAL ERROR: code=%d (%s)\n",
            func, file, line, code, cwal_rc_cstr(code));
    if(fmt){
      vfprintf(stderr, fmt, vargs);
      fwrite("\n", 1, 1, stderr);
    }
    exit(EXIT_FAILURE);
  }
}

void whcl__fatal2( char const * file, int line,
                   char const * func,
                   int code, char const * fmt, ... ){
  va_list args;
  va_start(args,fmt);
  whcl__fatalv(file, line, func, code, fmt, args);
  va_end(args);
}

/**
   Internal helper for whcl_lookup_vsym(). Fetches the n'th UTF8
   character from the given str, returning cwal_value_undefined() if n
   is out of range or the input appears to not be UTF8, and NULL on
   allocation error. The value is returned as a new length-1 string.

   TODO: negative values to count from the end, but then we'll
   also need to go patch string.charAt() for that.
*/
static cwal_value * whcl_charat_by_index( whcl_engine * const el,
                                          cwal_string const * const str,
                                          cwal_int_t n ){
  cwal_midsize_t slen = 0;
  unsigned char const * cstr =
    (unsigned char const * )cwal_string_cstr2(str, &slen);
  unsigned int cp = 0;
  assert(cstr);
  if(n < 0 || (cwal_size_t)n >= cwal_string_length_bytes(str)){
    return cwal_value_undefined();
  }else if(cwal_string_is_ascii(str)){
    //cp = cstr[n];
    return cwal_new_string_value(el->ec, (char const *)&cstr[n], 1)
      /* Cannot fail unless the 1-char string optimization is
         removed. */;
  }else if(cwal_utf8_char_at( cstr, cstr + slen, (cwal_size_t)n,
                              &cp )){
    return cwal_value_undefined();
  }
  {
    unsigned char buf[6] = {0,0,0,0,0,0};
    int const clen =
      cwal_utf8_char_to_cstr(cp, buf,(cwal_size_t)sizeof(buf));
    assert(clen<(int)sizeof(buf));
    if(clen<1) return cwal_value_undefined();
    else return cwal_new_string_value(el->ec, (char const *)buf,
                                      (cwal_size_t)clen);
  }
}

#define whcl__cstrv(EL, STR, nSTR)\
  cwal_new_string_value2(EL->ec, STR, nSTR)

#if 0
static cwal_value * whcl__props_can(whcl_engine * const el,
                                    cwal_value * const v){
  return cwal_props_can(v) ? v : cwal_value_prototype_get(el->ec, v);
}
#endif  

cwal_value * whcl__vsym_v_proxy(whcl_engine * const el,
                                cwal_value * self,
                                cwal_value const * const key,
                                cwal_value **foundIn){
  assert(self);
  cwal_value * rv = NULL;
  if(cwal_value_is_integer(key)){
    switch(cwal_value_type_id(self)){
      case CWAL_TYPE_ARRAY: // array[index]
        if(!(rv = cwal_array_get(cwal_value_get_array(self),
                                 (cwal_size_t)cwal_value_get_integer(key)))){
          rv = cwal_value_undefined();
        }
        if(foundIn) *foundIn = self;
        return rv;
      case CWAL_TYPE_TUPLE: // tuple[index]
        if(!(rv = cwal_tuple_get(cwal_value_get_tuple(self),
                                 (uint16_t)cwal_value_get_integer(key)))){
          rv = cwal_value_undefined();
        }
        if(foundIn) *foundIn = self;
        return rv;
      case CWAL_TYPE_STRING: // string[index]
        if(!(rv = whcl_charat_by_index( el, cwal_value_get_string(self),
                                        cwal_value_get_integer(key) ))){
          rv = cwal_value_undefined();
        }
        if(foundIn) *foundIn = self;
        return rv;
      default: break;
    }
  }
  if(whcl__v_is_prototype_string(el, key)){ // $val[__prototype]
    if(!(rv = cwal_value_prototype_get( el->ec, self ))){
      rv = cwal_value_undefined();
    }
    //whcl__dump_val(self,"vsym self");
    //whcl__dump_val((cwal_value*)key,"vsym key");
    //whcl__dump_val(rv,"rv");
    if(foundIn) *foundIn = self;
    return rv;
  }else if(!cwal_props_can(self)){
    if(!(self = cwal_value_prototype_get(el->ec, self))){
      //whcl__dump_val(self,"vsym self");
      //whcl__dump_val((cwal_value*)key,"vsym key");
#if 1
      /* Removing this blocks breaks one of the 000-103-proc.whcl unit
         tests because constructs like:

         decl x; echo x[1]

         trigger this block and set up an exception which the breaking
         tests specifically test for. TODO: figure out which cases
         really care about this and maybe have them check for it
         instead.
      */
      whcl_err_throw(el, CWAL_RC_TYPE,
                     "Value of type '%s' cannot have properties.",
                     cwal_value_type_name(self));
#endif
      return NULL;
    }
  }
  assert(self);
  cwal_kvp const * const kvp =
    cwal_prop_get_kvp_v(self, key, true, foundIn);
  cwal_value * v = kvp ? cwal_kvp_value(kvp) : NULL;
  for( cwal_propref * const p =
         v ? cwal_value_get_propref(v) : NULL; p; ){
    //whcl__dump_val((cwal_value*)key, "got matching propref");
    v = NULL;
    cwal_propref_resolve(p, self /*???*foundIn???*/, &v);
    if(!v && !whcl_err_has(el,true)) v = cwal_value_undefined();
    //whcl__dump_val(v, "resolved?");
    break;
  }
  return v;
}

cwal_value * whcl_var_search_v(whcl_engine * const el,
                               whcl_scope * sc,
                               bool searchParents,
                               cwal_value const * const key,
                               whcl_scope **foundIn){
  if(NULL==sc) sc = el->scopes.current;
  uint16_t level = sc->level;
  assert(level);
  while(true){
#if 0
    whcl__dump_val((cwal_value*)key, "var-search key");
    if(el->ct){
      whcl_stoken const * const t = whcl__script_at(el->ct, el->ct->token);
      whcl__dump_stok(el->ct, t, "var search @");
    }
#endif
    cwal_kvp const * const kvp = sc->props
      ? cwal_prop_get_kvp_v(sc->props, key, false, NULL)
      : NULL;
    if(kvp){
      cwal_value * v = cwal_kvp_value(kvp);
      for( cwal_propref * const p =
             v ? cwal_value_get_propref(v) : NULL; p; ){
        //whcl__dump_val((cwal_value*)key, "got matching propref");
        v = NULL;
        cwal_propref_resolve(p, sc->props, &v);
        //whcl__dump_val(v2, "resolved?");
        if(!v && !whcl_err_has(el,true)){
          /* We _really_ want to return NULL here, as opposed to
             `undefined`, for scopes but proprefs leave us in an
             uncomfortable limbo for this case. If we return NULL,
             then the current lookup will fail with (semantically)
             "not found," and the user will be told that this call's
             key does not resolve, which is a fib. Or half-fib. The
             precise fraction of fib is debatable, but it's not 100%
             truthful and it causes downstream confusion. Returning
             undefined for this case, like we (necessarily) do for
             proprefs which point to non-scope object/array entries,
             is a compromise until/unless a better solution can be
             found which allows us to return NULL from here without
             causing too much downstream havok. */
          v = cwal_value_undefined();
        }
        break;
      }
      if(foundIn) *foundIn = sc;
      return v;
    }else if(whcl_err_has(el, true)){
      /* Can happen when propref resolution triggers an error. */
      return NULL;
    }
    if(!searchParents || 1==level) break;
    else if(WHCL__SCOPE_F_CALL & sc->flags
            && 0==(WHCL__SCOPE_F_XSYM & sc->flags)){
      level = 1;
    }else{
      --level;
    }
    sc = whcl__scope_for_level(el, level);
  }
  return NULL;
}

int whcl_lookup_vsym_v(whcl_engine * const el,
                       cwal_value * self,
                       cwal_value const * const key,
                       bool failIfNotFound,
                       cwal_value **rv){
  int rc = 0;
  cwal_value * v = NULL;
  cwal_engine_error_reset(el->ec);
  assert(key);
  if(self){
    //whcl_err_reset(el);
    v = whcl__vsym_v_proxy(el, self, key, NULL);
  }else{/* scope property lookup */
    v = whcl_var_search_v(el, el->scopes.current, true, key, NULL);
  }
#if 1
  /* Removing this block breaks one of the 000-103-proc.whcl unit
     tests. See whcl__vsym_v_proxy() for details. */
  if(!v && (rc = whcl_err_has(el, true))){
    /* Catch errors from recursion involving CWAL_TYPE_PROPREF
       values */;
    return rc;
  }
#endif
  if(v || !failIfNotFound){
    //whcl_err_reset(el);
    rc = 0;
    if(rv) *rv = v;
  }else if(0==rc){
    cwal_size_t nV = 0;
    char const * cstr = cwal_value_get_cstr(key, &nV);
    //whcl__dump_val((cwal_value*)key, "unresolved key");
    //whcl__dump_stok(el->ct, whcl__script_errtoken_get(el->ct), "errtoken?");
    rc = cstr
      ? whcl_err_set(el, CWAL_RC_NOT_FOUND,
                       "Cannot resolve symbol '%.*s'.",
                       (int)nV, cstr)
      : whcl_err_set(el, CWAL_RC_NOT_FOUND,
                     "Cannot resolve symbol.");
  }
  return rc;
}

int whcl_lookup_vsym(whcl_engine * const el,
                     cwal_value * const self,
                     char const * key,
                     cwal_int_t keyLen,
                     bool failIfNotFound,
                     cwal_value **rv){
  cwal_value * const vk = whcl__cstrv(el, key, keyLen);
  if(!vk){
    WHCL__WARN_OOM;
    return CWAL_RC_OOM;
  }
  cwal_ref(vk);
  int const rc = whcl_lookup_vsym_v(el, self, vk, failIfNotFound, rv);
  if(0==rc && rv && *rv==vk/*via string interning*/) cwal_ref(*rv);
  cwal_unref(vk);
  if(0==rc && rv && *rv==vk) cwal_unhand(*rv);
  return rc;
}

cwal_value * whcl_get_v(whcl_engine * const el,
                        cwal_value * self,
                        cwal_value const * const key){
  cwal_value * v = NULL;
  whcl_lookup_vsym_v(el, self, key, false, &v);
  whcl_err_reset(el);
  return v;
}

cwal_value * whcl_get(whcl_engine * const el,
                      cwal_value * self,
                      char const * const key,
                      cwal_int_t keyLen){
  cwal_value * v = NULL;
  whcl_lookup_vsym(el, self, key, keyLen, false, &v);
  whcl_err_reset(el);
  return v;
}

cwal_value * whcl_var_search(whcl_engine * const el,
                             whcl_scope * sc,
                             bool searchParents,
                             char const * key,
                             cwal_int_t keyLen,
                             whcl_scope ** foundIn ){
  cwal_value * const vk = whcl__cstrv(el, key, keyLen);
  if(!vk) return NULL;
  cwal_ref(vk);
  cwal_value * rv = whcl_var_search_v(el, sc, searchParents, vk, foundIn);
  cwal_unref(vk);
  return rv;
}

/**
   Interal impl for whcl_scope_set_with_flags_v() and friends,
   differing from that routine in that it directly sets the key in the
   scope properties of the given scope (which must not be NULL),
   without explicitly checking whether that property exists
   first. This function may return any codes documented for
   cwal_prop_set_with_flags_v() except for CWAL_RC_NOT_FOUND, which
   this function treats as a non-error.

   Note that non-0 codes returned here are "unadorned" - the
   cwal_engine's error state is not updated from here. That behavior
   may change in the future.
*/
static int whcl__scope_set_with_flags_v(whcl_engine * const el,
                                        whcl_scope * const scope,
                                        cwal_value * const key, cwal_value * const v,
                                        cwal_flags16_t propertyFlags){
  cwal_value * const p = whcl_scope_props(el, scope);
  if(!p) {WHCL__WARN_OOM; return CWAL_RC_OOM;}
  int const rc = cwal_prop_set_with_flags_v(p, key, v, propertyFlags);
  switch(rc){
#if 0
    case 0:
      if(v && !cwal_prop_get_v(p, key)){
        whcl__dump_val(key,"var-set key, cannot read back");
        whcl__dump_val(v,"var-set value");
        if(el->ct){
          whcl__dump_stok(el->ct, whcl__script_at(el->ct, el->ct->token),
                          "Current script token.");
        }
        assert(whcl_get_v(el, p, key));
        assert(cwal_prop_get_v(p, key));
      }
      return rc;
#endif
    case CWAL_RC_NOT_FOUND: assert(!v); return 0;
    default: return rc;
  }
}

int whcl_scope_set_with_flags_v(whcl_engine * const el,
                                whcl_scope * scope,
                                bool searchParents,
                                cwal_value * const key,
                                cwal_value * const v,
                                uint16_t propertyFlags){
  if(!scope){
    scope = el->scopes.current;
    assert(scope);
  }
  int rc = 0;
  whcl_scope * foundIn = NULL;
  cwal_engine_error_reset(el->ec)
    /* Reminder to self: we cannot safely call whcl_err_reset() from
       here because doing so might indirectly destroy an exception
       object upon which we're setting a property. We can (and needed
       to), however, reset the non-exception error state because
       getting a vare can trigger an error via property alias
       resolution. We reset the error state so that (A) we can check
       for that case after getting a NULL back from the search and (B)
       so that we don't pick up a stale error report reported by
       earlier code. */;
  if(!whcl_var_search_v(el, scope, searchParents, key, &foundIn)){
    if(!v){
      rc = whcl_err_has(el, true);
      return rc ? rc
        : whcl_err_set(el, CWAL_RC_NOT_FOUND,
                       "Cannot set a non-declared variable.");
    }
  }
  rc = whcl__scope_set_with_flags_v(el, foundIn ? foundIn : scope, key,
                                    v, propertyFlags);
  switch(rc){
    case 0: case CWAL_RC_OOM: break;
    default:
      if(!whcl_err_has(el, true)){
        rc = whcl_err_set(el, rc, "Error %s setting var in scope #%d.",
                          cwal_rc_cstr(rc), (int)scope->level);
      }
      break;
  }
  return rc;
}


int whcl_scope_set_v(whcl_engine * const el,
                     whcl_scope * scope,
                     bool searchParents,
                     cwal_value * const key,
                     cwal_value * const v){
  return whcl_scope_set_with_flags_v(el, scope, searchParents,
                                     key, v, 0);
}

int whcl_set_with_flags_v(whcl_engine * const el,
                          cwal_value * self,
                          cwal_value * const key,
                          cwal_value * v,
                          uint16_t propertyFlags){
  int rc;
  if(self){
    if(cwal_value_is_integer(key)){
      cwal_int_t const n = cwal_value_get_integer(key);
      switch((n>=0) ? cwal_value_type_id(self) : CWAL_TYPE_UNDEF){
        case CWAL_TYPE_ARRAY:
          rc = cwal_array_set(cwal_value_get_array(self),
                              (cwal_size_t)n, v);
          goto end;
        case CWAL_TYPE_TUPLE:
          rc = cwal_tuple_set(cwal_value_get_tuple(self),
                              (uint16_t)n, v);
          goto end;
        default:
          break;
      }
    }
    if(!cwal_props_can(self)){
      return whcl_err_throw(el, CWAL_RC_TYPE,
                            "Cannot %s properties %s value of type %s.",
                            v ? "set" : "unset",
                            v ? "on" : "from",
                            cwal_value_type_name(self));
    }
    if(whcl__v_is_prototype_string(el,key)){
      if(v && (v==cwal_value_undefined()
               || v==cwal_value_null())){
        v = NULL;
      }
      rc = cwal_value_prototype_set(self, v);
    }else{
      rc = cwal_prop_set_with_flags_v(self, key,
                                      v, propertyFlags);
      switch(rc){
        case 0: break;
        case CWAL_RC_NOT_FOUND:
          if(!v) rc = 0;
          break;
          /* TODO: fill out meaningful error strings here and
             use the same mechanism with whcl_scope_set_with_flags_v(). */
      }
    }
  }else{/*self==NULL, so scope-level set*/
    rc = whcl_scope_set_with_flags_v(el, el->scopes.current,
                                     true, key, v, propertyFlags);
  }
  end:
  if(rc){
    cwal_size_t nStr = 0;
    char const * str = cwal_value_get_cstr(key, &nStr);
    if(str) rc = whcl_err_throw(el, rc, "Error %s setting %s '%.*s'.",
                                cwal_rc_cstr(rc),
                                self ? "propery" : "variable",
                                (int)nStr, str);
    else{
      str = cwal_value_type_name2(key, &nStr);
      rc = whcl_err_throw(el, rc,
                          "Error %s setting %s of type '%.*s'.",
                          cwal_rc_cstr(rc), self ? "propery" : "variable",
                          (int)nStr, str);
    }
  }
  return rc;
}

int whcl_set_with_flags(whcl_engine * const el,
                        cwal_value * self,
                        char const * key,
                        cwal_int_t keyLen,
                        cwal_value * const v,
                        uint16_t propertyFlags){
  cwal_value * const vk = whcl__cstrv(el, key, keyLen);
  if(!vk){
    WHCL__WARN_OOM;
    return CWAL_RC_OOM;
  }
  cwal_ref(vk);
  int const rc = whcl_set_with_flags_v(el, self, vk, v, propertyFlags);
  cwal_unref(vk);
  return rc;
}


int whcl_set_v(whcl_engine * const el,
               cwal_value * self,
               cwal_value * const key,
               cwal_value * const v){
  return whcl_set_with_flags_v(el, self, key, v, 0);
}

int whcl_set(whcl_engine * const el, cwal_value * self,
             char const * key,
             cwal_int_t keyLen, cwal_value * const v){
  cwal_value * const vk = whcl__cstrv(el, key, keyLen);
  if(!vk){
    WHCL__WARN_OOM;
    return CWAL_RC_OOM;
  }
  cwal_ref(vk);
  int const rc = whcl_set_v(el, self, vk, v);
  cwal_unref(vk);
  return rc;
}

int whcl_var_decl(whcl_engine * const el,
                  whcl_scope * const scope,
                  bool isConst,
                  char const * key,
                  cwal_int_t keyLen,
                  cwal_value * const v){
  cwal_value * const vk = whcl__cstrv(el, key, keyLen);
  if(!vk){
    WHCL__WARN_OOM;
    return CWAL_RC_OOM;
  }
  cwal_ref(vk);
  int const rc = whcl_var_decl_v(el, scope, isConst, vk, v);
  cwal_unref(vk);
  return rc;
}

int whcl_var_decl_v(whcl_engine * const el,
                    whcl_scope * const scope,
                    bool isConst,
                    cwal_value * const key,
                    cwal_value * const v){
  if(isConst && !v){
    return whcl_err_throw(el, CWAL_RC_MISUSE,
                          "Declaration of a const must "
                          "include a value.");
  }
  int rc;
  whcl_scope * foundIn = NULL;
  cwal_value * const fv = whcl_var_search_v(el, scope, false, key, &foundIn);
  if(0){
    whcl__dump_val(key,"decl key");
    whcl__dump_val(v,"decl val");
    whcl__dump_val(fv,"decl search");
  }
  if(fv){
    rc = whcl_err_set(el, CWAL_RC_ALREADY_EXISTS,
                      "Variable '%s' is already declared "
                      "in this scope.",
                      cwal_value_get_cstr(key, NULL));
  }else{
    //whcl__dump_val(key, "var-set key");
    //whcl__dump_val(v, "var-set v");
    //if(v) {MARKER(("var val type id=%s\n", cwal_type_id_name(cwal_value_type_id(v))));}
    rc = whcl__scope_set_with_flags_v(el, scope ? scope : el->scopes.current,
                                      key, v ? v : cwal_value_undefined(),
                                      isConst ? CWAL_VAR_F_CONST : 0);

    if(rc) rc = whcl_err_set(el, rc, "Error %s setting var '%s.",
                             cwal_rc_cstr(rc),
                             cwal_value_get_cstr(key, NULL));
  }
  return rc;
}


/*#define whcl__hash_token( TOKER, TOKEN )                              \
  whcl__hash_keyword(whcl_stoken_begin(TOKER, TOKEN), whcl_stoken_len(PTOKEN))
*/

/**
   Internal, recursive impl of whcl_dump_tokens(). `level` must
   initially be 0. `counter` must initially be NULL and recursive
   calls set it to a shared counter. `innerType` must initially be
   0. Recursive calls set it to 1 for tokens recursing via
   token->innerId and -1 for tokens recursing via token->subscriptId.
*/
static void whcl__dump_tokens(whcl_engine * const el,
                              whcl_script * const ct,
                              uint32_t flags,
                              unsigned int level,
                              uint32_t * counter,
                              short innerType){
  whcl_stoken const * const oldPos = whcl__script_token(ct);
  whcl_stoken const * ctok = 0;
  uint32_t tCount = 0;
  int rc = 0;
  char numTtype2[100] = {0};
  if(!level){
    assert(!counter);
    counter = &tCount;
    //cwal_outputf(el->ec, "whcl_script->chain:\n");
    if( !(WHCL_DUMP_TOKENS_NO_REWIND & flags) ){
      whcl__script_rewind(ct);
    }
  }
  if(!(ctok = whcl__script_token(ct))){
    whcl__script_next_token2(ct, &ctok);
  }
  for( ; ctok; whcl__script_next_token2(ct, &ctok)){
    ++*counter;
    assert(ctok);
    if(!ctok->id) break;
    //whcl__dump_stok(ct, ctok, "dumping...");
    if(0==level
       && (WHCL_DUMP_TOKENS_TO_EOX & flags)
       && whcl_t_is_eox(ctok->ttype)){
      break;
    }
    char const * zTtype2 = "";
    if(ctok->ttype2){
      numTtype2[0] = 0;
      zTtype2 = tok1_t_cstr(ctok->ttype2);
      if(!zTtype2){
        snprintf(numTtype2, sizeof(numTtype2)-1, " (#%d)", (int)ctok->ttype2);
      }else{
        snprintf(numTtype2, sizeof(numTtype2)-1, " (#%s)", zTtype2);
      }
      zTtype2 = numTtype2;
    }
    if((WHCL_DUMP_TOKENS_EOFS & flags)
       || !whcl_stoken_is_eof(ctok)){
      cwal_outputf(el->ec,
                   "%*s%s#%d: %s%s@ %d, %d: len %d ==> #%d",
                   level*2, "",
                   innerType<0 ? "s" : (innerType>0 ? "i" : ""),
                   //             ^ ctok->subscriptId  ^ ctok->innerId
                   (int)ctok->id,
                   tok1_t_cstr(ctok->ttype),
                   zTtype2,
                   (int)ctok->line, (int)ctok->column,
                   (int)whcl_stoken_len(ctok),
                   (int)ctok->nextId);
      switch((flags & WHCL_DUMP_TOKENS_VERBOSE) ? ctok->ttype : 0){
        case 0:
        case TOK1_T_EOL:
        case TOK1_T_EOF:
        case TOK1_T_Blank:
        case TOK1_T_CommentC:
        case TOK1_T_CommentCpp:
        case TOK1_T_CommentTCL:
        case TOK1_T_Whitespace:
          break;
        default: {
          cwal_midsize_t n = 0;
          char const * s =
            whcl_stoken_cstr(ct, ctok, &n, false);
          cwal_outputf(el->ec, " %.*s", (int)n, s);
          break;
        }
      }
      cwal_output(el->ec, "\n", 1);
    }
    if(TOK1_T_EOF==ctok->ttype) break;
    for(short i = 0; i < 2; ++i ){
      whcl_stoken_id const inner =
        0==i ? ctok->innerId : ctok->subscriptId;
      if(inner && inner!=ctok->id){
        whcl_script sub = whcl__script_empty;
        rc = whcl__script_sub_from_group(ct, &sub, 0==i);
        if(0==rc){
          whcl__dump_tokens(el, &sub, (flags & ~WHCL_DUMP_TOKENS_TO_EOX)
                            | WHCL_DUMP_TOKENS_NO_REWIND,
                            level + 1, counter, 0==i ? 1 : -1);
        }
        whcl__script_finalize(&sub);
        if(rc) break;
      }
    }
  }
  if(rc){
    char const * errMsg = 0;
    assert(!"whcl__script_next_token2() \"cannot fail.\"");
    rc = whcl__script_err_get(ct, &errMsg, NULL);
    whcl__fatal(rc, "Impossible(?) error via whcl__script_next_token2(): "
                "%d (%s): %s", rc, cwal_rc_cstr(rc), errMsg);
  }
  if(0==level){
    if(0==rc
       && (WHCL_DUMP_TOKENS_METRICS & flags)
       && tCount){
      cwal_outputf(el->ec, "\nTotal reachable token count: %"PRIu32
                   " (%"PRIu32" whcl_stoken bytes)\n",
                   tCount, (uint32_t)(tCount * sizeof(whcl_stoken)));
      cwal_outputf(el->ec, "Tokenizer total token count: %"PRIu32
                   " (%"PRIu32" whcl_stoken bytes)\n",
                   ct->chainLength,
                   (uint32_t)(ct->chainLength * sizeof(whcl_stoken)));
      if(tCount>ct->chainLength){
        cwal_outputf(el->ec, "Note that virtual EOF tokens may get "
                     "re-used, accounting for a higher-than-expected "
                     "reachable token count.\n");
      }
    }
    whcl__stoken_set(ct, oldPos);
  }
}

void whcl_dump_tokens(whcl_engine * const el,
                       whcl_script * const ct,
                       uint32_t flags){
  whcl__dump_tokens(el, ct, flags, 0, NULL, 0);
}

int whcl_install_callback( whcl_engine * const el,
                           cwal_value * const tgt,
                           cwal_callback_f callback,
                           char const * const name,
                           cwal_int_t nameLen, 
                           uint16_t propertyFlags,
                           void * const state,
                           cwal_finalizer_f const stateDtor,
                           void const * const stateTypeID ){
  int rc = 0;
  cwal_value * kv;
  cwal_value * fv;
  if(!callback) return CWAL_RC_MISUSE;
  else if(tgt && !cwal_props_can(tgt)) return CWAL_RC_TYPE;
  if(nameLen<0) nameLen = (cwal_int_t)cwal_strlen(name);
  if(!nameLen) return CWAL_RC_RANGE;
  kv = cwal_new_string_value(el->ec, name, (cwal_size_t)nameLen);
  fv = kv ? cwal_new_function_value(el->ec, callback, state,
                                    stateDtor,
                                    stateTypeID)
    : NULL;
  if(fv){
    cwal_ref(fv); cwal_ref(kv);
    rc = whcl_set_with_flags_v(el, tgt, kv, fv, propertyFlags);
    cwal_unref(kv); cwal_unref(fv);
  }else{
    cwal_refunref(kv);
    WHCL__WARN_OOM;
    rc = CWAL_RC_OOM;
  }
  return rc;
}

int whcl_install_functions( whcl_engine * const el,
                            cwal_value * const tgt,
                            whcl_func_def const * defs,
                            uint16_t propertyFlags ){
  int rc = 0;
  if(!defs) return CWAL_RC_MISUSE;
  else if(tgt && !cwal_props_can(tgt)) return CWAL_RC_TYPE;
  for( ; !rc && defs->name; ++defs ){
    cwal_value * const key =
      cwal_new_string_value(el->ec, defs->name, cwal_strlen(defs->name));
    cwal_value * const fv = key
      ? cwal_new_function_value( el->ec, defs->callback,
                                 defs->state,
                                 defs->stateDtor,
                                 defs->stateTypeID)
      : NULL;
    if(fv){
      cwal_ref(key); cwal_ref(fv);
      rc = whcl_set_with_flags_v(el, tgt, key, fv, propertyFlags);
      if(!rc && defs->cwalContainerFlags){
        cwal_container_client_flags_set( fv, defs->cwalContainerFlags );
        //MARKER(("test container func=%s@%p flags=0x%04x\n", defs->name,
        //        (void*)fv, cwal_container_client_flags_get(fv)));
      }
      cwal_unref(fv); cwal_unref(key);
    }else{
      cwal_refunref(key);
      WHCL__WARN_OOM;
      rc = CWAL_RC_OOM;
    }
  }
  return rc;
}

int whcl_function_forward( whcl_engine * const el,
                           cwal_function * const f,
                           uint16_t trimArgCount,
                           cwal_callback_args const * args,
                           cwal_value ** rv ){
  if(0==(el->scopes.current->flags & WHCL__SCOPE_F_XSYM)){
    cwal_flags16_t const cf =
      cwal_container_client_flags_get(cwal_function_value(f));
    if(WHCL_CONTAINER_F_XSYM & cf){
      el->scopes.current->flags |= WHCL__SCOPE_F_XSYM;
    }
  }
  return cwal_function_forward(f, trimArgCount, args, rv);
}

int whcl_function_call( cwal_function * f,
                        cwal_value * self,
                        cwal_flags16_t callFlags,
                        cwal_value ** resultVal,
                        uint16_t argc,
                        cwal_value * const * argv ){
  cwal_value * const fv = cwal_function_value(f);
  cwal_engine * const ec = cwal_value_engine(fv);
  whcl_engine * const el = whcl_engine_from_state(ec);
  assert(el);
  el->scopes.nextFlags = WHCL__SCOPE_F_CALL;
  //MARKER(("function cflags=0x%04x\n",
  //        cwal_container_client_flags_get(fv)));
  if((WHCL_FCALL_F_XSYM & callFlags) ||
     (WHCL_CONTAINER_F_XSYM & cwal_container_client_flags_get(fv))){
    el->scopes.nextFlags |= WHCL__SCOPE_F_XSYM;
  }
  if(WHCL_FCALL_F_REUSE_WSCOPE & callFlags){
    el->scopes.nextFlags |= WHCL__SCOPE_F_REUSE;
  }
  //MARKER(("el->scopes.nextFlags = 0x%04x\n", (int)el->scopes.nextFlags));
  int const rc = cwal_function_call(f, self, resultVal, argc, argv);
  el->scopes.nextFlags = 0;
  return rc;
}


int whcl__cb_command( cwal_callback_args const * args, cwal_value **rv ){
  if(!args->argc){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                         "Expecting: subcommand [args...]");
  }
  cwal_size_t nName = 0;
  char const * subName = 0;
  cwal_value * const arg = args->argv[0];
  cwal_value * vsub = NULL;
  whcl_engine * const el = whcl_engine_from_args(args);
  assert(el);
  subName = cwal_value_get_cstr(arg, &nName);
  whcl_lookup_vsym_v(el, args->self, arg, false, &vsub);
  if(!vsub) {
    //whcl__dump_val(args->self, "self");
    return subName
      ? cwal_cb_throw(args, CWAL_RC_NOT_FOUND,
                      "Cannot find subcommand method '%.*s'.",
                      (int)nName, subName)
      : cwal_cb_throw(args, CWAL_RC_NOT_FOUND,
                      "Cannot find subcommand method (non-string name).");
  }
  cwal_function * const f = cwal_value_get_function(vsub);
  if(!f){
    return subName
      ? cwal_cb_throw(args, CWAL_RC_NOT_FOUND,
                      "Expecting '%.*s' to be a subcommand method.",
                      (int)nName, subName)
      : cwal_cb_throw(args, CWAL_RC_NOT_FOUND,
                      "Expecting property to be a subcommand method.");
  }else if(args->callee == f){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                         "Throwing to avoid an infinite loop in "
                         "subcommand dispatching. Do not try to "
                         "directly invoke the command dispatcher method "
                         "(%.*s).", (int)nName, subName);
  }
  return whcl_function_forward(el, f, 1, args, rv);
}

int whcl_install_command_cb2(whcl_engine * const el, cwal_callback_f cb,
                             cwal_value * const tgt){
  cwal_value * const fv =
    cwal_new_function_value(el->ec, cb, NULL, NULL, NULL);
  if(!fv){
    WHCL__WARN_OOM;
    return CWAL_RC_OOM;
  }
  cwal_ref(fv);
  int rc = whcl_set_with_flags_v(el, tgt, el->cache.keyCommand,
                                 fv,
                                 CWAL_VAR_F_CONST | CWAL_VAR_F_HIDDEN);
  cwal_unref(fv);
  return rc;
}

int whcl_install_command_cb(whcl_engine * const el, cwal_value * const tgt){
  
  if(!el->cache.commandCb){
    el->cache.commandCb = cwal_new_function_value(el->ec, whcl__cb_command,
                                                  NULL, NULL, NULL);
    if(!el->cache.commandCb){
      WHCL__WARN_OOM;
      return CWAL_RC_OOM;
    }
    cwal_value_prototype_set(el->cache.commandCb, NULL);
    cwal_ref(el->cache.commandCb);
    int const rc = whcl_stash_set_v(el, el->cache.commandCb,
                                    el->cache.commandCb);
    cwal_unref(el->cache.commandCb);
    if(rc){
      el->cache.commandCb = NULL;
      return rc;
    }
  }
  return whcl_set_with_flags_v(el, tgt, el->cache.keyCommand,
                               el->cache.commandCb,
                               CWAL_VAR_F_CONST | CWAL_VAR_F_HIDDEN);
}


#if WHCL_USE_SIGNALS
/**
   whcl_engine interruption via signals...

   Potential TODO: instead of manipulating an whcl_engine instance
   directly from the signal handler, which has potential
   timing-related issues vis-a-vis whcl_engine lifetime, simply set a
   global flag here and check/consume it from the whcl_engine APIs. The
   disadvantage to that is that we get less precise interruption
   location information (limited to where an engine explicitly checks
   it), but that seems like a small price to pay for "more correct"
   whcl_engine lifetime interaction.
*/
static void whcl_sigc_handler(int s){
  if(whclInterruptable && !whclInterruptable->flags.interrupted){
    whcl_engine * const el = whclInterruptable;
    whcl_script const * ct = el ? el->ct : NULL;
    whclInterruptable = NULL /* disable concurrent interruption */;
    if(ct){
      cwal_error * const err = cwal_engine_error(el->ec);
      MARKER(("Interruping via signal handler!\n"));
      /* fake an error pos which whcl_err_toker() will see... */
      //whcl__script_errtoken_set(ct, whcl__script_token(ct));
      whcl_err_set( el, CWAL_RC_INTERRUPTED,
                    "Interrupted by signal #%d.", s);
      if(err->line>0){
        /* We do this to help track down locations where interruption is
           triggered but does not preempt the normal result code of the
           interrupted operation like it should.
        */
        MARKER(("Interrupt was at line %d, column %d of '%.*s'.\n",
                err->line, err->col,
                (int)err->script.used, (char const *)err->script.mem));
      }
    }else if(el){
      whcl_err_set(el, CWAL_RC_INTERRUPTED,
                   "Interrupted by signal #%d.", s);
    }
    if(el){
      el->flags.interrupted = CWAL_RC_INTERRUPTED;
    }
    assert(!whclInterruptable);
    if(NULL==whclInterruptable) whclInterruptable = el /* re-enable interruption */;
  }
}
#endif
/* WHCL_USE_SIGNALS */

void whcl_set_interrupt_handlable( whcl_engine * const el ){
#if WHCL_USE_SIGNALS
  if((whclInterruptable = el)){
    struct sigaction sigIntHandler;
    sigIntHandler.sa_handler = whcl_sigc_handler;
    sigemptyset(&sigIntHandler.sa_mask);
    sigIntHandler.sa_flags = 0;
    sigaction(SIGINT, &sigIntHandler, NULL);
  }
#else
  if(el){/*unused*/}
#endif
}

int whcl_interrupt( whcl_engine * const el ){
  whcl_err_set(el, CWAL_RC_INTERRUPTED,
               "Interrupted by %s().", __func__);
  return el->flags.interrupted = CWAL_RC_INTERRUPTED;
}


bool whcl_cstr_to_rc(char const *str, cwal_int_t len, int * code){
  cwal_size_t const n = (len<0) ? cwal_strlen(str) : (cwal_size_t)len;
  switch(whcl__hash_keyword(str, len)){
#define THEN(RC,STR) if((cwal_size_t)sizeof(STR)-1 == n            \
                     && 0==cwal_compare_cstr(str, n, STR, n))   \
    { *code = RC; return true; } else return false
    
/* Values generated by kwasher.c */
    case 0x0493e96d: THEN(CWAL_RC_OK,"CWAL_RC_OK");
    case 0x00056345: THEN(CWAL_RC_OK,"OK");
    case 0x249f6076: THEN(CWAL_RC_ERROR,"CWAL_RC_ERROR");
    case 0x0025b576: THEN(CWAL_RC_ERROR,"ERROR");
    case 0x0927db88: THEN(CWAL_RC_OOM,"CWAL_RC_OOM");
    case 0x000ac800: THEN(CWAL_RC_OOM,"OOM");
    case 0x249f247a: THEN(CWAL_RC_FATAL,"CWAL_RC_FATAL");
    case 0x0026374a: THEN(CWAL_RC_FATAL,"FATAL");
    case 0x493e9a87: THEN(CWAL_RC_CONTINUE,"CWAL_RC_CONTINUE");
    case 0x0124f4bd: THEN(CWAL_RC_CONTINUE,"CONTINUE");
    case 0x249f28c6: THEN(CWAL_RC_BREAK,"CWAL_RC_BREAK");
    case 0x0024094e: THEN(CWAL_RC_BREAK,"BREAK");
    case 0x493f1986: THEN(CWAL_RC_RETURN,"CWAL_RC_RETURN");
    case 0x005997b6: THEN(CWAL_RC_RETURN,"RETURN");
    case 0x124faf80: THEN(CWAL_RC_EXIT,"CWAL_RC_EXIT");
    case 0x0012d950: THEN(CWAL_RC_EXIT,"EXIT");
    case 0x493eb50a: THEN(CWAL_RC_EXCEPTION,"CWAL_RC_EXCEPTION");
    case 0x025b309e: THEN(CWAL_RC_EXCEPTION,"EXCEPTION");
    case 0x493e96d0: THEN(CWAL_RC_ASSERT,"CWAL_RC_ASSERT");
    case 0x00470e70: THEN(CWAL_RC_ASSERT,"ASSERT");
    case 0x493ef3d7: THEN(CWAL_RC_MISUSE,"CWAL_RC_MISUSE");
    case 0x005423ef: THEN(CWAL_RC_MISUSE,"MISUSE");
    case 0x493f3eb1: THEN(CWAL_RC_NOT_FOUND,"CWAL_RC_NOT_FOUND");
    case 0x02aa361c: THEN(CWAL_RC_NOT_FOUND,"NOT_FOUND");
    case 0x493e85fc: THEN(CWAL_RC_ALREADY_EXISTS,"CWAL_RC_ALREADY_EXISTS");
    case 0x47066359: THEN(CWAL_RC_ALREADY_EXISTS,"ALREADY_EXISTS");
    case 0x249f5e3a: THEN(CWAL_RC_RANGE,"CWAL_RC_RANGE");
    case 0x002cc112: THEN(CWAL_RC_RANGE,"RANGE");
    case 0x124fd939: THEN(CWAL_RC_TYPE,"CWAL_RC_TYPE");
    case 0x0016eff1: THEN(CWAL_RC_TYPE,"TYPE");
    case 0x493f8263: THEN(CWAL_RC_UNSUPPORTED,"CWAL_RC_UNSUPPORTED");
    case 0x0b9ce0c6: THEN(CWAL_RC_UNSUPPORTED,"UNSUPPORTED");
    case 0x493dfd49: THEN(CWAL_RC_ACCESS,"CWAL_RC_ACCESS");
    case 0x0046f891: THEN(CWAL_RC_ACCESS,"ACCESS");
    case 0x493f426e: THEN(CWAL_RC_IS_VISITING,"CWAL_RC_IS_VISITING");
    case 0x09fb3416: THEN(CWAL_RC_IS_VISITING,"IS_VISITING");
    case 0x493f6b29: THEN(CWAL_RC_IS_VISITING_LIST,"CWAL_RC_IS_VISITING_LIST");
    case 0x4fd9f8dc: THEN(CWAL_RC_IS_VISITING_LIST,"IS_VISITING_LIST");
    case 0x493ee88d: THEN(CWAL_RC_DISALLOW_NEW_PROPERTIES,"CWAL_RC_DISALLOW_NEW_PROPERTIES");
    case 0x4a4d62c0: THEN(CWAL_RC_DISALLOW_NEW_PROPERTIES,"DISALLOW_NEW_PROPERTIES");
    case 0x493ebbd8: THEN(CWAL_RC_DISALLOW_PROP_SET,"CWAL_RC_DISALLOW_PROP_SET");
    case 0x4a4d6400: THEN(CWAL_RC_DISALLOW_PROP_SET,"DISALLOW_PROP_SET");
    case 0x493eed20: THEN(CWAL_RC_DISALLOW_PROTOTYPE_SET,"CWAL_RC_DISALLOW_PROTOTYPE_SET");
    case 0x4a4d854c: THEN(CWAL_RC_DISALLOW_PROTOTYPE_SET,"DISALLOW_PROTOTYPE_SET");
    case 0x493ed268: THEN(CWAL_RC_CONST_VIOLATION,"CWAL_RC_CONST_VIOLATION");
    case 0x4945b69f: THEN(CWAL_RC_CONST_VIOLATION,"CONST_VIOLATION");
    case 0x493eb510: THEN(CWAL_RC_LOCKED,"CWAL_RC_LOCKED");
    case 0x0052fba0: THEN(CWAL_RC_LOCKED,"LOCKED");
    case 0x493ebdbd: THEN(CWAL_RC_CYCLES_DETECTED,"CWAL_RC_CYCLES_DETECTED");
    case 0x493a2b3b: THEN(CWAL_RC_CYCLES_DETECTED,"CYCLES_DETECTED");
    case 0x493ecee7: THEN(CWAL_RC_DESTRUCTION_RUNNING,"CWAL_RC_DESTRUCTION_RUNNING");
    case 0x4a55c37a: THEN(CWAL_RC_DESTRUCTION_RUNNING,"DESTRUCTION_RUNNING");
    case 0x493e75a9: THEN(CWAL_RC_FINALIZED,"CWAL_RC_FINALIZED");
    case 0x0263ae4a: THEN(CWAL_RC_FINALIZED,"FINALIZED");
    case 0x493eb9ff: THEN(CWAL_RC_HAS_REFERENCES,"CWAL_RC_HAS_REFERENCES");
    case 0x4eb14fc1: THEN(CWAL_RC_HAS_REFERENCES,"HAS_REFERENCES");
    case 0x493ee7ea: THEN(CWAL_RC_INTERRUPTED,"CWAL_RC_INTERRUPTED");
    case 0x09f94676: THEN(CWAL_RC_INTERRUPTED,"INTERRUPTED");
    case 0x493e1d30: THEN(CWAL_RC_CANCELLED,"CWAL_RC_CANCELLED");
    case 0x02494002: THEN(CWAL_RC_CANCELLED,"CANCELLED");
    case 0x0493e6d9: THEN(CWAL_RC_IO,"CWAL_RC_IO");
    case 0x0004fab9: THEN(CWAL_RC_IO,"IO");
    case 0x493e5a81: THEN(CWAL_RC_CANNOT_HAPPEN,"CWAL_RC_CANNOT_HAPPEN");
    case 0x249b08f2: THEN(CWAL_RC_CANNOT_HAPPEN,"CANNOT_HAPPEN");
    case 0x493f35b5: THEN(CWAL_RC_JSON_INVALID_CHAR,"CWAL_RC_JSON_INVALID_CHAR");
    case 0x50e7cacf: THEN(CWAL_RC_JSON_INVALID_CHAR,"JSON_INVALID_CHAR");
    case 0x493f5ac4: THEN(CWAL_RC_JSON_INVALID_KEYWORD,"CWAL_RC_JSON_INVALID_KEYWORD");
    case 0x50e7e5e6: THEN(CWAL_RC_JSON_INVALID_KEYWORD,"JSON_INVALID_KEYWORD");
    case 0x493f8c32: THEN(CWAL_RC_JSON_INVALID_ESCAPE_SEQUENCE,"CWAL_RC_JSON_INVALID_ESCAPE_SEQUENCE");
    case 0x50e809f7: THEN(CWAL_RC_JSON_INVALID_ESCAPE_SEQUENCE,"JSON_INVALID_ESCAPE_SEQUENCE");
    case 0x493f996d: THEN(CWAL_RC_JSON_INVALID_UNICODE_SEQUENCE,"CWAL_RC_JSON_INVALID_UNICODE_SEQUENCE");
    case 0x50e817e2: THEN(CWAL_RC_JSON_INVALID_UNICODE_SEQUENCE,"JSON_INVALID_UNICODE_SEQUENCE");
    case 0x493f4b3c: THEN(CWAL_RC_JSON_INVALID_NUMBER,"CWAL_RC_JSON_INVALID_NUMBER");
    case 0x50e7dc04: THEN(CWAL_RC_JSON_INVALID_NUMBER,"JSON_INVALID_NUMBER");
    case 0x493f761d: THEN(CWAL_RC_JSON_NESTING_DEPTH_REACHED,"CWAL_RC_JSON_NESTING_DEPTH_REACHED");
    case 0x50e8787d: THEN(CWAL_RC_JSON_NESTING_DEPTH_REACHED,"JSON_NESTING_DEPTH_REACHED");
    case 0x493f72bd: THEN(CWAL_RC_JSON_UNBALANCED_COLLECTION,"CWAL_RC_JSON_UNBALANCED_COLLECTION");
    case 0x50e8ec6f: THEN(CWAL_RC_JSON_UNBALANCED_COLLECTION,"JSON_UNBALANCED_COLLECTION");
    case 0x493f3f54: THEN(CWAL_RC_JSON_EXPECTED_KEY,"CWAL_RC_JSON_EXPECTED_KEY");
    case 0x50e7d9b8: THEN(CWAL_RC_JSON_EXPECTED_KEY,"JSON_EXPECTED_KEY");
    case 0x493f49a9: THEN(CWAL_RC_JSON_EXPECTED_COLON,"CWAL_RC_JSON_EXPECTED_COLON");
    case 0x50e7e0f5: THEN(CWAL_RC_JSON_EXPECTED_COLON,"JSON_EXPECTED_COLON");
    case 0x493ebbb7: THEN(CWAL_SCR_CANNOT_CONSUME,"CWAL_SCR_CANNOT_CONSUME");
    case 0x5aae018c: THEN(CWAL_SCR_CANNOT_CONSUME,"SCR_CANNOT_CONSUME");
    case 0x493ef2f5: THEN(CWAL_SCR_INVALID_OP,"CWAL_SCR_INVALID_OP");
    case 0x5ab24bb4: THEN(CWAL_SCR_INVALID_OP,"SCR_INVALID_OP");
    case 0x493f61c5: THEN(CWAL_SCR_UNKNOWN_IDENTIFIER,"CWAL_SCR_UNKNOWN_IDENTIFIER");
    case 0x5ab6a880: THEN(CWAL_SCR_UNKNOWN_IDENTIFIER,"SCR_UNKNOWN_IDENTIFIER");
    case 0x493eeeeb: THEN(CWAL_SCR_CALL_OF_NON_FUNCTION,"CWAL_SCR_CALL_OF_NON_FUNCTION");
    case 0x5aae2f17: THEN(CWAL_SCR_CALL_OF_NON_FUNCTION,"SCR_CALL_OF_NON_FUNCTION");
    case 0x493f0807: THEN(CWAL_SCR_MISMATCHED_BRACE,"CWAL_SCR_MISMATCHED_BRACE");
    case 0x5ab2bb8c: THEN(CWAL_SCR_MISMATCHED_BRACE,"SCR_MISMATCHED_BRACE");
    case 0x493f3040: THEN(CWAL_SCR_MISSING_SEPARATOR,"CWAL_SCR_MISSING_SEPARATOR");
    case 0x5ab3b7b5: THEN(CWAL_SCR_MISSING_SEPARATOR,"SCR_MISSING_SEPARATOR");
    case 0x493f5451: THEN(CWAL_SCR_UNEXPECTED_TOKEN,"CWAL_SCR_UNEXPECTED_TOKEN");
    case 0x5ab6000b: THEN(CWAL_SCR_UNEXPECTED_TOKEN,"SCR_UNEXPECTED_TOKEN");
    case 0x493f41d2: THEN(CWAL_SCR_UNEXPECTED_EOF,"CWAL_SCR_UNEXPECTED_EOF");
    case 0x5ab5f1bc: THEN(CWAL_SCR_UNEXPECTED_EOF,"SCR_UNEXPECTED_EOF");
    case 0x493eeb38: THEN(CWAL_SCR_DIV_BY_ZERO,"CWAL_SCR_DIV_BY_ZERO");
    case 0x5ab205e1: THEN(CWAL_SCR_DIV_BY_ZERO,"SCR_DIV_BY_ZERO");
    case 0x493f41df: THEN(CWAL_SCR_SYNTAX,"CWAL_SCR_SYNTAX");
    case 0x05ab8018: THEN(CWAL_SCR_SYNTAX,"SCR_SYNTAX");
    case 0x124fa756: THEN(CWAL_SCR_EOF,"CWAL_SCR_EOF");
    case 0x00b55bc6: THEN(CWAL_SCR_EOF,"SCR_EOF");
    case 0x493f8488: THEN(CWAL_SCR_TOO_MANY_ARGUMENTS,"CWAL_SCR_TOO_MANY_ARGUMENTS");
    case 0x5ab7ca4d: THEN(CWAL_SCR_TOO_MANY_ARGUMENTS,"SCR_TOO_MANY_ARGUMENTS");
    case 0x493f250d: THEN(CWAL_SCR_EXPECTING_IDENTIFIER,"CWAL_SCR_EXPECTING_IDENTIFIER");
    case 0x5ab29312: THEN(CWAL_SCR_EXPECTING_IDENTIFIER,"SCR_EXPECTING_IDENTIFIER");
    
#undef THEN
  }
  return 0;
}

int whcl__next_token_no_eol(whcl_script * const ct,
                            whcl_stoken const ** tgt){
  int rc = 0;
  whcl_stoken const * k = 0;
  whcl_stoken_id const thePutback = ct->token;
  *tgt = NULL;
  for( ; 0==(rc = whcl__script_next_token2(ct, &k)) && k;
       k = NULL ){
    switch(k->ttype){
      case TOK1_T_EOL: continue;
      default:
        *tgt = k;
        ct->pbToken = thePutback;
        return 0;
    }
  }
  assert(NULL==k);
  assert(rc);
  return rc;
}


bool whcl_val_is_flag( cwal_value const * const v, char const * const f,
                       cwal_int_t fLen ){
  cwal_size_t nV = 0;
  char const * vStr = v ? cwal_value_get_cstr(v, &nV) : NULL;
  if(!vStr) return false;
  else if(fLen < 0) fLen = (cwal_int_t)cwal_strlen(f);
  return nV == (cwal_size_t)fLen && 0==memcmp(vStr, f, nV);
}

bool whcl_arg_has_flag( cwal_callback_args const * args,
                        uint16_t * argNdx, char const **flag,
                        cwal_size_t * len){
  bool rc = false;
  if(*argNdx < args->argc){
    cwal_size_t n = 0;
    char const * s = cwal_value_get_cstr(args->argv[*argNdx], &n);
    if(s && n>1 && '-'==s[0]){
      ++*argNdx;
      if(flag) *flag = s;
      if(len) *len = n;
      rc = true;
    }
  }
  return rc;
}


void whcl__value_to_lhs_scope( cwal_value const * lhs, cwal_value * const v){
  assert(!cwal_value_is_builtin(lhs)
         && "or else a caller is misusing this function.");
  if(lhs != v){
    cwal_scope * const s = cwal_value_scope(lhs);
    assert(s && "Very bad. Corrupt value.");
    if(s) cwal_value_rescope(s, v);
  }
}

int whcl__install_into_whcl(whcl_engine * const el,
                            char const * name,
                            cwal_value * const v){
  int rc;
  rc = cwal_prop_set_with_flags( el->cache.vWhcl,
                                 name, cwal_strlen(name),
                                 v, CWAL_VAR_F_CONST);
  return rc;
}

int whcl_install_typename_v(whcl_engine * const el, cwal_value * const tgt,
                            cwal_value * const name){
  return whcl_set_with_flags_v(el, tgt, el->cache.keyTypename,
                               name, CWAL_VAR_F_CONST | CWAL_VAR_F_HIDDEN);
}

int whcl_install_typename(whcl_engine * const el,
                          cwal_value * const tgt,
                          char const * name){
  cwal_value * const n =
    cwal_new_string_value(el->ec, name, cwal_strlen(name));
  int rc;
  if(!n){ WHCL__WARN_OOM; rc = CWAL_RC_OOM; }
  else{
    cwal_ref(n);
    rc = whcl_install_typename_v(el, tgt, n);
    cwal_unref(n);
  }
  return rc;
}


int whcl_install_core_apis(whcl_engine * const el){
  if(el->cache.installAPI & WHCL__INSTALL_API_core) return 0;
  int rc = 0;
  if(!whcl_prototype_object(el)){
    rc = whcl_err_has(el, true);
    if(!rc) rc = CWAL_RC_OOM;
    fprintf(stderr,"WARNING: whcl core API init failed with code %s!\n",
            cwal_rc_cstr(rc));
  }else{
    extern int whcl__cb_install_api( cwal_callback_args const *, cwal_value ** )
      /* defined in mod.c */;
    whcl_func_def const funcs[] = {
      WHCL_FUNC2("install-api", whcl__cb_install_api),
      WHCL_FUNC2("load-module", whcl_cb_module_load),
      whcl_func_def_empty_m
    };
    rc = whcl_install_functions(el, el->cache.vWhcl, funcs,
                                CWAL_VAR_F_CONST);
  }
  if(0==rc) el->cache.installAPI |= WHCL__INSTALL_API_core;
  return rc;
}

int whcl_tokenize_path_to_array( cwal_engine * const e, cwal_array ** tgt,
                               char const * path,
                               cwal_int_t pathLen ){
  int rc = 0;
  cwal_array * ar = *tgt ? *tgt : cwal_new_array(e);
  cwal_value * arV;
  char const * t = 0;
  cwal_size_t tLen = 0;
  whcl_path_toker pt = whcl_path_toker_empty;
  if(!ar){
    WHCL__WARN_OOM;
    return CWAL_RC_OOM;
  }
  arV = cwal_array_value(ar);
  cwal_value_ref(arV);
  whcl_path_toker_init(&pt, path, pathLen);
  while(0==whcl_path_toker_next(&pt, &t, &tLen)){
    cwal_value * const v = cwal_new_string_value(e, t, tLen);
    if(!v){
      rc = CWAL_RC_OOM;
      break;
    }
    cwal_value_ref(v);
    rc = cwal_array_append(ar, v);
    cwal_value_unref(v);
    if(rc) break;
  }
  if(rc){
    if(ar == *tgt) cwal_value_unhand(arV);
    else cwal_value_unref(arV);
  }else{
    *tgt = ar /* maybe a no-op */;
    cwal_value_unhand(arV);
  }
  return rc;
}

int whcl_cb_tokenize_path( cwal_callback_args const * args, cwal_value **rv ){
  int rc = 0;
  cwal_array * tgt = 0;
  cwal_value * tgtV = 0;
  char const * path;
  cwal_size_t pathLen;
  if(!args->argc || args->argc>2) goto misuse;
  path = cwal_value_get_cstr(args->argv[0], &pathLen);
  if(!path) goto misuse;
  if(2==args->argc){
    tgtV = args->argv[1];
    tgt = cwal_value_get_array(tgtV);
    if(!tgt) goto misuse;
  }else{
    tgt = cwal_new_array(args->engine);
    if(!tgt) return CWAL_RC_OOM;
    tgtV = cwal_array_value(tgt);
  }
  cwal_value_ref(tgtV);
  rc = pathLen
    ? whcl_tokenize_path_to_array(args->engine, &tgt,
                                path, (cwal_int_t)pathLen)
    : 0;
  if(rc){
    cwal_value_unref(tgtV);
  }else{
    cwal_value_unhand(tgtV);
    *rv = tgtV;
  }
  return rc;
  misuse:
  return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                             "Expecting (string path[, array target]) "
                             "argument(s).");
}

void whcl_path_toker_init( whcl_path_toker * pt, char const * path, cwal_int_t len ){
  *pt = whcl_path_toker_empty;
  pt->pos = pt->begin = path;
  pt->end = pt->begin + ((len>=0) ? (cwal_size_t)len : cwal_strlen(path));
}

int whcl_path_toker_next( whcl_path_toker * const pt, char const ** token,
                          cwal_size_t * const len ){
  if(!pt->pos || pt->pos>=pt->end) return CWAL_RC_RANGE;
  else if(!pt->separators || !*pt->separators) return CWAL_RC_MISUSE;
  else{
    char const * pos = pt->pos;
    char const * t;
    char const * sep;
    for( sep = pt->separators; *sep; ++sep){
      if(*sep & 0x80) return CWAL_RC_MISUSE;
      /* non-ASCII */
    }
    for( ; pos<pt->end; ){
      /*skip leading separators*/
      for( sep = pt->separators;
           *sep && *pos!=*sep; ++sep ){
      }
      if(*pos == *sep) ++pos;
      else break;
    }
    t = pos;
    for( ; pos<pt->end; ){
      /*skip until the next separator*/
      for( sep = pt->separators;
           *sep && *pos!=*sep; ++sep ){
      }
      if(*pos == *sep) break;
      else ++pos;
    }
    pt->pos = pos;
    if(pos>t){
      *token = t;
      *len = (cwal_size_t)(pos - t);
      return 0;
    }
    return CWAL_RC_NOT_FOUND;
  }
}

cwal_value * whcl_namespace(whcl_engine * const el){
  return el->cache.vWhcl;
}

int whcl__install_sub(whcl_engine * const el, cwal_value * const tgt,
                      char const * name, bool installCommandMethod,
                      cwal_value **theSub){
  int rc;
  cwal_value * sub = cwal_new_object_value(el->ec);
  if(!sub){ WHCL__WARN_OOM; return CWAL_RC_OOM; }
  cwal_value_prototype_set(sub, NULL);
  cwal_ref(sub);
  rc = cwal_prop_set(tgt, name, cwal_strlen(name), sub);
  cwal_unref(sub);
  if(0==rc){
    assert(cwal_value_refcount(sub) && "Has a ref via tgt[name].");
    if(installCommandMethod) rc = whcl_install_command_cb(el, sub);
    if(0==rc && theSub) *theSub = sub;
  }
  return rc;
}

cwal_value * whcl_whcl_object(whcl_engine * const el){
  return el->cache.vWhcl;
}

static int whcl__cb_json_parse_impl( cwal_callback_args const * args,
                                  cwal_value **rv, bool isFilename ){
  int rc;
  cwal_value * root = NULL;
  cwal_size_t slen = 0;
  cwal_json_parse_info pInfo = cwal_json_parse_info_empty;
  char const * cstr;
  cwal_value * arg = args->argc ? args->argv[0] : args->self;
  cwal_engine * e = args->engine;
  /*if(isFilename && (rc = s2_cb_disable_check(args, S2_DISABLE_FS_READ))){
    return rc;
    }*/
  cstr = cwal_value_get_cstr(arg, &slen);
  if(!cstr){
    cwal_buffer * b = cwal_value_buffer_part(e, arg);
    if(b){
      cstr = (char const *)b->mem;
      slen = b->used;
    }
    if(!cstr){
      return cwal_exception_setf(e, CWAL_RC_MISUSE,
                                 "Expecting a %s as argument or 'this'.",
                                 isFilename
                                 ? "filename" : "JSON (string|Buffer)");
    }
  }
  rc = isFilename
    ? cwal_json_parse_filename( e, cstr, &root, &pInfo )
    : cwal_json_parse_cstr( e, cstr, slen, &root, &pInfo );
  if(rc){
    if(pInfo.errorCode){
      return cwal_exception_setf(e, rc,
                                 "Parsing JSON failed at byte "
                                 "offset %"CWAL_SIZE_T_PFMT
                                 ", line %"CWAL_SIZE_T_PFMT
                                 ", column %"CWAL_SIZE_T_PFMT
                                 " with code %d (%s).",
                                 (cwal_size_t)pInfo.length,
                                 (cwal_size_t)pInfo.line,
                                 (cwal_size_t)pInfo.col,
                                 (int)pInfo.errorCode,
                                 cwal_rc_cstr(pInfo.errorCode));
    }else{
      return cwal_exception_setf(e, rc,
                                 "Parsing JSON failed with code %d (%s).",
                                 rc, cwal_rc_cstr(rc));
    }
  }
  assert(root);
  *rv = root;
  return 0;
}

int whcl__cb_json_parse_string( cwal_callback_args const * args, cwal_value **rv ){
  return whcl__cb_json_parse_impl(args, rv, false);
}

int whcl__cb_json_parse_file( cwal_callback_args const * args, cwal_value **rv ){
  return whcl__cb_json_parse_impl(args, rv, true);
}

int whcl__install_json( whcl_engine * const el ){
  if(el->cache.installAPI & WHCL__INSTALL_API_json) return 0;
  cwal_value * sub = NULL;
  cwal_value * const tgt = el->cache.vWhcl;
  char const * name = "json";
  int rc = whcl_install_core_apis(el);
  if(0==rc) rc = whcl__install_sub(el, tgt, name, true, &sub);
  if(0==rc){
    whcl_func_def const funcs[] = {
    /* WHCL_FUNC2("experiment", whcl_cb_container_experiment), */
      WHCL_FUNC2("parse", whcl__cb_json_parse_string),
      WHCL_FUNC2("parse-file", whcl__cb_json_parse_file),
      WHCL_FUNC2("stringify", whcl_cb_arg_to_json_token),
      whcl_func_def_empty_m
    };
    rc = whcl_install_functions(el, sub, funcs, 0);
    if(!rc){
      /* Install json.clone() convenience method */
      rc = whcl_eval_cstr_with_var(el, "J", sub, "JSON module init",
                                   "set -const J[clone] proc {v} "
                                   "{return [P [S $v]]} using {"
                                   "P J[parse] S J[stringify]"
                                   "}",
                                   -1, 0);
    }
  }
  if(0==rc) el->cache.installAPI |= WHCL__INSTALL_API_json;
  return rc;
}

/**
   Works mostly like whcl__create_value() except that it works on
   tok1_ens and handles only a subset of the token types. Intended
   solely for use with whcl_cb_tokenize_line().
*/
static int whcl__tok1_en_value(whcl_engine * const el, tok1_en const * tok,
                               cwal_value **rv ){
  int rc = 0;
  cwal_value * v = NULL;
  char const * tBeg = tok1_en_begin(tok);
  cwal_size_t const tLen = tok1_en_len(tok);
#define RC if(v) rc = 0; \
  else { WHCL__WARN_OOM; rc = CWAL_RC_OOM; }(void)0
  switch(tok->ttype){
    case TOK1_T_LiteralNumber: {
      cwal_int_t di = 0;
      switch(tok->ttype2){
        case TOK1_T_LiteralIntBin:
        case TOK1_T_LiteralIntDec:
        case TOK1_T_LiteralIntHex:
        case TOK1_T_LiteralIntOct:
          whcl_parse_int(tBeg, tLen, &di);
          v = cwal_new_integer(el->ec, di);
          RC;
          break;
        case TOK1_T_LiteralDouble:{
          cwal_double_t dd = 0;
          cwal_cstr_to_double(tBeg, tLen, &dd);
          v = cwal_new_double(el->ec, dd);
          RC;
          break;
        }
        default:
          whcl__fatal(CWAL_RC_CANNOT_HAPPEN,
                      "Invalid ttype2 mapping: %s",
                      tok1_t_cstr(tok->ttype2));
      }
      break;
    }/*TOK1_T_LiteralNumber*/
    case TOK1_T_QuotedString:{
      cwal_buffer * const escapeBuf = &el->escBuf;
      cwal_size_t const oldBufUsed = escapeBuf->used;
      rc = tok1_unescape_string(el->ec, tok1_en_begin2(tok),
                                tok1_en_end2(tok),
                                escapeBuf );
      if(rc){
        rc = cwal_exception_setf(el->ec, CWAL_RC_RANGE==rc
                                 ? CWAL_SCR_SYNTAX : rc,
                                 "Unescaping string failed with rc=%s, "
                                 "likely due to non-UTF8 content "
                                 "or an unknown \\Uxxxxxxxx sequence.",
                                 cwal_rc_cstr(rc));
        goto buffer_cleanup;
      }
      /*MARKER("STRING: [%.*s]\n", (int)(escapeBuf->used - oldUsed),
        (char const *)escapeBuf->mem+oldUsed);*/
      assert(escapeBuf->mem ? (0 == escapeBuf->mem[escapeBuf->used]) : 1);
      v = cwal_new_string_value(el->ec, escapeBuf->used
                                ? (char const *)escapeBuf->mem : "",
                                escapeBuf->used);
      RC;
      /* whcl__dump_val(v,"string literal"); */
      buffer_cleanup:
      escapeBuf->used = oldBufUsed;
      if(escapeBuf->mem) escapeBuf->mem[oldBufUsed] = 0;
      break;
    }
    default:{
      /* Use the raw token bytes as a string... */
      cwal_size_t const len = tok1_en_len2(tok);
      char const * begin = tok1_en_begin2(tok);
      char const * end = tok1_en_end2(tok);
      assert(begin); assert(end); assert(begin <= end);
      v = cwal_new_string_value(el->ec, begin, len);
      RC;
      break;
    }
  }/*tok->ttype*/
#undef RC
  if(0==rc){
    assert(v);
    *rv = v;
  }
  return rc;  
}

/**
   If tok is an Identifier which matches one of the built-in
   values (true, false, null, undefined), that value is returned,
   else NULL is returned.
*/
cwal_value * whcl__tok1_is_tfnu( tok1_en const * tok ){
  cwal_value * rv = 0;
  if(TOK1_T_Identifier == tok->ttype){
    cwal_size_t const tlen = tok1_en_len(tok);
    switch(tlen){
      case 4:
        if(0==cwal_compare_cstr("true", 4,
                                tok1_en_begin(tok), tlen)){
          rv = cwal_value_true();
        }else if(0==cwal_compare_cstr("null", 4,
                                      tok1_en_begin(tok), tlen)){
          rv = cwal_value_null();
        }
        break;
      case 5:
        if(0==cwal_compare_cstr("false", 5,
                                tok1_en_begin(tok), tlen)){
          rv = cwal_value_false();
        }
        break;
      case 9:
        if(0==cwal_compare_cstr("undefined", 9,
                                tok1_en_begin(tok), tlen)){
          rv = cwal_value_undefined();
        }
        break;
      default:
        break;
    }
  }
  return rv;
}


int whcl_cb_tokenize_line(cwal_callback_args const * args, cwal_value ** rv){
  cwal_array * ar = 0;
  cwal_value * arV = 0;
  tok1_izer pt = tok1_izer_empty;
  cwal_size_t lineLen = 0;
  char const * line = 1==args->argc
    ? cwal_value_get_cstr(args->argv[0], &lineLen)
    : 0;
  int rc;
  if(!line) {
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                         "Expecting a single string argument.");
  }
  tok1_izer_init( args->engine, &pt, line, (cwal_int_t)lineLen,
                   TOK1_F_LINEAR_TOKER );
  pt.name = "tokenize-line input";
  //MARKER(("Tokenizing line: %.*s\n", (int)lineLen, line));
  whcl_engine * const el = whcl_engine_from_args(args);
  while( !(rc=whcl__next_token_base(&pt, NULL))
         && !tok1_izer_is_eof(&pt) ){
    /*MARKER(("Token %s: %.*s\n", tok1_t_cstr(pt.token.ttype),
      (int)tok1_en_len(&pt.token), tok1_en_begin(&pt.token)));*/
    tok1_izer_errtoken_set(&pt, &pt.token);
    if(tok1_t_is_junk(pt.token.ttype)) continue;
    cwal_value * v = NULL;
    if(!ar){
      ar = cwal_new_array(args->engine);
      if(!ar){
        WHCL__WARN_OOM;
        rc = CWAL_RC_OOM;
        goto toss;
      }
      arV = cwal_array_value(ar);
      cwal_ref(arV);
    }
    v = whcl__tok1_is_tfnu(&pt.token);
    if(!v){
      rc = whcl__tok1_en_value( el, &pt.token, &v );
      if(rc) break;
    }
    cwal_ref(v);
    rc = cwal_array_append(ar, v);
    cwal_unref(v);
    if(rc) break;
  }
  if(rc) goto toss;
  cwal_unhand(arV);
  *rv = arV ? arV : cwal_value_undefined();
  return rc;
  toss:
  cwal_unref(arV);
  assert(rc);
  switch(rc){
    case CWAL_RC_EXCEPTION:
    case CWAL_RC_OOM:
    case CWAL_RC_INTERRUPTED:
      break;
    default:
      if(pt.errMsg){
        rc = cwal_cb_throw(args, rc, "Line tokenization failed "
                           "with code %s near line %d, column %d: %s",
                           cwal_rc_cstr(rc), (int)pt.token.line,
                           (int)pt.token.column, pt.errMsg);
      }else{
        rc = cwal_cb_throw(args, rc, "Line tokenization failed with "
                           "code %s, possibly near line %d column %d.",
                           cwal_rc_cstr(rc), (int)pt.token.line,
                           (int)pt.token.column);
      }
      break;
  }
  tok1_izer_finalize( &pt );
  return rc;
}

int whcl_install_argv(whcl_engine * const el, int argc,
                      char const * const * argv){
  int rc;
  cwal_value * v = cwal_new_array_value(el->ec);
  if(!v){
    WHCL__WARN_OOM;
    return CWAL_RC_OOM;
  }
  cwal_ref(v);
  rc = cwal_parse_argv_flags(el->ec, argc, argv, &v);
  if(0==rc){
    rc = whcl_set_with_flags(el, whcl_namespace(el), "ARGV", 4, v,
                             CWAL_VAR_F_CONST);
  }
  cwal_unref(v);
  return rc;
}

int whcl_ctor_method_set( whcl_engine * const el,
                          cwal_value * const container,
                          cwal_function * const method ){
  if(!container || !method) return CWAL_RC_MISUSE;
  else if(!cwal_props_can(container)) return CWAL_RC_TYPE;
  else{
    return whcl_set_with_flags_v( el, container, el->cache.keyNewCtor,
                                  cwal_function_value(method),
                                  CWAL_VAR_F_CONST | CWAL_VAR_F_HIDDEN );
  }
}

int whcl_ctor_callback_set( whcl_engine * const el,
                            cwal_value * const container,
                            cwal_callback_f method ){
  int rc;
  cwal_function * f;
  cwal_value * fv;
  if(!container || !method) return CWAL_RC_MISUSE;
  else if(!cwal_props_can(container)) return CWAL_RC_TYPE;
  fv = cwal_new_function_value(el->ec, method, 0, 0, 0);
  f = fv ? cwal_value_get_function(fv) : NULL;
  if(f){
    cwal_ref(fv);
    rc = whcl_ctor_method_set( el, container, f );
    cwal_unref(fv);
  }else{
    WHCL__WARN_OOM;
    rc = CWAL_RC_OOM;
  }
  return rc;
}

int whcl_ctor_apply( whcl_engine * const el, cwal_value * const operand,
                     cwal_function * ctor, cwal_array * args,
                     cwal_value **rv ){
  cwal_value * newThis = NULL;
  cwal_value * vProto = NULL;
  int rc;
  whcl_scope * scel = NULL;
  *rv = 0;
  if(!operand || !rv) return CWAL_RC_MISUSE;
  else if(ctor){
    vProto = cwal_value_prototype_get(el->ec,
                                      cwal_function_value(ctor));
  }else{
    whcl__new_find_ctor( el, operand, &ctor, &vProto );
    if(!ctor){
      return whcl_err_set(el, CWAL_RC_TYPE,
                          "Could not resolve constructor function.");
    }
    assert(vProto);
  }
  if(!(scel = whcl__scope_push(el, 0))) return CWAL_RC_OOM;
  if(args) cwal_ref(cwal_array_value(args));
  newThis = cwal_new_object_value(el->ec);
  if(!newThis){
    WHCL__WARN_OOM;
    rc = CWAL_RC_OOM;
    goto end;
  }
  cwal_ref(newThis);
  rc = whcl_set_v( el, newThis, el->cache.keyPrototype, vProto)
    /* instead of cwal_value_prototype_set(newThis, operand) so that
       we get consistent error reporting (via whcl_set_v(), as opposed
       to an unadorned error code via the lower-level function). */;
  if(!rc){
    cwal_value * ctorResult = 0 /* the ctor result */;
    uint16_t const vFlags
      = cwal_container_client_flags_set( newThis, WHCL__VAL_F_IS_NEWING );
    rc = args
      ? cwal_function_call_array(NULL, ctor, newThis, &ctorResult, args)
      : cwal_function_call(ctor, newThis, &ctorResult, 0, NULL);
    cwal_container_client_flags_set( newThis, vFlags );
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
      /* whcl__dump_val(ctorResult,"ctorResult"); */
      /* whcl_dump_val(newThis,"newThis"); */
      cwal_ref(ctorResult);
      cwal_unref(newThis);
      newThis = ctorResult;
    }
  }
  end:
  whcl_scope_pop(el, scel, newThis);
  if(rc){
    cwal_unref(newThis);
  }else{
    assert(newThis);
    *rv = newThis;
    cwal_unhand(newThis);
  }
  if(args) cwal_unref(cwal_array_value(args));
  return rc;
}


int whcl_stash_hidden_member( cwal_value * const who, cwal_value * const what ){
  int rc = cwal_props_can(who) ? 0 : CWAL_RC_TYPE;
  cwal_value * key;
  cwal_engine * const e = cwal_value_engine(who);
  if(rc) return rc;
  assert(e && "if who can props then who has an engine.");
  if(!e) return CWAL_RC_MISUSE;
  key = cwal_new_unique( e, 0 );
  if(!key) return CWAL_RC_OOM;
  cwal_ref(key);
  rc = cwal_prop_set_with_flags_v( who, key, what,
                                   CWAL_VAR_F_HIDDEN
                                   | CWAL_VAR_F_CONST );
  cwal_unref(key);
  return rc;
}

int whcl_set_this(whcl_engine * const el, cwal_value * const that){
  return whcl_scope_set_v(el, NULL, false, el->cache.keyThis, that);
}

void whcl_feature_flag_set( whcl_engine * const el,
                            enum whcl_engine_feature_e f,
                            bool on ){
  switch(f){
    case WHCL_FEATURE_F_TRACE_ASSERT:
      el->flags.traceAssert = on; break;
    case WHCL_FEATURE_F_TRACE_AFFIRM:
      el->flags.traceAffirm = on; break;
    case WHCL_FEATURE_F_DEBUG_BLOCK:
      el->flags.enableDebugBlock = on; break;
  }
}

int whcl_set_from_script_v( whcl_engine * const el, char const * src,
                            cwal_int_t srcLen, cwal_value * const addResultTo,
                            cwal_value * const propName ){
  if(!src || !addResultTo || !propName) return CWAL_RC_MISUSE;
  else if(!cwal_props_can(addResultTo)) return CWAL_RC_TYPE;
  else{
    int rc;
    cwal_size_t const holderLen = whcl__holder_len(el);
    if((rc = whcl__holder_push(el, propName))
       || (rc = whcl__holder_push(el, addResultTo))){
      goto end;
    }
    cwal_value * rv = 0;
    char const * pn = cwal_value_get_cstr(propName, 0);
    if(srcLen<0) srcLen = (cwal_int_t)cwal_strlen(src);
    rc = whcl_eval_cstr( el, true, pn ? pn : __func__,
                         src, srcLen, &rv );
    whcl_check_for_return(el, &rc, &rv);
    if(!rc){
      cwal_ref(rv);
      rc = whcl_set_v( el, addResultTo, propName,
                       rv ? rv : cwal_value_undefined() );
      cwal_unref(rv);
    }
    end:
    whcl__holder_truncate(el, holderLen, NULL);
    return rc;
  }  
}


int whcl_set_from_script( whcl_engine * const el, char const * src,
                          cwal_int_t srcLen, cwal_value * const addResultTo,
                          char const * propName ){
  if(!src || !addResultTo || !propName){return CWAL_RC_MISUSE;}
  else if(!cwal_props_can(addResultTo)) return CWAL_RC_TYPE;
  else if(!*propName) return CWAL_RC_RANGE;
  else{
    int rc;
    cwal_value * const prop = cwal_new_string_value(el->ec, propName,
                                                    cwal_strlen(propName));
    cwal_ref(prop);
    rc = prop
      ? whcl_set_from_script_v( el, src, srcLen, addResultTo, prop )
      : CWAL_RC_OOM;
    cwal_unref(prop);
    return rc;
  }  
}

void whcl_dump_metrics(whcl_engine * const el){
  cwal_engine * const e = el->ec;
  cwal_dump_allocation_metrics( e );
  cwal_outputf(e, "%.60c\nwhcl-specific metrics:\n", '-');
  if(el->scopes.blockCount){
    unsigned const bc = el->scopes.blockCount;
    unsigned const bs = whcl__scope_block_size;
    cwal_outputf(e, "whcl_engine::scopes::blockCount = %u * %u "
                 "scopes per block = %u bytes\n",
                 bc, bs, bc * bs * sizeof(whcl_scope));
    if(el->scopes.maxLevel){
      cwal_outputf(e, "max whcl_scope depth: %"CWAL_SIZE_T_PFMT"\n",
                   el->scopes.maxLevel);
    }
  }
  cwal_outputf(e, "whcl_engine sweep interval=%d, vacuum interval=%d, "
               "total sweeps=%d, vacuums=%u\n",
               (int)el->sweeper.sweepInterval, (int)el->sweeper.vacuumInterval,
               el->sweeper.sweepTotal - el->sweeper.vacuumTotal,
               el->sweeper.vacuumTotal);
  if(el->recycler.scriptFuncs.max){
    uint32_t const h = el->recycler.scriptFuncs.hits;
    uint32_t const m = el->recycler.scriptFuncs.misses;
    uint32_t const z = (uint32_t)sizeof(whcl__func);
    cwal_outputf(e, "whcl__func recycler: sizeof(whcl__func)=%"PRIu32", "
                 "hits=%"PRIu32" (%u bytes), "
                 "misses=%"PRIu32" (%u bytes)\n",
                 z, h, (unsigned)(h * z),
                 m, (unsigned)(m * z));
  }
}

char * whcl__strdup(cwal_engine * const e,
                    char const * s, cwal_int_t n){
  if(n<0) n = (cwal_int_t)cwal_strlen(s);
  char * x = (char *)cwal_malloc(e, n + 1);
  if(!x){WHCL__WARN_OOM;}
  else{
    memcpy(x, s, n);
    x[n] = 0;
  }
  return x;
}

int cwal_json_override_f_whcl(cwal_engine * e, cwal_value * src,
                              cwal_value **rv, void * state){
  int rc = 0;
  if(e || state){/*unused*/}
  whcl_engine * const el = whcl_engine_from_state(e);
  assert(el);
  cwal_value * v = 0
    ? cwal_prop_get(src, "to-jsonable", 11)
    : whcl_get(el, src, "to-jsonable", 11);
  if(v && cwal_value_is_function(v)){
    /* This feature already exists in cwal_json_output() but we
       reimplement here so that we can guard the whcl_engine against
       vacuuming during json output callbacks. The JSON API internally
       uses temporary values (via this very callback mechanism) which
       it cannot make vacuum-safe without lots of hoop-jumping.
       Because this callback may invoke script code, which could
       trigger a vacuum, it could well clean up internal temp values
       in the JSON API. Thus we disable vacuuming for the duration of
       the call. */
    cwal_value * frv = NULL;
    cwal_function * const f = cwal_value_get_function(v);
    cwal_ref(v);
    ++el->sweeper.guard.vacuum;
    rc = cwal_function_call(f, src, &frv, 0, NULL)
      /* Just musing: it's conceivable that this callback could remove
         the to-jsonable property which we're currently calling,
         potentially freeding it. That's why holding a ref to it
         before the call is important. That said: cwal_function_call()
         both adds a ref and makes the function vacuum-proof for the
         duration of the call, so maybe having a ref here is
         overengineering. */;
    --el->sweeper.guard.vacuum;
    if(rc){
      cwal_unref(v);
      return rc;
    }else if(frv){
      *rv = frv;
      if(frv == v){
        /* Far-fetched corner case: we need to ensure that we don't
           nuke this if the to-jsonable property is somehow cleaned up
           during the above call, leaving us with (possibly) the only
           reference. */
        cwal_unhand(v);
      }
      else cwal_unref(v);

      return 0;
    }
    // Else fall through...
    cwal_unref(v);
  }
  v = NULL;
  switch(cwal_value_type_id(src)){
    default: break;
    case CWAL_TYPE_NATIVE: {
      //whcl__dump_val(src, __func__);
      whcl_script * const scr = whcl_value_get_script(src);
      if(scr){
        rc = whcl_script_to_jsonable(scr, &v);
        if(v){
          assert(0==rc);
          *rv = v;
        }
      }
      break;
    }
  }
  return rc;
}

bool whcl_parse_int(char const * str, cwal_int_t slen,
                      cwal_int_t * rv ){
  if(slen<0) slen = (cwal_int_t)cwal_strlen(str);
  while( slen && tok1_is_space(*str) ){
    ++str;
    --slen;
  }
  if(!slen) return false;
  int const prefix = ('-'==*str) ? -1 : ('+'==*str ? 1 : 0);
  if(prefix){
    ++str;
    --slen;
  }
  if(!slen) return false;
  tok1_izer pr = tok1_izer_empty;
  tok1_izer_init( NULL, &pr, str, slen, 0 );
  if(whcl__next_token_base(&pr, NULL)) return false;
  tok1_en const tn = pr.token;
  if(0==whcl__next_token_base(&pr, NULL)
     && !tok1_t_is_eof(pr.token.ttype)) return false;
  switch(tn.ttype){
    default: return false;
    case TOK1_T_LiteralNumber: {
      char const * tBeg = tok1_en_begin(&tn);
      cwal_size_t const tLen = tok1_en_len(&tn);
      bool check = false;
      switch(tn.ttype2){
        case TOK1_T_LiteralIntBin:
          assert( tLen > 2 /*"0b" prefix */);
          check = 0==whcl__script_parse_binary_digits(tBeg+2, tLen-2, rv);
          break;
        case TOK1_T_LiteralIntDec:
          check = 0==whcl__script_parse_decimal_digits(tBeg, tLen, rv);
          break;
        case TOK1_T_LiteralIntHex:
          assert( tLen > 2 /*"0x" prefix */);
          check = 0==whcl__script_parse_hex_digits(tBeg+2, tLen-2, rv);
          break;
        case TOK1_T_LiteralIntOct:
          assert( tLen > 2 /*"0o" prefix */);
          check = 0==whcl__script_parse_octal_digits(tBeg+2, tLen-2, rv);
          break;
        default:
          return false;
      }
      if(rv && check && prefix<0) *rv = -*rv;
      return check;
    }
  }
  return false;
}

int whcl_parse_number( cwal_engine * const e, char const * src,
                       cwal_int_t slen, cwal_value ** rv ){
  cwal_int_t inty = 0;
  if(whcl_parse_int(src, slen, &inty)){
    *rv = cwal_new_integer(e, inty);
    return *rv ? 0 : CWAL_RC_OOM;
  }else{
    cwal_double_t dbl = 0.0;
    if(0==cwal_cstr_to_double(src, slen, &dbl)){
      *rv = cwal_new_double(e, dbl);
      return *rv ? 0 : CWAL_RC_OOM;
    }else{
      *rv = 0;
      return 0;
    }
  }
}

#undef MARKER
#undef whcl__hash_token
#undef whcl__cstrv
#undef whcl__rescopeS
#undef whcl__rescopeE
#undef whcl__scope_current
#undef whcl__scope_for_level
#undef whcl__scope_at
