/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

#include "s2_internal.h" /* must come first b/c of config macros */
#include <assert.h>
#include <stdlib.h>
#include <string.h> /* memcmp() */
#include <stdio.h> /* FILE, fprintf() */
#include <errno.h>
#include "wh/cwal/cwal_printf.h"

#define S2_USE_SIGNALS S2_HAVE_SIGACTION

#if S2_USE_SIGNALS
#include <signal.h> /* sigaction(), if our feature macros are set right */
#endif

#if 1
#define MARKER(pfexp) if(1) printf("MARKER: %s:%d:\t",__FILE__,__LINE__); if(1) printf pfexp
#else
#define MARKER(pfexp) (void)0
#endif

const s2_stoken s2_stoken_empty = s2_stoken_empty_m;
#if 0
const s2_op s2_op_empty = s2_op_empty_m;
#endif
const s2_engine s2_engine_empty = s2_engine_empty_m;
const s2_stoken_stack s2_stoken_stack_empty = s2_stoken_stack_empty_m;
const s2_estack s2_estack_empty = s2_estack_empty_m;
const s2_sweep_guard s2_sweep_guard_empty = s2_sweep_guard_empty_m;
const s2_scope s2_scope_empty = s2_scope_empty_m;
const s2_enum_builder s2_enum_builder_empty = {
0/*se*/,
0/*flags*/,
0/*entryCount*/,
0/*entries*/
};
const s2_kvp_each_state s2_kvp_each_state_empty = {
0/*e*/, 0/*self*/, 0/*callback*/, 0/*valueArgFirst*/
};

#if S2_USE_SIGNALS
static s2_engine * s2Interruptable = 0;
#endif

s2_engine * s2_engine_alloc( cwal_engine * e ){
  s2_engine * rc = (s2_engine*)cwal_malloc(e, sizeof(s2_engine));
  if(rc){
    *rc = s2_engine_empty;
    rc->allocStamp = e;
    rc->e = e /* need this for dealloc */;
  }
  return rc;
}

void s2_estack_clear( s2_engine * se, s2_estack * st, char allowRecycle ){
  s2_stoken_stack_clear( se, &st->vals, allowRecycle );
  s2_stoken_stack_clear( se, &st->ops, allowRecycle );
}

void s2_estack_swap( s2_estack * lhs, s2_estack * rhs ){
  s2_estack const tmp = *lhs;
  *lhs = *rhs;
  *rhs = tmp;
}

void s2_engine_stack_swap( s2_engine * se, s2_estack * st ){
  s2_estack const tmp = se->st;
  se->st = *st;
  *st = tmp;
}

void s2_engine_reset_stack( s2_engine * se ){
  s2_estack_clear(se, &se->st, 1);
}


void s2_engine_free_recycled_funcs( s2_engine * se ) /* in s2_eval.c */;
void s2_modules_close( s2_engine * ) /* in s2_mod.c */;

/**
   Frees up all data stored in se which refers to cwal_values. It
   must only be called immediately before the top-most stack is
   popped. Cleaned-up data includes any s2_ob()-related buffers.
*/
static void s2_engine_cleanup_values(s2_engine * se){
  assert(se->e);
  assert(!se->scopes.current);
  assert(cwal_scope_current_get(se->e) == &se->scopes.topScope);
  if(se->stash){
    cwal_value_unref( se->stash );
    se->stash = 0;
  }
  memset(&se->cache, 0, sizeof(se->cache))
    /* se->cache.keyXXX all point back into se->stash */;
  if(se->funcStash){
    cwal_value_unref( cwal_hash_value(se->funcStash) );
    se->funcStash = 0;
  }
  cwal_exception_set( se->e, 0 );
  s2_propagating_set( se, 0 );
  while(se->ob.count) s2_ob_pop(se);
  cwal_list_reserve(se->e, &se->ob, 0);
}

void s2_engine_finalize( s2_engine * se ){
  cwal_engine * e;
  void const * allocStamp;
  if(!se) return;
#if S2_USE_SIGNALS
  if(s2Interruptable==se) s2Interruptable = 0;
#endif
  allocStamp = se->allocStamp;
  e = se->e;
  if(!e){
    assert(!se->st.vals.top);
    assert(!se->recycler.stok.top);
    assert(!se->st.ops.top);
    assert(!se->buffer.mem);
    assert(!se->stash);
    assert(!se->dotOp.lhs);
    *se = s2_engine_empty;
  }else{
    s2_ukwd_free(se);
    while( se->scopes.level ){
      /* reminder: se took over cwal's top-most scope during init. */
      cwal_scope_pop(se->e);
    }
    assert(!se->e->current);
    assert(!se->scopes.list && "Should have been cleaned up via pop hook.");
    assert(!se->scopes.current && "Should have been cleaned up via pop hook.");
    s2_modules_close(se)
      /* must come after all scopes are gone because the modules might
         have introduced memory which the scopes reference via native
         values or function bindings. */;

    /*
      Reminder: the following cleanup is only legal as long as we
      don't use/deref any Values...

      e.g if the token stack is changed to ref its values, we need to
      moved its cleanup higher up.
    */
    s2_engine_free_recycled_funcs(se);
    s2_engine_reset_stack(se);
    s2_stoken_stack_clear( se, &se->recycler.stok, 0 );
    cwal_buffer_clear(e, &se->buffer);
    *se = s2_engine_empty;
    if(allocStamp==e) cwal_free(e, se);
    cwal_engine_destroy( e );
  }
}


/* In eval.c */
int s2_callback_hook_pre(cwal_callback_args const * argv, void * state);
/* In eval.c */
int s2_callback_hook_post(cwal_callback_args const * argv, void * state, int fRc, cwal_value * rv);
/* cwal_callback_f() pre- and post-call() hooks. This is where we install
   'argv' and 'this'.
*/
static const cwal_callback_hook cwal_callback_hook_s2 = {
    NULL /*state*/,
    s2_callback_hook_pre,
    s2_callback_hook_post
};

static char const * s2_type_name_proxy( cwal_value const * v,
                                        cwal_size_t * len ){
  cwal_value const * tn = cwal_prop_get(v, "__typename", 10);
  return tn ? cwal_value_get_cstr(tn, len) : NULL;
}

static void s2_engine_subexpr_save2(s2_engine * e, s2_subexpr_savestate * to,
                                    char resetIt){
  to->ternaryLevel = e->ternaryLevel;
  if(resetIt) e->ternaryLevel = 0;
}

void s2_engine_subexpr_save(s2_engine * e, s2_subexpr_savestate * to){
  s2_engine_subexpr_save2(e, to, 1);
}

#define s2__scope_for_level(SE,LVL)             \
  (((LVL) && (LVL)<=(SE)->scopes.level)         \
   ? ((SE)->scopes.list + (LVL) - 1)            \
   : (s2_scope*)NULL)

s2_scope * s2_scope_for_level( s2_engine const * se,
                               cwal_size_t level ){
  assert(se);
  return s2__scope_for_level(se, level);
}

#define s2__scope_current(SE)                            \
  (((SE) && (SE)->e && (SE)->e->current)                  \
   ? s2__scope_for_level((SE), (SE)->e->current->level)   \
   : 0)

s2_scope * s2_scope_current( s2_engine const * se ){
  return s2__scope_current(se);
}



/**
   Ensures that se->scopes.list has at least newDepth scopes in
   reserve. Returns 0 on success, CWAL_RC_OOM on allocation error. If
   newDepth is 0 then it frees se->scopes.list, but that may only
   be used during s2_engine cleanup (else an assert() will fail).
*/
static int s2_reserve_scopes( s2_engine * se, cwal_size_t newDepth ){
  if(0==newDepth){
    assert(!se->scopes.current && "Else internal mismanagement of s2_engine::scopes.");
    cwal_realloc(se->e, se->scopes.list, 0);
    se->scopes.list = 0;
    se->scopes.alloced = 0;
  }
  else if(se->scopes.alloced < newDepth){
    s2_scope * sc;
    cwal_size_t newCount = se->scopes.alloced
      ? (se->scopes.alloced * 8 / 5) : 16;
    assert(newCount > newDepth);
    sc = (s2_scope *)cwal_realloc( se->e, se->scopes.list,
                                   (cwal_size_t)(newCount * sizeof(s2_scope)) );
    if(!sc) return CWAL_RC_OOM;
    ++se->metrics.totalScopeAllocs;
    se->scopes.list = sc;
    se->scopes.alloced = newCount;
  }
  return 0;
}

/**
   cwal_engine_vtab::hook::scope_push() hook. state must be a
   (s2_engine*). This function syncronizes the cwal scope stack with
   our s2_scope stack. The only error condition is a potential
   CWAL_RC_OOM if reserving a block of s2_scope entries fails. It
   keeps all scopes in a single array, which has a maximum length
   directly related to (but not identical to) the highest-ever scope
   depth and is expanded as needed (but never shrinks until the engine
   is finalized).
*/
static int s2_scope_hook_push( cwal_scope * s, void * state ){
  s2_engine * se = (s2_engine*)state;
  s2_scope * s2sc;
  int rc;
  assert(0 < s->level);
  assert(se);
  assert(s2_engine_from_state(s->e) == se);
  rc = s2_reserve_scopes(se, s->level);
  if(rc) return rc;
  /*MARKER(("Pushing scope level %d (%d reserved)\n", (int)s->level,
    (int)se->scopes.alloced));*/
  se->scopes.level = s->level;
  s2sc = s2__scope_for_level(se, s->level);
  *s2sc = s2_scope_empty;
  s2sc->cwalScope = s;
  s2sc->flags = se->scopes.nextFlags;
  s2_engine_subexpr_save2(se, &s2sc->saved,
                         (S2_SCOPE_F_KEEP_TERNARY_LEVEL
                          & se->scopes.nextFlags)
                         ? 0 : 1);
  se->scopes.nextFlags = 0;
  se->scopes.current = s2sc;
  ++se->metrics.totalScopesPushed;
  if(s->level > se->metrics.maxScopeDepth){
    se->metrics.maxScopeDepth = (int)s->level;
  }
  return rc;
}

/**
   cwal_engine_vtab::hook::scope_pop() hook. state must be
   a (s2_engine*). 
*/
static void s2_scope_hook_pop( cwal_scope const * s, void * state ){
  s2_engine * se = (s2_engine*)state;
  s2_scope * s2sc;
  /*MARKER(("Popping scope level %d\n", (int)s->level));*/
  assert(se);
  assert(0 < s->level);
  assert(s2_engine_from_state(s->e) == se);
  assert(se->scopes.alloced >= s->level);
  s2sc = s2__scope_for_level(se, s->level);
  assert(s2sc);
  assert(s == s2sc->cwalScope);
  se->scopes.current = s2__scope_for_level(se, s->level-1);
  se->scopes.level = s->level-1;
  assert(s->level>1
         ? (s->level == 1 + se->scopes.current->cwalScope->level)
         : !se->scopes.current);
  s2_dotop_state(se, 0, 0, 0);
  if(s2sc->evalHolder){
    cwal_value * av = cwal_array_value(s2sc->evalHolder);
    s2sc->evalHolder = 0;
    assert(1 == cwal_value_refcount(av));
    assert(s2sc->cwalScope == cwal_value_scope(av));
    cwal_value_unref(av);
  }
  s2_engine_subexpr_restore(se, &s2sc->saved);
  *s2sc = s2_scope_empty;
  if(1==s->level){
    /* Final scope is popping. Let's clean se->scopes.list. */
    /*MARKER(("pop hook: the engine is shutting down. Freeing %d s2_scopes.\n",
      (int)se->scopes.alloced));*/
    assert(!se->scopes.current);
    s2_engine_cleanup_values(se);
    s2_reserve_scopes(se, 0);
  }
}

static char const * cwal_rc_cstr_f_s2(int rc){
  switch((enum s2_rc_e)rc){
#define CASE(X) case X: return #X
    CASE(S2_RC_placeholder);
    CASE(S2_RC_END_EACH_ITERATION);
    CASE(S2_RC_TOSS);
    CASE(S2_RC_end);
    CASE(S2_RC_CLIENT_BEGIN);
#undef CASE
  }
  return 0;
}

void s2_static_init(void){
  static int rcFallback = 0;
  if(!rcFallback && 1==++rcFallback){
    /* yes, there's a small race condition here, but it's not tragic
       except in a 1-in-a-bazillion case where X instances happen to
       successfully race here, where X is the static limit of
       cwal_rc_cstr_f fallbacks. */
    cwal_rc_cstr_fallback(cwal_rc_cstr_f_s2);
  }
}

int s2_engine_init( s2_engine * se, cwal_engine * e ){
  void const * allocStamp;
  int rc = 0;
  s2_static_init();
  if(!se || !e) return CWAL_RC_MISUSE;
  allocStamp = se->allocStamp;
  assert(!se->e || (se->e == e));
  *se = s2_engine_empty;
  se->allocStamp = allocStamp;
  se->e = e;

  {
    cwal_callback_hook hook = cwal_callback_hook_s2;
    hook.state = se;
    cwal_callback_hook_set(e, &hook);
    cwal_engine_type_name_proxy( e, s2_type_name_proxy );
  }

  /* 
     What follows assumes that the client has created no cwal_values
     [of interest] before this routine was called. cwal necessarily
     pushes a scope during its setup, but we need to pop that scope
     and start a new top scope so that the s2/cwal scope levels
     stay in sync.
  */
  assert(se->e->current);
  assert(se->e->current==&se->e->topScope);

  rc = cwal_engine_client_state_set( e, se,
                                     &s2_engine_empty, 0 )
    /* Required by the s2 function callback hook mechanism. */;
  assert(!rc && "Can only fail if client state was already set!");

  /* Clear all cwal-level scopes and start with a new scope
     stack, so that we can hook up our s2_scopes. */
  if( (rc = cwal_scope_pop(se->e)) ) return rc;
  assert(!se->e->current);
  assert(!e->vtab->hook.scope_push);
  assert(!e->vtab->hook.scope_pop);
  e->vtab->hook.scope_push = s2_scope_hook_push;
  e->vtab->hook.scope_pop = s2_scope_hook_pop;
  e->vtab->hook.scope_state = se;

  /**
     cwal always needs at least 1 scope active. Now that we have our
     scope hooks in place, pop a new top-level scope.
  */
  {
    cwal_scope * cs = &se->scopes.topScope;
    if( (rc = cwal_scope_push(se->e, &cs) ) ) return rc;
    assert(se->e->current);
    assert(1==se->e->current->level);
    assert(se->e->current==&se->scopes.topScope);
    assert(se->e==se->scopes.topScope.e);
    assert(se->scopes.alloced >= 10 && "But we start with at least 10 scopes?");
  }

  /* We need some strings as keys in a few places, so put a copy in
     the stash.

     Reminder: it/they're owned by the stash.
  */
#define STASHVAL(MEMBER,VAL)                    \
  if(!rc) do{                                           \
      if(!(se->cache.MEMBER = VAL)) rc = CWAL_RC_OOM;                   \
      else{                                                             \
        cwal_value_ref(se->cache.MEMBER);                               \
        rc = s2_stash_set_v( se, se->cache.MEMBER, se->cache.MEMBER );  \
        cwal_value_unref(se->cache.MEMBER);                             \
        if(rc){                                                         \
          se->cache.MEMBER = 0;                                         \
        }                                                               \
      }                                                                 \
    } while(0)
#define STASHKEY(MEMBER,STR)                                            \
  STASHVAL(MEMBER,cwal_new_string_value(e, (STR), (cwal_size_t)(sizeof(STR)-1)))

  STASHKEY(keyPrototype,"prototype");
  STASHKEY(keyThis,"this");
  STASHKEY(keyArgv,"argv");
  STASHKEY(keyValue,"value");
  STASHKEY(keyName,"name");
  STASHKEY(keyTypename,"__typename");
  STASHKEY(keyScript,"script");
  STASHKEY(keyLine,"line");
  STASHKEY(keyColumn,"column");
  STASHKEY(keyStackTrace,"stackTrace");
  STASHKEY(keyCtorNew,"__new");
  STASHKEY(keyImportFlag,"doPathSearch");
#if S2_TRY_INTERCEPTORS
  STASHKEY(keyInterceptee,"interceptee");
#endif
#undef STASHKEY
#undef STASHVAL

  return rc;
}

/**
   Enumeration of various scope-sweeping options.
 */
enum S2SweepModes {
SweepMode_VacuumRecursive = -2,
SweepMode_SweepRecursive = -1,
SweepMode_Default = 0,
SweepMode_Sweep = 1,
SweepMode_Vacuum = 2
};
/**
   Might or might not trigger a sweep or vacuum, depending partially
   on the initialBroomMode _hint_ and various internal state.
*/
static int s2_engine_sweep_impl( s2_engine * se, enum S2SweepModes initialBroomMode ){
  int rc = 0;
  s2_scope * sc = s2__scope_current(se);
  assert(se && se->e);
  assert(sc);
  /*MARKER(("SWEEP RUN #%d? sweepTick=%d, s-guard=%d, v-guard=%d\n",
          se->sweepTotal, sc->sguard.sweepTick,
          sc->sguard.sweep, sc->sguard.vacuum));*/
  if(!sc){
    assert(!"Someone called s2_engine_sweep() without cwal_scope_push().");
    return CWAL_RC_MISUSE;
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
  if(sc->sguard.sweep>0 || se->sweepInterval<=0) return 0;
  else if(SweepMode_Default==initialBroomMode
          && (++sc->sguard.sweepTick != se->sweepInterval)) return 0;
  else{
    enum S2SweepModes sweepMode =
      (SweepMode_Default==initialBroomMode)
      ? SweepMode_Sweep : initialBroomMode;
    int valsSwept = 0;
    ++se->sweepTotal;
    sc->sguard.sweepTick = 0;
    /* See if we can/should use sweep2 or vacuum... */
    if(SweepMode_Default==initialBroomMode
       && se->vacuumInterval>0
       && !sc->sguard.vacuum
       && (0 == (se->sweepTotal % se->vacuumInterval))
       ){
      sweepMode = SweepMode_Vacuum;
    }
    /* MARKER(("SWEEP RUN #%d mode=%d\n", se->sweepTotal, sweepMode)); */
    switch(sweepMode){
      default:
        assert(!"Invalid sweep mode!");
        return CWAL_RC_MISUSE;
      case SweepMode_Default:
      case SweepMode_Sweep:
        valsSwept = (int)cwal_engine_sweep(se->e);
        break;
      case SweepMode_SweepRecursive:
#if 0
        assert(!"NO!"); /* this cannot work with current code */
#else
        /* 20181126: this actually works, at least in the s2 unit
           test scripts. It's not terribly interesting in terms of
           cleaning up, though - a recursive vacuum is really the
           Holy Grail of cleanup. */
#endif
        /*MARKER(("SWEEP RUN #%d mode=%d\n", se->sweepTotal, sweepMode));*/
        valsSwept = (int)cwal_engine_sweep2(se->e, 1);
        break;
      case SweepMode_Vacuum:
        assert(SweepMode_Vacuum==sweepMode);
        rc = cwal_engine_vacuum(se->e, se->flags.traceSweeps
                                ? &valsSwept : 0
                                /* b/c this reporting costs */);
        assert(!rc && "Vacuum \"cannot fail\".");
        break;
      case SweepMode_VacuumRecursive:{
        /**
           20181126: nope, recursive vacuum still doesn't quite
           work. It pulls being-eval'd values out from under various
           pending expressions. e.g. the 'using' part of a function
           definition can trigger it, as can loop constructs.

           [Later that day:] If we guard a scope against vacuuming
           based on both s->sguard.vacuum and s->sguard.sweep,
           recursive vacuum actually works (meaning it doesn't crash
           us), but (A) in the unit test suite it's not cleaning up
           any more than non-recursive and (B) it's slow, and would
           need to be called infrequently. A recursive vacuum would
           only hypothetically be useful for catching pathological
           use cases such as this contrived bit of code:

           // Create a pathologically cyclic structure:
           var a = [1,2,3], o = {a};
           o.a[] = o; o.a[] = a;
           o[o] = o; o[a] = a;
           scope {
             print(__FLC, 'nulling...');
             o = a = null;
             print(__FLC, 'nulled');
             for( var i = 0; i < 20; ++i ){print(__FLC, i)}
             print(__FLC,"scope closing");
           }
           print(__FLC,"scope closed");
           ; ; ; ; ; ;
           print(__FLC,"done");

           As long as we're in that inner scope (or lower), neither
           'a' nor 'o' can be cleaned up without a recursive vacuum
           because they're cyclic. Even if we try to recursively
           vacuum, we can only clean them if the sweep/vacuum guards
           do not prohibit it the vacuum. (The guards are there to
           protect being-eval'd stuff, and the main eval impl always
           (IIRC) sweep-guards the currently-evaluating expression.)
           i.e. chances are good(?) that we wouldn't be able to vacuum
           the scope even if we wanted to.

           A quick test of exactly that case shows that the scope
           which owns 'a' and 'o' is indeed protected (because the
           'scope' keyword is part of a pending expression, and
           therefore causes that scope to sweep-guard) until the
           'scope' completes.

           Recursive vacuum is arbitrarily expensive and is rarely
           useful, so there's really no reason to enable it by
           default, but it might be interesting to add a "mega-gc"
           function or keyword which forces a recursive vacuum, with
           the caveat that that the sguard can block it from
           happening. If we bypass the sguard, we eventually _will_
           clean up stuff we definitely don't want to clean up.
        */
        s2_scope * s = s2__scope_current(se);
        for( ; !rc && s ; s = s2__scope_for_level(se, s->cwalScope->level-1) ){
          if(!s->sguard.vacuum && !s->sguard.sweep){
            int count = 0;
            MARKER(("Attempting recursive vaccum on scope level %d...\n",
                    (int)s->cwalScope->level));
            rc = cwal_scope_vacuum(s->cwalScope, se->flags.traceSweeps
                                   ? &count : 0
                                   /* b/c this reporting costs */);
            valsSwept += count;
          }else{
            MARKER(("Skipping recursive vaccum on scope level %d due to sguard.\n",
                    (int)s->cwalScope->level));
          }
        }
        assert(!rc && "Vacuum \"cannot fail\".");
        break;
      }
    }
    if(se->flags.traceSweeps>2
       || (se->flags.traceSweeps && valsSwept)){
      char const * label = "???";
      s2_ptoker const * const pt = se->currentScript;
      s2_ptoken const * tok = pt ? &pt->token : 0;
      char const * tokEnd = tok ? s2_ptoken_end(tok) : 0;
      const char * ptEnd = pt ? s2_ptoker_end(pt) : 0;
      switch(sweepMode){
        case SweepMode_Vacuum: label = "vacuum"; break;
        case SweepMode_VacuumRecursive: label = "recursive vacuum"; break;
        case SweepMode_Sweep: label = "sweep"; break;
        case SweepMode_SweepRecursive: label = "recursive sweep"; break;
        default: break;
      }
      if(se->flags.traceSweeps>1 && valsSwept
         && tokEnd
         && tokEnd>=ptEnd
         && tokEnd-1<ptEnd){
        s2_linecol_t line = 0, col = 0;
        cwal_size_t nLen = 0;
        char const * scriptName = s2_ptoker_name_first( pt, &nLen );
        s2_ptoker_count_lines( pt, tokEnd-1
                               /* ^^^tokEnd might be at EOF, which isn't legal here */,
                               &line, &col );
        MARKER(("Swept up %d value(s) in %s mode somewhere around %.*s:%d:%d\n",
                valsSwept, label,
                (int)(nLen ? nLen : 5), nLen ? scriptName : "<" "???" ">" /* trigraph! */,
                line, col));
      }else{
        MARKER(("Swept up %d value(s) in %s mode\n", valsSwept, label));
      }
    }
    return rc;
  }
}

int s2_engine_sweep( s2_engine * se ){
  return s2_engine_sweep_impl(se, SweepMode_Default);
}

int s2_cb_internal_experiment( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  s2_engine * se = s2_engine_from_args(args);
  assert(se);
  se->flags.traceSweeps += 3;
  rc = s2_engine_sweep_impl( se, SweepMode_VacuumRecursive );
  se->flags.traceSweeps -= 3;
  *rv = cwal_value_undefined();
  return rc;
}

void s2_engine_vacuum( s2_engine * se ){
  s2_scope * sc = s2__scope_current(se);
  assert(se && se->e);
  assert(sc);
  if(!sc->sguard.sweep && !sc->sguard.vacuum){
#ifdef DEBUG
    int vacCount = 0;
    cwal_engine_vacuum(se->e, &vacCount);
    assert(vacCount >= 0);
#else
    cwal_engine_vacuum(se->e, NULL);
#endif
  }
}

void s2_stoken_stack_push( s2_stoken_stack * ts, s2_stoken * t ){
  assert(ts);
  assert(t);
  assert(!t->next);
  assert(t != ts->top);
  t->next = ts->top;
  ts->top = t;
  ++ts->size;
}

s2_stoken * s2_stoken_stack_pop( s2_stoken_stack * ts ){
  s2_stoken * t = 0;
  assert(ts);
  if(ts->size>0){
    t = ts->top;
    assert(t);
    ts->top = t->next;
    t->next = 0;
    --ts->size;
  }
  return t;
}

void s2_stoken_stack_clear( s2_engine * se, s2_stoken_stack * st, char allowRecycle ){
  s2_stoken * t;
  while( (t = s2_stoken_stack_pop(st)) ){
    s2_stoken_free(se, t, allowRecycle);
  }
}

static void s2_engine_trace_stack(s2_engine * se, s2_stoken const * t, char isPush){
  int const ttype = t->ttype;
  s2_op const * op = s2_ttype_op(ttype);
  assert(se->flags.traceTokenStack);
  cwal_outputf(se->e, "s2_engine::traceTokenStack: after %s ",
               isPush ? "push" : "pop");
  if(op){
    cwal_outputf(se->e, "op (%s) ", op->sym);
  }else{
    cwal_outputf(se->e, "token %s (typename=%s) ",
                 s2_ttype_cstr(ttype),
                 t->value ? cwal_value_type_name(t->value) : "<NULL>");
  }
  cwal_outputf(se->e, "tokens=%d, ops=%d\n",
               se->st.vals.size, se->st.ops.size);

}

#define s2__ttrace(isPush, TOK) if(se->flags.traceTokenStack) s2_engine_trace_stack(se, TOK, isPush)

void s2_engine_push( s2_engine * se, s2_stoken * t ){
  s2_stoken_stack_push( s2_stoken_op(t) ? &se->st.ops : &se->st.vals, t );
  s2__ttrace(1, t);
}

void s2_engine_push_valtok( s2_engine * se, s2_stoken * t ){
  s2_stoken_stack_push( &se->st.vals, t );
  s2__ttrace(1, t);
}

void s2_engine_push_op( s2_engine * se, s2_stoken * t ){
  s2_stoken_stack_push( &se->st.ops, t );
  s2__ttrace(1, t);
}

s2_stoken * s2_engine_push_ttype( s2_engine * se, int i ){
  s2_stoken * t = s2_stoken_alloc2( se, i, 0 );
  if(t) s2_engine_push( se, t );
  return t;
}

s2_stoken * s2_engine_push_val( s2_engine * se, cwal_value * v ){
  if(!se || !v) return 0;
  else{
    s2_stoken * t = s2_stoken_alloc2( se, S2_T_Value, v );
    if(t) s2_engine_push_valtok( se, t );
    return t;
  }
}

s2_stoken * s2_engine_push_tv( s2_engine * se, int ttype, cwal_value * v ){
  s2_stoken * t = s2_stoken_alloc2( se, ttype, v );
  if(t) s2_engine_push_valtok( se, t );
  return t;
}


s2_stoken * s2_engine_push_int( s2_engine * se, cwal_int_t i ){
  s2_stoken * rc = 0;
  cwal_value * v = cwal_new_integer(se->e, i);
  if(!v) return 0;
  else{
    if(!(rc = s2_engine_push_val(se, v))){
      cwal_value_unref(v);
    }
  }
  return rc;
}


s2_stoken * s2_engine_peek_token( s2_engine * se ){
  return se->st.vals.top;
}

cwal_value * s2_engine_peek_value( s2_engine * se ){
  return se->st.vals.top ? se->st.vals.top->value : 0;
}

s2_stoken * s2_engine_peek_op( s2_engine * se ){
  return se->st.ops.top;
}

static s2_stoken * s2_engine_pop_token_impl( s2_engine * se, 
                                             s2_stoken_stack * ts,
                                             char returnItem ){
  s2_stoken * rc = s2_stoken_stack_pop(ts);
  if(rc){
    s2__ttrace(0, rc);
    if(!returnItem){
      s2_stoken_free( se, rc, 1 );
      rc = 0;
    }
  }
  return rc;
}

#undef s2__ttrace

s2_stoken * s2_engine_pop_token( s2_engine * se, char returnItem ){
  return s2_engine_pop_token_impl(se, &se->st.vals, returnItem);
}

s2_stoken * s2_engine_pop_op( s2_engine * se, char returnItem ){
  return s2_engine_pop_token_impl(se, &se->st.ops, returnItem);
}

cwal_value * s2_engine_pop_value( s2_engine * se ){
  cwal_value * v = 0;
  s2_stoken * t = s2_engine_pop_token(se, 1);
  if(t){
    v = t->value;
    t->value = 0;
    s2_stoken_free(se, t, 1);
  }
  return v;
}

#if 0
void s2_engine_stack_replace( s2_engine * se, s2_estack const * src,
                              s2_estack * priorStacks ){
  if(priorStacks) *priorStacks = se->st;
  else s2_engine_reset_stack(se);
  se->st = src ? *src : s2_estack_empty;
}
#endif

s2_stoken * s2_stoken_alloc( s2_engine * se ){
  s2_stoken * s;
  assert(se);
  assert(se->e);
  ++se->metrics.tokenRequests;
  s = s2_stoken_stack_pop(&se->recycler.stok);
  if(!s){
    s = (s2_stoken*)cwal_malloc(se->e, sizeof(s2_stoken));
    if(s){
      ++se->metrics.tokenAllocs;
      cwal_engine_adjust_client_mem(se->e,
                                    (cwal_int_t)sizeof(s2_stoken));
      assert(se->e->metrics.clientMemCurrent>0);
    }
  }
  if(s){
    *s = s2_stoken_empty;
    if(++se->metrics.liveTokenCount > se->metrics.peakLiveTokenCount){
      se->metrics.peakLiveTokenCount = se->metrics.liveTokenCount;
    }
  }
  return s;
}

s2_stoken * s2_stoken_alloc2( s2_engine * se, int type, cwal_value * v ){
  s2_stoken * t = s2_stoken_alloc(se);
  if(t){
    t->ttype = type;
    t->value = v;
  }
  return t;
}

void s2_stoken_free( s2_engine * se, s2_stoken * t, char allowRecycle ){
  /*
    Reminder: t does not hold a reference to t->value, so we do not
    let a reference go here. Any stray stack machine values
    (e.g. those left over during error handling mid-expression) will
    be cleaned up by the scope which is, more likely than not, about
    to pop as a result of error propagation (or it's the global scope,
    in which case it's free to sweep them up).
  */
  assert(se);
  assert(se->e);
  assert(t);
  assert(!t->next);
  --se->metrics.liveTokenCount;
  if(allowRecycle && (se->recycler.stok.size < se->recycler.maxSTokens)){
    s2_stoken_stack_push( &se->recycler.stok, t );
  }else{
    *t = s2_stoken_empty;
    assert(se->e->metrics.clientMemCurrent>0);
    cwal_engine_adjust_client_mem(se->e, -((cwal_int_t)sizeof(s2_stoken)));
    cwal_free2( se->e, t, sizeof(s2_stoken) );
  }
}

static int s2_process_op_impl( s2_engine * se, s2_op const * op,
                               char popOpStack ){
  int rc = se->flags.interrupted;
  s2_stoken_stack * st = &se->st.vals;
  int const oldStackSize = st->size;
  int popArgCount = 0;
  cwal_value * rv = 0;
  s2_stoken * topOp = 0;
  assert(op);
  se->opErrPos = 0;
  if(rc) return rc;
  else if(se->flags.traceTokenStack){
    MARKER(("running operator %s (#%d) arity=%d assoc=%d prec=%d\n",
            op->sym, op->id, op->arity, op->assoc, op->prec));
  }
  /**
    pop op (if needed) first so that we can guaranty operators that
    they are not the top op on the stack when they are called. Useful
    for '=', which will want to know if the LHS is a dot operator or
    not.
  */
  if(popOpStack){
    topOp = s2_engine_pop_op(se, 1);
  }
  if(!op->call){
    rc = s2_engine_err_set(se, CWAL_RC_UNSUPPORTED,
                           "Operator %s does not have an internal "
                           "call() impl.",
                           op->sym);
  }     
  else if(op->arity>=0){
#if 1
    assert((st->size >= op->arity)
           || (op->assoc>0 && op->arity==1 /* unary +, -, ~ */));
#endif
    if(st->size < op->arity){
      rc = s2_engine_err_set(se, CWAL_RC_RANGE,
                             "Not enough operands on the stack.");
    }else{
      rc = op->call(op, se, op->arity, &rv);
      popArgCount = op->arity;
    }
  }else{
#if 0
    assert(!"not possible... except when running the old test.c, it seems...");
#else
    /* Variadic operator ... */
    s2_stoken * t = se->st.vals.top;
    int i = 0;
    char doBreak = 0;
    /* Count how the arguments by looking for
       a S2_T_MarkVariadicStart token. */
    for( ; t && !doBreak && (i <st->size); t = t->next ){
      switch(t->ttype){
        case S2_T_MarkVariadicStart:
          doBreak = 1;
          break;
        default:
          ++i;
          break;
      }
    }
    assert(doBreak);
    if(!doBreak){
      rc = s2_engine_err_set(se, CWAL_RC_MISUSE,
                             "Missing S2_T_MarkVariadicStart!");
    }else{
      /* MARKER(("variadic argc=%d\n", i)); */
      rc = op->call(op, se, i, &rv);
      assert( st->size == (oldStackSize - i) );
      if(1){
        S2_UNUSED_VAR s2_stoken * variadicCheck = s2_engine_peek_token(se);
        assert(variadicCheck);
        assert(S2_T_MarkVariadicStart == variadicCheck->ttype);
      }
      s2_engine_pop_token( se, 0 ) /* S2_T_MarkVariadicStart */;
      popArgCount = i + 1 /* variadic marker */;
    }
#endif
  }
  if(!rc){
    assert( st->size == (oldStackSize - popArgCount) );
    if(st->size != (oldStackSize - popArgCount)){
      rc = s2_engine_err_set(se, CWAL_RC_MISUSE,
                             "Unexpected stack size after "
                             "running operator '%s'\n",
                             op->sym);
    }else if(rv){
      /* Push the result value... */
      s2_stoken * tResult = topOp ? topOp/*recycle it*/ : 0;
      topOp = 0;
      if(!tResult){
        tResult = s2_stoken_alloc(se);
        if(!tResult) rc = CWAL_RC_OOM;
      }else{
        *tResult = s2_stoken_empty;
      }
      if(tResult){
        tResult->ttype = rv ? S2_T_Value : S2_T_Undefined;
        tResult->value = rv ? rv : cwal_value_undefined();
        s2_engine_push_valtok(se, tResult);
#if 1
        /*
          20171115: 'abc'[2][0][0][0][0][0]

          is assert()ing (sometimes) in cwal_value_ref() when string
          interning is on because 'c' is an interned value but the
          above op chain is not ref'ing it (as it should). We could
          patch that in s2_eval_expr_impl(), but this is a more
          general problem which also potentially affect any place this
          routine is used, e.g. function calls may theoretically
          trigger it.

          Reminder: the above might fail in s2sh interactive mode
          while not failing in a unit test script: triggering it is
          dependent on engine-level state. The eval-hold resolves it,
          in either case.
        */
        rc = s2_eval_hold(se, rv);
#endif
      }else{
        if(rv) cwal_refunref(rv);
      }
    }
  }else{
    assert(!rv);
  }
  if(topOp){
    if(rc && !se->opErrPos) se->opErrPos = s2_ptoken_begin(&topOp->srcPos);
    s2_stoken_free(se, topOp, 1);
  }
  return s2_check_interrupted(se, rc);
}


int s2_process_op( s2_engine * se, s2_op const * op ){
  return s2_process_op_impl(se, op, 0);
}

int s2_process_top( s2_engine * se ){
  s2_stoken_stack * so = &se->st.ops;
  s2_op const * op = s2_stoken_op(so->top);
  char const * srcPos = so->top ? s2_ptoken_begin(&so->top->srcPos) : 0;
  int rc = s2_check_interrupted(se, 0);
  if(rc) return rc;
  else if(!op){
    rc = s2_engine_err_set(se, CWAL_SCR_SYNTAX,
                           "Token type %s (#%d) is not an operator.",
                           so->top ? s2_ttype_cstr(so->top->ttype) : "<NULL>",
                           so->top ? so->top->ttype : S2_T_INVALID);
  }else if(op
           && op->arity>0
           && se->st.vals.size<op->arity){
    rc = s2_engine_err_set(se, CWAL_SCR_SYNTAX,
                             "Value stack does not have enough values for "
                             "operator '%s'.", op->sym);
  }else{
    rc = s2_process_op_impl( se, op, 1 );
    if(rc && srcPos && !se->opErrPos) se->opErrPos = srcPos;
  }
  return rc;
}

int s2_process_op_type( s2_engine * se, int ttype ){
  s2_op const * op = s2_ttype_op(ttype);
  return op
    ? s2_process_op(se, op)
    : CWAL_RC_TYPE;
}

int s2_error_set( s2_engine * se, cwal_error * err, int code, char const * fmt, ... ){
  int rc;
  va_list args;
  va_start(args,fmt);
  rc = cwal_error_setv(se->e, err, code, fmt, args);
  va_end(args);
  return rc;
}


/* in s2_eval.c */
int s2_strace_generate( s2_engine * se, cwal_value ** rv );

int s2_add_script_props2( s2_engine * se,
                          cwal_value * ex,
                          char const * scriptName,
                          int line, int col){
  int rc = 0;
  /**
     FIXME: clean up these setters to not strand these temporary
     values (on error cases) until the next sweep.
  */
  if(scriptName && *scriptName){
    cwal_value * snv = cwal_new_string_value(se->e,
                                              scriptName,
                                              cwal_strlen(scriptName));
    cwal_value_ref(snv);
    rc = snv
      ? cwal_prop_set_v(ex, se->cache.keyScript, snv)
      : CWAL_RC_OOM;
    cwal_value_unref(snv);
  }
  if(!rc && line>0){
    rc = cwal_prop_set_v(ex, se->cache.keyLine,
                         cwal_new_integer(se->e, line));
    if(!rc) rc = cwal_prop_set_v(ex, se->cache.keyColumn,
                                 cwal_new_integer(se->e, col));
  }
  /* MARKER(("strace count=%u\n", se->strace.count)); */
  if(!rc
     && se->flags.exceptionStackTrace
     && se->strace.count
     && !cwal_prop_has_v(ex, se->cache.keyStackTrace, 0)){
    cwal_value * stackTrace = 0;
    rc = s2_strace_generate(se, &stackTrace);
    if(!rc && stackTrace){
      /* s2_dump_val(stackTrace, "stackTrace"); */
      cwal_value_ref(stackTrace);
      rc = cwal_prop_set_v(ex, se->cache.keyStackTrace, stackTrace);
      cwal_value_unref(stackTrace);
    }
  }
  return rc;
}

int s2_add_script_props( s2_engine * se, cwal_value * ex, s2_ptoker const * script ){
  if(ex && !script) script = se->currentScript;
  if(!ex
     || !script
     || !cwal_props_can(ex)
     || cwal_prop_has_v(ex, se->cache.keyLine, 1)
     /* ^^^ very cursory check for "already has this state" */
     ) return 0;
  else{
    s2_linecol_t line = 0, col = 0;
    cwal_size_t scriptNameLen = 0;
    s2_ptoker const * top = 0;
    char const * scriptName = s2_ptoker_name_first(script, &scriptNameLen);
    char const * errPos = s2_ptoker_err_pos(script);
    assert(errPos);
    s2_ptoker_count_lines(top ? top : script, errPos, &line, &col);
    if(scriptName || (line>0)){
      return s2_add_script_props2(se, ex, scriptName, line, col);
    }else{
      return 0;
    }
  }
}

int s2_exception_add_script_props( s2_engine * se, s2_ptoker const * script ){
  cwal_value * ex = cwal_exception_get(se->e);
  return ex ? s2_add_script_props(se, ex, script) : 0;
}

cwal_value * s2_error_exception( s2_engine * se,
                                 cwal_error * err,
                                 char const * scriptName,
                                 int line, int col ){
  return cwal_error_exception(se->e, err, scriptName, line, col);
}

int s2_throw_err( s2_engine * se, cwal_error * err,
                  char const * script,
                  int line, int col ){
  return cwal_error_throw(se->e, err, script, line, col);
}

int s2_throw_value( s2_engine * se, s2_ptoker const * pr, int code, cwal_value *v ){
  int rc = 0, rc2 = 0;
  cwal_value * exv;
  char wasException;
  assert(v);
  if(CWAL_RC_OOM==code) return code;
  else if(!pr) pr = se->currentScript;
  exv = cwal_exception_value(cwal_value_exception_part(se->e, v));
  wasException = exv ? 1 : 0;
  if(!exv){
    exv = cwal_new_exception_value(se->e, code ? code : CWAL_RC_EXCEPTION, v);
    if(!exv) rc2 = CWAL_RC_OOM;
  }
  if(exv){
    cwal_value * const propTarget = wasException ? v : exv;
    cwal_value_ref(exv);
    if(pr && !cwal_prop_has_v(propTarget, se->cache.keyLine, 1)
      /* ^^^ *seems* (at an admittedly quick glance) to not inherit
         location information, so we'll harvest it. */){
      /* 20191228: this was changed to only set the properties if the
         passed-in object seems to not have/inherit them to begin
         with. Previously we were, when v inherited an exception,
         setting this state in v even through v inherited it from exv,
         which led to, e.g., duplicate (but ever-so-slightly
         different) stackTrace values.
      */
      rc2 = s2_add_script_props(se, propTarget, pr);
    }
    if(!rc2){
      rc = cwal_exception_set(se->e, propTarget);
    }
    cwal_value_unref(exv)
      /* On success, cwal holds a reference. On error, if exv was
         derived from v then the caller had better hold a reference to
         it or else we're all doomed. If exv is a prototype of v,
         rather than being v itself, it has a reference through that
         association. */;
  }
  return rc2 ? rc2 : rc;
}


int s2_throw( s2_engine * se, int code, char const * fmt, ... ){
  int rc;
  switch(code){
    case CWAL_RC_OOM: rc = code;
      break;
    default: {
      va_list args;
      va_start(args,fmt);
      rc = cwal_error_setv( se->e, NULL, code, fmt, args);
      va_end(args);
      if(rc==code) rc = cwal_error_throw(se->e, 0, 0, 0, 0);
      break;
    }
  }
  return rc;
}

int s2_engine_err_has( s2_engine const * se ){
  int rc = se->flags.interrupted;
  if(!rc){
    rc = cwal_engine_error_get(se->e, NULL, NULL);
    if(!rc){
      rc = cwal_exception_get(se->e)
        ? CWAL_RC_EXCEPTION : 0;
    }
  }
  return rc;
}

int s2_engine_err_setv( s2_engine * se, int code, char const * fmt, va_list vargs ){
  return cwal_error_setv(se->e, 0, code, fmt, vargs);
}

int s2_engine_err_set( s2_engine * se, int code, char const * fmt, ... ){
  int rc;
  va_list args;
  va_start(args,fmt);
  rc = cwal_error_setv(se->e, NULL, code, fmt, args);
  va_end(args);
  return rc;
}

void s2_engine_err_reset( s2_engine * se ){
  se->flags.interrupted = 0;
  cwal_engine_error_reset(se->e);
}

void s2_engine_err_reset2( s2_engine * se ){
  s2_engine_err_reset(se);
  cwal_exception_set(se->e, 0);
  s2_propagating_set(se, 0);
}

void s2_engine_err_clear( s2_engine * se ){
  se->flags.interrupted = 0;
  cwal_error_clear(se->e, 0);
}

int s2_engine_err_get( s2_engine const * se, char const ** msg, cwal_size_t * msgLen ){
  return cwal_engine_error_get(se->e, msg, msgLen);
}

void s2_dump_value( cwal_value * v, char const * msg,
                    char const * file, char const * func, int line ){
  cwal_scope const * sc = cwal_value_scope(v);
  static cwal_json_output_opt jopt = cwal_json_output_opt_empty_m;
  static int once = 0;
  FILE * out = stdout /* Reminder: we cannot use cwal_output() because
                         v might be a built-in const without a cwal_engine
                         instance. */;
  if(v){
    assert((sc || cwal_value_is_builtin(v))
           && "Seems like we've cleaned up too early.");
  }
  if(!once){
    jopt.cyclesAsStrings = 1;
    jopt.functionsAsObjects = 0;
    jopt.addNewline = 1;
    jopt.indentSingleMemberValues = 0;
    jopt.indent = 0;
    jopt.indentString.str = "  ";
    jopt.indentString.len = cwal_strlen(jopt.indentString.str);
    once = 1;
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
    case CWAL_TYPE_FUNCTION:
    case CWAL_TYPE_BUFFER:
    case CWAL_TYPE_NATIVE:
    case CWAL_TYPE_HASH:
    case CWAL_TYPE_UNIQUE:
      fprintf(out, "%s@%p\n",
              cwal_value_type_name(v), (void const*)v);
      break;
    case CWAL_TYPE_UNDEF:
      fwrite("undefined\n", 10, 1, out);
      break;
    default:
      cwal_json_output_FILE( v, out, &jopt );
      break;
  }
}

void s2_fatal( int code, char const * fmt, ... ){
  va_list args;
  cwal_printf_FILE(stderr, "FATAL ERROR: code=%d (%s)\n",
                   code, cwal_rc_cstr(code));
  if(fmt && *fmt){
    va_start(args,fmt);
    cwal_printfv_FILE(stderr, fmt, args);
    va_end(args);
    fwrite("\n", 1, 1, stderr);
  }
  abort();
}

int s2_var_decl_v( s2_engine * se, cwal_value * key,
                   cwal_value * v, uint16_t flags ){
  return cwal_var_decl_v(se->e, 0, key, v, flags);
}

int s2_var_decl( s2_engine * se, char const * key, cwal_size_t keyLen,
                 cwal_value * v, uint16_t flags ){
  return cwal_var_decl(se->e, 0, key, keyLen, v, flags);
}

static int s2_stash_init(s2_engine * se){
  assert(!se->stash);
  se->stash = cwal_new_hash_value(se->e, 47)
    /* 20200118: the stash currently only contains
       ~26 values, not including anything which
       loadable modules might install. */
    ;
  if(!se->stash) return CWAL_RC_OOM;
  else{
    cwal_scope * topScope;
    topScope = &se->scopes.topScope;
    assert(topScope->level);
    cwal_value_rescope(topScope, se->stash);
    cwal_value_ref(se->stash)
      /* Make sure it doesn't get swept up! */
      ;
    cwal_value_make_vacuum_proof(se->stash, 1)
      /* And not vacuumed, either. */
      ;
  }
  return 0;
}

int s2_stash_set_v( s2_engine * se, cwal_value * key, cwal_value * v ){
  int rc = 0;
  cwal_hash * h;
  if(!key || !v) return CWAL_RC_MISUSE;
  else if(!se->stash){
    rc = s2_stash_init(se);
    if(rc) return rc;
  }
  h = cwal_value_get_hash(se->stash);
  rc = cwal_hash_insert_v(h, key, v, 1 );
  if(!rc){
    rc = cwal_hash_grow_if_loaded(h, 0.75);
  }
  return rc;
}

int s2_stash_set( s2_engine * se, char const * key, cwal_value * v ){
  int rc;
  cwal_value * kv = cwal_new_string_value(se->e, key, cwal_strlen(key));
  if(kv){
    cwal_value_ref(kv);
    rc = s2_stash_set_v(se, kv, v);
    cwal_value_unref(kv);
  }else{
    rc = CWAL_RC_OOM;
  }
  return rc;
}

cwal_value * s2_stash_get2( s2_engine * se, char const * key,
                            cwal_size_t keyLen){
    if(!se || !key || !*key || !se->stash) return NULL;
    else {
      return cwal_hash_search( cwal_value_get_hash(se->stash),
                                key, keyLen );
    }
}

cwal_kvp * s2_stash_get2_kvp( s2_engine * se, char const * key,
                              cwal_size_t keyLen){
    if(!se || !key || !*key || !se->stash) return NULL;
    else {
      if(0==keyLen && *key) keyLen = cwal_strlen(key);
      return cwal_hash_search_kvp( cwal_value_get_hash(se->stash),
                                    key, keyLen );
    }
}

cwal_value * s2_stash_get( s2_engine * se, char const * key ){
  return s2_stash_get2(se, key, cwal_strlen(key));
}

cwal_value *
s2_stash_get_v( s2_engine * se, cwal_value const * key ){
    if(!se || !key) return NULL;
    else return se->stash
           ? cwal_hash_search_v( cwal_value_get_hash(se->stash),
                                 key )
           : 0;
}


#define HAS_ENUM_FLAG(ClientFlags) \
  (S2_VAL_F_CLASS_ENUM & (ClientFlags))
#define HAS_DOTLIKE_FLAG(ClientFlags) \
  ((S2_VAL_F_DOT_LIKE_OBJECT & (ClientFlags))   \
   || HAS_ENUM_FLAG(ClientFlags))


#if S2_TRY_INTERCEPTORS

/* static */ cwal_function * s2_value_is_interceptor( cwal_value const * v ){
  return (cwal_container_flags_get(v) & CWAL_CONTAINER_INTERCEPTOR)
    ? cwal_value_get_function(v)
    : 0;
}

/**
   Proxy for handling get/set interceptor calls.

   If S2_TRY_INTERCEPTORS is false, this is a no-op which returns 0,
   else...

   If s2_value_is_interceptor(func) then it is treated like an
   interceptor, calling the function on self and passing it the
   setterArg (if not NULL) or no arguments (assumed to be the getter
   call form). If rv and setterArg are not NULL, the result goes in
   *rv: the result of setters is ignored by the framework (the setter
   APIs simply have no way to communicate overridden results all the
   way back up the stack).
 */
static int s2__check_intercept( s2_engine * se,
                                cwal_value * propFoundIn,
                                cwal_value * self,
                                cwal_value * func,
                                cwal_value * setterArg,
                                cwal_value **rv ){
  int rc = 0;
  cwal_function * f = s2_value_is_interceptor(func);
  if(f){
    cwal_value * frv = 0;
    /*if(propFoundIn != self){
      s2_dump_val(propFoundIn,"propFoundIn");
      s2_dump_val(self,       "self       ");
      }*/
    rc = cwal_function_call2( f, propFoundIn,
                              self,
                              setterArg ? NULL : &frv,
                              setterArg ? 1 : 0,
                              setterArg ? &setterArg : NULL );
    if(rc){
      /* Error! We hope an exception was thrown so that
         the caller of this func can respond to it. */
      assert(!frv);
    }else{
      if(rv) *rv = frv ? frv : cwal_value_undefined();
      else if(frv){
        cwal_refunref(frv);
        frv = 0;
      }
    }
  }
  return rc;
}

#endif/* end S2_TRY_INTERCEPTORS */

/**
   Internal helper for s2_get_v_proxy2(). Fetches the n'th UTF8
   character from the given str, returning cwal_value_undefined() if n
   is out of range or the input appears to not be UTF8, and NULL on
   allocation error. The value is returned as a new length-1 string.

   TODO: negative values to count from the end, but then we'll
   also need to go patch string.charAt() for that.
*/
cwal_value * s2_charat_by_index( s2_engine *se,
                                 cwal_string const * str,
                                 cwal_int_t n ){
  cwal_midsize_t slen = 0;
  unsigned char const * cstr =
    (unsigned char const * )cwal_string_cstr2(str, &slen);
  unsigned int cp = 0;
  assert(cstr);
  if(n < 0 || (cwal_size_t)n >= cwal_string_length_bytes(str)){
    return cwal_value_undefined();
  }else if(cwal_string_is_ascii(str)){
    cp = cstr[n];
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
    else return cwal_new_string_value(se->e, (char const *)buf,
                                      (cwal_size_t)clen);
  }
}

/**
   Internal s2_get_v() impl. Possibly recursively looks up self's
   protototype chain until it finds the given property key. Implements
   several special-case lookups as well:

   - a hash-type 'self' container self with the "dot uses hash
   entries" flag searches hash entries first and falls back to
   property lookup.

   - If key is the string "prototype" then self's prototype is
   returned.

   - If self is-a string and key is-a integer then it does a
   character-at operation, returning the value as a length-1 string or
   (if the index is out of range) the undefined value (or NULL on
   allocation error).

   Reminder to self: this is what fixed the problem that in some
   callbacks, this.xyx was always resolving to undefined.

   if foundIn is not NULL then (A) prototype lookups are enabled and
   (B) *foundIn gets set to the prototype in which the property is
   found (which can be different from self).
*/
static cwal_value * s2_get_v_proxy2( s2_engine * se,
                                     cwal_value * self,
                                     cwal_value * key,
                                     cwal_value ** foundIn){
  cwal_value * v = NULL;
  cwal_hash * h = 0;
  cwal_value * foundAt = 0;
#if S2_TRY_INTERCEPTORS
  cwal_value * origin = self;
#endif
  
  uint16_t vflags = cwal_container_client_flags_get(self);
  assert(se && self && key);
  s2_engine_err_reset(se);
  /* s2_dump_val(key,"get_v_proxy key"); */
  if(s2_value_is_prototype_string(se, key)){
    /* x.prototype */
    return cwal_value_prototype_get( se->e, self );
  }else if(cwal_value_is_unique(self)
           && s2_value_is_value_string(se, key)){
    /* enum.entry.value */
    return cwal_unique_wrapped_get( self );
  }else if(cwal_value_is_string(self)
           && cwal_value_is_integer(key)){
    /* string[index] */
    cwal_int_t const n = cwal_value_get_integer(key);
    return s2_charat_by_index( se, cwal_value_get_string(self), n );
  }

#if 0 && S2_TRY_INTERCEPTORS
  cwal_prop_getX_v( self, key, &v );
#else
  while(!v && self){
    cwal_kvp const * kvp = 0;
    if(HAS_DOTLIKE_FLAG(vflags)
       && (h = cwal_value_get_hash(self))){
      kvp = cwal_hash_search_kvp_v(h, key);
#if 1
      /* Fall back to its own properties, instead of
         immediately going up to the prototype. */
      if(!kvp){
        kvp = cwal_prop_get_kvp_v(self, key, 0, NULL);
      }
#endif
    }else{
      kvp = cwal_prop_get_kvp_v(self, key, 0, NULL);
    }
    if(kvp) v = cwal_kvp_value(kvp);
    if(v) {
      foundAt = self;
      break;
    }
    else if(!foundIn) break;
    self = cwal_value_prototype_get(se->e, self);
    if(self) vflags = cwal_container_client_flags_get(self);
  }

  if(v){
    if(foundIn) *foundIn = foundAt;
#if S2_TRY_INTERCEPTORS
    s2__check_intercept( se, foundAt, origin, v, 0, &v );
#endif
  }
#endif
  return v;
}

cwal_value * s2_var_get_v( s2_engine * se, int scopeDepth,
                           cwal_value const * key ){
  return cwal_scope_search_v( cwal_scope_current_get(se->e),
                              scopeDepth, key, 0 );
}

cwal_value * s2_var_get( s2_engine * se, int scopeDepth,
                         char const * key, cwal_size_t keyLen ){
  return (!se || !key)
        ? 0
        : cwal_scope_search( cwal_scope_current_get(se->e),
                              scopeDepth, key,
                              keyLen, 0 );
}

int s2_var_set( s2_engine * se, int scopeDepth,
                char const * key, cwal_size_t keyLen,
                cwal_value * v ){
  return key
    ? cwal_scope_chain_set( cwal_scope_current_get(se->e),
                            scopeDepth, key, keyLen, v )
    : CWAL_RC_MISUSE;
}

int s2_var_set_v( s2_engine * se, int scopeDepth,
                  cwal_value * key, cwal_value * v ){
  return key
    ? cwal_scope_chain_set_v( cwal_scope_current_get(se->e),
                              scopeDepth, key, v )
    : CWAL_RC_MISUSE;
}

int s2_set_v( s2_engine * se, cwal_value * self,
              cwal_value * key, cwal_value * v ){

  return s2_set_with_flags_v(se, self, key, v,
                             CWAL_VAR_F_PRESERVE);
}

/**
   Internal proxy for s2_set_with_flags_v(). It handles the setting of
   Object properties and Hash entries if
   HAS_DOTLIKE_FLAG(clientFlags). kvpFlags are the flags to set on the
   property (e.g. CWAL_VAR_F_CONST and CWAL_VAR_F_HIDDEN are fairly
   common). Pass CWAL_VAR_F_PRESERVE to keep any existing flags.
*/
static int s2_set_prop_proxy(s2_engine * se,
                             cwal_value * self,
                             cwal_value * key, cwal_value * v,
                             uint16_t kvpFlags ){
  int rc = 0;
  uint16_t const clientFlags = cwal_container_client_flags_get(self);
  cwal_hash * h = HAS_DOTLIKE_FLAG(clientFlags)
    ? cwal_value_get_hash(self)
    : 0;
  if(se){/*avoid unused param warning*/}
#if !S2_TRY_INTERCEPTORS
  if(h){
    rc = v
      ? cwal_hash_insert_with_flags_v(h, key, v, 1, kvpFlags )
      : cwal_hash_remove_v(h, key);
  }else{
    rc = cwal_prop_set_with_flags_v( self, key, v, kvpFlags );
  }
#else
  if(!v){
    rc = h
      ? cwal_hash_remove_v(h, key)
      : cwal_prop_unset_v(self, key);
  }
  else if(!cwal_value_may_iterate(self)){
    /* compatibility crutch. Some (not all) of the 'set' code works
       fine without this, but we have tests which check for this
       condition. Plus, it matches what the s2 manual says. */
    return CWAL_RC_IS_VISITING;
  }
  else{
    cwal_value * foundIn = 0;
    cwal_kvp * kvp = h
      ? cwal_hash_search_kvp_v(h, key)
      : cwal_prop_get_kvp_v(self, key, 1, &foundIn);
    cwal_value * const fv = kvp ? cwal_kvp_value(kvp) : 0;
    cwal_function * f = 0;
    assert( kvp ? !!fv : 1 );
    /* s2_dump_val(key,"set key"); */
    /* s2_dump_val(fv,"set val"); */
    /* s2_dump_val(cwal_prop_get_v(self, key),"prop-get()"); */
    /*if(cwal_value_is_array(self)){
      s2_dump_val(self,"set self");
      }*/
    /*if(fv && cwal_value_is_function(fv)){
      s2_dump_val(fv,"fv");
      MARKER(("func with flags: %02x\n",
        cwal_container_client_flags_get(fv)));
    }*/
    if(!kvp && (clientFlags & S2_VAL_F_DISALLOW_UNKNOWN_PROPS)){
      rc = CWAL_RC_DISALLOW_NEW_PROPERTIES;
    }
    else if(fv && (f = s2_value_is_interceptor(fv))){
      /* MARKER(("setter interceptor!\n")); */
      rc = s2__check_intercept( se,
                                foundIn ? foundIn : self,
                                self, fv, v, 0 );
      /* Never reset kvpFlags for these */
    }
    else{
      if(!kvp && h){
        rc = cwal_hash_insert_with_flags_v(h, key, v, 1, kvpFlags);
      }
      else if(kvp && (h || foundIn==self)){
        rc = cwal_kvp_value_set2(kvp, v);
        if(!rc){
          cwal_value_rescope( cwal_value_scope(self), v )
            /* Without this rescope, we end up using a stale
               value at some point in some specific code
               constellations. */;
          cwal_kvp_flags_set( kvp, kvpFlags );
        }
      }
      else{
        rc = cwal_prop_set_with_flags_v( self, key, v, kvpFlags )
          /* ^^^ that requires a second property lookup internally,
             but with no prototype search. It's required to keep the
             "property assignment never overwrites inherited
             properties" behaviour.
          */
        ;
      }
    }
    assert(CWAL_RC_MISUSE != rc);
  }
#endif/* end S2_TRY_INTERCEPTORS */
  return rc;
}

int s2_set_with_flags_v( s2_engine * se, cwal_value * self,
                         cwal_value * key, cwal_value * v,
                         uint16_t kvpFlags ){
  int rc = 0;
  char const *errMsg = 0;
  /* if(!kvpFlags) kvpFlags = CWAL_VAR_F_PRESERVE; */
  s2_engine_err_reset(se);
  if(self){
    /* Object/property access */
    char const isKeyAnInt = cwal_value_is_integer(key);
    cwal_tuple * tp = 0;
#if 0
    /* 20191210: we may want to consider extending the
       CWAL_CONTAINER_DISALLOW_PROP_SET flag semantics to include
       array and tuple indexes. Noting, however, that tuples don't
       have container flags.
    */
    if(cwal_container_flags_get(self)
       & CWAL_CONTAINER_DISALLOW_PROP_SET){
      rc = CWAL_RC_DISALLOW_PROP_SET;
      errMsg = "Setting properties is disallowed on this value.";
    }else
#endif
#if 0
    /* Reminder we probably(?) don't(?) want this to guard prototype
       changing */
    if((rc = s2_immutable_container_check(se,self, 0))){
      return rc;
    }else
#endif
    if(isKeyAnInt && (tp=cwal_value_get_tuple(self))){
      uint16_t const tlen = cwal_tuple_length(tp);
      cwal_int_t const n = cwal_value_get_integer(key);
      if(n<0 || n>=(cwal_int_t)tlen){
        return s2_engine_err_set(se, CWAL_RC_RANGE,
                                 "Index %d is out of range "
                                 "for a length-%d tuple.",
                                 (int)n, (int)tlen);
      }
      rc = cwal_tuple_set(tp, (uint16_t)n, v);
      assert(!rc && "the only error cases involve memory corruption or bad args.");
    }
    else if(cwal_props_can(self)){ /* A container */
      cwal_array * ar;
      if(isKeyAnInt && (ar=cwal_value_array_part(se->e,self))){
        /* ==> Array[Index] */
        /*
          Reminder: cwal_value_array_part() ends up setting entries in
          arrays used as prototypes, but that is arguably expected.
        */
        cwal_int_t i;
        if((rc = s2_immutable_container_check(se,self, 0))){
          return rc;
        }
        i = cwal_value_get_integer(key);
        if(i<0){
          rc = CWAL_RC_RANGE;
          errMsg = "Array indexes may not be negative.";
        }else{
          /*MARKER("Setting array index #%u\n",(unsigned)i);
            s2_dump_val(v,"array entry");*/
          rc = cwal_array_set(ar, (cwal_size_t)i, v);
          if(rc) errMsg = "cwal_array_set() failed.";
        }
      }/* end array[int] */
      else if(s2_value_is_prototype_string(se, key)){
        /* Special case: prototype pseudo-keyword/property */
        if(!cwal_props_can(self)){
          /* Normally never reached b/c assignment ops catch
             this case, but non-assignment ops can also call
             this. */
          rc = CWAL_RC_ACCESS;
          errMsg = "Cannot re-assign prototypes of non-container types "
            "- they are fixed in place at the C level.";
        }
        else if(cwal_value_null()==v || cwal_value_undefined()==v){
          /* Special case: allow unsetting the prototype via
             assignment to these. Normally this would fail in
             cwal_value_prototype_set() because it expects (as a
             prototype) a container type or NULL. Maybe that
             limitation (in cwal) is unnecessary.
          */
          v = 0;
        }
        if(!rc){
          rc = cwal_value_prototype_set( self, v );
          switch(rc){
            case 0: break;
            case CWAL_RC_DISALLOW_PROTOTYPE_SET:
              errMsg = "Setting the prototype is disallowed on this value.";
              break;
            case CWAL_RC_CYCLES_DETECTED:
              errMsg = "Setting prototype would introduce a cycle in the prototype chain.";
              break;
            case CWAL_RC_TYPE:
              errMsg = "Invalid type for a prototype (only containers "
                "allowed, or null/undefined to remove the prototype).";
              break;
            default:
              errMsg = "cwal_value_prototype_set() failed";
              break;
          }
        }
      }/*prototype pseudo-property*/
      else{
        /* Set container property... */
        uint16_t const clientFlags = cwal_container_client_flags_get(self);
        if((rc = s2_immutable_container_check(se,self, 0))){
          return rc;
        }
        else if(HAS_ENUM_FLAG(clientFlags)){
          rc = CWAL_RC_DISALLOW_PROP_SET;
        }else{
          rc = s2_set_prop_proxy( se, self, key, v, kvpFlags );
        }
        /*if(rc){
          MARKER(("rc=%s\n", cwal_rc_cstr(rc)));
        }*/
        switch(rc){
          case CWAL_RC_OOM: break;
          case CWAL_RC_NOT_FOUND:{
            if(clientFlags & S2_VAL_F_DISALLOW_UNKNOWN_PROPS){
              cwal_size_t keyLen = 0;
              char const * keyStr = cwal_value_get_cstr(key,&keyLen);
              if(keyStr){
                return s2_engine_err_set(se, rc,
                                         "Unknown property '%.*s'.",
                                         (int)keyLen, keyStr);
              }else{
                return s2_engine_err_set(se, rc,
                                         "Unknown property with key type '%s'.",
                                         cwal_value_type_name(key));
              }
            }
            break;
          }
          case CWAL_RC_TYPE:
            if(cwal_prop_key_can(key)){
              return s2_engine_err_set(se, rc,
                                       "Invalid target type (%s) "
                                       "for assignment.",
                                       cwal_value_type_name(self));
            }else{
              return s2_engine_err_set(se, rc,
                                       "Type (%s) is not valid as a property key.",
                                       cwal_value_type_name(key));
            }
          case CWAL_RC_DISALLOW_NEW_PROPERTIES:
            return s2_engine_err_set(se, rc,
                                     "Container does not allow "
                                     "new properties.");
          case CWAL_RC_DISALLOW_PROP_SET:
            return s2_engine_err_set(se, rc,
                                     "Container is marked as immutable.");
          case CWAL_RC_CONST_VIOLATION:{
            cwal_size_t keyLen = 0;
            char const * keyStr = cwal_value_get_cstr(key,&keyLen);
            if(keyStr){
              return s2_engine_err_set(se, rc,
                                       "Cannot assign to const '%.*s'.",
                                       (int)keyLen, keyStr);
            }else{
              return s2_engine_err_set(se, rc,
                                       "Cannot assign to const property.",
                                       cwal_value_type_name(self));
            }
          }
          case CWAL_RC_LOCKED:{
            cwal_size_t len = 0;
            char const * tname = cwal_value_type_name2(v, &len);
            return s2_engine_err_set(se, rc,
                                     "'%.*s' value is currently locked against modification.",
                                     (int)len, tname);
          }
          case CWAL_RC_IS_VISITING_LIST:
            return s2_engine_err_set(se, rc,
                                     "Cannot perform this operation on a list/hash "
                                     "during traversal.");
          case CWAL_RC_IS_VISITING:
          case CWAL_RC_ACCESS/*historical*/:{
            return s2_engine_err_set(se, rc,
                                     "Cannot modify properties "
                                     "during traversal.");
          }
        }/*prop/hash set rc check*/
      }/*set property*/
    }else{
      cwal_size_t tlen = 0;
      char const * tn = cwal_value_type_name2(self, &tlen);
      return s2_engine_err_set(se, CWAL_RC_TYPE,
                               "Cannot set properties on "
                               "non-container type '%.*s'.",
                               (int)tlen, tn);
    }
  }else{
    /* Scope-level set */
    cwal_scope * s = cwal_scope_current_get(se->e);
#if 0
    s2_dump_val(key,"KEY Setting scope var");
    s2_dump_val(v,"VALUE Setting scope var");
    {
      cwal_kvp const * kvp = cwal_scope_search_kvp_v(s, -1, key, 0);
      if(kvp){
        s2_dump_val(cwal_kvp_key(kvp),"KEY Setting scope var");
        s2_dump_val(cwal_kvp_value(kvp),"VALUE Setting scope var");
        MARKER(("KVP flags=0x%04u\n", cwal_kvp_flags(kvp)));
      }
    }
#endif
    /**
       FIXME?: disable a SET on a scope variable which is undeclared in
       all scopes, throw an error in that case.  Requires a search+set
       (and set has to do its own search, again) or a flag to one of
       the cwal-level setters which tells it to fail if the var cannot
       be found. That's handled at the evaluation/operator level.
    */
    rc = cwal_scope_chain_set_with_flags_v( s,
                                           v ? -1 : 0 /* don't allow 'unset'
                                                         across scopes*/,
                                           key, v, kvpFlags );
    switch(rc){
      case 0:
      case CWAL_RC_EXCEPTION:
        break;
      case CWAL_RC_NOT_FOUND:
        assert(!v && "But the code said so!");
        /* if(!v) rc = 0; */
        break;
      case CWAL_RC_CONST_VIOLATION:{
        cwal_size_t keyLen = 0;
        char const * keyStr = cwal_value_get_cstr(key,&keyLen);
        if(keyStr){
          rc = s2_engine_err_set(se, rc,
                                 "Cannot assign to const '%.*s'.",
                                 (int)keyLen, keyStr);
        }else{
          errMsg = "Cannot assign to a const.";
        }
        break;
      }
      case CWAL_RC_DISALLOW_NEW_PROPERTIES:
        rc = s2_engine_err_set(se, rc,
                               "Scope's storage does not allow "
                               "new properties.");
        break;
      case CWAL_RC_DISALLOW_PROP_SET:
        rc = s2_engine_err_set(se, rc,
                               "Scope's storage is marked as immutable.");
        break;
      default:
        errMsg = "s2_var_set_v() failed.";
        break;
    }
  }
  if(errMsg && (CWAL_RC_EXCEPTION!=rc)){
    assert(rc);
    rc = s2_engine_err_set(se, rc, "%s", errMsg );
  }
  return rc;
}

int s2_set_with_flags( s2_engine * se, cwal_value * self,
                       char const * key, cwal_size_t keyLen,
                       cwal_value * v,
                       uint16_t flags ){
  int rc = 0;
  /* This impl removes lots of dupe code at the cost of a potentially
     temporary string. */
  cwal_value * kv;
  if(!se || !key) return CWAL_RC_MISUSE;
  kv = cwal_new_string_value(se->e, key, keyLen);
  rc = kv ? 0 : CWAL_RC_OOM;
  if(!rc) {
    cwal_value_ref(kv);
    rc = s2_set_with_flags_v( se, self, kv, v, flags );
    cwal_value_unref(kv);
  }
  return rc;
}


int s2_set( s2_engine * se, cwal_value * self,
            char const * key, cwal_size_t keyLen,
            cwal_value * v ){
  return s2_set_with_flags( se, self, key, keyLen, v, CWAL_VAR_F_PRESERVE );
}

#define s2__err(SE) (SE)->e->err

static int s2__get_check_exception(s2_engine * se, int rc){
  if(!rc){
    /* rc = s2_check_interrupted(se, rc); */
    if(!rc && cwal_exception_get(se->e)) rc = CWAL_RC_EXCEPTION;
    else if( s2__err(se).code ){
      rc = 1
        ? s2__err(se).code
        : s2_throw_err_ptoker(se, 0);
    }
  }
  return rc;
}


static cwal_value * s2_get_v_proxy( s2_engine * se,
                                    cwal_value * self,
                                    cwal_value * key ){
  cwal_value * foundIn = 0;
  return s2_get_v_proxy2(se, self, key, &foundIn);
}


/**
   C-string equivalent of s2_get_v_proxy().
*/
static cwal_value * s2_get_proxy( s2_engine * se,
                                  cwal_value * self,
                                  char const * key,
                                  cwal_size_t keyLen ){
#if 1
  /* quick/dirty hack to avoid having add S2_TRY_INTERCEPTORS
     support to this function just yet. This adds allocations,
     which is bad, of course. */
  cwal_value * ks = cwal_new_string_value(se->e, key, keyLen);
  cwal_value * rv = 0;
  s2_engine_err_reset(se);
  if(!ks){
    if(se->currentScript){
      s2_err_ptoker(se, se->currentScript, CWAL_RC_OOM, 0);
    }else{
      s2_error_set(se, 0, CWAL_RC_OOM, 0);
      assert(s2__err(se).code);
    }
    return 0;
  }
  cwal_value_ref(ks);
  rv = s2_get_v_proxy(se, self, ks);
  cwal_value_unref(ks);
  return rv;
#else
  cwal_hash * h = 0;
  cwal_value * v = 0;
  uint16_t vflags = 0;
  assert(se && self && key);
  if(9==keyLen && 'p'==*key && (0==memcmp( key, "prototype", 9 ))){
    return cwal_value_prototype_get( se->e, self );
  }
  else if(5==keyLen && 'v'==*key && (0==memcmp( key, "value", 5 ))
          && cwal_value_is_unique(self)){
    return cwal_unique_wrapped_get( self );
  }

  /* s2_dump_val(self,key); */
  while(!v && self){
    vflags = cwal_container_client_flags_get(self);
    if(HAS_DOTLIKE_FLAG(vflags)
       && (h = cwal_value_get_hash(self))){
      v = cwal_hash_search(h, key, keyLen);
#if 1
      /* Fall back to its own properties, instead of
         immediately going up to the prototype. */
      if(!v) v = cwal_prop_get(self, key, keyLen);
#endif
    }else{
      v = cwal_prop_get(self, key, keyLen);
    }
    self = v ? 0 : cwal_value_prototype_get(se->e, self);
    /* s2_dump_val(self,key); */
  }
  /* s2_dump_val(v,key); */
  return v;
#endif
}

int s2_get_v( s2_engine * se, cwal_value * self,
              cwal_value * key, cwal_value ** rv ){
  int rc = 0;
  cwal_value * xrv = 0;
  if(!self){
    /* do a scope lookup */
    xrv = s2_var_get_v( se, -1, key );
    rc = 0;
  }else{
    cwal_array * ar;
    cwal_tuple * tp = 0;
    char const isKeyAnInt = cwal_value_is_integer(key);
    assert(key);
    xrv = 0;
    if(isKeyAnInt
       && (ar=cwal_value_array_part(se->e,self))){
      cwal_int_t const i = cwal_value_get_integer(key);
      if(i<0){
        rc = s2_engine_err_set(se, CWAL_RC_RANGE,
                               "Array indexes may not be negative.");

      }else{
        xrv = cwal_array_get(ar, (cwal_size_t)i);
        rc = 0;
      }
    }else if(isKeyAnInt
             && (tp = cwal_value_get_tuple(self))){
      cwal_size_t const tlen = cwal_tuple_length(tp);
      cwal_int_t const n = cwal_value_get_integer(key);
      if(n<0 || n>=(cwal_int_t)tlen){
        return s2_engine_err_set(se, CWAL_RC_RANGE,
                                 "Index %d is out of range "
                                 "for a length-%d tuple.",
                                 (int)n, (int)tlen);
      }
      xrv = cwal_tuple_get(tp, (cwal_size_t)n);
      rc = 0;
    }
    else {
      if(isKeyAnInt && cwal_value_is_string(self)){
        /* string[index] */
        if(cwal_value_get_integer(key)<0){
          rc = s2_engine_err_set(se, CWAL_RC_RANGE,
                                 "String indexes may not be negative.");
        }else if(!(xrv = s2_get_v_proxy( se, self, key ))){
          rc = CWAL_RC_OOM;
        }else if( (rc = s2__get_check_exception(se, rc)) ){
          cwal_refunref(xrv);
          xrv = 0;
        }
      }else{
        /* Object property lookup... */
        xrv = s2_get_v_proxy( se, self, key );
        if( (rc = s2__get_check_exception(se, rc)) ){
          cwal_refunref(xrv);
          xrv = 0;
        }
      }
    }
    if(!rc && !xrv){
      uint16_t const selfFlags = cwal_container_client_flags_get(self);
      if(S2_VAL_F_DISALLOW_UNKNOWN_PROPS & selfFlags){
        cwal_size_t keyLen = 0;
        char const * keyStr = cwal_value_get_cstr(key,&keyLen);
        rc = keyLen
          ? s2_engine_err_set(se, CWAL_RC_NOT_FOUND,
                              /* DISALLOW_NEW_PROPERTIES? */
                              "Unknown property '%.*s'.",
                              (int)keyLen, keyStr)
          : s2_engine_err_set(se, CWAL_RC_NOT_FOUND,
                              "Unknown property with key type '%s'.",
                              cwal_value_type_name(key));
      }
    }
  }
  if(!rc) *rv = xrv;
  return rc;
}

int s2_get( s2_engine * se, cwal_value * self,
            char const * key, cwal_size_t keyLen,
            cwal_value ** rv ){
  int rc = 0;
  cwal_value * v = 0;
  if(!key) return CWAL_RC_MISUSE;
  else if(self){
    v = s2_get_proxy( se, self, key, keyLen );
    rc = s2__get_check_exception(se, rc);
    if(!rc){
      if(v){
        *rv = v;
      }
      else{
        uint16_t const selfFlags = cwal_container_client_flags_get(self);
        if(S2_VAL_F_DISALLOW_UNKNOWN_PROPS & selfFlags){
          rc = s2_engine_err_set(se, CWAL_RC_NOT_FOUND,
                                 "Unknown property '%.*s'.",
                                 (int)keyLen, key);
        }
      }
    }
  }else{
    v = s2_var_get( se, -1, key, keyLen );
    rc = s2__get_check_exception(se, rc);
    if(rc){
      assert(!v);
    }else{
      *rv = v;
    }
  }
  return rc;
}


int s2_install_core_prototypes(s2_engine * se){
  int rc = CWAL_RC_OOM;
  cwal_value * v;
  if(! (v = s2_prototype_object(se)) ) goto end;
  /* Object installs the other core ones, to try to stave off
     potential chicken/egg timing issues. */
  rc = 0;
  end:
  return rc;
}

int s2_throw_err_ptoker( s2_engine * se, s2_ptoker const * pr ){
  char const * errPos;
  s2_linecol_t line = 0, col = 0;
  if(!pr) pr = se->currentScript;
  assert(s2__err(se).code);
  assert(pr);
  errPos = se->opErrPos
    ? se->opErrPos
    : s2_ptoker_err_pos(pr);
  assert(errPos);
  s2_ptoker_count_lines( pr, errPos, &line, &col);
  return s2_throw_err(se, NULL, s2_ptoker_name_first(pr, 0),
                      line, col);
}

/** @internal

    Appends se->err with file/line column info based on (st ? st :
    se->currentScript).

    Returns se->err.code on success and CWAL_RC_OOM if allocating the
    script name part fails.
*/
int s2_err_ammend_flc(s2_engine * se, s2_ptoker const * st){
  int rc = 0;
  char const * name;
  cwal_size_t nameLen = 0;
  char const * errPos;
  s2_linecol_t line = 0, col = 0;
  cwal_error * const err = &s2__err(se);
  assert(err->code);
  if(!st) st = se->currentScript;
  errPos = se->opErrPos
    ? se->opErrPos
    : s2_ptoker_err_pos(st);
  assert(errPos);
  name = s2_ptoker_name_first(st, &nameLen);
  s2_ptoker_count_lines(st, errPos, &line, &col);
  err->line = line;
  err->col = col;
  err->script.used = 0;
  if(!rc && name){
    rc = cwal_buffer_append( se->e, &err->script, name, nameLen );
  }else if(err->script.capacity){
    err->script.mem[0] = 0;
  }
  return rc ? rc : err->code;
}

static int s2_err_ptoker_impl( s2_engine * se, char throwIt,
                               s2_ptoker const * st,
                               int code, char const * fmt,
                               va_list vargs ){
  int rc = 0;
  char const * name;
  cwal_size_t nameLen = 0;
  cwal_error * const err = &s2__err(se);
  cwal_buffer * obuf = &err->msg;
  s2_ptoker const * top = s2_ptoker_top_parent( st );
  char const * errPos = se->opErrPos
    ? se->opErrPos
    : s2_ptoker_err_pos(st);
  s2_linecol_t line = 0, col = 0;
  assert(errPos);
  if(errPos>=s2_ptoker_end(st)
     && s2_ptoker_end(st) > s2_ptoker_begin(st)){
    /* Shameless workaround for syntax errors at
       the end of a script */
    errPos = s2_ptoker_end(st)-1;
  }
  s2_engine_err_reset(se);
  err->code = code ? code : CWAL_RC_EXCEPTION;
  name = s2_ptoker_name_first(st, &nameLen);
  /* expand source range to include parent parsers,
     so that we get the right line/column numbers.
  */
  if(errPos>=s2_ptoker_begin(top) && errPos<s2_ptoker_end(top)){
    s2_ptoker_count_lines(st, errPos, &line, &col);
  }
  if(CWAL_RC_OOM==code){
    rc = CWAL_RC_OOM;
  }
  else if(!throwIt && errPos
     && !se->currentScript
     /*  ^^^^^^ elide location info from error string when it looks
         like a script-side exception is up-coming */
     ) {
    char const * tailPart = ": ";
    if(name){
      rc = cwal_buffer_printf( se->e, obuf, "%s:", name );
    }
    if(!rc && errPos < s2_ptoker_end(top)){
      if(name){
        rc = cwal_buffer_printf( se->e, obuf,
                                 "%d:%d%s",
                                 line, col, tailPart);
      }else{
        rc = cwal_buffer_printf( se->e, obuf,
                                 "line %d, col %d%s",
                                 line, col, tailPart);
      }
    }else if(errPos == s2_ptoker_end(top)){
      rc = cwal_buffer_printf( se->e, obuf,
                               "@ EOF%s", tailPart);
    }else{
      rc = cwal_buffer_printf( se->e, obuf,
                               "@ unknown source position%s",
                               tailPart);
    }
  }

  if(!rc){
    if(fmt && *fmt){
      rc = cwal_buffer_printfv(se->e, obuf, fmt, vargs);
    }else{
      rc = cwal_buffer_printf(se->e, obuf,
                              "Error #%d (%s)%s%s",
                              code, cwal_rc_cstr(code),
                              (st->errMsg ? ": " : ""),
                              st->errMsg ? st->errMsg : "");
    }
  }
  err->line = line;
  err->col = col;
  err->script.used = 0;
  if(!rc && name){
    rc = cwal_buffer_append( se->e, &err->script, name, nameLen );
  }
  if(!rc && throwIt){
    rc = s2_throw_err(se, err, name, line, col);
  }
  return s2_check_interrupted(se, rc ? rc : err->code);
}

int s2_err_ptoker( s2_engine * se, s2_ptoker const * st,
                   int code, char const * fmt, ... ){
  int rc;
  va_list args;
  va_start(args,fmt);
  rc = s2_err_ptoker_impl(se, 0, st, code, fmt, args);
  va_end(args);
  return rc;
}

int s2_throw_ptoker( s2_engine * se, s2_ptoker const * st,
                    int code, char const * fmt, ... ){
  int rc;
  va_list args;
  va_start(args,fmt);
  rc = s2_err_ptoker_impl(se, 1, st, code, fmt, args);
  va_end(args);
  return rc;
}


int s2_slurp_braces( s2_engine *se, s2_ptoker * st,
                     s2_ptoken * out ){
  int const opener = st->token.ttype;
  int closer;
  int rc = 0;
  int level = 1;
  s2_ptoken origSrc;
  s2_ptoken origPb;
  int adjustedOpener = opener;
  char const * typeLabel = 0;
  char const * end = 0;
  if(!out) out = &st->token;
  switch(opener){
    case S2_T_ParenOpen:
      closer = S2_T_ParenClose;
      adjustedOpener = S2_T_ParenGroup;
      typeLabel = "(PARENS)";
      break;
    case S2_T_BraceOpen:
      closer = S2_T_BraceClose;
      adjustedOpener = S2_T_BraceGroup;
      typeLabel = "[BRACES]";
      break;
    case S2_T_SquigglyOpen:
      adjustedOpener = S2_T_SquigglyBlock;
      closer = S2_T_SquigglyClose;
      typeLabel = "{SQUIGGLY}";
      break;
    default:
      return s2_err_ptoker(se, st, CWAL_RC_TYPE,
                                  "Invalid token type #%d (%s) for "
                                  "s2_slurp_braces()",
                                  opener, s2_ttype_cstr(opener));
  }
  origSrc = st->token;
  origPb = *s2_ptoker_putback_get(st);
  switch(opener){
    case S2_T_SquigglyOpen:
      /*
        consider: { ..., <<<X ... X, ... }
    
        The heredoc might contain stuff which confuses the parens.
        Because of that, we can't simply do a byte-traversal here,
        but have to go through s2_next_token().
      */
      /* fall through */
    case S2_T_ParenOpen:
    case S2_T_BraceOpen:{
      s2_ptoken errTok = st->token;
      for( ; (0==(rc=s2_next_token(se, st, 0, 0))); ){
        /*
          consider: ( ..., <<<X ... X, ... )
    
          The heredoc might contain stuff which confuses the parens.
          Because of that, we can't simply do a byte-traversal here,
          but have to go through s2_next_token().
        */
        s2_ptoken const * tok = &s2_ptoker_token(st)
          /* Reminder to self: this "could" be lifted outside of the
             loop because tok's address is constant, but eventual API
             changes might break that. */;
        end = s2_ptoken_end(tok);
        if(s2_ttype_is_eof(tok->ttype)){
          end = 0;
          break;
        }else{
          if(opener == tok->ttype){
            ++level;
            errTok = *tok;
          }
          else if(closer == tok->ttype){
            assert(level>0);
            if(!--level) break;
          }
        }
      }
      if(!rc && 0 != level){
        s2_ptoker_errtoken_set(st, &errTok);
        assert(typeLabel);
        rc = s2_err_ptoker( se, st, CWAL_SCR_SYNTAX,
                            "Unexpected EOF while slurping %s block.",
                            typeLabel);
      }
      break;
    }/*Parens/Braces*/
  }/* switch */

  if(!rc && !end){
    s2_ptoker_errtoken_set(st, &origSrc);

#if 0
    MARKER(("slurped <<%.*s>>\n",
            /* (int)s2_ptoken_len2(out), s2_ptoken_adjbegin(out), */
            (int)(s2_ptoken_end(&s2_ptoker_token(st)) - s2_ptoken_begin(&origSrc)),
            s2_ptoken_begin(&origSrc)));
#endif

    rc = s2_err_ptoker( se, st, CWAL_SCR_SYNTAX,
                        "Unexpected EOF or mismatched braces "
                        "while slurping %s block.",
                        typeLabel);
  }else if(!rc){
    assert(end);
    out->ttype = adjustedOpener;
    s2_ptoken_begin_set(out, s2_ptoken_begin(&origSrc));
    s2_ptoken_adjbegin_set(out, s2_ptoken_begin(out));
    s2_ptoken_end_set(out, end);
    assert(s2_ptoken_len(out) >= 2);
    assert(opener == (int)*s2_ptoken_begin(out));
    assert(closer == (int)*(s2_ptoken_end(out)-1));
    /* Skip leading whitespaces */
    while(((s2_ptoken_adjbegin_incr(out, 1)) < (end-1))
          && s2_is_space((int)*s2_ptoken_adjbegin(out))){
    }
    /* Skip trailing whitespaces */
    s2_ptoken_adjend_set(out, end - 1/* == closer */);
    while( ((s2_ptoken_adjend_incr(out,-1)) > s2_ptoken_adjbegin(out))
           && s2_is_space((int)*s2_ptoken_adjend(out)) ){
    }
    s2_ptoken_adjend_set(out, (s2_ptoken_adjend(out)+1))/* possibly back to the '}' byte */;
    out->ttype = adjustedOpener;
    rc = 0;
#if 0
    MARKER(("slurped <<%.*s>> out->adjEnd=%d\n",
            /* (int)s2_ptoken_len2(out), s2_ptoken_adjbegin(out), */
            (int)s2_ptoken_len(out), s2_ptoken_begin(out),
            *s2_ptoken_adjend(out)));
#endif
    assert(opener == *s2_ptoken_begin(out));
    assert(closer == *(s2_ptoken_end(out)-1));
    assert( s2_ptoken_adjend(out) >= s2_ptoken_adjbegin(out) );
    assert( s2_ptoken_adjend(out) <= s2_ptoken_end(out) );
  }
  if(out != &st->token){
    s2_ptoker_token_set(st, &origSrc);
    s2_ptoker_putback_set(st, &origPb);
  }
  return rc;
}

int s2_slurp_heredoc( s2_engine * se, s2_ptoker * st, s2_ptoken * tgt ){
  int rc = 0;
  char const * docBegin;
  char const * docEnd;
  char const * idBegin;
  char const * idEnd;
  char const * theEnd = NULL;
  s2_ptoken const origin = st->token;
  s2_ptoken tId = s2_ptoken_empty;
  cwal_size_t idLen;
  int modeFlag = 0;
  int const docType = st->token.ttype;
  assert(S2_T_HeredocStart==st->token.ttype
         || S2_T_HeredocStart2==st->token.ttype);
  if(!tgt) tgt = &st->token;
  if(S2_T_HeredocStart2 == docType){
    tId = st->token;
  }
  rc = s2_ptoker_next_token(st);
  if(rc) goto tok_err;
  else if(S2_T_Colon==st->token.ttype){
    modeFlag = st->token.ttype;
  }
  if(S2_T_HeredocStart2 == docType){
    if(!modeFlag){
      s2_ptoker_putback(st)
        /* Fixes line number if we just read an EOL. */;
    }
    idBegin = "}}}";
    idEnd = s2_ptoken_end(&origin) + (modeFlag ? 1 : 0);
    idLen = 3;
  }else{
    if(S2_T_Colon==modeFlag){
      rc = s2_ptoker_next_token(st);
      if(rc) goto tok_err;
    }
    tId = st->token;
    switch(st->token.ttype){
      case S2_T_Identifier:
      case S2_T_LiteralStringDQ:
      case S2_T_LiteralStringSQ:
        /* NO: case S2_T_LiteralStringBT: We don't want to give the
           impression that var expansion is performed in this context.*/
        break;
      default:
        rc = CWAL_SCR_SYNTAX;
        st->errMsg = "Expecting identifier or "
          "quoted string "
          "at start of HEREDOC.";
        goto tok_err;
    }
    idBegin = s2_ptoken_begin(&tId);
    idEnd = s2_ptoken_end(&tId);
    idLen = s2_ptoken_len(&tId);
  }
  docBegin = idEnd;
  switch(modeFlag){ /* Strip leading whitespace... */
    case S2_T_Colon:
      if('\n'==*docBegin || ('\r'==*docBegin && '\n'==docBegin[1])){
        docBegin += '\r'==*docBegin ? 2 : 1;
        ++st->currentLine;
        st->currentCol = 0;
      }else if(s2_is_space((int)*docBegin)){
        ++st->currentCol;
        ++docBegin;
      }
      break;
    default:
      while(s2_is_space((int)*docBegin)){
        if('\n'==*docBegin){
          ++st->currentLine;
          st->currentCol = 0;
        }else{
          ++st->currentCol;
        }
        ++docBegin;
      }
  }
  rc = CWAL_SCR_SYNTAX;
  for( docEnd = docBegin; docEnd < s2_ptoker_end(st); ++docEnd ){
    if(*docEnd != *idBegin){
      if('\n'==*docEnd){
        ++st->currentLine;
        st->currentCol = 0;
      }else{
        ++st->currentCol;
      }
      continue;
    }
    else if(0 == memcmp( docEnd, idBegin, idLen)){
      char const * back = docEnd-1;
      theEnd = docEnd + idLen;
      switch(modeFlag){
        case S2_T_Colon:
          if(back>=docBegin && ('\n'==*back || s2_is_space((int)*back))) --docEnd;
          break;
        default:
          for( ; back>=docBegin && s2_is_space((int)*back); --back) --docEnd;
      }
      rc = 0;
      st->currentCol += (s2_linecol_t)idLen;
      break;
    }
  }
  if(rc){
    s2_ptoker_errtoken_set(st, &tId);
    rc = s2_err_ptoker(se, st,
                       rc, "Did not find end of HEREDOC "
                       "starting with '%.*s'.",
                       (int)idLen, idBegin);
  }else{
    s2_ptoken_begin_set(tgt, s2_ptoken_begin(&origin));
    s2_ptoken_end_set(tgt, theEnd);
    tgt->ttype = S2_T_Heredoc;
    s2_ptoken_adjbegin_set(tgt, docBegin);
    s2_ptoken_adjend_set(tgt, docEnd);
    tgt->line = origin.line;
    tgt->column = origin.column;
    assert(docEnd>=docBegin);
#if 0
    idBegin = s2_ptoken_begin(&tId);
    idEnd = s2_ptoken_end(&tId);
    MARKER(("HEREDOC<%.*s> body: %.*s\n",
            (int)(idEnd-idBegin), idBegin,
            (int)(docEnd-docBegin), docBegin));
#endif
  }
  return rc;
  tok_err:
  assert(rc);
  assert(st->errMsg);
  rc = s2_err_ptoker(se, st, rc, "%s", st->errMsg);
  return rc;
}


int s2_ptoker_next_is_ttype( s2_engine * se, s2_ptoker * pr, int nextFlags, int ttype, char consumeIt ){
  s2_ptoken next = s2_ptoken_empty;
  s2_next_token( se, pr, nextFlags, &next);
  if(next.ttype==ttype){
    if(consumeIt){
      s2_ptoker_token_set(pr, &next);
    }
    return ttype;
  }else{
    s2_ptoker_next_token_set(pr, &next);
    return 0;
  }
}

static int s2_next_token_eol_skipper( int ttype ){
  switch(ttype){
    case S2_T_EOL:
    case S2_T_NL:
      return ttype;
    default:
      return s2_ttype_is_junk(ttype);
  }
}

static int s2_next_token_eol_noskipper( int ttype ){
  switch(ttype){
    case S2_T_EOL:
    case S2_T_NL:
      return 0;
    default:
      return s2_ttype_is_junk(ttype);
  }
}


int s2_next_token( s2_engine * se, s2_ptoker * st,
                   int flags,
                   s2_ptoken * tgt ){
  s2_ptoken tt = s2_ptoken_empty;
  s2_ptoken const oldT = st->token;
  s2_ptoken const oldP = *s2_ptoker_putback_get(st);
  s2_ttype_predicate_f const skipper =
    (S2_NEXT_NO_SKIP_EOL & flags)
    ? s2_next_token_eol_noskipper
    : s2_next_token_eol_skipper;
  int rc;
  ++se->metrics.nextTokenCalls;
  /* s2_engine_err_reset(se); */
  rc = s2_ptoker_lookahead_skip( st, &tt, skipper);
  rc = s2_check_interrupted(se, rc);
  if(rc){
    assert(st->errMsg || se->flags.interrupted);
    return se->flags.interrupted
      ? se->flags.interrupted
      : (s2__err(se).code
         ? s2__err(se).code
         : s2_err_ptoker( se, st, rc, "%s", st->errMsg ))
      ;
  }else{
    s2_ptoker_token_set(st, &tt);
    switch((S2_NEXT_NO_POSTPROCESS & flags) ? 0 : tt.ttype){
      case S2_T_Semicolon:
        st->token.ttype = S2_T_EOX;
        break;
      case S2_T_CR:
        assert(!"skipped by the junk-skipper!");
        CWAL_SWITCH_FALL_THROUGH;
      case S2_T_NL:
        assert( S2_NEXT_NO_SKIP_EOL & flags );
        st->token.ttype = S2_T_EOL;
        break;
      case S2_T_SquigglyOpen:
      case S2_T_ParenOpen:
      case S2_T_BraceOpen:
        /* Group these at the tokenization level to simplify
           evaluation. This completely removes open/close
           parens/braces handling from the eval side by evaluating
           these groups as a Value by recursively eval'ing their
           content.
        */
        rc = s2_slurp_braces(se, st, 0 );
        break;
      case S2_T_HeredocStart:
      case S2_T_HeredocStart2:
        rc = s2_slurp_heredoc(se, st, 0);
        assert(rc ? 1 : S2_T_Heredoc==st->token.ttype);
        break;
      default:
        break;
    }
  }
  if(rc && !s2_ptoker_errtoken_has(st)){
    s2_ptoker_errtoken_set(st, &st->token);
  }
  if(tgt){
    *tgt = st->token;
    s2_ptoker_token_set(st, &oldT);
    s2_ptoker_putback_set(st, &oldP);
  }else if(!rc){
    s2_ptoker_putback_set(st, &oldT);
  }
  return s2_check_interrupted(se, rc);
}


int s2_ptoken_create_value( s2_engine * se,
                            s2_ptoker const * pr,
                            s2_ptoken const * t,
                            cwal_value ** rv ){
  int rc = CWAL_RC_TYPE;
  cwal_value * v = NULL;
  cwal_engine * e = se->e;
#define RC rc = v ? CWAL_RC_OK : CWAL_RC_OOM
  switch(t->ttype){
    case S2_T_LiteralIntBin:
    case S2_T_LiteralIntDec:
    case S2_T_LiteralIntHex:
    case S2_T_LiteralIntOct:{
      cwal_int_t dd = 0;
      char const check = s2_ptoken_parse_int(t, &dd);
      assert(check &&
             "If this is false, then there's a mismatch "
             "between the tokenizer and converter.");
      if(check){/*avoid unused var warning in non-debug builds*/}
      v = cwal_new_integer(e, dd);
      RC;
      break;
    }
    case S2_T_LiteralDouble:{
      cwal_double_t dd = 0;
      s2_ptoken_parse_double( t, &dd);
      v = cwal_new_double(e, dd);
      RC;
      break;
    }
    case S2_T_LiteralStringSQ:
    case S2_T_LiteralStringDQ:{
      cwal_buffer * escapeBuf = &se->buffer;
      cwal_size_t const oldUsed = escapeBuf->used;
      cwal_size_t newLen;
      char const * begin;
      cwal_kvp * kvp = 0;
      assert(s2_ptoken_len(t)>=2 /* for the quotes */);
      rc = s2_unescape_string(e,
                              s2_ptoken_begin(t) + 1 /*quote*/,
                              s2_ptoken_end(t) - 1 /*quote*/,
                              escapeBuf );
      if(rc){
        rc = s2_err_ptoker(se, pr, CWAL_RC_RANGE==rc ? CWAL_SCR_SYNTAX : rc,
                           /*      allow catch() to block ^^^^^ this! */
                           "Unescaping string failed with rc=%s, "
                           "likely due to non-UTF8 content "
                           "or an unknown \\Uxxxxxxxx sequence.",
                           cwal_rc_cstr(rc))
          /**
             20191220: whether or not this should trigger
             s2_throw_ptoker() (exception) or s2_err_ptoker() (fatal)
             is debatable. It seems unfortunate that an invalid \U
             should outright kill a script, especially if it arrives
             via input from outside the script. Treating it as a
             non-exception using the code CWAL_SCR_SYNTAX, we enable
             catch{...} to optionally downgrade it to an exception.
          */;
        escapeBuf->used = oldUsed;
        break;
      }
      assert(escapeBuf->used >= oldUsed);

      /*MARKER("STRING: [%.*s]\n", (int)(escapeBuf->used - oldUsed),
        (char const *)escapeBuf->mem+oldUsed);*/
      assert(0 == escapeBuf->mem[escapeBuf->used]);
      newLen = escapeBuf->used - oldUsed;
      begin = newLen ? (char const *)(escapeBuf->mem+oldUsed) : 0;
      /* Check if we have a stashed string with this value. If so, use it.*/
      kvp = (begin && newLen<=10
             /*minor ^^^^^^^^^^^ optimization: max length of any of the
              * core cached strings*/)
        ? s2_stash_get2_kvp(se, begin, newLen)
        : 0;
      if(kvp){
        v = cwal_kvp_key(kvp) /* the KEY, not the value, else we end up
                                 doing really stupid stuff! */;
#if 0
        MARKER(("Reusing stashed string: %.*s\n", (int)newLen, begin ));
#endif
      }
      else{
        v = cwal_new_string_value(e, begin, newLen);
      }
      escapeBuf->used = oldUsed;
      /* s2_dump_val(v,"string literal"); */
      RC;
      break;
    }
    case S2_T_SquigglyBlock:{
      cwal_size_t const len = s2_ptoken_len2(t);
      char const * beg = s2_ptoken_adjbegin(t);
      char const * end = s2_ptoken_adjend(t);
      assert(beg);
      assert(end);
      assert(beg <= end);
      v = cwal_new_string_value(e, beg, len);
      /*MARKER(("SquigglyBlock ==> cwal_string: '{' %s '}'\n",
        cwal_value_get_cstr(v, 0)));*/
      RC;
      break;
    }
    default:{
      /* Use the raw token bytes as a string... */
      cwal_size_t const len = s2_ptoken_len2(t);
      char const * begin = s2_ptoken_begin2(t);
      char const * end = s2_ptoken_end2(t);
      cwal_kvp * kvp;
      assert(begin && end && begin <= end);
      /* If se->stash has a matching string in it, simply return
         that. If code calling this is too lax/lazy with refs, this
         will backfire horribly, as they fetch a value from here,
         thinking it's new, then deref it. */
      /* MARKER("STRING: [%.*s]\n", (int)(end - begin), begin); */
      /* MARKER("STRING: len=%ld\n", (long) (end - begin) ); */
      kvp = len ? s2_stash_get2_kvp(se, begin, len) : 0;
      if(kvp){
        v = cwal_kvp_key(kvp);
        /* MARKER(("got stashed string: %.*s\n", (int)len, begin)); */
        /* s2_dump_val(v,"got stashed string?"); */
      }
      else if(!v){
        /* didn't find a stashed copy, so create a new string... */
        v = cwal_new_string_value(e, begin, len);
      }
      /* s2_dump_val(v,"stringified token"); */
      RC;
      break;
    }
  }
  if(!rc) {
    assert(v);
    *rv = v;
  }else{
    assert(!v);
  }
  return rc;
#undef RC
}/* s2_ptoken_create_value() */

#if 0
/** @internal

    Deprecated, currenly unused.

    A helper for binary and unary operator overloading. All of the
    pointer arguments must point to valid memory.

    This routine looks for an operator Function in one of lhs or rhs
    (as described below) and, if found, calls it. opName is the name
    of the operator (the property name in script space) and opLen
    must be the non-0 length of that operator.

    Binary mode:

    If neither lhs nor rhs are NULL then:

    - the operator is treated like a binary call with lhs as the 'this'
    for the call and parameters (lhs, rhs)


    Unary prefix mode:

    If lhs is NULL then:

    - rhs must not be NULL

    - the operator is assumed to be treated like a unary prefix call on the
    rhs, and the operator function (if any) is called with rhs as the 'this'
    and _no_ arguments.


    Unary suffix mode:

    If lhs is not NULL and rhs is:

    - assume the op is a unary suffix operator for lhs and call it with
    lhs as 'this' and itself as an arg.

    If lhs (or rhs) has no such function, or finds a non-function,
    *theRc is set to 0 and 0 is returned.


    If it finds a function, it calls that function and sets *rv to its
    result value, *theRc to its return value, then returns non-0.

    On any error (running the function or fetching the operator) then
    *theRc is set to the error value and this function will return 0
    if the operator was not found or not called, and non-0 if it was
    called (in which case *theRc came from the operator function).
*/
char s2_value_op_proxy( s2_engine * se, char const * opName,
                        cwal_size_t opLen, cwal_value * lhs,
                        cwal_value * rhs, 
                        cwal_value **rv, int * theRc );
char s2_value_op_proxy( s2_engine * se, char const * opName, cwal_size_t opLen,
                        cwal_value * lhs, cwal_value * rhs, 
                        cwal_value **rv, int * theRc ){
  cwal_value * fv = 0;
  int rc;
  cwal_value * theThis = lhs ? lhs : rhs;
  cwal_function * fn;
  assert(se && se->e);
  assert(opName && opLen);
  assert(theRc);
  assert(rv);
  assert(lhs || rhs);
  /*
    Check for member function...
  */
  rc = s2_get( se, theThis, opName, opLen, &fv );
  if(rc || !fv){
    *theRc = rc;
    return 0;
  }
  else if((fn = cwal_value_function_part(se->e, fv))){
    cwal_value * argv[2] = {0,0};
    int argc = (lhs && rhs) ? 2 : (lhs ? 1 : 0);
    argv[0] = argc ? lhs : 0;
    argv[1] = argc ? rhs : 0;
    /* s2_dump_val(theThis,opName); */
    /* s2_dump_val(argv[0],opName); */
    /* s2_dump_val(argv[1],opName); */
    *theRc = cwal_function_call( fn, theThis, rv, argc, argv );
    return 1;
  }else{
    /* A non-function value with that name? */
    *theRc = 0;
    return 0;
  }
}
#endif
/* currently unused, but potentially still useful, code */

int s2_value_to_buffer( cwal_engine * e, cwal_buffer * buf, cwal_value * arg ){
  int rc = 0;
  switch(cwal_value_type_id(arg)){
    case CWAL_TYPE_STRING:{
      cwal_string const * s = cwal_value_get_string(arg);
      rc = cwal_buffer_append( e, buf, cwal_string_cstr(s),
                               cwal_string_length_bytes(s) );
      break;
    }
    case CWAL_TYPE_BOOL: {
      char const b = cwal_value_get_bool(arg);
      rc = cwal_buffer_append( e, buf,
                               b ? "true" : "false",
                               b ? 4 : 5);
      break;
    }
    case CWAL_TYPE_UNDEF:
      rc = cwal_buffer_append( e, buf, "undefined", 9);
      break;
    case CWAL_TYPE_NULL:
      rc = cwal_buffer_append( e, buf, "null", 4);
      break;
    case CWAL_TYPE_INTEGER:
      rc = cwal_buffer_printf( e, buf, "%"CWAL_INT_T_PFMT,
                               cwal_value_get_integer(arg));
      break;
    case CWAL_TYPE_DOUBLE:
      rc = cwal_buffer_printf( e, buf, "%"CWAL_DOUBLE_T_PFMT,
                               cwal_value_get_double(arg));
      if(!rc){
        /* Trim trailing zeroes... */
        unsigned char * pos = buf->mem + buf->used - 1;
        while(pos>buf->mem && '0' == *pos && '.' != *(pos-1)) {
          *pos = 0;
          --pos;
          --buf->used;
        }
      }
      break;
    case CWAL_TYPE_BUFFER:{
      cwal_buffer const * vb = cwal_value_get_buffer(arg);
      assert(vb);
      if(vb->used){
        rc = cwal_buffer_reserve( e, buf, buf->used + vb->used + 1 )
          /* Preallocation required in case buf===vb */;
        if(!rc){
          if(vb==buf){
            memmove(buf->mem + buf->used , vb->mem, vb->used);
            buf->used *= 2;
            buf->mem[buf->used] = 0;
            break;
          }else{
            rc = cwal_buffer_append( e, buf, vb->mem, vb->used );
          }
        }
      }
      break;
    }
    case CWAL_TYPE_OBJECT:
    case CWAL_TYPE_ARRAY:
    case CWAL_TYPE_TUPLE:{
      cwal_output_buffer_state job = cwal_output_buffer_state_empty;
      job.e = e;
      job.b = buf;
      rc = cwal_json_output( arg, cwal_output_f_buffer, &job, NULL );
      break;
    }          
    case CWAL_TYPE_HASH:
    case CWAL_TYPE_FUNCTION:
    case CWAL_TYPE_NATIVE:
    case CWAL_TYPE_UNIQUE:
      rc = cwal_buffer_printf(e, buf, "%s@%p",
                              cwal_value_type_name(arg),
                              (void const*)arg);
      break;
    default:
      rc = cwal_exception_setf( e, CWAL_RC_TYPE,
                                "Don't know how to to-string arguments "
                                "of type '%s'.",
                                cwal_value_type_name(arg));
      break;
  }
  return rc;
}

int s2_cb_value_to_string( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  cwal_buffer buf = cwal_buffer_empty;
  rc = s2_value_to_buffer(args->engine, &buf, args->self);
  if(!rc
     && !(*rv = cwal_string_value(cwal_buffer_to_zstring( args->engine, &buf)))
     ){
    rc = CWAL_RC_OOM;
  }
  cwal_buffer_reserve(args->engine, &buf, 0);
  return rc;
}

static int s2_cb_to_json_token_impl( cwal_callback_args const * args,
                                     cwal_value **rv,
                                     char useSelf
                                     /*0=use args->argv[0], else args->self*/
                                     ){
  if(!args->argc && !useSelf){
    misuse:
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "'toJSONString' requires one argument.");
  }else{
    int rc = 0;
    cwal_value * v = NULL;
    cwal_value * vIndent;
    cwal_json_output_opt outOpt = cwal_json_output_opt_empty;
    cwal_buffer buf = cwal_buffer_empty;
    cwal_buffer * jb = NULL;
    cwal_size_t oldUsed;
    char selfBuf = 0;
    int indentIndex = -1;
    int cyclesIndex = -1;
    if(useSelf){
      jb = cwal_value_buffer_part(args->engine, args->self);
      /* Change semantics when this===Buffer */
      v = jb
        ? (args->argc ? args->argv[0] : NULL)
        : args->self;
      indentIndex = jb ? 1 : 0;
    }
    else{
      assert(args->argc);
      v = args->argv[0];
      indentIndex = 1;
    }
    if(!v) goto misuse;
    cyclesIndex = indentIndex + 1;
    assert(indentIndex >= 0);
    vIndent = (indentIndex >= (int)args->argc)
      ? NULL
      : args->argv[indentIndex];
    outOpt.cyclesAsStrings = (cyclesIndex < (int)args->argc)
      ? cwal_value_get_bool(args->argv[cyclesIndex])
      : 0;
    selfBuf = jb ? 1 : 0;
    if(!selfBuf) jb = &buf;
    oldUsed = jb->used;
    if(vIndent){
      /* Accept indentation as either a string, an integer, or an enum
         entry which wraps one of those.
      */
      vIndent = s2_value_unwrap(vIndent);
      if(!(outOpt.indentString.str =
           cwal_value_get_cstr(vIndent, &outOpt.indentString.len))){
        outOpt.indent = cwal_value_get_integer(vIndent);
      }
    }
    rc = cwal_json_output_buffer( args->engine, v,
                                  jb, &outOpt );
    if(CWAL_RC_CYCLES_DETECTED==rc){
      rc = cwal_exception_setf(args->engine, rc,
                               "Cycles detected in JSON output.");
    }else if(!rc){
      v = selfBuf
        /* ? cwal_new_integer(args->engine,
           (cwal_int_t)(jb->used - oldUsed))*/
        ? args->self
        /* TODO? Use a z-string and transfer the buffer memory? */
        : cwal_new_string_value( args->engine,
                                  ((char const *)jb->mem + oldUsed),
                                  jb->used - oldUsed );
      if(!v) rc = CWAL_RC_OOM;
      else *rv = v;
    }
    if(buf.mem) cwal_buffer_reserve( args->engine, &buf, 0 );
    return rc;
  }
}

int s2_cb_this_to_json_token( cwal_callback_args const * args, cwal_value **rv ){
    return s2_cb_to_json_token_impl( args, rv, 1 );
}

int s2_cb_arg_to_json_token( cwal_callback_args const * args, cwal_value **rv ){
    return s2_cb_to_json_token_impl( args, rv, 0 );
}




int s2_cb_value_compare( cwal_callback_args const * args, cwal_value **rv ){
  int rc;
  cwal_value * lhs;
  cwal_value * rhs;
  if(!args->argc || (args->argc>3)){
    return cwal_exception_setf( args->engine, CWAL_RC_MISUSE,
                                "Expecting (value [,value [,bool typeStrict]]) arguments.");
  }
  lhs = (args->argc > 1)
    ? args->argv[0]
    : args->self;
  rhs = (args->argc > 1)
    ? args->argv[1]
    : args->argv[0];
  if(lhs==rhs){
    *rv = cwal_new_integer(args->engine, 0) /* does not allocate */;
    return 0;
  }
  if((3==args->argc) && cwal_value_get_bool(args->argv[2])){
    cwal_type_id const tL = cwal_value_type_id(lhs);
    cwal_type_id const tR = cwal_value_type_id(rhs);
    if(tL != tR){
      rc = (int)(tL-tR);
      *rv = cwal_new_integer(args->engine, (cwal_int_t)(tL<tR ? -1 : 1))
        /* does not allocate */;
      return 0;
    }
  }
  rc = cwal_value_compare(lhs, rhs);
  /* Minor optimization:
     
     Normalize rc to one of (-1, 0, 1) because we just happen to
     know that cwal_new_integer() does not allocate for those 3
     values. cwal_value_compare() does not guaranty any specific
     values, only general-purpose comaparator semantics.
  */
  *rv = cwal_new_integer(args->engine, (cwal_int_t)(rc>0 ? 1 : (rc < 0 ? -1 : 0)))
    /* does not allocate */;
  return 0;
}

cwal_engine * s2_engine_engine(s2_engine * se){
  return se ? se->e : 0;
}


void s2_engine_subexpr_restore(s2_engine * e,
                               s2_subexpr_savestate const * from){
  e->ternaryLevel = from->ternaryLevel;
}

int s2_scope_push_with_flags( s2_engine * se, cwal_scope * tgt,
                              uint16_t scopeFlags ){
  int rc = s2_check_interrupted(se, 0);
  if(rc) return rc;
  assert(se);
  assert(se->e);
  assert(tgt);
  assert(!tgt->level);
  assert(!tgt->e);
  if(!se || !se->e || !tgt || tgt->e || tgt->level) return CWAL_RC_MISUSE;
  se->scopes.nextFlags = scopeFlags;
  rc = cwal_scope_push( se->e, &tgt );
  if(!rc){
    assert(tgt->level);
    assert(s2__scope_current(se)->flags == scopeFlags);
    assert(tgt == s2__scope_for_level(se, tgt->level)->cwalScope);
    assert(0 == se->scopes.nextFlags);
  }else{
    se->scopes.nextFlags = 0;
  }
  return rc;
}

s2_engine * s2_engine_from_state( cwal_engine * e ){
  return (s2_engine *) cwal_engine_client_state_get( e, &s2_engine_empty );
}

s2_engine * s2_engine_from_args( cwal_callback_args const * args ){
  return (s2_engine *) cwal_engine_client_state_get( args->engine, &s2_engine_empty );
}

static int s2_cb_json_parse_impl( cwal_callback_args const * args,
                                  cwal_value **rv, char isFilename ){
  int rc;
  cwal_value * root = NULL;
  cwal_size_t slen = 0;
  cwal_json_parse_info pInfo = cwal_json_parse_info_empty;
  char const * cstr;
  cwal_value * arg = args->argc ? args->argv[0] : args->self;
  cwal_engine * e = args->engine;
  if(isFilename && (rc = s2_cb_disable_check(args, S2_DISABLE_FS_READ))){
    return rc;
  }
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

int s2_cb_json_parse_string( cwal_callback_args const * args, cwal_value **rv ){
  return s2_cb_json_parse_impl(args, rv, 0);
}

int s2_cb_json_parse_file( cwal_callback_args const * args, cwal_value **rv ){
  return s2_cb_json_parse_impl(args, rv, 1);
}

int s2_install_json( s2_engine * se, cwal_value * tgt, char const * key ){
  cwal_value * mod = NULL;
  int rc;
  if(!se || !tgt) return CWAL_RC_MISUSE;
  else if(!cwal_props_can(tgt)) return CWAL_RC_TYPE;

  if(key && *key){
    mod = cwal_new_object_value(se->e);
    if(!mod) return CWAL_RC_OOM;
    if( (rc = cwal_prop_set(tgt, key, cwal_strlen(key), mod)) ){
      cwal_value_unref(mod);
      return rc;
    }
  }else{
    mod = tgt;
  }

  cwal_value_ref(mod);
  {
    s2_func_def const funcs[] = {
    /* S2_FUNC2("experiment", s2_cb_container_experiment), */
      S2_FUNC2("parse", s2_cb_json_parse_string),
      S2_FUNC2("parseFile", s2_cb_json_parse_file),
      S2_FUNC2("stringify", s2_cb_arg_to_json_token),
      s2_func_def_empty_m
    };
    rc = s2_install_functions(se, mod, funcs, 0);
    if(!rc){
      /* Install mod.clone() convenience method */
      rc = s2_eval_cstr_with_var(se, "J", mod, "JSON module init",
                                 "J.clone=proc()"
                                 "using.{P:J.parse,S:J.stringify}"
                                 "{return using.P(using.S(argv.0))}",
                                 -1, 0);
    }
  }
  if(rc){
    if(tgt==mod) cwal_value_unhand(mod);
    else cwal_value_unref(mod);
  }else{
    cwal_value_unhand(mod);
  }
  return rc;
}

int s2_cb_getenv( cwal_callback_args const * args, cwal_value **rv ){
  cwal_size_t len = 0;
  char const * key = args->argc
    ? cwal_value_get_cstr(args->argv[0], &len)
    : 0;
  if(!key){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting a string argument.");
  }else{
    char const * val = getenv(key);
    *rv = val
      ? cwal_new_string_value(args->engine, val, cwal_strlen(val))
      /* ^^^ we "could" use x-strings and save a few bytes
         of memory, but if we add setenv() we'd likely shoot
         ourselves in the foot here.
      */
      : cwal_value_undefined();
    return *rv ? 0 : CWAL_RC_OOM;
  }
}

int s2_cb_rc_hash( cwal_callback_args const * args, cwal_value **rv ){
  s2_engine * se = s2_engine_from_args(args);
  int rc;
  cwal_int_t i;
  cwal_hash * h;
  cwal_value * hv;
  static const char * stashKey = "RcHash";
  assert(se);
  hv = s2_stash_get(se, stashKey);
  if(hv){
    *rv = hv;
    return 0;
  }
  h = cwal_new_hash(args->engine, 137)
    /*the installed set has 110 entries as of 20171013*/;
  if(!h) return CWAL_RC_OOM;
  hv = cwal_hash_value(h);
  cwal_value_ref(hv);
  for( i = 0; i <= (cwal_int_t)S2_RC_CLIENT_BEGIN; ++i ){
    char const * str = cwal_rc_cstr2(i);
    if(str){
      cwal_value * k;
      cwal_value * v = cwal_new_xstring_value(args->engine,
                                              str, cwal_strlen(str));
      if(!v){
        rc = CWAL_RC_OOM;
        break;
      }
      cwal_value_ref(v);
      k = cwal_new_integer(args->engine, i);
      if(!k){
        cwal_value_unref(v);
        rc = CWAL_RC_OOM;
        break;
      }
      cwal_value_ref(k);
      rc = cwal_hash_insert_v( h, k, v, 0 );
      cwal_value_unref(k);
      cwal_value_unref(v);
      if(rc) break;
#if 1
      /* reverse mapping. */
      rc = cwal_hash_insert_v( h, v, k, 0 );
      if(rc) break;
#endif
    }
  }
  if(rc){
    cwal_value_unref(hv);
  }else{
    *rv = hv;
    s2_stash_set( se, stashKey, hv )
      /* if this fails, no big deal */;
    cwal_value_unhand(hv);
  }
  return 0;
}

int s2_cb_new_unique( cwal_callback_args const * args, cwal_value **rv ){
  *rv = cwal_new_unique(args->engine, args->argc ? args->argv[0] : 0);
  return *rv ? 0 : CWAL_RC_OOM;
}

int s2_interrupt( s2_engine * se ){
  s2_ptoker const * pt = se->currentScript;
  if(pt){
    /* fake an error pos which s2_err_ptoker() will see... */
    se->opErrPos = s2_ptoken_begin(&pt->token)
      ? s2_ptoken_begin(&pt->token) : s2_ptoker_begin(pt);
    s2_err_ptoker( se, pt, CWAL_RC_INTERRUPTED,
                   "Interrupted by s2_interrupt().");
    se->opErrPos = 0;      
  }else{
    s2_engine_err_set(se, CWAL_RC_INTERRUPTED,
                      "Interrupted by s2_interrupt().");
  }
  return se->flags.interrupted = CWAL_RC_INTERRUPTED;
}

#if S2_USE_SIGNALS
/**
   s2_engine interruption via signals...

   Potential TODO: instead of manipulating an s2_engine instance
   directly from the signal handler, which has potential
   timing-related issues vis-a-vis s2_engine lifetime, simply set a
   global flag here and check/consume it from the s2_engine APIs. The
   disadvantage to that is that we get less precise interruption
   location information (limited to where an engine explicitly checks
   it), but that seems like a small price to pay for "more correct"
   s2_engine lifetime interaction.
*/
static void s2_sigc_handler(int s){
  if(s2Interruptable && !s2Interruptable->flags.interrupted){
    s2_engine * se = s2Interruptable;
    s2_ptoker const * pt = se ? se->currentScript : 0;
    s2Interruptable = 0 /* disable concurrent interruption */;
    if(pt){
      cwal_error * const err = &s2__err(se);
      MARKER(("Interruping via signal handler!\n"));
      /* fake an error pos which s2_err_ptoker() will see... */
      se->opErrPos = s2_ptoken_begin(&pt->token)
        ? s2_ptoken_begin(&pt->token) : s2_ptoker_begin(pt)
        /* kludge for the following call to get an error location */;
      s2_err_ptoker( se, pt, CWAL_RC_INTERRUPTED,
                     "Interrupted by signal #%d.", s);
      se->opErrPos = 0;
      if(err->line>0){
        /* We do this to help track down locations where interruption is
           triggered but does not preempt the normal result code of the
           interrupted operation like it should.
        */
        MARKER(("Interrupt was at line %d, column %d of '%.*s'.\n",
                err->line, err->col,
                (int)err->script.used, (char const *)err->script.mem));
      }
    }else if(se){
      s2_engine_err_set(se, CWAL_RC_INTERRUPTED,
                        "Interrupted by signal #%d.", s);
    }
    if(se){
      se->flags.interrupted = CWAL_RC_INTERRUPTED;
    }
    assert(!s2Interruptable);
    s2Interruptable = se /* re-enable interruption */;
  }
}
#endif
/* S2_USE_SIGNALS */

int s2_check_interrupted( s2_engine * se, int rc ){
  switch(rc){
    case CWAL_RC_ASSERT:
    case CWAL_RC_CANNOT_HAPPEN:
    case CWAL_RC_EXIT:
    case CWAL_RC_FATAL:
    case CWAL_RC_INTERRUPTED:
    case CWAL_RC_OOM:
      return rc;
    default:
        return se->e->fatalCode
          ? (se->flags.interrupted = se->e->fatalCode)
          : (se->flags.interrupted
             ? se->flags.interrupted
             : rc);
  }
}

void s2_set_interrupt_handlable( s2_engine * se ){
#if S2_USE_SIGNALS
  if((s2Interruptable = se)){
    struct sigaction sigIntHandler;
    sigIntHandler.sa_handler = s2_sigc_handler;
    sigemptyset(&sigIntHandler.sa_mask);
    sigIntHandler.sa_flags = 0;
    sigaction(SIGINT, &sigIntHandler, NULL);
  }
#endif
}

bool s2_cstr_to_rc(char const *str, cwal_int_t len, int * code){
  cwal_size_t const n = (len<0) ? cwal_strlen(str) : (cwal_size_t)len;
  if(n<10/*length of shortest entry: CWAL_RC_OK*/) return 0;
  switch(s2_hash_keyword(str, n)){
#define W(RC) if((cwal_size_t)sizeof(#RC)-1 == n                \
                     && 0==cwal_compare_cstr(str, n, #RC, n))   \
    { *code = RC; return 1; } else return 0
    /* Generated by s2-keyword-hasher.s2 (or equivalent): */

    case 0x00014848: W(CWAL_RC_OK);
    case 0x000a4d67: W(CWAL_RC_ERROR);
    case 0x000292ae: W(CWAL_RC_OOM);
    case 0x000a4761: W(CWAL_RC_FATAL);
    case 0x005266b4: W(CWAL_RC_CONTINUE);
    case 0x000a47d0: W(CWAL_RC_BREAK);
    case 0x0014a578: W(CWAL_RC_RETURN);
    case 0x000525e8: W(CWAL_RC_EXIT);
    case 0x00a4e143: W(CWAL_RC_EXCEPTION);
    case 0x00149814: W(CWAL_RC_ASSERT);
    case 0x0014a19c: W(CWAL_RC_MISUSE);
    case 0x00a552b9: W(CWAL_RC_NOT_FOUND);
    case 0x1493fc9a: W(CWAL_RC_ALREADY_EXISTS);
    case 0x000a4d4e: W(CWAL_RC_RANGE);
    case 0x00052a2e: W(CWAL_RC_TYPE);
    case 0x029615ff: W(CWAL_RC_UNSUPPORTED);
    case 0x001488a0: W(CWAL_RC_ACCESS);
    case 0x02953c70: W(CWAL_RC_IS_VISITING);
    case 0x52a80e30: W(CWAL_RC_IS_VISITING_LIST);
    case 0x5259accb: W(CWAL_RC_DISALLOW_NEW_PROPERTIES);
    case 0x5259b77c: W(CWAL_RC_DISALLOW_PROP_SET);
    case 0x5259c2c8: W(CWAL_RC_DISALLOW_PROTOTYPE_SET);
    case 0x2938b265: W(CWAL_RC_CONST_VIOLATION);
    case 0x00149b62: W(CWAL_RC_LOCKED);
    case 0x2936e803: W(CWAL_RC_CYCLES_DETECTED);
    case 0x526013da: W(CWAL_RC_DESTRUCTION_RUNNING);
    case 0x00a4b185: W(CWAL_RC_FINALIZED);
    case 0x149b07dc: W(CWAL_RC_HAS_REFERENCES);
    case 0x02941f9f: W(CWAL_RC_INTERRUPTED);
    case 0x00a469dd: W(CWAL_RC_CANCELLED);
    case 0x00014804: W(CWAL_RC_IO);
    case 0x0a48d963: W(CWAL_RC_CANNOT_HAPPEN);
    case 0x5297a321: W(CWAL_RC_JSON_INVALID_CHAR);
    case 0x5297ae5a: W(CWAL_RC_JSON_INVALID_KEYWORD);
    case 0x5297c0c2: W(CWAL_RC_JSON_INVALID_ESCAPE_SEQUENCE);
    case 0x5297c884: W(CWAL_RC_JSON_INVALID_UNICODE_SEQUENCE);
    case 0x5297adcb: W(CWAL_RC_JSON_INVALID_NUMBER);
    case 0x529835fe: W(CWAL_RC_JSON_NESTING_DEPTH_REACHED);
    case 0x529901c6: W(CWAL_RC_JSON_UNBALANCED_COLLECTION);
    case 0x52979d2b: W(CWAL_RC_JSON_EXPECTED_KEY);
    case 0x5297a088: W(CWAL_RC_JSON_EXPECTED_COLON);
    case 0x293361be: W(CWAL_SCR_CANNOT_CONSUME);
    case 0x029429d7: W(CWAL_SCR_INVALID_OP);
    case 0x52a35f1b: W(CWAL_SCR_UNKNOWN_IDENTIFIER);
    case 0x5266c911: W(CWAL_SCR_CALL_OF_NON_FUNCTION);
    case 0x5289396d: W(CWAL_SCR_MISMATCHED_BRACE);
    case 0x528deb5d: W(CWAL_SCR_MISSING_SEPARATOR);
    case 0x52a00aa1: W(CWAL_SCR_UNEXPECTED_TOKEN);
    case 0x294ffd4d: W(CWAL_SCR_UNEXPECTED_EOF);
    case 0x0527f4f2: W(CWAL_SCR_DIV_BY_ZERO);
    case 0x00295497: W(CWAL_SCR_SYNTAX);
    case 0x0005251c: W(CWAL_SCR_EOF);
    case 0x52a959aa: W(CWAL_SCR_TOO_MANY_ARGUMENTS);
    case 0x52859352: W(CWAL_SCR_EXPECTING_IDENTIFIER);
    case 0x512547c8: W(S2_RC_END_EACH_ITERATION);
    case 0x000146f8: W(S2_RC_TOSS);
#undef W
  }
  return 0;
}

void s2_value_upscope( s2_engine * se, cwal_value * v ){
  s2_scope * sc = s2__scope_current(se);
  assert(sc);
  if(sc && sc->cwalScope->parent){
#if 1
    cwal_value_upscope(v);
#else
    cwal_value_rescope(sc->cwalScope->parent, v);
#endif
  }
}

int s2_immutable_container_check_cb( cwal_callback_args const * args, cwal_value const * v ){
  return s2_immutable_container_check( s2_engine_from_args(args), v, 1 );
}

int s2_immutable_container_check( s2_engine * se, cwal_value const * v, int throwIt ){
  cwal_flags16_t const containerFlags = cwal_container_flags_get(v);
  assert(se && se->e);
  if(CWAL_CONTAINER_DISALLOW_PROP_SET & containerFlags){
    char const * fmt = "Setting/clearing properties is "
      "disallowed on this container (of type '%s').";
    char const * typeName = cwal_value_type_name(v);
    if(!typeName) typeName = "<? ? ?>";
    if(throwIt){
      return s2_throw(se, CWAL_RC_DISALLOW_PROP_SET,
                      fmt, typeName);
    }else{
      return s2_engine_err_set(se, CWAL_RC_DISALLOW_PROP_SET,
                               fmt, typeName);
    }
  }
  return 0;
}

int s2_install_value( s2_engine *se, cwal_value * tgt,
                      cwal_value * v,
                      char const * name,
                      int nameLen,
                      uint16_t propertyFlags ){
  int rc;
  cwal_value * vname;
  if(nameLen<0) nameLen = (int)cwal_strlen(name);
  if(!se || !v || !name) return CWAL_RC_MISUSE;
  else if(!nameLen) return CWAL_RC_RANGE;
  else if(tgt && !cwal_props_can(tgt)) return CWAL_RC_TYPE;
  vname = cwal_new_string_value(se->e, name, (cwal_size_t)nameLen);
  if(!vname) return CWAL_RC_OOM;
  /**
     We use s2_set_xxx() to honor the dot-like-object flag on hashes.

     OTOH: 1) that's the only place in this function we use the
     s2_engine object and 2) the dot-like-object bits have been
     removed from the public scripting API and are internally used
     only for enums (which are immutable, so we can't install
     functions into them). 3) we can derive an s2_engine from a
     cwal_engine with s2_engine_from_state(). That argues for
     changing this signature to take a cwal_engine instead. We'll
     try that one day.
  */
  cwal_value_ref(vname);
  rc = tgt
    ? s2_set_with_flags_v( se, tgt, vname, v, propertyFlags )
    : cwal_scope_chain_set_with_flags_v(cwal_scope_current_get(se->e),
                                        0, vname, v, propertyFlags);
  cwal_value_unref(vname);
  return rc;
}

int s2_install_callback( s2_engine *se, cwal_value * tgt,
                         cwal_callback_f callback,
                         char const * name,
                         int nameLen, 
                         uint16_t propertyFlags,
                         void * state,
                         cwal_finalizer_f stateDtor,
                         void const * stateTypeID ){
  int rc = 0;
  cwal_value * fv;
  if(!se || !callback) return CWAL_RC_MISUSE;
  else if(tgt && !cwal_props_can(tgt)) return CWAL_RC_TYPE;
  fv = cwal_new_function_value(se->e, callback, state, stateDtor,
                               stateTypeID);
  if(fv){
    cwal_value_ref(fv);
    rc = s2_install_value( se, tgt, fv, name, nameLen, propertyFlags );
    cwal_value_unref(fv);
  }else{
    rc = CWAL_RC_OOM;
  }
  return rc;
}
                          

int s2_install_functions( s2_engine *se, cwal_value * tgt,
                          s2_func_def const * defs,
                          uint16_t propertyFlags ){
  int rc = 0;
  if(!se || !defs) return CWAL_RC_MISUSE;
  else if(tgt && !cwal_props_can(tgt)) return CWAL_RC_TYPE;
  for( ; !rc && defs->name; ++defs ){
    cwal_value * fv = cwal_new_function_value( se->e, defs->callback,
                                               defs->state,
                                               defs->stateDtor,
                                               defs->stateTypeID);
    if(!fv){
      rc = CWAL_RC_OOM;
      break;
    }
    cwal_value_ref(fv);
    rc = s2_install_value( se, tgt, fv, defs->name, -1,
                           propertyFlags );
    if(!rc && defs->cwalContainerFlags){
      cwal_container_client_flags_set( fv, defs->cwalContainerFlags );
    }
    cwal_value_unref(fv);
  }
  return rc;
}

int s2_container_config( cwal_value * v, char allowPropSet,
                         char allowNewProps,
                         char allowGetUnknownProps ){
  if(!cwal_props_can(v)) return CWAL_RC_TYPE;
  else{
    cwal_flags16_t containerFlags = cwal_container_flags_get(v);
    cwal_flags16_t clientFlags = cwal_container_client_flags_get(v);
    if(allowPropSet){
      containerFlags &= ~CWAL_CONTAINER_DISALLOW_PROP_SET;
    }else{
      containerFlags |= CWAL_CONTAINER_DISALLOW_PROP_SET;
    }
    if(allowNewProps){
      containerFlags &= ~CWAL_CONTAINER_DISALLOW_NEW_PROPERTIES;
    }else{
      containerFlags |= CWAL_CONTAINER_DISALLOW_NEW_PROPERTIES;
    }
    if(allowGetUnknownProps){
      clientFlags &= ~S2_VAL_F_DISALLOW_UNKNOWN_PROPS;
    }else{
      clientFlags |= S2_VAL_F_DISALLOW_UNKNOWN_PROPS;
    }
    cwal_container_flags_set(v, containerFlags);
    cwal_container_client_flags_set(v, clientFlags);
    return 0;
  }
}


/** @internal 

    Expects cachedKey to be an entry from se->cache or NULL. ckey/keyLen describe
    an input string to compare against the final argument. This function returns
    true (non-0) if the final argument compares string-equivalent to (ckey,keyLen)
    OR is the same pointer address as the 2nd argument, else it returns false.
*/
static char s2_value_is_cached_string( s2_engine const * se,
                                       cwal_value const * cachedKey,
                                       char const * ckey, cwal_size_t keyLen,
                                       cwal_value const * key ){
  if(cachedKey == key) return 1;
  else{
      cwal_size_t xkeyLen = 0;
      char const * xkey = cwal_value_get_cstr(key, &xkeyLen);
      if(se){/*avoid unused param warning*/}
      return (xkeyLen==keyLen && *ckey==*xkey && 0==cwal_compare_cstr( ckey, keyLen,
                                                                       xkey, keyLen))
        ? 1 : 0;
  }
}

char s2_value_is_value_string( s2_engine const * se, cwal_value const * key ){
  return s2_value_is_cached_string( se, se->cache.keyValue, "value", 5, key );
}

char s2_value_is_prototype_string( s2_engine const * se, cwal_value const * key ){
  return s2_value_is_cached_string( se, se->cache.keyPrototype, "prototype", 9, key );
}


void s2_hash_dot_like_object( cwal_value * hv, int dotLikeObj ){
  /* cwal_hash * hash = cwal_value_get_hash(hv); */
  uint16_t f = cwal_container_client_flags_get(hv);
  cwal_container_client_flags_set(hv,
                                  dotLikeObj
                                  ? (S2_VAL_F_DOT_LIKE_OBJECT | (f & S2_VAL_F_COMMON_MASK))
                                  : (~S2_VAL_F_DOT_LIKE_OBJECT & f));
#if !defined(NDEBUG)
  assert(dotLikeObj
         ? (S2_VAL_F_DOT_LIKE_OBJECT & cwal_container_client_flags_get(hv))
         : !(S2_VAL_F_DOT_LIKE_OBJECT & cwal_container_client_flags_get(hv))
         );
  f = cwal_container_client_flags_get(hv);
  if(dotLikeObj){
    assert(S2_VAL_F_MODE_DOT == (f & S2_VAL_F_MODE_MASK));
    assert((S2_VAL_F_DOT_LIKE_OBJECT & 0xFF)  == (f & 0xFF));
  }else{
    assert(S2_VAL_F_MODE_DOT != (f & S2_VAL_F_MODE_MASK));
    assert((S2_VAL_F_DOT_LIKE_OBJECT & 0xFF) != (f & 0xFF));
  }
#endif
}

/**
  Internal impl for s2_minify_script() and friends.
*/
static int s2_minify_script_impl( s2_engine * se, cwal_buffer const * src,
                                  cwal_output_f dest, void * destState,
                                  unsigned int * nlCount ){
  int rc = 0;
  s2_ptoker pr = s2_ptoker_empty;
  s2_ptoken prev = s2_ptoken_empty;
  s2_ptoken const * t = &pr.token;
  s2_ptoker const * oldScript = se->currentScript;
  static const unsigned int nlEveryN = 3 /* output approx. 1 of every nlEveryN EOLs */;

  rc = s2_ptoker_init_v2( se->e, &pr, (char const *)src->mem,
                          (cwal_int_t)src->used,
                          0 );
  if(rc) goto end;
  for( ; !rc
         && !(rc = s2_next_token(se, &pr, S2_NEXT_NO_SKIP_EOL, 0))
         && !s2_ptoker_is_eof(&pr);
       prev = pr.token){
    assert(!s2_ttype_is_junk(t->ttype));
    /*
      We have to keep (some) newlines so that block constructs which
      optionally use EOL as EOX will work :/.
    */
    if(s2_ttype_is_eol(t->ttype)){
      ++(*nlCount);
      if(t->ttype==prev.ttype){
        /* elide runs of EOLs */
        continue;
      }
    }
    else if(S2_T_SquigglyBlock==t->ttype
       || S2_T_BraceGroup==t->ttype
       || S2_T_ParenGroup==t->ttype){
      cwal_buffer kludge = cwal_buffer_empty;
      s2_ptoker sub = s2_ptoker_empty;
      char const * opener;
      char const * closer;
      s2_ptoker_sub_from_token(&pr, t, &sub);
      kludge.mem = (unsigned char *)s2_ptoker_begin(&sub);
      kludge.used = kludge.capacity = (cwal_size_t)s2_ptoker_len(&sub);
      if(S2_T_BraceGroup==t->ttype){
        opener = "[";
        closer = "]";
      }else if(S2_T_ParenGroup==t->ttype){
        opener = "(";
        closer = ")";
      }else{
        opener = "{";
        closer = "}";
      }
      rc = dest( destState, opener, 1 );
      if(!rc) rc = s2_minify_script_impl( se, &kludge,
                                          dest, destState, nlCount );
      if(!rc) rc = dest( destState, closer, 1 );
      continue;
    }
    if(s2_ttype_is_eol(t->ttype)
       && S2_T_SquigglyBlock!=prev.ttype
       && (*nlCount % nlEveryN)){
      continue; /* we need EOLs intact after _some_ {blocks}, but
                   we can elide all others. We'll insert some now
                   and then, though, just for readability.*/
    }
    if((s2_ptoken_empty.ttype != prev.ttype /* first pass */)
       && !s2_is_space(t->ttype)
       && !s2_ttype_is_eol(t->ttype)
       && !s2_is_space(prev.ttype)
       && !s2_ttype_op(prev.ttype)
       && !s2_ttype_op(t->ttype)
       && (int)':' != t->ttype /* for 2nd half of ternary if */
       /*&& s2_ttype_is_pod(t->ttype)
         && s2_ttype_is_pod(prev.ttype)*/
       ){
      rc = dest( destState, " ", 1);
    }
    if(!rc){
      assert(s2_ptoken_begin(t) && s2_ptoken_end(t));
      assert(s2_ptoken_end(t) > s2_ptoken_begin(t));
      rc = dest( destState, s2_ptoken_begin(t), s2_ptoken_len(t) );
    }
  }
  end:
  s2_ptoker_finalize(&pr);
  se->currentScript = oldScript;
  return rc;
}

int s2_minify_script_buffer( s2_engine * se, cwal_buffer const * src,
                             cwal_buffer * dest ){
  if(src == dest) return CWAL_RC_MISUSE;
  else{
    unsigned int nlCount = 0;
    cwal_output_buffer_state job = cwal_output_buffer_state_empty;
    job.e = se->e;
    job.b = dest;
    return s2_minify_script_impl( se, src, cwal_output_f_buffer, &job,
                                  &nlCount );
  }
}

int s2_minify_script( s2_engine * se, cwal_input_f src, void * srcState,
                      cwal_output_f out, void * outState ){
  cwal_buffer buf = cwal_buffer_empty;
  int rc = cwal_buffer_fill_from(se->e, &buf, src, srcState);
  if(!rc){
    unsigned int nlCount = 0;
    cwal_buffer_clear(se->e, &buf);
    rc = s2_minify_script_impl( se, &buf, out, outState, &nlCount );
  }
  cwal_buffer_clear(se->e, &buf);
  return rc;
}

int s2_cb_minify_script( cwal_callback_args const * args, cwal_value ** rv ){
  s2_engine * se = s2_engine_from_args(args);
  int rc = 0;
  char const * src = 0;
  cwal_size_t srcLen = 0;
  cwal_buffer * dest = 0;
  cwal_value * destV = 0;
  assert(se);
  src = args->argc
    ? cwal_value_get_cstr(args->argv[0], &srcLen)
    : 0;
  if(!src){
    return cwal_exception_setf(args->engine, CWAL_RC_MISUSE,
                               "Expecting one string or Buffer argument.");
  }
  if(args->argc>1){
    dest = cwal_value_get_buffer(args->argv[1]);
  }
  if(!dest){
    dest = cwal_new_buffer(args->engine, srcLen/2);
    if(!dest) return CWAL_RC_OOM;
  }
  destV = cwal_buffer_value(dest);
  assert(destV);
  cwal_value_ref(destV);
  if(srcLen && *src){
    cwal_buffer kludge = cwal_buffer_empty;
    kludge.mem = (unsigned char *)src;
    kludge.used = srcLen;
    kludge.capacity = srcLen;
    rc = s2_minify_script_buffer(se, &kludge, dest);
  }
  if(rc){
    cwal_value_unref(destV);
  }else{
    *rv = destV;
    cwal_value_unhand(destV);
  }
  return rc;
}

int s2_ctor_method_set( s2_engine * se, cwal_value * container, cwal_function * method ){
  if(!se || !container || !method) return CWAL_RC_MISUSE;
  else if(!cwal_props_can(container)) return CWAL_RC_TYPE;
  else{
    return s2_set_with_flags_v( se, container, se->cache.keyCtorNew,
                                cwal_function_value(method),
                                CWAL_VAR_F_CONST | CWAL_VAR_F_HIDDEN );
  }
}

int s2_ctor_callback_set( s2_engine * se, cwal_value * container, cwal_callback_f method ){
  int rc;
  cwal_function * f;
  cwal_value * fv;
  if(!se || !container || !method) return CWAL_RC_MISUSE;
  else if(!cwal_props_can(container)) return CWAL_RC_TYPE;
  fv = cwal_new_function_value(se->e, method, 0, 0, 0);
  f = fv ? cwal_value_get_function(fv) : NULL;
  if(f){
    cwal_value_ref(fv);
    rc = s2_ctor_method_set( se, container, f );
    cwal_value_unref(fv);
  }else{
    rc = CWAL_RC_OOM;
  }
  return rc;
}

char s2_value_is_newing(cwal_value const * v){
  return (cwal_container_client_flags_get( v )
          & S2_VAL_F_IS_NEWING) ? 1 : 0;
}


int s2_ctor_fetch( s2_engine * se, s2_ptoker const * pr,
                   cwal_value * operand,
                   cwal_function **rv,
                   int errPolicy ){
  cwal_value * vtor = 0;
  vtor = s2_get_v_proxy2( se, operand, se->cache.keyCtorNew, 0 );
  if(!vtor){
    static char const * msgfmt =
      "Construction requires a container "
      "with a constructor function property named '%s'.";
    if(!errPolicy) return CWAL_RC_NOT_FOUND;
    else if(errPolicy<0){
      return pr
        ? s2_throw_ptoker(se, pr, CWAL_RC_NOT_FOUND, msgfmt,
                          cwal_value_get_cstr(se->cache.keyCtorNew,0))
        : s2_throw(se, CWAL_RC_NOT_FOUND, msgfmt,
                   cwal_value_get_cstr(se->cache.keyCtorNew,0))
        ;
    }else{
      return pr
        ? s2_err_ptoker(se, pr, CWAL_RC_NOT_FOUND, msgfmt,
                            cwal_value_get_cstr(se->cache.keyCtorNew,0))
        : s2_engine_err_set(se, CWAL_RC_NOT_FOUND, msgfmt,
                            cwal_value_get_cstr(se->cache.keyCtorNew,0))
      ;
    }
  }else{
    static const char * msgfmt =
      "Construction requires a container with "
      "a constructor function property named '%s', "
      "but the ctor resolves to type '%s'.";
    cwal_function * f = cwal_value_function_part(se->e, vtor);
    if(f){
      if(rv) *rv = f;
      return 0;
    }
    else if(!errPolicy) return CWAL_RC_NOT_FOUND;
    else if(errPolicy<0){
      return pr
        ? s2_throw_ptoker(se, pr, CWAL_RC_NOT_FOUND, msgfmt,
                          cwal_value_get_cstr(se->cache.keyCtorNew,0),
                          cwal_value_type_name(vtor))
        : s2_throw(se, CWAL_RC_NOT_FOUND, msgfmt,
                   cwal_value_get_cstr(se->cache.keyCtorNew,0),
                   cwal_value_type_name(vtor))
      ;
    }else{
      return pr
        ? s2_err_ptoker(se, pr, CWAL_RC_NOT_FOUND, msgfmt,
                        cwal_value_get_cstr(se->cache.keyCtorNew,0),
                        cwal_value_type_name(vtor))
        : s2_engine_err_set(se, CWAL_RC_NOT_FOUND, msgfmt,
                            cwal_value_get_cstr(se->cache.keyCtorNew,0),
                            cwal_value_type_name(vtor))
        ;
    }
  }
}

/**
   Returns true if v is legal for use as an operand do the "new"
   keyword. This arguably should return true only if the property
   is found in v itself, not in prototypes. Hmmm.
*/
char s2_value_is_newable( s2_engine * se, cwal_value * v ){
#if 0
  cwal_value * vtor = 0;
  /* see notes in s2_keyword_f_new() */
  return (s2_get_v( se, v, se->cache.keyCtorNew, &vtor ))
    ? 0
    : !!cwal_value_function_part(se->e, vtor ? vtor : v);
#eif 0
  cwal_value * vtor = 0;
  return (s2_get_v( se, v, se->cache.keyCtorNew, &vtor ))
    ? 0
    : vtor ? !!cwal_value_function_part(se->e, vtor) : 0;
#else
  if(!cwal_props_can(v)) return 0;
  else{
    cwal_function * f = 0;
    s2_ctor_fetch(se, NULL, v, &f, 0);
    return f ? 1 : 0;
  }
#endif
}

unsigned int s2_sizeof_script_func_state(){
  return (unsigned int)sizeof(s2_func_state);
}

void s2_value_to_lhs_scope( cwal_value const * lhs, cwal_value * v){
  assert(lhs);
  assert(v);
  assert(!cwal_value_is_builtin(lhs)
         && "or else a caller is misusing this function.");
  if(lhs != v){
    cwal_scope * const s = cwal_value_scope(lhs);
    assert(s && "Very bad. Corrupt value.");
    if(s) cwal_value_rescope(s, v);
  }
}

cwal_value * s2_propagating_get( s2_engine * se ){
  return cwal_propagating_get(se->e);
}

cwal_value * s2_propagating_take( s2_engine * se ){
  return cwal_propagating_take(se->e);
}

cwal_value * s2_propagating_set( s2_engine * se, cwal_value * v ){
  return cwal_propagating_set(se->e, v);
}

int s2_typename_set_v( s2_engine * se, cwal_value * container, cwal_value * name ){
  return s2_set_with_flags_v(se, container, se->cache.keyTypename, name,
                             CWAL_VAR_F_CONST | CWAL_VAR_F_HIDDEN);
}

int s2_typename_set( s2_engine * se, cwal_value * container,
                     char const * name, cwal_size_t nameLen ){
  int rc;
  cwal_value * v = cwal_new_string_value(se->e, name, nameLen);
  if(v){
    cwal_value_ref(v);
    rc = s2_typename_set_v( se, container, v);
    cwal_value_unref(v);
  }else{
    rc = CWAL_RC_OOM;
  }
  return rc;
}


char const * s2_strstr( char const * haystack, cwal_size_t hayLen,
                        char const * needle, cwal_size_t needleLen ){
  char const * pos = haystack;
  char const * end = haystack + hayLen - needleLen+1;
  if(hayLen<needleLen || !needleLen || end <= pos) return 0;
  for( ; pos >= haystack && pos < end; ++pos){
    if(0==memcmp(pos, needle, needleLen)) return pos;
  }
  return 0;                          
}


int s2_set_from_script_v( s2_engine * se, char const * src,
                          int srcLen, cwal_value * addResultTo,
                          cwal_value * propName ){
  if(!se || !src || !addResultTo || !propName){
    return CWAL_RC_MISUSE;
  }
  else if(!cwal_props_can(addResultTo)) return CWAL_RC_TYPE;
  else{
    int rc;
    cwal_value * rv = 0;
    char const * pn = cwal_value_get_cstr(propName, 0);
    if(srcLen<0) srcLen = (int)cwal_strlen(src);
    rc = s2_eval_cstr( se, 1, pn ? pn : "s2_set_from_script_v",
                       src, srcLen, &rv );
    switch(rc){
      case CWAL_RC_RETURN:
        rv = s2_propagating_take(se);
        s2_engine_err_reset(se);
        rc = 0;
        break;
    }
    if(!rc){
      cwal_value_ref(rv);
      rc = s2_set_v( se, addResultTo, propName,
                     rv ? rv : cwal_value_undefined() );
      cwal_value_unref(rv);
    }
    return rc;
  }  
}


int s2_set_from_script( s2_engine * se, char const * src,
                        int srcLen, cwal_value * addResultTo,
                        char const * propName,
                        cwal_size_t propNameLen ){
  if(!se || !src || !addResultTo || !propName){
    return CWAL_RC_MISUSE;
  }
  else if(!propNameLen) return CWAL_RC_RANGE;
  else if(!cwal_props_can(addResultTo)) return CWAL_RC_TYPE;
  else{
    int rc;
    cwal_value * prop = cwal_new_string_value(se->e, propName,
                                              propNameLen);
    cwal_value_ref(prop);
    rc = prop
      ? s2_set_from_script_v( se, src, srcLen, addResultTo,
                              prop )
      : CWAL_RC_OOM;
    cwal_value_unref(prop);
    return rc;
  }  
}

int s2_cb_glob_matches_str( cwal_callback_args const * args,
                            cwal_value ** rv ){
  int rc = 0;
  cwal_int_t policy = -1;
  char const * glob =
    (args->argc>0) ? cwal_value_get_cstr(args->argv[0], NULL) : NULL;
  char const * str =
    (args->argc>1) ? cwal_value_get_cstr(args->argv[1], NULL) : NULL;
  char const * sSelf = cwal_value_get_cstr(args->self, NULL);
  if(sSelf){
    str = glob;
    glob = sSelf;
    if(args->argc>1) policy = cwal_value_get_integer(args->argv[1]);
  }else{
    if(args->argc>2) policy = cwal_value_get_integer(args->argv[2]);
  }
  if(!glob || !str){
    rc = cwal_cb_throw(args, CWAL_RC_MISUSE,
                     "Expecting (glob,string) arguments "
                     "OR globStringInstance.thisFunc(otherString) "
                     "usage.");
  }
  else if(!*glob){
    return cwal_cb_throw(args, CWAL_RC_RANGE,
                       "Glob string may not be empty.");
  }
  else{
    *rv = cwal_glob_matches_cstr(glob, str,
                                 policy<0
                                 ? CWAL_GLOB_WILDCARD
                                 : (policy>0
                                    ? CWAL_GLOB_LIKE
                                    : CWAL_GLOB_LIKE_NOCASE))
      ? cwal_value_true() : cwal_value_false();
  }
  return rc;
}

int s2_cb_tokenize_line(cwal_callback_args const * args, cwal_value ** rv){
  cwal_array * ar = 0;
  cwal_value * arV = 0;
  s2_ptoker pt = s2_ptoker_empty;
  cwal_size_t lineLen = 0;
  s2_engine * se = s2_engine_from_args(args);
  char const * line = args->argc
    ? cwal_value_get_cstr(args->argv[0], &lineLen)
    : 0;
  int rc;
  if(!line) {
    return cwal_cb_throw(args, CWAL_RC_MISUSE, "Expecting a single string argument.");
  }
  rc = s2_ptoker_init_v2( args->engine, &pt, line, (cwal_int_t)lineLen, 0 );
  if(rc) goto toss;
  pt.name = "tokenize line input";
#if 0
  ar = cwal_new_array(args->engine);
  if(!ar){
    rc = CWAL_RC_OOM;
    goto toss;
  }
  arV = cwal_array_value(ar);
  cwal_value_ref(arV);
#endif
  /* pt.parent = se->currentScript; */
  while( !(rc=s2_next_token(se, &pt, 0, 0))
         && !s2_ptoker_is_eof(&pt) ){
    cwal_value * v = 0;
    /* if(s2_ttype_is_junk(pt.token.ttype)) continue; */
#if 1
    if(!ar){
      ar = cwal_new_array(args->engine);
      if(!ar){
        rc = CWAL_RC_OOM;
        goto toss;
      }
      arV = cwal_array_value(ar);
      cwal_value_ref(arV);
    }
#endif
    v = s2_ptoken_is_tfnu(&pt.token);
    if(!v){
      rc = s2_ptoken_create_value( se, &pt, &pt.token, &v );
      if(rc) break;
    }
    cwal_value_ref(v);
    rc = cwal_array_append(ar, v);
    cwal_value_unref(v);
    if(rc) break;
  }
  if(rc) goto toss;
  assert(!rc);
  cwal_value_unhand(arV);
  *rv = arV;
  return rc;
  toss:
  cwal_value_unref(arV);
  assert(rc);
  switch(rc){
    case CWAL_RC_EXCEPTION:
    case CWAL_RC_OOM:
    case CWAL_RC_INTERRUPTED:
      break;
    default:
      if(s2_ptoker_errtoken_has(&pt)){
        rc = s2_throw_ptoker(se, &pt, rc,
                             "Failed with code %d (%s).",
                             rc, cwal_rc_cstr(rc));
      }else{
        rc = cwal_cb_throw(args, rc, "Failed with code %d (%s).",
                         rc, cwal_rc_cstr(rc));
      }
      break;
  }
  s2_ptoker_finalize( &pt );
  return rc;
}

int s2_eval_hold( s2_engine * se, cwal_value * v ){
  s2_scope * sc = s2__scope_current(se);
  return (v && sc->evalHolder && !cwal_value_is_builtin(v))
    ? cwal_array_append(sc->evalHolder, v)
    : 0;
}

int s2_stash_hidden_member( cwal_value * who, cwal_value * what ){
  int rc = cwal_props_can(who) ? 0 : CWAL_RC_TYPE;
  cwal_value * key;
  cwal_engine * e = cwal_value_engine(who);
  if(rc) return rc;
  assert(e && "if who can props then who has an engine.");
  if(!e) return CWAL_RC_MISUSE;
  key = cwal_new_unique( e, 0 );
  if(!key) return CWAL_RC_OOM;
  cwal_value_ref(key);
  rc = cwal_prop_set_with_flags_v( who, key, what,
                                   CWAL_VAR_F_HIDDEN
                                   | CWAL_VAR_F_CONST );
  cwal_value_unref(key);
  return rc;
}

int s2_trigger_exit( cwal_callback_args const * args, cwal_value * result ){
  cwal_propagating_set(args->engine,
                       result ? result : cwal_value_undefined());
  return CWAL_RC_EXIT;
}

cwal_value * s2_value_unwrap( cwal_value * v ){
  return cwal_value_is_unique(v)
    ? cwal_unique_wrapped_get(v)
    : v;
}

cwal_value const * s2_value_unwrap_c( cwal_value const * v ){
  return cwal_value_is_unique(v)
    ? cwal_unique_wrapped_get(v)
    : v;
}

char const * s2_value_cstr( cwal_value const * v, cwal_size_t * len ){
  if(cwal_value_is_unique(v)) v = cwal_unique_wrapped_get(v);
  return cwal_value_get_cstr(v, len);
}

int s2_seal_container( cwal_value * v, char sealIt ){
  if(!v) return CWAL_RC_MISUSE;
  else if(!cwal_props_can(v)) return CWAL_RC_TYPE;
  else{
    cwal_flags16_t const sealFlags =
      CWAL_CONTAINER_DISALLOW_PROP_SET
      | CWAL_CONTAINER_DISALLOW_PROTOTYPE_SET;
    cwal_flags16_t flags = cwal_container_flags_get(v);
    if(sealIt) flags |= sealFlags;
    else flags &= ~sealFlags;
    cwal_container_flags_set(v, flags);
  }
  return 0;
}

int s2_cb_seal_object( cwal_callback_args const * args,
                       cwal_value ** rv ){
  uint16_t i = 0;
  cwal_value * last = 0;
  char doSeal = 1;
  if(!args->argc){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting one or more Container-type arguments.");
  }
#if 0
  /* This enables setting the seal/unseal flag by passing
     a bool first argument, but allowing script code to unseal
     objects will lead to madness. */
  if(cwal_value_is_bool(args->argv[0])){
    doSeal = cwal_value_get_bool(args->argv[0]);
    i = 1;
  }
#endif
  for(; i < args->argc; ++i ){
    cwal_value * const c = args->argv[i];
    if(!cwal_props_can(c)){
      return cwal_cb_throw(args, CWAL_RC_TYPE,
                         "Argument #%d is not a Container type.",
                         (int)i);
    }
    s2_seal_container(c, doSeal);
    last = c;
  }
  *rv = last;
  return 0;
}

int s2_tokenize_path_to_array( cwal_engine * e, cwal_array ** tgt, char const * path,
                               cwal_int_t pathLen ){
  int rc = 0;
  cwal_array * ar = *tgt ? *tgt : cwal_new_array(e);
  cwal_value * arV;
  char const * t = 0;
  cwal_size_t tLen = 0;
  s2_path_toker pt = s2_path_toker_empty;
  if(!ar){
    return CWAL_RC_OOM;
  }
  arV = cwal_array_value(ar);
  cwal_value_ref(arV);
  s2_path_toker_init(&pt, path, pathLen);
  while(0==s2_path_toker_next(&pt, &t, &tLen)){
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


int s2_cb_tokenize_path( cwal_callback_args const * args, cwal_value **rv ){
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
    ? s2_tokenize_path_to_array(args->engine, &tgt,
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
  return cwal_cb_throw(args, CWAL_RC_MISUSE,
                     "Expecting (string path[, array target]) argument(s).");
}

char const * s2_home_get(s2_engine * se, cwal_size_t * len){
  char const * e = getenv("S2_HOME");
  S2_UNUSED_ARG(se);
  if(e && len) *len = cwal_strlen(e);
  return e;
}

void s2_disable_set( s2_engine * se, cwal_flags32_t f ){
  se->flags.disable = f;
}

int s2_disable_set_cstr( s2_engine * se, char const * str, cwal_int_t strLen,
                         cwal_flags32_t * result ){
  cwal_flags32_t f = 0;
  if(strLen<0) strLen = (cwal_int_t)cwal_strlen(str);
  if(strLen>0){
    char const * t = 0;
    cwal_size_t tLen = 0;
    s2_path_toker pt = s2_path_toker_empty;
    s2_path_toker_init(&pt, str, strLen);
    pt.separators = " ,";
    while(0==s2_path_toker_next(&pt, &t, &tLen)){
#define CHECK(KEY,F) if(0==cwal_compare_cstr(KEY,tLen,t,tLen)){ f|=F; continue; }(void)0
      switch(tLen){
        case 4:
          if(0==cwal_compare_cstr("none",tLen,t,tLen)){
            f = 0;
            continue;
          }
          break;
        case 5:
          CHECK("fs-io", S2_DISABLE_FS_IO);
          break;
        case 6:
          CHECK("fs-all", S2_DISABLE_FS_ALL);
          break;
        case 7:
          CHECK("fs-read", S2_DISABLE_FS_READ);
          CHECK("fs-stat", S2_DISABLE_FS_STAT);
          break;
        case 8:
          CHECK("fs-write", S2_DISABLE_FS_READ);
          break;
        default:
          break;
      }
#undef CHECK
      return s2_engine_err_set(se, CWAL_RC_RANGE,
                               "Unknown feature-disable flag: %.*s",
                               (int)tLen, t);
    }
  }
  se->flags.disable = f;
  if(result) *result = f;
  return 0;
}


cwal_flags32_t s2_disable_get( s2_engine const * se ){
  return se->flags.disable;
}

/**
   Returns the name of the first s2_disabled_features flag which
   matches f, checking from lowest to highest value.
*/
static char const * s2_disable_first_flag_name( cwal_flags32_t f ){
#define CHECK(F) if(f & F) return #F
  CHECK(S2_DISABLE_FS_STAT);
  else CHECK(S2_DISABLE_FS_READ);
  else CHECK(S2_DISABLE_FS_WRITE);
  else return "???";
#undef CHECK
}

int s2_disable_check( s2_engine * se, cwal_flags32_t f ){
  int rc = 0;
  if(f & se->flags.disable){
    rc = s2_engine_err_set(se, CWAL_RC_ACCESS,
                           "Feature flag(s) 0x%08x disallowed by "
                           "s2_disabled_features flag %s.",
                           (unsigned)f,
                           s2_disable_first_flag_name(f & se->flags.disable));
  }
  return rc;
}

int s2_disable_check_throw( s2_engine * se, cwal_flags32_t f ){
  int rc = s2_disable_check(se, f);
  if(rc){
    rc = s2_throw_err(se, 0, 0, 0, 0);
    assert(rc != 0);
  }
  return rc;
}

int s2_cb_disable_check( cwal_callback_args const * args, cwal_flags32_t f ){
  return s2_disable_check_throw(s2_engine_from_args(args), f);
}

#if S2_TRY_INTERCEPTORS
/**
   Script usages:

   1) f( obj, key, func )

   Flags func as an interceptor and installs func as obj[key].

   2) Function f(func)

   Flags func as an interceptor. For the first form, it also adds the
   function as a property of the given object.
*/
int s2_cb_add_interceptor( cwal_callback_args const * args, cwal_value **rv ){
  cwal_value * tgt = 0;
  cwal_value * key = 0;
  cwal_value * fv = 0;
  cwal_function * f = 0;
  if(1==args->argc){
    fv = args->argv[0];
  }else if(3==args->argc){
    tgt = args->argv[0];
    key = args->argv[1];
    fv = args->argv[2];
  }
  f = fv ? cwal_value_function_part(args->engine, fv) : 0;
  if(!f || (tgt && !cwal_props_can(tgt))){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting (Function) or "
                       "(Object, mixed, Function) arguments.");
  }else{
    s2_engine * se = s2_engine_from_args(args);
    /* uint16_t const flags = cwal_container_client_flags_get(fv); */
    int const rc = key
      ? s2_set_with_flags_v( se, tgt, key, fv,
                             CWAL_VAR_F_HIDDEN
                             | CWAL_VAR_F_CONST )
      : 0;
    if(!rc){
#if 0
      s2_func_state * fst = rc ? 0 : s2_func_state_for_func(f);
      if(fst){
        /* A script func. Optimization: if it contains the text
           se->cache.keyInterceptee then set a flag to tell downstream
           code to set the se->cache.keyInterceptee var when calling
           the interceptor. */
        char const * str = cwal_value_get_cstr(fst->vSrc, 0);
        char const * key = cwal_value_get_cstr(se->cache.keyInterceptee,
                                               0);
        if(strstr(str, key)){
          fst->flags |= S2_FUNCSTATE_F_INTERCEPTEE;
        }
      }
#endif
      /* cwal_container_client_flags_set(fv, flags | S2_VAL_F_FUNC_INTERCEPTOR); */
      cwal_container_flags_set(fv, cwal_container_flags_get(fv)
                               | CWAL_CONTAINER_INTERCEPTOR);
      *rv = 1==args->argc
        ? fv
        : cwal_function_value(args->callee);
    }
    return rc;
  }
}
#endif/*S2_TRY_INTERCEPTORS*/

#undef MARKER
#undef HAS_DOTLIKE_FLAG
#undef HAS_ENUM_FLAG
#undef s2__scope_current
#undef s2__scope_for_level
#undef s2__err
#undef S2_USE_SIGNALS
