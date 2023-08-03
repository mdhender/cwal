/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */

/**
   Impl for script-side function bits.
*/
#include "internal.h"
#include <string.h>
#include <assert.h>

#include <stdio.h>
#if 1
#define MARKER(pfexpr) if(1) printf("MARKER: %s:%d:\t",__FILE__,__LINE__); if(1) printf pfexpr
#else
#define MARKER if(0) printf
#endif


static const whcl__func whcl__func_empty = {
  NULL/*vImports*/, NULL/*vName*/, NULL/*vScriptName*/,
  NULL/*next*/,0/*flags*/,{/*tok*/
  whcl__script_empty_m/*script*/,
  NULL/*name*/, NULL/*params*/,NULL/*body*/
}
};

whcl__func * whcl__func_alloc(whcl_engine * const el){
  whcl__func * rc = NULL;
  if(el->recycler.scriptFuncs.count){
    rc = el->recycler.scriptFuncs.head;
    el->recycler.scriptFuncs.head = rc->next;
    rc->next = NULL;
    --el->recycler.scriptFuncs.count;
    ++el->recycler.scriptFuncs.hits;
  }else{
    ++el->recycler.scriptFuncs.misses;
    rc = (whcl__func*)cwal_malloc2(el->ec, sizeof(whcl__func));
  }
  if(rc) *rc = whcl__func_empty;
  else{
    WHCL__WARN_OOM;
  }
  return rc;
}

void whcl__func_free(whcl_engine * const el, whcl__func * const f,
                     bool allowRecycle){
  if(!f) return;
  else if(f->next || f==el->recycler.scriptFuncs.head){
    // f is still in the recycler
    whcl__func * rf = el->recycler.scriptFuncs.head;
    whcl__func * fprev = NULL;
    assert(!allowRecycle && "Else internal mismanagement.");
    assert(!f->vImports);
    assert(!f->vName);
    assert(!f->vScriptName);
    assert(rf && "Else f->next was incorrectly set.");
    for(; rf ; fprev = rf, rf = rf->next){
      if(f==rf){
        if(fprev) fprev->next = rf->next;
        else el->recycler.scriptFuncs.head = rf->next;
        rf->next = NULL;
        --el->recycler.scriptFuncs.count;
        break;
      }
    }
  }
  assert(!f->next && "Else f->next was incorrectly set.");
  if(el->scopes.level){
    /* Huge kludge: for reasons i don't understand, f->vName is, on
       occassion during engine shutdown, being outright destroyed (as
       opposed to being placed in the gc queue) before we can unref it
       here. Workaround: when in shutdown mode, leave these values
       for the scope to clean up (which it will). */
    if(f->vImports) cwal_unref(f->vImports);
    if(f->vName) cwal_unref(f->vName);
    if(f->vScriptName){/* Managed via whcl__script_name() */}
  }
  whcl__script_finalize(&f->tok.script);
  assert(!f->tok.script.chain);
  *f = whcl__func_empty;
  if(allowRecycle
     && el->recycler.scriptFuncs.count < el->recycler.scriptFuncs.max ){
    ++el->recycler.scriptFuncs.count;
    f->next = el->recycler.scriptFuncs.head;
    el->recycler.scriptFuncs.head = f;
  }else{
    cwal_free2(el->ec, f, sizeof(whcl__func));
  }
}

/** cwal finalizer for Function state. m must be a (whcl__func*). */
static void cwal_finalizer_f_whcl__func( cwal_engine * e, void * m ){
  whcl__func * const fs = (whcl__func*)m;
  whcl_engine * const el = whcl_engine_from_state(e);
  assert(fs);
  assert(el);
  whcl__func_free( el, fs, true );
}

whcl__func * whcl__func_for_func(cwal_function * const f){
  return (whcl__func *)cwal_function_state_get(f, &whcl__func_empty);
}

/**
   Looks at ct's next token, skipping EOLs, and extracts a function
   parameter name and default value from it:

   ```
   proc {aName anotherName {name3 defaultValue}} {...}
   #    ^____________this is ct________________^
   ```

   Returns 0 on non-error. `*tName` gets set to the token of the param
   name, or NULL if no more params are availalbe. `*tDefaultVal` is
   set to the token of the default value, or NULL if no default value
   is specified.
*/
static int whcl__func_get_param(whcl_script * const ct,
                                whcl_stoken const **tName,
                                whcl_stoken const **tDefaultVal){
  whcl_stoken const * k = NULL;
  whcl__next_token_no_eol(ct, &k);
  assert(k);
  //whcl__dump_stok(ct, k, "get-param key 1");
  if(TOK1_T_SquigglyGroup == k->ttype){
    whcl_script sub = whcl__script_empty;
    whcl__script_sub_from_group(ct, &sub, true);
    whcl__next_token_no_eol(&sub, &k);
    //whcl__dump_stok(&sub, k, "get-param key 2");
    if(whcl_stoken_is_eof(k)){
      return whcl__script_err(ct, CWAL_SCR_SYNTAX,
                          "Empty function parameter {} block.");
    }else{
      *tName = k;
      whcl__next_token_no_eol(&sub, &k);
      //whcl__dump_stok(ct, k, "get-param dflt 2");
      if(whcl_stoken_is_eof(k)){
        *tDefaultVal = NULL;
      }else{
        *tDefaultVal = k;
        k = whcl_stoken_sibling(ct, k);
        if(k && !whcl_stoken_is_eof(k)){
          whcl__script_errtoken_set(ct, k);
          return whcl__script_err(ct, CWAL_SCR_SYNTAX,
                              "Extra token after function parameter "
                              "default value.");
        }
      }
    }
  }else if(whcl_stoken_is_eof(k)){
    k = *tName = *tDefaultVal = NULL;
  }else{
    *tName = k;
    *tDefaultVal = NULL;
  }
  if(*tName && TOK1_T_Identifier != (*tName)->ttype){
    whcl__script_errtoken_set(ct, *tName);
    return whcl__script_err(ct, CWAL_SCR_SYNTAX,
                        "Expecting identifier for parameter name.");
  }
  return 0;
}

/**
   Returns true if v is-a string and tokenizes like to a
   TOK1_T_WHCLFlag-type token, else returns false. Note that by the
   time this is processed, we have lost any info about whether the
   value was originally quoted or not, meaning that we can mistake
   non-flag arguments for flags. We really want to be able to treat

   func "-b"

   differently from:

   func -b

   but we don't have enough info at this level to do that.
*/
static bool whcl__value_is_flag(cwal_value const * const v){
  cwal_size_t n = 0;
  char const * s = cwal_value_is_string(v)
    ? cwal_value_get_cstr(v, &n) : NULL;
  if(!s || !n || '-'!=s[0]) return false;
  char const * zEnd = NULL;
  int const idChars =
    whcl__read_identifier2(s, s+n, &zEnd,
                           WHCL__SCRIPT_F_IDENTIFIER_DASHES2);
  return idChars>1 && zEnd==s+n;
}

/**
   Used by cwal_callback_f_whcl() to map callback arguments
   to script function parameters.
*/
static int whcl__func_process_params(whcl_engine * const el,
                                     cwal_array * const cbArgv,
                                     whcl__func * const fst){
  int rc = 0;
  uint16_t const nArgv = (uint16_t)cwal_array_length_get(cbArgv);
  uint16_t i = 0 /* current index into cbArgv */;
  whcl_script sub = whcl__script_empty;
  cwal_value * v;
  whcl_stoken const * tName;
  whcl_stoken const * tDflt;
  whcl_stoken const * tArg = NULL;
  whcl__stoken_set(&fst->tok.script, fst->tok.params);
  rc = whcl__script_sub_from_group(&fst->tok.script, &sub, true);
  if(rc) return rc;
  cwal_value * const av = cwal_array_value(cbArgv);
#if 1
  /** check for leading _declared_ -flags in the param list. These are
      _not_ counted as part of the argv list. Instead, -flag arguments
      are added as member properties of cbArgv[-flag-name]=falseand
      skipped over for purposes of matching up cbArgv to the list of
      declared params. Any such flags which appear in the inbound
      argument list (cbArgv) cause those properties to be set to
      true.

      TODO: for --two-dash-flags, permit a trailing =VALUE part. That
      first requires more complete flag support at the
      tokenizing/compilation level, to move the value part
      into flagToken->innerId.
 */
  rc = whcl__next_token_no_eol(&sub, &tArg);
  assert(tArg);
  for( ; 0==rc && TOK1_T_WHCLFlag==tArg->ttype; ){
    rc = cwal_prop_set(av, whcl_stoken_begin(&sub, tArg),
                       whcl_stoken_len(tArg),
                       cwal_value_false());
    if(0==rc) rc = whcl__next_token_no_eol(&sub, &tArg);
  }
  if(rc) goto end;
  whcl__script_putback(&sub)/*first non-flag*/;
#elif 0
  /* Alternate syntax: { -{-flag -flag2...} param1 ...paramN } */
  if(TOK1_T_OpMinus==tok->ttype){
    tok = whcl__script_next_token1(&sub);
    assert(tok);
    if(TOK1_T_SquigglyGroup!=tok->ttype){
      return whcl__script_err(&sub, CWAL_SCR_SYNTAX,
                          "Expecting {...} after '-' in "
                          "proc param list.");
    }
    rc = whcl__func_process_param_flags(el, sub, tok, cbArgv);
    if(rc) goto end;
  }else{
    whcl__script_rewind(&sub);
  }
#endif

  /* Distinguish between -flag and non-flag arguments. Add _leading_
     _declared_ -flag args as bool true member properties of cbArgv
     and skip them for purposes of matching declared params with
     cbArgv entries. Once any non-flag argument is seen, stop
     processing flags. The wrinkles in this are:

     1) __command-style dispatching may happen before this and leave
     us with a different (shorter) argc->argv than the one which was
     initially set up in whcl__call_cwal_f().  That one is
     surmountable via the addition of new state to el->fcall, but the
     more serious wrinkle is...

     2) function.call/apply() can leave us with arguments with no
     direct token mappings due to their levels of indirection. Working
     around that is a much larger hurdle which surpasses my current
     ambitions.

     #2 leaves us with only(?) one(?) viable approach to
     distinguishing between -flags and -flag-looking-args
     (i.e. `-flag` vs `"-flag"`): require explicit declaration of
     expected flags in the param list and any -flag-like-thing which
     is not in that list is not considered a flag, but a normal
     argument. We could arguably fail if passed a non-declared flag
     but we cannot _reliably_ distingush such flags from legitimate
     flag-looking arguments.
  */
  for( i = 0; i < nArgv; ){
    cwal_value * const v = cwal_array_get(cbArgv, i);
    cwal_size_t tLen = 0;
    char const * tStr = cwal_value_get_cstr(v, &tLen);
    if(!tStr || !whcl__value_is_flag(v)
       || !cwal_prop_has(av, tStr, tLen, false)
       /* ^^^^ Non-declared flag. Treat it as a normal argument. */){
      break;
    }
    rc = cwal_prop_set_v(av, v, cwal_value_true());
    if(rc){ WHCL__WARN_OOM; goto end; }
    ++i;
  }

  /* Stuff remaining args in argv and declare those which match
     declared arg positions as local vars, honoring default values
     for any which were not passed via cbArgv. */
  while(0==rc){
    tName = tDflt = NULL;
    rc = whcl__func_get_param(&sub, &tName, &tDflt);
    if(rc || !tName) break;
    v = NULL;
    if(i < nArgv){
      v = cwal_array_get(cbArgv, i);
      ++i;
    }else if(tDflt){
      rc = whcl__eval_token(el, &sub, tDflt, 0, &v);
      if(rc) break;
      else if(!v){
        MARKER(("WARNING: successful eval of token resulted in NULL.\n"));
        whcl__dump_stok(&sub, tDflt, "tDflt");
        v = cwal_value_undefined();
      }
    }else{
      v = cwal_value_undefined();
    }
    cwal_ref(v);
    //whcl__dump_val(v, "declaring arg value");
    rc = whcl_var_decl(el, NULL, false,
                       whcl_stoken_cstr(&sub, tName, NULL, false),
                       (cwal_int_t)tName->length, v);
    cwal_unref(v);
  }
  end:
  whcl__script_finalize(&sub);
  return rc;
}

/**
   Called by cwal_callback_f_whcl() to install the following vars in
   argv->scope:

   - argv
   - this
   - funcNameIfSetInFuncDecl
   - symbols imported via the 'using' keyword.

   It does not set up named parameters.

   On success it sets `*cbArgv` to an array which has been set
   as `argv` in args->scope and returns 0. On error, non-0.
*/
static int whcl__func_populate_scope(whcl_engine * const el,
                                     whcl__func * const fs,
                                     cwal_callback_args const * args,
                                     cwal_array ** cbArgv){
  int rc = 0;
  if(fs->vName){
    rc = whcl_scope_set_v(el, NULL, false, fs->vName,
                          cwal_function_value(args->callee));
  }
  if(0==rc){
    if(fs->vImports && !(WHCL__FUNC_F_NO_IMPORT_USING & fs->flags)){
      rc = whcl__scope_import_props(el, NULL, fs->vImports);
    }
  }
  if(rc) return rc;
  cwal_array * const ar = cwal_new_array(args->engine);
  if(!ar){
    WHCL__WARN_OOM;
    return CWAL_RC_OOM;
  }
  cwal_value * const av = cwal_array_value(ar);
  cwal_ref(av);
#if 1
  for( int i = (int)args->argc; 0==rc && i > 0; --i ){
    /* insert in reverse order as a reallocation optimization */
    /* whcl__dump_val(args->argv[i-1],"args->argv[i-1]"); */
    rc = cwal_array_set(ar, (cwal_size_t)i-1, args->argv[i-1]);
  }
#else
  /** This naive approach to -flag arg handling is the only(?)
      one which "works" with both __command dispatching and
      forwarding via function.call/apply(). However, it cannot
      _reliably_ distinguish an intended flag from a flag-like
      arg which is not intended to be a flag. */
  bool hasSeenNonFlag = false;
  for( uint16_t i = 0; 0==rc && i<args->argc; ++i ){
    cwal_value * const v = args->argv[i];
    if(!hasSeenNonFlag && whcl__value_is_flag(v)){
      rc = cwal_prop_set_v(av, v, cwal_value_true());
    }else{
      hasSeenNonFlag = true;
      rc = cwal_array_append(ar, v);
    }
  }
#endif
  if(0==rc) rc = whcl_set_this(el, args->self);
  if(rc){
    cwal_unref(av);
    assert(CWAL_RC_OOM==rc);
    WHCL__WARN_OOM;
    return rc;
  }
  if(0){
    MARKER(("Function call() argc=%d...\n", (int)args->argc));
    whcl__dump_val(args->self, "args->self");
    whcl__dump_val(av, "argv");
  }
  rc = whcl_scope_set_v(el, NULL, false, el->cache.keyArgv, av);
  cwal_unref(av);
  if(0==rc) *cbArgv = ar;
  return rc;
}

/**
   The cwal_callback_f() impl used by script-side functions
   created by the proc builtin.
*/
static int cwal_callback_f_whcl( cwal_callback_args const * args, cwal_value ** rv ){
  int rc = 0;
  whcl__func * const fst = whcl__func_for_func(args->callee);
  whcl_engine * const el = whcl_engine_from_args(args);
  assert(fst && el);
  whcl_script * const oldToker = el->ct;
  whcl_script * const ct = &fst->tok.script;
  whcl__func * const oldScriptFunc = el->fcall.currentScriptFunc;
  cwal_array * cbArgv = NULL;
  cwal_value * const oldUsing = el->fcall.currentUsing;
  whcl_scope * scel = NULL;
  bool ownScope = false;
  //MARKER(("fst->flags = %04x\n", (int)fst->flags));
  //whcl__dump_val(cwal_function_value(args->callee), "callee");
  //whcl__dump_val(args->self,"args->self");
  //whcl__dump_val(fst->vName,"fst->vName");
  if(WHCL__FUNC_F_EMPTY == (fst->flags & WHCL__FUNC_F_EMPTY)){
    /* Nothing to do */
    goto end;
  }
  //MARKER(("el->scopes.nextFlags = 0x%04x\n", (int)el->scopes.nextFlags));
  if(WHCL__SCOPE_F_REUSE & el->scopes.nextFlags){
    scel = whcl_scope_current(el);
    scel->flags |= (el->scopes.nextFlags & ~WHCL__SCOPE_F_REUSE);
    el->scopes.nextFlags = 0;
  }else{
    if(!(scel = whcl__scope_push(el, el->scopes.nextFlags))){
      return CWAL_RC_OOM;
    }
    assert(0==el->scopes.nextFlags);
    ownScope = true;
  }
  el->fcall.currentScriptFunc = fst;
  el->ct = ct;
  rc = whcl__func_populate_scope(el, fst, args, &cbArgv);
  if(rc) goto end;
  el->fcall.currentUsing = fst->vImports ? fst->vImports : cwal_value_undefined();
  assert(cwal_value_refcount(cwal_array_value(cbArgv))
         && "Expected callback scope setup to take a ref.");
  /** Reminder: cbArgv is stored in the current scope as the var argv.
      That makes it vacuum-proof against any vacuums triggered by the
      upcoming parameter handling. */
#if 0
  MARKER(("Calling script function,\n"));
  whcl__dump_val(fst->vName, "fst->vName");
  whcl__dump_val(fst->vScriptName, "fst->vScriptName");
  whcl__dump_val(fst->vImports, "fst->vImports");
  whcl__dump_val(cwal_array_value(cbArgv), "cbArgv");
#endif
  if(0==(WHCL__FUNC_F_EMPTY_PARAMS & fst->flags)){
    /* It might seem intuitive that we can skip params processing if
       the _body_ is empty, but processing params can invoke
       functions, i.e. have side effects, so we need to always process
       them. Potential TODO: scan the parameters for a CallBlock
       token. If none is found, set a flag which tells us that we can
       skip processing non-empty params if the body is empty. */
    rc = whcl__func_process_params(el, cbArgv, fst);
  }
  if(0==rc && 0==(WHCL__FUNC_F_EMPTY_BODY & fst->flags)){
    rc = whcl__eval_sub2(el, false, ct, fst->tok.body, 0, NULL);
  }
  switch(rc){
    case 0:
      *rv = cwal_value_undefined();
      break;
    case CWAL_RC_RETURN:
      rc = 0;
      *rv = cwal_propagating_take(args->engine);
      assert(*rv && "Else violation of CWAL_RC_RETURN semantics.");
      break;
  }

  end:
  /** Reminder: cbArgv's only known refcount point is from it being
      stored in the current scope. We're not leaking it here. It
      "might" have been propagated out via the script func, but that's
      perfectly fine and legitimate. */
  el->fcall.currentUsing = oldUsing;
  el->ct = oldToker;
  el->fcall.currentScriptFunc = oldScriptFunc;
  if(ownScope) whcl_scope_pop(el, scel, rc ? NULL : *rv);
  return rc;
}

/**
   A cwal_value_rescoper_f() intended for use with
   cwal_function instances holding whcl__func native
   data.
*/
static int cwal_value_rescoper_f_whcl__func(cwal_scope * s,
                                            cwal_value * v){
  cwal_function * const f = cwal_value_get_function(v);
  whcl__func * const fst =  whcl__func_for_func(f);
  if(fst->vImports) cwal_value_rescope(s, fst->vImports);
  if(fst->vName) cwal_value_rescope(s, fst->vName);
  /* fst->vScriptName is already safely held in the function script
     name cache.
     if(fst->vScriptName) cwal_value_rescope(s, fst->vScriptName);*/
  return 0;
}

cwal_function * whcl__func_new(whcl_engine * const el,
                               whcl__func ** rv){
  whcl__func * const fst = whcl__func_alloc(el);
  if(!fst) return NULL;
  cwal_function * const f =
    cwal_new_function(el->ec, cwal_callback_f_whcl,
                      fst, cwal_finalizer_f_whcl__func,
                      &whcl__func_empty);
  if(!f){
    WHCL__WARN_OOM;
    whcl__func_free(el, fst, true);
    return NULL;
  }
  cwal_function_set_rescoper( f, cwal_value_rescoper_f_whcl__func );
  *rv = fst;
  return f;
}

cwal_value * whcl__script_name(whcl_engine * const el,
                               char const * fname,
                               uint16_t nameLen){
  cwal_value * vn;
  assert(el->cache.scriptNames);
  vn = cwal_prop_get(el->cache.scriptNames, fname, nameLen);
  if(vn) return vn;
  vn = cwal_new_string_value(el->ec, fname, nameLen);
  if(!vn){
    WHCL__WARN_OOM;
    return NULL;
  }else{
    cwal_ref(vn);
    int const rc = cwal_prop_set_v(el->cache.scriptNames, vn, vn);
    cwal_unref(vn);
    return rc ? NULL : vn;
  }
}

int whcl__bic_f_proc(whcl__bic const * const bic,
                     whcl__args const * const args,
                     cwal_value **rv){
  int rc = 0;
  cwal_size_t const holdLen = whcl__holder_len(args->el);
  cwal_function * f = NULL;
  cwal_value * fv = NULL;
  whcl__func * fst = NULL;
  uint16_t ndx = 1;
  bool declGlobal = false;
  bool isAnon = args->isSubcall;
  cwal_flags16_t funcFlags = 0;
  whcl_stoken const * tArg;

  while( (tArg = whcl__args_hasflag(args, ndx)) ){
    ++ndx;
    if(whcl_stoken_matches(args->ct, tArg, "-global", 7)){
      declGlobal = true;
      continue;
    }else if(whcl_stoken_matches(args->ct, tArg, "-anon", 5)){
      isAnon = true;
      continue;
    }else if(whcl_stoken_matches(args->ct, tArg, "-local", 6)){
      /* This is the default behavior but the flag is provided
         to give users a way to override the special-case -anon
         default when args->isSubcall. */
      isAnon = declGlobal = false;
      continue;
    }
#if 1
    /* EXPERIMENTAL AND SUBJECT TO CHANGE */
    else if(whcl_stoken_matches(args->ct, tArg, "-xsym", 5)){
      funcFlags |= WHCL_CONTAINER_F_XSYM;
      continue;
    }
#endif
    else{
      rc = whcl_err_throw(args->el, CWAL_RC_MISUSE,
                          "Invalid flag for %s: %.*s",
                          args->commandName,
                          (int)tArg->length,
                          whcl_stoken_cstr(args->ct, tArg, NULL, false));
      goto end;
    }
  }

  rc = whcl__args_ncheck(args, ndx, 2);
  if(rc) goto end;
  else if(declGlobal && isAnon){
    goto proc_malformed;
  }
  
  f = whcl__func_new(args->el, &fst);
  if(!f) goto proc_oom;
  assert(fst);
  fv = cwal_function_value(f);
  if(funcFlags) cwal_container_client_flags_set(fv, funcFlags);
  assert(fv);
  cwal_ref(fv);
  if((rc = whcl__holder_push(args->el, fv))) goto end; 
  tArg = args->argv[ndx++];
  whcl_script * fCT = &fst->tok.script;
  rc = whcl__script_slice(args->ct,
                          args->argv[1],
                          args->argv[args->argc-1], &fCT);
  if(rc) goto end;
  assert(fCT->chain);
  assert(fCT->ownSrc);
  tArg = whcl__script_next_token1(fCT);
  while(TOK1_T_WHCLFlag==tArg->ttype){
    tArg = whcl__script_next_token1(fCT);
  }
  if(TOK1_T_Identifier==tArg->ttype){
    fst->tok.fname = tArg;
  }else if(TOK1_T_SquigglyGroup!=tArg->ttype){
    goto proc_malformed;
  }else{
    fst->tok.params = tArg;
    if(!whcl_stoken_has_inner_content(fCT, fst->tok.params)){
      fst->flags |= WHCL__FUNC_F_EMPTY_PARAMS;
    }
  }
  tArg = whcl__script_next_token1(fCT);
  if(!tArg){ tArg = args->argv[0]; goto proc_malformed; }
  else if(TOK1_T_SquigglyGroup!=tArg->ttype){goto proc_malformed;}
  else if(fst->tok.params){
    fst->tok.body = tArg;
  }else{
    fst->tok.params = tArg;
    tArg = whcl__script_next_token1(fCT);
    if(!tArg){tArg = args->argv[0]; goto proc_malformed;}
    else if(TOK1_T_SquigglyGroup!=tArg->ttype){goto proc_malformed;}
    fst->tok.body = tArg;    
  }
  if(!whcl_stoken_has_inner_content(fCT, fst->tok.body)){
    fst->flags |= WHCL__FUNC_F_EMPTY_BODY;
  }

  if(args->ct->name){
    if((fst->vScriptName
        = whcl__script_name(args->el, args->ct->name,
                            (uint16_t)cwal_strlen(args->ct->name)))){
      fCT->name = cwal_value_get_cstr(fst->vScriptName, NULL);
      assert(fCT->name);
    }else{
      goto proc_oom;
    }
  }

  tArg = whcl__script_next_token1(fCT);

  if(tArg && !whcl_stoken_is_eof(tArg)){
    if(!(TOK1_T_BIV_using==tArg->ttype2    // "using"
         || (TOK1_T_WHCLFlag==tArg->ttype // "-using"
             && whcl_stoken_matches(fCT, tArg, "-using", 6)))){
      rc = whcl__throw(args->el, fCT, tArg, CWAL_SCR_SYNTAX,
                       "Expecing 'using', '-using', or nothing "
                       "at the end of proc.");
      goto end;
    }
    whcl_stoken const * const tUsing = tArg;
    uint32_t rdobjFlags = 0;
    whcl_stoken const * tBeforeUsingBody = tUsing;
    if(TOK1_T_WHCLFlag==tUsing->ttype /* -using */){
      fst->flags |= WHCL__FUNC_F_NO_IMPORT_USING;
    }
    tArg = whcl__script_next_token1(fCT);
    if(tArg && whcl_stoken_matches(fCT, tArg, "-scope", 6)){
      rdobjFlags |= WHCL__READ_OBJ_F_SCOPE;
      tBeforeUsingBody = tArg;
      tArg = whcl__script_next_token1(fCT);
    }
    whcl__script_errtoken_set(fCT, tArg);
    if(!tArg || TOK1_T_SquigglyGroup!=tArg->ttype){
      rc = whcl__script_err(fCT, CWAL_SCR_SYNTAX,
                        "Expecting {...object body...} after 'using'.");
      goto end;
    }
    whcl__script_errtoken_set(fCT, NULL);
    rc = whcl__read_new_object(args->el, fCT, tBeforeUsingBody,
                               rdobjFlags, &fst->vImports);
    if(rc) goto end;
    cwal_ref(fst->vImports);
    //whcl__dump_val(fst->vImports, "fst->vImports");
    tArg = whcl__script_next_token1(fCT);
    //whcl__dump_stok(fCT, tArg, "fCT token");
  }
      
  if(tArg && TOK1_T_EOF!=tArg->ttype){
    whcl__script_errtoken_set(args->ct, args->argv[args->argc-1]);
    rc = whcl__script_err(args->ct, CWAL_SCR_SYNTAX,
                      "Extra tokens after %s.",
                      bic->name);
    goto end;
  }

  if(fst->tok.fname){
    fst->vName = cwal_new_string_value(args->el->ec,
                                       whcl_stoken_cstr(fCT, fst->tok.fname, NULL, false),
                                       (cwal_size_t)fst->tok.fname->length);
    if(!fst->vName) goto proc_oom;
#if 1
    /* KLUDGE: in the "single-scope" eval model, fst->vName is being
       prematurely destroyed by means beyond my understanding unless
       we stash it inside a container (such as the function
       object). We should not need this, and it's fragile in the sense
       that a call to cwal_props_clear(fv) will pull it out from under
       us. Reminder to self: value recycling can hide this problem but
       do not be fooled: it exists. */
    cwal_ref(fst->vName) /* this ref is owned by the destructor */;
    rc = cwal_prop_set_with_flags_v(fv, fst->vName, fst->vName,
                                    CWAL_VAR_F_HIDDEN |
                                    CWAL_VAR_F_CONST);
    if(rc){
      cwal_unref(fst->vName); 
      fst->vName = NULL;
      goto end;
    }
#else
    cwal_ref(fst->vName);
#endif
  }else if(!isAnon){
    whcl__script_errtoken_set(args->ct, args->argv[2]);
    rc = whcl_err_set(args->el, CWAL_SCR_SYNTAX,
                      "Cannot declare a non-anonymous function "
                      "without a name.");
    goto end;
  }
    
  if(!isAnon){
    assert(fst->vName);
    rc = whcl_var_decl_v(args->el, declGlobal
                         ? whcl_scope_for_level(args->el, 1)
                         : NULL, false, fst->vName, fv);
    if(rc) goto end;
  }

  //whcl__dump_stok(fCT, fst->tok.params, "fst->tok.params");
  //whcl__dump_stok(fCT, fst->tok.body, "fst->tok.body");
  
  end:
  whcl__holder_truncate(args->el, holdLen, NULL);
  if(rc || !rv){
    cwal_unref(fv);
  }else{
    assert(fv);
    assert(cwal_value_refcount(fv));
    *rv = fv;
    cwal_unhand(fv);
  }  
  return rc;
  proc_malformed:
  if(!tArg || TOK1_T_EOF==tArg->ttype){
    tArg = args->argv[args->argc-1];
  }
  whcl__script_errtoken_set(args->ct, tArg);
  //whcl__dump_stok(args->ct, tArg, "err pos");
  rc = whcl__script_err(args->ct, CWAL_SCR_SYNTAX,
                        "Malformed proc. Expecting: %s",
                        bic->usage);
  goto end;
  proc_oom:
  WHCL__WARN_OOM;
  rc = whcl__err_oom(args->el);
  goto end;
}

#undef MARKER
