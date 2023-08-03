/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
/**
   Static routines and config data intended to be included directly
   into the two regex modules. This file is not intended to be
   compiled standalone.

   2022-03: though this code initially supported a 3rd-party
   JavaScript-style regex library, that lib was found to be horribly
   buggy and the developer abandoned it, so those bits are essentially
   placeholders for whichever implementation eventually gets slotted
   in there.
*/

#if !defined(RX_FLAVOR) || (RX_FLAVOR!=1 && RX_FLAVOR!=2)
#  error "Define RX_FLAVOR to 1 (POSIX) or 2 (JS) before including this file."
#endif
#define RX_FLAVOR_POSIX 1
#define RX_FLAVOR_JS 2
#if !defined(REGEX_MAX_CAPTURES)
# error "Missing REGEX_MAX_CAPTURES #define"
#endif

#if RX_FLAVOR_POSIX == RX_FLAVOR
#  define RX_IS_POSIX 1
#  define RX_REGEX_T regex_t
#  define RX_MATCH_T regmatch_t
#  define RX_MATCHES_CLEAR memset(&matches[0], 0, sizeof(matches))
#else
#  define RX_IS_POSIX 0
#endif

#if RX_FLAVOR_JS == RX_FLAVOR
#  define RX_IS_JS 1
#  define RX_REGEX_T struct Reprog
#  define RX_MATCH_T struct Resub
#  define RX_MATCHES_CLEAR memset(&matches, 0, sizeof(matches))
#else
#  define RX_IS_JS 0
#endif

/**
   Extended regex flags. These must not be passed to the underlying
   regexec()/regcomp() routines or else the underlying API might not
   like it. e.g.  POSIX regexec() complains "invalid regex" if passed
   a flags value it does not know. So... keep these values above 0xFF
   and mask all flags passed to those routines with &0xFF.
*/
enum {
/* custom not-REG_EXTENDED flag */
REG__BASIC = 0x100,
/**
   Tells replace() to eval its replacement value (presumably a string,
   but it need not be because of how we eval it).
*/
REG__REPLACE_EVAL = 0x200,
/**
   Tells replace() to export the matches to an array named, in the
   scope of a replacement callback or eval, $.  The indexes of $
   correspond to conventional regex conventions: 0=the whole match and
   1+ are captures.
*/
REG__REPLACE_DOLLAR = 0x400
};

/**
   Creates a new cwal string from [str, str+slen) and appends it to to
   the given array. Returns 0 on success, CWAL_RC_OOM on error.
*/
static int rx_append_string( cwal_array * tgt, char const * str,
                               cwal_size_t slen ){
  int rc;
  cwal_engine * const e = cwal_value_engine(cwal_array_value(tgt));
  cwal_value * v = cwal_new_string_value(e, str, slen);
  if(v){
    cwal_value_ref(v);
    rc = cwal_array_append(tgt, v);
    cwal_value_unref(v);
  }else{
    rc = CWAL_RC_OOM;
  }
  return rc;
}

/**
   Extract the match at the position specified by the 4th argument.

   The origin argument is required only for POSIX regexes, and must
   point to the start of the string from which the captures were
   taken.

   If matchNumber is out of bounds, NULL is returned and *nStr is set
   to 0, else the start of the string is returned and *nStr is set to
   its length in bytes.

*/
#if RX_IS_POSIX
#define POSIX_END_INDEX(RX) \
  (unsigned int)RX->re_nsub<REGEX_MAX_CAPTURES \
  ? RX->re_nsub+1 : REGEX_MAX_CAPTURES
static char const * rx_match_extract(regex_t const * regtype,
                                       char const * origin,
                                       regmatch_t const * matches,
                                       unsigned int matchNumber,
                                       cwal_size_t * nStr){
  regmatch_t const * m = matches+matchNumber;
  unsigned int const max = POSIX_END_INDEX(regtype);
  assert(origin && "Required for POSIX regexes.");
  if(matchNumber >= max){
    *nStr = 0;
    return 0;
  }
  *nStr = (cwal_size_t)(m->rm_eo - m->rm_so);
  return origin + m->rm_so;
}
#else
static char const * rx_match_extract(struct Reprog const * regtype,
                                       char const * origin,
                                       struct Resub const * matches,
                                       unsigned int matchNumber,
                                       cwal_size_t * nStr){
  assert((unsigned)matches->nsub >= matchNumber)
    /* JS regex fails before we can get in this situation. */
    ;
  if(matchNumber >= (unsigned)matches->nsub){
    if(regtype || origin){/*unused in this impl*/}
    *nStr = 0;
    return 0;
  }
  *nStr = (cwal_size_t)(matches->sub[matchNumber].ep-matches->sub[matchNumber].sp);
  return matches->sub[matchNumber].sp;
}
#endif

/**
   An internal helper to copy regex matches to an array. Must not be
   called unless regexec() (or equivalent) has just succeeded.

   For the given list of regex matches, this routine appends
   "something" to the given tgt array:

   If doSubs is true, each match string in the first matches->nsub
   matches is appended to the array.

   If doSubs is false, the 0th match string in the first (whole-pattern)
   match is appended to the array.

   In either case, if the number of matches 0, 0 is returned and this
   function has no side effects.

   Returns 0 on success, non-0 on fatal error (CWAL_RC_OOM being the
   only one, assuming all arguments are valid).

   For the POSIX regex module: 1) origin must point to the origin
   string, as the matches don't contain that info (they use integer
   offsets). 2) matches must be an array of regmatch_t with at least
   REGEX_MAX_CAPTURES elements.

   For the JS regex module: 1) origin may be NULL - this
   implementation does not need it. matches must be a pointer to a
   struct Resub containing the match state.
*/
static int rx_add_matches_to_list(
#if RX_IS_POSIX
                                    regex_t const * regtype,
                                    regmatch_t const * matches,
#elif RX_IS_JS
                                    struct Reprog const * regtype,
                                    struct Resub const * matches,
#endif
                                    char const * origin,
                                    cwal_array * tgt,
                                    int doSubs
                                    ){
  int rc = 0;
  unsigned int i;
#if RX_IS_POSIX
  unsigned int const end = POSIX_END_INDEX(regtype);
    /* POSIX doesn't count $0 in re_nsub. It can also apparently set
       re_nsub to be higher than the number of captures allocated for
       it.
    */;
  assert(origin && "Required for POSIX regexes.");
#elif RX_IS_JS
  unsigned int const end = (unsigned int)matches->nsub;
#endif
  /*MARKER(("doSubs=%d, end=%u\n", doSubs, end));*/
  for(i = 0; !rc && i < (doSubs ? end : 1); ++i){
    cwal_size_t nStr = 0;
    char const * str = rx_match_extract(regtype, origin, matches, i, &nStr);
    /*MARKER(("i#%d = len=%d %.*s\n", (int)i, (int)nStr, (int)nStr, origin));*/
    if(!str) break;
    rc = rx_append_string(tgt, str, nStr);
  }
  assert(CWAL_RC_OOM!=rc);
  return rc;
}

/**
   This function creates a "$" array for storing match results.

   If origin and matches are not NULL, all captures are
   extracted and added as individual strings in the resulting array.

   If rxSelf is not NULL then it must be a container-type object and
   this function assigns rxSelf['$'] to the new array.

   If rv is not NULL, the result array is written to *rv, in which
   case the caller owns it unless it was also set as a property of
   rxSelf. (Either way, the caller does not hold their own refcount
   point for *rv until they explicitly reference it.)

   At least one of rxSelf or rv must be non-NULL.
*/
static int rx_create_matchlist( cwal_engine * const e,
                                cwal_value * const rxSelf,
#if RX_IS_POSIX
                                regex_t const * regtype,
                                regmatch_t const * matches,
#elif RX_IS_JS
                                struct Reprog const * regtype,
                                struct Resub const * matches,
#endif
                                char const * origin,
                                cwal_value ** rv ){
  int rc = CWAL_RC_OOM;
  cwal_array * const ar = cwal_new_array(e);
  cwal_value * const av = cwal_array_value(ar);
  assert((rxSelf || rv) && "One of rxSelf or rv must be non-NULL.");
  if(av){
    rc = 0;
    cwal_value_ref(av);
    if(rxSelf){
      rc = cwal_prop_set(rxSelf, "$", 1, av);
    }
    if(!rc && origin && matches){
      rc = rx_add_matches_to_list( regtype, matches, origin, ar, 1 );
    }
    if(rc || !rv){
      cwal_value_unref(av) /* rxSelf may still hold a ref */;
    }else{
      cwal_value_unhand(av);
      *rv = av;
    }
  }
  return rc;
}
  
#define DOLLAR_ERSATZ "_"
//#define DOLLAR_ERSATZ2 "€"

/**
   Internal helper for the replace() implementations.

   Must be passed the callback args, destination buffer, and
   replacement value, which it evals using `[eval -> $repl]`, then
   appends the result of the eval to the given destination buffer.

   If the dollar parameter is not NULL, it is set as a scope-local
   variable named $ in the eval's (new) scope.

   Returns 0 on success and may propagate an cwal-level error/exception
   or error.
*/
static int rx_replace_eval( cwal_callback_args const * args,
                            cwal_buffer * dest,
                            cwal_value * repl,
                            cwal_value * dollar){
  int rc;
  cwal_engine * const e = args->engine;
  whcl_engine * const el = whcl_engine_from_state(e);
  cwal_value * ev = 0;
  whcl_scope * scel = whcl_scope_push(el);
  if(!scel) return CWAL_RC_OOM;
  //rc = whcl_set_v(el, NULL, se->cache.keyThis, args->self);
  //if(rc) goto end;
  if(dollar){
    rc = whcl_set(el, NULL, DOLLAR_ERSATZ, sizeof(DOLLAR_ERSATZ)-1, dollar);
#if defined(DOLLAR_ERSATZ2)
    if(0==rc) whcl_set_v(el, NULL,
                         cwal_new_xstring_value(args->engine,
                                                DOLLAR_ERSATZ2,
                                                sizeof(DOLLAR_ERSATZ2)-1),
                         dollar);
#endif
    if(rc) goto end;
  }
  /*
    To eval the replacement, set is as a scope-local var and call
    (eval->R) on it. That allows us to not concern ourselves with the
    data type of the replacement. Contrast that with extracting the
    replacement's string and evaling that (which would also have the
    potential for "the buffer modification problem").
  */
  rc = whcl_set(el, NULL, "R", 1, repl);
  if(rc) goto end;
  rc = whcl_set_this(el, args->self);
  if(0==rc){
    rc = whcl_eval_cstr(el, false, "regex.replace arg eval",
                        "eval -> $R", -1, dest ? &ev : NULL);
  }
  if(dest){
    cwal_ref(ev);
    if(!rc){
      rc = whcl_value_to_buffer(el, dest, ev ? ev : cwal_value_undefined());
    }
    cwal_unref(ev);
  }
  end:
  whcl_scope_pop(el, scel, NULL);
  return rc;
}

/**
   Internal helper for the replace() implementations.

   This function handles the basic callback variant of
   replace(string,callback). It calls callback(matchStr)
   and appends the result of the call to the dest buffer.

   If the dollar parameter is not NULL, it is set as a scope-local
   variable named $ in the call's (new) scope.

   Returns 0 on success and may propagate an cwal-level error/exception
   or error.
*/
static int rx_replace_callback( cwal_callback_args const * args,
                                cwal_function * callback,
                                cwal_buffer * dest,
                                char const * matchStr,
                                cwal_size_t matchLen,
                                cwal_value * dollar ){
  cwal_value * fargs = 0;
  cwal_value * frv = 0;
  whcl_engine * const el = whcl_engine_from_args(args);
  whcl_scope * scel;
  int rc;
  if(!(scel = whcl_scope_push(el))) return CWAL_RC_OOM;
  if(dollar){
    rc = whcl_set(el, NULL, DOLLAR_ERSATZ, sizeof(DOLLAR_ERSATZ)-1, dollar);
#if defined(DOLLAR_ERSATZ2)
    if(0==rc) whcl_set_v(el, NULL,
                         cwal_new_xstring_value(args->engine,
                                                DOLLAR_ERSATZ2,
                                                sizeof(DOLLAR_ERSATZ2)-1),
                         dollar);
#endif
    if(rc) goto end;
  }
  fargs = cwal_new_string_value(args->engine, matchStr, matchLen);
  if(!fargs){ WHCL__WARN_OOM; rc = CWAL_RC_OOM; goto end; }
  cwal_ref(fargs);
  rc = whcl_function_call(callback, args->self, WHCL_FCALL_F_REUSE_WSCOPE,
                          dest ? &frv : NULL, 1, &fargs );
  //MARKER(("scope level=%d replace callback rc=%s\n", (int)scel->level, cwal_rc_cstr(rc)));
  cwal_unref(fargs);
  if(dest){
    cwal_ref(frv);
    if(!rc){
      rc = whcl_value_to_buffer(el, dest, frv
                                ? frv : cwal_value_undefined());
    }
    cwal_unref(frv);
  }
  end:
  whcl_scope_pop(el, scel, NULL);
  return rc;
}


/**
   Mode-of-operation flags for use with rx_flag_arg().
 */
enum rx_flag_arg_mode {
/** Indicates flags for regcomp(). */
RX_FLAG_ARG_COMPILE = 0,
/** Indicates flags for regexec(). */
RX_FLAG_ARG_EXEC = 1,
/** Indicates flags for regexec() plus some specific to regex.replace(). */
RX_FLAG_ARG_REPLACE = 2
};

/**
   Normalizes regcomp()/regexec() flag values, depending on the value
   of the 2nd argument.

   If v is not NULL then...

   - If v is a string, it is treated a set of mode-specific letters
   identifying the flags, and an unknown letter triggers an exception.

   - If v is an integer it is treated like a bitmask of regex flags,
   and unknown flags are silently ignored.

   - If neither of those apply, an exception is triggered.

   If v is NULL, the flags evaluate to a flavor- and mode-specific
   default value.

   On success:

   - 0 is returned.

   - *flags is assigned the new normalized flags value.

   - zFlags, if not NULL, will contain the NUL-terminated string form
   of those flags. It must be at least 7 bytes long.

   On error an exception is triggered an
   CWAL_RC_EXCEPTION is returned (or a lower-level error if throwing
   fails (e.g. OOM)).

   TODO: the bitmask flags are, for purposes of the script-side API,
   deprecated. Add a (cwal_value**) parameter which, if not NULL, gets
   set to the string form of the normalized arguments. Then use that
   for the regex.flags property value.
*/
static int rx_flag_arg(cwal_engine * e, enum rx_flag_arg_mode mode,
                         cwal_value const * v, int * flags,
                         char * zFlags ){
  char const *modeNames[] = {
  /* The order of these must match enum rx_flag_arg_mode, as those
     values are used for array indexes here. */
  "compile", "match", "replace"
  };
  char * fPos = zFlags;
#if RX_IS_POSIX
  /* POSIX */
  int f = RX_FLAG_ARG_COMPILE==mode ? REG_EXTENDED : 0;
  /*if(f && zFlags) *fPos++='e';*/
#elif RX_IS_JS
  /* JS */
  int f = 0;
#endif
#define ZF if(fPos) *fPos++ = *str
  if(v){
    cwal_size_t nStr =0;
    char const * str = cwal_value_get_cstr(v, &nStr);
    if(str){
      char const * end = str + nStr;
      char invalid = 0;
      for( ; str < end; ++str ){
        invalid = 0;
#if RX_IS_POSIX
        switch(mode){
          case RX_FLAG_ARG_COMPILE:
            switch(*str){
              case (int)'B': f &= ~REG_EXTENDED; ZF; break;
              case (int)'e': f |= REG_EXTENDED; ZF; break;
              case (int)'i': f |= REG_ICASE; ZF; break;
              case (int)'n': f |= REG_NEWLINE; ZF; break;
              case (int)'s': f |= REG_NOSUB; ZF; break;
              default: invalid = *str; break;
            }
            break;
          case RX_FLAG_ARG_EXEC:
            switch(*str){
              case (int)'b': f |= REG_NOTBOL; ZF; break;
              case (int)'e': f |= REG_NOTEOL; ZF; break;
              default: invalid = *str; break;
            }
            break;
          case RX_FLAG_ARG_REPLACE:
            switch(*str){
              case (int)'b': f |= REG_NOTBOL; ZF; break;
              case (int)'e': f |= REG_NOTEOL; ZF; break;
              case (int)'E': f |= REG__REPLACE_EVAL; ZF; break;
              case (int)'_':
              case (int)'$': f |= REG__REPLACE_DOLLAR; ZF; break;
              default: invalid = *str; break;
            }
            break;
        }
#elif RX_IS_JS
        switch(mode){
          case  RX_FLAG_ARG_COMPILE:
            switch(*str){
              case (int)'i': f |= REG_ICASE; ZF; break;
              case (int)'n': f |= REG_NEWLINE; ZF; break;
              default: invalid = *str; break;
            }
            break;
          case RX_FLAG_ARG_EXEC:
            switch(*str){
              case (int)'b': f |= REG_NOTBOL; ZF; break;
              default: invalid = *str; break;
            }
            break;
          case RX_FLAG_ARG_REPLACE:
            switch(*str){
              case (int)'b': f |= REG_NOTBOL; ZF; break;
              case (int)'E': f |= REG__REPLACE_EVAL; ZF; break;
              case (int)'_':
              case (int)'$': f |= REG__REPLACE_DOLLAR; ZF; break;
              default: invalid = *str; break;
            }
            break;
        }
#endif
#undef ZF
        if(invalid) break;
      }
      /* Note that a \0 char in str will trigger invalid='\0' (0), but
         that seems like a harmless corner case. */
      if(invalid){
        return cwal_exception_setf(e, CWAL_RC_RANGE,
                                   "Invalid %s flag letter '%c'.",
                                   modeNames[mode], invalid);
      }
    }else if(cwal_value_is_integer(v)){
      cwal_int_t const vf = cwal_value_get_integer(v);
#define ZF(CH) if(fPos) *fPos++=CH
#if RX_IS_POSIX
      switch(mode){
        case RX_FLAG_ARG_COMPILE:
          if(REG_NOSUB & vf) { f |= REG_NOSUB; ZF('s'); }
          if(REG_NEWLINE & vf) { f |= REG_NEWLINE; ZF('n'); }
          if(REG_ICASE & vf) { f |= REG_ICASE; ZF('i'); }
          if(REG__BASIC & vf) { f &= ~REG_EXTENDED; ZF('B'); }
          break;
        case RX_FLAG_ARG_EXEC:
        case RX_FLAG_ARG_REPLACE:
          if(REG_NOTBOL & vf) { f |= REG_NOTBOL; ZF('b'); }
          if(REG_NOTEOL & vf) { f |= REG_NOTEOL; ZF('e'); }
          break;
      }
#elif RX_IS_JS
      switch(mode){
        case RX_FLAG_ARG_COMPILE:
          if(REG_NEWLINE & vf) { f |= REG_NEWLINE; ZF('n'); }
          if(REG_ICASE & vf) { f |= REG_ICASE; ZF('i'); }
          break;
        case RX_FLAG_ARG_EXEC:
        case RX_FLAG_ARG_REPLACE:
          if(REG_NOTBOL & vf) { f |= REG_NOTBOL; ZF('i'); }
          break;
      }
#endif
#undef ZF
    }else{
      return cwal_exception_setf(e, CWAL_RC_TYPE,
                                 "Regex %s flags must be an integer or string.",
                                 modeNames[mode]);
    }
  }
  *flags = f;
  if(fPos) *fPos = 0;
  return 0;
}

static int rx_cb_compile( cwal_callback_args const * args, cwal_value **rv );
static int rx_cb_destroy( cwal_callback_args const * args, cwal_value **rv );
static int rx_cb_exec( cwal_callback_args const * args, cwal_value **rv );
static int rx_cb_match_all( cwal_callback_args const * args, cwal_value **rv );
static int rx_cb_replace( cwal_callback_args const * args, cwal_value **rv );
static int rx_cb_eachmatch( cwal_callback_args const * args, cwal_value **rv );
static int rx_cb_split( cwal_callback_args const * args, cwal_value **rv );
static int rx_cb_test( cwal_callback_args const * args, cwal_value **rv );
#if RX_IS_POSIX
static int rx_posix_cb_error(cwal_callback_args const * const args, char const * msgPrefix,
                            int rxcode, regex_t * self);
static int rx_posix_check_capture_count(cwal_callback_args const * args,
                                        regex_t const * rx );
#define RX_POSIX_THIS_HAS_NOSUB rx_posix_has_nosub(args->self)
static int rx_posix_has_nosub(cwal_value * vRegex){
  cwal_size_t fLen = 0;
  cwal_size_t i;
  char const * f = cwal_value_get_cstr(cwal_prop_get(vRegex, "flags", 5), &fLen);
  for( i = 0; i < fLen; ++i ){
    if('s' == f[i]) return 1;
  }
  return 0;
}
static int rx_posix_nono_nosub(cwal_callback_args const * const args){
  return cwal_cb_throw(args, CWAL_RC_MISUSE,
                     "This regex was compiled with the NOSUB flag, "
                     "so this routine cannot work :(.");
}
#endif

/**
   cwal_native type ID for type-safe (void*) checks. Its _value_ is
   irrelevant - we just use its address.
*/
static const int rx_typeid = 42;
/**
   Type identifier for the regex prototype's state pointer (void*)
   which gets stashed in the module's function. Its _value_ is
   irrelevant - we just use its address.
*/
static const int rx_prototype_id = 42;

/* helper macro for use in callback funcs */
#define THIS_REGEX \
    cwal_native * nself = cwal_value_get_native(args->self);    \
    RX_REGEX_T * self = nself \
        ? (RX_REGEX_T *)cwal_native_get(nself, &rx_typeid) : NULL; \
    if(!self) return cwal_cb_throw(args, CWAL_RC_TYPE,                      \
                                 "'this' is not (or is no longer) "\
                                 "a native regex instance.")


/**
   string regex.replace(string haystack, mixed replacement
                        [, int maxReplacements = 0 [, mixed execFlags=0]])

   string regex.replace(string haystack, mixed replacement,
                        string execFlags)

   Replaces instances of the regex's complete match in haystack with the
   given replacement. If replacement is a function, it is passed the
   complete match text and the result of the call becomes the
   replacement text. If replacement is a string but execFlags contains
   the 'E' flag then the replacement string gets eval'd. In the
   context of a replacement function or eval, 'this' is the this
   regex.

   If execFlags is passed it, it must be an integer of flags suitable
   for passing to regexec(). Use 0 for the defaults.

   If maxReplacements is passed in, it must be an integer. A value of
   0 or less means unlimited, else the number of replacements is
   limited to the given value.
*/
static int rx_cb_replace( cwal_callback_args const * args, cwal_value **rv ){
#if RX_IS_POSIX
  regmatch_t matches[REGEX_MAX_CAPTURES];
#elif RX_IS_JS
  struct Resub matches;
#endif  
  char const * haystack;
  char const * haystackEnd;
  char const * cursor = 0;
  char const * prevEnd = 0;
  cwal_size_t nHaystack = 0;
  cwal_value * theRv = 0;
  cwal_function * callback = 0;
  cwal_value * vRepl = 0;
  cwal_int_t replMax = 0;
  cwal_int_t replCount = 0;
  cwal_buffer buf = cwal_buffer_empty;
  whcl_engine * const el = whcl_engine_from_args(args);
  int rc = 0;
  int execFlags = 0;
  THIS_REGEX;
#if RX_IS_POSIX
  if(RX_POSIX_THIS_HAS_NOSUB){
    goto err_nosub;
  }
#endif
  if(args->argc<2 || args->argc>4){
    usage:
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting "
                       "(string text, mixed replacement "
                       "[, int maxRepl=0 [, string flags]]) "
                       "or (text, replacement, string flags) "
                       "arguments.");
  }else{
    cwal_value * arg = args->argv[0];
    haystack = cwal_value_is_string(arg)
      ? cwal_value_get_cstr(arg, &nHaystack)
      : 0;
    if(!haystack){
      return cwal_cb_throw(args, CWAL_RC_MISUSE,
                         "Expecting a string argument.");
    }
    assert(cwal_value_refcount(arg) || cwal_value_is_builtin(arg));
    if(!nHaystack){
      /* There's nothing to do here. */
      *rv = arg;
      return 0;
    }
    arg = args->argv[1];
    /*whcl_dump_val(args->argv[0], "argv[0]");
      whcl_dump_val(args->argv[1], "argv[1]");*/
    vRepl = arg;
    callback = cwal_value_get_function(vRepl);
    if(!callback && whcl_value_get_script(vRepl)){
      /* If it's a whcl.Script, always apply these flags */
      execFlags |= REG__REPLACE_EVAL | REG__REPLACE_DOLLAR;
    }
    if(3==args->argc && cwal_value_is_string(args->argv[2])){
      /* (string text, mixed replacement, string flags) */
      arg = args->argv[2];
      rc = rx_flag_arg(args->engine, RX_FLAG_ARG_REPLACE,
                         arg, &execFlags, 0);
      if(rc) return rc;
    }else{
      /* (string text, mixed replacement, int maxRepl, mixed flags) */
      arg = args->argc>2 ? args->argv[2] : 0;
      if(arg){
        if(!cwal_value_is_number(arg)){
          goto usage;
        }
        replMax = cwal_value_get_integer(arg);
      }
      arg = args->argc>3 ? args->argv[3] : 0;
      if(arg){
        rc = rx_flag_arg(args->engine, RX_FLAG_ARG_REPLACE,
                           arg, &execFlags, 0);
        if(rc) return rc;
      }
    }
  }
  haystackEnd = haystack + nHaystack;
  cursor = haystack;
  prevEnd = haystack;
  RX_MATCHES_CLEAR;
  for( ; cursor < haystackEnd; ){
    int rxrc;
    cwal_value * vDollar = 0;
#if RX_IS_POSIX
    rxrc = (replMax>0 && replCount++==replMax)
      ? REG_NOMATCH
      : regexec( self, cursor, REGEX_MAX_CAPTURES,
                 matches, execFlags & 0xFF )
      /*it returns 0 on a match*/;
    if(rxrc && REG_NOMATCH!=rxrc){
      rc = rx_posix_cb_error(args, "regexec()", rxrc, self);
      break;
    }
#elif RX_IS_JS
    rxrc = 
      (replMax>0 && replCount++==replMax)
      ? 1
      : regexec( self, cursor, &matches, execFlags & 0xFF )
      /*it returns 0 on a match*/;
#endif
    if(rxrc){
      if(prevEnd == haystack){
        /* No matches found: optimize this to simply return the input
           string. */
        theRv = args->argv[0];
        break;
      }
      /* Else append any remaining bits to the result */
      rc = cwal_buffer_append(args->engine, &buf, cursor,
                              (cwal_size_t)(haystackEnd - cursor));
      break;
    }
    
#if RX_IS_POSIX
#  define MATCH_BEGIN (cursor+matches[0].rm_so)
#  define MATCH_END (cursor+matches[0].rm_eo)
#  define MATCHES_PTR matches
    if(MATCH_BEGIN == MATCH_END /* ==> NOSUB */){
      /* Just in case the "flags" property check didn't work
         (e.g. flag was removed). */
      goto err_nosub;
    }
    rc = rx_posix_check_capture_count(args, self);
    if(rc) break;
#elif RX_IS_JS
#  define MATCH_BEGIN matches.sub[0].sp
#  define MATCH_END matches.sub[0].ep
#  define MATCHES_PTR &matches
#endif
    if(prevEnd < MATCH_BEGIN){
      /* Append non-matched part between the end of the previous match
         (or start of input) and current match start. */
      rc = cwal_buffer_append(args->engine, &buf, prevEnd,
                              (cwal_size_t)(MATCH_BEGIN - prevEnd));
    }
    /*MARKER(("current buffer: %.*s\n", (int)buf.used, (char const *)buf.mem));*/
    if(REG__REPLACE_DOLLAR & execFlags){
      /*MARKER(("$ nsub=%d\n",(int)self->re_nsub));*/
      rc = rx_create_matchlist(args->engine, NULL,
                               self, MATCHES_PTR, cursor, &vDollar);
      if(rc) break;
      cwal_value_ref(vDollar);
    }
    if(callback){
      /* Reminder: having a callback trumps the REG__REPLACE_EVAL
         flag, and that's on purpose. That flag is effectively a no-op
         when the replacement is a function. */
      rc = rx_replace_callback(args, callback, &buf,
                               MATCH_BEGIN,
                               (cwal_size_t)(MATCH_END-MATCH_BEGIN),
                               vDollar);
    }else if(REG__REPLACE_EVAL & execFlags){
      rc = rx_replace_eval(args, &buf, vRepl, vDollar);
    }else{
      rc = whcl_value_to_buffer(el, &buf, vRepl);
    }
    if(REG__REPLACE_DOLLAR & execFlags){
      cwal_value_unref(vDollar);
    }
    if(rc) break;
    prevEnd = cursor = MATCH_END;
  }
#undef MATCH_BEGIN
#undef MATCH_END
#undef MATCHES_PTR
  goto end;
#if RX_IS_POSIX
  err_nosub:
  rc = rx_posix_nono_nosub(args);
#endif
  end:
  assert(theRv ? 0==buf.mem : 1);
  if(rc){
    assert(!theRv);
  }else{
    *rv = theRv ? theRv : cwal_buffer_to_zstring_value(args->engine, &buf);
    if(!*rv) rc = CWAL_RC_OOM;
  }
  if(buf.mem) cwal_buffer_reserve(args->engine, &buf, 0);
  return rc;
}

/**
  regex.eachMatch(string text, string|function callback
                  [, string matchFlags])

  For each match against the given text, calls or evals the
  callback. matchFlags may be any flags supported by replace(), noting
  that the replace()-specific 'E' and '$' flags are implied when the
  callback is a string to eval (because the eval would have no
  information about the match without '$' and the string needs the 'E'
  flag to get eval'd). 'E' is ignored when the callback is a function.

  Yet again, POSIX NOSUB regexes cannot work with this routine and
  will trigger an exception.
*/
static int rx_cb_eachmatch( cwal_callback_args const * args, cwal_value **rv ){
#if RX_IS_POSIX
  regmatch_t matches[REGEX_MAX_CAPTURES];
#elif RX_IS_JS
  struct Resub matches;
#endif  
  char const * haystack;
  char const * haystackEnd;
  char const * cursor = 0;
  cwal_size_t nHaystack = 0;
  cwal_function * callback = 0;
  cwal_value * vArg2 = 0;
  int rc = 0;
  int execFlags = 0;
  THIS_REGEX;
#if RX_IS_POSIX
  if(RX_POSIX_THIS_HAS_NOSUB){
    goto err_nosub;
  }
#endif
  if(args->argc<2 || args->argc>3){
    usage:
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting "
                       "(string text, function|string|Script callback "
                       "[, string flags]) "
                       "arguments.");
  }else{
    cwal_value * arg = args->argv[0];
    haystack = cwal_value_is_string(arg)
      ? cwal_value_get_cstr(arg, &nHaystack)
      : 0;
    if(!haystack){
      return cwal_cb_throw(args, CWAL_RC_MISUSE,
                         "Expecting a string argument.");
    }
    if(!nHaystack){
      /* There's nothing to do here. */
      *rv = cwal_value_undefined();
      return 0;
    }
    arg = args->argv[1];
    vArg2 = arg;
    callback = cwal_value_get_function(arg);
    if(!callback
       && !cwal_value_is_string(vArg2)
       && !whcl_value_get_script(vArg2)){
      goto usage;
    }
    else if(3==args->argc && cwal_value_is_string(args->argv[2])){
      /* (string text, mixed replacement, string flags) */
      arg = args->argv[2];
      rc = rx_flag_arg(args->engine, RX_FLAG_ARG_REPLACE,
                         arg, &execFlags, 0);
      if(rc) return rc;
    }
  }
  if(!callback){
    /* We have a string or script to eval. The $ and E flags are
       implicit in this case, else it's pretty useless. */
    execFlags |= REG__REPLACE_EVAL | REG__REPLACE_DOLLAR;
  }
  haystackEnd = haystack + nHaystack;
  cursor = haystack;
  RX_MATCHES_CLEAR;
  for( ; cursor < haystackEnd; ){
    int rxrc;
    cwal_value * vDollar = 0;
#if RX_IS_POSIX
    rxrc = regexec( self, cursor, REGEX_MAX_CAPTURES,
                    matches, execFlags & 0xFF )
      /*it returns 0 on a match*/;
    if(rxrc && REG_NOMATCH!=rxrc){
      rc = rx_posix_cb_error(args, "regexec()", rxrc, self);
      break;
    }
#elif RX_IS_JS
    rxrc = regexec( self, cursor, &matches, execFlags & 0xFF )
      /*it returns 0 on a match*/;
#endif
    if(rxrc){
      break;
    }
    
#if RX_IS_POSIX
#  define MATCH_BEGIN (cursor+matches[0].rm_so)
#  define MATCH_END (cursor+matches[0].rm_eo)
#  define MATCHES_PTR matches
    if(MATCH_BEGIN == MATCH_END /* ==> NOSUB */){
      /* Just in case the "flags" property check didn't work
         (e.g. flag was removed). */
      goto err_nosub;
    }
    rc = rx_posix_check_capture_count(args, self);
    if(rc) break;
#elif RX_IS_JS
#  define MATCH_BEGIN matches.sub[0].sp
#  define MATCH_END matches.sub[0].ep
#  define MATCHES_PTR &matches
#endif
    /*MARKER(("current buffer: %.*s\n", (int)buf.used, (char const *)buf.mem));*/
    if(REG__REPLACE_DOLLAR & execFlags){
      /*MARKER(("$ nsub=%d\n",(int)self->re_nsub));*/
      rc = rx_create_matchlist(args->engine, NULL,
                                 self, MATCHES_PTR, cursor, &vDollar);
      cwal_value_ref(vDollar);
      if(rc){
        cwal_value_unref(vDollar);
        break;
      }
    }
    if(callback){
      /* Reminder: having a callback trumps the REG__REPLACE_EVAL
         flag, and that's on purpose. That flag is effectively a no-op
         when the replacement is a function. */
      rc = rx_replace_callback(args, callback, 0,
                                 MATCH_BEGIN,
                                 (cwal_size_t)(MATCH_END-MATCH_BEGIN),
                                 vDollar);
    }else if(REG__REPLACE_EVAL & execFlags){
      rc = rx_replace_eval(args, NULL, vArg2, vDollar);
    }
    if(REG__REPLACE_DOLLAR & execFlags){
      cwal_value_unref(vDollar);
    }
    if(rc) break;
    cursor = MATCH_END;
  }
#undef MATCH_BEGIN
#undef MATCH_END
#undef MATCHES_PTR
  goto end;
#if RX_IS_POSIX
  err_nosub:
  rc = rx_posix_nono_nosub(args);
#endif
  end:
  if(!rc){
    *rv = cwal_value_undefined();
  }
  return rc;
}


/**
   Internal callback impl for test() and exec(). For test(), pass
   false for the 2nd argument, and for exec() pass true.
*/
static int rx_cb_exec_impl( cwal_callback_args const * args,
                            bool doCaptures,
                            cwal_value **rv ){
#if RX_IS_POSIX
  regmatch_t matches[REGEX_MAX_CAPTURES];
#elif RX_IS_JS
  struct Resub matches;
#endif  
  char const * str;
  cwal_size_t strLen = 0;
  /* cwal_int_t offset = 0; */
  int flags = 0;
  int rc;
  THIS_REGEX;
  str = args->argc
    ? cwal_value_get_cstr(args->argv[0], &strLen)
    : 0;
  if(!str){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting a non-empty string argument.");
  }
  rc = rx_flag_arg(args->engine, RX_FLAG_ARG_EXEC,
                     args->argc>1 ? args->argv[1] : 0, &flags, 0);
  if(rc) return rc;
  RX_MATCHES_CLEAR;
#if RX_IS_POSIX
#  define MATCHES_PTR matches
#  define GOT_NO_MATCHES (0==matches[0].rm_eo)
  rc = regexec( self, str, doCaptures ? REGEX_MAX_CAPTURES : 0,
                doCaptures ? matches : 0, flags & 0xFF );
  if(rc && REG_NOMATCH!=rc){
    return rx_posix_cb_error(args, doCaptures ? "exec()" : "test()",
                             rc, self);
  }
#elif RX_IS_JS
#  define MATCHES_PTR &matches
#  define GOT_NO_MATCHES (0==matches.nsub)
  rc = regexec( self, str, doCaptures ? &matches : NULL,
                flags & 0xFF );
#endif
  if(rc/*it returns 0 on a match*/){
    *rv = cwal_value_false();
    return 0;
  }else if(!doCaptures || GOT_NO_MATCHES){
    /* No captures or NOSUB, so just return a boolean... */
    *rv = cwal_value_true();
    return 0;
  }else{
    /* Return list of captures... */
    cwal_array * mlist = 0;
    cwal_value * mV = 0;
#if RX_IS_POSIX
    rc = rx_posix_check_capture_count(args, self);
    if(rc) return rc;
#endif
    rc = CWAL_RC_OOM;
    mlist = cwal_new_array(args->engine);
    if(mlist){
      mV = cwal_array_value(mlist);
      cwal_value_ref(mV);
      rc = rx_add_matches_to_list(self, MATCHES_PTR, str, mlist, 1);
    }
    if(rc){
      cwal_value_unref(mV);
    }else{
      *rv = cwal_value_unhand(mV);
    }
  }
#undef GOT_NO_MATCHES
#undef MATCHES_PTR
  return rc;
}

static int rx_cb_exec( cwal_callback_args const * args, cwal_value **rv ){
  return rx_cb_exec_impl(args, 1, rv);
}

static int rx_cb_test( cwal_callback_args const * args, cwal_value **rv ){
  return rx_cb_exec_impl(args, 0, rv);
}

static int rx_char_length( unsigned char c ){
  switch(0xF0 & c) {
    case 0xF0: return 4;
    case 0xE0: return 3;
    case 0xC0: return 2;
    default: return 1;
  }
}

/**
   array split(string text [, int limit = 0 [, mixed matchFlags = 0]])

   array split(string text, string matchFlags)

   Works like string.split(string pattern, int limit), but splits on
   this regex's pattern.
*/
static int rx_cb_split( cwal_callback_args const * args, cwal_value **rv ){
#if RX_IS_POSIX
  regmatch_t matches[1];
#elif RX_IS_JS
  struct Resub matches;
#endif
  char const * haystack;
  char const * haystackEnd;
  char const * cursor = 0;
  char const * prevEnd = 0;
  cwal_int_t limit = -1;
  cwal_int_t count = 0;
  int execFlags = 0;
  int rc = 0;
  cwal_array * mlist = 0;
  THIS_REGEX;
#if RX_IS_POSIX
  if(RX_POSIX_THIS_HAS_NOSUB){
    goto err_nosub;
  }
#endif
  if(args->argc<1 || args->argc>3){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                       "Expecting (string text [, int limit=-1 [, "
                       "int matchFlags=0]]) arguments.");
  }else{
    cwal_value * arg = args->argv[0];
    cwal_size_t nHaystack = 0;
    haystack = cwal_value_get_cstr(arg, &nHaystack);
    haystackEnd = haystack + nHaystack;
    cursor = haystack;
    prevEnd = haystack;
    if(!haystack){
      return cwal_cb_throw(args, CWAL_RC_MISUSE,
                         "Expecting a string or non-empty buffer argument.");
    }
    if(2==args->argc && cwal_value_is_string(args->argv[1])){
      /* (text, string flags) */
        rc = rx_flag_arg(args->engine, RX_FLAG_ARG_EXEC,
                           args->argv[1], &execFlags, 0);
        if(rc) return rc;
    }else if(args->argc>1){
      /* (text, int limit [, mixed flags]) */
      arg = args->argv[1];
      if(arg){
        limit = cwal_value_get_integer(arg);
        arg = args->argc>2 ? args->argv[2] : 0;
        if(arg){
          rc = rx_flag_arg(args->engine, RX_FLAG_ARG_EXEC,
                             arg, &execFlags, 0);
          if(rc) return rc;
        }
      }
    }
  }
  RX_MATCHES_CLEAR;
  mlist = cwal_new_array(args->engine);
  if(!mlist){
    rc = CWAL_RC_OOM;
    goto end;
  }
  if(cursor == haystackEnd){
    /* Special case: empty input */
    rc = rx_append_string(mlist, "", 0);
  }
#define IN_LIMIT (limit<0 || count<=limit)
#define cwal_vstring_empty cwal_new_string_value(args->engine, "", 0)
  for( ; cursor < haystackEnd; ){
    int rxrc;
    char const * mB;
    char const * mE;
#if RX_IS_POSIX
    rxrc = regexec( self, cursor, 1, matches, execFlags & 0xFF );
#elif RX_IS_JS
    if(cursor > haystack){
      /* This is taken from the split() impl in the JS engine this
         regex impl derives from. i'm honestly not sure why he did
         it. */
      execFlags |= REG_NOTBOL;
    }
    rxrc = regexec( self, cursor, &matches, execFlags & 0xFF );
#endif
    if(rxrc){ /* no match */
      /*MARKER(("No match. cursor=[%c]\n", *cursor));*/
      break;
    }
#if RX_IS_POSIX
    rc = rx_posix_check_capture_count(args, self);
    if(rc) break;
    mB = cursor+matches[0].rm_so;
    mE = cursor+matches[0].rm_eo;
#elif RX_IS_JS
    mB = matches.sub[0].sp;
    mE = matches.sub[0].ep;
#endif
    if(mB==prevEnd){
      /* Match at end of previous match or start of input. */
      if(prevEnd==haystack){
        if(mB==mE){
          /*MARKER(("Zero-width match at start of input. Skipping.\n"));*/
          /*
            Is skipping this the right thing to do? By comparison with JS
            in Firefox:
          
            'a || b c '.split(/ XXX/) // where XXX is really a *
            Array(6) [ "a", "|", "|", "b", "c", "" ]
          */
          cursor = mE+1;
          continue;
        }
        ++count; if(!IN_LIMIT) break;
        /*MARKER(("Match #%d at start of input.\n",(int)count));*/
        rc = rx_append_string(mlist, prevEnd,
                                  (cwal_size_t)(mB - prevEnd));
      }else{
        if(mE>mB){
          ++count; if(!IN_LIMIT) break;
          /*MARKER(("Match #%d at end of previous match. Match len=%d\n",
            (int)count, (int)(mE-mB)));*/
          rc = rx_append_string(mlist, prevEnd,
                                  (cwal_size_t)(mB - prevEnd));
        }
      }
      if(rc) break;

      if(mE>=haystackEnd){
        /* Match at end of input */
        ++count; if(!IN_LIMIT) break;
        /*MARKER(("Match at end of input.\n"));*/
        rc = cwal_array_append(mlist, cwal_vstring_empty);
        prevEnd = mE;
        break;
      }
      assert(prevEnd == cursor);
      prevEnd = cursor = mE;
      if(mE == mB) cursor += rx_char_length(*cursor);
      continue;
    }

    assert(mB>prevEnd);
    ++count; if(!IN_LIMIT) break;
    rc = rx_append_string(mlist, prevEnd,
                            (cwal_size_t)(mB - prevEnd));
    if(rc) break;
    /*MARKER(("token #%d prev. part: [%.*s]\n", (int)count, (int)(mB-prevEnd), prevEnd));
      MARKER(("token #%d match=[%.*s]\n",(int)count,(int)(mE-mB), mB));
    */
    if(mE>=haystackEnd){
      /* Match at end of input */
      /*MARKER(("Match at end of input.\n"));*/
      ++count; if(!IN_LIMIT) break;
      rc = cwal_array_append(mlist, cwal_vstring_empty);
      prevEnd = mE;
      break;
    }
    prevEnd = cursor = mE;
  }
  if(!rc && prevEnd<haystackEnd){
    ++count;
    if(IN_LIMIT){
      /*MARKER(("Capturing the tail...\n"));*/
      rc = rx_append_string(mlist, prevEnd,
                              (cwal_size_t)(haystackEnd - prevEnd));
    }
  }
#undef cwal_vstring_empty
#undef IN_LIMIT
#undef MATCH_END
  goto end;
#if RX_IS_POSIX
  err_nosub:
  rc = rx_posix_nono_nosub(args);
#endif
  end:
  if(rc){
    cwal_array_unref(mlist);
  }else{
    *rv = cwal_array_value(mlist);
  }
  return rc;
}

/**
   mixed matchAll(string text [, mixed matchFlags = 0 [, bool captureAll=false])

   mixed matchAll(string text [, bool captureAll=false])


   This function has two distinct modes:

   - If `captureAll` is `false` (the default) then if a match is
   found, a list of all complete match strings (not split into
   sub-captures) is returned. i.e. a single-dimensional list of
   strings.

   - If `captureAll` is `true` then if a match is found, a list of lists
   is returned, with each sub-list having the same structure as the
   result of `exec()`. i.e. `[["full match 1","capture 1",..."capture
   N"], ["full match 2",...] ...]`.

   In both cases, if no match is found a falsy value is returned.

   `matchFlags` is an optional flag to change how matching works,
   exactly as described for `exec()`.

   Note that for the two-argument form, the 2nd argument is only
   recognized as the `captureAll` toggle if it is a genuine boolean, not
   an arbitrary truthy/falsy value. i.e. `matchAll("blah",true)` will
   toggle `captureAll` on, but `matchAll("blah", "1")` would treat the
   2nd argument as the `flags` and use the default value for
   `captureAll`.

*/
static int rx_cb_match_all( cwal_callback_args const * args, cwal_value **rv ){
#if RX_IS_POSIX
  regmatch_t matches[REGEX_MAX_CAPTURES];
#elif RX_IS_JS
  struct Resub matches;
#endif  
  char const * str;
  char const * strEnd;
  cwal_size_t strLen = 0;
  int flags = 0;
  int rc = 0;
  cwal_array * mlist = 0;
  cwal_value * mV = 0;
  bool captureAll = false;
  uint16_t argNdx = 0;
  THIS_REGEX;
  for(; whcl_arg_has_flag(args, &argNdx, &str, &strLen);
      str = NULL, strLen = 0){
    if(8==strLen && 0==memcmp("-capture", str, 8)){
      captureAll = true;
      continue;
    }
    --argNdx;
    str = NULL;
    strLen = 0;
    break;
  }
  str = args->argc>argNdx
    ? cwal_value_get_cstr(args->argv[argNdx], &strLen)
    : NULL;
  /*MARKER(("match all pattern: %s\n",
    cwal_value_get_cstr(cwal_prop_get(args->self,"pattern",7), 0)));*/
  if(!str){
    return cwal_cb_throw(args, CWAL_RC_MISUSE,
                         "Expecting a non-empty string argument.");
  }
  strEnd = str + strLen;
#if RX_IS_POSIX
  if(RX_POSIX_THIS_HAS_NOSUB){
    goto err_nosub;
  }
#endif
  ++argNdx;
  if(args->argc>argNdx){
    /* ( str, flags ) */
    cwal_value * varg = args->argv[argNdx++];
    rc = rx_flag_arg(args->engine, RX_FLAG_ARG_EXEC,
                     varg, &flags, 0);
    if(rc) return rc;
  }
  RX_MATCHES_CLEAR;
  for( ; str < strEnd; ){
#if RX_IS_POSIX
#  define MATCH_END (str+matches[0].rm_eo)
#  define MATCHES_PTR matches
    int const rxrc = regexec( self, str,
                              captureAll ? REGEX_MAX_CAPTURES : 1,
                              matches, flags & 0xFF );
    if(REG_NOMATCH==rxrc){
      break;
    }else if(rxrc){
      rc = rx_posix_cb_error(args, "regexec()", rxrc, self);
      break;
    }
    if(str == MATCH_END /* ==> NOSUB */){
      /* Just in case the "flags" property check didn't
         work (e.g. flag was removed). */
      goto err_nosub;
    }
    rc = rx_posix_check_capture_count(args, self);
    if(rc) break;
#elif RX_IS_JS
#  define MATCH_END matches.sub[0].ep
#  define MATCHES_PTR &matches
    if(regexec( self, str, &matches, flags & 0xFF )){
      break;
    }
#endif
    if(!mlist){
      mlist = cwal_new_array(args->engine);
      if(!mlist){
        rc = CWAL_RC_OOM;
        break;
      }
      mV = cwal_array_value(mlist);
      cwal_value_ref(mV);
    }
    if(captureAll){
      cwal_array * aSub = cwal_new_array(args->engine);
      if(!aSub) rc = CWAL_RC_OOM;
      else{
        cwal_value * subV = cwal_array_value(aSub);
        cwal_value_ref(subV);
        rc = rx_add_matches_to_list( self, MATCHES_PTR,
                                       str, aSub, 1 );
        if(!rc) rc = cwal_array_append(mlist, subV);
        cwal_value_unref(subV);            
      }
    }else{
        rc = rx_add_matches_to_list( self, MATCHES_PTR,
                                       str, mlist, 0 );
    }
    if(rc) break;
    assert(str != (MATCH_END));
    str = MATCH_END;
  }
#undef MATCH_END
#undef MATCHES_PTR
  goto end;
#if RX_IS_POSIX
  err_nosub:
  rc = rx_posix_nono_nosub(args);
#endif
  end:
  if(mV){
    if(rc) cwal_value_unref(mV);
    else{
      cwal_value_unhand(mV);
      *rv = mV;
    }
  }else if(!rc){
    /* No matches */
    *rv = cwal_value_false();
  }
  return rc;
}

/**
   "Manual" destructor. Disconnects the native from its Value
   part and frees the native parts. After calling this,
   all methods will throw an exception when called, for lack
   of a native 'this'.
*/
static int rx_cb_destroy( cwal_callback_args const * args,
                            cwal_value **rv ){
    THIS_REGEX;
    cwal_native_clear( nself, 1 );
    *rv = cwal_value_undefined();
    return 0;
}

static cwal_value * rx_create_prototype( whcl_engine * const el ){
    int rc = 0;
    cwal_value * proto;
    char const * pKey =
#if RX_IS_POSIX
      "regex-posix"
#elif RX_IS_JS
      "regex-js"
#endif
      ;
    proto = cwal_new_object_value(el->ec);
    if(!proto) return NULL;
    cwal_ref(proto);
    cwal_value_prototype_set(proto, 0);
    rc = whcl_install_typename(el, proto, pKey);
    if(!rc){
      const whcl_func_def funcs[] = {
        WHCL_FUNC2("test", rx_cb_test),
        WHCL_FUNC2("split", rx_cb_split),
        WHCL_FUNC2_XSYM("replace", rx_cb_replace),
        WHCL_FUNC2("match-all", rx_cb_match_all),
        WHCL_FUNC2_XSYM("each-match", rx_cb_eachmatch),
        WHCL_FUNC2("exec", rx_cb_exec),
        WHCL_FUNC2("destroy", rx_cb_destroy),
        whcl_func_def_empty_m
      };
      rc = whcl_install_functions( el, proto, funcs, 0 );
      if(0==rc) rc = whcl_install_command_cb(el, proto);
    }
    if(rc){
      cwal_unref(proto);
      proto = NULL;
    }else{
      cwal_unhand(proto);
    }
    return proto;
}

static int rx_module_init( whcl_engine * const el, cwal_value ** rv ){
    int rc;
    cwal_value * mod;
    cwal_value * proto;
    proto = rx_create_prototype(el);
    if(!proto) return CWAL_RC_OOM;
    cwal_ref(proto);
    mod = cwal_new_function_value(el->ec, rx_cb_compile,
                                  proto, 0, &rx_prototype_id);
    if(!mod){ WHCL__WARN_OOM; cwal_unref(proto); return CWAL_RC_OOM; }
    rc = whcl_stash_hidden_member(mod, proto);
    if(!rc){
      rc = whcl_set_with_flags(el, mod, "instance-prototype", 18,
                               proto, CWAL_VAR_F_CONST);
    }
    cwal_unref(proto);
    if(rc) goto end;
    assert(cwal_value_refcount(proto) && "but it's stashed!");
    { /* Set mod[flavor] to the name of the regex flavor */
      cwal_value * v;
#if RX_IS_POSIX
      v = cwal_new_string_value(el->ec, "posix", 5);
#elif RX_IS_JS
      v = cwal_new_string_value(el->ec, "js", 2);
#endif
      if(v){
        cwal_ref(v);
        rc = cwal_prop_set_with_flags(mod, "flavor", 6, v,
                                      CWAL_VAR_F_CONST);
        cwal_unref(v);
      }else{
        WHCL__WARN_OOM;
        rc = CWAL_RC_OOM;
      }
    }
    end:
    if(rc){
      cwal_unref(mod);
    }else{
      cwal_unhand(mod);
      *rv = mod;
    }
    return rc;
}

#undef RX_FLAVOR_POSIX
#undef RX_FLAVOR_JS
#undef RX_FLAVOR
#undef POSIX_END_INDEX
#undef RX_IS_JS
#undef RX_IS_POSIX
#undef RX_MATCHES_CLEAR
#undef RX_MATCH_T
#undef RX_REGEX_T
#undef THIS_REGEX
#undef RX_POSIX_THIS_HAS_NOSUB
#undef DOLLAR_ERSATZ
#undef DOLLAR_ERSATZ2
