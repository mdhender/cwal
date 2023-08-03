/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
/**
   The core sanity tests for the s2 engine...
*/
#if defined(NDEBUG)
#  undef NDEBUG
#endif
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "libs2.h"
#include "s2_internal.h"

#if 1
#define MARKER(pfexp) if(1) printf("%s:%d():\t",__FILE__,__LINE__); if(1) printf pfexp
#else
#define MARKER(X) (void)0
#endif

static struct {
  char enableValueRecycling;
  char showCwalMemStats;
  int verboseMode;
  int traceAssertions;
} App = {
1/*enableValueRecycling*/,
0/*showCwalMemStats*/,
0/*traceAssertions*/,
0/*verboseMode*/
};

static void dump_s2_err( s2_engine * se ){
  if(se->e->err.code){
    MARKER(("se->e->err: #%d %s: %.*s\n",
            se->e->err.code,
            cwal_rc_cstr(se->e->err.code),
            (int)se->e->err.msg.used,
            (char const *)se->e->err.msg.mem));
  }
}

/**
   Called via cwal_engine_init().
*/
static int shell_init_engine(cwal_engine *e, cwal_engine_vtab * vtab){
  int rc = 0;
  int32_t featureFlags = 1
    ? 0
    : CWAL_FEATURE_ZERO_STRINGS_AT_CLEANUP;
  if(vtab){/*avoid unused param warning*/}

  /* ??? several TODOs here wrt engine configuration... */

  cwal_engine_feature_flags(e, featureFlags);

#define REMAX(T,N) cwal_engine_recycle_max( e, CWAL_TYPE_ ## T, (N) )
  REMAX(UNDEF,0);
  if(App.enableValueRecycling){
    REMAX(ARRAY,10);
    REMAX(BUFFER,2);
    REMAX(DOUBLE,40);
    REMAX(EXCEPTION,2);
    REMAX(FUNCTION,20);
    REMAX(INTEGER,40);
    REMAX(KVP,50);
    REMAX(NATIVE,5);
    REMAX(OBJECT,20);
    REMAX(STRING,50)/*Reminder: strings are not "as recycleable" as
                      other types because we can only recycle strings
                      if their lengths match (approximately) the new
                      string's length. */;
    REMAX(SCOPE,4);
    REMAX(WEAKREF,4);
    REMAX(PROPREF,20);
    REMAX(TUPLE,20);
  }
#undef REMAX

  /* Reminder to self:

     DO NOT allocate any vars/values here, as s2 will (in just a moment)
     pop the top scope and nuke them.
  */
  return rc;
}

/** String-is-internable predicate for cwal_engine. */
static bool cstr_is_internable( void * state,
                                char const * str_,
                                cwal_size_t len ){
  enum { MaxLen = 32U };
  unsigned char const * str = (unsigned char const *)str_;
  assert(str);
  assert(len>0);
  if(len>MaxLen) return 0;
  else if(1==len){
    /*if(*str<32) return 0;
      else
    */
    return (*str>127) ? 0 : 1;
  }
  /* else if(len<4) return 1; */
  else{
    /* Read a UTF8 identifier... */
    char const * zEnd = str_+len;
    char const * tail = str_;
    s2_read_identifier(str_, zEnd, &tail);
    if((tail>str_)
       && (len==(cwal_size_t)(tail-str_))
       ){
      return 1;
    }
    /* MARKER(("Not internable: %s\n", str_)); */
    if(state){/*avoid unused param warning*/}
    return 0;
  }
}

int my_cwal_callback_f(cwal_callback_args const * argv, cwal_value ** rv){
  if(argv){/*avoid unused param warning*/}
  MARKER(("my_callback_f()\n"));
  *rv = cwal_value_true();
  return 0;
}

void test_1(s2_engine * se){
  s2_stoken * t;
  cwal_engine * e = se->e;
  cwal_value * rv = 0;
  S2_UNUSED_VAR int rc = 0;
  /* cwal_scope SC = cwal_scope_empty; */
  /* cwal_scope * scope = &SC; */
  MARKER(("test_1()\n"));
  /* rc = s2_scope_push(se, scope); */
  assert(0==rc);

  t = s2_stoken_alloc2( se, S2_T_MarkVariadicStart, 0 );
  s2_engine_push( se, t );
  
  t = s2_stoken_alloc2( se, S2_T_LiteralIntDec, cwal_new_integer(e, 1) );
  s2_engine_push( se, t );
  
  t = s2_stoken_alloc2( se, S2_T_LiteralString,
                       cwal_new_string_value(e, "2", 1) );
  s2_engine_push( se, t );

  t = s2_stoken_alloc2( se, S2_T_Function,
                       cwal_new_function_value(e, my_cwal_callback_f,
                                               0, 0, 0));
  s2_engine_push( se, t );

  s2_engine_push_ttype( se, S2_T_Foo );

  assert(4 == se->st.vals.size);
  assert(1 == se->st.ops.size);

  rc = s2_process_top( se );
  assert(0==rc);
  assert(1 == se->st.vals.size);
  assert(0 == se->st.ops.size);
  rv = s2_engine_pop_value( se );
  assert(rv == cwal_value_true());
  rv = 0;
  s2_engine_sweep(se);

  t = s2_stoken_alloc2( se, S2_T_MarkVariadicStart, 0 );
  s2_engine_push( se, t );
  s2_engine_push_ttype( se, S2_T_Foo );
  assert(1 == se->st.vals.size);
  assert(1 == se->st.ops.size);
  rc = s2_process_top( se );
  assert(0==rc);
  rv = s2_engine_pop_value( se );
  assert(rv == cwal_value_true());
  rv = 0;
  s2_engine_sweep(se);


  if(1){
    cwal_int_t rhs = 3;
    cwal_int_t lhs = 11;
    s2_engine_push_int(se, lhs);
    s2_engine_push_int(se, rhs);
    s2_engine_push_ttype(se, S2_T_OpPlus);

    rc = s2_process_top( se );
    assert(0==rc);
    assert(1==se->st.vals.size);
    rv = s2_engine_pop_value(se);
    assert(rv);
    assert((lhs+rhs) == cwal_value_get_integer(rv));
    MARKER(("Added: %d + %d == %d\n", (int)lhs, (int)rhs, (int)cwal_value_get_integer(rv)));

    assert(0==se->st.vals.size);

    s2_engine_push_int(se, rhs);
    s2_engine_push_ttype(se, S2_T_OpMinusUnary);
    assert(1==se->st.vals.size);
    assert(1==se->st.ops.size);
    rc = s2_process_top( se);
    assert(0==rc);
    assert(1==se->st.vals.size);
    rv = s2_engine_pop_value(se);
    assert(rv);
    assert((-rhs) == cwal_value_get_integer(rv));
    MARKER(("Unary negated: -(%d) == %d\n", (int)rhs, (int)cwal_value_get_integer(rv)));
    rv = 0;

    s2_engine_sweep(se);

    s2_engine_push_int(se, lhs);
    s2_engine_push_int(se, rhs);
    s2_engine_push_ttype(se, S2_T_OpMultiply);
    assert(1==se->st.ops.size);
    rc = s2_process_top( se );
    assert(0==rc);
    assert(0==se->st.ops.size);
    assert(1==se->st.vals.size);
    rv = s2_engine_pop_value(se);
    assert(rv);
    assert((lhs*rhs) == cwal_value_get_integer(rv));
    MARKER(("Multiplied: %d * %d == %d\n", (int)lhs, (int)rhs, (int)cwal_value_get_integer(rv)));
    rv = 0;
    s2_engine_sweep(se);

    s2_engine_push_int(se, lhs);
    s2_engine_push_int(se, rhs);
    s2_engine_push_ttype(se, S2_T_OpModulo);
    rc = s2_process_top( se );
    assert(0==rc);
    rv = s2_engine_pop_value(se);
    assert((lhs%rhs) == cwal_value_get_integer(rv));
    MARKER(("Modulo: %d %% %d == %d\n", (int)lhs, (int)rhs, (int)cwal_value_get_integer(rv)));
    rv = 0;

    s2_engine_push_int(se, lhs);
    s2_engine_push_int(se, rhs);
    s2_engine_push_ttype(se, S2_T_OpDivide);
    rc = s2_process_top( se );
    assert(0==rc);
    rv = s2_engine_pop_value(se);
    assert(rv);
    assert((lhs/rhs) == cwal_value_get_integer(rv));
    MARKER(("Divide: %d / %d == %d\n", (int)lhs, (int)rhs, (int)cwal_value_get_integer(rv)));
    rv = 0;

    s2_engine_sweep(se);

    s2_engine_push_int(se, lhs);
    s2_engine_push_int(se, 0);
    s2_engine_push_ttype(se, S2_T_OpDivide);
    rc = s2_process_top( se );
    assert(CWAL_SCR_DIV_BY_ZERO==rc);
    MARKER(("Divide by zero!\n"));
    s2_engine_reset_stack(se);

    assert(0==se->st.vals.size);

    s2_engine_push_int(se, lhs);
    s2_engine_push_int(se, 0);
    s2_engine_push_ttype(se, S2_T_OpModulo);
    rc = s2_process_top( se );
    assert(CWAL_SCR_DIV_BY_ZERO==rc);
    MARKER(("Modulo by zero!\n"));
    s2_engine_reset_stack(se);

    s2_engine_sweep(se);

    lhs = 0x100;
    rhs = 0x111;
    s2_engine_push_int(se, lhs);
    s2_engine_push_int(se, rhs);
    s2_process_op_type(se, S2_T_OpXOr);
    rv = s2_engine_pop_value(se);
    assert( (lhs ^ rhs) == cwal_value_get_integer(rv) );
    MARKER(("0x%x ^ 0x%x = 0x%x\n", (unsigned)lhs, (unsigned)rhs,
            (unsigned)cwal_value_get_integer(rv)));

    s2_engine_sweep(se);

    lhs = 0x101;
    rhs = 0x010;
    s2_engine_push_int(se, lhs);
    s2_engine_push_int(se, rhs);
    s2_process_op_type(se, S2_T_OpOrBitwise);
    rv = s2_engine_pop_value(se);
    assert( (lhs | rhs) == cwal_value_get_integer(rv) );
    MARKER(("0x%x | 0x%x = 0x%x\n", (unsigned)lhs, (unsigned)rhs,
            (unsigned)cwal_value_get_integer(rv)));

    s2_engine_sweep(se);

    lhs = 0x1110;
    rhs = 0x1010;
    s2_engine_push_int(se, lhs);
    s2_engine_push_int(se, rhs);
    s2_process_op_type(se, S2_T_OpAndBitwise);
    rv = s2_engine_pop_value(se);
    assert( (lhs & rhs) == cwal_value_get_integer(rv) );
    MARKER(("0x%x & 0x%x = 0x%x\n", (unsigned)lhs, (unsigned)rhs,
            (unsigned)cwal_value_get_integer(rv)));

    s2_engine_sweep(se);

    rhs = 0x1010;
    s2_engine_push_int(se, rhs);
    s2_process_op_type(se, S2_T_OpNegateBitwise);
    rv = s2_engine_pop_value(se);
    assert( ~rhs == cwal_value_get_integer(rv) );
    MARKER(("~(0x%x) = 0x%x\n", (unsigned)rhs,
            (unsigned)cwal_value_get_integer(rv)));

    s2_engine_sweep(se);

    rhs = 32;
    s2_engine_push_int(se, rhs);
    s2_process_op_type(se, S2_T_OpNot);
    rv = s2_engine_pop_value(se);
    assert(rv);
    assert(cwal_value_false()==rv);
    MARKER(("!(%d) = %d\n", (int)rhs, (int)cwal_value_get_integer(rv)));

  }

  /* assert(scope == cwal_scope_current_get(e)); */
  /* s2_scope_pop(e); */
  s2_engine_sweep(se);
}



typedef struct {
  int errCode;
  cwal_value * expect;
  char const * src;
} TestScript;
static const TestScript TestScript_empty = {0,0,0};

static struct {

  int runCount;
  int totalScriptStringLength;
} RunnerMetrics = {
0, 0
};

/**
   To work around a descrepancy-bug in results between skip- and
   non-skip mode regarding 0 vs cwal_value_undefined().
 */
#define SCRIPT_CHECKER_EVAL_IT 1

void script_checker( s2_engine * se, TestScript const * ts ){
  int rc;
  cwal_value * rv = 0;
  S2_UNUSED_VAR cwal_scope * callingScope = cwal_scope_current_get(se->e);
  cwal_scope scope_ = cwal_scope_empty;
  char const evalIt = SCRIPT_CHECKER_EVAL_IT
    /* Set to 0 to run all evaluation in "skip mode." Memory costs
       should drop, but the parsing process still works its way through
       the script in the same manner.
    */
    ;
  char const pushScope = 1;
  cwal_scope * scope = pushScope ? &scope_ : 0;
  assert(callingScope && (callingScope->level>0));
  s2_engine_err_reset(se);
  cwal_exception_set(se->e, 0);
  ++RunnerMetrics.runCount;
  RunnerMetrics.totalScriptStringLength += (int) cwal_strlen(ts->src);
  if(ts->errCode){
    MARKER(("Expecting error code %s for: %s\n",
            cwal_rc_cstr(ts->errCode), ts->src));
  }else{
    MARKER(("Running script: %s\n", ts->src));
    if(App.verboseMode>1){
      s2_dump_val( evalIt ? ts->expect : 0, "expecting result" );
    }
  }

  if(scope){
    rc = cwal_scope_push2(se->e, scope);
    if(rc){
      s2_fatal(rc, "s2_scope_push() failed!");
    }
  }
  if(!evalIt) ++se->skipLevel;
  rc = s2_eval_cstr(se, 0, "test script",
                    ts->src, -1,
                    /* &rv  */ evalIt ? &rv : 0);
  if(!evalIt) --se->skipLevel;
  if(rc){
    char const * msg = 0;
    s2_engine_err_get( se, &msg, 0);
    if(rc != ts->errCode){
      if(CWAL_RC_EXCEPTION == rc){
        s2_dump_val( cwal_exception_get(se->e),
                     "exception");
        s2_fatal(CWAL_RC_ASSERT,
                 "Script threw unexpected exception.");
      }else{
        assert(msg);
        s2_fatal(CWAL_RC_ASSERT,
                 "Script unexpectedly failed: #%d %s: %s\n",
                 rc, cwal_rc_cstr(rc), msg);
      }
    }else if(App.verboseMode){
      if(CWAL_RC_EXCEPTION == rc){
        s2_dump_val( cwal_exception_get(se->e),
                     "Script threw expected exception");
      }else{
        assert(msg);
        MARKER(("Script failed as expected: #%d %s: %s\n",
                rc, cwal_rc_cstr(rc), msg));
      }
    }
  }else if(ts->errCode){
    s2_dump_val( rv, "unexpected success result" );
    s2_fatal(CWAL_RC_ASSERT,
             "Script unexpectedly succeeded. "
             "Expected rc %s\n",
             cwal_rc_cstr(ts->errCode));
  }else{
    if(App.verboseMode){
      s2_dump_val( rv, "result          " );
    }
    if(evalIt){
      rc = cwal_value_compare(ts->expect, rv);
      if(rc){
        s2_dump_val( ts->expect, "DOH! Was expecting" );
        s2_dump_val( rv, "DOH! but got" );
        s2_fatal( CWAL_RC_ASSERT,
                  "Result does not match expectations.");
      }
    }else if(ts->expect){
      if(App.verboseMode){
        MARKER(("Result not evaluated due to skip mode.\n"));
      }
      /* assert(cwal_value_undefined()==rv); */
    }else if(rv){
      s2_dump_val( rv, "unexpected result" );
      s2_fatal( CWAL_RC_ASSERT,
                "Expected non-NULL Result.");
    }
    /* MARKER(("Script passed\n")); */
  }
  s2_engine_err_reset(se);
  if(scope){
    assert(s2_scope_current(se)->cwalScope == scope);
    cwal_scope_pop(se->e);
  }
}


void check_script(s2_engine * se,
                int expectErrcode,
                cwal_value * expectResult,
                char const * src){
  TestScript sc = TestScript_empty;
  sc.errCode=expectErrcode;
  sc.expect = expectResult;
  /* assert(expectResult); */
  cwal_value_ref(sc.expect);
  sc.src = src;
  script_checker(se, &sc);
  cwal_value_unhand(sc.expect);
}

void check_script_xerr(s2_engine * se, int expectResult,
                    char const * src){
  check_script( se, expectResult, 0, src );
}

void check_script_xval(s2_engine * se, cwal_value * expectResult,
                    char const * src){
  check_script( se, 0, expectResult, src );
}


#define CVSTR(STR) cwal_new_string_value(se->e, (STR), cwal_strlen((STR)))
#define CVINT(INT) cwal_new_integer(se->e, (INT))
#define CVDBL(DBL) cwal_new_double(se->e, (DBL))
#define CVTRUE cwal_value_true()
#define CVFALSE cwal_value_false()
#define CVUNDEF cwal_value_undefined()
#define CVNULL cwal_value_null()

enum {
eSyn = CWAL_SCR_SYNTAX,
eExc = CWAL_RC_EXCEPTION
};
#define SWEEP s2_engine_sweep
#define RUN(EXPECT_VAL, SRC) check_script_xval(se, (EXPECT_VAL), (SRC)); SWEEP(se)
#define ERR(EXPECT_CODE, SRC) check_script_xerr(se, (EXPECT_CODE), (SRC)); SWEEP(se)
#define ERRSYN(SRC) check_script_xerr(se, CWAL_SCR_SYNTAX, (SRC)); SWEEP(se)
#define RUNTR(EXPECT_VAL, SRC) \
  ++se->flags.traceTokenStack; check_script_xval(se, (EXPECT_VAL), (SRC));   \
  --se->flags.traceTokenStack; SWEEP(se)
#define ERRTR(EXPECT_CODE, SRC) \
  ++se->flags.traceTokenStack; check_script_xerr(se, (EXPECT_CODE), (SRC));   \
  --se->flags.traceTokenStack; SWEEP(se)
void test_n(s2_engine *se){

  goto start;

  start: /* move this to skip certain tests! Or add a (goto end) somewhere */

  /* goto end; */


  ERR( eExc, "\n   (1) (2) /* consecutive values w/o an operator */");
  ERR( eSyn, "(/*empty parens expr. currently not allowed*/)" );
  ERR( eSyn, " ;;; ( ; ) /* currently the same thing */" );
  ERR( eSyn, "  \n\n(1+3-) ; /* error line should==3, col==4*/" );
  ERR( eSyn, "3 + + // invalid op at EOF");
  ERR( eSyn, "* *  // invalid prefix op");
  ERR( eSyn, "1, 2, }, 3 // mismatched '}'");
  ERR( eSyn, "((1)); (( ) // mismatched '('");
  ERR( eSyn, "([(1)) // mismatched '[';");
  ERR( eSyn, "((print[1)) // error is caught in the [...] subparse;");

  RUN( 0, ";");
  RUN( 0, "");

  RUN( CVFALSE, "1 && 0");
  RUN( CVFALSE, "0 && 1 /* short-circuiting */");
  RUN( CVTRUE, "0 || 7");
  RUN( CVINT(7), "0 ||| 7");
  RUN( CVTRUE, "3 || 7 /* short-circuiting */");
  RUN( CVINT(3), "3 ||| 7");
  RUN( CVINT(3), "3 ||| 0");
  RUN( CVTRUE, "1 && 2");

  /* just to see some breakage:

     ERR( CWAL_RC_FATAL, "3;" );
     RUN( CVTRUE, "// add some line/col info here\n   3 2" );
  */

  RUN( CVINT(0x1010), "0x1000 | 0x0010");
  RUN( CVINT(0x0110), "0 ||| 0x0100 ^ 0x0010");

  RUN( CVINT(3), "3" );
  RUN( CVINT(3), "3; /* === 3 b/c only one EOX marker */" );
  RUN( 0, "3; ; /* === undefined b/c of multiple EOX markers */" );
  RUN( 0, "3;\n\n\n; // newlines don't count as EOX for this purpose" );
  RUN( 0, "1; /* === undefined b/c of multiple EOX markers */;" );
  RUN( CVINT(3), "3;\n\n\n // newlines don't count as EOX for this purpose" );

  RUN( CVINT(8), "3 + 4 - 8 + 9");
  RUN( CVINT(14), "1 + 2 * 4 + 5" );

  RUN( CVINT(1), "(1)" );
  RUN( CVINT(4), "(1+3)" );

  RUN( CVSTR("arguable"), "(1+3; 'arguable') /* still uncertain about this */" );

  RUN( CVINT(14), "1 + (2 * 4) + 5" );
  RUN( CVINT(14), "(((1) + (2 * 4)) + (5))" );
  RUN( CVINT(-7), "1 + +2 * -4" );
  RUN( CVDBL(-2.3), "1.2 + 0x2 - 5.5" );
  RUN( CVINT(42), "0o3 + (2 * (7 + 3)) * 2 - 1" );
  RUN( CVINT(21), "1 + (0x3 + 0o2) * 4" );
  RUN( CVINT(14), "(4*2 + 2 * 3)" );
  RUN( CVINT(35), "7 * (1, 3, 5)" );
  RUN( CVINT(47), "7 * (1, 3, 5) + ((1, 3, 5) + 7)" );
  RUN( CVINT(24), "(1*2, (3*4, (7+1))) * 3" );
  RUN( CVINT(4), "(((2 * 5) - (1 * 2)) / (11 - 9)) " );
  RUN( CVINT(-7), "1 + +((2)) * -4" );
  RUN( CVINT(1), "1 + !4");
  RUN( CVINT(5), "1 + 3, 5");
  RUN( CVINT(5), "1 * 3, 5");
  RUN( CVINT(6), "5 / 2, 2 * 3");
  RUN( CVINT(35), "3 + 7 * 2 * 3 - 5 * 2");
  RUN( CVINT(-24), "-(1*2, (3*4, (7+1))) * 3");
  RUN( CVINT(12), "(1, 3, 5) + 7.0");
  RUN( CVINT(6), "((3,2,1) + (1,3,2) + (1,2,3))");
  RUN( CVDBL(7.2), "(1, 3, 5.2) + 2");
  RUN( CVINT(5), "1 + !4, 5");
  RUN( CVINT(7), "5 + !0 * 2");
  RUN( CVINT(3), "5 + !0 * 2, 3");
  RUN( CVINT(-3), "5 + !0 * 2, -3");
  RUN( CVINT(-7), "5 + (!0 * 2), -7");
  RUN( CVINT(14), "1;2 + 5 * 7; 3 + 11");
  RUN( CVINT(-15), "2 + 5 * 7;3 + 11, -(5*3)");
  RUN( CVINT(99), "- - 99 /* why not? */" );
  RUN( CVINT(-99), "- - + -99 /* why not? */" );

  RUN( CVSTR("bye"), "1 + 2, (3+7 * 21), 92;  'hi!', (33 * 3 + 7), 'bye'");
#if 0
  RUN( CVSTR("bye"), "'by'+'e'");
  RUN( CVSTR("bye"), "((('b')+'ye'));");
#endif

  RUN( CVINT(100), "18+2, (3+7+1);\n21 / 7, (31 * 3 + 7)  ;");
  RUN( CVINT(19), "3\n-9 * 3, 17 ; +2, 19");
  RUN( CVINT(-4), "   -((1,2*7,3), 4)");
  RUN( CVINT(2),
       "1 , (31 * 3 + 7)  ; 3 ; /*(1,'comment')*/ ; 8 % 3\n;");
  RUN( CVSTR(";"), " '  string  '; '  \tanother string\n';"
       "';'/*<== that's the string we want, not this one ==>*/ ;");
  RUN( CVSTR("a heredoc!"), "<<<X a heredoc!   X");
  RUN( CVSTR("another heredoc"), "<<<X another heredocX "
       "// note that the ending identifier need not be "
       "preceeded by whitespace!");
  RUN( CVSTR("hi hi hi"),
       "<<<'hi' hi hi hi 'hi'"
       "// quotes can be used around a HEREDOC tag, and any other "
       "chars may appear in those quotes.");
  RUN( CVSTR("hiho hiho hiho"),
       "<<<\"hiho\" hiho hiho hiho \"hiho\"// double-quotes can also be "
       "used around a heredoc tag");
  ERR( eSyn,
       "<<<'hi' but quotes must match \"hi\"");
  
  RUN( CVSTR("seemingly mismatched )( parens"),
       "(1, <<<'()' make sure slurp_braces handles this] '()', "
       "<<<XX seemingly mismatched )( parens   XX)");

  ERR( eSyn, "1 - ;" );

  RUN( CVTRUE, "1 < 3");
  RUN( CVFALSE, "1 > 3");
  RUN( CVTRUE, "3 > 1");
  RUN( CVFALSE, "3 < 1");
  RUN( CVTRUE, "3 <= 4");
  RUN( CVTRUE, "4 >= 4");
  RUN( CVFALSE, "3 >= 4");
  RUN( CVFALSE, "1 == 3");
  RUN( CVTRUE, "1 == 1");
  RUN( CVTRUE, "1 == 1.0");
  RUN( CVFALSE, "1 === 1.0");
  RUN( CVTRUE, "1 !== 1.0");
  RUN( CVFALSE, "1 !== 1");

  RUN( CVTRUE, "3 === 1 * 2 + 1");
  RUN( CVTRUE, "5 === 1 * 2 * 1 + 3");
  RUN( CVTRUE, "2 + 24 === 1 * 2 * 1 + 3 * 8");
  RUN( CVTRUE, "26.0 !== 1 * 2 * 1 + 3 * 8");
  RUN( CVFALSE,"26.0 === 1 * 2 * 1 + 3 * 8");
  RUN( CVTRUE, "26 === 1 * 2.0 * 1 + 3 * 8");
  RUN( CVTRUE, "2*6+4*2 <= 1 * 2 * 1 + 3 * 8");

  RUN(CVINT(3), "-((-2)) + 1");

  RUN(CVINT(7), "1 +\n6");
  RUN(CVINT(7),  "1\n+\n6");
  RUN(CVINT(6), "1;\n+ 6");

#if 0
  /* Addition of identifier resolution broke these: */
  RUN( CVSTR("finished"),
       "3; ; 3 + '+14' ; 'almost'; 'done'; ''; finished; ");
  RUN( CVSTR("identifier"), ";;;identifier;" );
  RUN( CVINT(8), "identifier+3+'5' "
       "/* identifier expansion is pending. Until then they convert "
       "as strings (0 in this case) */");
#endif
  goto end /* just to have referenced it at least once */;
  end:
  /* Need an expr after goto, apparently :/. */
  ++se->flags.traceTokenStack;
  --se->flags.traceTokenStack;
}


static void test_2(s2_engine * se){
  cwal_value * v, *check, *varA, *vPrint,
    *undef = cwal_value_undefined();
  S2_UNUSED_VAR int rc;
  MARKER(("test_2()...\n"));

  v = s2_prototype_object(se);
  assert(v);
  MARKER(("v type name: %s\n", cwal_value_type_name(v)));
  if(!v){/*avoid unused var in non-debug build*/}
  vPrint = v = s2_var_get( se, -1, "print", 5 );
  assert(vPrint);
  assert(cwal_value_is_function(vPrint));

  s2_dump_val(v, "found 'print' function");

  cwal_prop_set(v, "self", 4, v);

  check = 0;
  /* s2_engine_err_reset(se); */
  rc = s2_get(se, v, "magic", 5, &check);
  MARKER(("magic get=%s\n", cwal_rc_cstr(rc)));
  assert(0==rc);
  varA = check;

  s2_var_decl( se, "a", 1, varA, 0 );

  RUN( varA, "print.magic /* gets set up at s2 init */" );
  RUN( undef, "print.nope /* undefined member */" );
  RUN( undef, "print.'nope' /* undefined member */" );
  ERR( CWAL_RC_EXCEPTION, "print.nope.veryNope /* attempt to dot-deref undefined */" );

#if SCRIPT_CHECKER_EVAL_IT
  ERR( CWAL_RC_EXCEPTION, "1 + x" );
  ERR( CWAL_RC_EXCEPTION, "print.(magic)" );
#else
  RUN( undef, "1 + x" );
  RUN( undef, "print.(magic)" );
#endif
  RUN( varA, "a" );
  RUN( CVINT(21), "a/2" );

  RUN( varA, "print[((('magic')))]" );
  RUN( CVINT(84), "a + print['it\\'s a kind of','magic']" );

  RUN(CVINT(1806), "print.'magic' + print['magic'] * print.magic");

  RUN(undef, "print['self'].none");
  RUN(varA, "print['self'].self.'self'.magic");
  RUN(vPrint, "print['self']");
  RUN(vPrint, "print['self'].self");
  RUN(vPrint, "print.self.self.self");
  RUN(vPrint, "print.self.self['self']");
  RUN(varA, "print.self.self[<<<X self X].self.magic");
  RUN(vPrint, "print['self']['self'][<<<X self X].self");
  RUN(vPrint, "print['self'].self[<<<X self X]['self']");
  RUN(CVTRUE, "assert 42 === print['self'].self[<<<X self X]['self'].magic");
  RUN(varA, "print['self'].self[<<<X self X]['self'].magic");
  RUN(vPrint, "((print['self']))[<<<X self X]['self']");
  RUN(varA, "((print)['self']).self['self'].magic");
  v = cwal_new_integer(se->e, -cwal_value_get_integer(varA));
  cwal_value_ref(v);
  RUN(v, "-print.magic");
  RUN(v, "-print['magic']");
  cwal_value_unref(v);
  v = 0;

  RUN(CVINT(1), "eval 1");
  RUN(CVTRUE, "eval { 1 && 3 && 7 }");
  RUN(CVINT(2), "eval 4 - '2'");
  RUN(CVINT(3), "eval 4 - '2', 3");
  RUN(CVINT(7), "eval 4 - '2' + scope {\n\t10/2*\n\t3-10\n}");
  /* RUN(undef, "scope{1+0+0.0-1.0}"); */
  ERR(eSyn, "eval");
  ERR(eSyn, "eval 7; scope; // eval/scope require an expr or {...}.");


  RUN(CVUNDEF, "undefined");
  RUN(CVTRUE, "true");
  RUN(CVFALSE, "false");
  RUN(CVNULL, "null");
  RUN(CVINT(1), "null + undefined + true");
  RUN(CVTRUE, "undefined === undefined");
  RUN(CVTRUE, "null == undefined");
  RUN(CVTRUE, "null !== undefined");
  RUN(CVFALSE, "null != undefined");
  RUN(CVTRUE, "true || false");
  RUN(CVFALSE, "true && false");
  RUN(CVTRUE, "false || 0 || 0.0 || true");
  RUN(CVFALSE, "false && 1 && 2 && undefined");
  RUN(CVNULL, "undefined ||| null");
  RUN(CVFALSE, "undefined || null");
  RUN(CVNULL, "eval null");
  RUN(CVUNDEF, "scope{}");
  ERR(eSyn, "0 || scope{ {}");
  RUN(CVINT(7), "scope{4+3} ||| 0 ||| 3 ||| 8 /* make sure '7' survives sweeping */");

  RUN(CVINT(-1), "false && true ||| -1");
  RUN(CVTRUE, "true && true && true ||| 0");
  RUN(CVINT(-1), "0 && true && true ||| -1");
  RUN(CVINT(-1), "false && true ||| -1");
  RUN(CVTRUE, "false && -1 || 7");
  RUN(CVINT(2), "2 ||| scope{xyz} ||| 3 // scope gets eval'd: short-circuting eval is incomplete");
  RUN(CVTRUE, "2 ||| scope{xyz} || 3 // bool b/c of final || op.");
  RUN(CVTRUE, "2 || 3");
  RUN(CVTRUE, "2 || scope{error} /* note that 'error' symbol is skipped here */");
  RUN(CVTRUE, "2 || scope{error} || error + another_error /* and skipped here */");
  ERR(eSyn, "1 || scope{ {} /* syntax errors are not short-circuited */");
  ERR(eSyn, "0 || scope{ {}");

  RUN(CVTRUE, "0 || 1 * 2 + 3");
  RUN(CVINT(5), "0 ||| 1 * 2 + 3");
  RUN(CVINT(3), "0 ||| 1 * 2, 3");
  RUN(CVINT(42), "1 || 3, 42");
  ERR(CWAL_RC_EXCEPTION, "0 || scope{noSuchIdentifier} ||| 71");
  RUN(CVTRUE, "2 || scope{error} ||| 71");
  RUN(CVINT(2), "2 ||| scope{error} ||| 71");

  RUN(CVFALSE, "0 && error && another + error * 3");
  RUN(CVINT(3) , "0 || 0 && error ||| 3");

  /* 20190706: test fix of boolean lookup key bug... */
  RUN(CVUNDEF, "{}[true]" /* i have NO CLUE where these x[true] hits are coming from! */);
  RUN(CVUNDEF, "[][true]");
  RUN(CVUNDEF, "''[true]");
  RUN(CVUNDEF, "0.0[true]");
  RUN(CVUNDEF, "0[true]");
  RUN(CVUNDEF, "print[true]");
  RUN(CVFALSE, "var o={prototype:null}; o[true]=1; !!o['true']");
  RUN(CVUNDEF,"{}[false]");
  RUN(CVUNDEF,"[][false]");
  RUN(CVTRUE, "var o={prototype:null}; o[false]=1; undefined===o[0]");
  RUN(CVTRUE, "var o={prototype:null, 0:1}; undefined===o[false]");
  
  /* RUN(0, "''.prototype"); */
  goto end;

#if 1
  /* put broken tests here */
#endif

  goto end;
  end:

  MARKER(("test_2() done\n"));
}

void test_3(s2_engine *se){
  cwal_value * myV = cwal_new_integer(se->e, 42);
  cwal_value_ref(myV);
  RUN(CVINT(7), "1 ? 2 + 5 : 3");
  RUN(CVINT(46), "0 ? error : false ? another + error : 13 * 3 + 7");
  RUN(CVINT(6), "0 ? error : true ? 13 * 3 + 7 : another + error, 6");
  RUN(CVINT(6), "0 ? error : false ? another + error : 13 * 3 + 7, 6");
  RUN(CVTRUE, "1 - 2 + 1 ? false : true");

  RUN(myV, "print.magic /* gets set up at s2 init */");
  RUN(myV, "true ? print[((('magic')))] : {ignore all this}");
  ERR(eSyn, "true ? print.'magic' : 3x/*malformed number*/");
  ERR(CWAL_RC_EXCEPTION, "false ? print.'magic' : blah // unknown identifier");
  RUN(myV, "true ? print.'magic' : blah // unknown identifier");

  ERRSYN("true ? print.magic : // missing RHS expr");
  ERRSYN("true ? print.magic : ;// missing RHS of ':'");
  ERRSYN("true ?  : 1 ;// missing LHS of ':'");

  RUN(myV, "42 ?: 1");
  RUN(myV, "undefined ?: 42");
  ERRSYN("true ?: // missing RHS of ?:");
  ERRSYN("?: true // missing LHS of ?:");

#define A1(SRC) RUN(CVTRUE,"assert " SRC)
#define A0(SRC) ERR(CWAL_RC_ASSERT, "assert " SRC)

  A1("true === true");
  A1("true == 1\n");
  A1("true != 0");
  A0("false");
  A0("false == 1");
  A1("false != '0'");
  A1("false == +'0'");
  A1("false == ''");
  A0("!!''");
  RUN(CVINT(42),"true ? 1 ? 4 + 38 : -1 : 0");
  A0("42 === true ? 1 ? 4 + 38 : -1 : 0 /* === has higher prec than '?' */;");
  A1("42 === (true ? 1 ? 4 + 38 : -1 : 0)");
  A1("1<3 && 2<print.magic");
  A0("null < undefined");
  A0("null > undefined");
  A0("null > undefined\n\n/**/");
  ++se->flags.traceTokenStack;
  --se->flags.traceTokenStack;

#undef A0
#undef A1

  cwal_value_unref(myV);
}

#undef ERR
#undef RUN
#undef RUNTR
#undef CVTRUE
#undef CVFALSE
#undef CVINT
#undef CVSTR
#undef CVDBL
#undef SWEEP


int shell_main(int argc, char const * const * argv){
  enum {
  UseStackEngine = 1 /* cwal_engine */,
  UseStackInterp = 1 /* s2_engine */
  }; 
  typedef struct {
    char enabled;
      void (*func)(s2_engine *);
  } Runner;
  cwal_engine E = cwal_engine_empty;
  cwal_engine * e = UseStackEngine ? &E : 0;
  cwal_engine_vtab vtab = cwal_engine_vtab_basic;
  s2_engine SE = s2_engine_empty;
  s2_engine * se = UseStackInterp ? &SE : 0;
  int rc;
  if(argc || argv){/*avoid unused param warning*/}
  vtab.tracer = cwal_engine_tracer_FILE;
  vtab.tracer.state = stdout;
  vtab.hook.on_init = shell_init_engine;
  vtab.interning.is_internable = cstr_is_internable;
  vtab.outputer.state.data = stdout;
  vtab.outputer.state.finalize = cwal_finalizer_f_fclose;
  rc = cwal_engine_init( &e, &vtab );
  if(rc){
    cwal_engine_destroy(e);
    goto end;
  }

  se = UseStackInterp ? &SE : s2_engine_alloc(e);
  assert(se);
  assert(UseStackInterp ? !se->allocStamp : (se->allocStamp == e));

  rc = s2_engine_init( se, e );
#if 0
  if(!rc) rc = s2_install_core_prototypes(se);
#endif
  if(rc) goto end;


  {
    cwal_value * v;
#define VCHECK if(!v){rc = CWAL_RC_OOM; goto end;} (void)0
#define RC if(rc) goto end; (void)0
    v = cwal_new_function_value( e, s2_cb_print, 0, 0, 0 );
    VCHECK;
    
    rc = cwal_var_decl(e, 0, "print", 5, v, 0);
    RC;
    { /* just for some property access test code */
      cwal_value * magic = cwal_new_integer(e, 42);
      cwal_prop_set( v, "magic", 5, magic );
    }
#undef VCHECK
#undef RC
  }


  se->scopes.current->sguard.vacuum = 1
    /* required to avoid vacuuming up our non-script-visible values */;
  se->flags.traceAssertions = App.traceAssertions;
  {
    Runner const * test;
    const Runner tests[] = {
    {1, test_1},
    {1, test_n},
    {1, test_2},
    {1, test_3},
    {0, 0}
    };
    for( test = tests; test->func; ++test ){
      if(test->enabled) test->func(se);
    }
  }

  end:
  if(App.showCwalMemStats){
    puts("");
    MARKER(("cwal-level allocation metrics:\n"));
    cwal_dump_allocation_metrics(se->e);
  }
  if(se->metrics.tokenAllocs){
    puts("");
    MARKER(("s2-side metrics:\n"));
    MARKER(("Total script_checker() runs: %d across %d bytes "
            "of script code.\n",
            RunnerMetrics.runCount,
            RunnerMetrics.totalScriptStringLength));
    MARKER(("Peak sub-expression parse depth: %d\n",
            se->metrics.peakSubexpDepth));
    MARKER(("Total s2_stokens (sizeof=%u) requested=%u, "
            "allocated=%u (=%u bytes), alive=%u, peakAlive=%u, "
            "recyclerSize=%d\n",
            (unsigned)sizeof(s2_stoken), se->metrics.tokenRequests,
            se->metrics.tokenAllocs, (unsigned)(se->metrics.tokenAllocs * sizeof(s2_stoken)),
            se->metrics.liveTokenCount, se->metrics.peakLiveTokenCount,
            se->recycler.stok.size));
    if(se->metrics.tokenAllocs < se->metrics.tokenRequests){
      MARKER(("Saved %u bytes and %u allocs via stack token reycling :D.\n",
              (unsigned)((se->metrics.tokenRequests * sizeof(s2_stoken))
                         - (se->metrics.tokenAllocs * sizeof(s2_stoken))),
              se->metrics.tokenRequests - se->metrics.tokenAllocs
              ));
    }
  }
  assert(!se->st.vals.size);
  assert(!se->st.ops.size);
  dump_s2_err(se);
  s2_engine_finalize(se);
  return rc;
}

static void show_sizeofs(){
#define SO(T) MARKER(("sizeof("#T")=%d\n", (int)sizeof(T)))
  SO(s2_stoken);
  SO(s2_ptoken);
  SO(s2_ptoker);
  SO(((s2_ptoker*)NULL)->_lcCache);
  SO(s2_func_state);
  /*SO(s2_op);
    SO(s2_engine);
    SO(cwal_engine);
    SO(cwal_double_t);
    SO(cwal_size_t);
    SO(cwal_int_t);*/
#undef SO
}  

int main(int argc, char const * const * argv)
{
  int rc = 0;
  int i;
  char const * arg;
  int gotNoop = 0;
#define ARG(X) else if(0==strcmp(X, arg))
  for( i = 1; i < argc; ++i ){
    arg = argv[i];
    if(0){}
    ARG("-mem"){
      App.showCwalMemStats = 1;
    }ARG("-v"){
      ++App.verboseMode;
    }ARG("--r"){
        App.enableValueRecycling = 0;
    }ARG("-A"){
      ++App.traceAssertions;
    }ARG("-z"){
      ++gotNoop;
      show_sizeofs();
    }
    else{
      MARKER(("Unknown argument: %s\n", arg));
      goto misuse;
    }
  }

  if(!gotNoop){
    rc = shell_main(argc, argv);
  }
  goto end;
  misuse:
  rc = CWAL_RC_MISUSE;
  end:
  MARKER(("Done! rc=%d (%s)\n",
          rc, cwal_rc_cstr(rc)));
  return rc ? EXIT_FAILURE : EXIT_SUCCESS;
}
