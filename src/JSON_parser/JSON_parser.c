/*
Copyright (c) 2007-2013 Jean Gressmann (jean@0x42.de)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

/*
    Changelog:
        2013-09-08
            Updated license to to be compatible with Debian license requirements.

        2012-06-06
            Fix for invalid UTF16 characters and some comment fixex (thomas.h.moog@intel.com).

        2010-11-25
            Support for custom memory allocation (sgbeal@googlemail.com).

        2010-05-07
            Added error handling for memory allocation failure (sgbeal@googlemail.com).
            Added diagnosis errors for invalid JSON.

        2010-03-25
            Fixed buffer overrun in grow_parse_buffer & cleaned up code.

        2009-10-19
            Replaced long double in JSON_value_struct with double after reports
            of strtold being broken on some platforms (charles@transmissionbt.com).

        2009-05-17
            Incorporated benrudiak@googlemail.com fix for UTF16 decoding.

        2009-05-14
            Fixed float parsing bug related to a locale being set that didn't
            use '.' as decimal point character (charles@transmissionbt.com).

        2008-10-14
            Renamed states.IN to states.IT to avoid name clash which IN macro
            defined in windef.h (alexey.pelykh@gmail.com)

        2008-07-19
            Removed some duplicate code & debugging variable (charles@transmissionbt.com)

        2008-05-28
            Made JSON_value structure ansi C compliant. This bug was report by
            trisk@acm.jhu.edu

        2008-05-20
            Fixed bug reported by charles@transmissionbt.com where the switching
            from static to dynamic parse buffer did not copy the static parse
            buffer's content.
*/



#include <assert.h>
#include <ctype.h>
#include <float.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <locale.h>

#include "JSON_parser.h"

#ifdef _MSC_VER
#   if _MSC_VER >= 1400 /* Visual Studio 2005 and up */
#      pragma warning(disable:4996) /* unsecure sscanf */
#      pragma warning(disable:4127) /* conditional expression is constant */
#   endif
#endif


#define true  1
#define false 0
#define XX   -1     /* the universal error code */

/* values chosen so that the object size is approx equal to one page (4K) */
#ifndef JSON_PARSER_STACK_SIZE
#   define JSON_PARSER_STACK_SIZE 128
#endif

#ifndef JSON_PARSER_PARSE_BUFFER_SIZE
#   define JSON_PARSER_PARSE_BUFFER_SIZE 3500
#endif

typedef void* (*JSON_debug_malloc_t)(size_t bytes, const char* reason);

#ifdef JSON_PARSER_DEBUG_MALLOC
#   define JSON_parser_malloc(func, bytes, reason) ((JSON_debug_malloc_t)func)(bytes, reason)
#else
#   define JSON_parser_malloc(func, bytes, reason) func(bytes)
#endif

typedef unsigned short UTF16;

struct JSON_parser_struct {
    JSON_parser_callback callback;
    void* ctx;
    signed char state, before_comment_state, type, escaped, comment, allow_comments, handle_floats_manually, error;
    char decimal_point;
    UTF16 utf16_high_surrogate;
    int current_char;
    int depth;
    int top;
    int stack_capacity;
    signed char* stack;
    char* parse_buffer;
    size_t parse_buffer_capacity;
    size_t parse_buffer_count;
    signed char static_stack[JSON_PARSER_STACK_SIZE];
    char static_parse_buffer[JSON_PARSER_PARSE_BUFFER_SIZE];
    JSON_malloc_t malloc;
    JSON_free_t free;
};

#define COUNTOF(x) (sizeof(x)/sizeof(x[0]))

/*
    Characters are mapped into these character classes. This allows for
    a significant reduction in the size of the state transition table.
*/



enum classes {
    C_SPACE,  /* space */
    C_WHITE,  /* other whitespace */
    C_LCURB,  /* {  */
    C_RCURB,  /* } */
    C_LSQRB,  /* [ */
    C_RSQRB,  /* ] */
    C_COLON,  /* : */
    C_COMMA,  /* , */
    C_QUOTE,  /* " */
    C_BACKS,  /* \ */
    C_SLASH,  /* / */
    C_PLUS,   /* + */
    C_MINUS,  /* - */
    C_POINT,  /* . */
    C_ZERO ,  /* 0 */
    C_DIGIT,  /* 123456789 */
    C_LOW_A,  /* a */
    C_LOW_B,  /* b */
    C_LOW_C,  /* c */
    C_LOW_D,  /* d */
    C_LOW_E,  /* e */
    C_LOW_F,  /* f */
    C_LOW_L,  /* l */
    C_LOW_N,  /* n */
    C_LOW_R,  /* r */
    C_LOW_S,  /* s */
    C_LOW_T,  /* t */
    C_LOW_U,  /* u */
    C_ABCDF,  /* ABCDF */
    C_E,      /* E */
    C_ETC,    /* everything else */
    C_STAR,   /* * */
    NR_CLASSES
};

static const signed char ascii_class[128] = {
/*
    This array maps the 128 ASCII characters into character classes.
    The remaining Unicode characters should be mapped to C_ETC.
    Non-whitespace control characters are errors.
*/
    XX,      XX,      XX,      XX,      XX,      XX,      XX,      XX,
    XX,      C_WHITE, C_WHITE, XX,      XX,      C_WHITE, XX,      XX,
    XX,      XX,      XX,      XX,      XX,      XX,      XX,      XX,
    XX,      XX,      XX,      XX,      XX,      XX,      XX,      XX,

    C_SPACE, C_ETC,   C_QUOTE, C_ETC,   C_ETC,   C_ETC,   C_ETC,   C_ETC,
    C_ETC,   C_ETC,   C_STAR,   C_PLUS, C_COMMA, C_MINUS, C_POINT, C_SLASH,
    C_ZERO,  C_DIGIT, C_DIGIT, C_DIGIT, C_DIGIT, C_DIGIT, C_DIGIT, C_DIGIT,
    C_DIGIT, C_DIGIT, C_COLON, C_ETC,   C_ETC,   C_ETC,   C_ETC,   C_ETC,

    C_ETC,   C_ABCDF, C_ABCDF, C_ABCDF, C_ABCDF, C_E,     C_ABCDF, C_ETC,
    C_ETC,   C_ETC,   C_ETC,   C_ETC,   C_ETC,   C_ETC,   C_ETC,   C_ETC,
    C_ETC,   C_ETC,   C_ETC,   C_ETC,   C_ETC,   C_ETC,   C_ETC,   C_ETC,
    C_ETC,   C_ETC,   C_ETC,   C_LSQRB, C_BACKS, C_RSQRB, C_ETC,   C_ETC,

    C_ETC,   C_LOW_A, C_LOW_B, C_LOW_C, C_LOW_D, C_LOW_E, C_LOW_F, C_ETC,
    C_ETC,   C_ETC,   C_ETC,   C_ETC,   C_LOW_L, C_ETC,   C_LOW_N, C_ETC,
    C_ETC,   C_ETC,   C_LOW_R, C_LOW_S, C_LOW_T, C_LOW_U, C_ETC,   C_ETC,
    C_ETC,   C_ETC,   C_ETC,   C_LCURB, C_ETC,   C_RCURB, C_ETC,   C_ETC
};


/*
    The state codes.
*/
enum states {
    GO,  /* start    */
    OK,  /* ok       */
    OB,  /* object   */
    KE,  /* key      */
    CO,  /* colon    */
    VA,  /* value    */
    AR,  /* array    */
    ST,  /* string   */
    ESC,  /* escape   */
    U1,  /* u1       */
    U2,  /* u2       */
    U3,  /* u3       */
    U4,  /* u4       */
    MI,  /* minus    */
    ZE,  /* zero     */
    IT,  /* integer  */
    FR,  /* fraction */
    E1,  /* e        */
    E2,  /* ex       */
    E3,  /* exp      */
    T1,  /* tr       */
    T2,  /* tru      */
    T3,  /* true     */
    F1,  /* fa       */
    F2,  /* fal      */
    F3,  /* fals     */
    F4,  /* false    */
    N1,  /* nu       */
    N2,  /* nul      */
    N3,  /* null     */
    C1,  /* /        */
    C2,  /* / *     */
    C3,  /* *        */
    FX,  /* *.* *eE* */
    D1,  /* second UTF-16 character decoding started by \ */
    D2,  /* second UTF-16 character proceeded by u */
    NR_STATES
};

enum actions
{
    CB = -10, /* comment begin */
    CE = -11, /* comment end */
    FA = -12, /* false */
    TR = -13, /* false */
    NU = -14, /* null */
    DE = -15, /* double detected by exponent e E */
    DF = -16, /* double detected by fraction . */
    SB = -17, /* string begin */
    MX = -18, /* integer detected by minus */
    ZX = -19, /* integer detected by zero */
    IX = -20, /* integer detected by 1-9 */
    EX = -21, /* next char is escaped */
    UC = -22  /* Unicode character read */
};


static const signed char state_transition_table[NR_STATES][NR_CLASSES] = {
/*
    The state transition table takes the current state and the current symbol,
    and returns either a new state or an action. An action is represented as a
    negative number. A JSON text is accepted if at the end of the text the
    state is OK and if the mode is MODE_DONE.

                 white                                      1-9                                   ABCDF  etc
             space |  {  }  [  ]  :  ,  "  \  /  +  -  .  0  |  a  b  c  d  e  f  l  n  r  s  t  u  |  E  |  * */
/*start  GO*/ {GO,GO,-6,XX,-5,XX,XX,XX,XX,XX,CB,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX},
/*ok     OK*/ {OK,OK,XX,-8,XX,-7,XX,-3,XX,XX,CB,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX},
/*object OB*/ {OB,OB,XX,-9,XX,XX,XX,XX,SB,XX,CB,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX},
/*key    KE*/ {KE,KE,XX,XX,XX,XX,XX,XX,SB,XX,CB,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX},
/*colon  CO*/ {CO,CO,XX,XX,XX,XX,-2,XX,XX,XX,CB,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX},
/*value  VA*/ {VA,VA,-6,XX,-5,XX,XX,XX,SB,XX,CB,XX,MX,XX,ZX,IX,XX,XX,XX,XX,XX,FA,XX,NU,XX,XX,TR,XX,XX,XX,XX,XX},
/*array  AR*/ {AR,AR,-6,XX,-5,-7,XX,XX,SB,XX,CB,XX,MX,XX,ZX,IX,XX,XX,XX,XX,XX,FA,XX,NU,XX,XX,TR,XX,XX,XX,XX,XX},
/*string ST*/ {ST,XX,ST,ST,ST,ST,ST,ST,-4,EX,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST,ST},
/*escape ES*/ {XX,XX,XX,XX,XX,XX,XX,XX,ST,ST,ST,XX,XX,XX,XX,XX,XX,ST,XX,XX,XX,ST,XX,ST,ST,XX,ST,U1,XX,XX,XX,XX},
/*u1     U1*/ {XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,U2,U2,U2,U2,U2,U2,U2,U2,XX,XX,XX,XX,XX,XX,U2,U2,XX,XX},
/*u2     U2*/ {XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,U3,U3,U3,U3,U3,U3,U3,U3,XX,XX,XX,XX,XX,XX,U3,U3,XX,XX},
/*u3     U3*/ {XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,U4,U4,U4,U4,U4,U4,U4,U4,XX,XX,XX,XX,XX,XX,U4,U4,XX,XX},
/*u4     U4*/ {XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,UC,UC,UC,UC,UC,UC,UC,UC,XX,XX,XX,XX,XX,XX,UC,UC,XX,XX},
/*minus  MI*/ {XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,ZE,IT,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX},
/*zero   ZE*/ {OK,OK,XX,-8,XX,-7,XX,-3,XX,XX,CB,XX,XX,DF,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX},
/*int    IT*/ {OK,OK,XX,-8,XX,-7,XX,-3,XX,XX,CB,XX,XX,DF,IT,IT,XX,XX,XX,XX,DE,XX,XX,XX,XX,XX,XX,XX,XX,DE,XX,XX},
/*frac   FR*/ {OK,OK,XX,-8,XX,-7,XX,-3,XX,XX,CB,XX,XX,XX,FR,FR,XX,XX,XX,XX,E1,XX,XX,XX,XX,XX,XX,XX,XX,E1,XX,XX},
/*e      E1*/ {XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,E2,E2,XX,E3,E3,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX},
/*ex     E2*/ {XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,E3,E3,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX},
/*exp    E3*/ {OK,OK,XX,-8,XX,-7,XX,-3,XX,XX,XX,XX,XX,XX,E3,E3,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX},
/*tr     T1*/ {XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,T2,XX,XX,XX,XX,XX,XX,XX},
/*tru    T2*/ {XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,T3,XX,XX,XX,XX},
/*true   T3*/ {XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,CB,XX,XX,XX,XX,XX,XX,XX,XX,XX,OK,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX},
/*fa     F1*/ {XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,F2,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX},
/*fal    F2*/ {XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,F3,XX,XX,XX,XX,XX,XX,XX,XX,XX},
/*fals   F3*/ {XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,F4,XX,XX,XX,XX,XX,XX},
/*false  F4*/ {XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,CB,XX,XX,XX,XX,XX,XX,XX,XX,XX,OK,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX},
/*nu     N1*/ {XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,N2,XX,XX,XX,XX},
/*nul    N2*/ {XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,N3,XX,XX,XX,XX,XX,XX,XX,XX,XX},
/*null   N3*/ {XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,CB,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,OK,XX,XX,XX,XX,XX,XX,XX,XX,XX},
/*/      C1*/ {XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,C2},
/*/star  C2*/ {C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C3},
/**      C3*/ {C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,CE,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C2,C3},
/*_.     FX*/ {OK,OK,XX,-8,XX,-7,XX,-3,XX,XX,XX,XX,XX,XX,FR,FR,XX,XX,XX,XX,E1,XX,XX,XX,XX,XX,XX,XX,XX,E1,XX,XX},
/*\      D1*/ {XX,XX,XX,XX,XX,XX,XX,XX,XX,D2,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX},
/*\      D2*/ {XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,XX,U1,XX,XX,XX,XX},
};


/*
    These modes can be pushed on the stack.
*/
enum modes {
    MODE_ARRAY = 1,
    MODE_DONE = 2,
    MODE_KEY = 3,
    MODE_OBJECT = 4
};

static void set_error(JSON_parser jc)
{
    switch (jc->state) {
        case GO:
            switch (jc->current_char) {
            case '{': case '}': case '[': case ']':
                jc->error = JSON_E_UNBALANCED_COLLECTION;
                break;
            default:
                jc->error = JSON_E_INVALID_CHAR;
                break;
            }
            break;
        case OB:
            jc->error = JSON_E_EXPECTED_KEY;
            break;
        case AR:
            jc->error = JSON_E_UNBALANCED_COLLECTION;
            break;
        case CO:
            jc->error = JSON_E_EXPECTED_COLON;
            break;
        case KE:
            jc->error = JSON_E_EXPECTED_KEY;
            break;
        /* \uXXXX\uYYYY */
        case U1: case U2: case U3: case U4: case D1: case D2:
            jc->error = JSON_E_INVALID_UNICODE_SEQUENCE;
            break;
        /* true, false, null */
        case T1: case T2: case T3: case F1: case F2: case F3: case F4: case N1: case N2: case N3:
            jc->error = JSON_E_INVALID_KEYWORD;
            break;
        /* minus, integer, fraction, exponent */
        case MI: case ZE: case IT: case FR: case E1: case E2: case E3:
            jc->error = JSON_E_INVALID_NUMBER;
            break;
        default:
            jc->error = JSON_E_INVALID_CHAR;
            break;
    }
}

static int
push(JSON_parser jc, int mode)
{
/*
    Push a mode onto the stack. Return false if there is overflow.
*/
    assert(jc->top <= jc->stack_capacity);

    if (jc->depth < 0) {
        if (jc->top == jc->stack_capacity) {
            const size_t bytes_to_copy = jc->stack_capacity * sizeof(jc->stack[0]);
            const size_t new_capacity = jc->stack_capacity * 2;
            const size_t bytes_to_allocate = new_capacity * sizeof(jc->stack[0]);
            void* mem = JSON_parser_malloc(jc->malloc, bytes_to_allocate, "stack");
            if (!mem) {
                jc->error = JSON_E_OUT_OF_MEMORY;
                return false;
            }
            jc->stack_capacity = (int)new_capacity;
            memcpy(mem, jc->stack, bytes_to_copy);
            if (jc->stack != &jc->static_stack[0]) {
                jc->free(jc->stack);
            }
            jc->stack = (signed char*)mem;
        }
    } else {
        if (jc->top == jc->depth) {
            jc->error = JSON_E_NESTING_DEPTH_REACHED;
            return false;
        }
    }
    jc->stack[++jc->top] = (signed char)mode;
    return true;
}


static int
pop(JSON_parser jc, int mode)
{
/*
    Pop the stack, assuring that the current mode matches the expectation.
    Return false if there is underflow or if the modes mismatch.
*/
    if (jc->top < 0 || jc->stack[jc->top] != mode) {
        return false;
    }
    jc->top -= 1;
    return true;
}


#define parse_buffer_clear(jc) \
    do {\
        jc->parse_buffer_count = 0;\
        jc->parse_buffer[0] = 0;\
    } while (0)

#define parse_buffer_pop_back_char(jc)\
    do {\
        assert(jc->parse_buffer_count >= 1);\
        --jc->parse_buffer_count;\
        jc->parse_buffer[jc->parse_buffer_count] = 0;\
    } while (0)



void delete_JSON_parser(JSON_parser jc)
{
    if (jc) {
        if (jc->stack != &jc->static_stack[0]) {
            jc->free((void*)jc->stack);
        }
        if (jc->parse_buffer != &jc->static_parse_buffer[0]) {
            jc->free((void*)jc->parse_buffer);
        }
        jc->free((void*)jc);
     }
}

int JSON_parser_reset(JSON_parser jc)
{
    if (NULL == jc) {
        return false;
    }

    jc->state = GO;
    jc->top = -1;

    /* parser has been used previously? */
    if (NULL == jc->parse_buffer) {

        /* Do we want non-bound stack? */
        if (jc->depth > 0) {
            jc->stack_capacity = jc->depth;
            if (jc->depth <= (int)COUNTOF(jc->static_stack)) {
                jc->stack = &jc->static_stack[0];
            } else {
                const size_t bytes_to_alloc = jc->stack_capacity * sizeof(jc->stack[0]);
                jc->stack = (signed char*)JSON_parser_malloc(jc->malloc, bytes_to_alloc, "stack");
                if (jc->stack == NULL) {
                    return false;
                }
            }
        } else {
            jc->stack_capacity = (int)COUNTOF(jc->static_stack);
            jc->depth = -1;
            jc->stack = &jc->static_stack[0];
        }

        /* set up the parse buffer */
        jc->parse_buffer = &jc->static_parse_buffer[0];
        jc->parse_buffer_capacity = COUNTOF(jc->static_parse_buffer);
    }

    /* set parser to start */
    push(jc, MODE_DONE);
    parse_buffer_clear(jc);

    return true;
}

JSON_parser
new_JSON_parser(JSON_config const * config)
{
/*
    new_JSON_parser starts the checking process by constructing a JSON_parser
    object. It takes a depth parameter that restricts the level of maximum
    nesting.

    To continue the process, call JSON_parser_char for each character in the
    JSON text, and then call JSON_parser_done to obtain the final result.
    These functions are fully reentrant.
*/

    int use_std_malloc = false;
    JSON_config default_config;
    JSON_parser jc;
    JSON_malloc_t alloc;

    /* set to default configuration if none was provided */
    if (NULL == config) {
        /* initialize configuration */
        init_JSON_config(&default_config);
        config = &default_config;
    }

    /* use std malloc if either the allocator or deallocator function isn't set */
    use_std_malloc = NULL == config->malloc || NULL == config->free;

    alloc = use_std_malloc ? malloc : config->malloc;

    jc = (JSON_parser)JSON_parser_malloc(alloc, sizeof(*jc), "parser");

    if (NULL == jc) {
        return NULL;
    }

    /* configure the parser */
    memset(jc, 0, sizeof(*jc));
    jc->malloc = alloc;
    jc->free = use_std_malloc ? free : config->free;
    jc->callback = config->callback;
    jc->ctx = config->callback_ctx;
    jc->allow_comments = (signed char)(config->allow_comments != 0);
    jc->handle_floats_manually = (signed char)(config->handle_floats_manually != 0);
    jc->decimal_point = *localeconv()->decimal_point;
    /* We need to be able to push at least one object */
    jc->depth = config->depth == 0 ? 1 : config->depth;

    /* reset the parser */
    if (!JSON_parser_reset(jc)) {
        jc->free(jc);
        return NULL;
    }

    return jc;
}

static int parse_buffer_grow(JSON_parser jc)
{
    const size_t bytes_to_copy = jc->parse_buffer_count * sizeof(jc->parse_buffer[0]);
    const size_t new_capacity = jc->parse_buffer_capacity * 2;
    const size_t bytes_to_allocate = new_capacity * sizeof(jc->parse_buffer[0]);
    void* mem = JSON_parser_malloc(jc->malloc, bytes_to_allocate, "parse buffer");

    if (mem == NULL) {
        jc->error = JSON_E_OUT_OF_MEMORY;
        return false;
    }

    assert(new_capacity > 0);
    memcpy(mem, jc->parse_buffer, bytes_to_copy);

    if (jc->parse_buffer != &jc->static_parse_buffer[0]) {
        jc->free(jc->parse_buffer);
    }

    jc->parse_buffer = (char*)mem;
    jc->parse_buffer_capacity = new_capacity;

    return true;
}

static int parse_buffer_reserve_for(JSON_parser jc, unsigned chars)
{
    while (jc->parse_buffer_count + chars + 1 > jc->parse_buffer_capacity) {
        if (!parse_buffer_grow(jc)) {
            assert(jc->error == JSON_E_OUT_OF_MEMORY);
            return false;
        }
    }

    return true;
}

#define parse_buffer_has_space_for(jc, count) \
    (jc->parse_buffer_count + (count) + 1 <= jc->parse_buffer_capacity)

#define parse_buffer_push_back_char(jc, c)\
    do {\
        assert(parse_buffer_has_space_for(jc, 1)); \
        jc->parse_buffer[jc->parse_buffer_count++] = c;\
        jc->parse_buffer[jc->parse_buffer_count]   = 0;\
    } while (0)

#define assert_is_non_container_type(jc) \
    assert( \
        jc->type == JSON_T_NULL || \
        jc->type == JSON_T_FALSE || \
        jc->type == JSON_T_TRUE || \
        jc->type == JSON_T_FLOAT || \
        jc->type == JSON_T_INTEGER || \
        jc->type == JSON_T_STRING)


static int parse_parse_buffer(JSON_parser jc)
{
    if (jc->callback) {
        JSON_value value, *arg = NULL;

        if (jc->type != JSON_T_NONE) {
            assert_is_non_container_type(jc);

            switch(jc->type) {
                case JSON_T_FLOAT:
                    arg = &value;
                    if (jc->handle_floats_manually) {
                        value.vu.str.value = jc->parse_buffer;
                        value.vu.str.length = jc->parse_buffer_count;
                    } else {
                        /* not checking with end pointer b/c there may be trailing ws */
                        value.vu.float_value = strtod(jc->parse_buffer, NULL);
                    }
                    break;
                case JSON_T_INTEGER:
                    arg = &value;
                    sscanf(jc->parse_buffer, JSON_PARSER_INTEGER_SSCANF_TOKEN, &value.vu.integer_value);
                    break;
                case JSON_T_STRING:
                    arg = &value;
                    value.vu.str.value = jc->parse_buffer;
                    value.vu.str.length = jc->parse_buffer_count;
                    break;
            }

            if (!(*jc->callback)(jc->ctx, jc->type, arg)) {
                return false;
            }
        }
    }

    parse_buffer_clear(jc);

    return true;
}

#define IS_HIGH_SURROGATE(uc) (((uc) & 0xFC00) == 0xD800)
#define IS_LOW_SURROGATE(uc)  (((uc) & 0xFC00) == 0xDC00)
#define DECODE_SURROGATE_PAIR(hi,lo) ((((hi) & 0x3FF) << 10) + ((lo) & 0x3FF) + 0x10000)
static const unsigned char utf8_lead_bits[4] = { 0x00, 0xC0, 0xE0, 0xF0 };

static int decode_unicode_char(JSON_parser jc)
{
    int i;
    unsigned uc = 0;
    char* p;
    int trail_bytes;

    assert(jc->parse_buffer_count >= 6);

    p = &jc->parse_buffer[jc->parse_buffer_count - 4];

    for (i = 12; i >= 0; i -= 4, ++p) {
        unsigned x = *p;

        if (x >= 'a') {
            x -= ('a' - 10);
        } else if (x >= 'A') {
            x -= ('A' - 10);
        } else {
            x &= ~0x30u;
        }

        assert(x < 16);

        uc |= x << i;
    }

    /* clear UTF-16 char from buffer */
    jc->parse_buffer_count -= 6;
    jc->parse_buffer[jc->parse_buffer_count] = 0;

    if (uc == 0xffff || uc == 0xfffe) {
        return false;
    }

    /* attempt decoding ... */
    if (jc->utf16_high_surrogate) {
        if (IS_LOW_SURROGATE(uc)) {
            uc = DECODE_SURROGATE_PAIR(jc->utf16_high_surrogate, uc);
            trail_bytes = 3;
            jc->utf16_high_surrogate = 0;
        } else {
            /* high surrogate without a following low surrogate */
            return false;
        }
    } else {
        if (uc < 0x80) {
            trail_bytes = 0;
        } else if (uc < 0x800) {
            trail_bytes = 1;
        } else if (IS_HIGH_SURROGATE(uc)) {
            /* save the high surrogate and wait for the low surrogate */
            jc->utf16_high_surrogate = (UTF16)uc;
            return true;
        } else if (IS_LOW_SURROGATE(uc)) {
            /* low surrogate without a preceding high surrogate */
            return false;
        } else {
            trail_bytes = 2;
        }
    }

    jc->parse_buffer[jc->parse_buffer_count++] = (char) ((uc >> (trail_bytes * 6)) | utf8_lead_bits[trail_bytes]);

    for (i = trail_bytes * 6 - 6; i >= 0; i -= 6) {
        jc->parse_buffer[jc->parse_buffer_count++] = (char) (((uc >> i) & 0x3F) | 0x80);
    }

    jc->parse_buffer[jc->parse_buffer_count] = 0;

    return true;
}

static int add_escaped_char_to_parse_buffer(JSON_parser jc, int next_char)
{
    assert(parse_buffer_has_space_for(jc, 1));

    jc->escaped = 0;
    /* remove the backslash */
    parse_buffer_pop_back_char(jc);
    switch(next_char) {
        case 'b':
            parse_buffer_push_back_char(jc, '\b');
            break;
        case 'f':
            parse_buffer_push_back_char(jc, '\f');
            break;
        case 'n':
            parse_buffer_push_back_char(jc, '\n');
            break;
        case 'r':
            parse_buffer_push_back_char(jc, '\r');
            break;
        case 't':
            parse_buffer_push_back_char(jc, '\t');
            break;
        case '"':
            parse_buffer_push_back_char(jc, '"');
            break;
        case '\\':
            parse_buffer_push_back_char(jc, '\\');
            break;
        case '/':
            parse_buffer_push_back_char(jc, '/');
            break;
        case 'u':
            parse_buffer_push_back_char(jc, '\\');
            parse_buffer_push_back_char(jc, 'u');
            break;
        default:
            return false;
    }

    return true;
}

static int add_char_to_parse_buffer(JSON_parser jc, int next_char, int next_class)
{
    if (!parse_buffer_reserve_for(jc, 1)) {
        assert(JSON_E_OUT_OF_MEMORY == jc->error);
        return false;
    }

    if (jc->escaped) {
        if (!add_escaped_char_to_parse_buffer(jc, next_char)) {
            jc->error = JSON_E_INVALID_ESCAPE_SEQUENCE;
            return false;
        }
    } else if (!jc->comment) {
        if ((jc->type != JSON_T_NONE) | !((next_class == C_SPACE) | (next_class == C_WHITE)) /* non-white-space */) {
            parse_buffer_push_back_char(jc, (char)next_char);
        }
    }

    return true;
}

#define assert_type_isnt_string_null_or_bool(jc) \
    assert(jc->type != JSON_T_FALSE); \
    assert(jc->type != JSON_T_TRUE); \
    assert(jc->type != JSON_T_NULL); \
    assert(jc->type != JSON_T_STRING)


int
JSON_parser_char(JSON_parser jc, int next_char)
{
/*
    After calling new_JSON_parser, call this function for each character (or
    partial character) in your JSON text. It can accept UTF-8, UTF-16, or
    UTF-32. It returns true if things are looking ok so far. If it rejects the
    text, it returns false.
*/
    int next_class, next_state;

/*
    Store the current char for error handling
*/
    jc->current_char = next_char;

/*
    Determine the character's class.
*/
    if (next_char < 0) {
        jc->error = JSON_E_INVALID_CHAR;
        return false;
    }
    if (next_char >= 128) {
        next_class = C_ETC;
    } else {
        next_class = ascii_class[next_char];
        if (next_class <= XX) {
            set_error(jc);
            return false;
        }
    }

    if (!add_char_to_parse_buffer(jc, next_char, next_class)) {
        return false;
    }

/*
    Get the next state from the state transition table.
*/
    next_state = state_transition_table[jc->state][next_class];
    if (next_state >= 0) {
/*
    Change the state.
*/
        jc->state = (signed char)next_state;
    } else {
/*
    Or perform one of the actions.
*/
        switch (next_state) {
/* Unicode character */
        case UC:
            if(!decode_unicode_char(jc)) {
                jc->error = JSON_E_INVALID_UNICODE_SEQUENCE;
                return false;
            }
            /* check if we need to read a second UTF-16 char */
            if (jc->utf16_high_surrogate) {
                jc->state = D1;
            } else {
                jc->state = ST;
            }
            break;
/* escaped char */
        case EX:
            jc->escaped = 1;
            jc->state = ESC;
            break;
/* integer detected by minus */
        case MX:
            jc->type = JSON_T_INTEGER;
            jc->state = MI;
            break;
/* integer detected by zero */
        case ZX:
            jc->type = JSON_T_INTEGER;
            jc->state = ZE;
            break;
/* integer detected by 1-9 */
        case IX:
            jc->type = JSON_T_INTEGER;
            jc->state = IT;
            break;

/* floating point number detected by exponent*/
        case DE:
            assert_type_isnt_string_null_or_bool(jc);
            jc->type = JSON_T_FLOAT;
            jc->state = E1;
            break;

/* floating point number detected by fraction */
        case DF:
            assert_type_isnt_string_null_or_bool(jc);
            if (!jc->handle_floats_manually) {
/*
    Some versions of strtod (which underlies sscanf) don't support converting
    C-locale formated floating point values.
*/
                assert(jc->parse_buffer[jc->parse_buffer_count-1] == '.');
                jc->parse_buffer[jc->parse_buffer_count-1] = jc->decimal_point;
            }
            jc->type = JSON_T_FLOAT;
            jc->state = FX;
            break;
/* string begin " */
        case SB:
            parse_buffer_clear(jc);
            assert(jc->type == JSON_T_NONE);
            jc->type = JSON_T_STRING;
            jc->state = ST;
            break;

/* n */
        case NU:
            assert(jc->type == JSON_T_NONE);
            jc->type = JSON_T_NULL;
            jc->state = N1;
            break;
/* f */
        case FA:
            assert(jc->type == JSON_T_NONE);
            jc->type = JSON_T_FALSE;
            jc->state = F1;
            break;
/* t */
        case TR:
            assert(jc->type == JSON_T_NONE);
            jc->type = JSON_T_TRUE;
            jc->state = T1;
            break;

/* closing comment */
        case CE:
            jc->comment = 0;
            assert(jc->parse_buffer_count == 0);
            assert(jc->type == JSON_T_NONE);
            jc->state = jc->before_comment_state;
            break;

/* opening comment  */
        case CB:
            if (!jc->allow_comments) {
                return false;
            }
            parse_buffer_pop_back_char(jc);
            if (!parse_parse_buffer(jc)) {
                return false;
            }
            assert(jc->parse_buffer_count == 0);
            assert(jc->type != JSON_T_STRING);
            switch (jc->stack[jc->top]) {
            case MODE_ARRAY:
            case MODE_OBJECT:
                switch(jc->state) {
                case VA:
                case AR:
                    jc->before_comment_state = jc->state;
                    break;
                default:
                    jc->before_comment_state = OK;
                    break;
                }
                break;
            default:
                jc->before_comment_state = jc->state;
                break;
            }
            jc->type = JSON_T_NONE;
            jc->state = C1;
            jc->comment = 1;
            break;
/* empty } */
        case -9:
            parse_buffer_clear(jc);
            if (jc->callback && !(*jc->callback)(jc->ctx, JSON_T_OBJECT_END, NULL)) {
                return false;
            }
            if (!pop(jc, MODE_KEY)) {
                return false;
            }
            jc->state = OK;
            break;

/* } */ case -8:
            parse_buffer_pop_back_char(jc);
            if (!parse_parse_buffer(jc)) {
                return false;
            }
            if (jc->callback && !(*jc->callback)(jc->ctx, JSON_T_OBJECT_END, NULL)) {
                return false;
            }
            if (!pop(jc, MODE_OBJECT)) {
                jc->error = JSON_E_UNBALANCED_COLLECTION;
                return false;
            }
            jc->type = JSON_T_NONE;
            jc->state = OK;
            break;

/* ] */ case -7:
            parse_buffer_pop_back_char(jc);
            if (!parse_parse_buffer(jc)) {
                return false;
            }
            if (jc->callback && !(*jc->callback)(jc->ctx, JSON_T_ARRAY_END, NULL)) {
                return false;
            }
            if (!pop(jc, MODE_ARRAY)) {
                jc->error = JSON_E_UNBALANCED_COLLECTION;
                return false;
            }

            jc->type = JSON_T_NONE;
            jc->state = OK;
            break;

/* { */ case -6:
            parse_buffer_pop_back_char(jc);
            if (jc->callback && !(*jc->callback)(jc->ctx, JSON_T_OBJECT_BEGIN, NULL)) {
                return false;
            }
            if (!push(jc, MODE_KEY)) {
                return false;
            }
            assert(jc->type == JSON_T_NONE);
            jc->state = OB;
            break;

/* [ */ case -5:
            parse_buffer_pop_back_char(jc);
            if (jc->callback && !(*jc->callback)(jc->ctx, JSON_T_ARRAY_BEGIN, NULL)) {
                return false;
            }
            if (!push(jc, MODE_ARRAY)) {
                return false;
            }
            assert(jc->type == JSON_T_NONE);
            jc->state = AR;
            break;

/* string end " */ case -4:
            parse_buffer_pop_back_char(jc);
            switch (jc->stack[jc->top]) {
            case MODE_KEY:
                assert(jc->type == JSON_T_STRING);
                jc->type = JSON_T_NONE;
                jc->state = CO;

                if (jc->callback) {
                    JSON_value value;
                    value.vu.str.value = jc->parse_buffer;
                    value.vu.str.length = jc->parse_buffer_count;
                    if (!(*jc->callback)(jc->ctx, JSON_T_KEY, &value)) {
                        return false;
                    }
                }
                parse_buffer_clear(jc);
                break;
            case MODE_ARRAY:
            case MODE_OBJECT:
                assert(jc->type == JSON_T_STRING);
                if (!parse_parse_buffer(jc)) {
                    return false;
                }
                jc->type = JSON_T_NONE;
                jc->state = OK;
                break;
            default:
                return false;
            }
            break;

/* , */ case -3:
            parse_buffer_pop_back_char(jc);
            if (!parse_parse_buffer(jc)) {
                return false;
            }
            switch (jc->stack[jc->top]) {
            case MODE_OBJECT:
/*
    A comma causes a flip from object mode to key mode.
*/
                if (!pop(jc, MODE_OBJECT) || !push(jc, MODE_KEY)) {
                    return false;
                }
                assert(jc->type != JSON_T_STRING);
                jc->type = JSON_T_NONE;
                jc->state = KE;
                break;
            case MODE_ARRAY:
                assert(jc->type != JSON_T_STRING);
                jc->type = JSON_T_NONE;
                jc->state = VA;
                break;
            default:
                return false;
            }
            break;

/* : */ case -2:
/*
    A colon causes a flip from key mode to object mode.
*/
            parse_buffer_pop_back_char(jc);
            if (!pop(jc, MODE_KEY) || !push(jc, MODE_OBJECT)) {
                return false;
            }
            assert(jc->type == JSON_T_NONE);
            jc->state = VA;
            break;
/*
    Bad action.
*/
        default:
            set_error(jc);
            return false;
        }
    }
    return true;
}

int
JSON_parser_done(JSON_parser jc)
{
    if ((jc->state == OK || jc->state == GO) && pop(jc, MODE_DONE))
    {
        return true;
    }

    jc->error = JSON_E_UNBALANCED_COLLECTION;
    return false;
}


int JSON_parser_is_legal_white_space_string(const char* s)
{
    int c, char_class;

    if (s == NULL) {
        return false;
    }

    for (; *s; ++s) {
        c = *s;

        if (c < 0 || c >= 128) {
            return false;
        }

        char_class = ascii_class[c];

        if (char_class != C_SPACE && char_class != C_WHITE) {
            return false;
        }
    }

    return true;
}

int JSON_parser_get_last_error(JSON_parser jc)
{
    return jc->error;
}


void init_JSON_config(JSON_config* config)
{
    if (config) {
        memset(config, 0, sizeof(*config));

        config->depth = JSON_PARSER_STACK_SIZE - 1;
        config->malloc = malloc;
        config->free = free;
    }
}

#undef XX
#undef COUNTOF
#undef parse_buffer_clear
#undef parse_buffer_pop_back_char
