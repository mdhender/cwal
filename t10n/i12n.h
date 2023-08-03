/* -*- Mode: C; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 2 -*- */ 
/* vim: set ts=2 et sw=2 tw=80: */
/**
   i12n ==> i[nterpretatio]n is an experiment in delegating the
   interpretation of c9n-based data structures to an API which can be
   used by multiple cwal-based languages.

   This ongoing experiment aims to provide a more or less generic
   implementation of the following services for concrete cwal-based
   languages, and/or to provide related features to facilitate the
   creation of such:

   - Stack machine evaluation based on the t10n and i9n APIs.

   - Basic implementations of common numeric operations on cwal_value
     types: add, subtract, multiply, divide, bitwise OR/AND/XOR,
     bitshift

   - Basic implementations of C- and C++-style operators, with the
     ability to hook in language-dependent implementation details.

   And maybe, just maybe, if they can be shoehorned in halfway
   generically:

   - A scope abstraction which sits atop cwal_scope, analog to
     s2_scope. This would be particularly useful for the stack machine
     for purposes of managing the lifetimes of on-the-stack values,
     analog to how s2's "eval holder" works.
*/
#ifndef NET_WANDERINGHORSE_CWAL_I12N_H_INCLUDED_
#define NET_WANDERINGHORSE_CWAL_I12N_H_INCLUDED_
#include "libcwal.h"
#include <stdbool.h>
#include "c9n.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
   Represents a combination value/operator for an s2_engine.  Each
   token represents one operand or operator for an s2 evaluation
   stack. They get allocated often, but recycled by their associated
   s2_engine, so allocations after the first few stack-pops are
   O(1) and cost no new memory.

   Token instances must not be in use more than once concurrently,
   e.g. a token may not be in more than one stack at a time, nor may
   it be in the same stack multiple times. Multiple entries may
   reference the same value, provided it is otherwise safe from being
   swept/vacuumed up.

*/
struct i12n_stoken{
  /**
     A t10n_token_types_e value.
  */
  int ttype;
  /**
     Certain token types have a value associated with them.  The
     tokenization process will create these, but will not add a
     reference to them (because doing so complicates lifetimes, in
     particular for result values which need up-scoping). This means
     the client must be careful when using them, to ensure that they
     get a ref if one is needed, and to either clean them up or
     leave them to the GC if they don't want them.
  */
  cwal_value * value;

  /**
     Used for creating chains (e.g. a stack).
  */
  i12n_stoken * next;

  /**
     Used by the parser to communicate source code location
     information to the stack machine. This is necessary so that
     errors generated at the stack machine level (the operator
     implementations) can report the location information, though the
     stack machine does not otherwise know anything about the source
     code.
  */
  c9n_token srcPos;
};
/**
   Empty-initialized s2_stoken structure, intended for
   const-copy initialization.
*/
#define i12n_stoken_empty_m {                     \
    T10N_T_INVALID/*ttype*/,                      \
    0/*value*/,                               \
    0/*next*/,                                \
    c9_token_empty_m/*srcPos*/               \
  }


#ifdef __cplusplus
}/*extern "C"*/
#endif
#endif
/* include guard */
