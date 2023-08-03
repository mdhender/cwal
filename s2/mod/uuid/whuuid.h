#if !defined(WANDERGINHORSE_NET_WHUUID_H_INCLUDED)
#define WANDERGINHORSE_NET_WHUUID_H_INCLUDED 1
#include <stdio.h> /* only for decl of FILE. */
/************************************************************************
An experiment in creating random UUIDs (http://wikipedia.org/wiki/Uuid).


Author: Stephan Beal (http://wanderinghorse.net/home/stephan/)

License: Public Domain


Features:

- Small API. Only two relevant classes and a handful of functions.

- Uses a client-specified RNG source. Two are provided with the
library. The RNG source may be arbitrarily stateful, and each may have
instance-specific data.

- State objects have a uniform cleanup interface, but each implementation
defines which cleanup behaviours need to be performed (e.g. closing
an input file).

- Fairly fast, assuming your RNG is. (My 2.6GHz PC can generate, and send
them to stdout, just over 1.3 million UUIDs per second.)


Misfeatures:

- Does not support a specific version of UUID, as detailed at
[http://wikipedia.org/wiki/Uuid]. Its UUIDs have random data in all
positions, as opposed to reserving certain positions for specific
values or using specified algorithms to generate the values. Thus the
UUIDs it generates are similar to Version 4 UUIDs except that no bytes
are reserved for specific values.

PS: i don't really consider that to be a mis-feature. IMHO UUIDs
should be completely random, with no reserved bytes.


------------------------------------------------------------------------
TIP: checking for duplicate UUIDs

The sqlite3 tool can be used for checking for duplicate UUIDs. Simply
print the UUIDs, one per line, and feed them into sqlite3 like so:

@code
sqlite3> create table ids (id,unique(id));
sqlite3> .import myUUIDListFile ids
@endcode

If sqlite3 does not complain, there were no duplicates.

You can also test by sorting the list, removing duplicates, and
checking the length of the list. e.g. assume we have a file named "1m"
containing 1 million UUIDs. From a Unix shell we could do:

@code
~> sort -u < 1m > 1ms
~> ls -la 1m 1ms
@endcode

If the files have the same size then there were no duplicates.

In my tests i have not encountered duplicates except when testing
a deterministic RNG with a specific seed.
************************************************************************/

/** @def WHUUID_CONFIG_KEEP_METRICS

    If WHUUID_CONFIG_KEEP_METRICS is a true value then the library keeps track
    of how many times a given hex digit value is generated by the
    whuuid_rng class. It has a minimal performance hit, but if
    the data will never be used then it can be turned off.
*/
#define WHUUID_CONFIG_KEEP_METRICS 1

/** @enum whuuid_constants

A list of constant values used by the whuuid API.

*/
enum whuuid_constants {
/**
   The length of a UUID canonical-form string, not including
   a trailing NUL byte. e.g.:

   00000000-0000-0000-0000-000000000000
*/
whuuid_length_canonical = 36,
/**
   The length of a UUID in canonical form, including
   a trailing NUL byte.
*/
whuuid_length_cstring = whuuid_length_canonical + 1,
/**
   The number of bytes of data necessary to store
   a UUID in "raw" form.
*/
whuuid_length_bytes = 16
};

/**
   Represents a single UUID.
*/
struct whuuid_t
{
    unsigned char bytes[whuuid_length_bytes];
};
typedef struct whuuid_t whuuid_t;
/**
   A zero-initialized whuiid_t initialization object.
*/
extern const whuuid_t whuuid_t_empty;

/**
   A class holding RNG information. Each instance manages a single RNG
   source, which is used to populate any number of whuiid_t objects
   with random data. They may or may not need to dynamically allocate
   resources (e.g. open a file containing random data), depending
   on the implementation.   

   They should normally be initialized via factory functions, and
   those functions should:

   a) Allocate any private resources the object needs and store them in
   self->impl.

   b) Set the cleanup() member function to a function which knows how
   to clean up any resources stored in self->impl.

   c) Set the rand() member to a function which knows how to use
   the private state to generate random data.


   The most basic usage looks something like this:

   @code
   whuuid_rng st = whuuid_rng_lcrng; // a Linear Congruent RNG
   whuuid_t u = whuuid_t_empty;
   char buffer[whuuid_length_canonical+1]; // buffer for UUID string
   buffer[whuuid_length_canonical] = 0; // add trailing NUL
   for( int i =0; i < 100; ++i )
   {// generate 100 UUIDs to print them
       whuuid_fill_rand( &u, &st ); // generate UUID using st->rand()
       whuuid_to_string( &u, buffer );
       puts(buffer);
   }
   st.cleanup(&st); // see below.
   @endcode

   In that particular case the state object has no state which
   needs cleaning, but we could also set up a FILE as an input source,
   in which case we need to clean up the object:

   @code
   st = whuuid_rng_FILE;
   st.impl = fopen("/dev/urandom", "r");
   ... use st ...
   st.cleanup(&st); // will fclose() the file
   @endcode

   If a state object is dynamically allocated then it should be freed
   after calling its cleanup() member to free any
   implementation-specific resources.
*/
struct whuuid_rng
{
    /**
       Must set *tgt to sizeof(unsigned int) random bytes. Must return
       0 on success or non-zero if something goes wrong (e.g. the
       input source has failed or run out of numbers). How it uses (or
       ignores) the self argument is implementation-specific.
    */
    int (*rand)( struct whuuid_rng * self, unsigned int  * tgt );
    /**
       Must clean up self, but not free self itself. How it does this
       is implementation-specific. If it has no private state,
       this function may be NULL.

       whuuid_rng objects can be allocated on the stack or via
       arbitrary mechanisms, so the cleanup routine must not free the
       self object. How it is freed (after it is cleaned up) depends
       on how it was allocated.
    */
    void (*cleanup)( struct whuuid_rng * self );
    /**
       Implementations may store any private state here. This member is
       not for public use.
    */
    void * impl;
    /**
       Stores the distribution of values created by this state
       object. whuuid_fill_rand() updates these values.
    */
    unsigned long distribution[whuuid_length_bytes];
};


/** Convenience typedef. */
typedef struct whuuid_rng whuuid_rng;

/**
   A zero-initialized whuiid_state initialization object.
*/
extern const whuuid_rng whuuid_rng_empty;

/**
   An almost-empty whuiid_state initialization object with
   its rand() member set to whuuid_lc_rand.
*/
extern const whuuid_rng whuuid_rng_lcrng;

/**
   A whuuid_state initialization object with its rand() member set to
   whuuid_FILE_rand and its cleanup() member set to
   whuuid_FILE_cleanup.  Clients may copy this then set the impl
   member to point it to an opened FILE handle. The FILE handle will
   be closed when the cleanup() member is called. If the state object
   should not close the file when it cleans up, set the cleanup()
   member to NULL.
*/
extern const whuuid_rng whuuid_rng_FILE;

/**
   Implements the whuuid_rng::rand() interface.

   This implementaion uses/abuses st->impl to store a numeric state
   value for a linear congruent RNG. If st->impl is NULL then a seed
   value is generated using some external source (we malloc() a few
   bytes to get a random address, and we use that address as a
   seed). The state value is stored directly in st->impl and does not
   need to be cleaned up. (The memory malloc()ed to get the initial
   seed is free()d immediately after it is malloc()ed.)

   Returns 0 on success, non-zero on error. The only error conditions
   are !st or !tgt. A malloc() error on the initial seeding will not
   cause an error (but causes a determinate (but unspecified) seed
   value to be used).

   In my (informal/unscientific) tests, this RNG works very well for
   generating UUIDs, out-performing /dev/urandom in terms of even
   numeric distribution most of the time.
*/
int whuuid_lc_rand( whuuid_rng * st, unsigned int *tgt );

/**
   Implements the whuuid_rng::rand() interface.

   If st->impl is not NULL it is assumed to be-a (FILE*) and
   sizeof(unsigned int) bytes are read from it and returned via the
   tgt argument.

   Returns non-zero if !st or !st->impl, or if reading from the file
   fails.

   Results are undefined if st->impl is non-null but is-not-a FILE.

   Note that this implementation does nothing fancy like buffering
   some larger amount of random input. Each call reads sizeof(int)
   bytes. If performance is of a concern, create an implementation
   which stores a struct containing the FILE and the buffer somewhere
   in st->impl and reads the input in larger blocks. Also implement a
   cleanup function which can free the buffer.

   @see whuuid_FILE_cleanup()
   @see whuuid_rng_FILE
*/
int whuuid_FILE_rand( whuuid_rng * st, unsigned int * tgt );

/**
   Implements the whuuid_rng::cleanup() interface for state
   objects where obj->impl is-a FILE handle opened via
   fopen() (or equivalent).
   
   Assumes self->impl is-a (FILE*) and calls fclose() on it.
*/
void whuuid_FILE_cleanup( whuuid_rng * self );

/**
   Converts src->bytes to a canonical-form UUID string.  dest must be
   valid memory at least whuuid_length_canonical bytes long, and on
   success exactly whuuid_length_canonical bytes will be written to it.
   No terminating null is added.

   Returns 0 on success, non-zero on error. The only error conditions
   are (!src) or (!dest).
*/
int whuuid_to_string( whuuid_t const * src, char * dest );

/**
   Populates all of dest->bytes, using st->rand() to collect the
   random bytes. It calls st->rand() enough times to collect
   whuuid_length_bytes bytes.

   Returns 0 on success, non-0 on error. The error conditions are:

   - !st or !dest

   - st->rand() returns non-0, in which case that error code is passed
   back to the caller.

   st->distribution is modified by this function to record the number
   of times any given digit (hex 0..f) is generated via a call to
   rand() (but note that each call to st->rand() is used to generate
   (sizeof(unsigning int)*2) digits).

   This routine does not guaranty that the bytes returned by
   st->rand() are used in the exact same order as they are returned.
*/
int whuuid_fill_rand( whuuid_t * dest, whuuid_rng * st );

/**
   Copies whuuid_length_bytes bytes from src to dest->bytes.

   Returns 0 on success. The only error cases are !dest or !src.
*/
int whuuid_fill( whuuid_t * dest, unsigned char const * src );


/**
   Compares lhs->bytes and rhs->bytes and
   returns 0, less than 0, or greater than 0 depending on whether
   lhs equals, is less than, or is greater to rhs, respectively.
   i.e. it behaves like memcmp(3).

   A NULL value for lhs or rhs compares as less-than any other value
   except NULL, to which it compares equal.
*/
short whuuid_compare( whuuid_t const * lhs, whuuid_t const * rhs );

/**
   Debugging/testing function which dumps the RNG distribution counts
   of st to the given FILE handle. The stats are updated on each call
   to whuuid_fill_rand() IF the WHUUID_CONFIG_KEEP_METRICS macro is
   set to a true value when the library is built.

   If full is non-zero then a full list of metrics is dumped,
   otherwise just an overview.

   Returns 0 on success, non-zero on error (!dest, !st, or
   WHUUID_CONFIG_KEEP_METRICS is false).
*/
int whuuid_dump_distribution( whuuid_rng const * st, short full, FILE * dest );

#endif /* WANDERGINHORSE_NET_WHUUID_H_INCLUDED */
