#if !defined(ORG_FOSSIL_SCM_FELTA_H_INCLUDED)
#define ORG_FOSSIL_SCM_FELTA_H_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif
/**
   "Felta" (Fossil Delta) is a standalone C library version of a
   slightly extended variant of the core delta-generation code from
   the Fossil SCM (https://fossil-scm.org). It allows client code to
   easily create and apply deltas of both binary and text content
   (limited by local RAM and a _signed_ 32-bit size range).

   The basic delta format is described here:

   https://fossil-scm.org/index.html/doc/trunk/www/delta_format.wiki
   https://www.fossil-scm.org/index.html/doc/trunk/www/delta_encoder_algorithm.wiki
   
   But this variant changes the header slightly to support multiple
   integer encodings. See felta_create() for the details.

   Copyright (c) 2006 D. Richard Hipp (https://www.hwaci.com/drh/)
   Copyright (c) 2012, 2013 stephan beal (https://wanderinghorse.net/home/stephan/)

   This program is free software; you can redistribute it and/or
   modify it under the terms of the Simplified BSD License (also known
   as the "2-Clause License" or "FreeBSD License").

   This program is distributed in the hope that it will be useful, but
   without any warranty; without even the implied warranty of
   merchantability or fitness for a particular purpose.

   THIS COPY HAS BEEN MODIFIED FROM ITS ORIGINAL by stephan beal, who
   forked it out of Fossil for generic re-use in the form of a
   standalone library. It has also been refactored to send deltas to
   arbitrary destinations via a callback, rather than being hard-coded
   for string output (which is now implemented in terms of a
   callback). It can also optionally be built with different integer
   encodings, but that makes it incompatible with the original
   algorithm. This rendition calls the delta format Felta, in
   reference to its origins.
*/
    

/**
   felta API result codes. These MUST have negative values.
*/
enum felta_rc {
/** The conventional not-an-error code. */
FELTA_RC_OK = 0,
/** Allocation failed. */
FELTA_RC_OOM = -1,
/** Some value is outside of its expected range. */
FELTA_RC_RANGE = -2,
/** Invalid separator seen while traversing a delta. */
FELTA_RC_INVALID_SEPARATOR = -10,
/** Some internal size does not have its expected value.
 More specific form of FELTA_RC_RANGE. */
FELTA_RC_INVALID_SIZE = -11,
/** Checksum is invalid (currently used due to refactoring). */
FELTA_RC_CHECKSUM_MISMATCH = -12,
/** An invalid delta operator seen while traversing a delta. */
FELTA_RC_INVALID_OPERATOR = -13,
/** Delta is not properly terminated. */
FELTA_RC_UNTERMINATED = -14,
/** I/O error (intended for use by felta_output_f() implementations). */
FELTA_RC_IO = -20
};

/**
   For the given felta_rc value, this returns the constant's name in
   string form, or some default string if rc is not a felta_rc value.
   The returned bytes are static and NUL-terminated.
*/
char const * felta_rc_cstr(int rc);

enum felta_constants {
    /**
       The minimum length CLIENTS need to
       add to an output buffer for use with
       felta_create().

       This is an internal magic number: the maximum
       about of overhead needed for a 100% diff.
    */
    FELTA_DELTA_PADDING_SIZE = 61
};

/**
  Creates a new delta from two inputs.

  zV1 must point to lenV1 bytes of memory which serves as the
  "original" (or v1) copy. zV2 must point to lenV2 bytes of memory
  which represent the "new" (or v2) copy.
  
  The delta is written into the zDelta buffer, preallocated by the
  caller, which must be at least FELTA_DELTA_PADDING_SIZE bytes longer
  than zV2. The delta string will be NUL-terminated by this routine,
  but it might also contain embedded NUL characters if either the zV1
  or zV2 files are binary.

  On success this function returns the length of the delta string in
  bytes (_excluding_ any presumed final NUL terminator character). On
  error it returns a negative result code from the felta_rc enum.

  Output Format:

  The delta begins with a byte describing the integer encoding: one of
  (d,x,b), for bases 10, 16, and 64, respectively. The rest of the
  docs describe base64 because the original implementation supported
  only base64 (but my brain isn't big enough to debug base64
  encodings, so decimal and hex were added). This encoding applies to
  all further numbers emitted in the delta.

  Immediately after the integer encoding byte comes a number encoded
  in the encoding described by the first byte, followed by a newline.
  This number is the number of bytes in the TARGET file.  Thus, given
  a delta file z, a program can compute the size of the output file
  simply by reading the first line and decoding the number found
  there. The felta_output_size() routine does exactly this.

  After the initial size number, the delta consists of a series of
  literal text segments and commands to copy from the SOURCE file.  A
  copy command looks like this:

  NNN@MMM,

  where NNN is the number of bytes to be copied and MMM is the offset
  into the source file of the first byte (both base-64).  If NNN is 0
  it means copy the rest of the input file.  Literal text is like
  this:

  NNN:TTTTT

  where NNN is the number of bytes of text and TTTTT is the text.

  The last term is of the form

  NNN;

  In this case, NNN is a 32-bit bigendian checksum of the output file
  that can be used to verify that the delta applied correctly.

  Pure text input generate a pure text delta. Binary input generates a
  delta that may contain some binary data.

  Algorithm:

  The encoder first builds a hash table to help it find matching
  patterns in the source file. 16-byte chunks of the source file
  sampled at evenly spaced intervals are used to populate the hash
  table.

  Next we begin scanning the target file using a sliding 16-byte
  window.  The hash of the 16-byte window in the target is used to
  search for a matching section in the source file.  When a match is
  found, a copy command is added to the delta.  An effort is made to
  extend the matching section to regions that come before and after
  the 16-byte hash window.  A copy command is only issued if the
  result would use less space than just quoting the text
  literally. Literal text is added to the delta for sections that do
  not match or which can not be encoded efficiently using copy
  commands.

  This implementation requires dynamically-allocated memory for its
  hashtable. The cost is approximately sizeof(int)*(lenV1/HASHSIZE)*2,
  where HASHSIZE is a compile-time constant defaulting to 16.
*/
int felta_create(unsigned const char *zV1,
                 unsigned int lenV1,
                 unsigned const char *zV2,unsigned int lenV2,
                 unsigned char *zDelta);

/**
   ACHTUNG: this is not known to work 100% correctly, and it's
   clear why. As it effectively duplicates all of felta_apply(),
   but without the output routine, the error is presumably one
   on the part of the porter of this code (that'd be me).

   Verifies the integrity of a delta against a document created from
   it. src must point to srcLen bytes of memory containing content
   created by felta_apply() or equivalent (i.e. the result of applying
   the delta provided to this function). delta must point to deltaLen
   bytes of content created by felta_create() or equivalent. This
   function seeks out the size- and checksum information from the
   given delta and verifies that... well, that the sizes and checksums
   match between src and delta.

   If verification passes, the calculated total length of src (as
   opposed to lenSrc) is returned (they will be the same on success,
   though). On error a negative value from the felta_rc enum is
   returned, containing a rough description of the problem.
   
   On error, if zErr is not NULL, a static error message is stored in
   *zErr.

   Reminder to self: we have to traverse the whole delta to find its
   checksum. We cannot work our way back from the end.
*/
int felta_verify_checksum( unsigned char const * src, unsigned int lenSrc,
                           unsigned char const * delta, unsigned int deltaLen,
                           const char ** zErr);
    
/**
   A typedef for felta output destinations.

   The state pointer is an implementation-specific value. This
   function must consume n bytes of the data pointer and return 0 on
   success. On error it must return a NEGATIVE value.
*/
typedef int (*felta_output_f)( void * state, void const * data, unsigned int n );
/**
   A felta_output_f() implementation which requires state to be a (FILE*).
   If it is NULL, stdout is used in its place.
*/
int felta_output_f_FILE( void * state, void const * data, unsigned int n );
    
/**
   Works identically to felta_create() but sends its output to the
   given callback function. out(outState,...) is called each time this
   function generates data. If it returns an error (any value other
   than 0) then that value is returned from this function.

   Note that this variation does NOT automatically
   NUL-append the output, and the caller should do so after calling
   this if needed for the given output channel.
*/
int felta_create2(unsigned const char *zV1,unsigned int lenV1,
                  unsigned const char *zV2, unsigned int lenV2,
                  felta_output_f out, void * outState );


/**
  Return the size (in bytes) of the output from applying a
  delta. zDelta is assumed to be a delta created using felta_create()
  or equivalent.

  On success it returns a positive value: the length, in bytes, of
  the buffer needed for holding this applied delta, NOT including the
  trailing NUL byte (which must be accounted for by the caller or
  valgrind might get upset). On error a negative value from the
  felta_rc enum is returned.

  This routine is provided so that a procedure which needs to call
  felta_apply() can learn how much space is required for the output
  and hence allocate any needed space.
*/
int felta_output_size(unsigned const char *zDelta,
                      unsigned int lenDelta);

/**
  Applies a delta created with felta_create() (or equivalent).

  The output buffer must be big enough to hold the whole output file
  and a NUL terminator at the end.  The felta_output_size() routine
  will determine the base size, but the client must account for a
  trailing NUL.

  The zDelta delta string must be NUL-terminated, but the delta string
  may contain embedded NUL characters (if the input and output are
  binary files) so we also have to pass in the length of the delta in
  the lenDelta parameter.

  On success this function returns the size of the output in bytes
  (excluding the final NUL terminator character).  If the delta string
  is malformed or intended for use with a source other than zSrc, then
  this routine returns one of the negative values from the felta_rc
  enum.

  On error, a static error message is written to *zErr if zErr is not
  NULL.

  Depending on compile-time options, this function _might_ perform a
  felta_verify_checksum() on the resulting output. If it does and the
  checksum fails, this function will fail. In such a case, zOut will
  almost certainly have been modified but its contents are effectively
  undefined.

  See felta_create() for a description of the delta file format.
*/
int felta_apply(unsigned const char *zSrc,      /* The source or pattern */
                unsigned int lenSrc,   /* Length of the source */
                unsigned const char *zDelta,    /* Delta to apply to the pattern */
                unsigned int lenDelta, /* Length of the delta */
                unsigned char *zOut,          /* Write the output into this preallocated buffer */
                const char ** zErr  /* Write static error message here */
                );
/**
   Works just like felta_apply2() except that it sends its output to the given
   output function and it does not (cannot) perform auto-validation of
   the generated diff vs. the resulting output.
*/
int felta_apply2(unsigned const char *zSrc,      /* The source or pattern file */
                 unsigned int lenSrc,            /* Length of the source file */
                 unsigned const char *zDelta,    /* Delta to apply to the pattern */
                 unsigned int lenDelta,          /* Length of the delta */
                 felta_output_f out,    /* Where to send the output */
                 void * outState,        /* State for the output function */
                 const char ** zErr  /* Write static error message here */
                 );

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif
/*ORG_FOSSIL_SCM_FELTA_H_INCLUDED*/
