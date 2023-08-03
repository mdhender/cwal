/* sqlite3.c MUST come first or it throws errors caused indirectly by
   something which gets #included in the other files. */
#ifdef SQLITE3_C
#  define INC_STRINGIFY_(f) #f
#  define INC_STRINGIFY(f) INC_STRINGIFY_(f)
#  include INC_STRINGIFY(SQLITE3_C)
#endif
#include "whx_sq3.c"
#include "mod_sqlite3.c"
