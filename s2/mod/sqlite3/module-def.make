MODULE.NAME := sqlite3
MODULE.OBJ := s2x_sq3.o mod_sqlite3.o
MODULE.TESTS := test.s2 udf.s2
MODULE.LDFLAGS := $(SQLITE3_LDFLAGS)
MODULE.CPPFLAGS := $(SQLITE3_CPPFLAGS)
# Maintenance reminder: our DLL name is sqlite3.so, which confuses the
# build if $(SQLITE3.C) is in the same dir as the module. It still
# works as expected, but sqlite3.o gets recompiled far more often than
# needed, which is annoying because it takes so long to compile.
D := $(DIR.s2-mod)/sqlite3
ifeq (,$(SQLITE3.C))
  $(warning For maximum control and portability, it is recommended \
    that you build against a local copy of sqlite3.[ch] (just place \
    them in $(D)/local));
else
  # use local or configure-provided sqlite3.[ch]...
  SQLITE3.OBJ := local/sqlite3.o
  MODULE.OBJ += $(SQLITE3.OBJ)
  # sqlite3.c build flags, mostly taken from the Fossil SCM's build
  # of this same file.
  SQLITE3.C.CPPFLAGS := -DNDEBUG=1 \
      -DSQLITE_DQS=0 \
      -DSQLITE_THREADSAFE=0 \
      -DSQLITE_DEFAULT_MEMSTATUS=0 \
      -DSQLITE_DEFAULT_WAL_SYNCHRONOUS=1 \
      -DSQLITE_LIKE_DOESNT_MATCH_BLOBS \
      -DSQLITE_OMIT_DECLTYPE \
      -DSQLITE_OMIT_DEPRECATED \
      -DSQLITE_OMIT_PROGRESS_CALLBACK \
      -DSQLITE_OMIT_SHARED_CACHE \
      -DSQLITE_OMIT_LOAD_EXTENSION \
      -DSQLITE_MAX_EXPR_DEPTH=0 \
      -DSQLITE_ENABLE_LOCKING_STYLE=0 \
      -DSQLITE_DEFAULT_FILE_FORMAT=4 \
      -DSQLITE_ENABLE_EXPLAIN_COMMENTS \
      -DSQLITE_ENABLE_FTS4 \
      -DSQLITE_ENABLE_DBSTAT_VTAB \
      -DSQLITE_ENABLE_FTS5 \
      -DSQLITE_ENABLE_STMTVTAB \
      -DSQLITE_HAVE_ZLIB \
      -DSQLITE_ENABLE_DBPAGE_VTAB \
      -DSQLITE_TRUSTED_SCHEMA=0
# The set of -Wno-... flags which we need for a quiet sqlite3.c
# compilation is very compiler-specific, so...
  SQLITE3.C.CFLAGS := $(filter-out -W%,$(CFLAGS))
  $(D)/$(SQLITE3.OBJ): $(SQLITE3.C)
  $(D)/$(SQLITE3.OBJ): CPPFLAGS+=$(SQLITE3.C.CPPFLAGS)
  $(D)/$(SQLITE3.OBJ): CFLAGS:=$(SQLITE3.C.CFLAGS)
  ifneq (3,$(SQLITE3_ORIGIN))
  # Building local/sqlite3.o from an out-of-free sqlite3.c. We need to override
  # the implicit rule to get it building properly...
  $(D)/$(SQLITE3.OBJ):
	@$(call ShakeNMake.CALL.SETX,"CC [$(SQLITE3.C)] => [$@]"); \
	$(CC) -c $(CFLAGS) $(CPPFLAGS) $(SQLITE3.C) -o $@
  endif
  $(D)/static_module.o: CPPFLAGS+=-DSQLITE3_C=$(SQLITE3.C) $(SQLITE3.C.CPPFLAGS)
  $(D)/static_module.o: CFLAGS:=$(SQLITE3.C.CFLAGS)
endif
# $(SQLITE3.OBJ) takes a long time to build, so we would ideally only
# clean it during distlclean, but the build automation for the modules
# will include MODULE.OBJ for cleanup _after_ this file is included
# and we currently have no way to trump that. If you're not using
# ccache to speed up builds, now's a nice time to do so.
DISTCLEAN.s2-mod-sqlite3 := $(SQLITE3.OBJ)
# CLEAN.s2-mod-sqlite3 := $(filter-out $(LOCAL_SQ3.O),$(CLEAN.s2-mod-sqlite3))
