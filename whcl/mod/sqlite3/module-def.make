module.obj := whx_sq3.o mod_sqlite3.o
module.tests := test1.whcl test-udf.whcl
module.LDFLAGS := $(SQLITE3_LDFLAGS)
module.CPPFLAGS := $(SQLITE3_CPPFLAGS)
module.builtin := 1
ifeq (0,$(SQLITE3_ORIGIN))
  module.enabled := 0
else
  module.enabled := 1
endif

# Maintenance reminder: our module DLL name is sqlite3.so, which
# confuses the build if $(SQLITE3.C) is in the same dir as the
# module. It still works as expected, but sqlite3.o gets recompiled
# far more often than needed, which is annoying because it takes so
# long to compile.
D := $(WHCL.module.sqlite3.dir)
ifneq (,$(SQLITE3.C))
  # use local or configure-provided sqlite3.[ch]...
  WHCL.SQLITE3.C := $(SQLITE3.C)
  WHCL.SQLITE3.OBJ := local/sqlite3.o
  module.obj += $(WHCL.SQLITE3.OBJ)
  # sqlite3.c build flags, mostly taken from the Fossil SCM's build
  # of this same file.
  WHCL.SQLITE3.C.CPPFLAGS := -DNDEBUG=1 \
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
  WHCL.SQLITE3.C.CFLAGS := $(filter-out -W%,$(CFLAGS))
  $(D)/local:
	mkdir $@
  $(D)/$(WHCL.SQLITE3.OBJ): $(D)/local
  $(D)/$(WHCL.SQLITE3.OBJ): $(WHCL.SQLITE3.C)
  $(D)/$(WHCL.SQLITE3.OBJ): CPPFLAGS+=$(WHCL.SQLITE3.C.CPPFLAGS)
  $(D)/$(WHCL.SQLITE3.OBJ): CPPFLAGS+=$(SQLITE3_CPPFLAGS)
  $(D)/$(WHCL.SQLITE3.OBJ): CFLAGS:=$(WHCL.SQLITE3.C.CFLAGS)
  ifneq (3,$(SQLITE3_ORIGIN))
    # Building local/sqlite3.o from an out-of-free sqlite3.c. We need to override
    # the implicit rule to get it building properly...
    $(D)/$(WHCL.SQLITE3.OBJ): $(WHCL.SQLITE3.C)
		@$(call ShakeNMake.CALL.SETX,"CC [$(WHCL.SQLITE3.C)] => [$@]"); \
		$(CC) -c $(CFLAGS) $(CPPFLAGS) $(WHCL.SQLITE3.C) -o $@
  endif
  $(D)/static_module.o: CPPFLAGS+=-DSQLITE3_C=$(WHCL.SQLITE3.C) $(WHCL.SQLITE3.C.CPPFLAGS)
  $(D)/static_module.o: CFLAGS:=$(WHCL.SQLITE3.C.CFLAGS)
endif
# $(WHCL.SQLITE3.OBJ) takes a long time to build, so we would ideally only
# clean it during distlclean, but the build automation for the modules
# will include module.obj for cleanup _after_ this file is included
# and we currently have no way to trump that. If you're not using
# ccache to speed up builds, now's a nice time to do so.
DISTCLEAN.whcl-mod-sqlite3 += $(WHCL.SQLITE3.OBJ)
