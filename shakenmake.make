########################################################################
# ShakeNMake is a GNU Makefile mini-framework for small C/C++ source
# trees (with a small handful of directories, building libraries
# and/or applications. It relies very much on GNU Make 3.81+ and its
# dependencies generation relies on GCC (for the preprocessor only),
# even if GCC will not be used be used as the primary compiler.
#
all:
########################################################################
# Maintainer's/hacker's notes:
#
# - This copy of shakenmake has been heavily hacked for purposes
#   specific to the libfossil source tree and is possibly not a great
#   basis for use in other projects.
#
# - Vars names starting with ShakeNMake are mostly internal to this
#   makefile and are considered "private" unless documented otherwise.
#   Notable exceptions are most of the ShakeNMake.CALL entries, which
#   are $(call)able functions, and ShakeNMake.EVAL entries, which are
#   $(eval)able code.
#
########################################################################

########################################################################
# Calculate the top of the source tree, in case this is included
# from a subdir.
ShakeNMake.MAKEFILE := $(word $(words $(MAKEFILE_LIST)),$(MAKEFILE_LIST))
$(ShakeNMake.MAKEFILE):# avoid breaking some deps checks if someone renames this file (been there, done that)
TOP_SRCDIR_REL ?= $(patsubst %/,%,$(dir $(ShakeNMake.MAKEFILE)))
#TOP_SRCDIR := $(shell cd -P $(TOP_SRCDIR_REL) && pwd)
TOP_SRCDIR := $(realpath $(TOP_SRCDIR_REL))
#$(error TOP_SRCDIR_REL=$(TOP_SRCDIR_REL)   TOP_SRCDIR=$(TOP_SRCDIR))
#CPPFLAGS += $(INCLUDES)

MAKING_CLEAN ?= 0
ifneq (,$(strip $(filter distclean clean,$(MAKECMDGOALS))))
  MAKING_CLEAN:=1
endif
ifneq (,$(strip $(findstring clean-,$(MAKECMDGOALS))))
  # ^^^ if any clean-XXX or distclean-XXX target is run,
  # assume we're globally cleaning up. Using clean-xxx and non-clean
  # rules in the same invocation is madness.
  MAKING_CLEAN:=1
endif
#$(info MAKING_CLEAN=$(MAKING_CLEAN))

PACKAGE.MAKEFILE := $(firstword $(MAKEFILE_LIST))# normally either Makefile or GNUmakefile
$(PACKAGE.MAKEFILE):

########################################################################
# ShakeNMake.CALL.FIND_FILE call()able function:
# $1 = app name
# $2 = optional path ($(PATH) is always included)
ShakeNMake.CALL.FIND_FILE = $(firstword $(wildcard $(addsuffix /$(1),$(subst :, ,$(2) $(PATH)))))
########################################################################
ShakeNMake.BIN.AR ?= $(call ShakeNMake.CALL.FIND_FILE,$(AR))
ShakeNMake.BIN.GCC ?= $(call ShakeNMake.CALL.FIND_FILE,gcc)
ShakeNMake.BIN.INSTALL ?= $(call ShakeNMake.CALL.FIND_FILE,install)

ifneq (,$(COMSPEC))
$(warning Setting ShakeNMake.SMELLS.LIKE.WINDOWS to 1)
ShakeNMake.SMELLS.LIKE.WINDOWS := 1
ShakeNMake.EXTENSIONS.DLL = .dll
ShakeNMake.EXTENSIONS.LIB = .lib
ShakeNMake.EXTENSIONS.EXE = .exe
else
ShakeNMake.SMELLS.LIKE.WINDOWS := 0
ShakeNMake.EXTENSIONS.DLL = .so
ShakeNMake.EXTENSIONS.LIB = .a
ShakeNMake.EXTENSIONS.EXE =# no whitespace, please
endif

ShakeNMake.SUBMAKE.INDENT := '===== '
########################################################################
# Cleanup rules.
# To ensure proper cleanup order, define deps on clean-., like so:
# clean-.: clean-mysubdir
# clean-mysubdir: clean-prereqsubdir
#...
.PHONY: clean distclean clean-. distclean-.
clean-.:
	@-deep=0; while [[ $$deep -lt $(MAKELEVEL) ]]; do \
		echo -n $(ShakeNMake.SUBMAKE.INDENT); deep=$$((deep+1)); done; \
	echo "Cleaning up $(CURDIR)..."; \
	rm -f ./$@.$$$$ $(CLEAN_FILES) 

clean: | clean-.

distclean-.:
	@-deep=0; while [[ $$deep -lt $(MAKELEVEL) ]]; do \
		echo -n $(ShakeNMake.SUBMAKE.INDENT); deep=$$((deep+1)); done; \
	echo "Dist-cleaning $(CURDIR)..."; \
	rm -f ./$@.$$$$ $(CLEAN_FILES) $(DISTCLEAN_FILES) 
distclean: | distclean-.
ifeq (0,1)
# These rules semantically conflict with ShakeNMake.EVAL.CLEAN-SET.
clean-%:
	@$(MAKE) --no-print-directory -C $* clean
distclean-%:
	@$(MAKE) --no-print-directory -C $* distclean
endif

########################################################################
# $(call)able which installs clean-$(1) and distclean-$(1) rules which
# rm's all files/dirs listed in $(CLEAN.$(1)) and $(DISTCLEAN.$(1))
# when the clean resp. distclean targets are run. The intention of
# this is to allow splitting-up of the CLEAN_FILES/DISTCLEAN_FILES
# into separate sets to avoid potential too-many-files problems on
# large top-level build files.
define ShakeNMake.EVAL.CLEAN-SET
clean-.: clean-$(1)
clean-$(1):
	@echo "Cleaning up [$(1)]..."; \
	rm -f ./$@.$$$$ $$(CLEAN.$(1))
distclean-.: distclean-$(1)
distclean-$(1):
	@echo "Dist-cleaning up [$(1)]..."; \
	rm -f ./$@.$$$$ $$(CLEAN.$(1)) $$(DISTCLEAN.$(1))
ifneq (shakenmake,$(1))
clean-shakenmake: clean-$(1)
distclean-shakenmake: distclean-$(1)
endif
endef
# For each space-delimited argument, eval's ShakeNMake.EVAL.CLEAN-SET
# for that argument.
define ShakeNMake.CALL.CLEAN-SET
$(foreach cset,$(1),$(eval $(call ShakeNMake.EVAL.CLEAN-SET,$(cset))))
endef

########################################################################
# An internal hack to enable "quiet" output. $(1) is a string which
# is shown ONLY if ShakeNMake.QUIET!=1
ShakeNMake.QUIET ?= 0
define ShakeNMake.CALL.SETX
if test x1 = "x$(ShakeNMake.QUIET)"; then echo $(1); else set -x; fi
endef
########################################################################

########################################################################
# Building a static library:
#
#   myLib.LIB.OBJECTS = foo.o bar.o
#   $(call ShakeNMake.CALL.RULES.LIBS,myLib)
#
# A target named myLib.LIB is created for building the library and
# $(myLib.LIB) holds the name of the compiled lib (typically
# "myLib.a").
#
########################################################################
# ShakeNMake.EVAL.RULES.LIB creates rules to build static library
# $(1).a
define ShakeNMake.EVAL.RULES.LIB
$(1).LIB = $(1).a
$(1).LIB: $$($(1).LIB)
CLEAN.shakenmake += $$($(1).LIB)
$$($(1).LIB): $$($(1).LIB.OBJECTS)
ifneq (1,$$(MAKING_CLEAN))
	@$(call ShakeNMake.CALL.SETX,"AR [$$@]"); \
		$$(ShakeNMake.BIN.AR) crs $$@ $$($(1).LIB.OBJECTS)
endif
endef
define ShakeNMake.CALL.RULES.LIBS
$(foreach liba,$(1),$(eval $(call ShakeNMake.EVAL.RULES.LIB,$(liba))))
endef
# end ShakeNMake.EVAL.RULES.LIB
########################################################################

########################################################################
# ShakeNMake.EVAL.RULES.DLL builds builds
# $(1)$(ShakeNMake.EXTENSIONS.DLL) from object files defined by
# $(1).DLL.OBJECTS and $(1).DLL.SOURCES. Flags passed on to the shared
# library linker ($(CC)) include:
#
#   LDFLAGS, $(1).DLL.LDFLAGS, LDADD, -shared -static-libgcc,
#   $(1).DLL.OBJECTS, $(1).DLL.LDFLAGS
#
# Also defines the var $(1).DLL, which expands to the filename of the DLL,
# (normally $(1)$(ShakeNMake.EXTENSIONS.DLL)).
define ShakeNMake.EVAL.RULES.DLL
$(1).DLL = $(1)$(ShakeNMake.EXTENSIONS.DLL)
ifneq (.DLL,$(ShakeNMake.EXTENSIONS.DLL))
$(1).DLL: $$($(1).DLL)
endif
CLEAN.shakenmake += $$($(1).DLL)
$$($(1).DLL): $$($(1).DLL.SOURCES) $$($(1).DLL.OBJECTS)
ifneq (1,$$(MAKING_CLEAN))
	@test x = "x$$($(1).DLL.OBJECTS)$$($(1).DLL.SOURCES)" && { \
	echo "$(1).DLL.OBJECTS and/or $(1).DLL.SOURCES are/is undefined!"; exit 1; }; \
	$(call ShakeNMake.CALL.SETX,"LD [$$@]"); \
	 $$(CC) -o $$@ -shared $$(LDFLAGS) $(LDLIBS) \
		$$($(1).DLL.OBJECTS) \
		$$($(1).DLL.LDFLAGS)
endif
endef
########################################################################
# $(call ShakeNMake.CALL.RULES.DLLS,[list]) calls and $(eval)s
# ShakeNMake.EVAL.RULES.DLL for each entry in $(1)
define ShakeNMake.CALL.RULES.DLLS
$(foreach dll,$(1),$(eval $(call ShakeNMake.EVAL.RULES.DLL,$(dll))))
endef
# end ShakeNMake.CALL.RULES.DLLS and friends
########################################################################


########################################################################
# ShakeNMake.CALL.RULES.BIN is intended to be called like so:
# $(eval $(call ShakeNMake.CALL.RULES.BIN,MyApp))
#
# It builds a binary named $(1) by running $(CXX) and passing it:
#
# LDFLAGS, $(1).BIN.LDFLAGS, $(1).BIN.OBJECTS
define ShakeNMake.CALL.RULES.BIN
$(1).BIN := $(1)$(ShakeNMake.EXTENSIONS.EXE)
$(1).BIN: $$($(1).BIN)
# Many developers feel that bins should not be cleaned by 'make
# clean', but instead by distclean, but i'm not one of those
# developers. i subscribe more to the school of thought that distclean
# is for cleaning up configure-created files. That said, shake-n-make
# isn't designed to use a configure-like process, so that is probably
# moot here and we probably (maybe?) should clean up bins only in
# distclean. As always: hack it to suit your preference:
CLEAN.shakenmake += $$($(1).BIN)
$$($(1).BIN): $$($(1).BIN.OBJECTS)
ifneq (1,$$(MAKING_CLEAN))
	@test x = "x$$($(1).BIN.OBJECTS)" && { \
	echo "$(1).BIN.OBJECTS is undefined!"; exit 1; }; \
	$(call ShakeNMake.CALL.SETX,"LINK [$$@]"); \
	$$(CC) -o $$@ \
		$$($(1).BIN.OBJECTS) \
		$$(LDFLAGS) $$($(1).BIN.LDFLAGS)
endif
endef
########################################################################
# $(call ShakeNMake.EVAL.RULES.BINS,[list]) calls and $(eval)s
# ShakeNMake.CALL.RULES.BIN for each entry in $(1)
define ShakeNMake.CALL.RULES.BINS
$(foreach bin,$(1),$(eval $(call ShakeNMake.CALL.RULES.BIN,$(bin))))
endef
# end ShakeNMake.CALL.RULES.BIN and friends
########################################################################

########################################################################
# Sets up rules for subdir $1.
########################################################################
# Newer (2021-09-01) subdir rules, ported over from toc...
#
# $(call ShakeNMake.CALL.MAKE-SUBDIRS,dirs_list[,target_name=all])
# Walks each dir in $(1), calling $(MAKE) $(2) for each one.
#
#   $1 = list of dirs
#   $2 = make target
#
# Note that this approach makes parallel builds between the dirs in
# $(1) impossible, so it should only be used for targets where
# parallelizing stuff may screw things up or is otherwise not desired
# or not significant.
ShakeNMake.CALL.MAKE-SUBDIRS = \
	test "x$(1)" = "x" -o "x." = "x$(1)" && exit 0; \
	tgt="$(if $(2),$(2),all)"; \
	make_nop_arg=''; \
	for b in $(1) "_x_"; do test "._x_" = ".$$$$b" && break; \
		pwd=$$$$(pwd); \
		deep=0; while [[ $$$$deep -le $$(MAKELEVEL) ]]; do \
			echo -n $$(ShakeNMake.SUBMAKE.INDENT); deep=$$$$((deep+1)); \
		done; \
		echo "Making '$$$$tgt' in $$$${PWD}/$$$${b}"; \
		$$(MAKE) -C $$$${b} --quiet --no-print-directory $$$${tgt} || exit; \
		cd $$$$pwd || exit; \
	done
.PHONY: subdirs $(ShakeNMake.subdirs) $(patsubst %,subdir-%,$(ShakeNMake.subdirs))
$(ShakeNMake.subdirs):
	@+$(call ShakeNMake.MAKE-SUBDIRS,$@,all)
subdirs: $(ShakeNMake.subdirs)
subdir-%:# run all in subdir $*
	@+$(call ShakeNMake.MAKE-SUBDIRS,$*,all)
subdirs-%:# run target % in $(ShakeNMake.subdirs)
	@+$(call ShakeNMake.MAKE-SUBDIRS,$(ShakeNMake.subdirs),$*)
#########################################################################
.PHONY: subdir-.
subdir-.:
define ShakeNMake.CALL.SUBDIR
.PHONY: $(1)
subdir-$(1):
	@+$(call ShakeNMake.CALL.MAKE-SUBDIRS,$(1),all)
clean: clean-$(1)
clean-.: clean-$(1)
distclean-.: distclean-$(1)
subdir-install-$(1):
	@+$(call ShakeNMake.CALL.MAKE-SUBDIRS,$(1),install)
install: subdir-install-$(1)
subdir-uninstall-$(1):
	@+$(call ShakeNMake.CALL.MAKE-SUBDIRS,$(1),uninstall)
uninstall: subdir-uninstall-$(1)
ShakeNMake.subdirs += $(1)
endef
define ShakeNMake.CALL.SUBDIRS
$(foreach dir,$(1),$(eval $(call ShakeNMake.CALL.SUBDIR,$(dir))))
endef
# end ShakeNMake.CALL.SUBDIRS and friends
########################################################################



########################################################################
# Automatic dependencies generation for C/C++ code...
# To disable deps generation, set ShakeNMake.USE_MKDEPS=0 *before*
# including this file.
#ShakeNMake.USE_MKDEPS := 1
ifeq (,$(ShakeNMake.BIN.GCC))
ShakeNMake.USE_MKDEPS ?= 0
else
ShakeNMake.USE_MKDEPS ?= 1
endif
#$(warning ShakeNMake.USE_MKDEPS=$(ShakeNMake.USE_MKDEPS));
ShakeNMake.CISH_DEPS_FILE := $(TOP_SRCDIR)/.make.c_deps
$(ShakeNMake.CISH_DEPS_FILE):
ifeq (1,$(ShakeNMake.USE_MKDEPS))
# ShakeNMake.CISH_SOURCES contains the list of source files to run through
# the deps generator.
ShakeNMake.CISH_SOURCES ?= $(wildcard *.cpp *.c *.c++ *.h *.hpp *.h++ *.hh)
#$(warning ShakeNMake.CISH_SOURCES=$(ShakeNMake.CISH_SOURCES))
ifneq (,$(ShakeNMake.CISH_SOURCES))
ShakeNMake.BIN.MKDEP = gcc -w -E -MM -MG -MP $(CPPFLAGS)
CLEAN.shakenmake += $(ShakeNMake.CISH_DEPS_FILE)
ShakeNMake.INJECTED_DEPS := $(filter-out $(ShakeNMake.CISH_DEPS_FILE),$(MAKEFILE_LIST))
#ShakeNMake.INJECTED_DEPS := $(filter-out $(ShakeNMake.MAKEFILE),$(MAKEFILE_LIST))
# ^^^^ wouldn't normally do that but this file changes so seldom and deps generation
# takes ages on a raspberry pi.
$(ShakeNMake.CISH_DEPS_FILE): $(PACKAGE.MAKEFILE) $(ShakeNMake.MAKEFILE) $(ShakeNMake.CISH_SOURCES)
ifneq (1,$(MAKING_CLEAN))
	@if [ x = "x$(ShakeNMake.CISH_SOURCES)" ]; then touch $@; exit 0; fi; \
	echo "Generating dependencies..."; \
	$(ShakeNMake.BIN.MKDEP) $(ShakeNMake.CISH_SOURCES) 2>/dev/null > $@; \
	echo "Unmangling gcc-generated deps..."; \
	perl -i -pe 's|^(.+)\.o:\s*((\w+/)*)|$$3$$1.o: $(ShakeNMake.INJECTED_DEPS) $$2|' $@;
endif
# Ugly tree-specific hack for libfossil:
# gcc -E -MM strips leading directory parts on the dep target
# (why, i cannot imagine), so we've got to hack its output.
# Example: input= src/Foo.cpp
# Generated deps:
#  Foo.o: src/Foo.cpp ...
# So we re-extract that first path and prepend it to Foo.o using the
# above perl.  We also add $(MAKEFILE_LIST) has a prereq of all src
# files because i belong to the school of tought which believes that
# changes to the arbitrary build options set in the makefiles should
# cause a rebuild.
.PHONY: deps
deps: $(ShakeNMake.CISH_DEPS_FILE)

ifneq (,$(strip $(filter distclean clean install,$(MAKECMDGOALS))))
# $(warning Skipping C/C++ deps generation.)
# ABSOLUTEBOGO := $(shell $(ShakeNMake.BIN.RM) -f $(ShakeNMake.CISH_DEPS_FILE))
else
#$(warning Including C/C++ deps.)
-include $(ShakeNMake.CISH_DEPS_FILE)
endif
endif
# ^^^^ ifneq(,$(ShakeNMake.CISH_SOURCES))
deps:
endif
# ^^^^ end $(ShakeNMake.USE_MKDEPS)
########################################################################

########################################################################
# Deps, Tromey's Way:
# $(call ShakeNBake.CALL.MkDep.Tromey,source-file,object-file,depend-file)
ifeq (0,1)
.dfiles.list = $(wildcard *.o.d) $(shell find src -name '*.o.d')
ifneq (,$(.dfiles.list))
# $(error include $(.dfiles.list))
include $(.dfiles.list)
endif

define ShakeNBake.CALL.MkDep.Tromey
$(ShakeNMake.BIN.GCC) -MM \
	-w	\
	-MF $(1).d         \
	-MP            \
	-MT $2         \
	$(CPPFLAGS)    \
	$(TARGET_ARCH) \
	$1
endef

CLEAN.shakenmake += $(wildcard *.o.d)
%.o: %.c $(mkdir_compdb)
	@$(call ShakeNMake.CALL.SETX,"CC [$@]"); \
	$(COMPILE.c) -MM -MF $(@).d -MP -MT $@.d $(COMPILE.c.OPTIONS) $(OUTPUT_OPTION) $<
#	$(call ShakeNBake.CALL.MkDep.Tromey,$<,$@,$(@).d);

%.o: %.cpp
	@$(call ShakeNMake.CALL.SETX,"C++ [$@]"); \
	$(COMPILE.cc) -MM -MF $@.d -MP -MT $@ $(COMPILE.cc.OPTIONS) $(OUTPUT_OPTION) $<
#	$(COMPILE.cc) $(COMPILE.cc.OPTIONS) $(OUTPUT_OPTION) $<
#	$(call ShakeNBake.CALL.MkDep.Tromey,$<,$@,$(@).d);
else
%.o: %.c $(mkdir_compdb)
	@$(call ShakeNMake.CALL.SETX,"CC [$@]"); \
	$(COMPILE.c) $(COMPILE.c.OPTIONS) $(OUTPUT_OPTION) $<

%.o: %.cpp
	@$(call ShakeNMake.CALL.SETX,"C++ [$@]"); \
	$(COMPILE.cc) $(COMPILE.cc.OPTIONS) $(OUTPUT_OPTION) $<
endif # Tromey's Way

########################################################################
# Create etags tags...
#
# ShakeNMake.ETAGS.FILTER may optionally be set to a pipe symbol
# followed by a command (or chain of piped commands) which will filter
# its input (file names, one per line) for use in passing to
# $(bin_etags). e.g. it can be used to remove amalgamation-build
# sources from the tags step.
ifneq (1,$(ShakeNMake.ETAGS.DISABLE))
# ShakeNMake.ETAGS.FILTER := | grep -v 'libfossil*.[ch]'
bin_etags := $(call ShakeNMake.CALL.FIND_FILE,etags)
ifeq (.,$(TOP_SRCDIR_REL))
ifneq (,$(bin_etags))
TAGTHESE := $(shell find . -name '*.[ch]' -print $(ShakeNMake.ETAGS.FILTER) )
ifneq (,$(TAGTHESE))
    $(TAGTHESE):
    TAGFILE := TAGS
    $(TAGFILE): $(TAGTHESE) # $(MAKEFILE_LIST)
	@echo "Creating $@..."; \
	for x in $(TAGTHESE); do echo $$x; done | $(bin_etags) -f $@ -; \
		touch $@; true
# ^^^^ Martin G. reports that etags fails with "Unknown option: -" on his
# Ubuntun 18.04 system. Since this isn't a critical feature, we can just
# force this step to succeed.
    tags: $(TAGFILE)
    CLEAN.shakenmake += $(TAGFILE)
    all: $(TAGFILE)
endif
endif
# ^^^ bin_etags
endif
# ^^^ TOP_SRCDIR_REL==.
endif
# ^^^ $(ShakeNMake.TAGS.DISABLE)

#%.o:
#	@$$(call ShakeNMake.CALL.SETX,"CC [$@] ..."); \
#	$(COMPILE.c) $(COMPILE.c.OPTIONS) -o $@

########################################################################
# Installation rules...
#
# Sample usage:
#
#  ShakeNMake.install.bins = mybin myotherbin # installs to $(DESTDIR)$(prefix)/bin
#  ShakeNMake.install.libs = mylib.a myotherlib.a # installs to $(DESTDIR)$(prefix)/lib
#
# Where (bins, libs...) is the name of an installation rule set, the
# complete list of which is defined somewhere below.
#
# Then `make install` will try to DTRT with those.
#
# These rules are quite ancient - dating back to the very early 2000's
# - and arguably severely overengineered. As part of the port out of
# the original tree, the subdir handling "might" have been broken, so
# be on the lookout for that.
ifeq (,$(wildcard $(ShakeNMake.BIN.INSTALL)))
uninstall install:
	@echo "Install rules require that the variable ShakeNMake.BIN.INSTALL be set."; exit 1
else
DESTDIR ?=
prefix ?= /usr/local
########################################################################
# $(call ShakeNMake.CALL.INSTALL.grep_kludge,file_name)
# This is an ancient kludge force [un]install to silently fail without an
# error when passed an empty file list. This was done because saner
# approaches to checking this failed to work on some older machines.
ShakeNMake.CALL.INSTALL.grep_kludge = echo $(1) "" | grep -q '[a-zA-Z0-9]' || exit 0
########################################################################
# $(call ShakeNMake.CALL.INSTALL,file_list,destdir[,install-sh-flags])
# Installs files $(1) to $(DESTDIR)$(2). $(3) is passed on to
# $(ShakeNMake.BIN.INSTALL).
ShakeNMake.CALL.INSTALL = $(call ShakeNMake.CALL.INSTALL.grep_kludge,$(1)); \
	tgtdir="$(DESTDIR)$(2)"; \
	test -d "$$tgtdir" || mkdir -p "$${tgtdir}" \
		|| { err=$$?; echo "$(@): mkdir -p $${tgtdir} failed"; exit $$err; }; \
	for b in $(1) ""; do test -z "$$b" && continue; \
		b2=$${b\#\#*/}; \
		target="$${tgtdir}/$$b2"; \
		cmd="$(ShakeNMake.BIN.INSTALL) $(3) $$b $$target"; echo $$cmd; $$cmd || exit; \
	done

########################################################################
# $(call ShakeNMake.CALL.UNINSTALL,file_list,source_dir)
# removes all files listed in $(1) from target directory $(DESTDIR)$(2).
#
# Maintenance reminder:
# The while/rmdir loop is there to clean up empty dirs left over by
# the uninstall. This is very arguable but seems more or less
# reasonable. The loop takes care to stop when it reaches $(DESTDIR),
# since DESTDIR is presumably (but not necessarily) created by another
# authority.
ShakeNMake.CALL.UNINSTALL =  $(call ShakeNMake.CALL.INSTALL.grep_kludge,$(1)); \
	tgtdir="$(DESTDIR)$(2)"; \
	test -e "$${tgtdir}" || exit 0; \
	for b in $(1) ""; do test -z "$$b" && continue; \
		b2=$${b\#\#*/}; \
		fp="$${tgtdir}/$$b2"; test -e "$$fp" || continue; \
		cmd="rm $$fp"; echo $$cmd; $$cmd || exit $$?; \
	done; \
	tgtdir="$(2)"; \
	while test x != "x$${tgtdir}" -a '$(prefix)' != "$${tgtdir}" \
		-a '/' != "$${tgtdir}" -a -d "$(DESTDIR)$${tgtdir}"; do \
		rmdir $(DESTDIR)$${tgtdir} 2>/dev/null || break; \
		echo "Removing empty dir: $(DESTDIR)$${tgtdir}"; \
		tgtdir=$${tgtdir%/*}; \
	done; true

ShakeNMake.INSTALL.flags.nonbins = -m 0644
ShakeNMake.INSTALL.flags.bins = -s -m 0755
ShakeNMake.INSTALL.flags.bin-scripts = -m 0755
ShakeNMake.INSTALL.flags.dlls = -m 0755
#.PHONY: install-subdirs
#install: subdirs-install
#install-subdirs: subdirs-install
#.PHONY: uninstall-subdirs
#uninstall-subdirs: subdirs-uninstall
#uninstall: subdirs-uninstall

########################################################################
# $(call ShakeNMake.CALL.define-install-set,SET_NAME,dest_dir,install_flags)
define ShakeNMake.EVAL.define-install-set
$(if $(1),,$(error ShakeNMake.CALL.define-install-set requires an install set name as $$1))
$(if $(2),,$(error ShakeNMake.CALL.define-install-set requires an installation path as $$2))

$(if $(ShakeNMake.install.$(1).dupecheck),$(error ShakeNMake.CALL.define-install-set: rules for $1 have already been created. \
	You cannot create them twice.))
ShakeNMake.install.$(1).dupecheck := 1
ShakeNMake.install.$(1).dest ?= $(2)
ShakeNMake.install.$(1).install-flags ?= $(3)

.PHONY: install-$(1)
install-$(1):
	@test x = "x$$(ShakeNMake.install.$(1))" && exit 0; \
	$$(call ShakeNMake.CALL.INSTALL,$$(ShakeNMake.install.$(1)),$$(ShakeNMake.install.$(1).dest),$$(ShakeNMake.install.$(1).install-flags))
install: install-$(1)

.PHONY: uninstall-$(1)
uninstall-$(1):
	@test x = "x$$(ShakeNMake.install.$(1))" && exit 0; \
	$$(call ShakeNMake.CALL.UNINSTALL,$$(ShakeNMake.install.$(1)),$$(ShakeNMake.install.$(1).dest))
uninstall: uninstall-$(1)
endef
ShakeNMake.CALL.define-install-set = $(eval $(call ShakeNMake.EVAL.define-install-set,$(1),$(2),$(3)))
# set up the initial install locations and install flags:
ShakeNMake.INSTALL.target_basenames := bins sbins \
				bin-scripts \
				libs dlls \
				package_libs package_dlls \
				headers package_headers \
				package_data docs \
				man1 man2 man3 man4 \
				man5 man6 man7 man8 man9
$(call ShakeNMake.CALL.define-install-set,bins,$(prefix)/bin,$(ShakeNMake.INSTALL.flags.bins))
$(call ShakeNMake.CALL.define-install-set,bin-scripts,$(prefix)/bin,$(ShakeNMake.INSTALL.flags.bin-scripts))
#$(call ShakeNMake.CALL.define-install-set,sbins,$(prefix)/sbin,$(ShakeNMake.INSTALL.flags.bins))
$(call ShakeNMake.CALL.define-install-set,libs,$(prefix)/lib,$(ShakeNMake.INSTALL.flags.nonbins))
$(call ShakeNMake.CALL.define-install-set,dlls,$(prefix)/lib,$(ShakeNMake.INSTALL.flags.dlls))
#$(call ShakeNMake.CALL.define-install-set,package_libs,$(prefix)/lib/$(package.name),$(ShakeNMake.INSTALL.flags.nonbins))
#$(call ShakeNMake.CALL.define-install-set,package_dlls,$(prefix)/lib/$(package.name),$(ShakeNMake.INSTALL.flags.dlls))
#$(call ShakeNMake.CALL.define-install-set,headers,$(prefix)/include,$(ShakeNMake.INSTALL.flags.nonbins))
#$(call ShakeNMake.CALL.define-install-set,package_headers,$(prefix)/include/$(package.name),$(ShakeNMake.INSTALL.flags.nonbins))
#$(call ShakeNMake.CALL.define-install-set,package_data,$(prefix)/share/$(package.name),$(ShakeNMake.INSTALL.flags.nonbins))
#$(call ShakeNMake.CALL.define-install-set,docs,$(prefix)/share/doc/$(package.name),$(ShakeNMake.INSTALL.flags.nonbins))
# Set up man page entries...
$(foreach NUM,1 2 3 4 5 6 7 8 9,$(call \
	ShakeNMake.CALL.define-install-set,man$(NUM),$(prefix)/share/man/man$(NUM),$(ShakeNMake.INSTALL.flags.nonbins)))
endif
# /install
########################################################################
$(call ShakeNMake.CALL.CLEAN-SET,shakenmake)
