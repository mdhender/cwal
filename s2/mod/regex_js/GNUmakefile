#########################################################################
# jsregex loadable module
########################################################################
$(error The regex_js module is no longer maintained because of numerous \
upstream bugs. The author seems to have abandoned it.)

MODULE.NAME := regex_js
MODULE.OBJ := jsregex.o regexp.o utf.o utftype.o
MODULE.TESTS := test.s2
include ../module.make

retest.o regexp.o jsregex.o static_module.o: CPPFLAGS+=-DMAXSUB=$(S2_REGEX_CAPTURE_LIMIT)
jsregex.o static_module.o: CPPFLAGS+=-DREGEX_MAX_CAPTURES=$(S2_REGEX_CAPTURE_LIMIT)
jsregex.o: ../regex_posix/s2rx_common_static.c
jsregex.o: Makefile
########################################################################
# A regex test binary using "hidden" main() in regexp.c:
retest.c: regexp.c
	cp -p $< $@
retest.o: CPPFLAGS+=-DTEST
retest.BIN.OBJECTS := retest.o utf.o utftype.o
$(eval $(call ShakeNMake.CALL.RULES.BINS,retest))
all: $(retest.BIN)
CLEAN_FILES += retest.c
# /retest
########################################################################

