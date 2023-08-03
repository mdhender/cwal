module.obj := porex.o
module.tests := test.whcl
module.cname := regex_posix
module.builtin := 1
ifeq (1,$(HAVE_REGCOMP))
module.enabled := 1
endif

WHCL_REGEX_CAPTURE_LIMIT ?= 10

$(WHCL.module.regex-posix.dir)/rx_common_static.c:
$(WHCL.module.regex-posix.dir)/porex.o $(WHCL.module.regex-posix.dir)/static_module.o: \
  $(WHCL.module.regex-posix.dir)/rx_common_static.c
$(WHCL.module.regex-posix.dir)/porex.o $(WHCL.module.regex-posix.dir)/static_module.o: \
  CPPFLAGS+=-DREGEX_MAX_CAPTURES=$(WHCL_REGEX_CAPTURE_LIMIT)
