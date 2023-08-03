########################################
# FFI (Foreign Function Interface)
#
# 2018-11-26: this was contributed code which is long-since
# untested/unmaintained. "The plan" is to eventually re-evaluate it
# for use as a loadable module.
#
# This snippet was taken from {top_srcdir}/s2/Makefile and references
# vars defined in that file.
ENABLE_FFI ?= 0
ifneq (0,$(ENABLE_FFI))
  $(info Enabling FFI)
  s2_ffi.o: CPPFLAGS += -DS2_ENABLE_FFI=$(ENABLE_FFI)
  shell.o: CPPFLAGS += -DS2_ENABLE_FFI=$(ENABLE_FFI)
  S2_CLIENT_LDFLAGS += -lavcall -lcallback
  ENABLE_FFI_IMPORT ?= 0
  ifneq (0,$(ENABLE_FFI_IMPORT))
    $(info Enabling FFI Import)
    s2_ffi.o: CPPFLAGS += -DS2_ENABLE_FFI_IMPORT=$(ENABLE_FFI_IMPORT)
  endif
  S2_SRC += s2_ffi.c
  S2_OBJ += s2_ffi.o
endif
########################################
