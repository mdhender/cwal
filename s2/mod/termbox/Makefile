all:
MODULE.NAME := termbox
include ../../auto.make
# ^^^^^ needed for $(call)-able funcs but we cannot include
# module.make until all our module vars are set up...

# TODO: get rid of this hand-rolled libtermbox lookup and
# use the bits set up by autosetup...
TERMBOX_H := $(call ShakeNMake.CALL.FIND_HEADER,termbox.h)
ifneq (,$(TERMBOX_H))
#$(info termbox.h found: $(TERMBOX_H))
TERMBOX_CPPFLAGS := -I$(dir $(TERMBOX_H))
TERMBOX_LIB := $(call ShakeNMake.CALL.FIND_LIB,libtermbox.a libtermbox.so)
ifneq (,$(TERMBOX_LIB))
  #$(info termbox lib found: $(TERMBOX_LIB))
  TERMBOX_LDFLAGS := -L$(dir $(TERMBOX_LIB)) -ltermbox
  MODULE.OBJ := mod_termbox.o
  MODULE.LDFLAGS += $(TERMBOX_LDFLAGS)
  mod_termbox.o: CPPFLAGS+=$(TERMBOX_CPPFLAGS)
#  mod_termbox.o: CPPFLAGS+=-DS2_AMALGAMATION_BUILD
# Workaround for C++-style comments in termbox.h:
# http://code.google.com/p/termbox/issues/detail?id=7
  mod_termbox.o: CFLAGS+=-std=c99
  include ../module.make
  static_module.o: CPPFLAGS+=$(TERMBOX_CPPFLAGS)
else
  $(info termbox libs not found - not building module.)
endif
else
  $(info termbox.h not found - not building module.)
endif


########################################################################
# C++ bits...
ifneq (,$(CXX))
ifneq (,$(TERMBOX_LIB))
pptest.BIN.OBJECTS := tbpp.o pptest.o
$(pptest.BIN.OBJECTS): CPPFLAGS+=$(TERMBOX_CPPFLAGS)
# $(S2_CLIENT_LDFLAGS) 
pptest.BIN.LDFLAGS := -lstdc++ $(TERMBOX_LDFLAGS)
$(eval $(call ShakeNMake.CALL.RULES.BINS,pptest))
all: $(pptest.BIN.OBJECTS) $(pptest.BIN)
endif
endif


#$(info CPPFLAGS=$(CPPFLAGS))
#$(info CXXFLAGS=$(CXXFLAGS))
#$(info CXX=$(CXX))
#$(info CFLAGS=$(CFLAGS))
