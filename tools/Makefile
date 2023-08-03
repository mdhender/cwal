#!/usr/bin/make -f

all:
include ../auto.make

########################################################################
# Bins...
tochararray.BIN.OBJECTS := tochararray.o
#tochararray.BIN.LDFLAGS := $(S2_CLIENT_LDFLAGS)
$(eval $(call ShakeNMake.CALL.RULES.BINS,tochararray))
all: $(tochararray.BIN)

stripcomments.BIN.OBJECTS := stripcomments.o
#stripcomments.BIN.LDFLAGS := $(S2_CLIENT_LDFLAGS)
$(eval $(call ShakeNMake.CALL.RULES.BINS,stripcomments))
all: $(stripcomments.BIN)


minihash.BIN.OBJECTS := minihash.o
$(minihash.BIN.OBJECTS): CFLAGS+=-std=c99
#minihash.BIN.LDFLAGS := $(S2_CLIENT_LDFLAGS)
$(eval $(call ShakeNMake.CALL.RULES.BINS,minihash))
all: $(minihash.BIN)
