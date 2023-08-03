# This file is a workaround to enable a clean rebuild of _just_ libcwal.
ifeq (,$(libcwal.c))
#^^^^^ make-cwal.make not yet included
include make-cwal.make
else
default:
endif
