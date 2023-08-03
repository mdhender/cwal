#########################################################################
# A module which compiles s2 script into C code for import via s2's
# module loader.
MODULE.NAME := iterate
MODULE.OBJ := $(MODULE.NAME).o
MODULE.TESTS := test.s2
MODULE.SCRIPT.s2 := ../../require.d/$(MODULE.NAME).s2
$(MODULE.NAME).minify = 1
$(MODULE.NAME).stripComments = 0
include ../module.make
