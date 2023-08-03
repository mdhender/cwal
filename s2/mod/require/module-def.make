#########################################################################
# A module which compiles require.s2 into C code for import via s2's
# module loader.

############################################################
MODULE.NAME := require
MODULE.OBJ := $(MODULE.NAME).o
MODULE.TESTS := test.s2
MODULE.SCRIPT.s2 := $(DIR.s2)/require.d/$(MODULE.NAME).s2
# Configuration for the up-coming $(call)...
#$(MODULE.NAME).stripComments := 1
MODULE.require.stripComments := 1
MODULE.require.minify := 1
#include ../module.make
