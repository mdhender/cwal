# To be included from MODULE/module-def.make or MODULE/GNUmakefile.
#include module-def.make
# ^^^ not needed so long as we stick with MODULE.NAME == module dir name
ifeq (,$(libs2.thelib))
MODULE.NAME ?= $(notdir $(CURDIR))
# When including from <TOP>/make-s2.make, do nothing, else assume this
# is being included from/via a Makefile in the module's directory. The
# point of this is to enable running "make" and "make clean" directly
# from a module's directory during development and build/clean only
# that dir.
#ifeq (,$(MODULE.NAME))
#$(error MODULE.NAME is not set)
#endif
MFLAGS := -C ../../.. --no-print-directory
default:
	$(MAKE) $(MFLAGS) s2-mod-$(MODULE.NAME)
clean:
	$(MAKE) $(MFLAGS) clean-s2-mod-$(MODULE.NAME)
distclean:
	$(MAKE) $(MFLAGS) distclean-s2-mod-$(MODULE.NAME)
unit test:
	$(MAKE) $(MFLAGS) s2-mod-test-$(MODULE.NAME)
endif
