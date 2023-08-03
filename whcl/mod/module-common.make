# To be included from MODULE/module-def.make or MODULE/GNUmakefile.
#include module-def.make
# ^^^ not needed so long as we stick with module.name == module dir name
ifeq (,$(libwhcl.thelib))
module.name ?= $(notdir $(CURDIR))
# When including from <TOP>/make-whcl.make, do nothing, else assume this
# is being included from/via a Makefile in the module's directory. The
# point of this is to enable running "make" and "make clean" directly
# from a module's directory during development and build/clean only
# that dir.
#ifeq (,$(module.name))
#$(error module.name is not set)
#endif
MFLAGS := -C ../../.. --no-print-directory
default:
	$(MAKE) $(MFLAGS) whcl-mod-$(module.name)
clean:
	$(MAKE) $(MFLAGS) clean-whcl-mod-$(module.name)
distclean:
	$(MAKE) $(MFLAGS) distclean-whcl-mod-$(module.name)
unit test:
	$(MAKE) $(MFLAGS) whcl-mod-test-$(module.name)
endif
