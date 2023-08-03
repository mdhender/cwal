# To be included only from GNUmakefiles living exactly one directory
# down from the top directory.
SUB := $(notdir $(CURDIR))
ifeq (,$(wildcard ../make-$(SUB).make))
  $(error Expecting subdir name to match a make-DIRNAME.make file.)
endif
$(SUB):
	@$(MAKE) --no-print-directory -C .. $(SUB)
clean:
	@$(MAKE) --no-print-directory -C .. clean-$(SUB)
%:
	@$(MAKE) --no-print-directory -C .. $@
