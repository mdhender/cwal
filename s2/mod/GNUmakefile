TOPDIR := ../../
default:
	$(MAKE) -C $(TOPDIR) s2-mod
clean:
	$(MAKE) -C $(TOPDIR) clean-s2-mod
distclean:
	$(MAKE) -C $(TOPDIR) distclean-s2-mod

unit test:
	@dirs=$$(grep -l MODULE.TESTS */module-def.make | cut -d/ -f1); \
	for d in $$dirs; do \
		ls $$d/*.o >/dev/null 2>&1 || continue; \
		$(MAKE) --no-print-directory -C $(TOPDIR) s2-mod-test-$$d || exit; \
	done

%:
	$(MAKE) -C $(TOPDIR) $@
