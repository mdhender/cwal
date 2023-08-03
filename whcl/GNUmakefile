include ../_subdir.make


whclsh.bin := ./whclsh
$(whclsh.bin):
	$(MAKE) -C .. whcl

BANNER := ============================================================
UNIT_GENERATED := UNITS-all.whcl

UNIT_SCRIPT_LIST := $(sort $(wildcard unit/*.whcl))
UNIT_TESTS := $(UNIT_SCRIPT_LIST) $(UNIT_GENERATED)
ifeq (0,1)
  # 2022-03-07: recursive vacuum is known to trigger various
  # assert()s in cwal's deepest value management bits...
  # 2022-03-22: seems to work since this weekend's scope overhaul,
  # but is not _known_ to be reliable.
  UNIT_VACUUM := pragma vacuum -r
else ifeq (1,1)
  UNIT_VACUUM := pragma vacuum
else
  UNIT_VACUUM :=
endif
$(UNIT_GENERATED): $(UNIT_SCRIPT_LIST) $(MAKEFILE_LIST)
	@{ for f in $(UNIT_SCRIPT_LIST); do \
		echo "# start $$f"; \
		echo 'if {true} {'; cat $$f; \
		echo "$(UNIT_VACUUM)"; \
		echo "}; # end $$f"; \
	done; } > $@; ls -la $@

whclsh.flags.common := -is 1 -iv 5
# ---------------------^^^^^^^^^^^ painfully low sweep/vacuum
# intervals because those reveal value lifetime misuse where high
# values may hide it. That hurts performance badly, though.
whclsh.flags := -rv -rc -si
.PHONY: unit
unit: $(whclsh.bin) $(UNIT_GENERATED)
	@echo $(BANNER); \
	echo "Running tests with whclsh flags: $(whclsh.flags) $(whclsh.flags.common)"; \
	echo $(BANNER); \
	for t in $(UNIT_TESTS); do \
		echo $(whclsh.bin) $(whclsh.flags) $(whclsh.flags.common) $$t; \
		$(whclsh.bin) $(whclsh.flags) $(whclsh.flags.common) $$t || exit; \
	done

.PHONY: unit-rsc
unit-rsc: whclsh.flags:=-norv -norc -nosi
unit-rsc: unit
.PHONY: unit-r
unit-r: whclsh.flags:=-rv -norc -nosi
unit-r: unit
.PHONY: unit-s
unit-s: whclsh.flags:=-norv -norc -si
unit-s: unit
.PHONY: unit-c
unit-c: whclsh.flags:=-norv -rc -nosi
unit-c: unit

.PHONY: units
UNIT_TARGET_LIST := unit unit-rsc unit-r unit-s unit-c
units: 
	@echo UNIT_TARGET_LIST=$(UNIT_TARGET_LIST)
	@for i in $(UNIT_TARGET_LIST) ; do \
		echo "Making $$i ..."; \
		$(MAKE) $$i --no-print-directory || exit $$?; \
	done

include vg.make
