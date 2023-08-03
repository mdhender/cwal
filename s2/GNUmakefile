s2:
clean:
	$(MAKE) -C .. s2 clean-s2

THIS_MAKEFILE := $(word $(words $(MAKEFILE_LIST)), $(MAKEFILE_LIST))

s2sh.BIN := s2sh
s2sh2.BIN := s2sh2
########################################################################
# Unit test stuff...
UNIT_SCRIPT_LIST := $(sort $(subst ./,,$(wildcard unit/???-???-*.s2)))
$(UNIT_SCRIPT_LIST):
S2SH.SHELL.FLAGS ?= --a -S
# -w
UNIT_RUN_CMD = ./$(s2sh.BIN) $(S2SH.SHELL.FLAGS)
UNIT_MEGA.S2 := UNIT.s2
UNIT_MEGA2.S2 := UNIT-import.s2
UNIT_GENERATED := $(UNIT_MEGA.S2) $(UNIT_MEGA2.S2)
CLEAN_FILES += $(UNIT_GENERATED)
$(UNIT_MEGA.S2): $(UNIT_SCRIPT_LIST) $(s2sh.BIN) $(THIS_MAKEFILE)
	@echo "Generating $@..."
	@{ \
		echo "pragma(exception-stacktrace false);"; \
		for i in $(UNIT_SCRIPT_LIST); do \
			echo "scope {/* begin file: $$i */"; \
			cat $$i; \
			echo "/* end file: $$i */;;}"; \
			echo ""; \
		done; \
	} > $@
$(UNIT_MEGA2.S2): $(UNIT_SCRIPT_LIST) $(s2sh.BIN) $(THIS_MAKEFILE)
	@echo "Generating $@..."
	@{ \
		echo "pragma(exception-stacktrace false);"; \
		for i in $(UNIT_SCRIPT_LIST); do \
			echo "import('$$i');"; \
		done; \
	} > $@

.PHONY: unit
unit: $(UNIT_GENERATED)
	@for i in $(UNIT_SCRIPT_LIST) $(UNIT_GENERATED); do \
		cmd="$(UNIT_RUN_CMD) -f $$i"; \
		echo "****************************** Script [$$i]"; \
		echo $$cmd; $$cmd || exit $$?; \
		echo "****************************** Done [$$i]"; \
	done
	@echo "Done running through unit test scripts."
unit2: S2SH.SHELL.FLAGS:=-I -nosi
unit2: s2sh.BIN:=$(s2sh2.BIN)
unit2: $(s2sh2.BIN) unit
.PHONY: unit2 unit-r unit2-r
unit-r: S2SH.SHELL.FLAGS:=--a --R -C -S
unit2-r: S2SH.SHELL.FLAGS:=-I -norv -rc -si
unit2-r: s2sh.BIN:=$(s2sh2.BIN)
unit2-r: $(s2sh2.BIN) 
unit-r unit2-r: unit
.PHONY: unit-s unit2-s
unit-s: S2SH.SHELL.FLAGS:=--a --S -R -C
unit2-s: S2SH.SHELL.FLAGS:=-I -nosi -rv -rc
unit2-s: s2sh.BIN:=$(s2sh2.BIN)
unit2-s: $(s2sh2.BIN) 
unit2-s unit-s: unit
.PHONY: unit-c unit2-c
unit-c: S2SH.SHELL.FLAGS:=--a -S -R --C
unit2-c: S2SH.SHELL.FLAGS:=-I -si -rv -norc
unit2-c: s2sh.BIN:=$(s2sh2.BIN)
unit2-c: $(s2sh2.BIN) 
unit-c unit2-c: unit
.PHONY: unit-rc unit2-rc
unit-rc: S2SH.SHELL.FLAGS:=--a --R --C -S
unit2-rc: S2SH.SHELL.FLAGS:=-I -norv -norc -si
unit2-rc: s2sh.BIN:=$(s2sh2.BIN)
unit2-rc: $(s2sh2.BIN) 
unit-rc unit2-rc: unit
.PHONY: unit-rs unit2-rs
unit-rs: S2SH.SHELL.FLAGS:=--a --R --S -C
unit2-rs: S2SH.SHELL.FLAGS:=-I -norv -nosi -rc
unit2-rs: s2sh.BIN:=$(s2sh2.BIN)
unit2-r2: $(s2sh2.BIN) 
unit-rs unit2-rs: unit
.PHONY: unit-sc unit2-sc
unit-sc: S2SH.SHELL.FLAGS:=--a -R --S --C
unit2-sc: S2SH.SHELL.FLAGS:=-I -rv -nosi -norc
unit2-sc: s2sh.BIN:=$(s2sh2.BIN)
unit2-sc: $(s2sh2.BIN) 
unit-sc unit2-sc: unit
.PHONY: unit-rsc unit2-rsc
unit-rsc: S2SH.SHELL.FLAGS:=--a --R --C --S
unit2-rsc: S2SH.SHELL.FLAGS:=-I -norv -norc -nosi
unit2-rsc: s2sh.BIN:=$(s2sh2.BIN)
unit2-rsc: $(s2sh2.BIN) 
unit-rsc unit2-rsc: unit
# This level of indirections breaks the bash code syntax
# in the unit target, and i've no clue why.
# Writing out the equivalent make code by hand works fine.
#define UNIT2_FU
#$(1): S2SH.SHELL.FLAGS:=$(2)
#$(1): s2sh.BIN:=$(s2sh2.BIN)
#.PHONY: $(1)
#endef
#$(call UNIT2_FU,unit2,-I +si)
#$(call UNIT2_FU,unit2-rsc,-I +rv +rc +si)

UNIT_TARGET_LIST := unit unit-r unit-s unit-c unit-rs unit-rc unit-sc unit-rsc

units:
	@echo UNIT_TARGET_LIST=$(UNIT_TARGET_LIST)
	@for i in $(UNIT_TARGET_LIST) ; do \
		echo "Making $$i ..."; \
		$(MAKE) $$i || exit $$?; \
	done
units2: UNIT_TARGET_LIST:=$(subst unit,unit2,$(UNIT_TARGET_LIST))
units2: s2sh.BIN:=$(s2sh2.BIN)
units2: units
.PHONY: units2
include vg.make

%:
	$(MAKE) -C .. s2 $@

