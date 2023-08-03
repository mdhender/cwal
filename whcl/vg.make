# We keep these in a separate file to avoid a rebuild when changing
# these flags. Damn... some automatic rules foil us here.
########################################################################
# Valgrind sanity checks...
VG := $(shell which valgrind)
ifneq (,$(VG))
VG_REPORT := VG.report.csv
VG_FLAGS ?= --leak-check=full -v --show-reachable=yes --track-origins=yes
#SCRIPT_LIST := $(shell ls -1 test-[0-9]*.s2 | sort)
# Whether or not to collect massif-based stats...
RUN_MASSIF := 1
.PHONY: vg vgall
# Reminder: don't use -A b/c it breaks output buffering tests!
VG_SHELL_FLAGS=

ifeq (0,1)
  VG_SHELL_FLAGS += -fst
endif

VG_SHELL_FLAGS:=-rv -si -rc
VG_UNIT_RUN_CMD = $(whclsh.bin) $(VG_SHELL_FLAGS) $(whclsh.flags.common)
VG_UNIT_SCRIPT_LIST := $(sort $(UNIT_SCRIPT_LIST)) $(UNIT_GENERATED)
VG_REPORT_COLUMNS := script,shellFlags,allocs,frees,totalMemory,peakMemUsage,peakMemUsageUnit

vg: $(whclsh.bin) $(UNIT_GENERATED)
	@echo "Running: $(VG) $(VG_FLAGS) $(VG_UNIT_RUN_CMD) ..."; \
	export LD_LIBRARY_PATH="$(TOP_SRCDIR):$${LD_LIBRARY_PATH}"; \
	massif_tmp=tmp.massif; \
	for i in $(VG_UNIT_SCRIPT_LIST); do \
		vgout=$$i.vg; \
		xtraargs="-o $$i._out -f $$i"; \
		cmd="$(VG) $(VG_FLAGS) $(VG_UNIT_RUN_CMD) $$xtraargs"; \
		echo -n "**** Valgrinding [$$i]: "; \
		$$cmd 2>&1 | tee $$vgout | grep 'total heap usage' || exit $$?; \
		grep 'ERROR SUMMARY' $$vgout | grep -v ' 0 errors' && echo "See $$vgout!"; \
		vgout=$$i.massif; \
		cmd="$(VG) --tool=massif --time-unit=B --massif-out-file=$$vgout --heap-admin=0  $(VG_UNIT_RUN_CMD) $$xtraargs"; \
		test x1 = x$(RUN_MASSIF) || continue; \
		echo -n "**** Massifing [$$i]: "; \
		$$cmd > $$massif_tmp 2>&1 || exit $$?; \
		echo -n "==> $$vgout Peak RAM: "; \
	    ms_print $$vgout | perl -n -e 'if(m/^(\s+)([KM]B)$$/){my $$x=$$2; $$_=<>; m/^(\d[^^]+)/; print $$1." ".$$x; exit 0;}'; \
		echo; \
	done; \
	rm -f $$massif_tmp
	@test x1 = x$(RUN_MASSIF) || exit 0; \
	echo "Done running through whcl scripts. Collecting stats..."; \
	echo '$(VG_REPORT_COLUMNS)' > $(VG_REPORT); \
	for i in $(sort $(patsubst %.whcl,%.whcl.vg,$(VG_UNIT_SCRIPT_LIST))); do \
		base=$${i%%.vg}; \
		echo -n "$$base,$(VG_SHELL_FLAGS) $(whclsh.flags.common),"; \
		grep 'total heap usage' $$i | sed -e 's/,//g' -e 's/^\.\///' | awk '{printf "%d,%d,%d,",$$5,$$7,$$9}'; \
		ms_print $${base}.massif | perl -n -e 'if(m/^(\s+)([KM]B)$$/){my $$x=$$2; $$_=<>; m/^(\d[^^]+)/; print $$1.",".$$x; exit 0;}'; \
		echo; \
	done >> $(VG_REPORT); \
	rm -f unit/ms_print.tmp.* ms_print.tmp.*; \
	echo "Stats are in $(VG_REPORT):"; \
	tr ',' '\t' < $(VG_REPORT)
# The vgg* targets elide run only the $(UNIT_GENERATED) tests.

# 'vg' proxies which tweak various s2/cwal-level optimizations...
# vgr: no value recycling
.PHONY: vgr
vgr: VG_SHELL_FLAGS:=-norv -si -rc
vgr: VG_REPORT:=VG.report-r.csv
vgr: vg

# vgc: no chunk recycling
.PHONY: vgc
vgc: VG_SHELL_FLAGS:=-rv -si -norc
vgc: VG_REPORT:=VG.report-c.csv
vgc: vg

# vgs: no string interning
.PHONY: vgs
vgs: VG_SHELL_FLAGS:=-nosi -rv -rc
vgs: VG_REPORT:=VG.report-s.csv
vgs: vg

# vgrc: no value/chunk recycling
.PHONY: vgrc
vgrc: VG_SHELL_FLAGS:=-norv -norc -si
vgrc: VG_REPORT:=VG.report-rc.csv
vgrc: vg

# vgsrs: no string interning, no value recycling
.PHONY: vgrs
vgrs: VG_SHELL_FLAGS:=-norv -rc -nosi
vgrs: VG_REPORT:=VG.report-rs.csv
vgrs: vg

# vgsc: no string interning, no chunk recycling
.PHONY: vgsc
vgsc: VG_SHELL_FLAGS:=-rv -norc -nosi
vgsc: VG_REPORT:=VG.report-rs.csv
vgsc: vg

# vgsrsc: no string interning, no value/chunk recycling
.PHONY: vgrsc
vgrsc: VG_SHELL_FLAGS:=-norv -norc -nosi
vgrsc: VG_REPORT:=VG.report-rsc.csv
vgrsc: vg

VG_REPORT_MEGA := VG.report-mega.csv
VG_ALL_TARGETS := $(MAKE) vg \
	&& $(MAKE) vgr \
	&& $(MAKE) vgs \
	&& $(MAKE) vgc \
	&& $(MAKE) vgrc \
	&& $(MAKE) vgrs \
	&& $(MAKE) vgrsc
# disabled:
.PHONY: vgall
VG_ALL_RUN := true
vgall: $(whclsh.bin) $(UNIT_GENERATED)
	@start=$$(date); \
	echo "Start time: $$start"; \
	if $(VG_ALL_RUN); then $(VG_ALL_TARGETS) || exit; fi; \
	echo "Run time: $$start - $$(date)"; \
	rm -f $(VG_REPORT_MEGA); \
	{ \
	echo '$(VG_REPORT_COLUMNS)'; \
	for i in VG.report*.csv; do \
		test -s $$i || continue; \
		echo $$i | grep t-mega >/dev/null && continue; \
		sed -e 1d $$i; \
	done; }  | cut -d, -f1,2,4,5,6,7 | tr ',' '\t' > $(VG_REPORT_MEGA); \
	echo "Consolidated valgrind report is in [$(VG_REPORT_MEGA)]."
endif
#/$(VG)
########################################################################
