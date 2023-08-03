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
.PHONY: vg vgall vg2 vg2all
# Reminder: don't use -A b/c it breaks output buffering tests!
VG_SHELL_FLAGS=

ifeq (0,1)
# on my ARM box (ODroid U3), valgrind reports what i am claiming is
# one of those rare false-positive regarding use of initialized stack
# values being used for conditionals. It can't report the location,
# only that it's in s2sh somewhere, but i've narrowed it down to only
# being reported if cwal_dump_allocation_metrics() is called, which -v
# triggers. -v isn't much use for the valgrind tests, anyway, unless
# we want to go look at the (-m -v) dumps afterwards (which i don't
# remember having done in years).
  VG_SHELL_FLAGS += -v
endif

ifeq (0,1)
  VG_SHELL_FLAGS += -fst
endif


VG_UNIT_RUN_CMD = ./$(s2sh.BIN) $(VG_SHELL_FLAGS)
VG_UNIT_SCRIPT_LIST := $(sort $(UNIT_SCRIPT_LIST)) $(UNIT_GENERATED)
vg: $(s2sh.BIN) $(UNIT_GENERATED)
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
	echo "Done running through s2 scripts. Collecting stats..."; \
	echo 'script,allocs,frees,totalMemory,peakMemUsage,peakMemUsageUnit' > $(VG_REPORT); \
	for i in $(sort $(patsubst %.s2,%.s2.vg,$(VG_UNIT_SCRIPT_LIST))); do \
		base=$${i%%.vg}; \
		echo -n "$$base,"; \
		grep 'total heap usage' $$i | sed -e 's/,//g' -e 's/^\.\///' | awk '{printf "%d,%d,%d,",$$5,$$7,$$9}'; \
		ms_print $${base}.massif | perl -n -e 'if(m/^(\s+)([KM]B)$$/){my $$x=$$2; $$_=<>; m/^(\d[^^]+)/; print $$1.",".$$x; exit 0;}'; \
		echo; \
	done >> $(VG_REPORT); \
	rm -f unit/ms_print.tmp.* ms_print.tmp.*; \
	echo "Stats are in $(VG_REPORT):"; \
	tr ',' '\t' < $(VG_REPORT)
# The vgg* targets elide run only the $(UNIT_GENERATED) tests.
vgg: VG_UNIT_SCRIPT_LIST:=$(UNIT_GENERATED)
vgg: vg
vg2: VG_SHELL_FLAGS:=+a -rv -si -rc
vg2: s2sh.BIN:=$(s2sh2.BIN)
vg2: VG_REPORT:=VG.report-2.csv
vg2: vg
vgg2: VG_SHELL_FLAGS:=+a -rv -si -rc
vgg2: s2sh.BIN:=$(s2sh2.BIN)
vgg2: VG_UNIT_SCRIPT_LIST:=$(UNIT_GENERATED)
vgg2: VG_REPORT:=VG.report-g2.csv
vgg2: vg

CLEAN_FILES += $(wildcard \
	$(patsubst %.s2,%.s2.vg,$(UNIT_GENERATED)) \
	$(patsubst %,unit/%,*.vg *._out *.massif *.db *~ *.uncompressed *.z *.zip *.2) \
	)
# 'vg' proxies which tweak various s2/cwal-level optimizations...
# vgr: no value recycling
.PHONY: vgr vggr vg2r vgg2r
vgr: VG_SHELL_FLAGS:=--a --R -S -C
vgr: VG_REPORT:=VG.report-r.csv
vgr: vg
vg2r: VG_SHELL_FLAGS:=-I -norv -si -rc
vg2r: s2sh.BIN:=$(s2sh2.BIN)
vg2r: VG_REPORT:=VG.report-2r.csv
vg2r: vg
vggr: VG_UNIT_SCRIPT_LIST:=$(UNIT_GENERATED)
vggr: vgr
vgg2r: VG_SHELL_FLAGS:=-I -norv -si -rc
vgg2r: s2sh.BIN:=$(s2sh2.BIN)
vgg2r: VG_UNIT_SCRIPT_LIST:=$(UNIT_GENERATED)
vgg2r: VG_REPORT:=VG.report-g2r.csv
vgg2r: vg

# vgc: no chunk recycling
.PHONY: vgc vggc vg2c vgg2c
vgc: VG_SHELL_FLAGS:=--a -R -S --C
vgc: VG_REPORT:=VG.report-c.csv
vgc: vg
vg2c: VG_SHELL_FLAGS:=-I -rv -si -norc
vg2c: s2sh.BIN:=$(s2sh2.BIN)
vg2c: VG_REPORT:=VG.report-g2c.csv
vg2c: vg
vggc: VG_UNIT_SCRIPT_LIST:=$(UNIT_GENERATED)
vggc: vg
vgg2c: VG_SHELL_FLAGS:=-I -rv -si -norc
vgg2c: s2sh.BIN:=$(s2sh2.BIN)
vgg2c: VG_UNIT_SCRIPT_LIST:=$(UNIT_GENERATED)
vgg2c: VG_REPORT:=VG.report-2c.csv
vgg2c: vg

# vgs: no string interning
.PHONY: vgs vggs vg2s vgg2s
vgs: VG_SHELL_FLAGS:=--a --S -R -C
vgs: VG_REPORT:=VG.report-s.csv
vgs: vg
vg2s: VG_SHELL_FLAGS:=-I -nosi -rv -rc
vg2s: s2sh.BIN:=$(s2sh2.BIN)
vg2s: VG_REPORT:=VG.report-2s.csv
vg2s: vg
vggs: VG_UNIT_SCRIPT_LIST:=$(UNIT_GENERATED)
vggs: vgs
vgg2s: VG_SHELL_FLAGS:=-I -nosi -rv -rc
vgg2s: s2sh.BIN:=$(s2sh2.BIN)
vgg2s: VG_UNIT_SCRIPT_LIST:=$(UNIT_GENERATED)
vgg2s: VG_REPORT:=VG.report-g2s.csv
vgg2s: vg

# vgrc: no value/chunk recycling
.PHONY: vgrc vggrc vg2rc vgg2rc
vgrc: VG_SHELL_FLAGS:=--a --R --C -S
vgrc: VG_REPORT:=VG.report-rc.csv
vgrc: vg
vg2rc: VG_SHELL_FLAGS:=-I -norv -norc -si
vg2rc: s2sh.BIN:=$(s2sh2.BIN)
vg2rc: VG_REPORT:=VG.report-2rc.csv
vg2rc: vg
vggrc: VG_UNIT_SCRIPT_LIST:=$(UNIT_GENERATED)
vggrc: vgrc
vgg2rc: VG_SHELL_FLAGS:=-I -norv -norc -si
vgg2rc: s2sh.BIN:=$(s2sh2.BIN)
vgg2rc: VG_REPORT:=VG.report-g2rc.csv
vgg2rc: VG_UNIT_SCRIPT_LIST:=$(UNIT_GENERATED)
vgg2rc: vg
# vgsrs: no string interning, no value recycling
.PHONY: vgrs vggrs vg2rs vgg2rs
vgrs: VG_SHELL_FLAGS:=--a --R --S -C
vgrs: VG_REPORT:=VG.report-rs.csv
vgrs: vg
vg2rs: VG_SHELL_FLAGS:=-I -norv -rc -nosi
vg2rs: s2sh.BIN:=$(s2sh2.BIN)
vg2rs: VG_REPORT:=VG.report-2rs.csv
vg2rs: vg
vggrs: VG_UNIT_SCRIPT_LIST:=$(UNIT_GENERATED)
vggrs: vgrs
vgg2rs: VG_SHELL_FLAGS:=-I -norv -rc -nosi
vgg2rs: s2sh.BIN:=$(s2sh2.BIN)
vgg2rs: VG_REPORT:=VG.report-g2rs.csv
vgg2rs: VG_UNIT_SCRIPT_LIST:=$(UNIT_GENERATED)
vgg2rs: vg
# vgsc: no string interning, no chunk recycling
.PHONY: vgsc vggsc vg2rc vgg2rc
vgsc: VG_SHELL_FLAGS:=--a -R --S --C
vgsc: VG_REPORT:=VG.report-rs.csv
vgsc: vg
vg2sc: VG_SHELL_FLAGS:=-I -rv -norc -nosi
vg2sc: s2sh.BIN:=$(s2sh2.BIN)
vg2sc: VG_REPORT:=VG.report-2sc.csv
vg2sc: vg
vggsc: VG_UNIT_SCRIPT_LIST:=$(UNIT_GENERATED)
vggsc: vgsc
vgg2sc: VG_SHELL_FLAGS:=-I -rv -norc -nosi
vgg2sc: s2sh.BIN:=$(s2sh2.BIN)
vgg2sc: VG_REPORT:=VG.report-g2sc.csv
vgg2sc: VG_UNIT_SCRIPT_LIST:=$(UNIT_GENERATED)
vgg2sc: vg
# vgsrsc: no string interning, no value/chunk recycling
.PHONY: vgrsc vggrsc vg2rsc vgg2rsc
vgrsc: VG_SHELL_FLAGS:=--a --R --C --S
vgrsc: VG_REPORT:=VG.report-rsc.csv
vgrsc: vg
vg2rsc: VG_SHELL_FLAGS:=-I -norv -norc -nosi
vg2rsc: s2sh.BIN:=$(s2sh2.BIN)
vg2rsc: VG_REPORT:=VG.report-2rsc.csv
vg2rsc: vg
vggrsc: VG_UNIT_SCRIPT_LIST:=$(UNIT_GENERATED)
vggrsc: vgrsc
vgg2rsc: VG_SHELL_FLAGS:=-I -norv -norc -nosi
vgg2rsc: s2sh.BIN:=$(s2sh2.BIN)
vgg2rsc: VG_UNIT_SCRIPT_LIST:=$(UNIT_GENERATED)
vgg2rsc: VG_REPORT:=VG.report-g2rsc.csv
vgg2rsc: vg

VG_REPORT_MEGA := VG.report-mega.csv
CLEAN_FILES += $(VG_REPORT_MEGA)
VG_ALL_TARGETS := $(MAKE) vg \
	&& $(MAKE) vgr \
	&& $(MAKE) vgs \
	&& $(MAKE) vgc \
	&& $(MAKE) vgrc \
	&& $(MAKE) vgrs \
	&& $(MAKE) vgrsc
# disabled:
VG_ALL_TARGETS += && $(MAKE) vgs && $(MAKE) vgrs && $(MAKE) vgrsc
.PHONY: vggall vg2all vgg2all
vgall: $(s2sh.BIN) $(UNIT_GENERATED)
	@start=$$(date); \
	echo "Start time: $$start"; \
	$(VG_ALL_TARGETS) || exit; \
	echo "Run time: $$start - $$(date)"; \
	rm -f $(VG_REPORT_MEGA); \
	for i in VG.report*.csv; do \
		test -s $$i || continue; \
		echo $$i | grep t-mega >/dev/null && continue; \
		echo "Report: $$i ******************************"; \
		cut -d, -f1,2,4,5 $$i | tr ',' '\t'; \
	done > $(VG_REPORT_MEGA); \
	echo "Consolidated valgrind report is in [$(VG_REPORT_MEGA)]."
endif
vggall: VG_ALL_TARGETS:=$(patsubst vg%,vgg%,$(VG_ALL_TARGETS))
vggall: vgall
vg2all: VG_ALL_TARGETS:=$(subst vg,vg2,$(VG_ALL_TARGETS))
vg2all: vgall
vgg2all: VG_ALL_TARGETS:=$(subst vg,vgg2,$(VG_ALL_TARGETS))
vgg2all: vgall
#/$(VG)
########################################################################
