#!/usr/bin/bash
########################################################################
# Creates makefile code for whcl external modules. This beast used to
# be implemented directly in make and it was _only slightly_ less
# readable :/.
#
# Things to remember:
#
# - It will be called from the top of the source tree, so the paths
#   need to take that into account.
#
# - It relies on certain vars/features from the higher-level
#   makefiles.
#
# - It will be imported into the top-level makefile and must
#   avoid name collisions, in particular with like-named modules
#   in s2.
########################################################################

if [ -f whcl.c ]; then
   # Workaround for running this directly from the whcl dir during
   # development.
   cd ..
fi

dWhcl=whcl
dMod=$dWhcl/mod
modDefs=$(ls -1 $dMod/*/module-def.make | sort)
declare -a whclBins=(whclsh test)
declare -a modDirs
declare -a modNames
declare -a modStatic
for m in $modDefs; do
    d=${m%%/module-def.make}
    modDirs+=($d)
    n=${d#*/mod/}
    modNames+=($n)
    if [ -f $d/static_module.c ] || [ -f $d/static_module.cpp ]; then
        modStatic+=($n)
    fi
done

#echo modDirs=${modDirs[@]}
#echo modNames=${modNames[@]}
#echo modStatic=${modStatic[@]}

function emitStartCode() {
    cat <<EOF
################################################################################
# Set up external modules...
WHCL_MODULE_NAME_LIST :=
WHCL.MODULE_INIT_LIST :=
WHCL.INIT_M_CALLS :=
empty:=# intentionally empty
WHCL.CLIENT_BINS.MODULE_OBJ :=
WHCL.CLIENT_BINS.MODULE_LDFLAGS :=
WHCL.MODULE_SRC_ALL :=
WHCL.TEST-BANNER := ============================================================
whclsh.MOD-TEST-FLAGS :=
ifneq (1,\$(MAKING_CLEAN))
  whcl: whcl-mod
endif
# End of shared bootstrapping for external modules.
################################################################################
EOF
}

function emitClearModuleVars(){
    cat <<EOF
# Clear vars which are part of the module-def.make interface...
module.name :=
module.obj :=
module.tests :=
module.LDFLAGS :=
module.CFLAGS :=
module.CPPFLAGS :=
module.enabled :=
module.builtin :=
module.script.whcl :=
module.cname :=
EOF
}

# Import MODULE/module-def.make and set up the build based on it...
function emitModRules() {
    local mod=$1
    local MOD=WHCL.module.${mod}
    cat <<EOF
################################################################################
# Start of module [${mod}]
WHCL_MODULE_NAME_LIST += $mod
EOF
    emitClearModuleVars
    cat <<EOF
########################################
# Set up this module's numerous vars...
${MOD}.dir := \$(DIR.whcl-mod)/${mod}
${MOD}.def := \$(${MOD}.dir)/module-def.make
\$(eval include \$(${MOD}.def))
${MOD}.name := \$(if \$(module.name),\$(module.name),${mod})
${MOD}.obj := \$(patsubst %,\$(DIR.whcl-mod)/${mod}/%,\$(module.obj))
# Module may have EITHER static_module.c OR static_module.cpp, not both
${MOD}.static.c := \$(wildcard \$(DIR.whcl-mod)/${mod}/static_module.c)
${MOD}.static.cpp := \$(wildcard \$(DIR.whcl-mod)/${mod}/static_module.cpp)
${MOD}.static.src := \$(word 1,\$(${MOD}.static.c) \$(${MOD}.static.cpp))
${MOD}.static.obj := \$(if \$(${MOD}.static.src),\
    \$(${MOD}.dir)/static_module.o,\$(empty))
${MOD}.ALLOBJ := \$(${MOD}.obj) \$(${MOD}.static.obj)
${MOD}.LDFLAGS := \$(module.LDFLAGS)
${MOD}.CFLAGS := \$(module.CFLAGS)
${MOD}.CPPFLAGS := \$(module.CPPFLAGS)
${MOD}.script.whcl := \$(if \$(module.script.whcl),\$(wildcard \$(${MOD}.dir)/\$(module.script.whcl)),)
${MOD}.tests := \$(module.tests)
${MOD}.cname := \$(if \$(module.cname),\$(module.cname),\$(${MOD}.name))
${MOD}.enabled := \$(if \$(module.enabled),\$(module.enabled),0)
${MOD}.builtin := \$(if \$(module.builtin),\$(module.builtin),0)
\$(${MOD}.ALLOBJ): CPPFLAGS+=\$(${MOD}.CPPFLAGS) \\
  -I\$(DIR.whcl-mod)/${mod} \\
  \$(PROJECT_INCLUDES)
\$(${MOD}.ALLOBJ): CFLAGS+=\$(${MOD}.CFLAGS)
\$(${MOD}.obj): CPPFLAGS+=-DWHCL_MODULE_STANDALONE
whcl-mod-${mod}: \$(${MOD}.static.obj) \$(${MOD}.obj)
########################################
# Cleanup and deps...
\$(eval \$(call ShakeNMake.CALL.CLEAN-SET,whcl-mod-${mod}))
CLEAN.whcl-mod-${mod} += \$(${MOD}.ALLOBJ)
clean-whcl-mod: clean-whcl-mod-${mod}
distclean-whcl-mod: distclean-whcl-mod-${mod}
DEPS.whcl-mod-${mod}.SRC += \$(wildcard \$(${MOD}.dir)/*.[ch] \$(${MOD}.dir)/*.[ch]pp)
WHCL.MODULE_SRC_ALL += \$(DEPS.whcl-mod-${mod}.SRC)
DEPS.whcl-mod-${mod}.INCDIRS := \$(${MOD}.dir)
\$(eval \$(call ADD_C_DEPS,whcl-mod-${mod},\$(DEPS.whcl-mod-${mod}.SRC)))
########################################
# .whcl-to-.c conversion...
ifneq (,\$(${MOD}.script.whcl))
  \$(eval \$(call WHCL_MODULE_WHCLTOC,${mod}))
endif
########################################
# DLL and static module init...
#\$(info ${MOD}.enabled = \$(${MOD}.enabled))
ifeq (1,\$(${MOD}.enabled))
  ifeq (1,\$(${MOD}.builtin))
    ifeq (,\$(${MOD}.static.obj))
      \$(error Module ${mod} is marked as builtin but has no static module bits.)
    endif
    \$(${MOD}.static.obj): CPPFLAGS+=-DWHCL_MODULE_STATIC
    WHCL.CLIENT_BINS.MODULE_OBJ += \$(${MOD}.static.obj)
    WHCL.CLIENT_BINS.MODULE_LDFLAGS += \$(${MOD}.LDFLAGS)
    WHCL.INIT_M_CALLS +=M2(\$(${MOD}.cname),${mod})
    \$(WHCL_CLIENT_BINS): \$(${MOD}.static.obj)
  else
    \$(${MOD}.dir)/${mod}.DLL.OBJECTS := \$(${MOD}.obj)
    \$(${MOD}.dir)/${mod}.DLL.LDFLAGS := \$(${MOD}.LDFLAGS)
    \$(eval \$(call ShakeNMake.EVAL.RULES.DLL,\$(${MOD}.dir)/${mod}))
    whcl-mod-${mod}: \$(\$(${MOD}.dir)/${mod}.DLL)
    CLEAN.whcl-mod-${mod} += \$(\$(${MOD}.dir)/${mod}.DLL)
  endif
  whcl-mod: whcl-mod-${mod}
endif
########################################
# Module test scripts...
ifeq (1,\$(${MOD}.enabled))
ifeq (,\$(${MOD}.tests))
  whcl-mod-test-${mod}:
	@echo "Module [${mod}] does not define module.tests."
else
whcl-mod-test-${mod}: whcl-mod-${mod} \$(whclsh.BIN)
	@echo "\$(WHCL.TEST-BANNER)"; \\
	echo "Running module tests for [${mod}]..."; \\
	cd \$(${MOD}.dir); \\
	for x in \$(${MOD}.tests); do \\
        echo \$\$x; \\
		\$(realpath \$(whclsh.BIN)) \$(whclsh.MOD-TEST-FLAGS) \\
			-o \$\$x._out -f \$\$x || exit; \\
	done; \\
	cd - >/dev/null; \\
	echo "\$(WHCL.TEST-BANNER)"
endif
whcl-mod-test: whcl-mod-test-${mod}
else
whcl-mod-test-${mod}:
	@echo "Module [${mod}] is not enabled."
endif
EOF
    emitClearModuleVars
    cat <<EOF
# End of module [${mod}]. Whew.
################################################################################
EOF
}


function emitEndCode(){
cat <<EOF
################################################################################
# Finalize the module bits...
EOF
    for b in ${whclBins[@]}; do
        cat <<EOF
\$(DIR.whcl)/${b}.BIN.OBJECTS += \$(WHCL.CLIENT_BINS.MODULE_OBJ)
\$(DIR.whcl)/${b}.BIN.LDFLAGS += \$(WHCL.CLIENT_BINS.MODULE_LDFLAGS)
EOF
    done
cat <<EOF
\$(DIR.whcl)/mod.o: CPPFLAGS+=-DWHCL_STATIC_MODULE_INITS="\$(WHCL.INIT_M_CALLS)"
\$(DIR.whcl)/mod.o: .make-whcl-mod.make
EOF
}

emitStartCode
for m in ${modNames[@]}; do
    emitModRules $m
done
emitEndCode
