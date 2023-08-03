#!/bin/sh
# s2sh wrapper script. Configure it for the local
# system and drop it in your PATH somewhere...
: ${S2_HOME:="$HOME/fossil/cwal/s2"}
: ${S2SH_INIT_SCRIPT:="${S2_HOME}/s2sh.s2"}
: ${S2SH_HISTORY_FILE:="${S2_HOME}/s2sh.history"}
: ${S2_IMPORT_PATH:=".:${S2_HOME}"}
: ${S2_MODULE_PATH:=".:${S2_HOME}/mod"}
: ${S2_REQUIRE_PATH:="."}
for rp in "${PWD}/require.d" "${S2_HOME}/require.d"; do
    if [ -d "${rp}" ]; then
        S2_REQUIRE_PATH="${S2_REQUIRE_PATH}:${rp}"
    fi
done
export S2_HOME S2_IMPORT_PATH \
       S2_MODULE_PATH S2_REQUIRE_PATH \
       S2SH_INIT_SCRIPT S2SH_HISTORY_FILE
exec "${S2_HOME}/s2sh" "$@"
