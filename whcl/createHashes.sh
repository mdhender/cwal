#!/usr/bin/bash
########################################################################
# Uses ./kwhasher to generate various keyword/identifier hashes
# for use in whcl. All output goes to stdout in a copy/pastable
# form.
########################################################################

kwh=./kwhasher

if [ ! -x $kwh ]; then
    make whcl/kwhasher || exit
fi

banner='/* Values generated by kwasher.c */'
#echo BANNER=$banner
########################################################################
# Built-in commands...

function doBICs(){
    # words must be the names of whcl__builtins members. Words which
    # collide with C/C++ reserved words must end in _.
    local -a words=(
        affirm alias array assert_ break_ catch_
        concat const_ continue_
        __debug decl decr
        define do_ echo exception exit eval expr
        for_ foreach if_ incr info
        new_ object pragma proc
        return_ set throw_ unset while_ with
    )
    echo "For for pasting into whcl.c:whcl__bic_search():"
    echo; echo "$banner"
    for word in ${words[@]}; do
        a1=$word
        a2=${word//_/}
        # Obligatory special case...
        if [ __debug = $word ]; then
            a1=debug_
            a2=__debug
        fi
        hash=$(${kwh} $a2 | cut -d' ' -f1)
        echo "  case $hash: THEN(${a1},\"${a2}\");"
    done
    echo
}

########################################################################
# Built-in values...
function doBIVs(){
    local -a words=(
        true false null this undefined using whcl
        __COLUMN __LINE __FILE __FILEDIR __FLC
    )
    echo "For for pasting into whcl.c:whcl__biv_search():"
    echo; echo "$banner"
    while read hash word; do
        echo "  case $hash: THEN(${word},\"${word}\");"
    done < <($kwh ${words[@]})
    echo
}

########################################################################
# Pragmas...
function doPragmas(){
    local -a words=(
        __debug
        dump-tokens
        memory-metrics
        trace-assert
        vacuum
    )
    echo "For for pasting into/around whcl.c:whcl__bic_f_pragma():"
    echo;
    echo "$banner"
    echo "enum whcl__pragma_e {"
    echo -n "  WHCL__PRAGMA__invalid = 0"
    for word in ${words[@]}; do
        w=${word//-/_}
        echo ","
        echo -n "  WHCL__PRAGMA_${w} /* ${word} */"
    done
    echo; echo "};"

    echo; echo "$banner"
    while read hash word; do
        w=${word//-/_}
        echo "  case $hash: THEN(${w},\"${word}\");"
    done < <($kwh ${words[@]})
    echo "  default: return WHCL__PRAGMA__invalid;"
    echo
}

########################################################################
# Subcommands for the info builtin...
function doInfo(){
    local -A words=(
        [can-new]=''
        [has-array]=''
        [has-buffer]=''
        [has-enum]=''
        [has-exception]=''
        [has-function]=''
        [has-hash]=''
        [has-native]=''
        [has-object]=''
        [has-prototype]=''
        [is-array]=''
        [is-bool]=''
        [is-buffer]=''
        [is-container]=''
        [is-declared]=''
        [is-derefable]=''
        [is-double]=''
        [is-enum]=''
        [is-exception]=''
        [is-function]=''
        [is-hash]=''
        [is-integer]=''
        [is-iterating]=''
        [is-iterating-list]=''
        [is-iterating-props]=''
        [is-list]=''
        [is-local]=''
        [is-native]=''
        [is-newing]=''
        [is-number]=''
        [is-numeric]=''
        [is-object]=''
        [is-string]=''
        [is-tuple]=''
        [is-unique]=''
        [may-iterate]=''
        [may-iterate-list]=''
        [may-iterate-props]=''
        [type-name]=''
        [ref-count]=''
    )
    echo "For for pasting into/around whcl.c:whcl__bic_f_pragma():"
    echo;
    echo "$banner"
    echo "enum whcl__pragma_info_e {"
    echo -n "  WHCL__PRAGMA_INFO__invalid = 0"
    for word in ${!words[@]}; do
        w=${word//-/_}
        words[$word]=$w
        echo ","
        echo -n "  WHCL__PRAGMA_INFO_${w} /* ${word} */"
    done
    echo; echo "};"

    
    echo; echo "$banner"
    while read hash word; do
        w=${words[$word]}
        echo "  case $hash: THEN(\"${word}\",${w});"
    done < <($kwh ${!words[@]})
    echo "  default: return NULL;"
    echo
}

########################################################################
# Do string-to-CWAL_RC_...
function doRC(){
    local -a words=(
        CWAL_RC_OK
        CWAL_RC_ERROR
        CWAL_RC_OOM
        CWAL_RC_FATAL
        CWAL_RC_CONTINUE
        CWAL_RC_BREAK
        CWAL_RC_RETURN
        CWAL_RC_EXIT
        CWAL_RC_EXCEPTION
        CWAL_RC_ASSERT
        CWAL_RC_MISUSE
        CWAL_RC_NOT_FOUND
        CWAL_RC_ALREADY_EXISTS
        CWAL_RC_RANGE
        CWAL_RC_TYPE
        CWAL_RC_UNSUPPORTED
        CWAL_RC_ACCESS
        CWAL_RC_IS_VISITING
        CWAL_RC_IS_VISITING_LIST
        CWAL_RC_DISALLOW_NEW_PROPERTIES
        CWAL_RC_DISALLOW_PROP_SET
        CWAL_RC_DISALLOW_PROTOTYPE_SET
        CWAL_RC_CONST_VIOLATION
        CWAL_RC_LOCKED
        CWAL_RC_CYCLES_DETECTED
        CWAL_RC_DESTRUCTION_RUNNING
        CWAL_RC_FINALIZED
        CWAL_RC_HAS_REFERENCES
        CWAL_RC_INTERRUPTED
        CWAL_RC_CANCELLED
        CWAL_RC_IO
        CWAL_RC_CANNOT_HAPPEN
        CWAL_RC_JSON_INVALID_CHAR
        CWAL_RC_JSON_INVALID_KEYWORD
        CWAL_RC_JSON_INVALID_ESCAPE_SEQUENCE
        CWAL_RC_JSON_INVALID_UNICODE_SEQUENCE
        CWAL_RC_JSON_INVALID_NUMBER
        CWAL_RC_JSON_NESTING_DEPTH_REACHED
        CWAL_RC_JSON_UNBALANCED_COLLECTION
        CWAL_RC_JSON_EXPECTED_KEY
        CWAL_RC_JSON_EXPECTED_COLON
        CWAL_SCR_CANNOT_CONSUME
        CWAL_SCR_INVALID_OP
        CWAL_SCR_UNKNOWN_IDENTIFIER
        CWAL_SCR_CALL_OF_NON_FUNCTION
        CWAL_SCR_MISMATCHED_BRACE
        CWAL_SCR_MISSING_SEPARATOR
        CWAL_SCR_UNEXPECTED_TOKEN
        CWAL_SCR_UNEXPECTED_EOF
        CWAL_SCR_DIV_BY_ZERO
        CWAL_SCR_SYNTAX
        CWAL_SCR_EOF
        CWAL_SCR_TOO_MANY_ARGUMENTS
        CWAL_SCR_EXPECTING_IDENTIFIER
    )

    echo "For for pasting into whcl_cstr_to_rc():"
    echo; echo "$banner"
    for word in ${words[@]}; do
        hash=$(${kwh} $word | cut -d' ' -f1)
        echo "  case $hash: THEN(${word},\"${word}\");"
        s=${word#CWAL_RC_}
        s=${s#CWAL_} # for the CWAL_SCR_ entries
        hash=$(${kwh} $s | cut -d' ' -f1)
        echo "  case $hash: THEN(${word},\"${s}\");"
    done
    echo
};

########################################################################
# Do OB methods...
function doOB(){
    local -a words=(
        capture
        clear
        flush
        get-string
        level
        pop
        push
        take-buffer
        take-string
    )
    echo "For for pasting into ob.c:whcl__ob_subsubcommand():"
    echo; echo "$banner"
    while read hash word; do
        m=whcl__cb_ob_${word//-/_}
        echo "  case $hash: THEN(\"$word\",$m);"
    done < <($kwh ${words[@]})
    echo
}

########################################################################
# whcl__cb_install_api() args
function doInstallAPI(){
    local -A words=(
        [fs]=whcl__install_fs
        [json]=whcl__install_json
        [ob]=whcl__install_ob
        [os]=whcl__install_os
        [PathFinder]=whcl__install_pf
        [require]=whcl__install_require
        [Script]=whcl__install_script
        [time]=whcl__install_time
        [tmpl]=whcl__install_tmpl
    )
    echo "For for pasting into/near whcl.c:whcl_install_api():"
    echo;
    echo '#define WHCL__BUILTIN_MODULE_INITS \'
    local i=0
    for word in $(echo ${!words[@]} | xargs -n1 | sort); do
        echo -n " M(${word})"
    done
    echo

    echo; echo "$banner"
    while read hash word; do
        m=whcl_install_${word//-/_}
        f=${words[$word]}
        echo "  case $hash: THEN(\"$word\",$f);"
    done < <($kwh ${!words[@]})
    echo
}

########################################################################
# Dispatch args...
args="$@"
if [ x = "x$args" ]; then
    echo "Pass in one or more of: bic biv info install-api pragma rc ob"
    exit 1
fi

for i in $args; do
    case $i in
        biv|bivs)    doBIVs ;;
        bic|bics)    doBICs ;;
        info*)       doInfo ;;
        install-api) doInstallAPI ;;
        ob)          doOB ;;
        pragma*)     doPragmas ;;
        rc)          doRC ;;
        *) echo "Unknown argument: $i" 1>&2
           exit 1
    esac
done

