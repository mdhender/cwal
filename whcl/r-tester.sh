#!/bin/bash
#HELP>#####################################################################
# Runs require.whcl unit test scripts.
#
# Usages:
#
# 1) $0 [flags]
# searches for *.test.whcl in $(dirname $0)/require.d and runs them.
#
# 2) $0 [flags] moduleName ...moduleNameN
# Expects that each argument is a require.whcl module name and
# expects moduleName.test.whcl to exist in the require.whcl module
# search path.
#
# Runs each test in its own whclsh instance and exits with non-0 if
# any test fails.
#
# Flags:
#
# -V enables VERBOSE mode, which simply outputs some extra output.
#
# -vg enables valgrind/massif tests IF valgrind is found in the path.
# These generate some files which one may peruse to collect allocation
# metrics.
#
# Any flags not handled by this script are passed on to the underlying
# whclsh call (quoting might be a problem - avoid complex flags).
#
#HELP<###################################################################

dir=. #$(dirname $0)
#[[ '.' = "${dir}" ]] && dir=$PWD
whclsh=$dir/whclsh

[[ -x "$whclsh" ]] || whclsh=$(which whclsh 2>/dev/null)
[[ -x "$whclsh" ]] || {
    echo "whclsh not found :(. Looked in [${dir}] and \$PATH." 1>&2
    exit 1
}
echo "whclsh = ${whclsh}"
rdir=${dir}/require.d
[[ -d "$rdir" ]] || {
    echo "Missing 'require.d' dir: $rdir" 1>&2
    exit 2
}
#rdir=$(cd $rdir>/dev/null; echo $PWD)

DO_VG=0
VERBOSE=0
WHCLSH_ADD_FLAGS="${WHCLSH_XFLAGS}"
WHCLSH_SCRIPT_FLAGS="--whcl.require.home=${rdir} --whcl.require.path=${rdir}"
DO_DEBUG=0

list=''
for i in "$@"; do
    case $i in
        -V) VERBOSE=1
            ;;
        -vg) DO_VG=1
            ;;
        -debug)
            DO_DEBUG=1
            ;;
        -\?|--help)
            echo "$0:"
            sed -n '/^#HELP/,/^#HELP/p' < $0 | grep -v -e '^#HELP' \
                | sed -e 's/^#/    /g'
            exit 0;
            ;;
        -*)
            WHCLSH_ADD_FLAGS="${WHCLSH_ADD_FLAGS} $i"
            ;;
        *) list="$list $i.test"
        ;;        
    esac
done

[[ x = "x${list}" ]] && {
    cd $rdir || exit $? # oh, come on, steve, this isn't C!
    list=$(find . -name '*.test.whcl' | cut -c3- | sed -e 's/\.whcl$//g' | sort)
    cd - >/dev/null
}

[[ "x" = "x${list}" ]] && {
    echo "Didn't find any *.test.whcl scripts :(" 1>&2
    exit 3
}
list=$(echo $list) # remove newlines
#echo "Unit test list: $list"

function verbose(){
    [[ x1 = "x${VERBOSE}" ]] && echo $@
}

function vgTest(){
    local test=$1
    shift
    local flags="$@"
    local tmp=tmp.$test
    local vgout=vg.$test
    cat <<EOF > $tmp
whcl.install-api require
whcl.require 'nocache!${test}'
EOF
    local cmd="$vg --leak-check=full -v --show-reachable=yes --track-origins=yes $whclsh ${WHCLSHFLAGS} -f ${tmp} ${flags} -- ${WHCLSH_SCRIPT_FLAGS}"
    echo "Valgrind: $test"
    verbose -e "\t$cmd"
    $cmd &> $vgout || {
        rc=$?
        rm -f $tmp
        echo "Valgrind failed. Output is in ${vgout}"
        exit $rc
    }
    #rm -f $vgout
    echo "Valground: $test [==>$vgout]"
    vgout=massif.$test
    local msp=ms_print.$test
    cmd="$massif --massif-out-file=$msp $whclsh ${WHCLSHFLAGS} -f ${tmp} ${flags} -- ${WHCLSH_SCRIPT_FLAGS}"
    echo "Massifying: $test"
    verbose -e "\t$cmd"
    $cmd &> $vgout || {
        rc=$?
        rm -f $tmp
        echo "Massif failed. Output is in ${vgout}"
        exit $rc
    }
    echo "Massified $test: try: ms_print ${msp} | less"
}

if [[ x1 = x${DO_DEBUG} ]]; then
    whclsh="gdb --args $whclsh"
fi
WHCLSHFLAGS="-rv -rc -si ${WHCLSH_ADD_FLAGS}"
echo WHCLSHFLAGS=$WHCLSHFLAGS
for test in $list; do
    echo "Running require.whcl test: ${test}"
    outfile=${rdir}/${test}.test_out
    verbose "Output going to: $outfile"
    rm -f "$outfile"
    tmpfile=${rdir}/${test}.tmp
    {
        echo "whcl.install-api require"
        echo "whcl.require 'nocache!${test}'"
    } > $tmpfile
    cmd="$whclsh ${WHCLSHFLAGS} -o ${outfile} -f ${tmpfile} -- ${WHCLSH_SCRIPT_FLAGS}"
    echo "Running test [${test}]: $cmd"
    $cmd
    rc=$?
    [[ 0 -eq $rc ]] || {
        echo "Test '${test}' failed. Script output (if any) saved to [${outfile}]" 1>&2
        exit $rc
    }
    #echo "Test did not fail: ${test}"
    rm -f $tmpfile
    if [[ -s "$outfile" ]]; then
        verbose -e "\tOutput is in: ${outfile}"
    else
        rm -f "${outfile}"
    fi
done

if [[ x1 = "x$DO_VG" ]]; then
   vg=$(which valgrind)
    if [[ -x "$vg" ]]; then
        echo "Runing test(s) through valgrind..."
        massif="${vg} --tool=massif --time-unit=ms --heap-admin=0"
        for test in $list; do
            outfile=$rdir/${test}.test_out
            rm -f "$outfile"
            vgTest $test -o "${outfile}"
        done
    fi
fi

echo "Done! Tests run: ${list}"
