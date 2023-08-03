#!/bin/bash
########################################################################
# Creates various client-side distribution bundles for cwal and its
# child projects.

set -e
which fossil >/dev/null
version=$(cat manifest.uuid)
versionShort=$(cut -c1-12 manifest.uuid)
vTime=$(awk '/^D /{gsub(/T/, " ", $2); gsub(/\.[0-9]{1,3}/, "", $2); print $2}' manifest)
url=https://fossil.wanderinghorse.net/r/cwal
make amal
touch --date="${vTime}" manifest.uuid

function zipIt(){
    local d=$1
    local z=${d}.tar.gz
    tar czf $z $d
    touch --date="${vTime}" $z
    echo "============================================================"
    echo "= Sanity-testing $d..."
    cd $d
    make
    cd - >/dev/null
    rm -fr $d
    ls -la $z
    echo "= Done with $d"
    echo "============================================================"
}

########################################################################
########################################################################
# Core libcwal amalgamation...
dCwal=libcwal-amalgamation
rm -fr $dCwal
mkdir $dCwal

cp libcwal.[ch] src/test.c $dCwal
cp -p manifest.uuid $dCwal/VERSION
cat <<EOF > $dCwal/0_README.txt
This is the "amalgamation" distribution of libcwal:

    ${url}

Specifically, this file was generated from version ${versionShort}:

   ${url}/info/${version}

   Timestamped ${vTime} UTC

It is intended to be copied directly into client-side source trees and
compiled using the local tools. For example:

    cc -std=c99 -c libcwal.c

Many more docs can be found at the above URL.
EOF

cat <<EOF > $dCwal/Makefile
########################################################################
# Bare minimum makefile for inclusion with the libcwal client
# distribution. This builds the basic cwal test/demo app.
default: test

CFLAGS += -std=c99 -fPIC
LDFLAGS += -fPIC
CPPFLAGS += -I.
CC ?= cc
# Because the test app tests API-internal stuff, it includes
# libcwal.c directly, rather than including libcwal.h (public APIs).
test.o: test.c
	\$(CC) \$(CFLAGS) \$(CPPFLAGS) -c \$< -o \$@
test: test.o
	\$(CC) \$(LDFLAGS) -o \$@ test.o
	@echo "As a sanity test, try: ./\$@"

clean:
	rm -f *.o test
EOF

zipIt ${dCwal}

########################################################################
########################################################################
# whcl...
dWhcl=libwhcl-amalgamation
rm -fr $dWhcl
mkdir $dWhcl

cat <<EOF > $dWhcl/0_README.txt
This is the "amalgamation" distribution of the libwhcl sub-project
of libcwal:

    ${url}

Specifically, this file was generated from version ${versionShort}:

   ${url}/info/${version}

   Timestamped ${vTime} UTC

It is intended to be copied directly into client-side source trees and
compiled using the local tools. For example:

    cc -std=c99 -c libwhcl.c

Many more docs can be found at the above URL.

A very basic Makefile is included for building its shell app,
but it disables many options by default for maximum portability.
See the Makefile and included config.h for details.
EOF

autoconfigOn=(
	HAVE_CHDIR
	HAVE_GETCWD
	HAVE_LSTAT
	HAVE_MKDIR
	HAVE_OPENDIR
	HAVE_REALPATH
	HAVE_SIGACTION
	HAVE_STAT
)
autoconfigOff=(
	HAVE_CLOCK_GETTIME
	HAVE_DLFCN_H
	HAVE_DLOPEN
	HAVE_FORK
	HAVE_LIBDL
	HAVE_LIBLTDL
	HAVE_LTDL_H
	HAVE_POPEN
	HAVE_READLINE
	HAVE_REGCOMP
	HAVE_USLEEP
)

config_h_tmp=config.h.tmp
trap "rm -f $config_h_tmp" 0

{ # $config_h_tmp (shared by whcl and s2)
    cat <<EOF
#ifndef _CONFIG_H
#define _CONFIG_H
/* This is a basic config.h for libcwal extension code.
   compile them with -DHAVE_CONFIG_H to include this
   file and edit it to suit. Certain features requires
   additional packages. */

/* To enable readline editing support in the shell app, enable
   the following block, compile cliapp.c with -DHAVE_CONFIG_H
   and link with -lreadline and (on most systems) -lncurses.
*/
#if 0
#  define CLIAPP_ENABLE_READLINE 1
#elif 0
/* TO enable linenoise, instead of readline, use this
   and link with -llinenoise. */
#  define CLIAPP_ENABLE_LINENOISE 1
#endif
EOF
    for on in ${autoconfigOn[@]}; do
        echo "#ifndef $on"
		echo "#  define $on"
		echo "#endif"
    done
    for off in ${autoconfigOff[@]}; do
		echo "/* #undef $off */"
    done
    echo "#endif /* _CONFIG_H */"
} > $config_h_tmp

cat <<EOF > $dWhcl/Makefile
########################################################################
# Bare minimum makefile for inclusion with the whcl client
# distribution.
default: whclsh

CFLAGS += -std=c99 -fPIC
LDFLAGS += -fPIC -lm
CPPFLAGS += -I. -DHAVE_CONFIG_H
CC ?= cc

############################################################
# To add libreadline support to the shell:
# LDFLAGS += -lreadline -lncurses
# CLIAPP_CPPFLAGS = -DCLIAPP_ENABLE_READLINE
# Or use liblinenoise instead:
# LDFLAGS += -llinenoise
# CLIAPP_CPPFLAGS = -DCLIAPP_ENABLE_LINENOISE
############################################################

cliapp.o: cliapp.c
	\$(CC) \$(CFLAGS) \$(CPPFLAGS) \$(CLIAPP_CPPFLAGS) -c \$< -o \$@
whclsh_extend.o: whclsh_extend.c
	\$(CC) \$(CFLAGS) \$(CPPFLAGS) -c \$< -o \$@
whclsh.o: whclsh.c
	\$(CC) \$(CFLAGS) \$(CPPFLAGS) -DWHCLSH_EXTEND -c \$< -o \$@
libwhcl.o: libwhcl.c
	\$(CC) \$(CFLAGS) \$(CPPFLAGS) -c \$< -o \$@
whclsh.deps = whclsh.o whclsh_extend.o cliapp.o libwhcl.o
# ^^^ BSD make does not understand \$^
whclsh: \$(whclsh.deps)
	\$(CC) \$(LDFLAGS) -o \$@ \$(whclsh.deps)
	@echo "As a sanity test, try: ./\$@ dump-whcl-protos.whcl"

clean:
	rm -f *.o whclsh
EOF

cp $config_h_tmp $dWhcl/config.h
cp -p manifest.uuid $dWhcl/VERSION
cp -p \
   whcl/whclsh.c whcl/whclsh_extend.c \
   whcl/dump-whcl-protos.whcl \
   libwhcl.c libwhcl.h \
   s2/cliapp.[ch] \
   $dWhcl

zipIt ${dWhcl}

########################################################################
########################################################################
# s2
dS2=libs2-amalgamation
rm -fr $dS2
mkdir $dS2

cat <<EOF > $dS2/0_README.txt
This is the "amalgamation" distribution of the libs2 sub-project
of libcwal:

    ${url}

Specifically, this file was generated from version ${versionShort}:

   ${url}/info/${version}

   Timestamped ${vTime} UTC

It is intended to be copied directly into client-side source trees and
compiled using the local tools. For example:

    cc -std=c99 -c libs2.c

Many more docs can be found at the above URL.

A very basic Makefile is included for building its shell app,
but it disables many options by default for maximum portability.
See the Makefile and included config.h for details.
EOF

cat <<EOF > $dS2/Makefile
########################################################################
# Bare minimum makefile for inclusion with the libs2 client
# distribution.
default: s2sh2

CFLAGS += -std=c99 -fPIC
LDFLAGS += -fPIC
CPPFLAGS += -I. -DHAVE_CONFIG_H
CC ?= cc

############################################################
# To add libreadline support to the shell:
# LDFLAGS += -lreadline -lncurses
# CLIAPP_CPPFLAGS = -DCLIAPP_ENABLE_READLINE
# Or use liblinenoise instead:
# LDFLAGS += -llinenoise
# CLIAPP_CPPFLAGS = -DCLIAPP_ENABLE_LINENOISE
############################################################

cliapp.o: cliapp.c
	\$(CC) \$(CFLAGS) \$(CPPFLAGS) \$(CLIAPP_CPPFLAGS) -c \$< -o \$@
shell_extend.o: shell_extend.c
	\$(CC) \$(CFLAGS) \$(CPPFLAGS) -c \$< -o \$@
shell2.o: shell2.c
	\$(CC) \$(CFLAGS) \$(CPPFLAGS) -DS2_SHELL_EXTEND -c \$< -o \$@
libs2.o: libs2.c
	\$(CC) \$(CFLAGS) \$(CPPFLAGS) -c \$< -o \$@
s2sh2.deps = shell2.o shell_extend.o cliapp.o libs2.o
# ^^^ BSD make does not understand \$^
s2sh2: \$(s2sh2.deps)
	\$(CC) \$(LDFLAGS) -o \$@ \$(s2sh2.deps)
	@echo "As a sanity test, try: ./\$@ demo.s2"

clean:
	rm -f *.o s2sh2
EOF


cat <<EOF > $dS2/demo.s2
// Very brief demo script for s2.
print('Hello, world!');
print(<<<X Hello, world! X);
print(catch{throw "Hello, world!"}.message);
print(catch{throw {hi:"Hello, world!"}}.message.hi);
s2.io.output(catch{throw ["Hello, world!"]}.message[0], '\n');
s2out << scope{
    var ☺ = ["Hello","world!"];
    ☺.join(", ");
} << '\n';
print(["Hello","world!"].join(', '));
print(["Hello","world!"].withThis(function(){return this.join(', ')}));
print(for(;;) break "Hello, world!");
// Aaaaand, for good measure:
print.'operator<<' = proc(){ return this(argv.0) };
print << "Hello, world!";
EOF

cp $config_h_tmp $dS2/config.h
cp -p manifest.uuid $dS2/VERSION
cp -p libs2.[ch] \
   s2/shell_common.c s2/shell2.c s2/shell_extend.c \
   s2/cliapp.[ch] \
   $dS2

zipIt $dS2

########################################################################
# download.html and Fossil SCM /uv files
dl=download.html
vTimeShort=${vTime:0:16}

function outputDL() {
    local file=$1-amalgamation.tar.gz
    shift
    local desc=$@
    echo "<h2>$file</h2><p>$desc</p>"
    echo "<pre id='download-list'>"
    echo "<a href='${file}'>${file}</a>"
    echo "Size:" $(ls -la $file | awk '{print $5}') "bytes"
    echo "SHA3-256 hash:" $(fossil sha3sum $file | cut -d' ' -f1)
    echo "</pre>"
    fossil uv add $file --mtime "${vTime}"
}

{
    cat <<EOF
<div class='fossil-doc' data-title='Downloads'>
<style>
#download-list{ line-height: 1.5; font-size: 125%; white-space: pre-wrap; }
</style>
<p>This page hosts "amalgamation-style" builds of libcwal
  and its child projects. It only hosts the "latest" build
  and is updated periodically by a project contributor.
</p>
<p>Source tree version of the following files:
<a href='../info/${versionShort}'>${versionShort}</a>
(<a href='../timeline?c=${vTimeShort}'>$vTimeShort</a> UTC)
</p>
<hr/>
EOF
    outputDL libcwal "The <a href='../'>core libcwal</a> and a demo/test app." \
             "Note that the other distributions embed their own copies of this."
    outputDL libs2 "The <a href='../doc/ckout/s2/'>s2 scripting engine</a> and shell."
    outputDL libwhcl "WHCL: the <a href='../doc/ckout/whcl/manual/'>WanderingHorse.net Command Language</a> and shell."
    cat <<EOF
EOF
} > $dl
fossil uv add $dl --mtime "${vTime}"
fossil uv list
rm -f

echo "Use:"
echo "  fossil uv sync"
echo "to upload these to the remote repo."

# The End
########################################################################
