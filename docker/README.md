# Building a Static s2sh Using Docker

The problem: i make heavy use of [s2sh][] in CGI scripts on my
websites, but building the binaries directly on my web hoster risks
using too much CPU time and causing undesired Grief with the
administrators. Building purely static binaries on Linux is not
as straightforward as it really could be because glibc (the most
widely-used standard C library on Linux systems) cannot "really"
statically link certain networking libraries.

One solution is to build, on my local machine, purely static [s2sh][]
binaries in a [Docker](https://docker.io) environment using a build
chain which does not have that quirk/limitation. It's still important
that the build architecture is the same as the hoster's, but it's not
important that we build with the same libc. So...

(Disclaimer: Docker is still largely alien to me, so the details
described here might not reflect optimal, or even necessarily
*correct*, use of Docker.)

The following sequence of shell commands, run from this directory (or
a directory containing this one's Dockerfile), ought to build
a static binary of [s2sh][]:

```bash
docker build -f Dockerfile-cwal-static -t s2sh_static  . ;
docker create --name s2sh s2sh_static ;
docker cp s2sh:/cwal-src/s2/s2sh s2sh ;
strip s2sh ;
# OPTIONALLY copy the s2sh wrapper script:
#   docker cp s2sh:/cwal-src/s2/s2sh.sh s2sh.sh ;
docker container rm s2sh ;
```

The result is a fully static s2sh binary which includes various
modules which are commonly required in CGI scripts ("commonly" meaning
"on my systems").

About the optional s2sh wrapper script: s2sh benefits from having
several environment variables configured before it starts, so the
common practice ("common" meaning "on my systems") is to move the
compiled s2sh binary to `~/s2/s2sh.bin`, move `s2sh.sh` to
`~/bin/s2sh`, make it executable, and edit it to suit (noting that
`~/bin` is in my `$PATH` and `~/s2` (a.k.a. `$S2_HOME`) is where my
numerous s2 bits are stored). See [](../s2/manual/s2-home.md) for
more details.

Cleanup:

```bash
docker image rm s2sh_static
```

Aaaaarrrgggg: sometimes that removes only the latest image in the
stack and sometimes it removes everything. What's up with that?

For various reasons beyond my meager docker ken, it's common to see
leftover images which may or may not be useful later on. To remove all
of the intermediary images, use `docker image ls -a` to get a list of
all images and remove them by using their IDs:

```bash
docker image rm ID_1... ID_N
```

Certainly there's an easier way without resorting to `awk`? How about:

```bash
docker system prune --force
```

Optionally, remove the base OS image:

```bash
docker image rm alpine:edge
```

Because the `s2sh_static` image has to install so many OS packages,
removing the small core image (alpine) doesn't save all that much time
unless the internet connection is slow.

[s2sh]: ../s2/manual/s2sh.md
