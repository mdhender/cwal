#!/bin/bash
# Uses this directory's docker file to build a static s2sh
# (intended primarily for use as a CGI backend).
#
# Any arguments are passed on to the main docker build.
set -e # exit if any command fails
# set -o pipefail
# set -x
docker build \
       --build-arg cachebust="$(date)" \
       -f Dockerfile-cwal-static -t cwal_static \
       "$@" .
docker create --name cwal cwal_static
docker cp cwal:/cwal-src/s2/s2sh s2sh
docker cp cwal:/cwal-src/s2/s2sh2 s2sh2
docker cp cwal:/cwal-src/whcl/whclsh whclsh
strip s2sh s2sh2 whclsh
docker container rm cwal
echo "If you got this far, it worked."
ls -la s2sh s2sh2 whclsh
