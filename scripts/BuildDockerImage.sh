#!/bin/sh

BASEDIR=`pwd`
TMPDIR=`mktemp -d`

cp ${BASEDIR}/scripts/Dockerfile ${TMPDIR}/
pushd ${TMPDIR}
docker build -t halvm3-builder .
popd

