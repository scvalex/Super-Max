#!/bin/sh

DIR=$(dirname $0)
TMPFILE=/tmp/spellslinger-redist.tar.bz2

wget abstractbinary.org/pub/spellslinger-redist.tar.bz2 -O ${TMPFILE}
tar -xjvf ${TMPFILE} -C ${DIR}
rsync -avz ${DIR}/spellslinger-redist/ ${DIR}
rm -rf ${DIR}/spellslinger-redist/
