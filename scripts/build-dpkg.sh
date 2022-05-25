#!/usr/bin/env bash

rev=${1:-0}

if [[ "$(cat support/version)" =~ ^v([0-9.]*)$ ]]; then
    ver="${BASH_REMATCH[1]}"
else
    echo "dev build detected!" 1>&2
    ver="99.99.99"
fi

echo "version: ${ver}"
echo "revision: ${rev}"

cargo build \
      --manifest-path blockfish-engine/Cargo.toml \
      --features service \
      --bin blockfish \
      --release || exit 1

tmp=

function cleanup {
    set +e
    rm -rf "${tmp}"
}

trap cleanup EXIT
set -e

tmp=$(mktemp -d) || exit 1

root="${tmp}/blockfish-${ver}-${rev}_all"
mkdir -p ${root}/usr/bin
install -m 755 ./target/release/blockfish "${root}/usr/bin/blockfish"

mkdir -p ${root}/DEBIAN
readonly ctl="${root}/DEBIAN/control"
echo "Package: blockfish" > ${ctl}
echo "Version: ${ver}" >> ${ctl}
echo "Maintainer: iitalics" >> ${ctl}
echo "Architecture: all" >> ${ctl}
echo "Description: blockfish backend IPC service" >> ${ctl}

mkdir -p dist
dpkg-deb --root-owner-group -b "${root}" "dist/blockfish-${ver}-${rev}_all.deb"
exit 0
