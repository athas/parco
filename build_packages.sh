#!/bin/sh
#
# Check the Parco packages for consistency, then generate tarballs in
# the current directory.

set -e # Die on error.

if [ $# -lt 1 ]; then
    echo "Usage: $0 <git-tag>"
    exit 1
fi

tag=$1
parcoversion=$(awk '/^version/ {print $2}' parco/parco.cabal)

mktarball () {
    pkg=$1
    sub=$(echo $pkg | sed 's/parco-//')
    if [ $pkg = parco ] ||
        (grep -qi "name: *parco-$sub" $pkg/$pkg.cabal &&
         grep -qi "synopsis:.*$sub interface" $pkg/$pkg.cabal &&
         grep -qi "Parser monad instance for $sub" $pkg/$pkg.cabal &&
         grep -qi "build-depends:.*parco==$parcoversion" $pkg/$pkg.cabal); then
        git archive --prefix=$pkg-$tag/ --format=tar $tag:$pkg | gzip > $pkg-$tag.tar.gz
    else
        echo $pkg/$pkg.cabal is wrong, refusing to continue.
        exit 1
    fi
}

mainpkg=parco
subpkgs=$(find . -maxdepth 1 -type d -name 'parco-*' | sed 's|^./||')

mktarball $mainpkg

for pkg in $subpkgs; do
    mktarball $pkg
done
