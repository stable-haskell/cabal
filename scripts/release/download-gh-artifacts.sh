#!/bin/bash

set -eu
set -o pipefail

RELEASE=$1
VERSION=${RELEASE#cabal-install-v}
SIGNER=$2

echo "RELEASE: $RELEASE"
echo "SIGNER: $SIGNER"

for com in gh gpg curl sha256sum ; do
	command -V ${com} >/dev/null 2>&1
done

[ ! -e "gh-release-artifacts/cabal-${VERSION}" ]

mkdir -p "gh-release-artifacts/cabal-${VERSION}"

git archive --format=tar.gz -o "gh-release-artifacts/${RELEASE}/cabal-${TAG}-src.tar.gz" --prefix="cabal-${TAG}/" HEAD

cd "gh-release-artifacts/cabal-${VERSION}"

# github
gh release download "$RELEASE"

sha256sum ./* > SHA256SUMS
gpg --detach-sign -u "${SIGNER}" SHA256SUMS

gh release upload "$RELEASE" "cabal-${TAG}-src.tar.gz" SHA256SUMS SHA256SUMS.sig

