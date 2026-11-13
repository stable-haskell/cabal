/* SHA256 one-shot helper for the MicroHs (MHS) build of cabal-install.
 *
 * MicroHs cannot use the @cryptohash-sha256@ package, so
 * 'Distribution.Client.HashValue' calls 'cabal_sha256' through FFI
 * (see the @foreign import ccall@ there, which must be
 * header-qualified with this file, e.g.
 * @foreign import ccall "mhs_sha256.h cabal_sha256" ...@).
 *
 * On Apple platforms the system SDK provides CommonCrypto's
 * @CC_SHA256@ in @libSystem@, so no external library is needed at
 * link time.  Elsewhere (Linux and other POSIX systems) OpenSSL's
 * @SHA256()@ from @<openssl/sha.h>@ is used, which needs @-lcrypto@
 * at link time.
 */
#ifndef CABAL_MHS_SHA256_H
#define CABAL_MHS_SHA256_H

unsigned char *cabal_sha256(const void *data, unsigned long len,
                            unsigned char *md);

#endif /* CABAL_MHS_SHA256_H */
