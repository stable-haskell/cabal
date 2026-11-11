/* SHA256 one-shot helper for the MicroHs (MHS) build of cabal-install.
 *
 * See @mhs_sha256.h@ for the linkage contract.
 */

#include "mhs_sha256.h"

#if defined(__APPLE__)
#include <CommonCrypto/CommonDigest.h>

unsigned char *cabal_sha256(const void *data, unsigned long len,
                            unsigned char *md)
{
  return CC_SHA256(data, (CC_LONG) len, md);
}

#else

#include <openssl/sha.h>

unsigned char *cabal_sha256(const void *data, unsigned long len,
                            unsigned char *md)
{
  return SHA256((const unsigned char *) data, (size_t) len, md);
}

#endif
