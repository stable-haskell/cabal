/* Mode query for MicroHs handles, for the MHS (MicroHs) build of Cabal.
 *
 * MicroHs has no 'hSetBinaryMode' and exposes no Haskell-level query
 * for a handle's text/binary mode.  A text-mode handle (opened with
 * 'openFile') carries the UTF-8 transducer layer (@c_add_utf8@) on top
 * of the raw file layer; a binary-mode handle (opened with
 * 'openBinaryFile') is the raw file layer itself.  The transducer
 * layer does not implement block reads/writes, so 'BS.hPut' and
 * 'BS.hGetContents' silently corrupt non-ASCII bytes through it (see
 * "Distribution.Compat.Internal.MhsProcess").
 *
 * The outermost BFILE of each kind is identified by its @getb@
 * implementation: the transducer's is @getb_utf8@.  This depends on
 * the MicroHs 0.16.12.0 runtime layout (the method table starts the
 * @BFILE@ struct, see @src/runtime/bfile.c@): a runtime upgrade that
 * moves it breaks this probe, and the @T4b@ capture tests cover both
 * handle kinds to catch that.
 *
 * On non-MHS toolchains (and when @__MHS__@ is unset) this compiles
 * to a stub that is never called.
 */
#include "mhs_handle.h"

#ifdef __MHS__
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

/* MicroHs runtime symbols (src/runtime/bfile.c). */
struct mhs_bfile_mets {
  int (*getb)(void *);
  void (*ungetb)(int, void *);
  void (*putb)(int, void *);
  void (*flushb)(void *);
  void (*closeb)(void *);
  size_t (*readb)(uint8_t *, size_t, void *);
  size_t (*writeb)(const uint8_t *, size_t, void *);
  int (*isattyb)(void *);
  int64_t (*filesizeb)(void *);
};

struct mhs_bfile_file {
  struct mhs_bfile_mets mets;
  FILE *file;
};

struct mhs_bfile_utf8 {
  struct mhs_bfile_mets mets;
  struct mhs_bfile_mets *bfile;
  int unget;
  uint8_t pend[3];
  int npend;
};

extern int getb_utf8(void *);
extern int getb_file(void *);

int
mhs_handle_is_text(void *p)
{
  return ((struct mhs_bfile_mets *)p)->getb == getb_utf8;
}

int
mhs_handle_seek(void *p, int whence, int64_t offset)
{
  struct mhs_bfile_mets *bp = (struct mhs_bfile_mets *)p;
  if (!bp)
    return -1;
  if (bp->getb == getb_utf8) {
    struct mhs_bfile_utf8 *u = (struct mhs_bfile_utf8 *)bp;
    u->unget = -1;
    u->npend = 0;
    bp = u->bfile;
  }
  if (!bp || bp->getb != getb_file)
    return -1;
  FILE *f = ((struct mhs_bfile_file *)bp)->file;
  if (!f)
    return -1;
  int c_whence = SEEK_SET;
  if (whence == 1)
    c_whence = SEEK_CUR;
  else if (whence == 2)
    c_whence = SEEK_END;
#if defined(_WIN32)
  return _fseeki64(f, (__int64)offset, c_whence);
#else
  return fseeko(f, (off_t)offset, c_whence);
#endif
}
#else
int
mhs_handle_is_text(void *p)
{
  (void)p;
  return 0;
}

int
mhs_handle_seek(void *p, int whence, int64_t offset)
{
  (void)p;
  (void)whence;
  (void)offset;
  return -1;
}
#endif
