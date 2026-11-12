/* Mode query for MicroHs handles; see mhs_handle.c.  Shipped in
 * extra-source-files; carries the prototype for MicroHs-generated
 * FFI call sites. */
#ifndef MHS_HANDLE_H
#define MHS_HANDLE_H

#include <stdint.h>

/* Nonzero when the handle's outermost BFILE layer is MicroHs' UTF-8
 * transducer (text mode), zero for the raw file layer (binary mode).
 * Under @__MHS__@ this probes the 0.16.12.0 runtime layout;
 * otherwise it is a stub returning 0. */
int mhs_handle_is_text(void *p);

/* Seek the underlying stdio FILE of a MicroHs BFILE handle.
 * whence: 0 = SEEK_SET, 1 = SEEK_CUR, 2 = SEEK_END.
 * Returns 0 on success, -1 on failure. */
int mhs_handle_seek(void *p, int whence, int64_t offset);

#endif
