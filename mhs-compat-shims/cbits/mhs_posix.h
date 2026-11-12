/* Prototypes for the MicroHs POSIX helpers in mhs_posix.c,
 * plus the libc functions used through bare 'foreign import ccall'
 * imports (which MicroHs resolves without a C header in scope).
 *
 * MicroHs emits a call for every 'foreign import ccall' into its
 * generated C file, which is compiled as its own translation unit;
 * without these prototypes the implicit declarations are rejected
 * (and would be unsound for pointer-sized types such as the
 * 'realpath' result).
 *
 * The libc declarations below use the actual platform types
 * ('mode_t', 'ssize_t', 'size_t') from the standard headers rather
 * than hand-written approximations, so this header never conflicts
 * with the system headers on macOS or Linux.  Include this header
 * via a header-qualified import, e.g.
 *   foreign import ccall "mhs_posix.h mhs_path_kind" ...
 */
#ifndef MHS_POSIX_H
#define MHS_POSIX_H

#include <sys/stat.h> /* mkdir, chmod, mode_t */
#include <sys/types.h> /* ssize_t, size_t */
#include <stdlib.h> /* realpath */
#include <unistd.h> /* rmdir, symlink, readlink */

int mhs_path_kind(const char *path, int follow);
int mhs_create_exclusive(const char *path, mode_t mode);
int mhs_path_mode(const char *path, long *mode);
int mhs_path_mtime(const char *path, long *sec, long *nsec);
int mhs_set_mtime(const char *path, long sec, long nsec);

/* libc functions used through bare foreign imports */
int rename(const char *oldpath, const char *newpath);
int rmdir(const char *path);
int mkdir(const char *path, mode_t mode);
int chmod(const char *path, mode_t mode);
char *realpath(const char *path, char *resolved);
int symlink(const char *target, const char *linkpath);
ssize_t readlink(const char *path, char *buf, size_t bufsize);

#endif /* MHS_POSIX_H */
