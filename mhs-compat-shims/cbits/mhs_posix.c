/* MHS POSIX file-query helpers for the Cabal-syntax library.
 *
 * The MHS (MicroHs) build of Cabal-syntax cannot use the @unix@
 * package, and the C @struct stat@ layout is platform specific, so the
 * file-kind, modification-time and timestamp queries needed by
 * Distribution.Compat.Directory live here in C rather than behind
 * Haskell FFI.  Plain libc calls (@rename@, @rmdir@, @realpath@,
 * @symlink@, @readlink@, @mkdir@, @chmod@) are imported directly from
 * Haskell and need no helpers.
 *
 * The Haskell side (Distribution.Compat.Internal.MhsPosix) only calls
 * the three functions declared below.
 *
 * On non-POSIX platforms (notably Windows) these functions are stubs
 * that fail; the MHS port targets POSIX systems.
 */

#if defined(_WIN32)

/* Stubs so that the file still compiles where <sys/stat.h> lacks the
 * POSIX members used below. */

#include <errno.h>

int mhs_path_kind(const char *path, int follow)
{
  (void) path; (void) follow;
  errno = ENOSYS;
  return -1;
}

int mhs_path_mode(const char *path, long *mode)
{
  (void) path; (void) mode;
  errno = ENOSYS;
  return -1;
}

int mhs_path_mtime(const char *path, long *sec, long *nsec)
{
  (void) path; (void) sec; (void) nsec;
  errno = ENOSYS;
  return -1;
}

int mhs_set_mtime(const char *path, long sec, long nsec)
{
  (void) path; (void) sec; (void) nsec;
  errno = ENOSYS;
  return -1;
}

int mhs_create_exclusive(const char *path, int mode)
{
  (void) path; (void) mode;
  errno = ENOSYS;
  return -1;
}

#else /* POSIX */

#define _POSIX_C_SOURCE 200809L

#include <errno.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>
#include <utime.h>

/* Check the header declarations against these definitions in the
 * same translation unit.  Kept inside the POSIX branch: the header
 * needs <unistd.h>, which does not exist on Windows (whose stubs
 * above are unchanged). */
#include "mhs_posix.h"

/* Classify a path: 0 = missing, 1 = regular file, 2 = directory,
 * 3 = symbolic link, 4 = anything else, -1 = error (errno set).
 * With @follow@ set, symbolic links are followed (like @stat@);
 * otherwise the link itself is reported (like @lstat@).  A dangling
 * link reported with @follow@ set counts as missing, matching the
 * behaviour of the @directory@ package queries.
 */
int
mhs_path_kind(const char *path, int follow)
{
  struct stat st;

  if (follow ? stat(path, &st) != 0 : lstat(path, &st) != 0) {
    if (errno == ENOENT || errno == ENOTDIR) {
      return 0;
    }
    return -1;
  }
  if (!follow && S_ISLNK(st.st_mode)) {
    return 3;
  }
  if (S_ISREG(st.st_mode)) {
    return 1;
  }
  if (S_ISDIR(st.st_mode)) {
    return 2;
  }
  return 4;
}

/* Fetch the raw mode bits of @path@ (following links), as for
 * @st_mode@.  Returns 0 on success, -1 with errno set.
 */
int
mhs_path_mode(const char *path, long *mode)
{
  struct stat st;

  if (stat(path, &st) != 0) {
    return -1;
  }
  *mode = (long) st.st_mode;
  return 0;
}

/* Fetch the modification time of @path@ (following links) as seconds
 * plus nanoseconds.  Returns 0 on success, -1 with errno set.
 */
int
mhs_path_mtime(const char *path, long *sec, long *nsec)
{
  struct stat st;

  if (stat(path, &st) != 0) {
    return -1;
  }
#if defined(__APPLE__) && (!defined(_POSIX_C_SOURCE) || defined(_DARWIN_C_SOURCE))
  *sec = (long) st.st_mtimespec.tv_sec;
  *nsec = (long) st.st_mtimespec.tv_nsec;
#elif defined(__APPLE__)
  /* Strict POSIX view (this file defines _POSIX_C_SOURCE itself):
   * st_mtime plus st_mtimensec. */
  *sec = (long) st.st_mtime;
  *nsec = (long) st.st_mtimensec;
#else
  *sec = (long) st.st_mtim.tv_sec;
  *nsec = (long) st.st_mtim.tv_nsec;
#endif
  return 0;
}

/* Set the modification time of @path@ (following links), leaving the
 * access time unchanged where the platform allows.  Returns 0 on
 * success, -1 with errno set.
 */
int
mhs_set_mtime(const char *path, long sec, long nsec)
{
#if defined(__linux__) || defined(__APPLE__)
  struct timespec ts[2];
  ts[0].tv_sec = 0;
  ts[0].tv_nsec = UTIME_OMIT;
  ts[1].tv_sec = (time_t) sec;
  ts[1].tv_nsec = (long) nsec;
  return utimensat(AT_FDCWD, path, ts, 0);
#else
  /* No sub-second precision portably here; atime follows mtime. */
  (void) nsec;
  {
    struct utimbuf tb;
    tb.actime = (time_t) sec;
    tb.modtime = (time_t) sec;
    return utime(path, &tb);
  }
#endif
}

/* Atomically create a regular file, failing if the path already exists
 * (O_EXCL also refuses pre-existing symlinks, dangling or otherwise).
 * Returns 0 on success, 1 if the path exists, -1 with errno set on
 * other errors.
 */
int
mhs_create_exclusive(const char *path, mode_t mode)
{
  int fd = open(path, O_RDWR | O_CREAT | O_EXCL, mode);
  if (fd >= 0) {
    close(fd);
    return 0;
  }
  if (errno == EEXIST) {
    return 1;
  }
  return -1;
}

#endif /* _WIN32 */
