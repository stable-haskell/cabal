/* MHS process-spawning helpers for the Cabal library.
 *
 * The MHS (MicroHs) build of Cabal cannot use the @process@ package, so
 * process creation is implemented on top of @posix_spawnp(3)@ plus
 * @waitpid(2)@ and @kill(2)@.  The @posix_spawn_file_actions_t@ type is
 * opaque and its size and layout are platform specific, so all direct
 * manipulation of it lives here in C rather than behind Haskell FFI.
 * The Haskell side (Distribution.Compat.Internal.MhsProcess) only calls
 * the three functions declared below, passing plain integers, file
 * paths, and argument vectors.
 *
 * On non-POSIX platforms (notably Windows) these functions are stubs
 * that fail; the MHS port targets POSIX systems.
 */

#include "mhs_spawn.h"

#if defined(_WIN32)

/* Stubs so that the file still compiles where <spawn.h> is missing. */

#include <errno.h>

int mhs_spawn(const char *prog, char *const *argv,
              const char *std_in, int std_in_fd,
              const char *std_out, int std_out_fd,
              const char *std_err, int std_err_fd,
              char *const *envp)
{
  (void) prog; (void) argv;
  (void) std_in; (void) std_in_fd;
  (void) std_out; (void) std_out_fd;
  (void) std_err; (void) std_err_fd;
  (void) envp;
  errno = ENOSYS;
  return -1;
}

int mhs_wait_exited(int pid, int *code)
{
  (void) pid; (void) code;
  errno = ENOSYS;
  return -1;
}

int mhs_terminate(int pid)
{
  (void) pid;
  errno = ENOSYS;
  return -1;
}

#else /* POSIX */

#define _POSIX_C_SOURCE 200809L

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <spawn.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#if defined(__APPLE__)
#include <crt_externs.h>
#define MHS_ENVIRON (*_NSGetEnviron())
#else
extern char **environ;
#define MHS_ENVIRON environ
#endif

/* Redirect one standard stream for the child to be spawned.
 *
 * A non-NULL path redirects the stream to that file (read-only for
 * file descriptor 0, write-only otherwise).  Otherwise a non-negative
 * fd is duplicated onto the target descriptor.  Otherwise the stream
 * is inherited unchanged.
 */
static int
mhs_add_redirect(posix_spawn_file_actions_t *fa, int target,
                 const char *path, int fd)
{
  if (path != 0) {
    int flags = (target == 0) ? O_RDONLY : O_WRONLY;
    /* The files are pre-created by the caller, so no O_CREAT here. */
    return posix_spawn_file_actions_addopen(fa, target, path, flags, 0600);
  }
  if (fd >= 0) {
    return posix_spawn_file_actions_adddup2(fa, fd, target);
  }
  return 0;
}

/* Spawn @prog@ (looked up along PATH) with the given argument vector.
 * Returns the child pid, or -1 with errno set on failure.
 */
int
mhs_spawn(const char *prog, char *const *argv,
          const char *std_in, int std_in_fd,
          const char *std_out, int std_out_fd,
          const char *std_err, int std_err_fd,
          char *const *envp)
{
  posix_spawn_file_actions_t fa;
  pid_t pid = (pid_t) -1;
  int rc;

  rc = posix_spawn_file_actions_init(&fa);
  if (rc != 0) {
    errno = rc;
    return -1;
  }

  rc = mhs_add_redirect(&fa, 0, std_in, std_in_fd);
  if (rc == 0) {
    rc = mhs_add_redirect(&fa, 1, std_out, std_out_fd);
  }
  if (rc == 0) {
    rc = mhs_add_redirect(&fa, 2, std_err, std_err_fd);
  }
  if (rc != 0) {
    int e = rc;
    posix_spawn_file_actions_destroy(&fa);
    errno = e;
    return -1;
  }

  /* A NULL environment inherits the parent environment; otherwise
   * the child gets exactly the given bindings.  Executable lookup
   * still uses the parent PATH, matching System.Process. */
  rc = posix_spawnp(&pid, prog, &fa, (const posix_spawnattr_t *) 0,
                    argv, envp ? envp : MHS_ENVIRON);
  posix_spawn_file_actions_destroy(&fa);
  if (rc != 0) {
    errno = rc;
    return -1;
  }
  return (int) pid;
}

/* Non-blocking reaping probe.
 *
 * Returns 1 with @*code@ set when the child has exited (the exit
 * status, or the negated signal number when killed by a signal),
 * 0 when the child is still running, and -1 with errno set on error.
 */
int
mhs_wait_exited(int pid, int *code)
{
  int status;
  pid_t r;

  do {
    r = waitpid((pid_t) pid, &status, WNOHANG);
  } while (r == (pid_t) -1 && errno == EINTR);

  if (r == 0) {
    return 0;
  }
  if (r == (pid_t) -1) {
    return -1;
  }
  if (WIFEXITED(status)) {
    *code = WEXITSTATUS(status);
    return 1;
  }
  if (WIFSIGNALED(status)) {
    *code = -WTERMSIG(status);
    return 1;
  }
  /* Stopped or continued: keep polling. */
  return 0;
}

/* Deliver SIGTERM to the child.  Returns 0 on success, -1 with errno
 * set on failure.
 */
int
mhs_terminate(int pid)
{
  return kill((pid_t) pid, SIGTERM);
}

#endif /* _WIN32 */
