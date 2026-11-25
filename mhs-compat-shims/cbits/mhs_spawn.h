/* Prototypes for the MicroHs process helpers in mhs_spawn.c.
 *
 * MicroHs emits a call for every 'foreign import ccall' into its
 * generated C file, which is compiled as its own translation unit;
 * without these prototypes the implicit declarations are rejected
 * (and would be unsound for pointer-sized types).  Include this
 * header via a header-qualified import, e.g.
 *   foreign import ccall "mhs_spawn.h mhs_spawn" ...
 */
#ifndef MHS_SPAWN_H
#define MHS_SPAWN_H

int mhs_spawn(const char *prog, char *const *argv,
              const char *std_in, int std_in_fd,
              const char *std_out, int std_out_fd,
              const char *std_err, int std_err_fd,
              char *const *envp);
int mhs_wait_exited(int pid, int *code);
int mhs_terminate(int pid);

#endif /* MHS_SPAWN_H */
