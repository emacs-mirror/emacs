/*
TEST_HEADER
 id = $Id$
 summary = regression test for GitHub issue #9
 language = c
 link = testlib.o myfmt.o
 parameters =
END_HEADER
*/

#if !defined(MPS_OS_FR) && !defined(MPS_OS_LI)

#include "testlib.h"

static void test(void *stack_pointer)
{
  /* nothing to do on non-POSIX systems */
}

#else

#define _POSIX_C_SOURCE 199309L /* for nanosleep */

#include "mpsavm.h"
#include "mpscamc.h"
#include "myfmt.h"
#include "testlib.h"

#include <errno.h>
#include <pthread.h>
#include <semaphore.h>
#include <string.h>
#include <unistd.h>

/* On POSIX systems, the MPS suspends threads by sending them a signal
 * (see PThreadextSuspend in pthrdext.c). If the signal was delivered
 * while the mutator was blocked in a system call like open() or
 * read() then the system call could formerly fail with EINTR.
 *
 * The test creates a thread. The thread registers itself with the
 * MPS, posts on a semaphore to synchronize with the main thread, and
 * then blocks itself in a read() system call. The main thread then
 * calls mps_arena_collect() to cause the MPS to suspend all other
 * threads. If the issue is not fixed, then the read() call on the
 * thread fails with EINTR. Alternatively, if the issue is fixed, the
 * read() call is automatically restarted, the main thread writes to
 * the other end of the pipe so that the read() call completes, and we
 * tear everything down.
 */

typedef struct test_data_s {
  mps_arena_t arena;            /* The arena. */
  sem_t *sem;                   /* Semaphore to post once registered. */
  int fd;                       /* File descriptor to read from. */
} test_data_t;

static void *thread_fn(void *arg)
{
  test_data_t *data = arg;
  mps_thr_t thread;
  int e;
  char c;

  die(mps_thread_reg(&thread, data->arena), "mps_thread_reg");
  e = sem_post(data->sem);
  asserts(e == 0, "sem_post: %s", strerror(errno));

  /* There is a race here: if MPS goes ahead with the collection on
   * the main thread and manages to signal this thread before the
   * read() system call is entered, then the test condition is not
   * exercised (no opportunity for read() to fail with EINTR). This is
   * an acceptable race because it doesn't make the test fail, it just
   * fails to exercise the test condition.
   *
   * POSIX doesn't provide us with a portable way to ensure that the
   * thread has blocked in the system call before starting the
   * collection; on Linux we could poll /proc/TID/stat until the
   * thread enters state S (sleeping in an interruptible wait) but
   * this seems like a lot of trouble for a nice-to-have feature.
   */
  e = read(data->fd, &c, 1);
  asserts(e == 1, "read: %d: %s", e, strerror(errno));
  mps_thread_dereg(thread);
  return NULL;
}

static void test(void *stack_pointer)
{
  mps_arena_t arena;
  mps_fmt_t format;
  mps_pool_t pool;
  int pipefd[2];
  int e;
  sem_t sem;
  test_data_t data;
  pthread_t pthread;
  struct timespec timespec = { 0, 1000000 };

  die(mps_arena_create_k(&arena, mps_arena_class_vm(), mps_args_none), "mps_arena_create_k");
  die(mps_fmt_create_A(&format, arena, &fmtA), "create format");
  MPS_ARGS_BEGIN(args) {
     MPS_ARGS_ADD(args, MPS_KEY_FORMAT, format);
     die(mps_pool_create_k(&pool, arena, mps_class_amc(), args), "mps_pool_create_k");
  } MPS_ARGS_END(args);

  /* Create data structures for synchronizing with the thread. */
  e = pipe(pipefd);
  asserts(e == 0, "pipe: %s", strerror(errno));
  e = sem_init(&sem, 0, 0);
  asserts(e == 0, "sem_init: %s", strerror(errno));
  data.arena = arena;
  data.sem = &sem;
  data.fd = pipefd[0];

  /* Start the thread. */
  e = pthread_create(&pthread, NULL, thread_fn, &data);
  asserts(e == 0, "pthread_create: %s", strerror(e));

  /* Wait for the thread to register itself. */
  e = sem_wait(&sem);
  asserts(e == 0, "sem_wait: %s", strerror(errno));

  /* Delay to give the thread more time to block in read(). */
  e = nanosleep(&timespec, NULL);
  asserts(e == 0, "nanosleep: %s", strerror(errno));

  /* Run a collection: this causes the MPS to suspend the thread via
   * ArenaCollect, ArenaStartCollect, TraceStartCollectAll,
   * TraceCondemnEnd, ShieldHold, shieldSuspend, ThreadRingSuspend, and
   * PThreadextSuspend. */
  mps_arena_collect(arena);

  /* Write to the pipe so that the thread can complete its read. */
  e = write(pipefd[1], "x", 1);
  asserts(e == 1, "write: %s", strerror(errno));

  /* Wait for the thread to complete. */
  e = pthread_join(pthread, NULL);
  asserts(e == 0, "pthread_join: %s", strerror(e));

  /* Tear down MPS data structures. */
  mps_arena_park(arena);
  mps_pool_destroy(pool);
  mps_fmt_destroy(format);
  mps_arena_destroy(arena);
}

#endif

int main(void)
{
  run_test(test);
  pass();
  return 0;
}
