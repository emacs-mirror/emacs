/*
TEST_HEADER
 id = $Id$
 summary = regression test for GitHub issue #61
 language = c
 link = testlib.o newfmt.o
 parameters =
END_HEADER
*/

#include "testlib.h"

#if !defined(MPS_OS_W3)

static void test(void *stack_pointer)
{
  /* nothing to do on non-Windows systems */
}

#else

#include "mpsavm.h"
#include "mpscamc.h"
#include "newfmt.h"

#include <stdio.h>

#include <Windows.h>

/* On Windows, the MPS uses a vectored exception handler to intercept
 * and handle protection errors. These handlers need to save and
 * restore the value inside GetLastError(), otherwise the mutator may
 * observe that GetLastError() has ben cleared or set to something
 * else (especially if the MPS executed a system call that failed).
 *
 * This test first sets up an AMC pool with the new format in
 * newfmt.h. Then it creates a single root node with room for 100
 * elements, and populates them with nodes as well. Each of these
 * created nodes are populated with pointers from the root node to
 * give the MPS a reason to protect them.
 *
 * We then start looking for nodes residing in pages where the barrier
 * have been raised (both for reads and writes). We continuously
 * allocate new nodes for even indices in the root node. This will
 * trigger the MPS to do garbage collections, and only updating even
 * nodes will ensure that we will end up with pointers to both objects
 * in the nursery generation and in the older generation, increasing
 * the likelyhood of us finding a page with the barrier active.
 *
 * When we find a page with the read and write barrier active, we
 * store that pointer and set up the test we want to perform. We want
 * to set up a situation where we can trigger a barrier hit, and that
 * the MPS will call a system call that fails in the exception handler
 * when responding to the hit.
 *
 * The tool we use here is a separate thread. We create and register
 * an additional thread with the MPS. Then we keep that thread alive
 * until we know that the next thing the MPS will do is the barrier
 * hit (if it fails to halt the thread once, it assumes the thread is
 * no longer alive and will no longer try to halt it again). As such,
 * we terminate the thread when we have found a page that will trigger
 * the hit. Then, we can call SetLastError() with som error code, read
 * from the memory with a barrier. This triggers the exception
 * handler, which will realize that it needs to scan the particular
 * page, and thus need to stop threads. In this case, we know that the
 * SuspendThread() call will fail and thus clobbers the value in
 * GetLastError(). Then we can verify that the main thread does not
 * observe this change. This serves as a good (and hopefully,
 * reliable) illustration of what could happen, even if it is
 * tecnically a bad thing to have a terminated thread registered with
 * the MPS.
 */

/* Error code we check for. We pick an error unlikely to happen in the
 * MPS. Typically we see error code 5 (ERROR_ACCESS_DENIED) from
 * SuspendThread() in this case, so anything but that should work.
 */
#define CHECK_ERROR_CODE ERROR_NETWORK_BUSY

/* Number of cells to allocate. */
#define NUM_CELLS 100

/* Maximum attempts before giving up. It seems like 10 or so is enough
 * in most cases.
 */
#define MAX_ATTEMPTS 100

/* Get memory protection of the page containing the specified address. */
static DWORD memory_protection(void *ptr)
{
  MEMORY_BASIC_INFORMATION info;
  if (VirtualQuery(ptr, &info, sizeof(info)) == 0) {
    printf("VirtualQuery failed\n");
    fail();
  }
  return info.Protect;
}

/* Find a cell linked to from 'root' that is in a read- and write-protected page. */
static mycell *any_protected(mycell *root)
{
  int i;
  for (i = 0; i < NUM_CELLS; i++) {
    mycell *cell = getref(root, i);
    DWORD protection = memory_protection(cell);

    switch (protection) {
    case PAGE_EXECUTE:
    case PAGE_NOACCESS:
      return cell;
    }
  }

  return NULL;
}

/* Allocate an object and fill it with some pointers to encourage the
 * MPS to raise the read- and write-barrier for the object at some point.
 */
static mycell *alloc_cell(mps_ap_t ap, mycell *root)
{
  int i;
  mycell *cell = allocone(ap, NUM_CELLS);
  for (i = 0; i < NUM_CELLS; i++)
    setref(cell, i, getref(root, i));
  return cell;
}

/* Variables shared by the spawned thread and the main thread. */
static mps_arena_t arena;
static mps_thr_t extra_thread;
static HANDLE terminate_thread;

/* Function for the extra thread. The thread registers itself with the
 * MPS, waits until the main thread asks it to terminate, and then
 * exits.
 */
static DWORD WINAPI thread_fn(LPVOID parameter) {
  mps_thread_reg(&extra_thread, arena);
  WaitForSingleObject(terminate_thread, INFINITE);
  return 0;
}

static void test(void *stack_pointer)
{
  mps_fmt_t format;
  mps_pool_t pool;
  mps_thr_t thread;
  mps_root_t root;
  mps_chain_t chain;
  mps_ap_t ap;
  int i, j;

  mycell *root_cell;
  mycell *protected;

  HANDLE thread_handle;
  DWORD after_trip;

  mps_gen_param_s gen_params[2] = {
    /* Make the first generation fairly small to trigger the case we want. */
    { 512, 0.9 },
    { 10 * 1024, 0.5 }
  };

  die(mps_arena_create_k(&arena, mps_arena_class_vm(), mps_args_none), "mps_arena_create_k");
  die(mps_fmt_create_A(&format, arena, &fmtA), "create format");
  die(mps_chain_create(&chain, arena, 2, gen_params), "mps_chain_create");
  die(mps_thread_reg(&thread, arena), "mps_thread_reg");
  die(mps_root_create_thread(&root, arena, thread, stack_pointer), "mps_root_create_thread");
  MPS_ARGS_BEGIN(args) {
     MPS_ARGS_ADD(args, MPS_KEY_FORMAT, format);
     MPS_ARGS_ADD(args, MPS_KEY_CHAIN, chain);
     die(mps_pool_create_k(&pool, arena, mps_class_amc(), args), "mps_pool_create_k");
  } MPS_ARGS_END(args);

  die(mps_ap_create_k(&ap, pool, mps_args_none), "mps_ap_create_k");

  /* Launch the extra thread, keep it alive until we are ready. We
   * need to keep it alive since the MPS will stop trying to stop the
   * thread during collections as soon as it realizes that it has
   * terminated. Since this happens during collections, we don't want
   * it to terminate too early.
   */
  terminate_thread = CreateSemaphore(NULL, 0, 1, NULL);
  thread_handle = CreateThread(NULL, 0, thread_fn, NULL, 0, NULL);

  /* Allocate a number of cells to start with. */
  root_cell = allocone(ap, NUM_CELLS);
  for (i = 0; i < NUM_CELLS; i++) {
    setref(root_cell, i, alloc_cell(ap, root_cell));
  }

  /* Now, continue to allocate things, forcing the MPS to do
   * incremental collections from time to time. We do this until we
   * find that the MPS is in the middle of a collection and has raised
   * the read- and write-barrier for some of the objects.
   */
  for (i = 0; i < MAX_ATTEMPTS; i++) {
    /* Fill every other entry with new objects. This way, some will be
     * in the nursery generation, and others will be in higher generations.
     */
    for (j = 0; j < NUM_CELLS; j += 2)
      setref(root_cell, j, alloc_cell(ap, root_cell));

    /* Ask for some collection work (a very small amount). */
    mps_arena_step(arena, 0.0001, 10000);

    /* See if we are done. */
    protected = any_protected(root_cell);
    if (protected)
      break;
  }

  /* Make sure we actually triggered the situation we are looking for. */
  asserts(protected != NULL, "Failed to find a protected page after many attempts.");

  /* Signal the semaphore and wait for the other thread to exit. In
   * this way, we know that the MPS will try (and fail) to stop the
   * thread when we hit the barrier later.
   */
  ReleaseSemaphore(terminate_thread, 1, NULL);
  WaitForSingleObject(thread_handle, INFINITE);

  /* Now we have a page that will trip the barrier when we write to
   * it. So we do that and observe the value of GetLastError().
   */
  SetLastError(CHECK_ERROR_CODE);

  /* Read from it. In the current implementation, reads are handled by
   * scanning (which is what we want to trigger since that requires
   * suspending threads), which is enough. A write would also work. */
  getref(protected, 0);

  /* Check so that the error code we set is preserved. Typically,
   * GetLastError() will be 5 if it is not preserved (that is what
   * SuspendThread() sets it to on failure). */
  after_trip = GetLastError();
  asserts(after_trip == CHECK_ERROR_CODE, "Errno was not properly preserved in the exception handler!");

  /* Tear down MPS data structures. */
  mps_arena_park(arena);
  mps_ap_destroy(ap);
  mps_root_destroy(root);
  mps_thread_dereg(thread);
  mps_thread_dereg(extra_thread);
  mps_pool_destroy(pool);
  mps_chain_destroy(chain);
  mps_fmt_destroy(format);
  mps_arena_destroy(arena);

  /* Other cleanup. */
  CloseHandle(thread_handle);
  CloseHandle(terminate_thread);
}

#endif

int main(void)
{
  run_test(test);
  pass();
  return 0;
}
