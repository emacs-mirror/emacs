/*
 * susp.c
 *
 * Demonstrate an implementation of thread suspend and resume (similar
 * to the Solaris thr_suspend/thr_continue functions) using portable
 * POSIX functions.
 *
 * Note 1: Use of suspend and resume requires extreme care. You can
 * easily deadlock your application by suspending a thread that holds
 * some resource -- for example, a thread calling printf to print a
 * message may have libc mutexes locked, and no other thread will be
 * able to return from printf until the suspended thread is resumed.
 *
 * Note 2: This program is called "susp" rather than "suspend" to
 * avoid confusion (by your shell) with the suspend command.
 *
 * Note 3: This simple program will fail if any thread terminates
 * during the test. The value of ITERATIONS must be adjusted to a
 * value sufficiently large that the main thread can complete its two
 * suspend/continue loops before any thread_routine threads terminate.
 * */
#include <pthread.h>
#include <sched.h>
#include <signal.h>
#include <semaphore.h>
#include "errors.h"

#define THREAD_COUNT    20
#define ITERATIONS      80000

typedef struct {
    int		inuse;				/* 1 if in use, else 0 */
    pthread_t	id;				/* Thread ID */
    } Victim_t;

sem_t sem;
int thread_count = THREAD_COUNT;
int iterations = ITERATIONS;
pthread_mutex_t mut = PTHREAD_MUTEX_INITIALIZER;
pthread_once_t once = PTHREAD_ONCE_INIT;
Victim_t *array = NULL;
int bottom = 0;

/*
 * Handle SIGUSR1 in the target thread, to suspend it until receiving
 * SIGUSR2 (resume). Note that this is run with both SIGUSR1 and
 * SIGUSR2 blocked. Having SIGUSR2 blocked prevents a resume before we
 * can finish the suspend protocol.
 */
void
suspend_signal_handler (int sig)
{
    sigset_t signal_set;

    /*
     * Block all signals except SIGUSR2 while suspended.
     */
    sigfillset (&signal_set);
    sigdelset (&signal_set, SIGUSR2);
    sem_post (&sem);
    sigsuspend (&signal_set);

    /*
     * Once here, the resume signal handler has run to completion.
     */
    return;
}

/*
 * Handle SIGUSR2 in the target thread, to resume it. Note that
 * the signal handler does nothing. It exists only because we need
 * to cause sigsuspend() to return.
 */
void
resume_signal_handler (int sig)
{
    return;
}

/*
 * Dynamically initialize the "suspend package" when first used
 * (called by pthread_once).
 */
void
suspend_init_routine (void)
{
    int status;
    struct sigaction sigusr1, sigusr2;

    /*
     * Initialize a semaphore, to be used by the signal handler to
     * confirm suspension. We only need one, because suspend & resume
     * are fully serialized by a mutex.
     */
    status = sem_init (&sem, 0, 1);
    if (status == -1)
        errno_abort ("Initializing semaphore");

    /*
     * Allocate the suspended threads array. This array is used to guarantee
     * idempotency.
     */
    bottom = 10;
    array = (Victim_t*) calloc (bottom, sizeof (Victim_t));

    /*
     * Install the signal handlers for suspend/resume.
     *
     * We add SIGUSR2 to the sa_mask field for the SIGUSR1 handler. That
     * avoids a race if one thread suspends the target while another resumes
     * that same target. (The SIGUSR2 signal cannot be delivered before the
     * target thread calls sigsuspend.)
     */
    sigusr1.sa_flags = 0;
    sigusr1.sa_handler = suspend_signal_handler;
    sigemptyset (&sigusr1.sa_mask);
    sigaddset (&sigusr1.sa_mask, SIGUSR2);

    sigusr2.sa_flags = 0;
    sigusr2.sa_handler = resume_signal_handler;
    sigemptyset (&sigusr2.sa_mask);

    status = sigaction (SIGUSR1, &sigusr1, NULL);
    if (status == -1)
        errno_abort ("Installing suspend handler");
    
    status = sigaction (SIGUSR2, &sigusr2, NULL);
    if (status == -1)
        errno_abort ("Installing resume handler");
    
    return;
}

/*
 * Suspend a thread by sending it a signal (SIGUSR1), which will
 * block the thread until another signal (SIGUSR2) arrives.
 *
 * Multiple calls to thd_suspend for a single thread have no
 * additional effect on the thread -- a single thd_continue
 * call will cause it to resume execution.
 */
int 
thd_suspend (pthread_t target_thread)
{
    int status;
    int i;

    /*
     * The first call to thd_suspend will initialize the
     * package.
     */
    status = pthread_once (&once, suspend_init_routine);
    if (status != 0)
        return status;

    /* 
     * Serialize access to suspend, makes life easier
     */
    status = pthread_mutex_lock (&mut);
    if (status != 0)
        return status;

    /*
     * Threads that are suspended are added to the target_array; a request to
     * suspend a thread already listed in the array is ignored.
     */
    for (i = 0; i < bottom; i++) {
        if (array[i].inuse
	    && pthread_equal (array[i].id, target_thread)) {
            status = pthread_mutex_unlock (&mut);
            return status;
	}
    }

    /*
     * Ok, we really need to suspend this thread. So, lets find the location
     * in the array that we'll use. If we run off the end, realloc the array
     * for more space. We allocate 2 new array slots; we'll use one, and leave 
     * the other for the next time.
     */
    i = 0;
    while (i < bottom && array[i].inuse)
        i++;

    if (i >= bottom) {
	bottom += 2;
        array = (Victim_t*)realloc (
            array, (bottom * sizeof (Victim_t)));
        if (array == NULL) {
            pthread_mutex_unlock (&mut);
            return errno;
        }
        array[bottom-1].inuse = 0;	/* Clear new last entry */
    }

    /*
     * Initialize the target's data. We initialize a semaphore to synchronize
     * with the signal handler. After sending the signal, we wait until the
     * signal handler posts the semaphore. Then we can destroy the semaphore,
     * because we won't need it again.
     */
    array[i].id = target_thread;
    array[i].inuse = 1;

    status = pthread_kill (target_thread, SIGUSR1);
    if (status != 0) {
        pthread_mutex_unlock (&mut);
        return status;
    }

    /*
     * Wait for the victim to acknowledge suspension.
     */
    while ((status = sem_wait (&sem)) != 0) {
	if (errno != EINTR) {
	    pthread_mutex_unlock (&mut);
	    return errno;
	}
    }
	
    status = pthread_mutex_unlock (&mut);
    return status;
}

/*
 * Resume a suspended thread by sending it SIGUSR2 to break
 * it out of the sigsuspend() in which it's waiting. If the
 * target thread isn't suspended, return with success.
 */
int 
thd_continue (pthread_t target_thread)
{
    int status;
    int i = 0;

    /*
     * The first call to thd_suspend will initialize the package.
     */
    status = pthread_once (&once, suspend_init_routine);
    if (status != 0)
        return status;

    /* 
     * Serialize access to suspend, makes life easier.
     */
    status = pthread_mutex_lock (&mut);
    if (status != 0)
        return status;

    /*
     * Make sure the thread is in the suspend array. If not, it hasn't been
     * suspended (or it has already been resumed) and we can just carry on.
     */
    while (i < bottom && array[i].inuse
	   && pthread_equal (array[i].id, target_thread))
        i++;

    if (i >= bottom) {
        pthread_mutex_unlock (&mut);
        return 0;
    }

    /*
     * Signal the thread to continue, and remove the thread from
     * the suspended array.
     */
    status = pthread_kill (target_thread, SIGUSR2);
    if (status != 0) {
        pthread_mutex_unlock (&mut);
        return status;
    }

    array[i].inuse = 0;			/* Clear array element */
    status = pthread_mutex_unlock (&mut);
    return status;
}

static void *
thread_routine (void *arg)
{
    int number = (int)arg;
    int status;
    int i;
    char buffer[128];

    for (i = 1; i <= iterations; i++) {
        /*
	 * Every time each thread does 2000 interations, print a progress
         * report.
         */
        if (i % 2000 == 0) {
            sprintf (
                buffer, "Thread %02d: %d\n",
                number, i);
            write (1, buffer, strlen (buffer));
        }

        sched_yield ();
    }

    return (void *)0;
}

int
main (int argc, char *argv[])
{
    pthread_t threads[THREAD_COUNT];
    pthread_attr_t detach;
    int status;
    void *result;
    int i;

    status = pthread_attr_init (&detach);
    if (status != 0)
        err_abort (status, "Init attributes object");
    status = pthread_attr_setdetachstate (
        &detach, PTHREAD_CREATE_DETACHED);
    if (status != 0)
        err_abort (status, "Set create-detached");

    for (i = 0; i< THREAD_COUNT; i++) {
        status = pthread_create (
            &threads[i], &detach, thread_routine, (void *)i);
        if (status != 0)
            err_abort (status, "Create thread");
    }

    sleep (1);

    for (i = 0; i < THREAD_COUNT/2; i++) {
        printf ("Suspending thread %d.\n", i);
        status = thd_suspend (threads[i]);
        if (status != 0)
            err_abort (status, "Suspend thread");
    }

    printf ("Sleeping ...\n");
    sleep (1);

    for (i = 0; i < THREAD_COUNT/2; i++) {
        printf ("Continuing thread %d.\n", i);
        status = thd_continue (threads[i]);
        if (status != 0)
            err_abort (status, "Suspend thread");
    }

    for (i = THREAD_COUNT/2; i < THREAD_COUNT; i++) {
        printf ("Suspending thread %d.\n", i);
        status = thd_suspend (threads[i]);
        if (status != 0)
            err_abort (status, "Suspend thread");
    }

    printf ("Sleeping ...\n");
    sleep (1);

    for (i = THREAD_COUNT/2; i < THREAD_COUNT; i++) {
        printf ("Continuing thread %d.\n", i);
        status = thd_continue (threads[i]);
        if (status != 0)
            err_abort (status, "Continue thread");
    }

    /*
     * Request that each thread terminate. We don't bother waiting for them;
     * just trust that they will terminate in "reasonable time". When the last
     * thread exits, the process will exit.
     */
    for (i = 0; i < THREAD_COUNT; i++)
	pthread_cancel (threads[i]);

    pthread_exit (NULL);        /* Let threads finish */
}
