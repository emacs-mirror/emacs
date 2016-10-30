/* Copyright (c) 2005 Russ Cox, MIT; see COPYRIGHT */

#include "taskimpl.h"
#include <fcntl.h>
#include <stdio.h>

#ifdef LIBTASK_USE_FIBER
#define STRICT
#define NOMINMAX
#define UNICODE
#include <windows.h>
#endif

#ifdef LIBTASK_USE_PTHREAD
#include <pthread.h>
#endif

int	taskdebuglevel;
int	taskcount;
int	tasknswitch;
int	taskexitval;
Task	*taskrunning;

Context	taskschedcontext;
Tasklist	taskrunqueue;

Task	**alltask;
int		nalltask;

static char *argv0;
static	void		contextswitch(Context *from, Context *to);

static void
taskdebug(char *fmt, ...)
{
	va_list arg;
	char buf[128];
	Task *t;
	char *p;
	static int fd = -1;

return;
	va_start(arg, fmt);
	vfprint(1, fmt, arg);
	va_end(arg);
return;

	if(fd < 0){
		p = strrchr(argv0, '/');
		if(p)
			p++;
		else
			p = argv0;
		snprint(buf, sizeof buf, "/tmp/%s.tlog", p);
		if((fd = open(buf, O_CREAT|O_WRONLY, 0666)) < 0)
			fd = open("/dev/null", O_WRONLY);
	}

	va_start(arg, fmt);
	vsnprint(buf, sizeof buf, fmt, arg);
	va_end(arg);
	t = taskrunning;
	if(t)
		fprint(fd, "%d.%d: %s\n", getpid(), t->id, buf);
	else
		fprint(fd, "%d._: %s\n", getpid(), buf);
}

#if ! defined LIBTASK_USE_FIBER && ! defined LIBTASK_USE_PTHREAD
static void
taskstart(uint y, uint x)
{
	Task *t;
	ulong z;

	z = x<<16;	/* hide undefined 32-bit shift from 32-bit compilers */
	z <<= 16;
	z |= y;
	t = (Task*)z;

//print("taskstart %p\n", t);
	t->startfn(t->startarg);
//print("taskexits %p\n", t);
	taskexit(0);
//print("not reacehd\n");
}
#endif

#ifdef LIBTASK_USE_PTHREAD
static void *
thread_func (void *arg)
{
  Task *t = arg;
  ucontext_t *uc = &t->context.uc;
  if (pthread_mutex_lock (&uc->mutex) != 0) abort ();
  while (! uc->running)
    {
      if (pthread_cond_wait (&uc->cond, &uc->mutex) != 0) abort ();
    }
  if (pthread_mutex_unlock (&uc->mutex) != 0) abort ();
  t->startfn (t->startarg);
  return NULL;
}
#endif

static int taskidgen;

static Task*
taskalloc(void (*fn)(void*), void *arg, uint stack)
{
	Task *t;
	sigset_t zero;
	uint x, y;
	ulong z;

#ifdef LIBTASK_USE_FIBER
        t = malloc (sizeof *t);
#else
	/* allocate the task and stack together */
	t = malloc(sizeof *t+stack);
#endif
	if(t == nil){
		fprint(2, "taskalloc malloc: %r\n");
		abort();
	}
	memset(t, 0, sizeof *t);
#ifdef LIBTASK_USE_FIBER
        t->context.uc.fiber = CreateFiber (stack, fn, arg);
        if (t->context.uc.fiber == NULL)
          abort ();
#else
#ifdef LIBTASK_USE_PTHREAD
        if (pthread_mutex_init (&t->context.uc.mutex, NULL) != 0)
          abort ();
        if (pthread_cond_init (&t->context.uc.cond, NULL) != 0)
          abort ();
        {
          pthread_attr_t attr;
          if (pthread_attr_init (&attr) != 0)
            abort ();
          if (stack < PTHREAD_STACK_MIN)
            stack = PTHREAD_STACK_MIN;
          long pagesize = sysconf (_SC_PAGESIZE);
          if (stack % pagesize != 0)
            stack += pagesize - stack % pagesize;
          if (pthread_attr_setstacksize (&attr, stack) != 0)
            abort ();
          if (pthread_create (&t->context.uc.thread, &attr, thread_func, t) != 0)
            abort ();
          if (pthread_attr_destroy (&attr) != 0)
            abort ();
        }
#else
	t->stk = (uchar*)(t+1);
	t->stksize = stack;
#endif
#endif
	t->id = ++taskidgen;
#ifndef LIBTASK_USE_FIBER
	t->startfn = fn;
	t->startarg = arg;
#endif

#ifdef EMACS
        init_emacs_lisp_context (t->id == 1, &t->context.ec);
#endif

#if ! defined LIBTASK_USE_FIBER && ! defined LIBTASK_USE_PTHREAD
	/* do a reasonable initialization */
	memset(&t->context.uc, 0, sizeof t->context.uc);
	sigemptyset(&zero);
	sigprocmask(SIG_BLOCK, &zero, &t->context.uc.uc_sigmask);

	/* must initialize with current context */
	if(getcontext(&t->context.uc) < 0){
		fprint(2, "getcontext: %r\n");
		abort();
	}

	/* call makecontext to do the real work. */
	/* leave a few words open on both ends */
	t->context.uc.uc_stack.ss_sp = t->stk+8;
	t->context.uc.uc_stack.ss_size = t->stksize-64;
#if defined(__sun__) && !defined(__MAKECONTEXT_V2_SOURCE)		/* sigh */
#warning "doing sun thing"
	/* can avoid this with __MAKECONTEXT_V2_SOURCE but only on SunOS 5.9 */
	t->context.uc.uc_stack.ss_sp =
		(char*)t->context.uc.uc_stack.ss_sp
		+t->context.uc.uc_stack.ss_size;
#endif
	/*
	 * All this magic is because you have to pass makecontext a
	 * function that takes some number of word-sized variables,
	 * and on 64-bit machines pointers are bigger than words.
	 */
//print("make %p\n", t);
	z = (ulong)t;
	y = z;
	z >>= 16;	/* hide undefined 32-bit shift from 32-bit compilers */
	x = z>>16;
	makecontext(&t->context.uc, (void(*)())taskstart, 2, y, x);
#endif

	return t;
}

int
taskcreate(void (*fn)(void*), void *arg, uint stack)
{
	int id;
	Task *t;

	t = taskalloc(fn, arg, stack);
	taskcount++;
	id = t->id;
	if(nalltask%64 == 0){
		alltask = realloc(alltask, (nalltask+64)*sizeof(alltask[0]));
		if(alltask == nil){
			fprint(2, "out of memory\n");
			abort();
		}
	}
	t->alltaskslot = nalltask;
	alltask[nalltask++] = t;
	taskready(t);
	return id;
}

void
tasksystem(void)
{
	if(!taskrunning->system){
		taskrunning->system = 1;
		--taskcount;
	}
}

void
taskswitch(void)
{
	needstack(0);
	contextswitch(&taskrunning->context, &taskschedcontext);
}

void
taskready(Task *t)
{
	t->ready = 1;
	addtask(&taskrunqueue, t);
}

int
taskyield(void)
{
	int n;

	n = tasknswitch;
	taskready(taskrunning);
	taskstate("yield");
	taskswitch();
	return tasknswitch - n - 1;
}

int
anyready(void)
{
	return taskrunqueue.head != nil;
}

void
taskexitall(int val)
{
	exit(val);
}

void
taskexit(int val)
{
	taskexitval = val;
	taskrunning->exiting = 1;
	taskswitch();
}

static void
contextswitch(Context *from, Context *to)
{
#ifdef EMACS
        switch_emacs_lisp_context (&from->ec, &to->ec);
#endif
	if(swapcontext(&from->uc, &to->uc) < 0){
		fprint(2, "swapcontext failed: %r\n");
		assert(0);
	}
}

static void
taskscheduler(void)
{
	int i;
	Task *t;

#ifdef LIBTASK_USE_PTHREAD
        memset (&taskschedcontext, 0, sizeof taskschedcontext);
        if (pthread_mutex_init (&taskschedcontext.uc.mutex, NULL) != 0) abort ();
        if (pthread_cond_init (&taskschedcontext.uc.cond, NULL) != 0) abort ();
        taskschedcontext.uc.thread = pthread_self ();
        taskschedcontext.uc.running = true;
#endif

	taskdebug("scheduler enter");
	for(;;){
		if(taskcount == 0)
			exit(taskexitval);
		t = taskrunqueue.head;
		if(t == nil){
			fprint(2, "no runnable tasks! %d tasks stalled\n", taskcount);
			exit(1);
		}
		deltask(&taskrunqueue, t);
		t->ready = 0;
		taskrunning = t;
		tasknswitch++;
		taskdebug("run %d (%s)", t->id, t->name);
		contextswitch(&taskschedcontext, &t->context);
//print("back in scheduler\n");
		taskrunning = nil;
		if(t->exiting){
			if(!t->system)
				taskcount--;
			i = t->alltaskslot;
			alltask[i] = alltask[--nalltask];
			alltask[i]->alltaskslot = i;
			free(t);
		}
	}
}

void**
taskdata(void)
{
	return &taskrunning->udata;
}

/*
 * debugging
 */
void
taskname(char *fmt, ...)
{
	va_list arg;
	Task *t;

	t = taskrunning;
	va_start(arg, fmt);
	vsnprint(t->name, sizeof t->name, fmt, arg);
	va_end(arg);
}

char*
taskgetname(void)
{
	return taskrunning->name;
}

void
taskstate(char *fmt, ...)
{
	va_list arg;
	Task *t;

	t = taskrunning;
	va_start(arg, fmt);
	vsnprint(t->state, sizeof t->name, fmt, arg);
	va_end(arg);
}

char*
taskgetstate(void)
{
	return taskrunning->state;
}

void
needstack(int n)
{
#ifndef LIBTASK_USE_FIBER
	Task *t;

	t = taskrunning;

	if((char*)&t <= (char*)t->stk
	|| (char*)&t - (char*)t->stk < 256+n){
		fprint(2, "task stack overflow: &t=%p tstk=%p n=%d\n", &t, t->stk, 256+n);
		abort();
	}
#endif
}

static void
taskinfo(int s)
{
	int i;
	Task *t;
	char *extra;

	fprint(2, "task list:\n");
	for(i=0; i<nalltask; i++){
		t = alltask[i];
		if(t == taskrunning)
			extra = " (running)";
		else if(t->ready)
			extra = " (ready)";
		else
			extra = "";
		fprint(2, "%6d%c %-20s %s%s\n",
			t->id, t->system ? 's' : ' ',
			t->name, t->state, extra);
	}
}

/*
 * startup
 */

static int taskargc;
static char **taskargv;
int mainstacksize;

static void
taskmainstart(void *v)
{
	taskname("taskmain");
	taskmain(taskargc, taskargv);
}

int
main(int argc, char **argv)
{
	struct sigaction sa, osa;

	memset(&sa, 0, sizeof sa);
	sa.sa_handler = taskinfo;
	sa.sa_flags = SA_RESTART;
	sigaction(SIGQUIT, &sa, &osa);

#ifdef SIGINFO
	sigaction(SIGINFO, &sa, &osa);
#endif

	argv0 = argv[0];
	taskargc = argc;
	taskargv = argv;

#ifdef LIBTASK_USE_FIBER
        if (ConvertThreadToFiber (NULL) == NULL)
          return 2;
#endif

	if(mainstacksize == 0)
		mainstacksize = 256*1024;
#ifdef EMACS
        init_emacs_main_task ();
        mainstacksize = 0x400000;
#endif
	taskcreate(taskmainstart, nil, mainstacksize);
	taskscheduler();
	fprint(2, "taskscheduler returned in main!\n");
	abort();
	return 0;
}

/*
 * hooray for linked lists
 */
void
addtask(Tasklist *l, Task *t)
{
	if(l->tail){
		l->tail->next = t;
		t->prev = l->tail;
	}else{
		l->head = t;
		t->prev = nil;
	}
	l->tail = t;
	t->next = nil;
}

void
deltask(Tasklist *l, Task *t)
{
	if(t->prev)
		t->prev->next = t->next;
	else
		l->head = t->next;
	if(t->next)
		t->next->prev = t->prev;
	else
		l->tail = t->prev;
}

unsigned int
taskid(void)
{
	return taskrunning->id;
}
