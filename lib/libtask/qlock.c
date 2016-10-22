#include "taskimpl.h"

/*
 * locking
 */
static int
_qlock(QLock *l, int block)
{
	if(l->owner == nil){
		l->owner = taskrunning;
		return 1;
	}
	if(!block)
		return 0;
	addtask(&l->waiting, taskrunning);
	taskstate("qlock");
	taskswitch();
	if(l->owner != taskrunning){
		fprint(2, "qlock: owner=%p self=%p oops\n", l->owner, taskrunning);
		abort();
	}
	return 1;
}

void
qlock(QLock *l)
{
	_qlock(l, 1);
}

int
canqlock(QLock *l)
{
	return _qlock(l, 0);
}

void
qunlock(QLock *l)
{
	Task *ready;

	if(l->owner == 0){
		fprint(2, "qunlock: owner=0\n");
		abort();
	}
	if((l->owner = ready = l->waiting.head) != nil){
		deltask(&l->waiting, ready);
		taskready(ready);
	}
}

static int
_rlock(RWLock *l, int block)
{
	if(l->writer == nil && l->wwaiting.head == nil){
		l->readers++;
		return 1;
	}
	if(!block)
		return 0;
	addtask(&l->rwaiting, taskrunning);
	taskstate("rlock");
	taskswitch();
	return 1;
}

void
rlock(RWLock *l)
{
	_rlock(l, 1);
}

int
canrlock(RWLock *l)
{
	return _rlock(l, 0);
}

static int
_wlock(RWLock *l, int block)
{
	if(l->writer == nil && l->readers == 0){
		l->writer = taskrunning;
		return 1;
	}
	if(!block)
		return 0;
	addtask(&l->wwaiting, taskrunning);
	taskstate("wlock");
	taskswitch();
	return 1;
}

void
wlock(RWLock *l)
{
	_wlock(l, 1);
}

int
canwlock(RWLock *l)
{
	return _wlock(l, 0);
}

void
runlock(RWLock *l)
{
	Task *t;

	if(--l->readers == 0 && (t = l->wwaiting.head) != nil){
		deltask(&l->wwaiting, t);
		l->writer = t;
		taskready(t);
	}
}

void
wunlock(RWLock *l)
{
	Task *t;

	if(l->writer == nil){
		fprint(2, "wunlock: not locked\n");
		abort();
	}
	l->writer = nil;
	if(l->readers != 0){
		fprint(2, "wunlock: readers\n");
		abort();
	}
	while((t = l->rwaiting.head) != nil){
		deltask(&l->rwaiting, t);
		l->readers++;
		taskready(t);
	}
	if(l->readers == 0 && (t = l->wwaiting.head) != nil){
		deltask(&l->wwaiting, t);
		l->writer = t;
		taskready(t);
	}
}
