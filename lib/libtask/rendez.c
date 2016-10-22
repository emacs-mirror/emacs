#include "taskimpl.h"

/*
 * sleep and wakeup
 */
void
tasksleep(Rendez *r)
{
	addtask(&r->waiting, taskrunning);
	if(r->l)
		qunlock(r->l);
	taskstate("sleep");
	taskswitch();
	if(r->l)
		qlock(r->l);
}

static int
_taskwakeup(Rendez *r, int all)
{
	int i;
	Task *t;

	for(i=0;; i++){
		if(i==1 && !all)
			break;
		if((t = r->waiting.head) == nil)
			break;
		deltask(&r->waiting, t);
		taskready(t);
	}
	return i;
}

int
taskwakeup(Rendez *r)
{
	return _taskwakeup(r, 0);
}

int
taskwakeupall(Rendez *r)
{
	return _taskwakeup(r, 1);
}
