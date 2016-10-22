/* Copyright (c) 2005 Russ Cox, MIT; see COPYRIGHT */

#include "taskimpl.h"

Channel*
chancreate(int elemsize, int bufsize)
{
	Channel *c;

	c = malloc(sizeof *c+bufsize*elemsize);
	if(c == nil){
		fprint(2, "chancreate malloc: %r");
		exit(1);
	}
	memset(c, 0, sizeof *c);
	c->elemsize = elemsize;
	c->bufsize = bufsize;
	c->nbuf = 0;
	c->buf = (uchar*)(c+1);
	return c;
}

/* bug - work out races */
void
chanfree(Channel *c)
{
	if(c == nil)
		return;
	free(c->name);
	free(c->arecv.a);
	free(c->asend.a);
	free(c);
}

static void
addarray(Altarray *a, Alt *alt)
{
	if(a->n == a->m){
		a->m += 16;
		a->a = realloc(a->a, a->m*sizeof a->a[0]);
	}
	a->a[a->n++] = alt;
}

static void
delarray(Altarray *a, int i)
{
	--a->n;
	a->a[i] = a->a[a->n];
}

/*
 * doesn't really work for things other than CHANSND and CHANRCV
 * but is only used as arg to chanarray, which can handle it
 */
#define otherop(op)	(CHANSND+CHANRCV-(op))

static Altarray*
chanarray(Channel *c, uint op)
{
	switch(op){
	default:
		return nil;
	case CHANSND:
		return &c->asend;
	case CHANRCV:
		return &c->arecv;
	}
}

static int
altcanexec(Alt *a)
{
	Altarray *ar;
	Channel *c;

	if(a->op == CHANNOP)
		return 0;
	c = a->c;
	if(c->bufsize == 0){
		ar = chanarray(c, otherop(a->op));
		return ar && ar->n;
	}else{
		switch(a->op){
		default:
			return 0;
		case CHANSND:
			return c->nbuf < c->bufsize;
		case CHANRCV:
			return c->nbuf > 0;
		}
	}
}

static void
altqueue(Alt *a)
{
	Altarray *ar;

	ar = chanarray(a->c, a->op);
	addarray(ar, a);
}

static void
altdequeue(Alt *a)
{
	int i;
	Altarray *ar;

	ar = chanarray(a->c, a->op);
	if(ar == nil){
		fprint(2, "bad use of altdequeue op=%d\n", a->op);
		abort();
	}

	for(i=0; i<ar->n; i++)
		if(ar->a[i] == a){
			delarray(ar, i);
			return;
		}
	fprint(2, "cannot find self in altdq\n");
	abort();
}

static void
altalldequeue(Alt *a)
{
	int i;

	for(i=0; a[i].op!=CHANEND && a[i].op!=CHANNOBLK; i++)
		if(a[i].op != CHANNOP)
			altdequeue(&a[i]);
}

static void
amove(void *dst, void *src, uint n)
{
	if(dst){
		if(src == nil)
			memset(dst, 0, n);
		else
			memmove(dst, src, n);
	}
}

/*
 * Actually move the data around.  There are up to three
 * players: the sender, the receiver, and the channel itself.
 * If the channel is unbuffered or the buffer is empty,
 * data goes from sender to receiver.  If the channel is full,
 * the receiver removes some from the channel and the sender
 * gets to put some in.
 */
static void
altcopy(Alt *s, Alt *r)
{
	Alt *t;
	Channel *c;
	uchar *cp;

	/*
	 * Work out who is sender and who is receiver
	 */
	if(s == nil && r == nil)
		return;
	assert(s != nil);
	c = s->c;
	if(s->op == CHANRCV){
		t = s;
		s = r;
		r = t;
	}
	assert(s==nil || s->op == CHANSND);
	assert(r==nil || r->op == CHANRCV);

	/*
	 * Channel is empty (or unbuffered) - copy directly.
	 */
	if(s && r && c->nbuf == 0){
		amove(r->v, s->v, c->elemsize);
		return;
	}

	/*
	 * Otherwise it's always okay to receive and then send.
	 */
	if(r){
		cp = c->buf + c->off*c->elemsize;
		amove(r->v, cp, c->elemsize);
		--c->nbuf;
		if(++c->off == c->bufsize)
			c->off = 0;
	}
	if(s){
		cp = c->buf + (c->off+c->nbuf)%c->bufsize*c->elemsize;
		amove(cp, s->v, c->elemsize);
		++c->nbuf;
	}
}

static void
altexec(Alt *a)
{
	int i;
	Altarray *ar;
	Alt *other;
	Channel *c;

	c = a->c;
	ar = chanarray(c, otherop(a->op));
	if(ar && ar->n){
		i = rand()%ar->n;
		other = ar->a[i];
		altcopy(a, other);
		altalldequeue(other->xalt);
		other->xalt[0].xalt = other;
		taskready(other->task);
	}else
		altcopy(a, nil);
}

#define dbgalt 0
int
chanalt(Alt *a)
{
	int i, j, ncan, n, canblock;
	Channel *c;
	Task *t;

	needstack(512);
	for(i=0; a[i].op != CHANEND && a[i].op != CHANNOBLK; i++)
		;
	n = i;
	canblock = a[i].op == CHANEND;

	t = taskrunning;
	for(i=0; i<n; i++){
		a[i].task = t;
		a[i].xalt = a;
	}
if(dbgalt) print("alt ");
	ncan = 0;
	for(i=0; i<n; i++){
		c = a[i].c;
if(dbgalt) print(" %c:", "esrnb"[a[i].op]);
if(dbgalt) { if(c->name) print("%s", c->name); else print("%p", c); }
		if(altcanexec(&a[i])){
if(dbgalt) print("*");
			ncan++;
		}
	}
	if(ncan){
		j = rand()%ncan;
		for(i=0; i<n; i++){
			if(altcanexec(&a[i])){
				if(j-- == 0){
if(dbgalt){
c = a[i].c;
print(" => %c:", "esrnb"[a[i].op]);
if(c->name) print("%s", c->name); else print("%p", c);
print("\n");
}
					altexec(&a[i]);
					return i;
				}
			}
		}
	}
if(dbgalt)print("\n");

	if(!canblock)
		return -1;

	for(i=0; i<n; i++){
		if(a[i].op != CHANNOP)
			altqueue(&a[i]);
	}

	taskswitch();

	/*
	 * the guy who ran the op took care of dequeueing us
	 * and then set a[0].alt to the one that was executed.
	 */
	return a[0].xalt - a;
}

static int
_chanop(Channel *c, int op, void *p, int canblock)
{
	Alt a[2];

	a[0].c = c;
	a[0].op = op;
	a[0].v = p;
	a[1].op = canblock ? CHANEND : CHANNOBLK;
	if(chanalt(a) < 0)
		return -1;
	return 1;
}

int
chansend(Channel *c, void *v)
{
	return _chanop(c, CHANSND, v, 1);
}

int
channbsend(Channel *c, void *v)
{
	return _chanop(c, CHANSND, v, 0);
}

int
chanrecv(Channel *c, void *v)
{
	return _chanop(c, CHANRCV, v, 1);
}

int
channbrecv(Channel *c, void *v)
{
	return _chanop(c, CHANRCV, v, 0);
}

int
chansendp(Channel *c, void *v)
{
	return _chanop(c, CHANSND, (void*)&v, 1);
}

void*
chanrecvp(Channel *c)
{
	void *v;

	_chanop(c, CHANRCV, (void*)&v, 1);
	return v;
}

int
channbsendp(Channel *c, void *v)
{
	return _chanop(c, CHANSND, (void*)&v, 0);
}

void*
channbrecvp(Channel *c)
{
	void *v;

	_chanop(c, CHANRCV, (void*)&v, 0);
	return v;
}

int
chansendul(Channel *c, ulong val)
{
	return _chanop(c, CHANSND, &val, 1);
}

ulong
chanrecvul(Channel *c)
{
	ulong val;

	_chanop(c, CHANRCV, &val, 1);
	return val;
}

int
channbsendul(Channel *c, ulong val)
{
	return _chanop(c, CHANSND, &val, 0);
}

ulong
channbrecvul(Channel *c)
{
	ulong val;

	_chanop(c, CHANRCV, &val, 0);
	return val;
}
