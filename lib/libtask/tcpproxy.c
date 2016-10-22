#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>
#include <task.h>
#include <stdlib.h>
#include <sys/socket.h>

enum
{
	STACK = 32768
};

char *server;
int port;
void proxytask(void*);
void rwtask(void*);

int*
mkfd2(int fd1, int fd2)
{
	int *a;

	a = malloc(2*sizeof a[0]);
	if(a == 0){
		fprintf(stderr, "out of memory\n");
		abort();
	}
	a[0] = fd1;
	a[1] = fd2;
	return a;
}

void
taskmain(int argc, char **argv)
{
	int cfd, fd;
	int rport;
	char remote[16];

	if(argc != 4){
		fprintf(stderr, "usage: tcpproxy localport server remoteport\n");
		taskexitall(1);
	}
	server = argv[2];
	port = atoi(argv[3]);

	if((fd = netannounce(TCP, 0, atoi(argv[1]))) < 0){
		fprintf(stderr, "cannot announce on tcp port %d: %s\n", atoi(argv[1]), strerror(errno));
		taskexitall(1);
	}
	fdnoblock(fd);
	while((cfd = netaccept(fd, remote, &rport)) >= 0){
		fprintf(stderr, "connection from %s:%d\n", remote, rport);
		taskcreate(proxytask, (void*)cfd, STACK);
	}
}

void
proxytask(void *v)
{
	int fd, remotefd;

	fd = (int)v;
	if((remotefd = netdial(TCP, server, port)) < 0){
		close(fd);
		return;
	}

	fprintf(stderr, "connected to %s:%d\n", server, port);

	taskcreate(rwtask, mkfd2(fd, remotefd), STACK);
	taskcreate(rwtask, mkfd2(remotefd, fd), STACK);
}

void
rwtask(void *v)
{
	int *a, rfd, wfd, n;
	char buf[2048];

	a = v;
	rfd = a[0];
	wfd = a[1];
	free(a);

	while((n = fdread(rfd, buf, sizeof buf)) > 0)
		fdwrite(wfd, buf, n);
	shutdown(wfd, SHUT_WR);
	close(rfd);
}
