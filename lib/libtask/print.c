/* Copyright (c) 2004 Russ Cox.  See COPYRIGHT. */

#include "taskimpl.h"
#include <stdio.h>	/* for strerror! */

/*
 * Stripped down print library.  Plan 9 interface, new code.
 */

enum
{
	FlagLong = 1<<0,
	FlagLongLong = 1<<1,
	FlagUnsigned = 1<<2,
};

static char*
printstr(char *dst, char *edst, char *s, int size)
{
	int l, n, sign;

	sign = 1;
	if(size < 0){
		size = -size;
		sign = -1;
	}
	if(dst >= edst)
		return dst;
	l = strlen(s);
	n = l;
	if(n < size)
		n = size;
	if(n >= edst-dst)
		n = (edst-dst)-1;
	if(l > n)
		l = n;
	if(sign < 0){
		memmove(dst, s, l);
		if(n-l)
			memset(dst+l, ' ', n-l);
	}else{
		if(n-l)
			memset(dst, ' ', n-l);
		memmove(dst+n-l, s, l);
	}
	return dst+n;
}

char*
vseprint(char *dst, char *edst, char *fmt, va_list arg)
{
	int fl, size, sign, base;
	char *p, *w;
	char cbuf[2];

	w = dst;
	for(p=fmt; *p && w<edst-1; p++){
		switch(*p){
		default:
			*w++ = *p;
			break;
		case '%':
			fl = 0;
			size = 0;
			sign = 1;
			for(p++; *p; p++){
				switch(*p){
				case '-':
					sign = -1;
					break;
				case '0':
				case '1':
				case '2':
				case '3':
				case '4':
				case '5':
				case '6':
				case '7':
				case '8':
				case '9':
					size = size*10 + *p-'0';
					break;
				case 'l':
					if(fl&FlagLong)
						fl |= FlagLongLong;
					else
						fl |= FlagLong;
					break;
				case 'u':
					fl |= FlagUnsigned;
					break;
				case 'd':
					base = 10;
					goto num;
				case 'o':
					base = 8;
					goto num;
				case 'p':
				case 'x':
					base = 16;
					goto num;
				num:
				{
					static char digits[] = "0123456789abcdef";
					char buf[30], *p;
					int neg, zero;
					uvlong luv;

					if(fl&FlagLongLong){
						if(fl&FlagUnsigned)
							luv = va_arg(arg, uvlong);
						else
							luv = va_arg(arg, vlong);
					}else{
						if(fl&FlagLong){
							if(fl&FlagUnsigned)
								luv = va_arg(arg, ulong);
							else
								luv = va_arg(arg, long);
						}else{
							if(fl&FlagUnsigned)
								luv = va_arg(arg, uint);
							else
								luv = va_arg(arg, int);
						}
					}

					p = buf+sizeof buf;
					neg = 0;
					zero = 0;
					if(!(fl&FlagUnsigned) && (vlong)luv < 0){
						neg = 1;
						luv = -luv;
					}
					if(luv == 0)
						zero = 1;
					*--p = 0;
					while(luv){
						*--p = digits[luv%base];
						luv /= base;
					}
					if(base == 16){
						*--p = 'x';
						*--p = '0';
					}
					if(base == 8 || zero)
						*--p = '0';
					w = printstr(w, edst, p, size*sign);
					goto break2;
				}
				case 'c':
					cbuf[0] = va_arg(arg, int);
					cbuf[1] = 0;
					w = printstr(w, edst, cbuf, size*sign);
					goto break2;
				case 's':
					w = printstr(w, edst, va_arg(arg, char*), size*sign);
					goto break2;
				case 'r':
					w = printstr(w, edst, strerror(errno), size*sign);
					goto break2;
				default:
					p = "X*verb*";
					goto break2;
				}
			}
		    break2:
			break;
		}
	}

	assert(w < edst);
	*w = 0;
	return dst;
}

char*
vsnprint(char *dst, uint n, char *fmt, va_list arg)
{
	return vseprint(dst, dst+n, fmt, arg);
}

char*
snprint(char *dst, uint n, char *fmt, ...)
{
	va_list arg;

	va_start(arg, fmt);
	vsnprint(dst, n, fmt, arg);
	va_end(arg);
	return dst;
}

char*
seprint(char *dst, char *edst, char *fmt, ...)
{
	va_list arg;

	va_start(arg, fmt);
	vseprint(dst, edst, fmt, arg);
	va_end(arg);
	return dst;
}

int
vfprint(int fd, char *fmt, va_list arg)
{
	char buf[256];

	vseprint(buf, buf+sizeof buf, fmt, arg);
	return write(fd, buf, strlen(buf));
}

int
vprint(char *fmt, va_list arg)
{
	return vfprint(1, fmt, arg);
}

int
fprint(int fd, char *fmt, ...)
{
	int n;
	va_list arg;

	va_start(arg, fmt);
	n = vfprint(fd, fmt, arg);
	va_end(arg);
	return n;
}

int
print(char *fmt, ...)
{
	int n;
	va_list arg;

	va_start(arg, fmt);
	n = vprint(fmt, arg);
	va_end(arg);
	return n;
}

char*
strecpy(char *dst, char *edst, char *src)
{
	*printstr(dst, edst, src, 0) = 0;
	return dst;
}
