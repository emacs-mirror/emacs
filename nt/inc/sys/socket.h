/* Workable version of <sys/socket.h> based on winsock.h */

#ifndef _SOCKET_H_
#define _SOCKET_H_

/* defeat the multiple include protection */
#ifdef _WINSOCKAPI_
#undef _WINSOCKAPI_
#endif

/* avoid confusion with our version of select */
#ifdef select
#undef select
#define MUST_REDEF_SELECT
#endif

/* avoid clashing with our version of FD_SET if already defined */
#ifdef FD_SET
#undef FD_SET
#undef FD_CLR
#undef FD_ISSET
#undef FD_ZERO
#endif

/* allow us to provide our own version of fd_set */
#define fd_set ws_fd_set

/* avoid duplicate definition of timeval */
#ifdef HAVE_TIMEVAL
#define timeval ws_timeval
#endif

#include <winsock.h>

/* redefine select to reference our version */
#ifdef MUST_REDEF_SELECT
#define select sys_select
#undef MUST_REDEF_SELECT
#endif

/* revert to our version of FD_SET */
#undef FD_SET
#undef FD_CLR
#undef FD_ISSET
#undef FD_ZERO
#undef fd_set
#include "nt.h"

#ifdef HAVE_TIMEVAL
#undef timeval
#endif

/* shadow functions where we provide our own wrapper */
#define socket         sys_socket
#define bind           sys_bind
#define connect        sys_connect
#define htons          sys_htons
#define ntohs          sys_ntohs
#define inet_addr      sys_inet_addr
#define gethostname    sys_gethostname
#define gethostbyname  sys_gethostbyname
#define getservbyname  sys_getservbyname

int sys_socket(int af, int type, int protocol);
int sys_bind (int s, const struct sockaddr *addr, int namelen);
int sys_connect (int s, const struct sockaddr *addr, int namelen);
u_short sys_htons (u_short hostshort);
u_short sys_ntohs (u_short netshort);
unsigned long sys_inet_addr (const char * cp);
int sys_gethostname (char * name, int namelen);
struct hostent * sys_gethostbyname(const char * name);
struct servent * sys_getservbyname(const char * name, const char * proto);

/* we are providing a real h_errno variable */
#undef h_errno
extern int h_errno;

/* map winsock error codes to standard names */
#define EWOULDBLOCK             WSAEWOULDBLOCK
#define EINPROGRESS             WSAEINPROGRESS
#define EALREADY                WSAEALREADY
#define ENOTSOCK                WSAENOTSOCK
#define EDESTADDRREQ            WSAEDESTADDRREQ
#define EMSGSIZE                WSAEMSGSIZE
#define EPROTOTYPE              WSAEPROTOTYPE
#define ENOPROTOOPT             WSAENOPROTOOPT
#define EPROTONOSUPPORT         WSAEPROTONOSUPPORT
#define ESOCKTNOSUPPORT         WSAESOCKTNOSUPPORT
#define EOPNOTSUPP              WSAEOPNOTSUPP
#define EPFNOSUPPORT            WSAEPFNOSUPPORT
#define EAFNOSUPPORT            WSAEAFNOSUPPORT
#define EADDRINUSE              WSAEADDRINUSE
#define EADDRNOTAVAIL           WSAEADDRNOTAVAIL
#define ENETDOWN                WSAENETDOWN
#define ENETUNREACH             WSAENETUNREACH
#define ENETRESET               WSAENETRESET
#define ECONNABORTED            WSAECONNABORTED
#define ECONNRESET              WSAECONNRESET
#define ENOBUFS                 WSAENOBUFS
#define EISCONN                 WSAEISCONN
#define ENOTCONN                WSAENOTCONN
#define ESHUTDOWN               WSAESHUTDOWN
#define ETOOMANYREFS            WSAETOOMANYREFS
#define ETIMEDOUT               WSAETIMEDOUT
#define ECONNREFUSED            WSAECONNREFUSED
#define ELOOP                   WSAELOOP
/* #define ENAMETOOLONG            WSAENAMETOOLONG */
#define EHOSTDOWN               WSAEHOSTDOWN
#define EHOSTUNREACH            WSAEHOSTUNREACH
/* #define ENOTEMPTY               WSAENOTEMPTY */
#define EPROCLIM                WSAEPROCLIM
#define EUSERS                  WSAEUSERS
#define EDQUOT                  WSAEDQUOT
#define ESTALE                  WSAESTALE
#define EREMOTE                 WSAEREMOTE

#endif /* _SOCKET_H_ */

/* end of socket.h */
