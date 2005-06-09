#ifndef _PWD_H_
#define _PWD_H_
/*
 * pwd.h doesn't exist on NT, so we put together our own.
 */

struct passwd {
    char *pw_name;
    char *pw_passwd;
    int   pw_uid;
    int   pw_gid;
    int   pw_quota;
    char *pw_gecos;
    char *pw_dir;
    char *pw_shell;
};

typedef int uid_t;
typedef uid_t gid_t;

struct passwd * getpwnam (char *);
struct passwd * getpwuid (int);


#endif /* _PWD_H_ */

/* arch-tag: 68308424-cb2b-49ed-bb52-b347fee416bf
   (do not change this comment) */
