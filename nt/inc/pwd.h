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

#endif /* _PWD_H_ */
