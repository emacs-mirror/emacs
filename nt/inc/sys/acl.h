/* Emulation of Posix ACLs for Windows.  */

#ifndef ACL_H
#define ACL_H

#define NOMINMAX 1	/* don't define min and max */
#include <windows.h>

typedef PSECURITY_DESCRIPTOR acl_t;
typedef unsigned acl_type_t;

/* Values of acl_type_t  */
#define ACL_TYPE_ACCESS  0
#define ACL_TYPE_DEFAULT 1

typedef unsigned acl_perm_t;

extern int    acl_valid (acl_t);
extern acl_t  acl_get_file (const char *, acl_type_t);
extern int    acl_set_file (const char *, acl_type_t, acl_t);
extern char * acl_to_text (acl_t, ssize_t *);
extern acl_t  acl_from_text (const char *);
extern int    acl_free (void *);

#endif	/* ACL_H */
