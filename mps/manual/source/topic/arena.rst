.. _topic-arena:

======
Arenas
======

See //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/protocol/mps/arena/index.html

From //info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/doc/mps/ref-man/concepts/index.html

An objects that represents the state of the MPS.

You start a session with the MPS by making an arena and end the session by destroying 
the arena. Even if you do not destroy it, it is guaranteed not to hang on to resources; but it is better to destroy it, to finish off properly. Before destroying the arena, you must first destroy all objects and data in it.

Other types of objects are created "in the arena". They are part of the world within the arena, and may interoperate with each other.

It is possible to create multiple arenas, but you would only do this in unusual circumstances. It might be useful to have two active arenas and to try different things out in them. The maximum number of arenas that the system can support is around 10. Using multiple arenas is an advanced technique. For more information, see the Arena Protocol .

Arenas do not interact. However, they may conflict with each other in terms of resources.

::

    mps_arena_t arena;

    int main(void)
    {
        void *block;
        mps_res_t res;

        block = malloc(ARENA_SIZE);
        if (block == NULL) {
            printf("Not enough memory!");
            exit(1);
        }

        res = mps_arena_create(&arena, mps_arena_class_cl(), ARENA_SIZE, block);
        if (res != MPS_RES_OK) {
            printf("ARENA_SIZE too small");
            exit(2);
        }

        /* rest of program */
    }

::

    mps_arena_t arena;

    int main(void)
    {
        mps_res_t res;

        res = mps_arena_create(&arena, mps_arena_class_vm(), ARENA_SIZE);
        if (res != MPS_RES_OK) {
            printf("Not enough memory!");
            exit(1);
        }

        /* rest of program */
    }

::

    mps_arena_t arena;

    int main(void)
    {
        mps_res_t res;

        res = mps_arena_create(&arena, mps_arena_class_vmnz(), ARENA_SIZE);
        if (res != MPS_RES_OK) {
            printf("Not enough memory!");
            exit(1);
        }

        /* rest of program */
    }

::

    do {
        res = mps_arena_commit_limit_set(arena, limit - 100 * 1024);
        if (res != MPS_RES_OK)
            flush_caches();
    } while(res != MPS_RES_OK);

::

    mps_arena_t arena;

    int main(void)
    {
        mps_res_t res;

        res = mps_arena_create(&arena, mps_arena_class_vm(), ARENA_SIZE);
        if (res != MPS_ RES_OK) {
            printf("Not enough memory!");
            exit(1);
        }

        /* rest of program */
    }



------------------------------
Using idle time for collection
------------------------------

See <http://info.ravenbrook.com/mail/2003/01/03/14-13-25/0.txt>
