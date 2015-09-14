.. index::
   single: stretchy vectors
   single: atomic updates

.. _guide-stretchy-vector:

The stretchy vector problem
============================

The :ref:`previous chapter <guide-lang-root>` pointed out that:

    Because the MPS is :term:`asynchronous <asynchronous garbage
    collector>`, it might be scanning, moving, or collecting, at any
    point in time.

The consequences of this can take a while to sink in, so this chapter
discusses a particular instance that catches people out: the *stretchy
vector* problem (named after the |stretchy-vector|_ abstract class in
Dylan).

.. |stretchy-vector| replace:: ``<stretchy-vector>``
.. _stretchy-vector:  http://opendylan.org/books/drm/Collection_Classes#stretchy-vector

A *stretchy vector* is a vector that can change length dynamically.
Such a vector is often implemented using two objects: an array, and a
header object that stores the length and a pointer to an array.
Stretching (or shrinking) such a vector involves five steps:

1. allocate a new array;
2. copy elements from the old array to the new array;
3. clear unused elements in the new array (if stretching);
4. update the pointer to the array in the header;
5. update the length in the header.

For example:

.. code-block:: c

    typedef struct vector_s {
        type_t type;                /* TYPE_VECTOR */
        size_t length;              /* number of elements */
        obj_t *array;               /* array of elements */
    } vector_s, *vector_t;

    void resize_vector(vector_t vector, size_t new_length) {
        obj_t *new_array = realloc(vector->array, new_length * sizeof(obj_t));
        if (new_array == NULL)
            error("out of memory in resize_vector");
        if (vector->length < new_length) {
            memset(&vector->array[vector->length], 0,
                   (new_length - vector->length) * sizeof(obj_t));
        }
        vector->array = new_array;
        vector->length = new_length;
    }

When adapting this code to the MPS, the following problems must be
solved:

1. During step 2, the new array must be :term:`reachable` from the
   roots, and :term:`scannable <scan>`. (If it's not reachable, then
   it may be collected; if it's not scannable, then references it
   contains will not be updated when they are moved by the collector.)

   This can solved by storing the new array in a :term:`root` until
   the header has been updated. If the thread's stack has been
   registered as a root by calling :c:func:`mps_root_create_stack`
   then any local variable will do.

2. References in the new array must not be scanned until they have been
   copied or cleared. (Otherwise they will be invalid.)

   This can be solved by clearing the new array before calling
   :c:func:`mps_commit`.

3. The old array must be scanned at the old length (otherwise the scan
   may run off the end of the old array when the vector grows), and
   the new array must be scanned at the new length (otherwise the scan
   may run off the end of the old array when the vector shrinks).

4. The array object must be scannable without referring to the header
   object. (Because the header object may have been protected by the
   MPS: see :ref:`topic-format-cautions`.)

Problems 3 and 4 can be solved by storing the length in the array. The
revised data structures and resizing code might look like this:

.. code-block:: c

    typedef struct vector_s {
        type_t type;                /* TYPE_VECTOR */
        obj_t array;                /* TYPE_ARRAY object */
    } vector_s, *vector_t;

    typedef struct array_s {
        type_t type;                /* TYPE_ARRAY */
        size_t length;              /* number of elements */
        obj_t array[0];             /* array of elements */
    } array_s, *array_t;

    void resize_vector(vector_t vector, size_t new_length) {
        size_t size = ALIGN_OBJ(offsetof(array_s, array) + new_length * sizeof(obj_t));
        mps_addr_t addr;
        array_t array;

        do {
            mps_res_t res = mps_reserve(&addr, ap, size);
            if (res != MPS_RES_OK) error("out of memory in resize_vector");
            array = addr;
            array->type = TYPE_ARRAY;
            array->length = new_length;
            memset(array->array, 0, new_length * sizeof(obj_t));
            /* Now the new array is scannable, and it is reachable via the
             * local variable 'array', so it is safe to commit it. */
        } while(!mps_commit(ap, addr, size));

        /* Copy elements after committing, so that the collector will
         * update them if they move. */
        memcpy(array->array, vector->array->array,
               min(vector->array->length, new_length) * sizeof(obj_t));
        vector->array = array;
    }

Similar difficulties can arise even when adapting code written for
other garbage collectors. For example, here's the function
|setarrayvector|_ from Lua_:

.. |setarrayvector| replace:: ``setarrayvector()``
.. _setarrayvector: http://www.lua.org/source/5.2/ltable.c.html#setarrayvector
.. _Lua: http://www.lua.org

.. code-block:: c

    static void setarrayvector (lua_State *L, Table *t, int size) {
        int i;
        luaM_reallocvector(L, t->array, t->sizearray, size, TValue);
        for (i=t->sizearray; i<size; i++)
            setnilvalue(&t->array[i]);
        t->sizearray = size;
    }

Lua's garbage collector is :term:`synchronous <synchronous garbage
collector>`, so it can be assumed that there cannot be a garbage
collection between the assignment to ``t->array`` (resulting from the
expansion of the |luaM_reallocvector|_ macro) and the assignment to
``t->sizearray``, and so the collector will always consistently see
either the old array or the new array, with the correct size. This
assumption will no longer be correct if this code is adapted to the
MPS.

.. |luaM_reallocvector| replace:: ``luaM_reallocvector()``
.. _luaM_reallocvector: http://www.lua.org/source/5.2/lmem.h.html#luaM_reallocvector
