.. _pool-awl:

===========================
AWL (Automatic Weak Linked)
===========================


Automatic Weak Linked

For weak references, linked to normal references. The purpose of this pool class is to allow the client to implement hash tables (or, conceivably some similar datastructures) that are partly or wholly weak. A client might want a weak-key strong-value table, or a strong-key weak-value table (or in fact weak-key weak-value). For example, Java's java.util.WeakHashMap class.

The principle idea behind a weak-key strong-value hash table is that the keys are weakly referenced, so their presence in the table will not prevent the key object from being garbage collected. Once the key is no longer reachable, weak references to it may get splatted. Obviously once that has happened you can't get at the value corresponding to the key anymore, so the implementation is free to splat the value slot as well.

Pool Class AWL allows the implementation to splat the value slot at the same time that the (weak) key slot is splatted. (Or the other way around if you have strong-key weak-value tables).

Exactly how you do this is not yet well documented (but see below for more hints).

AWL has another special power: it enables better handing of barrier hits on weak objects. To explain the benefit we need to describe a problem first. The MPS uses a read-barrier to perform incremental garbage collection [@@ link to canned-for-client explanation of how a garbage collector works in broad terms]. When the client tries to read an object containing weak references the MPS may have protected it so that the MPS can process the object before the client gets to see it. The problem for weak objects is that the client may try and access the object at a point in the collection cycle when the MPS cannot yet determine the status of the objects that the weak object refers to. What the MPS does in this situation is assume that all the referenced objects are going to live. This assumption is correct but conservative; it may result in objects that are weakly referenced staying alive for longer than they need to. In the worst case this can result in a very large amount of memory being used by objects that are no longer needed.

In order to combat this problem the MPS sometimes does the following: Instead of processing the entire weak object and unprotecting it so that the client can access the object the MPS may emulate the processor instruction. When this happens the MPS doesn't process the entire weak object, it only processes the exact location that was being accessed (typically a single word), it emulates the processor instruction, and it keeps the object protected. This happens invisibly from the client's perspective, it's exactly as if the instruction executed as normal. The MPS instead of processing the entire object processes just a single word.

Naturally this emulation business is delicate and hairy and involves staring at the most badly written parts of low-level processor architecture manuals for days.

Emulation of accesses to protected objects happens when:

The object is a weak object allocated in an AWL pool.
The MPS is on Linux/IA-32 or Windows/IA-32. Extending this list to new (reasonable) operating systems should be tolerable (for example, OS X/IA-32), new processor architectures require more work.
The processor instruction that is accessing the object is of a suitable simple form. The MPS doesn't contain an emulator for all possible instructions that might access memory, so it only recognises and emulates a few forms of instruction. Currently this is simple MOVs from memory to a register or vice-versa, but it's best not to rely on the details.
Because of this emulation, AWL places a restriction on the format of objects allocated in it:

All slots in an object must either be a valid word-aligned reference, or the bottom bits of the word must be non-zero so that it does not look like an aligned pointer. "Aligned pointer" means a word whose numeric value (that is, treated as an unsigned integer) is a multiple of the architecture's natural word size (in bytes). In the very likely event that you're using a 32-bit machine that means that an aligned poiner is a multiple of 4 and its bottom 2 bits are both 0. Bottom line: pointers must be untagged and aligned, ints must be tagged with a non-zero tag.

AWL is one of the few pools that can allocate objects that contain non-exact references. The mps_ap_create method when used with an AWL pool accepts an extra argument, which is a rank of type mps_rank_t. The rank is either MPS_RANK_EXACT (to allocate ordinary objects contained exact references), or MPS_RANK_WEAK (to allocate objects that contain weak references). Example::

    if(MPS_RES_OK != mps_ap_create(&weakap, tablepool, MPS_RANK_WEAK) {
      /* Error. */
      ...
    }
    ...



See also NB's notes on weak hash tables in Dylan:
<https://info.ravenbrook.com/mail/2002/04/12/15-52-29/0.txt>
<https://info.ravenbrook.com/mail/2002/04/12/15-56-15/0.txt>
    


--------------------
AWL symbol reference
--------------------

::

   #include "mpscawl.h"


------------
Undocumented
------------

.. c:function:: mps_class_t mps_class_awl(void)
