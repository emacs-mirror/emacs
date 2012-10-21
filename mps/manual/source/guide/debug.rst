.. _guide-debug:

Debugging with the Memory Pool System
=====================================

Memory management errors are some of the most stubborn and difficult
to track down, because the effect so often appears at a distant point
in the program that is seemingly unrelated to the cause, and by the
time the error is revealed, the information needed to reconstruct the
cause has long vanished. Immediately after an :term:`overwriting
error`, the block that overran its bounds is fine, and the block that
was overwritten may not be visited for a long time. A failure to
:term:`fix` a :term:`reference` does not necessarily cause the object
pointed to by the missed reference to die immediately: there may be
other references to that object, or a garbage collection may be
delayed. And even if it does die, the space it occupies may not be
re-allocated for some time.


General advice
--------------

1. Compile with debugging information turned on (``-g`` on the GCC or
   Clang command line).

2. Build the :term:`cool` :term:`variety` of the MPS (by defining the
   preprocessor constant ``CONFIG_VAR_COOL``, for example by setting
   ``-DCONFIG_VAR_COOL`` on the GCC or Clang command line). This
   variety contains many internal consistency checks (including such
   checks on the :term:`critical path`, which make it too slow for
   use in production), and can generate debugging output in the form
   of the :term:`telemetry stream`, discussed below.

3. Prepare a reproducible test case if possible. The MPS may be
   :term:`asynchronous <asynchronous garbage collector>`, but it is
   deterministic, so in single-threaded applications you should be
   able to get consistent results. (But you need to beware of `address
   space layout randomization`_: if you perform computation based on
   the addresses of objects, for example, hashing objects by their
   address, then ASLR will cause your hash tables to be laid out
   differently on each run, which may effect the order of memory
   management operations.)

    .. _address space layout randomization: http://en.wikipedia.org/wiki/Address_space_layout_randomization

4. Run your test case inside the debugger. Use ``assert`` or ``abort``
   in your error handler (rather than ``exit``) so that you can enter
   the debugger with the contents of the control stack available for
   inspection.

.. _guide-debug-underscanning:

Underscanning
-------------

An easy mistake to make is to omit to :term:`fix` a :term:`reference`
when :term:`scanning <scan>` a :term:`formatted object`. For example,
in the Scheme intrepreter's :ref:`scan method <guide-lang-scan>`, I
might have forgetten to fix the first element of a pair:

.. code-block:: c
   :emphasize-lines: 2

    case TYPE_PAIR:
      /* oops, forgot: FIX(obj->pair.car); */
      FIX(obj->pair.cdr);
      base = (char *)base + ALIGN(sizeof(pair_s));
      break;

This means that as far as the MPS is concerned, the first element of
the pair is :term:`unreachable` and so :term:`dead`, so after
collecting the region of memory containing this object, the space will
be reused for other objects. So the pointer ``obj->pair.car`` might
end up pointing to the start of a valid object (but the wrong one), or
to the middle of a valid object, or to an unused region of memory, or
into an MPS internal control structure.

.. highlight:: none

The reproducible test case is simple. Run a garbage collection by
calling ``(gc)`` and then evaluate any expression::

    $ gdb ./scheme
    GNU gdb 6.3.50-20050815 (Apple version gdb-1820) (Sat Jun 16 02:40:11 UTC 2012)

    (gdb) run
    Starting program: example/scheme/scheme 
    Reading symbols for shared libraries +............................. done
    MPS Toy Scheme Example
    The prompt shows total allocated bytes and number of collections.
    Try (vector-length (make-vector 100000 1)) to see the MPS in action.
    You can force a complete garbage collection with (gc).
    If you recurse too much the interpreter may crash from using too much C stack.
    7944, 0> (gc)
    #[undefined]
    Collection started.
      Why: Client requests: immediate full collection.
      Clock: 11357
    Collection finished.
        live 1888
        condemned 7968
        not_condemned 0
        clock: 12008
    7968, 1> foo
    Assertion failed: (TYPE(frame) == TYPE_PAIR), function lookup_in_frame, file scheme.c, line 1065.

    Program received signal SIGABRT, Aborted.
    0x00007fff91aeed46 in __kill ()

What's going on? ::

    (gdb) backtrace
    #0  0x00007fff91aeed46 in __kill ()
    #1  0x00007fff90509df0 in abort ()
    #2  0x00007fff9050ae2a in __assert_rtn ()
    #3  0x0000000100003f55 in lookup_in_frame (frame=0x1003fa7d0, symbol=0x1003faf20) at scheme.c:1065
    #4  0x0000000100003ea6 in lookup (env=0x1003fb130, symbol=0x1003faf20) at scheme.c:1086
    #5  0x000000010000341f in eval (env=0x1003fb130, op_env=0x1003fb148, exp=0x1003faf20) at scheme.c:1134
    #6  0x000000010000261b in start (p=0x0, s=0) at scheme.c:2885
    #7  0x0000000100011ded in ProtTramp (resultReturn=0x7fff5fbff7d0, f=0x100002130 <start>, p=0x0, s=0) at protix.c:132
    #8  0x0000000100011d34 in mps_tramp (r_o=0x7fff5fbff7d0, f=0x100002130 <start>, p=0x0, s=0) at mpsi.c:1346
    #9  0x0000000100001ef7 in main (argc=1, argv=0x7fff5fbff830) at scheme.c:2994
    (gdb) frame 4
    #4  0x0000000100003ea6 in lookup (env=0x1003fb130, symbol=0x1003faf20) at scheme.c:1086
    1086	    binding = lookup_in_frame(CAR(env), symbol);
    (gdb) print (char *)symbol->symbol.string
    $1 = 0x1003faf30 "foo"

The backtrace shows that the interpreter is in the middle of looking
up the symbol ``foo`` in the environment. The Scheme intrepreter
implements the environment as a list of *frames*, each of which is a
list of *bindings*, each binding being a pair of a symbol and its
value, as shown here:

    .. figure:: ../diagrams/scheme-env.svg
        :align: center
        :alt: Diagram: The environment data structure in the Scheme interpreter.

        The environment data structure in the Scheme interpreter.

In this case, because the evaluation is taking place at top level,
there is only one frame in the environment (the global frame). And
it's this frame that's corrupt::

    (gdb) frame 3
    #3  0x0000000100003f55 in lookup_in_frame (frame=0x1003fa7d0, symbol=0x1003faf20) at scheme.c:1065
    1065	    assert(TYPE(frame) == TYPE_PAIR);
    (gdb) print frame->type.type
    $2 = 13

The number 13 is the value ``TYPE_PAD``. So instead of the expected
pair, ``frame`` points to a :term:`padding object`.

You might guess at this point that the frame had not been fixed, and
since you know that the frame is referenced by the ``car`` of the
first pair in the environment, that's the suspect reference. But in a
more complex situation this might not yet be clear. In such a
situation it can be useful to look at the sequence of events leading
up to the detection of the error, and in order to enable you to do
that, the MPS provides its :ref:`topic-telemetry` feature.


Telemetry
---------

In its :term:`cool` :term:`variety`, the MPS is capable of outputting
a configurable stream of events to assist with debugging and
profiling. The exact mechanism by which the telemetry stream is
controlled is in theory configurable via the :ref:`topic-plinth`, but
if you haven't done so then you can set the environment variables
:envvar:`MPS_TELEMETRY_CONTROL` and :envvar:`MPS_TELEMETRY_FILENAME`.
Setting the former to "``65535``" turns on all events, and the default
value for the latter is "``mpsio.log``" which is fine. So let's run
the test case with telemetry turned on::

    $ gdb ./scheme
    GNU gdb 6.3.50-20050815 (Apple version gdb-1820) (Sat Jun 16 02:40:11 UTC 2012)
    [...]
    (gdb) set environment MPS_TELEMETRY_CONTROL=65535
    (gdb) run
    Starting program: example/scheme/scheme 
    Reading symbols for shared libraries +............................. done
    MPS Toy Scheme Example
    [...]
    7944, 0> (gc)
    #[undefined]
    [...]
    7968, 1> foo
    Assertion failed: (TYPE(frame) == TYPE_PAIR), function lookup_in_frame, file scheme.c, line 1065.

    Program received signal SIGABRT, Aborted.
    0x00007fff91aeed46 in __kill ()

At this point there's still output in the MPS's internal event
buffers, which needs to be flushed. It would be a good idea to add a
call to :c:func:`mps_telemetry_flush` to the error handler, but for
now we can just call it directly from the debugger::

    (gdb) print mps_telemetry_flush()
    $1 = void

The MPS writes the telemetry to the log in an encoded form for speed.
It can be decoded using the :ref:`eventcnv <telemetry-eventcnv>`
program::

    (gdb) shell eventcnv -v | sort > mpsio.txt

The ``sort`` is useful because the events are not necessarily written
to the telemetry file in time order, but each event starts with a
timestamp so sorting makes a time series. The decoded events look like
this, with the timestamp in the first column (in units of
:c:type:`mps_clock_t`, typically 1 µs), the event type in the second
column, and then addresses or other data related to the event in the
remaining columns. All numbers are given in hexadecimal. ::

    00000C0DC0DA69DD395 VMCreate       100128000 100129000 10012A000
    00000C0DC0DA69E1ACF VMMap          100128000 100129000 10012A000
    00000C0DC0DA69FF118 Intern                2 Reservoir
    00000C0DC0DA69FF358 Label          1000BD4A8 sym 00002
    00000C0DC0DA6A04890 PoolInit       100129328 100129000 1000BD4A8
    00000C0DC0DA6A0C813 VMCreate       10012A000 100300000 100400000
    00000C0DC0DA6A120B4 VMMap          10012A000 100300000 100301000
    00000C0DC0DA6A1D0F1 ArenaCreateVM  100129000   100000   100000

You can search through the telemetry for events related to particular
addresses of interest. Here we look for events related to the address
of the corrupted ``frame`` object::

    (gdb) frame 3
    #3  0x0000000100003f55 in lookup_in_frame (frame=0x1003fa7d0, symbol=0x1003faf20) at scheme.c:1065
    1065	    assert(TYPE(frame) == TYPE_PAIR);
    (gdb) print frame
    $2 = (obj_t) 0x1003fa7d0
    (gdb) shell grep -i 1003fa7d0 mpsio.txt || echo not found
    not found

There are no events related to this address, so in particular this
address was never fixed.


Getting the size wrong
----------------------

Here's another kind of mistake: an off-by-one error in ``make_string``:

.. code-block:: c
   :emphasize-lines: 5

    static obj_t make_string(size_t length, char *string)
    {
      obj_t obj;
      mps_addr_t addr;
      size_t size = ALIGN(offsetof(string_s, string) + length/* oops, forgot: +1 */);
      do {
        mps_res_t res = mps_reserve(&addr, obj_ap, size);
        if (res != MPS_RES_OK) error("out of memory in make_string");
        obj = addr;
        obj->string.type = TYPE_STRING;
        obj->string.length = length;
        if (string)
          memcpy(obj->string.string, string, length+1);
      } while(!mps_commit(obj_ap, addr, size));
      total += size;
      return obj;
    }

Here's a test case that exercises this bug:

.. code-block:: scheme

    (define (church n f a) (if (eqv? n 0) a (church (- n 1) f (f a))))
    (church 1000 (lambda (s) (string-append s "x")) "")

And here's how it shows up in the debugger::

    $ gdb ./scheme
    GNU gdb 6.3.50-20050815 (Apple version gdb-1820) (Sat Jun 16 02:40:11 UTC 2012)
    [...]
    (gdb) set environment MPS_TELEMETRY_CONTROL=65535
    (gdb) run < test.scm
    Starting program: example/scheme/scheme < test.scm
    Reading symbols for shared libraries +............................. done
    MPS Toy Scheme Example
    [...]
    9960, 0> church
    Assertion failed: (0), function obj_skip, file scheme.c, line 2949.
    10816, 0> 
    Program received signal SIGABRT, Aborted.
    0x00007fff91aeed46 in __kill ()
    (gdb) backtrace
    #0  0x00007fff91aeed46 in __kill ()
    #1  0x00007fff90509df0 in abort ()
    #2  0x00007fff9050ae2a in __assert_rtn ()
    #3  0x00000001000014e3 in obj_skip (base=0x1003f9b88) at scheme.c:2949
    #4  0x0000000100068050 in amcScanNailedOnce (totalReturn=0x7fff5fbfef2c, moreReturn=0x7fff5fbfef28, ss=0x7fff5fbff0a0, pool=0x1003fe278, seg=0x1003fe928, amc=0x1003fe278) at poolamc.c:1485
    #5  0x0000000100067ca1 in amcScanNailed (totalReturn=0x7fff5fbff174, ss=0x7fff5fbff0a0, pool=0x1003fe278, seg=0x1003fe928, amc=0x1003fe278) at poolamc.c:1522
    #6  0x000000010006631f in AMCScan (totalReturn=0x7fff5fbff174, ss=0x7fff5fbff0a0, pool=0x1003fe278, seg=0x1003fe928) at poolamc.c:1595
    #7  0x000000010002686d in PoolScan (totalReturn=0x7fff5fbff174, ss=0x7fff5fbff0a0, pool=0x1003fe278, seg=0x1003fe928) at pool.c:405
    #8  0x0000000100074106 in traceScanSegRes (ts=1, rank=1, arena=0x10012a000, seg=0x1003fe928) at trace.c:1162
    #9  0x000000010002b399 in traceScanSeg (ts=1, rank=1, arena=0x10012a000, seg=0x1003fe928) at trace.c:1222
    #10 0x000000010002d020 in TraceQuantum (trace=0x10012a5a0) at trace.c:1833
    #11 0x000000010001f2d2 in TracePoll (globals=0x10012a000) at trace.c:1981
    #12 0x000000010000d75f in ArenaPoll (globals=0x10012a000) at global.c:684
    #13 0x000000010000ea40 in mps_ap_fill (p_o=0x7fff5fbff3e0, mps_ap=0x1003fe820, size=208) at mpsi.c:961
    #14 0x000000010000447d in make_string (length=190, string=0x0) at scheme.c:468
    #15 0x0000000100008ca2 in entry_string_append (env=0x1003cbe38, op_env=0x1003cbe50, operator=0x1003fad48, operands=0x1003f9af8) at scheme.c:2572
    #16 0x0000000100002fe4 in eval (env=0x1003cbe38, op_env=0x1003cbe50, exp=0x1003f9ae0) at scheme.c:1159
    #17 0x0000000100005ff5 in entry_interpret (env=0x1003cb958, op_env=0x1003cb970, operator=0x1003f99d8, operands=0x1003f9948) at scheme.c:1340
    #18 0x0000000100002fe4 in eval (env=0x1003cb958, op_env=0x1003cb970, exp=0x1003f9878) at scheme.c:1159
    #19 0x000000010000206b in start (p=0x0, s=0) at scheme.c:3213
    #20 0x000000010001287d in ProtTramp (resultReturn=0x7fff5fbff7a0, f=0x100001b80 <start>, p=0x0, s=0) at protix.c:132
    #21 0x00000001000127c4 in mps_tramp (r_o=0x7fff5fbff7a0, f=0x100001b80 <start>, p=0x0, s=0) at mpsi.c:1346
    #22 0x0000000100001947 in main (argc=1, argv=0x7fff5fbff808) at scheme.c:3322
    (gdb) frame 3
    #3  0x00000001000014e3 in obj_skip (base=0x1003f9b88) at scheme.c:2949
    2949	    assert(0);

The object being skipped is corrupt::

    (gdb) print obj->type.type
    $1 = 4168560

What happened to it? It's often helpful in these situations to have a
look at nearby memory. ::

    (gdb) x/20g obj
    0x1003f9b88:	0x00000001003f9b70	0x00000001003fb000
    0x1003f9b98:	0x0000000000000000	0x00000001003f9c90
    0x1003f9ba8:	0x00000001003fb130	0x0000000000000000
    0x1003f9bb8:	0x00000001003fb000	0x00000001003fb148
    0x1003f9bc8:	0x0000000000000000	0x00000001003f9730
    0x1003f9bd8:	0x00000001003f9a58	0x0000000000000000
    0x1003f9be8:	0x00000001003f9bc8	0x00000001003fb000
    0x1003f9bf8:	0x0000000000000000	0x00000001003fb0a0
    0x1003f9c08:	0x00000001003f9b40	0x0000000000000004
    0x1003f9c18:	0x000000010007b14a	0x0000000100005e30

You can see that this is a block containing mostly pairs (which have
tag 0 and consist of three words), though you can see an operator
(with tag 4) near the bottom. But what's that at the start of the
block, where ``obj``\'s tag should be? It looks like a pointer. So
what's in the memory just below ``obj``? Let's look at the previous
few words ::

    (gdb) x/10g (mps_word_t*)obj-8
    0x1003f9b48:	0x00000001003f9ae0	0x00000001003fb000
    0x1003f9b58:	0x0000000000000000	0x00000001003f9a80
    0x1003f9b68:	0x00000001003f9b80	0x0000000000000005
    0x1003f9b78:	0x0000000000000000	0x0000000000000000
    0x1003f9b88:	0x00000001003f9b70	0x00000001003fb000

Yes: there's a pair (with tag 0) at 0x1003f9b80. So it looks as though
the previous object might have returned the wrong size. The previous
object being the string (with tag 5) at 0x1003f9b70 which has length 0
and so should be three words long, but here occupies only two words.

This should be enough evidence to track down the cause.
