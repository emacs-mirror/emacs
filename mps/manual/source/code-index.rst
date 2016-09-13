.. _code-index:

Index to source code
====================


External MPS interface
----------------------

The external MPS interface consists of header files that the
:term:`client program` is expected to include, plus the single-file
source code (mps.c). See design.mps.interface-c_.

===========  ==================================================================
File         Description
===========  ==================================================================
mps.h        Public MPS interface.
mps.c        Single-file source code. See :ref:`guide-build`.
mpsacl.h     :ref:`topic-arena-client` external interface.
mpsavm.h     :ref:`topic-arena-vm` external interface.
mpscamc.h    :ref:`pool-amc` pool class external interface.
mpscams.h    :ref:`pool-ams` pool class external interface.
mpscawl.h    :ref:`pool-awl` pool class external interface.
mpsclo.h     :ref:`pool-lo` pool class external interface.
mpscmfs.h    :ref:`pool-mfs` pool class external interface.
mpscmv.h     :ref:`pool-mv` pool class external interface.
mpscmv2.h    Former (deprecated) :ref:`pool-mvt` pool class interface.
mpscmvff.h   :ref:`pool-mvff` pool class external interface.
mpscmvt.h    :ref:`pool-mvt` pool class external interface.
mpscsnc.h    :ref:`pool-snc` pool class external interface.
mpsio.h      :ref:`topic-plinth-io` interface.
mpslib.h     :ref:`topic-plinth-lib` interface.
===========  ==================================================================


Plinth
------

The :term:`plinth` provides an interface between the MPS and the
execution environment, to help support :term:`freestanding`
implementations. See :ref:`topic-plinth`.

===========  ==================================================================
File         Description
===========  ==================================================================
mpsioan.c    :ref:`topic-plinth-io` for "ANSI" (hosted) environments.
mpsliban.c   :ref:`topic-plinth-lib` for "ANSI" (hosted) environments.
===========  ==================================================================


Configuration
-------------

These header files provide platform-specific constants, type
declarations, and macros. See :ref:`topic-porting` and
design.mps.config_.

===========  ==================================================================
File         Description
===========  ==================================================================
clock.h      Fast high-resolution clocks.
config.h     MPS configuration header.
mpstd.h      Target detection header.
===========  ==================================================================


Core MPS
--------

============  =================================================================
File          Description
============  =================================================================
abq.c         Fixed-length queue implementation. See design.mps.abq_.
abq.h         Fixed-length queue interface. See design.mps.abq_.
arena.c       Arena implementation. See design.mps.arena_.
arenacl.c     :ref:`topic-arena-client` implementation.
arenavm.c     :ref:`topic-arena-vm` implementation.
arg.c         :ref:`topic-keyword` implementation.
arg.h         :ref:`topic-keyword` interface.
boot.c        Bootstrap allocator implementation. See design.mps.bootstrap_.
boot.h        Bootstrap allocator interface. See design.mps.bootstrap_.
bt.c          Bit table implementation. See design.mps.bt_.
bt.h          Bit table interface. See design.mps.bt_.
buffer.c      Buffer implementation. See design.mps.buffer_.
cbs.c         Coalescing block implementation. See design.mps.cbs_.
cbs.h         Coalescing block interface. See design.mps.cbs_.
check.h       Assertion interface. See design.mps.check_.
dbgpool.c     :ref:`topic-debugging` implementation.
dbgpool.h     :ref:`topic-debugging` interface.
dbgpooli.c    :ref:`topic-debugging` external interface.
event.c       :ref:`topic-telemetry` implementation.
event.h       :ref:`topic-telemetry` interface (internal).
eventcom.h    :ref:`topic-telemetry` interface (auxiliary programs).
eventdef.h    :ref:`topic-telemetry` event definitions.
failover.c    Fail-over allocator implementation. See design.mps.failover_.
failover.h    Fail-over allocator interface. See design.mps.failover_.
format.c      :ref:`topic-format` implementation.
freelist.c    Freelist allocator implementation. See design.mps.freelist_.
freelist.h    Freelist allocator interface. See design.mps.freelist_.
global.c      Global arena implementation.
land.c        Land implementation. See design.mps.land_.
ld.c          :ref:`topic-location` implementation.
locus.c       Locus manager implementation. See design.mps.locus_.
locus.h       Locus manager interface. See design.mps.locus_.
message.c     :ref:`topic-message` implementation.
meter.c       Debugging accumulator implementation.
meter.h       Debugging accumulator interface.
misc.h        Miscellaneous constant and macro definitions.
mpm.c         Miscellaneous support functions. See design.mps.writef_.
mpm.h         Core MPS interface. ("MPM" = "Memory Pool Manager")
mpmst.h       Core data structure declarations.
mpmtypes.h    Core type declarations.
mpsi.c        External interface implementation. See design.mps.interface-c_.
mpsiw3.c      Additional external interface implementation for Windows.
mpswin.h      Wrapper for windows.h.
nailboard.c   Nailboard implementation. See design.mps.nailboard_.
nailboard.h   Nailboard interface. See design.mps.nailboard_.
policy.c      Collection policy decisions. See design.mps.strategy_.
pool.c        Pool implementation. See design.mps.pool_.
poolabs.c     Abstract pool classes.
poolmrg.c     Manual Rank Guardian pool implementation. See design.mps.poolmrg_.
poolmrg.h     Manual Rank Guardian pool interface. See design.mps.poolmrg_.
protocol.c    Inheritance protocol implementation. See design.mps.protocol_.
protocol.h    Inheritance protocol interface. See design.mps.protocol_.
range.c       Address ranges implementation. See design.mps.range_.
range.h       Address ranges interface. See design.mps.range_.
ref.c         Ranks and zones implementation.
ring.c        Ring implementation. See design.mps.ring_.
ring.h        Ring interface. See design.mps.ring_.
root.c        :ref:`topic-root` implementation.
sa.c          Sparse array implementation.
sa.h          Sparse array interface.
sac.c         :ref:`topic-cache` implementation.
sac.h         :ref:`topic-cache` interface.
sc.h          Stack context interface.
scan.c        :ref:`topic-scanning` functions.
seg.c         Segment implementation. See design.mps.seg_.
shield.c      Shield implementation. See design.mps.shield_.
splay.c       Splay tree implementation. See design.mps.splay_.
splay.h       Splay tree interface. See design.mps.splay_.
trace.c       Trace implementation. See design.mps.trace_.
traceanc.c    More trace implementation. See design.mps.trace_.
tract.c       Chunk and tract implementation. See design.mps.arena_.
tract.h       Chunk and tract interface. See design.mps.arena_.
tree.c        Binary tree implementation.
tree.h        Binary tree interface.
version.c     MPS version implementation. See design.mps.version_.
walk.c        Formatted object walker.
============  =================================================================


Platform interfaces
-------------------

These modules provide interfaces to features that are not available in
standard C, and so may need to be ported to new platforms. See
:ref:`topic-porting`.

============  =================================================================
File          Description
============  =================================================================
lock.h        Lock interface. See design.mps.lock_.
lockan.c      Lock implementation for standard C.
lockix.c      Lock implementation for POSIX.
lockw3.c      Lock implementation for Windows.
prmcan.c      Mutator context implementation for standard C.
prmci3.h      Mutator context interface for IA-32.
prmci3fr.c    Mutator context implementation for FreeBSD, IA-32.
prmci3li.c    Mutator context implementation for Linux, IA-32.
prmci3w3.c    Mutator context implementation for Windows, IA-32.
prmci3xc.c    Mutator context implementation for OS X, IA-32.
prmci6.h      Mutator context interface for x86-64.
prmci6fr.c    Mutator context implementation for FreeBSD, x86-64.
prmci6li.c    Mutator context implementation for Linux, x86-64.
prmci6w3.c    Mutator context implementation for Windows, x86-64.
prmci6xc.c    Mutator context implementation for OS X, x86-64.
prmcix.h      Mutator context interface for POSIX.
prmcw3.h      Mutator context interface for Windows.
prmcxc.h      Mutator context interface for OS X.
prot.h        Protection interface. See design.mps.prot_.
protan.c      Protection implementation for standard C.
proti3.c      Protection implementation for IA-32.
proti6.c      Protection implementation for x86-64.
protix.c      Protection implementation for POSIX.
protli.c      Protection implementation for Linux.
protsgix.c    Protection implementation for POSIX (signals part).
protw3.c      Protection implementation for Windows.
protxc.c      Protection implementation for OS X.
protxc.h      Protection interface for OS X.
pthrdext.c    Protection implementation for POSIX (threads part).
pthrdext.h    Protection interface for POSIX (threads part).
sp.h          Stack probe interface. See design.mps.sp_.
span.c        Stack probe implementation for standard C.
spw3i3.c      Stack probe implementation for Windows, IA-32.
spw3i6.c      Stack probe implementation for Windows, x86-64.
ss.c          Stack scanning implementation (common part).
ss.h          Stack scanning interface. See design.mps.ss_.
ssan.c        Stack scanning implementation for standard C.
ssixi3.c      Stack scanning implementation for POSIX, IA-32.
ssixi6.c      Stack scanning implementation for POSIX, x86-64.
ssw3i3mv.c    Stack scanning implementation for Windows, IA-32, Visual C.
ssw3i3pc.c    Stack scanning implementation for Windows, IA-32, Pelles C.
ssw3i6mv.c    Stack scanning implementation for Windows, x86-64, Visual C.
ssw3i6pc.c    Stack scanning implementation for Windows, x86-64, Pelles C.
th.h          Threads interface. See design.mps.thread-manager_.
than.c        Threads implementation for standard C.
thix.c        Threads implementation for POSIX.
thw3.c        Threads implementation for Windows.
thw3.h        Threads interface for Windows.
thw3i3.c      Threads implementation for Windows, IA-32.
thw3i6.c      Threads implementation for Windows, x86-64.
thxc.c        Threads implementation for OS X.
vm.c          Virtual memory implementation (common part).
vm.h          Virtual memory interface. See design.mps.vm_.
vman.c        Virtual memory implementation for standard C.
vmix.c        Virtual memory implementation for POSIX.
vmw3.c        Virtual memory implementation for Windows.
============  =================================================================


Pool classes
------------

These files implement the supported :term:`pool classes`. Some of
these (MFS, MV) are used internally by the MPS; the others are
available for :term:`client programs` only. See :ref:`pool`.

===========  ==================================================================
File         Description
===========  ==================================================================
poolamc.c    :ref:`pool-amc` implementation.
poolams.c    :ref:`pool-ams` implementation.
poolams.h    :ref:`pool-ams` internal interface.
poolawl.c    :ref:`pool-awl` implementation.
poollo.c     :ref:`pool-lo` implementation.
poolmfs.c    :ref:`pool-mfs` implementation.
poolmfs.h    :ref:`pool-mfs` internal interface.
poolmv.c     :ref:`pool-mv` implementation.
poolmv.h     :ref:`pool-mv` internal interface.
poolmv2.c    :ref:`pool-amc` implementation.
poolmv2.h    :ref:`pool-mvt` internal interface.
poolmvff.c   :ref:`pool-mvff` implementation.
poolsnc.c    :ref:`pool-snc` implementation.
===========  ==================================================================


Auxiliary programs
------------------

These files implement auxiliary programs. See
:ref:`topic-telemetry-utilities`.

===========  ==================================================================
File         Description
===========  ==================================================================
eventcnv.c   :ref:`telemetry-mpseventcnv`.
eventrep.c   Event replaying implementation (broken).
eventrep.h   Event replaying interface (broken).
eventsql.c   :ref:`telemetry-mpseventsql`.
eventtxt.c   :ref:`telemetry-mpseventtxt`.
getopt.h     Command-line option interface. Adapted from FreeBSD.
getoptl.c    Command-line option implementation. Adapted from FreeBSD.
replay.c     Event replaying program (broken).
table.c      Address-based hash table implementation.
table.h      Address-based hash table interface.
===========  ==================================================================


Benchmarks
----------

===========  ==================================================================
File         Description
===========  ==================================================================
djbench.c    Benchmark for manually managed pool classes.
gcbench.c    Benchmark for automatically managed pool classes.
===========  ==================================================================


Test support
------------

This is code that's shared between test cases.

============  =================================================================
File          Description
============  =================================================================
fmtdy.c       Dylan object format implementation.
fmtdy.h       Dylan object format interface.
fmtdytst.c    Dylan object constructor implementation.
fmtdytst.h    Dylan object constructor interface.
fmthe.c       Dylan-like object format with headers (implementation).
fmthe.h       Dylan-like object format with headers (interface).
fmtno.c       Null object format implementation.
fmtno.h       Null object format interface.
fmtscheme.c   Scheme object format implementation.
fmtscheme.h   Scheme object format interface.
pooln.c       Null pool implementation.
pooln.h       Null pool interface.
testlib.c     Test utilities implementation.
testlib.h     Test utilities interface.
testthr.h     Test threads interface. See design.mps.testthr_.
testthrix.c   Test threads implementation for POSIX.
testthrw3.c   Test threads implementation for Windows.
============  =================================================================


Interactive test cases
----------------------

These test cases provide harness for interacting with parts of the
MPS, for exploring the interface and testing by hand. These predate
the use of continuous integration: we wouldn't write this kind of test
case now.

===========  ==================================================================
File         Description
===========  ==================================================================
bttest.c     Interactive bit tables test harness.
teletest.c   Interactive telemetry test harness.
===========  ==================================================================


Automated test cases
--------------------

These are test cases that run automatically and form the main test
suite. See design.mps.tests_.

================  =============================================================
File              Description
================  =============================================================
abqtest.c         Fixed-length queue test.
airtest.c         Ambiguous interior reference test.
amcss.c           :ref:`pool-amc` stress test.
amcsshe.c         :ref:`pool-amc` stress test (using in-band headers).
amcssth.c         :ref:`pool-amc` stress test (using multiple threads).
amsss.c           :ref:`pool-ams` stress test.
amssshe.c         :ref:`pool-ams` stress test (using in-band headers).
apss.c            :ref:`topic-allocation-point` stress test.
arenacv.c         Arena coverage test.
awlut.c           :ref:`pool-awl` unit test.
awluthe.c         :ref:`pool-awl` unit test (using in-band headers).
awlutth.c         :ref:`pool-awl` unit test (using multiple threads).
btcv.c            Bit table coverage test.
exposet0.c        :c:func:`mps_arena_expose` test.
expt825.c         Regression test for job000825_.
fbmtest.c         Free block manager (CBS and Freelist) test.
finalcv.c         :ref:`topic-finalization` coverage test.
finaltest.c       :ref:`topic-finalization` test.
fotest.c          Failover allocator test.
landtest.c        Land test.
locbwcss.c        Locus backwards compatibility stress test.
lockcov.c         Lock coverage test.
lockut.c          Lock unit test.
locusss.c         Locus stress test.
locv.c            :ref:`pool-lo` coverage test.
messtest.c        :ref:`topic-message` test.
mpmss.c           Manual allocation stress test.
mpsicv.c          External interface coverage test.
mv2test.c         :ref:`pool-mvt` test.
nailboardtest.c   Nailboard test.
poolncv.c         Null pool class test.
qs.c              Quicksort test.
sacss.c           :ref:`topic-cache` stress test.
segsmss.c         Segment splitting and merging stress test.
steptest.c        :c:func:`mps_arena_step` test.
tagtest.c         Tagged pointer scanning test.
walkt0.c          Formatted object walking test.
zcoll.c           Garbage collection progress test.
zmess.c           Garbage collection and finalization message test.
================  =============================================================


Build infrastructure
--------------------

These are makefiles (and makefile fragments) used to build the MPS.
See :ref:`topic-porting`.

=============  ================================================================
File           Description
=============  ================================================================
anangc.gmk     GNU makefile for platform ANANGC.
ananll.gmk     GNU makefile for platform ANANLL.
ananmv.nmk     NMAKE file for platform ANANMV.
comm.gmk       Common GNU make fragment.
commpost.nmk   Common NMAKE fragment (included before the compiler fragment).
commpre.nmk    Common NMAKE fragment (included after the compiler fragment).
fri3gc.gmk     GNU makefile for platform FRI3GC.
fri3ll.gmk     GNU makefile for platform FRI3LL.
fri6gc.gmk     GNU makefile for platform FRI6GC.
fri6ll.gmk     GNU makefile for platform FRI6LL.
gc.gmk         GNU make fragment for GCC.
gp.gmk         GNU make fragment for GCC/GProf (broken).
lii3gc.gmk     GNU makefile for platform LII3GC.
lii6gc.gmk     GNU makefile for platform LII6GC.
lii6ll.gmk     GNU makefile for platform LII6LL.
ll.gmk         GNU make fragment for Clang/LLVM.
mv.nmk         NMAKE fragment for Microsoft Visual C.
pc.nmk         NMAKE fragment for Pelles C.
w3i3mv.nmk     NMAKE file for platform W3I3MV.
w3i3pc.nmk     NMAKE file for platform W3I3PC.
w3i6mv.nmk     NMAKE file for platform W3I6MV.
w3i6pc.nmk     NMAKE file for platform W3I6PC.
xci3gc.gmk     GNU makefile for platform XCI3GC.
xci6ll.gmk     GNU makefile for platform XCI6LL.
=============  ================================================================


.. _design.mps.abq: design/abq.html
.. _design.mps.arena: design/arena.html
.. _design.mps.bootstrap: design/bootstrap.html
.. _design.mps.bt: design/bt.html
.. _design.mps.buffer: design/buffer.html
.. _design.mps.cbs: design/cbs.html
.. _design.mps.check: design/check.html
.. _design.mps.config: design/config.html
.. _design.mps.failover: design/failover.html
.. _design.mps.freelist: design/freelist.html
.. _design.mps.interface-c: design/interface-c.html
.. _design.mps.land: design/land.html
.. _design.mps.lock: design/lock.html
.. _design.mps.locus: design/locus.html
.. _design.mps.nailboard: design/nailboard.html
.. _design.mps.pool: design/pool.html
.. _design.mps.poolmrg: design/poolmrg.html
.. _design.mps.prmc: design/prmc.html
.. _design.mps.protocol: design/protocol.html
.. _design.mps.prot: design/prot.html
.. _design.mps.range: design/range.html
.. _design.mps.ring: design/ring.html
.. _design.mps.seg: design/seg.html
.. _design.mps.shield: design/shield.html
.. _design.mps.sp: design/sp.html
.. _design.mps.splay: design/splay.html
.. _design.mps.ss: design/ss.html
.. _design.mps.strategy: design/strategy.html
.. _design.mps.tests: design/tests.html
.. _design.mps.testthr: design/testthr.html
.. _design.mps.thread-manager: design/thread-manager.html
.. _design.mps.trace: design/trace.html
.. _design.mps.version: design/version.html
.. _design.mps.vm: design/vm.html
.. _design.mps.writef: design/writef.html
.. _job000825: https://www.ravenbrook.com/project/mps/issue/job000825
