<?xml version="1.0" encoding="iso-8859-1"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
        "http://www.w3.org/TR/2000/REC-xhtml1-20000126/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
<link rev="MADE" href="mailto:mm-web@ravenbrook.com" />
<link rel="Start" href="../" />
<link rel="Contents" href="../" />
<link rel="Glossary" href="" />
<meta name="DC.Date" content="2000-12-15T20:16:56" />
<meta name="description" content="Glossary of terms related to memory management" />
<meta name="keywords" content="dictionary, glossary, definition, memory management, garbage collection" />
<link rel="index" href="./" />
<link rel="help" href="help.html" />
<link rel="prev" href="c.html" />
<link rel="next" href="e.html" />
<title>The Memory Management Glossary: D</title>
</head>
<body lang="en" bgcolor="#FFFFFF" text="#000000" link="#000099" vlink="#660066" alink="#FF0000">
<h1 align="center">The Memory Management Glossary<br />
<big>D</big></h1>
<div align="center">&#160;<a href=".././">Contents</a> |
<a href="../news/">News</a> |
<a href="./">Glossary</a> |
<a href="../faq.html">FAQ</a> |
<a href="../articles/">Articles</a> |
<a href="../bib/">Bibliography</a> |
<a href="../links.html">Links</a> |
<a href="../feedback.html">Feedback</a></div>
<hr size="1" noshade="noshade" />
<p align="center"><a href="a.html">A</a>
<a href="b.html">B</a>
<a href="c.html">C</a>
<strong>D</strong>
<a href="e.html">E</a>
<a href="f.html">F</a>
<a href="g.html">G</a>
<a href="h.html">H</a>
<a href="i.html">I</a>
J
<a href="k.html">K</a>
<a href="l.html">L</a>
<a href="m.html">M</a>
<a href="n.html">N</a>
<a href="o.html">O</a>
<a href="p.html">P</a>
<a href="q.html">Q</a>
<a href="r.html">R</a>
<a href="s.html">S</a>
<a href="t.html">T</a>
<a href="u.html">U</a>
<a href="v.html">V</a>
<a href="w.html">W</a>
X
Y
<a href="z.html">Z</a> - <a href="help.html">Help</a></p>
<p>Our aim is for these entries to be accurate, comprehensible, and useful, and also to have an entry for all common memory management terms.  If you can't find the term you're looking for, if our definition doesn't help you, or if you'd like to suggest corrections or additions, please let us know via our <a href="../feedback.html">feedback page</a>.</p>

<p>For an explanation of the structure of the entries, and information on how to link to definitions, please see the <a href="help.html">glossary help page</a>.</p>

<hr />
<dl>
<dt><strong><a id="dangling.pointer" name="dangling.pointer">dangling pointer</a></strong></dt>
<dd><p>A dangling <a href="p.html#pointer">pointer</a> is a surviving <a href="r.html#reference">reference</a> to an <a href="o.html#object">object</a> that no longer exists at that <a href="a.html#address">address</a> </p>
<p>In <a href="m.html#manual.memory.management">manual memory management</a>, dangling pointers typically arise from one of:</p>

<ul>
  <li>A <a href="p.html#premature.free">premature free</a>, where an object is <a href="f.html#free-1">freed</a>, but a reference is retained;</li>
  <li>Retaining a reference to a <a href="s.html#stack.allocation">stack-allocated</a> object, after the relevant <a href="s.html#stack.frame">stack frame</a> has been popped.</li>
</ul>

<p>Dangling pointers can occur under <a href="a.html#automatic.memory.management">automatic memory management</a>, because of a <a href="g.html#garbage.collection">garbage collection</a> bug -- such as premature collection, or <a href="m.html#moving.garbage.collector">moving</a> without updating all <a href="r.html#reference">references</a> -- but this is much rarer because <a href="g.html#gc">GC</a> code is usually a single common core of reused code.</p></dd>


<dt><strong><a id="data.stack" name="data.stack">data stack</a></strong></dt>
<dd><p>A <a href="s.html#stack">stack</a> used to manage the storage of <a href="s.html#stack.allocation">stack-allocated</a> <a href="o.html#object">objects</a>, other than <a href="a.html#activation.record">activation records</a>, often under program control.</p>
<p>Because of the limitations that may be imposed on the <a href="c.html#control.stack">control stack</a>, or to support stack-like semantics for certain data structures, some language implementations manage additional data stacks in software for storing objects that have <a href="#dynamic.extent">dynamic extent</a> but that do not fit within the constraints of the control stack.</p>

<p><strong>See also:</strong> <a href="c.html#control.stack">control stack</a>.
</p></dd>

<dt><strong><a id="dead" name="dead">dead</a></strong></dt>
<dd><p>An <a href="o.html#object">object</a> is dead if it is not <a href="l.html#live">live</a>; that is, when the <a href="m.html#mutator">mutator</a> cannot reach any state in which it accesses the object.</p>
<p>It is not possible, in general, for <a href="g.html#garbage.collector">garbage collectors</a> to determine exactly which <a href="o.html#object">objects</a> are dead and which are live.  Instead, they use some approximation to detect objects that are provably dead, such as those that are <a href="u.html#unreachable">unreachable</a>.</p>

<p><strong>Opposites:</strong> <a href="l.html#live">live</a>.
<br />
<strong>See also:</strong> <a href="g.html#garbage">garbage</a>;
    <a href="u.html#undead">undead</a>;
    <a href="f.html#free-3">free<sup><small>(3)</small></sup></a>.
</p></dd>

<dt><strong><a id="deallocate" name="deallocate">deallocate</a></strong>
  (for full details, see <a href="f.html#free-1">free<sup><small>(1)</small></sup></a>)</dt>
<dd><p>In <a href="m.html#manual.memory.management">manual memory management</a>, to free or deallocate an <a href="o.html#object">object</a> is to tell the <a href="m.html#memory.manager">memory manager</a> that it is no longer needed.  The <a href="m.html#memory-1">memory<sup><small>(1)</small></sup></a> may then be <a href="r.html#recycle">recycled</a> by being used for subsequent <a href="a.html#allocate">allocation</a>, or by being returned to the operating system.</p></dd>

<dt><strong><a id="deferred.coalescing" name="deferred.coalescing">deferred coalescing</a></strong></dt>
<dd><p>Deferred coalescing is a policy which <a href="c.html#coalesce">coalesces</a> <a href="f.html#free.block">free blocks</a> some time after the blocks are freed, as opposed to coalescing free blocks immediately as they are freed.</p>
<p>Adjacent free blocks can be coalesced to form larger free blocks; deferred coalescing is a catch-all for policies which perform this coalescing sometime after the blocks were freed.</p>

<p>Given this rather flexible definition there are a number of choices for when to coalesce: as the <a href="f.html#free.list">free list</a> is traversed during allocation, when the allocation cannot be satisfied from the free list, periodically, and so on.  In addition there are choices to be made regarding how much coalescing to perform at any one time.</p></dd>


<dt><strong><a id="deferred.reference.counting" name="deferred.reference.counting">deferred reference counting</a></strong></dt>
<dd><p>Deferred <a href="r.html#reference.counting">reference counting</a> reduces the cost of maintaining reference counts by avoiding adjustments when the <a href="r.html#reference">reference</a> is stored on the <a href="s.html#stack">stack</a>.</p>
<p>On many systems, the majority of stores are made into local variables, which are kept on the stack.  Deferred reference counting leaves those out and counts only references stored in <a href="h.html#heap">heap</a> objects.  This requires compiler support, but can lead to substantial performance improvements.</p>

<p><a href="o.html#object">Objects</a> cannot be <a href="r.html#reclaim">reclaimed</a> as soon as their reference count becomes zero, because there might still be references to them from the stack.  Such objects are added to a <a href="z.html#zero.count.table">zero count table</a> (ZCT) instead.  If a reference to an object with a count of zero is stored into the heap, then the object is removed from the ZCT.  Periodically the stack is <a href="s.html#scan">scanned</a>, and any objects in the ZCT which were not referenced from the stack are reclaimed.</p>

<p>Deferred reference counting has been used successfully with several languages, notably <a href="../articles/lang.html#smalltalk">Smalltalk</a>.  However, since it fails to collect objects with <a href="c.html#cyclic.data.structure">cyclic</a> references, it is often used alongside a <a href="t.html#tracing.garbage.collection">tracing garbage collector</a>.</p>

<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#db76">L. Peter Deutsch, Daniel G. Bobrow. 1976. <cite>An Efficient, Incremental, Automatic Garbage Collector</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="derived.pointer" name="derived.pointer">derived pointer</a></strong>
  (for full details, see <a href="i.html#interior.pointer">interior pointer</a>)</dt>
<dd><p>An <em>interior pointer</em> is a pointer to <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> occupied by an <a href="o.html#object">object</a> which does not point to the start location.  Also called a <em>derived pointer</em> when it's derived from a <a href="b.html#base.pointer">base pointer</a>.</p></dd>

<dt><strong><a id="destructor-1" name="destructor-1">destructor<sup><small>(1)</small></sup></a></strong></dt>
<dd><p>A destructor is a function or a method that performs the explicit <a href="f.html#free-1">deallocation</a> of an <a href="o.html#object">object</a>.  It may also perform clean-up actions.</p>
<p><strong>Opposites:</strong> <a href="c.html#constructor-1">constructor<sup><small>(1)</small></sup></a>.
</p></dd>

<dt><strong><a id="destructor-2" name="destructor-2">destructor<sup><small>(2)</small></sup></a></strong></dt>
<dd><p>In <a href="../articles/lang.html#cplusplus">C++</a>, a <em>destructor</em> is a member function that is used to clean up when an object is being <a href="f.html#free-1">deallocated</a>.</p>
<p>When an object is being destroyed (by <code>delete</code> or automatically), the appropriate destructor is called, and then the actual deallocation of <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> is performed by <code>operator delete</code> or the run-time system (for <a href="s.html#static.allocation">static</a> and <a href="s.html#stack.allocation">stack allocation</a>).</p>

<p><strong>See also:</strong> <a href="c.html#constructor-2">constructor<sup><small>(2)</small></sup></a>.
</p></dd>

<dt><strong><a id="dgc" name="dgc">DGC</a></strong>
  (for full details, see <a href="#distributed.garbage.collection">distributed garbage collection</a>)</dt>
<dd><p>Distributed garbage collection is <a href="g.html#garbage.collection">garbage collection</a> in a system where <a href="o.html#object">objects</a> might not reside in the same <a href="a.html#address.space">address space</a> or even on the same machine.</p></dd>

<dt><strong><a id="direct.method" name="direct.method">direct method</a></strong></dt>
<dd><p>Direct methods of <a href="a.html#automatic.memory.management">automatic memory management</a> maintain information about the <a href="l.html#live">liveness</a> of each <a href="o.html#object">object</a>, detecting <a href="g.html#garbage">garbage</a> directly.</p>
<p>Such bits of information, e.g.,  <a href="r.html#reference.counting">reference counts</a>, are typically stored within the objects themselves.</p>

<p>Direct <a href="g.html#garbage.collection">garbage collection</a> can allow <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> to be <a href="r.html#reclaim">reclaimed</a> as soon as it becomes <a href="u.html#unreachable">unreachable</a>.  However, the stored information must be updated as the <a href="g.html#graph">graph</a> of objects changes; this may be an expensive operation, especially in <a href="#distributed.garbage.collection">distributed garbage collection</a> where it can lead to intensive communication between processors, and make garbage collection less robust to network failures.</p>

<p><strong>Opposites:</strong> <a href="i.html#indirect.method">indirect method</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#jones96">Richard E. Jones, Rafael Lins. 1996. <cite>Garbage Collection: Algorithms for Automatic Dynamic Memory Management</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="dirty.bit" name="dirty.bit">dirty bit</a></strong></dt>
<dd><p>A dirty bit is a flag indicating that a <a href="p.html#page">page</a> (or similar) has been written to since it was last examined.</p>
<p>Dirty bits are used by <a href="c.html#cache-2">caches<sup><small>(2)</small></sup></a> to determine which pages must be written out, and by garbage collectors in conjunction with <a href="w.html#write.barrier">write barriers</a>.</p></dd>


<dt><strong><a id="distributed.garbage.collection" name="distributed.garbage.collection">distributed garbage collection</a></strong>
  (also known as DGC)</dt>
<dd><p>Distributed garbage collection is <a href="g.html#garbage.collection">garbage collection</a> in a system where <a href="o.html#object">objects</a> might not reside in the same <a href="a.html#address.space">address space</a> or even on the same machine.</p>
<p>Distributed garbage collection is difficult to achieve in widely-distributed systems (over wide-area networks) because of the costs of synchronization and communication between processes. These costs are particularly high for a <a href="t.html#tracing.garbage.collection">tracing garbage collector</a>, so other techniques, including <a href="w.html#weighted.reference.counting">weighted reference counting</a>, are commonly used instead. </p></dd>


<dt><strong><a id="double.buddies" name="double.buddies">double buddies</a></strong></dt>
<dd><p>A <a href="b.html#buddy.system">buddy system</a> <a href="a.html#allocation.mechanism">allocation mechanism</a> using a pair of <a href="b.html#binary.buddies">binary buddy</a> systems with staggered size classes.</p>
<p>One system is a pure binary buddy, with powers-of-two classes (2, 4, 8,...).  The other uses some fixed multiple of powers-of-two (e.g., 3, 6, 12, ...).  This resembles <a href="w.html#weighted.buddies">weighted buddies</a>, but the two buddy systems are treated independently: blocks cannot be <a href="s.html#split">split</a> or <a href="c.html#coalesce">coalesced</a> from one to the other.</p>

<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wise78">David S. Wise. 1978. <cite>The double-buddy system</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="double.free" name="double.free">double free</a></strong></dt>
<dd><p>A double free is when an attempt is made to <a href="f.html#free-1">free<sup><small>(1)</small></sup></a> a <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> <a href="b.html#block">block</a> that has already been freed.</p>
<p>This usually occurs in <a href="m.html#manual.memory.management">manual memory management</a> when two parts of a program believe they are responsible for the management of the same block.</p>

<p>Many manual <a href="m.html#memory.manager">memory managers</a> have great trouble with double frees, because they cannot cheaply determine that <a href="f.html#free-1">deallocated</a> blocks were already free.  Instead, they corrupt their <a href="f.html#free.block.chain">free block chain</a>, which leads to mysterious problems when the same block is subsequently <a href="a.html#allocate">allocated</a>.</p>

<p><strong>See also:</strong> <a href="p.html#premature.free">premature free</a>.
</p></dd>

<dt><strong><a id="doubleword" name="doubleword">doubleword</a></strong>
  (also known as longword)</dt>
<dd><p>A <em>doubleword</em> is a unit of memory consisting of two adjacent <a href="w.html#word">words</a>.  In digital's Alpha architecture, it's called <em>a longword</em>.</p>
<p><strong>Historical note:</strong> On the Intel&reg; 80386, 80486. and Pentium&reg; processors, the doubleword of 32 bits is actually the <em>natural word size</em>, but the term <em>word</em> is still used for the 16-bit unit, as it was on earlier processors of this series.</p>

<p><strong>See also:</strong> <a href="q.html#quadword">quadword</a>.
</p></dd>

<dt><strong><a id="dram" name="dram">DRAM</a></strong>
  (for full details, see <a href="#dynamic.memory">dynamic memory</a>)</dt>
<dd><p>Dynamic memory, or dynamic RAM (DRAM, pronounced "dee ram"), is a type of <a href="r.html#ram">RAM</a>.</p></dd>

<dt><strong><a id="dynamic.allocation" name="dynamic.allocation">dynamic allocation</a></strong>
  (for full details, see <a href="h.html#heap.allocation">heap allocation</a>)</dt>
<dd><p><em>Heap allocation</em> or <em>dynamic allocation</em> means run-time <a href="a.html#allocate">allocation</a> and <a href="f.html#free-1">deallocation</a> of <a href="s.html#storage">storage</a> in arbitrary order.</p></dd>

<dt><strong><a id="dynamic.extent" name="dynamic.extent">dynamic extent</a></strong></dt>
<dd><p>An <a href="o.html#object">object</a> has dynamic <a href="e.html#extent">extent</a> if its <a href="l.html#lifetime">lifetime</a> is bounded by the execution of a function or some other block construct.</p>
<p>Objects of dynamic extent are usually <a href="s.html#stack.allocation">stack-allocated</a>.</p>

<p><strong>Similar terms:</strong> <a href="a.html#automatic.storage.duration">automatic storage duration</a>.
<br />
<strong>Opposites:</strong> <a href="i.html#indefinite.extent">indefinite extent</a>.
</p></dd>

<dt><strong><a id="dynamic.memory" name="dynamic.memory">dynamic memory</a></strong>
  (also known as dynamic RAM, DRAM)</dt>
<dd><p>Dynamic memory, or dynamic RAM (DRAM, pronounced "dee ram"), is a type of <a href="r.html#ram">RAM</a>.</p>
<p>Dynamic RAM requires periodic refreshing to avoid losing its contents (as opposed to <a href="s.html#static.memory-1">static memory<sup><small>(1)</small></sup></a>, the contents of which are preserved without any need for refreshing).  The refreshing is performed by additional "refresh hardware" usually external to the dynamic RAM package itself, sometimes by the main CPU.  Dynamic RAM is cheap and compact and is the choice for large amounts of relatively fast RAM, such as the <a href="m.html#main.memory">main memory</a> of PCs.  Dynamic RAM often comes packaged in SIMMs or DIMMs.</p>

<p><strong>See also:</strong> <a href="s.html#static.memory-1">static memory<sup><small>(1)</small></sup></a>;
    <a href="s.html#sdram">SDRAM</a>.
</p></dd>

<dt><strong><a id="dynamic.ram" name="dynamic.ram">dynamic RAM</a></strong>
  (for full details, see <a href="#dynamic.memory">dynamic memory</a>)</dt>
<dd><p>Dynamic memory, or dynamic RAM (DRAM, pronounced "dee ram"), is a type of <a href="r.html#ram">RAM</a>.</p></dd>

</dl>
<p align="center"><a href="a.html">A</a>
<a href="b.html">B</a>
<a href="c.html">C</a>
<strong>D</strong>
<a href="e.html">E</a>
<a href="f.html">F</a>
<a href="g.html">G</a>
<a href="h.html">H</a>
<a href="i.html">I</a>
J
<a href="k.html">K</a>
<a href="l.html">L</a>
<a href="m.html">M</a>
<a href="n.html">N</a>
<a href="o.html">O</a>
<a href="p.html">P</a>
<a href="q.html">Q</a>
<a href="r.html">R</a>
<a href="s.html">S</a>
<a href="t.html">T</a>
<a href="u.html">U</a>
<a href="v.html">V</a>
<a href="w.html">W</a>
X
Y
<a href="z.html">Z</a> - <a href="help.html">Help</a></p>

</body></html>