<?xml version="1.0" encoding="iso-8859-1"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
        "http://www.w3.org/TR/2000/REC-xhtml1-20000126/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
<link rev="MADE" href="mailto:mm-web@ravenbrook.com" />
<link rel="Start" href="../" />
<link rel="Contents" href="../" />
<link rel="Glossary" href="" />
<meta name="DC.Date" content="2000-12-15T20:16:59" />
<meta name="description" content="Glossary of terms related to memory management" />
<meta name="keywords" content="dictionary, glossary, definition, memory management, garbage collection" />
<link rel="index" href="./" />
<link rel="help" href="help.html" />
<link rel="prev" href="q.html" />
<link rel="next" href="s.html" />
<title>The Memory Management Glossary: R</title>
</head>
<body lang="en" bgcolor="#FFFFFF" text="#000000" link="#000099" vlink="#660066" alink="#FF0000">
<h1 align="center">The Memory Management Glossary<br />
<big>R</big></h1>
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
<a href="d.html">D</a>
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
<strong>R</strong>
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
<dt><strong><a id="ram" name="ram">RAM</a>, random access memory</strong></dt>
<dd><p>RAM (Random Access Memory) is a type of <a href="p.html#physical.memory-2">physical memory<sup><small>(2)</small></sup></a> that can be read from and written to.</p>
<p><strong>Similar terms:</strong> <a href="m.html#main.memory">main memory</a>.
<br />
<strong>See also:</strong> <a href="#rom">ROM</a>;
    <a href="s.html#static.ram">static RAM</a>;
    <a href="d.html#dynamic.ram">dynamic RAM</a>.
</p></dd>

<dt><strong><a id="raw" name="raw">raw</a></strong>
  (for full details, see <a href="u.html#unwrapped">unwrapped</a>)</dt>
<dd><p>A value is <em>unwrapped</em> or <em>raw</em> if it is not encoded with type information.</p></dd>

<dt><strong><a id="reachable" name="reachable">reachable</a></strong></dt>
<dd><p>An <a href="o.html#object">object</a> is <em>reachable</em> if it is <a href="#reference">referred</a> to by a <a href="#root">root</a>, or is referred to by a reachable object; that is, if it can be reached from the roots by following <a href="#reference">references</a>.</p>
<p>Reachability is used as an approximation to <a href="l.html#live">liveness</a> in <a href="t.html#tracing.garbage.collection">tracing garbage collection</a>.</p>

<p>In <a href="../articles/lang.html#java">Java</a><sup><small class="tm-small">TM</small></sup>, the <a href="#reference.object">reference objects</a> together with ordinary references and <a href="f.html#finalization">finalization</a> generate a hierarchy of reachability that guides the <a href="c.html#collector-1">collector<sup><small>(1)</small></sup></a> on what to do when an object is about to <a href="d.html#dead">die</a>.  There are six strengths:</p>

<ul>
  <li><a href="s.html#strongly.reachable">strongly reachable</a>;</li>
  <li><a href="s.html#softly.reachable">softly reachable</a>;</li>
  <li><a href="w.html#weakly.reachable">weakly reachable</a>;</li>
  <li><a href="f.html#finalization">finalizable</a>;</li>
  <li><a href="p.html#phantom.reachable">phantom reachable</a>;</li>
  <li><a href="u.html#unreachable">unreachable</a>.</li>
</ul>

<p>Basically, an object is only as reachable as the weakest link in the strongest path from the roots.  Note that the Java specification's description of the reachabilities is a bit patchy, but that's what it intends.  It is unspecified where Java Native Interface's <em>weak global references</em> fit into this.</p>

<p><strong>Similar terms:</strong> <a href="l.html#live">live</a>.
<br />
<strong>Opposites:</strong> <a href="u.html#unreachable">unreachable</a>.
<br />
<strong>Other links:</strong> <a href="http://java.sun.com/products/jdk/1.2/docs/guide/refobs/index.html">Java spec for reference objects</a>;
    <a href="http://developer.java.sun.com/developer/technicalArticles/ALT/RefObj/index.html">Reference Objects and Garbage Collection</a>.
</p></dd>

<dt><strong><a id="read.barrier" name="read.barrier">read barrier</a>, read-barrier</strong></dt>
<dd><p>A read <a href="b.html#barrier-1">barrier<sup><small>(1)</small></sup></a> is a block on reading from certain <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> <a href="m.html#memory.location">locations</a> by certain threads or processes.</p>
<p><strong>Relevance to memory management:</strong> Read barriers are used for <a href="i.html#incremental.garbage.collection">incremental</a> or <a href="p.html#parallel.garbage.collection">concurrent</a> <a href="g.html#garbage.collection">garbage collection</a>.</p>

<p><strong>See also:</strong> <a href="w.html#write.barrier">write barrier</a>.
</p></dd>

<dt><strong><a id="read.fault" name="read.fault">read fault</a></strong></dt>
<dd><p>An exception which occurs when reading from an address in <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a>.</p>
<p>This is probably either a <a href="p.html#page.fault">page fault</a>, an <a href="i.html#invalid.page.fault">invalid page fault</a> or a <a href="p.html#protection.fault">protection fault</a>.</p>

<p><strong>Similar terms:</strong> <a href="s.html#segmentation.violation">segmentation violation</a>.
<br />
<strong>See also:</strong> <a href="w.html#write.fault">write fault</a>.
</p></dd>

<dt><strong><a id="real.memory-1" name="real.memory-1">real memory<sup><small>(1)</small></sup></a></strong></dt>
<dd><p>A system with no <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> capability can be said to have <em>real memory</em>.</p>
<p><strong>Historical note:</strong> On older architectures, programs could only directly access data in real memory.  Where this was inefficient, they had to store data on disk, and sometimes had alternate portions of program image called <em>overlays</em>.</p>

<p><strong>Opposites:</strong> <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a>.
</p></dd>

<dt><strong><a id="real.memory-2" name="real.memory-2">real memory<sup><small>(2)</small></sup></a></strong>
  (for full details, see <a href="p.html#physical.memory-1">physical memory<sup><small>(1)</small></sup></a>)</dt>
<dd><p>Physical memory is <a href="m.html#memory-1">memory<sup><small>(1)</small></sup></a> that is wired to directly to the processor, addressable by <a href="p.html#physical.address">physical address</a>.</p></dd>

<dt><strong><a id="reclaim" name="reclaim">reclaim</a></strong></dt>
<dd><p><i>Reclaiming</i> an <a href="o.html#object">object</a> or the <a href="s.html#storage">storage</a> occupied by it is making it available for reuse after the object is no longer needed.</p>
<p>This word is usually used only in connection with <a href="a.html#automatic.memory.management">automatic memory management</a>.</p>

<p><strong>Similar terms:</strong> <a href="#recycle">recycle</a>.
</p></dd>

<dt><strong><a id="recycle" name="recycle">recycle</a></strong></dt>
<dd><p><i>Recycling</i> <a href="s.html#storage">storage</a> means making it available for reuse after it has been occupied by an <a href="o.html#object">object</a> that is no longer needed.</p>
<p>In simple cases, this might simply involve adding a <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> <a href="b.html#block">block</a> to the <a href="f.html#free.list">free list</a>.  Another
possibility is <a href="u.html#unmapped">unmapping</a> memory so that the <a href="b.html#backing.store">backing store</a> can be allocated to another process.</p>

<p><strong>Similar terms:</strong> <a href="#reclaim">reclaim</a>.
<br />
<strong>Other links:</strong> <a href="../articles/recycle.html">Beginner's Guide: Recycling techniques</a>.
</p></dd>

<dt><strong><a id="reference" name="reference">reference</a></strong></dt>
<dd><p>In memory management, <em>a reference</em> is the general term for a link from one <a href="o.html#object">object</a> to another.  Some programming languages have more specific meanings for the term.</p>
<p>The terms "<a href="p.html#pointer">pointer</a>" and "reference" are often interchangeable, but some programming languages differentiate the two in subtle ways.</p>

<p><strong>Similar terms:</strong> <a href="a.html#address">address</a>;
    <a href="p.html#pointer">pointer</a>.
</p></dd>

<dt><strong><a id="reference.counting" name="reference.counting">reference counting</a></strong></dt>
<dd><p>Reference counting systems perform <a href="a.html#automatic.memory.management">automatic memory management</a> by keeping a count in each <a href="o.html#object">object</a>, usually in a <a href="h.html#header">header</a>, of how many <a href="#reference">references</a> there are to the object.  Objects to which there are no references cannot be accessed by the <a href="m.html#mutator">mutator</a>; they are therefore <a href="d.html#dead">dead</a> and may be <a href="#reclaim">reclaimed</a>.</p>
<p>The reference count is incremented for each new reference, and is decremented if a reference is overwritten, or if the referring object is recycled.  If a reference count falls to zero, then the object is no  longer required and can be recycled.</p>

<p>There are four main problems with simple reference counting:</p>
<ul>
  <li>The reference count field usually has to have limited size, and the system therefore breaks down if the number of possible references to an object is unbounded;</li>
  <li>Reference counting involves an operation on every modification of a pointer, which increases code size, increases demand for <a href="m.html#memory.bandwidth">memory bandwidth</a>, decreases <a href="l.html#locality.of.reference">locality of reference</a> and can be a serious performance penalty (especially in multi-threaded environments where reference count updates require synchronization);</li>
  <li>Every object needs to be slightly larger in order to store the reference count;</li>
  <li>If any objects are part of a <a href="c.html#cyclic.data.structure">cyclic data structure</a> then they will always have a non-zero reference count, and hence won't be reclaimed when they are dead.</li>
</ul>

<p align="center"><em>Garbage with non-zero reference counts</em><br /><img alt="Diagram: Garbage with non-zero reference counts" src="../diagrams/refloop.png" border="2" height="216" width="352" /></p>

<p>Reference counting has the advantage that it can reclaim objects promptly, and for this reason it is often used to reclaim non-cyclic data structures in file systems, databases and  operating system kernels.  When there is a possibility of cyclic data structures, reference counting is sometimes used together with a <a href="t.html#tracing.garbage.collection">tracing garbage collector</a> that runs infrequently.  Such combinations are generally less efficient than using a tracing collector by itself, but the promptness of reference counting may be important.</p>

<p>Pauses due to reference counting are typically fairly short, and it may be appropriate as a form of <a href="i.html#incremental.garbage.collection">incremental garbage collection</a>.  But removing a single reference may cause the recycling of a large number of objects at once, so it is not suited to real-time systems where minimum pause times must be guaranteed.  There are more complex variations of the technique that address this problem.</p>

<p>Reference counting is often used because it can be implemented without any support from the language or compiler.  In <a href="../articles/lang.html#cplusplus">C++</a> this can be encapsulated in a class, using a <a href="s.html#smart.pointer">smart pointer</a>.  However, it would normally be more efficient to use a tracing garbage collector instead.  The performance of reference counting can be improved substantially with compiler support, using refinements such as <a href="d.html#deferred.reference.counting">deferred reference counting</a>, which has been successfully used in <a href="../articles/lang.html#smalltalk">Smalltalk</a> and other languages.</p>

<p>Despite the problems, reference counting is often used for <a href="d.html#distributed.garbage.collection">distributed garbage collection</a>.  This is because refinements such as <a href="w.html#weighted.reference.counting">weighted reference counting</a> require less inter-process communication than <a href="t.html#trace">tracing</a>.</p>

<p><strong>See also:</strong> <a href="l.html#limited-field.reference.count">limited-field reference count</a>;
    <a href="o.html#one-bit.reference.count">one-bit reference count</a>.
</p></dd>

<dt><strong><a id="reference.object" name="reference.object">reference object</a></strong></dt>
<dd><p>In <a href="../articles/lang.html#java">Java</a><sup><small class="tm-small">TM</small></sup>, a <em>reference object</em> (<code>java.lang.ref.Reference</code>) encapsulates a <a href="#reference">reference</a> to some other object, in order to make the <a href="g.html#garbage.collector">garbage collector</a> handle it specially.  In particular, a Java program can use this to detect when the referent becomes <a href="u.html#unreachable">unreachable</a>.</p>
<p>Basically, the encapsulated reference is a <a href="w.html#weak.reference-1">weak reference<sup><small>(1)</small></sup></a>; it will be cleared by the <a href="c.html#collector-1">collector<sup><small>(1)</small></sup></a> when all other references to the referent have disappeared.  However, in order to better control what happens at the end of an object's <a href="l.html#lifetime">lifetime</a>, Java 1.2 provides three classes of reference objects, each with its own peculiarities: <code>SoftReference</code>, <code>WeakReference</code>, and <code>PhantomReference</code>.  Each of these classes has its uses in managing memory.  The reference objects together with ordinary references and <a href="f.html#finalization">finalization</a> generate a hierarchy of <a href="#reachable">reachability</a> (q.v.) that guides the collector on what to do when an object is about to <a href="d.html#dead">die</a>.</p>

<p>A reference object can be <em>registered</em> with a queue, and it will be enqueued when the collector determines that the referent is <a href="s.html#softly.reachable">softly</a>, <a href="w.html#weakly.reachable">weakly</a> or <a href="p.html#phantom.reachable">phantom reachable</a>, as the case may be.  A program can use these queues to perform some action when an object is dying.  This allows finer control than the older <a href="f.html#finalization">finalization</a> mechanism alone.</p>

<p><strong>Historical note:</strong> This feature was introduced in Java 1.2 (confusingly, part of the Java<sup><small class="tm-small">TM</small></sup> 2 Platform).</p>

<p><strong>See also:</strong> <a href="s.html#soft.reference">soft reference</a>;
    <a href="w.html#weak.reference-2">weak reference<sup><small>(2)</small></sup></a>;
    <a href="p.html#phantom.reference">phantom reference</a>.
<br />
<strong>Other links:</strong> <a href="http://java.sun.com/products/jdk/1.2/docs/guide/refobs/index.html">Java spec for reference objects</a>;
    <a href="http://developer.java.sun.com/developer/technicalArticles/ALT/RefObj/index.html">Reference Objects and Garbage Collection</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#dbe93">R. Kent Dybvig, Carl Bruggeman, David Eby. 1993. <cite>Guardians in a Generation-Based Garbage Collector</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="region.inference" name="region.inference">region inference</a></strong></dt>
<dd><p>Region inference is a technique for determining when <a href="o.html#object">objects</a> become <a href="d.html#dead">dead</a> (even if they are <a href="#reachable">reachable</a>) by a static analysis of the program.</p>
<p>Region inference infers a <em>region</em> for each object.
When a region dies, all the objects in it are known to be <a href="d.html#dead">dead</a>, whether reachable or not.
Regions obey a strict <a href="s.html#stack">stack</a> discipline; that is, when a region dies, all younger regions also die.  In this way, region inference occupies a middle ground between <a href="s.html#stack.allocation">stack allocation</a> and <a href="h.html#heap.allocation">heap allocation</a>.</p>

<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#tt97">Mads Tofte, Jean-Pierre Talpin. 1997. <cite>Region-Based Memory Management</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="register" name="register">register</a></strong></dt>
<dd><p>Definition not yet available.  Please see our <a href="../feedback.html#submissions">feedback page</a> for submission information.</p></dd>

<dt><strong><a id="register.set.partitioning" name="register.set.partitioning">register set partitioning</a></strong></dt>
<dd><p>Run-time systems for <a href="g.html#garbage.collection">garbage-collected</a> languages sometimes partition the set of machine <a href="#register">registers</a> <i>a priori</i> into two categories: those always <a href="t.html#trace">traced</a> and updated by the <a href="g.html#garbage.collector">garbage collector</a> and those ignored by it.</p>
<p>The former are always maintained in a format understood by the collector; the latter are never used to hold <a href="#reference">references</a> to collectable <a href="o.html#object">objects</a>.  More complicated schemes are also possible.</p>

<p>This partitioning provides a separation of concerns between the compiler and the <a href="g.html#garbage.collector">garbage collector</a>.  The compiler can generate code that produces values the garbage collector would not be able to handle (say, because they have no <a href="t.html#tag">tags</a>), as long as those values are kept in the ignored registers.  The garbage collector can trust that the registers it looks at always contain valid data, and can perform <a href="e.html#exact.garbage.collection">exact garbage collection</a>.</p>

<p>Register set partitioning increases the demand for registers (<i>register pressure</i>), but may reduce the amount of <a href="b.html#boxed">boxing</a> needed.</p></dd>


<dt><strong><a id="relocation" name="relocation">relocation</a></strong></dt>
<dd><p><em>Relocating</em> means moving data from one location to another and updating all <a href="#reference">references</a>.</p>
<p>Relocation is often performed to avoid <a href="e.html#external.fragmentation">external fragmentation</a>.</p>

<p>Program loading sometimes relocates code and <a href="s.html#static.allocation">static</a> data.</p>

<p><strong>Similar terms:</strong> <a href="m.html#moving.garbage.collector">moving</a>.
<br />
<strong>See also:</strong> <a href="c.html#compaction">compaction</a>;
    <a href="m.html#moving.memory.manager">moving memory manager</a>.
</p></dd>

<dt><strong><a id="remembered.set" name="remembered.set">remembered set</a></strong></dt>
<dd><p>A remembered set is the technique of keeping a separate list of interesting <a href="#reference">references</a> between two sets of <a href="o.html#object">objects</a>, so you don't have to find them by <a href="s.html#scan">scanning</a>.</p>
<p>Many <a href="m.html#memory.management">memory management</a> algorithms depend on partitioning the objects and require special handling for references between partitions.  Keeping track of such references in a remembered set eliminates the need to scan the originating partition to find them.</p>

<p>A typical use in <a href="g.html#generational.garbage.collection">generational garbage collection</a> is remembering <a href="#reference">references</a> from an older <a href="g.html#generation">generation</a> to a younger one.</p>

<p><strong>Similar terms:</strong> <a href="e.html#entry.table-2">entry table<sup><small>(2)</small></sup></a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#ungar84">Dave Ungar. 1984. <cite>Generation Scavenging: A Non-disruptive High Performance Storage Reclamation Algorithm</cite>.</a></li>
  <li><a href="../bib/f.html#jones96">Richard E. Jones, Rafael Lins. 1996. <cite>Garbage Collection: Algorithms for Automatic Dynamic Memory Management</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="replicating.garbage.collector" name="replicating.garbage.collector">replicating garbage collector</a></strong></dt>
<dd><p>A variant of <a href="c.html#copying.garbage.collection">copying garbage collection</a>, which does not destroy the original <a href="o.html#object">object</a> when making a copy.</p>
<p>This is useful in an <a href="i.html#incremental.garbage.collection">incremental</a> or <a href="p.html#parallel.garbage.collection">concurrent</a> <a href="c.html#collector-1">collector<sup><small>(1)</small></sup></a>, as no <a href="#read.barrier">read-barrier</a> is required; the <a href="m.html#mutator">mutator</a> can continue to use old objects.  The collector uses a <a href="w.html#write.barrier">write-barrier</a> to replicate the writes to the new copies.</p>

<p><strong>See also:</strong> <a href="c.html#copying.garbage.collection">copying garbage collection</a>;
    <a href="b.html#broken.heart">broken heart</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#noph92">Scott Nettles, James O'Toole, David Pierce, Nickolas Haines. 1992. <cite>Replication-Based Incremental Copying Collection</cite>.</a></li>
  <li><a href="../bib/f.html#no93">Scott Nettles, James O'Toole. 1993. <cite>Real-Time Replication Garbage Collection</cite>.</a></li>
  <li><a href="../bib/f.html#no93a">Scott Nettles, James O'Toole. 1993. <cite>Implementing Orthogonal Persistence: A Simple Optimization Using Replicating Collection</cite>.</a></li>
  <li><a href="../bib/f.html#on94">James O'Toole, Scott Nettles. 1994. <cite>Concurrent Replicating Garbage Collection</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="reserved" name="reserved">reserved</a></strong></dt>
<dd><p>In a <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> system, it is usually possible to hold range of <a href="v.html#virtual.address">virtual addresses</a> <em>reserved</em> without making it <a href="m.html#mapped">mapped</a>.</p>
<p>Reserving addresses prevents other components of the program using the same addresses, without consuming <a href="s.html#swap.space">swap space</a>.  This technique is often used in <a href="b.html#bibop">BIBOP</a> schemes, where one might want to reserve a large amount of <a href="a.html#address.space">address space</a> but only sparsely map it.</p>

<p>On some systems there are special calls for reserving; on others one can create <a href="m.html#mapping">mappings</a> that don't need <a href="b.html#backing.store">backing store</a>, e.g., on some Unix&reg; systems, <code><a href="m.html#mmap">mmap</a> /dev/zero</code> with no access.</p>

<p><strong>See also:</strong> <a href="m.html#mapping">mapping</a>.
</p></dd>

<dt><strong><a id="resident" name="resident">resident</a></strong></dt>
<dd><p>In a <a href="c.html#cache-2">cache<sup><small>(2)</small></sup></a> system, that part of the cached storage
        which currently has a copy in the cache is called <i>resident</i>.
        Ideally, the <a href="w.html#working.set">working set</a> should be resident.</p>
<p><strong>See also:</strong> <a href="c.html#cache-2">cache<sup><small>(2)</small></sup></a>;
    <a href="s.html#storage.hierarchy">storage hierarchy</a>;
    <a href="#resident.set">resident set</a>.
</p></dd>

<dt><strong><a id="resident.set" name="resident.set">resident set</a></strong></dt>
<dd><p>In a <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> system, a process' resident set is that part of a process' <a href="a.html#address.space">address space</a> which is currently in <a href="m.html#main.memory">main memory</a>.  If this does not include all of the process' <a href="w.html#working.set">working set</a>, the system may <a href="t.html#thrash">thrash</a>.</p></dd>

<dt><strong><a id="rom" name="rom">ROM</a>, read-only memory</strong></dt>
<dd><p>ROM (read-only memory) is a type of <a href="p.html#physical.memory-2">physical memory<sup><small>(2)</small></sup></a> that can be read from, but not written to.
The contents of ROM are usually set in the factory.</p>
<p><strong>See also:</strong> <a href="#ram">RAM</a>.
</p></dd>

<dt><strong><a id="root" name="root">root</a></strong></dt>
<dd><p>In <a href="t.html#tracing.garbage.collection">tracing garbage collection</a>, a root holds a <a href="#reference">reference</a> or set of references to <a href="o.html#object">objects</a> that are <em>a priori</em> <a href="#reachable">reachable</a>.  The <a href="#root.set">root set</a> is used as the starting point in determining all reachable data.</p>
<p>Roots basically comprise the references in the state of the <a href="m.html#mutator">mutator</a>.  Typical roots are global variables, other <a href="s.html#static.allocation">static</a> data, and the <a href="c.html#control.stack">control stack</a>.</p>

<p><strong>See also:</strong> <a href="w.html#weak.root">weak root</a>;
    <a href="s.html#strong.root">strong root</a>;
    <a href="a.html#ambiguous.root">ambiguous root</a>;
    <a href="e.html#exact.root">exact root</a>.
</p></dd>

<dt><strong><a id="root.set" name="root.set">root set</a></strong></dt>
<dd><p>The <i>root set</i> is the collection of <a href="#root">roots</a> that the <a href="m.html#mutator">mutator</a> declares to the <a href="c.html#collector-2">collector<sup><small>(2)</small></sup></a>.</p>
<p><strong>See also:</strong> <a href="g.html#garbage.collection">garbage collection</a>.
</p></dd>

</dl>
<p align="center"><a href="a.html">A</a>
<a href="b.html">B</a>
<a href="c.html">C</a>
<a href="d.html">D</a>
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
<strong>R</strong>
<a href="s.html">S</a>
<a href="t.html">T</a>
<a href="u.html">U</a>
<a href="v.html">V</a>
<a href="w.html">W</a>
X
Y
<a href="z.html">Z</a> - <a href="help.html">Help</a></p>

</body></html>