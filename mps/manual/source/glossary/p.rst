<?xml version="1.0" encoding="iso-8859-1"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
        "http://www.w3.org/TR/2000/REC-xhtml1-20000126/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
<link rev="MADE" href="mailto:mm-web@ravenbrook.com" />
<link rel="Start" href="../" />
<link rel="Contents" href="../" />
<link rel="Glossary" href="" />
<meta name="DC.Date" content="2000-12-15T20:16:58" />
<meta name="description" content="Glossary of terms related to memory management" />
<meta name="keywords" content="dictionary, glossary, definition, memory management, garbage collection" />
<link rel="index" href="./" />
<link rel="help" href="help.html" />
<link rel="prev" href="o.html" />
<link rel="next" href="q.html" />
<title>The Memory Management Glossary: P</title>
</head>
<body lang="en" bgcolor="#FFFFFF" text="#000000" link="#000099" vlink="#660066" alink="#FF0000">
<h1 align="center">The Memory Management Glossary<br />
<big>P</big></h1>
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
<strong>P</strong>
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
<dt><strong><a id="padding" name="padding">padding</a></strong></dt>
<dd><p>Padding is redundant <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> within the memory <a href="a.html#allocate">allocated</a> to an <a href="o.html#object">object</a>.  It is usually inserted because of <a href="a.html#alignment">alignment</a> restrictions on the fields of the object or on the object itself.</p>
<p>Padding is a form of <a href="i.html#internal.fragmentation">internal fragmentation</a>.</p></dd>


<dt><strong><a id="page" name="page">page</a></strong></dt>
<dd><p>A <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> system usually deals with <a href="m.html#memory-1">memory<sup><small>(1)</small></sup></a> <a href="b.html#block">blocks</a> of fixed size as units for <a href="#paging">paging</a>.  These are known as <i>pages</i>.</p>
<p>Pages are often 4 <a href="k.html#kb">kB</a> or 8 kB in size.  This size is determined by the addressing hardware of the machine.</p></dd>


<dt><strong><a id="page.fault" name="page.fault">page fault</a>, page-fault</strong></dt>
<dd><p>An exception when accessing <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a>, usually resulting in a <a href="#page">page</a> being fetched from disk.</p>
<p>A page fault is an exception occurring during the translation of <a href="v.html#virtual.address">virtual addresses</a> to <a href="#physical.address">physical addresses</a>.  "Page fault" usually means an access to a page that has been <a href="#paged.out">paged out</a> and hence requires fetching from disk, but it is sometimes also used to mean <a href="i.html#invalid.page.fault">invalid page fault</a> or <a href="#protection.fault">protection fault</a>.</p>

<p><strong>See also:</strong> <a href="#paging">paging</a>;
    <a href="#paged.in">paged in</a>;
    <a href="#paged.out">paged out</a>;
    <a href="r.html#read.fault">read fault</a>;
    <a href="w.html#write.fault">write fault</a>.
</p></dd>

<dt><strong><a id="page.marking" name="page.marking">page marking</a>, page-marking</strong></dt>
<dd><p>Page marking is a form of <a href="c.html#card.marking">card-marking</a> where the <a href="c.html#card">card</a> is the same size as a <a href="#page">page</a></p></dd>

<dt><strong><a id="page.protection" name="page.protection">page protection</a></strong>
  (for full details, see <a href="#protection">protection</a>)</dt>
<dd><p>Many operating systems support protection of <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> <a href="#page">pages</a>.  Individual pages may be protected against a combination of read, write or execute accesses by a process. </p></dd>

<dt><strong><a id="page.table" name="page.table">page table</a></strong></dt>
<dd><p>In a <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> system, it is common to map between <a href="v.html#virtual.address">virtual addresses</a> and <a href="#physical.address">physical addresses</a> by means of a data structure called a <i>page table</i>.</p>
<p>The <a href="#page">page</a> number of an address is usually found from the most significant bits of the address; the remaining bits yield the offset of the <a href="l.html#location">location</a> within the page.  The page table is normally indexed by page number and contains information on whether the page is currently in <a href="m.html#main.memory">main memory</a>, and where it is in main memory or on disk.</p>

<p>Conventional page tables are sized to the virtual <a href="a.html#address.space">address space</a> and store the entire virtual address space description of each process.  Because of the need to keep the virtual-to-physical translation time low, a conventional page table is structured as a fixed, multi-level hierarchy, and can be very inefficient at representing a sparse virtual address space, unless the allocated pages are carefully aligned to the page table hierarchy.</p>

<p><strong>See also:</strong> <a href="i.html#inverted.page.table">inverted page table</a>.
</p></dd>

<dt><strong><a id="paged.in" name="paged.in">paged in</a></strong></dt>
<dd><p>In a <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> system, <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> is described as <em>paged in</em> if it is available in <a href="#physical.memory-1">physical memory<sup><small>(1)</small></sup></a>.</p>
<p><strong>Similar terms:</strong> <a href="s.html#swapped.in">swapped in</a>.
<br />
<strong>Opposites:</strong> <a href="#paged.out">paged out</a>.
<br />
<strong>See also:</strong> <a href="#paging">paging</a>.
</p></dd>

<dt><strong><a id="paged.out" name="paged.out">paged out</a></strong></dt>
<dd><p>In a <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> system, <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> is described as <em>paged out</em> if it is not available in <a href="#physical.memory-1">physical memory<sup><small>(1)</small></sup></a>.</p>
<p><strong>Similar terms:</strong> <a href="s.html#swapped.out">swapped out</a>.
<br />
<strong>Opposites:</strong> <a href="#paged.in">paged in</a>.
<br />
<strong>See also:</strong> <a href="#paging">paging</a>.
</p></dd>

<dt><strong><a id="paging" name="paging">paging</a></strong></dt>
<dd><p>In a <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> system, <em>paging</em> is the act of transferring <a href="#page">pages</a> between <a href="#physical.memory-1">physical memory<sup><small>(1)</small></sup></a> and <a href="b.html#backing.store">backing store</a> (usually disk).</p>
<p>When pages need to be paged out, a heuristic is used to select ones that will not be needed soon; "least recently used" is a popular one.</p>

<p><strong>Similar terms:</strong> <a href="s.html#swapping">swapping</a>.
<br />
<strong>See also:</strong> <a href="#paged.in">paged in</a>;
    <a href="#paged.out">paged out</a>.
</p></dd>

<dt><strong><a id="palimpsest" name="palimpsest">palimpsest</a></strong></dt>
<dd><p>A <a href="b.html#block">block</a> of <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> that has been <a href="a.html#allocate">allocated</a>, <a href="f.html#free-1">freed</a> (or <a href="r.html#reclaim">reclaimed</a>), and then allocated again.  Such memory may contain data from the previous use if portions of it remain uninitialised.</p>
<p>This commonly occurs on the <a href="s.html#stack">stack</a>, especially if the compiler allocates large <a href="s.html#stack.frame">stack frames</a> in anticipation of allocating data structures on the stack.</p>

<p>If the palimpsest is being <a href="s.html#scan">scanned</a> <a href="c.html#conservative.garbage.collection">conservatively</a>, such left-over data may cause <a href="u.html#unreachable">unreachable</a> <a href="o.html#object">objects</a> to appear <a href="r.html#reachable">reachable</a> and thus become <a href="f.html#floating.garbage">floating garbage</a>.  If it is scanned <a href="e.html#exact.garbage.collection">precisely</a>, such left-over data, if treated as <a href="#pointer">pointers</a>, is a bug.</p></dd>


<dt><strong><a id="parallel.garbage.collection" name="parallel.garbage.collection">parallel garbage collection</a></strong>
  (also known as concurrent garbage collection)</dt>
<dd><p>A parallel or concurrent <a href="c.html#collector-2">collector<sup><small>(2)</small></sup></a> executes simultaneously with the <a href="m.html#mutator">mutator</a>, usually on a multi-processor machine.</p>
<p>Concurrent <a href="g.html#gc">GC</a> must cope with the mutator changing <a href="o.html#object">objects</a> while collection occurs.  The problem is similar to that of <a href="i.html#incremental.garbage.collection">incremental GC</a>, but harder.  The solution typically involves <a href="b.html#barrier-1">barriers<sup><small>(1)</small></sup></a>.</p>

<p><strong>Similar terms:</strong> <a href="i.html#incremental.garbage.collection">incremental</a>.
<br />
<strong>See also:</strong> <a href="r.html#replicating.garbage.collector">replicating garbage collector</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#doligez93">Damien Doligez, Xavier Leroy. 1993. <cite>A concurrent, generational garbage collector for a multithreaded implementation of ML</cite>.</a></li>
  <li><a href="../bib/f.html#doligez94">Damien Doligez, Georges Gonthier. 1994. <cite>Portable, unobtrusive garbage collection for multiprocessor systems</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="perfect.fit" name="perfect.fit">perfect fit</a></strong></dt>
<dd><p>If an <a href="a.html#allocate">allocation</a> request is satisfied exactly from a <a href="f.html#free.block">free block</a> with no <a href="f.html#fragmentation">fragmentation</a>, this is said to be a <a href="#perfect.fit">perfect fit</a>.  </p>
<p><strong>See also:</strong> <a href="f.html#free.block">free block</a>;
    <a href="a.html#allocation.mechanism">allocation mechanism</a>;
    <a href="b.html#best.fit">best fit</a>.
</p></dd>

<dt><strong><a id="phantom.reachable" name="phantom.reachable">phantom reachable</a>, phantomly reachable</strong></dt>
<dd><p>In <a href="../articles/lang.html#java">Java</a><sup><small class="tm-small">TM</small></sup>, an object is <em>phantom reachable</em> if it is neither <a href="s.html#strongly.reachable">strongly</a> nor <a href="s.html#softly.reachable">softly</a> nor <a href="w.html#weakly.reachable">weakly reachable</a> and has been <a href="f.html#finalization">finalized</a> and there is a path from the <a href="r.html#root">roots</a> to it that contains at least one <a href="#phantom.reference">phantom reference</a>.</p>
<p>When the Java <a href="c.html#collector-1">collector<sup><small>(1)</small></sup></a> determines that an object is phantom reachable, the <a href="r.html#reference.object">reference objects</a> containing the phantom references are enqueued.</p>

<p>The Java specification says that the phantom reference is not cleared when the reference object is enqueued, but actually, there's no way in the language to tell whether that has been done or not.  In some implementations, JNI weak global references are weaker than phantom references, and provide a way to access phantom reachable objects.</p>

<p><strong>See also:</strong> <a href="r.html#reachable">reachability</a>.
<br />
<strong>Other links:</strong> <a href="http://java.sun.com/products/jdk/1.2/docs/api/java/lang/ref/PhantomReference.html">Java spec for class PhantomReference</a>;
    <a href="http://developer.java.sun.com/developer/technicalArticles/ALT/RefObj/index.html">Reference Objects and Garbage Collection</a>.
</p></dd>

<dt><strong><a id="phantom.reference" name="phantom.reference">phantom reference</a></strong></dt>
<dd><p>In <a href="../articles/lang.html#java">Java</a><sup><small class="tm-small">TM</small></sup> terminology, <em>phantom reference</em> is used to mean a <a href="r.html#reference">reference</a> encapsulated in a <a href="r.html#reference.object">reference object</a> of class <code>PhantomReference</code>.</p>
<p>Phantom references form one of three kinds of <a href="w.html#weak.reference-1">weak reference<sup><small>(1)</small></sup></a> in Java.  They are handy for performing clean-ups after an object has <a href="d.html#dead">died</a> and been <a href="f.html#finalization">finalized</a>.</p>

<p><strong>See also:</strong> <a href="#phantom.reachable">phantom reachable</a>.
<br />
<strong>Other links:</strong> <a href="http://java.sun.com/products/jdk/1.2/docs/api/java/lang/ref/PhantomReference.html">Java spec for class PhantomReference</a>;
    <a href="http://developer.java.sun.com/developer/technicalArticles/ALT/RefObj/index.html">Reference Objects and Garbage Collection</a>.
</p></dd>

<dt><strong><a id="physical.address" name="physical.address">physical address</a></strong>
  (also known as absolute address)</dt>
<dd><p>Physical <a href="a.html#address">addresses</a> are used to index into <a href="#physical.memory-1">physical memory<sup><small>(1)</small></sup></a>.  On some systems, they are called <em>absolute addresses</em>.</p>
<p>In a <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> system the application program handles <a href="v.html#virtual.address">virtual addresses</a> and these are translated to physical addresses by the <a href="m.html#mmu">MMU</a>.</p>

<p><strong>Opposites:</strong> <a href="v.html#virtual.address">virtual address</a>.
</p></dd>

<dt><strong><a id="physical.address.space" name="physical.address.space">physical address space</a></strong></dt>
<dd><p>The physical <a href="a.html#address.space">address space</a> is the space of <a href="#physical.address">physical addresses</a>.</p>
<p><strong>Opposites:</strong> <a href="v.html#virtual.address.space">virtual address space</a>.
</p></dd>

<dt><strong><a id="physical.memory-1" name="physical.memory-1">physical memory<sup><small>(1)</small></sup></a></strong>
  (also known as real memory(2))</dt>
<dd><p>Physical memory is <a href="m.html#memory-1">memory<sup><small>(1)</small></sup></a> that is wired to directly to the processor, addressable by <a href="#physical.address">physical address</a>.</p>
<p>This term is basically synonymous to <a href="m.html#main.memory">main memory</a>, but is used in contrast to <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> and  <a href="b.html#backing.store">backing store</a>.</p>

<p>While modern computers usually have lots of <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a>, performance is still closely related to the quantity of physical memory available.  If a system has insufficient physical memory, it may <a href="t.html#thrash">thrash</a>.</p>

<p><strong>Similar terms:</strong> <a href="m.html#main.memory">main memory</a>.
</p></dd>

<dt><strong><a id="physical.memory-2" name="physical.memory-2">physical memory<sup><small>(2)</small></sup></a></strong>
  (also known as physical storage)</dt>
<dd><p>Physical memory is <a href="m.html#memory-1">memory<sup><small>(1)</small></sup></a> on physical storage devices, such as <a href="r.html#ram">RAM</a> or disks.</p>
<p>This term is often contrasted to <a href="v.html#virtual.address.space">virtual address space</a> that might not be mapped to any actual storage.</p>

<p><strong>Similar terms:</strong> <a href="m.html#memory-1">memory<sup><small>(1)</small></sup></a>.
</p></dd>

<dt><strong><a id="physical.storage" name="physical.storage">physical storage</a></strong>
  (for full details, see <a href="#physical.memory-2">physical memory<sup><small>(2)</small></sup></a>)</dt>
<dd><p>Physical memory is <a href="m.html#memory-1">memory<sup><small>(1)</small></sup></a> on physical storage devices, such as <a href="r.html#ram">RAM</a> or disks.</p></dd>

<dt><strong><a id="pig.in.the.python" name="pig.in.the.python">pig in the python</a></strong>
  (also known as pig in the snake)</dt>
<dd><p>In a <a href="g.html#generational.garbage.collection">generational</a> collector, when a large and long-lived <a href="o.html#object">object</a> is
<a href="a.html#allocate">allocated</a> in <a href="n.html#nursery.space">nursery space</a>, collection effort will be wasted as that object
survives and is <a href="#promotion">promoted</a> from <a href="g.html#generation">generation</a> to generation.  This is especially
noticeable in a <a href="c.html#copying.garbage.collection">copying collector</a>, where the large object will be copied
many times.  This difficulty is similar to that of a python which swallows its prey whole
and is somewhat immobilized as it digests it.</p>
<p>Modern collectors permit objects to be allocated directly into appropriate
generations or pools to avoid this problem.  Long-lived objects can be
allocated directly into long-term generations.  Large objects can be
allocated directly into pools with special support for large objects (such
as copying by remapping, incremental copying, or not copying at all).</p>

<p><strong>See also:</strong> <a href="g.html#generational.garbage.collection">generational garbage collection</a>.
</p></dd>

<dt><strong><a id="pig.in.the.snake" name="pig.in.the.snake">pig in the snake</a></strong>
  (for full details, see <a href="#pig.in.the.python">pig in the python</a>)</dt>
<dd><p>In a <a href="g.html#generational.garbage.collection">generational</a> collector, when a large and long-lived <a href="o.html#object">object</a> is
<a href="a.html#allocate">allocated</a> in <a href="n.html#nursery.space">nursery space</a>, collection effort will be wasted as that object
survives and is <a href="#promotion">promoted</a> from <a href="g.html#generation">generation</a> to generation.  This is especially
noticeable in a <a href="c.html#copying.garbage.collection">copying collector</a>, where the large object will be copied
many times.  This difficulty is similar to that of a python which swallows its prey whole
and is somewhat immobilized as it digests it.</p></dd>

<dt><strong><a id="placement.policy" name="placement.policy">placement policy</a></strong>
  (for full details, see <a href="a.html#allocation.policy">allocation policy</a>)</dt>
<dd><p>The concrete policy used by an <a href="a.html#allocator">allocator</a> for choosing a <a href="f.html#free.block">free block</a> to satisfy an <a href="a.html#allocate">allocation</a> request.</p></dd>

<dt><strong><a id="pointer" name="pointer">pointer</a></strong></dt>
<dd><p><em>Pointer</em> data types represent a reference to an <a href="o.html#object">object</a> or a <a href="l.html#location">location</a>.</p>
<p>Pointers may be specialized by the type of the object referred to.</p>

<p>Typically, pointers are represented by an <a href="a.html#address">address</a>, but they can be more complicated when they need to carry more information, e.g., when the referent is smaller than a <a href="w.html#word">word</a>, an offset within the word might be needed.</p>

<p><strong>Similar terms:</strong> <a href="r.html#reference">reference</a>;
    <a href="a.html#address">address</a>.
<br />
<strong>See also:</strong> <a href="t.html#tag">tag</a>.
</p></dd>

<dt><strong><a id="precise.garbage.collection" name="precise.garbage.collection">precise garbage collection</a></strong>
  (for full details, see <a href="e.html#exact.garbage.collection">exact garbage collection</a>)</dt>
<dd><p><a href="g.html#garbage.collection">Garbage collection</a> is exact (or precise) if it deals only with <a href="e.html#exact.reference">exact references</a>.</p></dd>

<dt><strong><a id="precise.reference" name="precise.reference">precise reference</a></strong>
  (for full details, see <a href="e.html#exact.reference">exact reference</a>)</dt>
<dd><p>An exact  or precise or sure reference is a value the <a href="c.html#collector-1">collector<sup><small>(1)</small></sup></a> knows is a <a href="r.html#reference">reference</a>.</p></dd>

<dt><strong><a id="precise.root" name="precise.root">precise root</a></strong>
  (for full details, see <a href="e.html#exact.root">exact root</a>)</dt>
<dd><p>An exact or precise root is a <a href="r.html#root">root</a> that contains only <a href="e.html#exact.reference">exact references</a>.</p></dd>

<dt><strong><a id="premature.free" name="premature.free">premature free</a></strong>
  (also known as use after free)</dt>
<dd><p>A <i>premature free</i> or <i>use after free</i> occurs when <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> is <a href="f.html#free-1">deallocated</a>, but is later accessed.</p>
<p>
Under <a href="m.html#manual.memory.management">manual memory management</a>, this usually occurs when one part of a program decides it has finished using a memory <a href="b.html#block">block</a>, and is unaware that another part of the program is still using it.   This is rare under <a href="a.html#automatic.memory.management">automatic memory management</a>.</p>

<p><strong>See also:</strong> <a href="d.html#double.free">double free</a>.
</p></dd>

<dt><strong><a id="premature.promotion" name="premature.promotion">premature promotion</a></strong>
  (for full details, see <a href="#premature.tenuring">premature tenuring</a>)</dt>
<dd><p>When a short-lived <a href="o.html#object">object</a> <a href="a.html#allocate">allocated</a> in a <a href="g.html#generational.garbage.collection">generational garbage collector</a> is <a href="#promotion">promoted</a> (due to poor timing) into a less-frequently collected <a href="g.html#generation">generation</a>.  This <em>prematurely tenured</em> object may become <a href="g.html#garbage">garbage</a> very soon after promotion, but will not be <a href="r.html#reclaim">reclaimed</a> for some time because it is now in a less frequently collected generation.</p></dd>

<dt><strong><a id="premature.tenuring" name="premature.tenuring">premature tenuring</a></strong>
  (also known as premature promotion)</dt>
<dd><p>When a short-lived <a href="o.html#object">object</a> <a href="a.html#allocate">allocated</a> in a <a href="g.html#generational.garbage.collection">generational garbage collector</a> is <a href="#promotion">promoted</a> (due to poor timing) into a less-frequently collected <a href="g.html#generation">generation</a>.  This <em>prematurely tenured</em> object may become <a href="g.html#garbage">garbage</a> very soon after promotion, but will not be <a href="r.html#reclaim">reclaimed</a> for some time because it is now in a less frequently collected generation.</p>
<p>This problem is essentially due to quantization error -- all objects in a generation are treated as if they have the same age, even though they range from as old as the previous promotion cycle to new-born.</p>

<p>Modern <a href="g.html#garbage.collector">collectors<sup><small>(1)</small></sup></a> offer several remedies for premature tenuring:  If the client program knows that it is entering a phase that will create many short-lived objects, it can forestall all promotion until it knows it is done with those objects -- thus no objects will be prematurely promoted, they will all be seen as garbage.  Another solution is to create <a href="b.html#bucket">buckets</a> within generations to more accurately classify objects by age and only promote those which have reached a certain minimum.</p></dd>


<dt><strong><a id="primary.storage" name="primary.storage">primary storage</a></strong>
  (for full details, see <a href="m.html#main.memory">main memory</a>)</dt>
<dd><p>The <em>main memory</em> (or <em>primary storage</em>) of a computer is <a href="m.html#memory-1">memory<sup><small>(1)</small></sup></a> that is wired directly to the processor, consisting of <a href="r.html#ram">RAM</a> and possibly <a href="r.html#rom">ROM</a>.</p></dd>

<dt><strong><a id="promotion" name="promotion">promotion</a></strong>
  (also known as tenuring)</dt>
<dd><p>Promotion or tenuring is the act of moving an <a href="o.html#object">object</a> from its current <a href="g.html#generation">generation</a> to an <em>older</em> one (one that contains objects that are expected to survive longer).</p>
<p>"Tenuring" is used particularly about promotion to the oldest generation.</p>

<p><strong>See also:</strong> <a href="g.html#generational.garbage.collection">generational garbage collection</a>.
</p></dd>

<dt><strong><a id="protection" name="protection">protection</a></strong>
  (also known as memory protection, page protection)</dt>
<dd><p>Many operating systems support protection of <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> <a href="#page">pages</a>.  Individual pages may be protected against a combination of read, write or execute accesses by a process. </p>
<p>A process which attempts a protected access will trigger a <a href="#protection.fault">protection fault</a>.  Protection is typically implemented in hardware by the <a href="m.html#mmu">MMU</a> as part of the support for <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> .</p>

<p>Pages can be protected for a number of reasons: a <a href="g.html#generational.garbage.collection">generational</a> or <a href="i.html#incremental.garbage.collection">incremental</a> <a href="g.html#garbage.collector">garbage collector</a> may want to place <a href="b.html#barrier-1">barriers<sup><small>(1)</small></sup></a> on pages; an operating system may want to protect pages for security, or to implement "copy-on-write" or "demand-zero-filled" pages.</p>

<p><strong>See also:</strong> <a href="r.html#read.fault">read fault</a>;
    <a href="w.html#write.fault">write fault</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#ael88">Andrew Appel, John R. Ellis, Kai Li. 1988. <cite>Real-time Concurrent Collection on Stock Multiprocessors</cite>.</a></li>
  <li><a href="../bib/f.html#singhal92">Vivek Singhal, Sheetal V. Kakkad, Paul R. Wilson. 1992. <cite>Texas: An Efficient, Portable Persistent Store</cite>.</a></li>
  <li><a href="../bib/f.html#hm93">Antony L. Hosking, J. Eliot B. Moss. 1993. <cite>Protection traps and alternatives for memory management of an object-oriented language</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="protection.exception" name="protection.exception">protection exception</a></strong>
  (for full details, see <a href="#protection.fault">protection fault</a>)</dt>
<dd><p>A protection fault is an exception or trap which occurs when a process attempts to access <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> which has been <a href="#protection">protected</a>.</p></dd>

<dt><strong><a id="protection.fault" name="protection.fault">protection fault</a></strong>
  (also known as protection exception, protection violation)</dt>
<dd><p>A protection fault is an exception or trap which occurs when a process attempts to access <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> which has been <a href="#protection">protected</a>.</p>
<p><strong>Relevance to memory management:</strong> Some <a href="g.html#garbage.collector">garbage collectors</a> use handlers for protection faults to provide <a href="b.html#barrier-1">barriers<sup><small>(1)</small></sup></a>.</p>

<p><strong>See also:</strong> <a href="s.html#segmentation.violation">segmentation violation</a>;
    <a href="g.html#general.protection.fault">General Protection Fault</a>.
</p></dd>

<dt><strong><a id="protection.violation" name="protection.violation">protection violation</a></strong>
  (for full details, see <a href="#protection.fault">protection fault</a>)</dt>
<dd><p>A protection fault is an exception or trap which occurs when a process attempts to access <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> which has been <a href="#protection">protected</a>.</p></dd>

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
<strong>P</strong>
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