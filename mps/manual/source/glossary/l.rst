<?xml version="1.0" encoding="iso-8859-1"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
        "http://www.w3.org/TR/2000/REC-xhtml1-20000126/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
<link rev="MADE" href="mailto:mm-web@ravenbrook.com" />
<link rel="Start" href="../" />
<link rel="Contents" href="../" />
<link rel="Glossary" href="" />
<meta name="DC.Date" content="2000-12-15T20:16:57" />
<meta name="description" content="Glossary of terms related to memory management" />
<meta name="keywords" content="dictionary, glossary, definition, memory management, garbage collection" />
<link rel="index" href="./" />
<link rel="help" href="help.html" />
<link rel="prev" href="k.html" />
<link rel="next" href="m.html" />
<title>The Memory Management Glossary: L</title>
</head>
<body lang="en" bgcolor="#FFFFFF" text="#000000" link="#000099" vlink="#660066" alink="#FF0000">
<h1 align="center">The Memory Management Glossary<br />
<big>L</big></h1>
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
<strong>L</strong>
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
<dt><strong><a id="large.object.area" name="large.object.area">large object area</a></strong></dt>
<dd><p>An <a href="a.html#allocation.mechanism">allocation mechanism</a> designed to optimize the management of large <a href="o.html#object">objects</a> by separating them from small ones.</p>
<p>Large objects, typically objects one or more orders of magnitude larger than the <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> <a href="p.html#page">page</a> of a platform, can be costly to <a href="a.html#allocate">allocate</a>, initialize, and <a href="r.html#recycle">recycle</a>.  By segregating those objects into a separate area, they can be managed using specific mechanisms that would be inefficient for smaller objects but which can reduce the cost of manipulating large ones.</p>

<p>Some example mechanisms:</p>
<ul>
<li>In a <a href="c.html#copying.garbage.collection">copying collector</a> large objects can be managed separately using a <a href="m.html#mark-sweep">mark-and-sweep collector</a> to avoid copying costs (see <a href="../bib/f.html#ungar88"><cite>Tenuring Policies for Generation-Based Storage Reclamation</cite></a>).</li>
<li>By aligning large objects on page boundaries, they can be <a href="c.html#compaction">compacted</a> or copied by adjusting their <a href="m.html#mapping">mapping</a> in <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> (see <a href="../bib/f.html#withington91"><cite>How Real is "Real-Time" Garbage Collection?</cite></a>).</li>
<li>Large object may be split into a header and a body, where the header is fixed size and the bulk of the object is in the body (see <a href="../bib/f.html#ungar88"><cite>Tenuring Policies for Generation-Based Storage Reclamation</cite></a>).</li>
<li>By using a page-based <a href="r.html#read.barrier">read-barrier</a>, large objects can be initialized incrementally: For example, each page of the large object is initialized to zero when it is first read, rather than all at once at creation time.</li>
<li>In a copying collector, large objects can be copied incrementally using a similar technique (the new copy is initialized by the old copy) (see <a href="../bib/f.html#baker78"><cite>List Processing in Real Time on a Serial Computer</cite></a>).</li>
<li>Often large objects are <a href="#leaf.object">leaf objects</a> so do not need to be <a href="s.html#scan">scanned</a>, or are know to have a fixed <a href="f.html#format">format</a> with only a few <a href="r.html#reference">references</a> so they can be scanned more efficiently by a specialized scanner.</li>
<li>Often large objects have longer than average <a href="#lifetime">lifetimes</a> so are not allocated in a <a href="n.html#nursery.space">nursery space</a> of a <a href="g.html#generational.garbage.collection">generational garbage collector</a>.</li>
</ul></dd>


<dt><strong><a id="leaf.object" name="leaf.object">leaf object</a></strong>
  (also known as atomic object)</dt>
<dd><p>A leaf object is an <a href="o.html#object">object</a> that does not <a href="r.html#reference">reference</a> any other objects.</p>
<p>In a typed language, the compiler can often determine at compile time that certain types can be represented as leaf objects.  Usually these types are either a <a href="s.html#scalar.data.type">scalar data type</a> or a <a href="v.html#vector.data.type">vector data type</a> of scalars with bounded magnitude.</p>

<p><strong>Relevance to memory management:</strong> If leaf objects can be identified, a <a href="g.html#garbage.collector">garbage collector</a> can make certain optimizations:  leaf objects do not have to be <a href="s.html#scan">scanned</a> for references nor are <a href="b.html#barrier-1">barriers<sup><small>(1)</small></sup></a> needed to detect and maintain references in the object.</p></dd>


<dt><strong><a id="leak" name="leak">leak</a></strong>
  (for full details, see <a href="m.html#memory.leak">memory leak</a>)</dt>
<dd><p>A memory leak is where <a href="a.html#allocate">allocated</a> <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> is not <a href="f.html#free-1">freed</a> although it is never used again.</p></dd>

<dt><strong><a id="life" name="life">life</a></strong>
  (for full details, see <a href="#lifetime">lifetime</a>)</dt>
<dd><p>The lifetime or extent of an <a href="o.html#object">object</a> is the time for which the object is <a href="#live">live</a>.</p></dd>

<dt><strong><a id="lifetime" name="lifetime">lifetime</a></strong>
  (also known as extent, life)</dt>
<dd><p>The lifetime or extent of an <a href="o.html#object">object</a> is the time for which the object is <a href="#live">live</a>.</p>
<p><strong>See also:</strong> <a href="d.html#dynamic.extent">dynamic extent</a>;
    <a href="i.html#indefinite.extent">indefinite extent</a>.
</p></dd>

<dt><strong><a id="lifo-ordered.first.fit" name="lifo-ordered.first.fit">LIFO-ordered first fit</a></strong></dt>
<dd><p>The <a href="a.html#allocation.policy">allocation policy</a> that always uses the most-recently <a href="f.html#free-1">freed</a>
suitable <a href="f.html#free.block">free block</a>. Commonly implemented by pushing freed blocks on
the front of a <a href="f.html#free.block.chain">free block chain</a>, and then using <a href="f.html#first.fit">first fit</a>
allocation on this chain. <a href="f.html#free-1">free<sup><small>(1)</small></sup></a> can be very quick, depending on the
<a href="c.html#coalesce">coalescing</a> policy.</p>
<p>This policy may suffer from severe <a href="f.html#fragmentation">fragmentation</a> in the presence of
short-lived large objects of a single size. As smaller objects are
allocated, the free block chain fills up with fragments a little
smaller than the large object size.</p>

<p><strong>See also:</strong> <a href="f.html#first.fit">first fit</a>;
    <a href="f.html#fifo-ordered.first.fit">FIFO-ordered first fit</a>;
    <a href="a.html#address-ordered.first.fit">address-ordered first fit</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil95">Paul R. Wilson, Mark S. Johnstone, Michael Neely, David Boles. 1995. <cite>Dynamic Storage Allocation: A Survey and Critical Review</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="limited-field.reference.count" name="limited-field.reference.count">limited-field reference count</a></strong>
  (also known as sticky reference count)</dt>
<dd><p>A <a href="r.html#reference.counting">reference counting</a> technique whereby the field used to store the number of <a href="r.html#reference">references</a> to an <a href="o.html#object">object</a> has a limited size.  In particular, the field is not large enough to represent the maximum possible number of references to an object.</p>
<p>Using the observation that most objects are not referenced a great number of times, some systems that use reference counts only store the count accurately up to a certain maximum value.  If an object has more references than the maximum then the count "sticks" at the maximum and is never decremented.  Such objects are expected to be rare, but their <a href="s.html#storage">storage</a> can never be <a href="r.html#reclaim">reclaimed</a> using reference counting.  A separate (infrequently run) <a href="t.html#tracing.garbage.collection">tracing garbage collector</a> is often employed to reclaim this storage.</p>

<p>A degenerate form of limited-field reference counting is <a href="o.html#one-bit.reference.count">one-bit reference counting</a> where an object is considered to be referenced either exactly once or many times.</p></dd>


<dt><strong><a id="linear.addressing" name="linear.addressing">linear addressing</a></strong></dt>
<dd><p>In linear addressing, <a href="a.html#address">addresses</a> form a single, continuous <a href="a.html#address.space">address space</a>.  This term is used mostly in opposition to <a href="s.html#segmented.addressing">segmented addressing</a>.</p>
<p><strong>Opposites:</strong> <a href="s.html#segmented.addressing">segmented addressing</a>.
</p></dd>

<dt><strong><a id="live" name="live">live</a></strong>
  (also known as alive, active)</dt>
<dd><p><a href="m.html#memory-2">Memory<sup><small>(2)</small></sup></a> or an <a href="o.html#object">object</a> is live if the program will read from it in future.  The term is often used more broadly to mean <a href="r.html#reachable">reachable</a>.</p>
<p>It is not possible, in general, for <a href="g.html#garbage.collector">garbage collectors</a> to determine exactly which <a href="o.html#object">objects</a> are still live.  Instead, they use some approximation to detect objects that are provably <a href="d.html#dead">dead</a>, such as those that are not <a href="r.html#reachable">reachable</a>.</p>

<p><strong>Similar terms:</strong> <a href="r.html#reachable">reachable</a>.
<br />
<strong>Opposites:</strong> <a href="d.html#dead">dead</a>.
<br />
<strong>See also:</strong> <a href="u.html#undead">undead</a>.
</p></dd>

<dt><strong><a id="load" name="load">load</a></strong></dt>
<dd><p>To transfer data from <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> to a processor's <a href="r.html#register">registers</a>.</p>
<p>Load can also be used in the more general sense of moving data from a part of the <a href="m.html#memory.hierarchy">memory hierarchy</a> that is slow to access to one that is fast to access (For example, "it takes about 3 ms for the <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> system to load a <a href="p.html#page">page</a> from disk on this system").  When used in this sense, the qualified term <a href="c.html#cache-2">cache<sup><small>(2)</small></sup></a> load is common.</p>

<p><code>LOAD</code> (or an abbreviation) is also commonly used in many processor architectures as the mnemonic name for the machine code instructions that are used primarily to make data accessible to the CPU (by loading the data into registers usually).  In RISC architectures it is common for the load instructions to be the only means of making data accessible to the CPU; in CISC architectures it is common for a wide variety of instructions to implicitly or explicitly load data from memory.</p>

<p><strong>Opposites:</strong> <a href="s.html#store-1">store<sup><small>(1)</small></sup></a>.
</p></dd>

<dt><strong><a id="locality.of.reference" name="locality.of.reference">locality of reference</a></strong></dt>
<dd><p>Locality of reference is the extent to which successive accesses of nearby <a href="m.html#memory-1">memory<sup><small>(1)</small></sup></a> <a href="m.html#memory.location">locations</a> are nearby in time; for example, a program that reads all the elements of a contiguous array in turn or that repeatedly uses the same memory variable has good locality of reference.</p>
<p>Good locality of reference interacts well with <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> and <a href="c.html#cache-1">memory caches</a>, as it reduces the <a href="w.html#working.set">working set</a> and improves the <a href="h.html#hit.rate">hit rate</a>.</p>

<p>There are a number of specialized senses of locality of reference in certain fields such as distributed systems; these are not covered in depth here.</p>

<p><strong>Relevance to memory management:</strong> A <a href="m.html#mutator">mutator</a> may exhibit predictable properties such as accessing in turn <a href="o.html#object">objects</a> which were <a href="a.html#allocate">allocated</a> in turn, or accessing in turn objects which have <a href="r.html#reference">references</a> to each other.  An intelligent <a href="a.html#allocator">allocator</a> or <a href="c.html#copying.garbage.collection">copying garbage collector</a> can use this observation to improve locality of reference.</p>

<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#gzh93">Dirk Grunwald, Benjamin Zorn, R. Henderson. 1993. <cite>Improving the Cache Locality of Memory Allocation</cite>.</a></li>
  <li><a href="../bib/f.html#wlm92">Paul R. Wilson, Michael S. Lam, Thomas G. Moher. 1992. <cite>Caching Considerations for Generational Garbage Collection</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="location" name="location">location</a></strong>
  (for full details, see <a href="m.html#memory.location">memory location</a>)</dt>
<dd><p>Each separately-<a href="a.html#address">addressable</a> unit of <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> in which data can be stored is called a <em>memory location</em>.  Usually, these hold a <a href="b.html#byte-2">byte<sup><small>(2)</small></sup></a>, but the term can refer to <a href="w.html#word">words</a>.</p></dd>

<dt><strong><a id="logical.address" name="logical.address">logical address</a></strong>
  (for full details, see <a href="v.html#virtual.address">virtual address</a>)</dt>
<dd><p>In a <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> system, the <a href="a.html#address">addresses</a> that application programs deal with are known as <em>virtual addresses</em>.</p></dd>

<dt><strong><a id="longword" name="longword">longword</a></strong>
  (for full details, see <a href="d.html#doubleword">doubleword</a>)</dt>
<dd><p>A <em>doubleword</em> is a unit of memory consisting of two adjacent <a href="w.html#word">words</a>.  In digital's Alpha architecture, it's called <em>a longword</em>.</p></dd>

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
<strong>L</strong>
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