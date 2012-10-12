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
<link rel="prev" href="r.html" />
<link rel="next" href="t.html" />
<title>The Memory Management Glossary: S</title>
</head>
<body lang="en" bgcolor="#FFFFFF" text="#000000" link="#000099" vlink="#660066" alink="#FF0000">
<h1 align="center">The Memory Management Glossary<br />
<big>S</big></h1>
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
<a href="r.html">R</a>
<strong>S</strong>
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
<dt><strong><a id="sbrk" name="sbrk">sbrk</a></strong></dt>
<dd><p><code>sbrk</code> is a UNIX&reg; library function that adjusts the limit of the data segment;
this limit is known as the <em>break</em>.</p>
<p><code>sbrk</code> returns the previous value of the break, so <code>sbrk(0)</code> is a
common idiom for getting the current value.</p>

<p>Note that, if you use <code><a href="b.html#brk">brk</a></code>, you probably can't safely use <code>sbrk</code> as well, because it may store the last value of the break in a private variable.</p></dd>


<dt><strong><a id="scalar.data.type" name="scalar.data.type">scalar data type</a></strong></dt>
<dd><p>A scalar data type is a type that is representable in a single dimension and whose objects have only magnitude as value.</p>
<p>Examples of scalar data types include: integers, floating-point numbers, enumerations, and characters.</p>

<p><strong>Relevance to memory management:</strong> The objects of a scalar data type are <a href="l.html#leaf.object">leaf objects</a>.  Scalar data types with bounded magnitude can be represented compactly using <a href="v.html#value.object">value objects</a>.</p>

<p><strong>Historical note:</strong> Because compact representation solves many memory management issues, many older programming languages only offered bounded scalar data types.  For example, the <code>int</code> type in <a href="../articles/lang.html#c">C</a>  is defined to have a magnitude that can be represented by a <a href="w.html#word">word</a>.</p>

<p><strong>See also:</strong> <a href="v.html#vector.data.type">vector data type</a>;
    <a href="a.html#algebraic.data.type">algebraic data type</a>;
    <a href="v.html#value.object">value object</a>;
    <a href="l.html#leaf.object">leaf object</a>.
</p></dd>

<dt><strong><a id="scan" name="scan">scan</a></strong></dt>
<dd><p>The examination of an <a href="o.html#object">object</a> or an area of <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> to find <a href="r.html#reference">references</a>, typically as part of <a href="t.html#trace">tracing</a>.</p>
<p>Scanning examines memory that has been decided to be non-<a href="g.html#garbage">garbage</a>, to find references to objects that have been <a href="t.html#threatened.set">condemned</a>.</p></dd>


<dt><strong><a id="scavenging.garbage.collection" name="scavenging.garbage.collection">scavenging garbage collection</a></strong>
  (for full details, see <a href="c.html#copying.garbage.collection">copying garbage collection</a>)</dt>
<dd><p>Copying garbage collection is a kind of <a href="t.html#tracing.garbage.collection">tracing garbage collection</a> that operates by <a href="r.html#relocation">relocating</a> <a href="r.html#reachable">reachable</a> <a href="o.html#object">objects</a> (this is sometimes called <em>scavenging</em>) and then <a href="r.html#reclaim">reclaiming</a> objects that are left behind, which must be <a href="u.html#unreachable">unreachable</a> and therefore <a href="d.html#dead">dead</a>.</p></dd>

<dt><strong><a id="sdram" name="sdram">SDRAM</a></strong></dt>
<dd><p>Synchronous Dynamic Random Access Memory.  A high performance variant of <a href="d.html#dram">DRAM</a>.</p>
<p>SDRAM uses an external clock signal to synchronize its data input and output.  It is capable of achieving very high data rates for linear access to memory.</p></dd>


<dt><strong><a id="segmentation.violation" name="segmentation.violation">segmentation violation</a></strong></dt>
<dd><p>A segmentation violation occurs when an attempt is made to access <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> whose <a href="a.html#address">address</a> is well-formed, but to which access cannot be granted.  This might be due to either a <a href="p.html#protection.fault">protection fault</a> or an <a href="i.html#invalid.page.fault">invalid page fault</a>.</p>
<p>The term is sometimes used more loosely as a synonym for any memory access error, including a <a href="b.html#bus.error">bus error</a>.</p>

<p><strong>Similar terms:</strong> <a href="g.html#general.protection.fault">general protection fault</a>;
    <a href="r.html#read.fault">read fault</a>;
    <a href="w.html#write.fault">write fault</a>.
</p></dd>

<dt><strong><a id="segmented.addressing" name="segmented.addressing">segmented addressing</a></strong></dt>
<dd><p>In segmented addressing, <a href="a.html#address">addresses</a> are in two parts: a segment identifier and an offset into that segment.</p>
<p>Each segment has a base address and a limit.  If the offset is greater than the limit, the address is invalid (see <a href="#segmentation.violation">segmentation violation</a>).  Otherwise, the offset is added to the segment's base address, giving the unsegmented address.  Segment identifiers may be implicit; for instance, they may be obtained from a <em>current segment</em> register.</p>

<p>Segmentation may be layered on top of <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a>, in which case the unsegmented address is a <a href="v.html#virtual.address">virtual address</a>, or not, in which case it is a <a href="p.html#physical.address">physical address</a>.</p>

<p>Note that, in segmented architectures, you can have a two-dimensional <a href="a.html#address.space">address space</a>.</p>

<p>Segments are a feature of some processor architectures and operating systems.  This description does not cover all possible variations on segmentation.</p>

<p><strong>Historical note:</strong> Segment terminology may be used on unsegmented systems for historical reasons.  For instance, UNIX processes have <em>text segments</em>, even when running on an unsegmented system.</p>

<p><strong>Opposites:</strong> <a href="l.html#linear.addressing">linear addressing</a>.
</p></dd>

<dt><strong><a id="segregated.fit" name="segregated.fit">segregated fit</a></strong></dt>
<dd><p>One of the <a href="#segregated.free.list">segregated free list</a> class of <a href="a.html#allocation.mechanism">allocation mechanisms</a>. There is an array of <a href="f.html#free.list">free lists</a>, each holding <a href="f.html#free.block">free blocks</a> of a particular range of sizes. The <a href="a.html#allocator">allocator</a> identifies the appropriate free list and allocates from it (often using a <a href="#sequential.fit">sequential fit</a> mechanism such as <a href="f.html#first.fit">first fit</a>). If this fails, a larger block is taken from another list and split. </p>
<p>The details of the mechanism depend on the division of sizes between free lists. See <a href="e.html#exact.segregated.fit">exact segregated fit</a> and <a href="#strict.segregated.fit">strict segregated fit</a>.</p>

<p>This implements a <a href="g.html#good.fit">good fit</a> <a href="a.html#allocation.policy">allocation policy</a>.</p>

<p><strong>See also:</strong> <a href="#segregated.free.list">segregated free list</a>;
    <a href="a.html#allocation.mechanism">allocation mechanism</a>;
    <a href="f.html#free.list">free list</a>;
    <a href="e.html#exact.segregated.fit">exact segregated fit</a>;
    <a href="#strict.segregated.fit">strict segregated fit</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil95">Paul R. Wilson, Mark S. Johnstone, Michael Neely, David Boles. 1995. <cite>Dynamic Storage Allocation: A Survey and Critical Review</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="segregated.free.list" name="segregated.free.list">segregated free list</a>, segregated free-list</strong></dt>
<dd><p>A class of <a href="a.html#allocation.mechanism">allocation mechanism</a> which divides the <a href="f.html#free.list">free list</a> into several subsets, according to the size of the <a href="f.html#free.block">free blocks</a>.  A <a href="f.html#free-1">freed</a> or <a href="c.html#coalesce">coalesced</a> block is placed on the appropriate list.  An allocation request is serviced from the appropriate list.</p>
<p>This class of mechanism implements a <a href="g.html#good.fit">good fit</a> or <a href="b.html#best.fit">best fit</a> policy.</p>

<p>Variations within this class include <a href="#simple.segregated.storage">simple segregated storage</a>, <a href="#segregated.fit">segregated fit</a>, and <a href="b.html#buddy.system">buddy systems</a>.</p>

<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil95">Paul R. Wilson, Mark S. Johnstone, Michael Neely, David Boles. 1995. <cite>Dynamic Storage Allocation: A Survey and Critical Review</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="semi-conservative.garbage.collection" name="semi-conservative.garbage.collection">semi-conservative garbage collection</a></strong>
  (also known as mostly-precise garbage collection, mostly-exact garbage collection)</dt>
<dd><p>A variant of <a href="c.html#conservative.garbage.collection">conservative garbage collection</a> which deals with <a href="e.html#exact.reference">exact references</a> as well as <a href="a.html#ambiguous.reference">ambiguous references</a>.</p>
<p>For example, references from the <a href="r.html#root.set">root set</a> might be ambiguous, but <a href="o.html#object">objects</a> on the <a href="h.html#heap">heap</a> might be fully described and precisely <a href="#scan">scanned</a>.</p>

<p><strong>See also:</strong> <a href="m.html#mostly-copying.garbage.collection">mostly-copying garbage collection</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#bartlett88">Joel F. Bartlett. 1988. <cite>Compacting Garbage Collection with Ambiguous Roots</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="semi-space" name="semi-space">semi-space</a></strong></dt>
<dd><p>When an area of <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> is divided into two parts for the purposes of <a href="c.html#copying.garbage.collection">copying garbage collection</a>, the  parts are known as <i>semi-spaces</i>, or sometimes just <i>spaces</i>.</p>
<p>Each semi-space is a contiguous area of memory.
Semi-spaces are usually used for <a href="t.html#two-space.collector">two space collection</a>, but can be used for <a href="g.html#generational.garbage.collection">generational collection</a>.</p>

<p>The semi-space where <a href="o.html#object">objects</a> reside at the start of the collection is known as the <i>old semi-space</i>; the <i>new semi-space</i> is where objects will reside, and where new objects will be <a href="a.html#allocate">allocated</a>, when the collection is complete.</p>

<p><strong>See also:</strong> <a href="t.html#two-space.collector">two space collector</a>.
</p></dd>

<dt><strong><a id="semi-space.collector" name="semi-space.collector">semi-space collector</a></strong>
  (for full details, see <a href="t.html#two-space.collector">two-space collector</a>)</dt>
<dd><p>A two-space <a href="c.html#collector-1">collector<sup><small>(1)</small></sup></a> is a simple form of a <a href="c.html#copying.garbage.collection">copying garbage collector</a>.  The available <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> is divided into two halves, called <a href="#semi-space">semi-spaces</a>.  <a href="o.html#object">Objects</a> are allocated in one semi-space until it is full.  The <a href="r.html#reachable">reachable</a> objects are then copied into the other semi-space (usually using a <a href="c.html#cheney.scan">Cheney scan</a>) and the old semi-space is <a href="r.html#reclaim">reclaimed</a>.  <a href="a.html#allocate">Allocation</a> continues in the new semi-space until it is full, at which point the process is repeated in reverse.</p></dd>

<dt><strong><a id="sequential.fit" name="sequential.fit">sequential fit</a></strong></dt>
<dd><p>A class of <a href="a.html#allocation.mechanism">allocation mechanisms</a> that maintain the <a href="f.html#free.list">free list</a> as a single linear list of <a href="f.html#free.block">free blocks</a> (a <a href="f.html#free.block.chain">free block chain</a>).  Sequential fit mechanisms include <a href="f.html#first.fit">first fit</a> and <a href="n.html#next.fit">next fit</a>.</p>
<p>To quote <a href="../bib/f.html#wil95"><cite>Dynamic Storage Allocation: A Survey and Critical Review</cite></a>:</p>

<blockquote>The list is often doubly-linked and/or circularly linked.
Typically, sequential fit algorithms use Knuth's boundary tag
technique, and a doubly-linked list to make <a href="c.html#coalesce">coalescing</a> simple and
fast.  ... In considering sequential fits, it is probably most
important to keep strategy and policy issues in mind.  The classic
linear-list implementations may not scale well to large <a href="h.html#heap">heaps</a>, in
terms of time costs; as the number of free blocks grows the time to
search the list may become unacceptable.  More efficient and scalable
techniques are available, using totally or partially ordered trees, or
<a href="#segregated.fit">segregated fits</a>.</blockquote>

<p><strong>See also:</strong> <a href="b.html#bitmapped.fit">bitmapped fit</a>;
    <a href="i.html#indexed.fit">indexed fit</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil95">Paul R. Wilson, Mark S. Johnstone, Michael Neely, David Boles. 1995. <cite>Dynamic Storage Allocation: A Survey and Critical Review</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="sequential.store.buffer" name="sequential.store.buffer">sequential store buffer</a></strong>
  (also known as SSB)</dt>
<dd><p>A sequential store buffer is a technique for dividing the cost of a <a href="w.html#write.barrier">write-barrier</a> by remembering which <a href="o.html#object">objects</a> are modified and updating <a href="r.html#remembered.set">remembered sets</a> (and so on) at a later stage.</p>
<p>This turns out to be extremely efficient on pipelined architectures with branch prediction.</p></dd>


<dt><strong><a id="shared.memory" name="shared.memory">shared memory</a></strong></dt>
<dd><p><a href="m.html#memory.location">Memory locations</a> are <em>shared</em> if they are in the range of multiple <a href="a.html#address.space">address spaces</a>.</p></dd>

<dt><strong><a id="simple.object" name="simple.object">simple object</a></strong></dt>
<dd><p>In the <a href="../articles/lang.html#postscript">PostScript</a>&reg; language, <i>simple objects</i> are the <a href="u.html#unboxed">unboxed</a> objects.</p>
<p>Unlike a <a href="c.html#composite.object">composite object</a>, a simple object contains all its data in the object itself.</p>

<p><strong>Similar terms:</strong> <a href="u.html#unboxed">unboxed</a>.
<br />
<strong>Opposites:</strong> <a href="c.html#composite.object">composite object</a>.
</p></dd>

<dt><strong><a id="simple.segregated.storage" name="simple.segregated.storage">simple segregated storage</a></strong></dt>
<dd><p>A <a href="#segregated.free.list">segregated free list</a> <a href="a.html#allocation.mechanism">allocation mechanism</a> which divides <a href="#storage">storage</a> into <a href="p.html#page">pages</a> or other areas and only allocates <a href="o.html#object">objects</a> of a single size, or small range of sizes, within each area.  This makes allocation fast and avoids <a href="i.html#in-band.header">headers</a>, but may lead to high <a href="e.html#external.fragmentation">external fragmentation</a>, as unused parts of areas cannot be reused for other object sizes.</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil95">Paul R. Wilson, Mark S. Johnstone, Michael Neely, David Boles. 1995. <cite>Dynamic Storage Allocation: A Survey and Critical Review</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="smart.pointer" name="smart.pointer">smart pointer</a></strong></dt>
<dd><p>A smart pointer is an instance of a <a href="../articles/lang.html#cplusplus">C++</a> class that encapsulates a <a href="p.html#pointer">pointer</a> and performs <a href="r.html#reference.counting">reference counting</a>.</p>
<p>By overloading certain operators it is possible for the class to present the illusion of being a pointer, so that <code>operator*</code>, <code>operator-&gt;</code>, etc. can be used as normal.  Reference counting allows the objects that are referred to using the smart pointer class to have their <a href="#storage">storage</a> automatically <a href="r.html#reclaim">reclaimed</a> when they are no longer <a href="r.html#reference">referenced</a>.  It is a common technique used when trying to solve <a href="m.html#memory.management">memory management</a> problems in C++ applications.</p>

<p>However, reference counting is not always an appropriate memory management technique and smart pointers can be hard to implement properly in C++.  A <a href="t.html#tracing.garbage.collection">tracing garbage collector</a> might be worth considering.</p>

<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#edelson92a">Daniel R. Edelson. 1992. <cite>Smart pointers: They're smart, but they're not pointers</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="snap-out" name="snap-out">snap-out</a></strong>
  (also known as transport snap-out)</dt>
<dd><p>In a <a href="c.html#copying.garbage.collection">copying collector</a>, when there is a <a href="r.html#reference">reference</a> to an <a href="o.html#object">object</a> that was <a href="t.html#threatened.set">condemned</a>, but has been <a href="t.html#transport">transported</a>, snap-out is the adjustment of that reference to point to the preserved copy.</p>
<p>Typically the first transport leaves a <a href="f.html#forwarding.pointer">forwarding pointer</a> that enables the snap-out.</p>

<p align="center"><em>Snap-out</em><br /><img alt="Diagram: Snap-out" src="../diagrams/snap-out.png" border="2" height="149" width="363" /></p>

<p><strong>See also:</strong> <a href="b.html#broken.heart">broken heart</a>.
</p></dd>

<dt><strong><a id="snapshot-at-the-beginning" name="snapshot-at-the-beginning">snapshot-at-the-beginning</a>, snapshot at the beginning</strong></dt>
<dd><p>Snapshot-at-the-beginning algorithms for <a href="t.html#trace">tracing</a>, <a href="i.html#incremental.garbage.collection">incremental GC</a> note changes made by the <a href="m.html#mutator">mutator</a> to the <a href="g.html#graph">graph</a> of <a href="o.html#object">objects</a> and update the <a href="c.html#collector-2">collector<sup><small>(2)</small></sup></a> state to make it trace relevant <a href="e.html#edge">edges</a> that the mutator deletes.</p>
<p>In order for the collector to miss a <a href="r.html#reachable">reachable</a> <a href="o.html#object">object</a>, the following two conditions need to hold at some point during tracing:</p>

<ol>
<li>The mutator stores a <a href="r.html#reference">reference</a> to a <a href="w.html#white">white</a> object into a <a href="b.html#black">black</a> object.</li>
<li>All paths from any <a href="g.html#gray">gray</a> objects to that white object are destroyed.</li>
</ol>

<p>Snapshot-at-the-beginning algorithms ensure the second condition cannot occur, by causing the collector to process any reference that the mutator overwrites and that might be part of such a path.</p>

<p>They are so called because they keep track of references that existed at the beginning of the <a href="c.html#collection.cycle">collection cycle</a>.  Note that this does not mean all modifications need to be seen by the collector, only those needed to complete tracing without missing a reachable object (see <a href="../bib/f.html#pirinen98"><cite>Barrier techniques for incremental tracing</cite></a> for details), nor does it mean that it won't trace some references created during the collection.</p>

<p><strong>Historical note:</strong> This distinction between incremental-update and snapshot-at-the-beginning was first introduced for write-barrier algorithms, but it applies to any type of tracing algorithm.</p>

<p><strong>Opposites:</strong> <a href="i.html#incremental-update">incremental-update</a>.
<br />
<strong>See also:</strong> <a href="t.html#tri-color.marking">tri-color marking</a>;
    <a href="w.html#weak.tri-color.invariant">weak tri-color invariant</a>;
    <a href="b.html#barrier-1">barrier<sup><small>(1)</small></sup></a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil94">Paul R. Wilson. 1994. <cite>Uniprocessor Garbage Collection Techniques</cite>.</a></li>
  <li><a href="../bib/f.html#pirinen98">Pekka P. Pirinen. 1998. <cite>Barrier techniques for incremental tracing</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="soft.reference" name="soft.reference">soft reference</a></strong></dt>
<dd><p>In <a href="../articles/lang.html#java">Java</a><sup><small class="tm-small">TM</small></sup> terminology, <em>soft reference</em> is used to mean a <a href="r.html#reference">reference</a> encapsulated in a <a href="r.html#reference.object">reference object</a> of class <code>SoftReference</code>.</p>
<p>Soft references form one of three kinds of <a href="w.html#weak.reference-1">weak reference<sup><small>(1)</small></sup></a> in Java.  They are handy for building <a href="c.html#caching-3">caches<sup><small>(3)</small></sup></a> that are automatically flushed when memory is low.</p>

<p><strong>See also:</strong> <a href="#softly.reachable">softly reachable</a>.
<br />
<strong>Other links:</strong> <a href="http://java.sun.com/products/jdk/1.2/docs/api/java/lang/ref/SoftReference.html">Java spec for class SoftReference</a>;
    <a href="http://developer.java.sun.com/developer/technicalArticles/ALT/RefObj/index.html">Reference Objects and Garbage Collection</a>.
</p></dd>

<dt><strong><a id="softly.reachable" name="softly.reachable">softly reachable</a></strong></dt>
<dd><p>In <a href="../articles/lang.html#java">Java</a><sup><small class="tm-small">TM</small></sup>, an object is <em>softly reachable</em> if it is not <a href="#strongly.reachable">strongly reachable</a> and there is a path from the <a href="r.html#root">roots</a> to it that contains at least one <a href="#soft.reference">soft reference</a> but no <a href="w.html#weak.reference-2">weak<sup><small>(2)</small></sup></a> or <a href="p.html#phantom.reference">phantom references</a>.</p>
<p>When the Java <a href="c.html#collector-1">collector<sup><small>(1)</small></sup></a> determines that an object is softly reachable, it has the option of clearing the soft references involved, which will usually allow the object to be <a href="r.html#recycle">recycled</a>.  The idea is that they will only be cleared if the process is running short of <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a>.  If it is done, all soft references involved are cleared, so that the object is no longer softly reachable, and any affected <a href="r.html#reference.object">reference objects</a> which are registered with a queue are enqueued.</p>

<p><strong>See also:</strong> <a href="r.html#reachable">reachability</a>;
    <a href="w.html#weakly.reachable">weakly reachable</a>;
    <a href="p.html#phantom.reachable">phantom reachable</a>.
<br />
<strong>Other links:</strong> <a href="http://java.sun.com/products/jdk/1.2/docs/api/java/lang/ref/SoftReference.html">Java spec for class SoftReference</a>;
    <a href="http://developer.java.sun.com/developer/technicalArticles/ALT/RefObj/index.html">Reference Objects and Garbage Collection</a>.
</p></dd>

<dt><strong><a id="space.leak" name="space.leak">space leak</a></strong>
  (for full details, see <a href="m.html#memory.leak">memory leak</a>)</dt>
<dd><p>A memory leak is where <a href="a.html#allocate">allocated</a> <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> is not <a href="f.html#free-1">freed</a> although it is never used again.</p></dd>

<dt><strong><a id="spaghetti.stack" name="spaghetti.stack">spaghetti stack</a></strong>
  (for full details, see <a href="c.html#cactus.stack">cactus stack</a>)</dt>
<dd><p>A cactus stack is a <a href="#stack">stack</a> with branches. When diagrammed, its shape resembles that of a <a href="http://www.azstarnet.com/%7Efosnp/factsaboutsaguaros.html">saguaro cactus</a>.</p></dd>

<dt><strong><a id="split" name="split">split</a></strong></dt>
<dd><p>To divide a <a href="f.html#free.block">free block</a> into two smaller free blocks in the process
of satisfying an allocation request.</p>
<p>Deciding when to split a block is an important aspect of an
<a href="a.html#allocation.policy">allocation policy</a>.</p>

<p><strong>Opposites:</strong> <a href="c.html#coalesce">coalesce</a>.
<br />
<strong>See also:</strong> <a href="c.html#coalesce">coalesce</a>;
    <a href="a.html#allocation.policy">allocation policy</a>;
    <a href="f.html#free.block">free block</a>.
</p></dd>

<dt><strong><a id="sram" name="sram">SRAM</a></strong>
  (for full details, see <a href="#static.memory-1">static memory<sup><small>(1)</small></sup></a>)</dt>
<dd><p>Static <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> or static RAM (SRAM) is a type of <a href="p.html#physical.memory-2">physical memory<sup><small>(2)</small></sup></a> that does not need to be refreshed periodically to avoid losing state.</p></dd>

<dt><strong><a id="ssb" name="ssb">SSB</a></strong>
  (for full details, see <a href="#sequential.store.buffer">sequential store buffer</a>)</dt>
<dd><p>A sequential store buffer is a technique for dividing the cost of a <a href="w.html#write.barrier">write-barrier</a> by remembering which <a href="o.html#object">objects</a> are modified and updating <a href="r.html#remembered.set">remembered sets</a> (and so on) at a later stage.</p></dd>

<dt><strong><a id="stack" name="stack">stack</a></strong></dt>
<dd><p>A stack is a LIFO (last in, first out) collection: <a href="o.html#object">objects</a> may be <em>pushed</em> onto the stack, and <em>popped</em> off it in reverse order of pushing.</p>
<p>When people say "the stack", they usually mean the <a href="c.html#control.stack">control stack</a> supported by the OS and/or the processor.</p>

<p><strong>Relevance to memory management:</strong> <a href="#stack.allocation">Stack allocation</a> is an important technique.  Control stacks are central to the performance of the system and often require special handling.</p>

<p><strong>Historical note:</strong> The terms "stack", "push", and "pop" are taken from the spring-loaded dish stack found in cafeterias and salad bars where removing the top plate causes the others to rise up, exposing the next one, and adding a plate causes the spring to compress, leaving only that plate accessible.</p>

<p>So originally, the latest item was the "top", "down the stack" meant towards earlier items, and "up" towards later ones, but today many use "up" and "down" in the opposite sense.</p>

<p><strong>Similar terms:</strong> <a href="c.html#control.stack">control stack</a>.
<br />
<strong>See also:</strong> <a href="d.html#data.stack">data stack</a>;
    <a href="c.html#cactus.stack">cactus stack</a>.
</p></dd>

<dt><strong><a id="stack.allocation" name="stack.allocation">stack allocation</a></strong></dt>
<dd><p><em>Stack allocation</em> means run-time <a href="a.html#allocate">allocation</a> and <a href="f.html#free-1">deallocation</a> of <a href="#storage">storage</a> in last-in/first-out order.</p>
<p>Typically, stack allocation is performed on top of the main <a href="#stack">stack</a>, but one can have a separate <a href="d.html#data.stack">data stack</a> for this purpose as well, as in Forth, or even multiple ones, as in the <a href="../articles/lang.html#postscript">PostScript</a>&reg; language.</p>

<p>Allocation and deallocation are typically fast, since they can be done simply by adding or subtracting the size of the <a href="b.html#block">block</a> from the stack pointer.</p>

<p>Using only stack allocation, without heap allocation, is somewhat restrictive, as only objects whose size is known at compile-time can be returned from a procedure.</p>

<p>Some programming languages (such as some versions of <a href="../articles/lang.html#lisp">Lisp</a> and <a href="../articles/lang.html#c">C</a>) provide program-controlled stack <a href="a.html#allocate">allocation</a> and <a href="f.html#free-1">deallocation</a> of dynamic extent objects for efficiency, despite its being unsafe.</p>

<p><strong>Similar terms:</strong> <a href="a.html#automatic.storage.duration">automatic storage duration</a>.
<br />
<strong>Opposites:</strong> <a href="h.html#heap.allocation">heap allocation</a>;
    <a href="#static.allocation">static allocation</a>.
<br />
<strong>See also:</strong> <a href="r.html#region.inference">region inference</a>;
    <a href="d.html#dynamic.extent">dynamic extent</a>.
</p></dd>

<dt><strong><a id="stack.frame" name="stack.frame">stack frame</a></strong>
  (also known as stack record)</dt>
<dd><p>A stack frame or record is an <a href="a.html#activation.record">activation record</a> that is stored on the <a href="#stack">stack</a>.</p>
<p>In a register-based architecture, where the current activation record may be partially stored in registers, there may be hardware instructions that facilitate storing registers on the stack when another activation record is made current.  Such instructions may prescribe a particular layout for activation records.</p>

<p><strong>Relevance to memory management:</strong> Hardware support for saving and restoring registers, for stacks and for stack addressing may limit or otherwise prescribe the size and type of data that can be stored in a stack frame. Knowledge of the layout of each stack frame may assist a <a href="g.html#garbage.collector">garbage collector</a> in finding <a href="r.html#root">roots</a>.</p>

<p><strong>Similar terms:</strong> <a href="a.html#activation.record">activation record</a>.
<br />
<strong>See also:</strong> <a href="#stack">stack</a>.
</p></dd>

<dt><strong><a id="stack.record" name="stack.record">stack record</a></strong>
  (for full details, see <a href="#stack.frame">stack frame</a>)</dt>
<dd><p>A stack frame or record is an <a href="a.html#activation.record">activation record</a> that is stored on the <a href="#stack">stack</a>.</p></dd>

<dt><strong><a id="static.allocation" name="static.allocation">static allocation</a></strong></dt>
<dd><p><em>Static allocation</em> means <a href="a.html#allocate">allocation</a> of <a href="#storage">storage</a> before the program starts and retention until the end.</p>
<p>The locations of <a href="o.html#object">objects</a> are basically decided at compile-time, although they might be <a href="r.html#relocation">relocated</a> at load-time.  This implies the sizes of the objects must be known then.</p>

<p>Using only static allocation is restrictive, as sizes of data structures can't be dynamically varied, and procedures cannot be recursive.  However, it is also fast and eliminates the possibility of running out of memory.  For this reason, this scheme is sometimes used in real-time systems.</p>

<p><strong>Historical note:</strong> The first high-level language, <a href="../articles/lang.html#fortran">Fortran</a>, only had static allocation to begin with.  Later languages usually offer heap and/or stack allocation, but static allocation is often available as an option.</p>

<p><strong>Similar terms:</strong> <a href="#static.storage.duration">static storage duration</a>.
<br />
<strong>Opposites:</strong> <a href="#stack.allocation">stack allocation</a>;
    <a href="h.html#heap.allocation">heap allocation</a>.
<br />
<strong>See also:</strong> <a href="r.html#region.inference">region inference</a>;
    <a href="#static.memory-2">static memory<sup><small>(2)</small></sup></a>.
</p></dd>

<dt><strong><a id="static.memory-1" name="static.memory-1">static memory<sup><small>(1)</small></sup></a></strong>
  (also known as static RAM, SRAM)</dt>
<dd><p>Static <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> or static RAM (SRAM) is a type of <a href="p.html#physical.memory-2">physical memory<sup><small>(2)</small></sup></a> that does not need to be refreshed periodically to avoid losing state.</p>
<p>Static memory  is typically faster than <a href="d.html#dynamic.memory">dynamic memory</a>, or requires essentially no power to preserve its state, but rarely both. These benefits result in static RAM being used for <a href="c.html#cache-1">cache<sup><small>(1)</small></sup></a> memory, and also in portable, low-power applications (such as PDAs).  It is, however, more expensive than dynamic RAM and requires more transistors, making dynamic RAM the choice for large amounts of memory (the <a href="m.html#main.memory">main memory</a> of desktop machines, for example).</p>

<p><strong>Opposites:</strong> <a href="d.html#dynamic.memory">dynamic memory</a>.
</p></dd>

<dt><strong><a id="static.memory-2" name="static.memory-2">static memory<sup><small>(2)</small></sup></a></strong></dt>
<dd><p>The <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> where <a href="#static.allocation">statically allocated</a> objects are stored is sometimes known as <em>static memory</em>.  In the context of <a href="g.html#garbage.collection">garbage collection</a>, the term is used mean memory used to store <a href="#static.object">static objects</a>.</p>
<p><strong>See also:</strong> <a href="#static.storage.duration">static storage duration</a>.
</p></dd>

<dt><strong><a id="static.object" name="static.object">static object</a></strong></dt>
<dd><p>A static <a href="o.html#object">object</a> is non-<a href="m.html#moving.garbage.collector">moving</a>.  That is, it is not <a href="r.html#relocation">relocated</a> by a <a href="m.html#memory.manager">memory manager</a>; its <a href="a.html#address">address</a> does not change.</p></dd>

<dt><strong><a id="static.ram" name="static.ram">static RAM</a></strong>
  (for full details, see <a href="#static.memory-1">static memory<sup><small>(1)</small></sup></a>)</dt>
<dd><p>Static <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> or static RAM (SRAM) is a type of <a href="p.html#physical.memory-2">physical memory<sup><small>(2)</small></sup></a> that does not need to be refreshed periodically to avoid losing state.</p></dd>

<dt><strong><a id="static.storage.duration" name="static.storage.duration">static storage duration</a></strong></dt>
<dd><p>In <a href="../articles/lang.html#c">C</a> and <a href="../articles/lang.html#cplusplus">C++</a>, the <code>static</code> keyword applied to a file scope variable or function means it is local to the file; the <code>static</code> keyword applied to a function or a block scope variable means it is <a href="a.html#allocate">allocated</a> and initialized once only.</p>
<p>Objects declared locally in blocks with the <code>static</code> keyword are <a href="a.html#allocate">allocated</a> in <a href="#static.memory-2">static memory<sup><small>(2)</small></sup></a>, and initialized once (usually by the compiler/linker) instead of each time the block is entered.</p>

<p>Static variables within functions retain their value between function invocations, and therefore must form part of the <a href="r.html#root.set">root set</a> of any <a href="c.html#collector-1">collector<sup><small>(1)</small></sup></a>.</p>

<p><strong>Opposites:</strong> <a href="a.html#automatic.storage.duration">automatic storage duration</a>.
<br />
<strong>See also:</strong> <a href="l.html#lifetime">lifetime</a>.
</p></dd>

<dt><strong><a id="sticky.reference.count" name="sticky.reference.count">sticky reference count</a></strong>
  (for full details, see <a href="l.html#limited-field.reference.count">limited-field reference count</a>)</dt>
<dd><p>A <a href="r.html#reference.counting">reference counting</a> technique whereby the field used to store the number of <a href="r.html#reference">references</a> to an <a href="o.html#object">object</a> has a limited size.  In particular, the field is not large enough to represent the maximum possible number of references to an object.</p></dd>

<dt><strong><a id="storage" name="storage">storage</a></strong>
  (for full details, see <a href="m.html#memory-1">memory<sup><small>(1)</small></sup></a>)</dt>
<dd><p><i>memory</i> or <i>storage</i> (or <i>store</i>) is where data and instructions are stored.  For example, <a href="c.html#cache-1">caches<sup><small>(1)</small></sup></a>, <a href="m.html#main.memory">main memory</a>, floppy and hard disks are all storage devices.
</p></dd>

<dt><strong><a id="storage.hierarchy" name="storage.hierarchy">storage hierarchy</a></strong>
  (also known as memory hierarchy)</dt>
<dd><p>A typical computer has several different <em>levels</em> of <a href="#storage">storage</a>.
Each level of storage has a different speed, cost, and size.
The levels form a <em>storage hierarchy</em>, in which the topmost levels (those nearest the processor) are fastest, most expensive and smallest.</p>
<p>Levels typically include processor <a href="r.html#register">registers</a>, possibly some levels of <a href="c.html#cache-1">cache<sup><small>(1)</small></sup></a>, <a href="m.html#main.memory">main memory</a>, and possibly some levels of <a href="b.html#backing.store">backing store</a>.</p>

<p>Each level is commonly used as a <a href="c.html#cache-2">cache<sup><small>(2)</small></sup></a> for the next level.
For instance, <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> systems use main memory as a cache for backing store.</p>

<p align="center"><em>Storage hierarchy with relative speed, cost, and typical size</em><br /><img alt="Diagram: Storage hierarchy with relative speed, cost, and typical size" src="../diagrams/storage.png" border="2" height="388" width="347" /></p></dd>


<dt><strong><a id="storage.level" name="storage.level">storage level</a></strong></dt>
<dd><p>One level in a <a href="#storage.hierarchy">storage hierarchy</a>, for instance a <a href="c.html#cache-1">cache<sup><small>(1)</small></sup></a>,
        <a href="m.html#main.memory">main memory</a>, <a href="b.html#backing.store">backing store</a>, and so on.</p>
<p><strong>See also:</strong> <a href="#storage.hierarchy">storage hierarchy</a>.
</p></dd>

<dt><strong><a id="storage.management" name="storage.management">storage management</a></strong>
  (for full details, see <a href="m.html#memory.management">memory management</a>)</dt>
<dd><p>Memory management is the art and the process of coordinating and controlling the use of <a href="m.html#memory-1">memory<sup><small>(1)</small></sup></a> in a computer system.</p></dd>

<dt><strong><a id="store-1" name="store-1">store<sup><small>(1)</small></sup></a></strong></dt>
<dd><p>To transfer data from a processor's <a href="r.html#register">registers</a> to <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a>.</p>
<p>Store can also be used in the more general sense of transferring data from a part of the <a href="m.html#memory.hierarchy">memory hierarchy</a> that is fast to access to one that is slow to access.</p>

<p><code>STORE</code> (or an abbreviation) is also commonly used in many processor architectures as the mnemonic for the machine code instructions that store data into memory.</p>

<p><strong>Opposites:</strong> <a href="l.html#load">load</a>.
</p></dd>

<dt><strong><a id="store-2" name="store-2">store<sup><small>(2)</small></sup></a></strong>
  (for full details, see <a href="m.html#memory-1">memory<sup><small>(1)</small></sup></a>)</dt>
<dd><p><i>memory</i> or <i>storage</i> (or <i>store</i>) is where data and instructions are stored.  For example, <a href="c.html#cache-1">caches<sup><small>(1)</small></sup></a>, <a href="m.html#main.memory">main memory</a>, floppy and hard disks are all storage devices.
</p></dd>

<dt><strong><a id="strict.segregated.fit" name="strict.segregated.fit">strict segregated fit</a></strong></dt>
<dd><p>A <a href="#segregated.fit">segregated fit</a> <a href="a.html#allocation.mechanism">allocation mechanism</a> which has only one block
size on each <a href="f.html#free.list">free list</a>. A requested block size is rounded up to the
next provided size, and the first block on that list is returned. The
sizes must be chosen so that any block of a larger size can be <a href="#split">split</a>
into a number of smaller sized blocks. <a href="b.html#buddy.system">Buddy systems</a> are a special
case of strict segregated fit allocators.</p>
<p><strong>See also:</strong> <a href="b.html#buddy.system">buddy system</a>;
    <a href="#segregated.fit">segregated fit</a>;
    <a href="#segregated.free.list">segregated free list</a>;
    <a href="a.html#allocation.mechanism">allocation mechanism</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil95">Paul R. Wilson, Mark S. Johnstone, Michael Neely, David Boles. 1995. <cite>Dynamic Storage Allocation: A Survey and Critical Review</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="strong.reference" name="strong.reference">strong reference</a></strong></dt>
<dd><p>In a <a href="t.html#tracing.garbage.collection">tracing garbage collector</a>, a strong reference is a <a href="r.html#reference">reference</a> that keeps the <a href="o.html#object">object</a> it refers to <a href="a.html#alive">alive</a>.</p>
<p>A strong reference is the usual sort of reference; The term is usually used to draw a contrast with <a href="w.html#weak.reference-1">weak reference<sup><small>(1)</small></sup></a>.</p>

<p><strong>Opposites:</strong> <a href="w.html#weak.reference-1">weak reference<sup><small>(1)</small></sup></a>.
<br />
<strong>See also:</strong> <a href="#strong.root">strong root</a>.
</p></dd>

<dt><strong><a id="strong.root" name="strong.root">strong root</a></strong></dt>
<dd><p>A strong root is a <a href="r.html#root">root</a> such that all <a href="r.html#reference">references</a> in it are <a href="#strong.reference">strong references</a>.</p>
<p>A strong root is the usual sort of root; The term is usually used to draw a contrast with <a href="w.html#weak.root">weak root</a>.</p>

<p><strong>Opposites:</strong> <a href="w.html#weak.root">weak root</a>.
</p></dd>

<dt><strong><a id="strong.tri-color.invariant" name="strong.tri-color.invariant">strong tri-color invariant</a>, strong tri-colour invariant, strong tricolor invariant, strong tricolour invariant</strong></dt>
<dd><p>The strong <a href="t.html#tri-color.invariant">tri-color invariant</a> is the property of a <a href="r.html#reference">reference</a> <a href="g.html#graph">graph</a> that there is no <a href="e.html#edge">edge</a> from a <a href="b.html#black">black</a> <a href="n.html#node">node</a> to a <a href="w.html#white">white</a> node.</p>
<p>By preserving this property throughout <a href="t.html#tri-color.marking">tri-color marking</a>, a <a href="t.html#trace">tracing</a> algorithm can ensure that the <a href="c.html#collector-2">collector<sup><small>(2)</small></sup></a> will not miss reachable objects, even if the <a href="m.html#mutator">mutator</a> manipulates the graph during the collection.  This invariant can also be used to ensure that a <a href="c.html#copying.garbage.collection">copying garbage collector</a> doesn't confuse the mutator.  Mutator actions might need to change the <a href="c.html#color">color</a> of the nodes affected in order to preserve the invariant (see <a href="../bib/f.html#pirinen98"><cite>Barrier techniques for incremental tracing</cite></a> for details).</p>

<p>Algorithms using this invariant are <a href="i.html#incremental-update">incremental-update</a> algorithms.</p>

<p><strong>Similar terms:</strong> <a href="t.html#tri-color.invariant">tri-color invariant</a>.
<br />
<strong>See also:</strong> <a href="b.html#barrier-1">barrier<sup><small>(1)</small></sup></a>;
    <a href="w.html#weak.tri-color.invariant">weak tri-color invariant</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#pirinen98">Pekka P. Pirinen. 1998. <cite>Barrier techniques for incremental tracing</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="strongly.reachable" name="strongly.reachable">strongly reachable</a></strong></dt>
<dd><p>In <a href="../articles/lang.html#java">Java</a><sup><small class="tm-small">TM</small></sup>, an object is <em>strongly reachable</em>, if  there is a path from the <a href="r.html#root">roots</a> to it that contains only <a href="#strong.reference">strong references</a>, i.e., no <a href="r.html#reference.object">reference objects</a>.</p>
<p><strong>See also:</strong> <a href="r.html#reachable">reachability</a>;
    <a href="#softly.reachable">softly reachable</a>;
    <a href="w.html#weakly.reachable">weakly reachable</a>;
    <a href="p.html#phantom.reachable">phantom reachable</a>.
<br />
<strong>Other links:</strong> <a href="http://developer.java.sun.com/developer/technicalArticles/ALT/RefObj/index.html">Reference Objects and Garbage Collection</a>.
</p></dd>

<dt><strong><a id="suballocator" name="suballocator">suballocator</a></strong></dt>
<dd><p>A <em>suballocator</em> is an <a href="a.html#allocator">allocator</a> functioning on top of another allocator.</p>
<p>Suballocators work by <a href="a.html#allocate">allocating</a> large <a href="b.html#block">blocks</a> and <a href="#split">splitting</a> them for use, or by <a href="r.html#recycle">recycling</a> blocks locally.</p>

<p>Application programmers sometimes write their own suballocators when faced with an inefficient or inadequate <a href="m.html#memory.manager">memory manager</a>.  Suballocators can take advantage of special knowledge of program behavior, but are less efficient in general than fixing the underlying allocator, mainly because <a href="m.html#memory.management">memory management</a> is a <em>global</em> issue for an application, and a global strategy can make a big difference.
For example, different suballocators can interact catastrophically with each other and with the <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> system, causing the application's memory requirements to grow unnecessarily due to <a href="f.html#fragmentation">fragmentation</a>.</p></dd>


<dt><strong><a id="subgraph" name="subgraph">subgraph</a></strong></dt>
<dd><p>A subgraph S of a <a href="g.html#graph">graph</a> G is a graph such that all the <a href="n.html#node">nodes</a> in S are also in G and all the <a href="e.html#edge">edges</a> in S are also in G; that is, it is a part of a graph.</p></dd>

<dt><strong><a id="sure.reference" name="sure.reference">sure reference</a></strong>
  (for full details, see <a href="e.html#exact.reference">exact reference</a>)</dt>
<dd><p>An exact  or precise or sure reference is a value the <a href="c.html#collector-1">collector<sup><small>(1)</small></sup></a> knows is a <a href="r.html#reference">reference</a>.</p></dd>

<dt><strong><a id="swap.space" name="swap.space">swap space</a></strong></dt>
<dd><p><a href="b.html#backing.store">Backing store</a> used by a <a href="#swapping">swapping</a> system.</p>
<p><strong>See also:</strong> <a href="#swapping">swapping</a>;
    <a href="b.html#backing.store">backing store</a>.
</p></dd>

<dt><strong><a id="swapped.in" name="swapped.in">swapped in</a></strong></dt>
<dd><p>A process or <a href="p.html#page">page</a> is <em>swapped in</em> if it is available in <a href="p.html#physical.memory-1">physical memory<sup><small>(1)</small></sup></a>.
This usually applies to the entire program image.</p>
<p><strong>Similar terms:</strong> <a href="p.html#paged.in">paged in</a>.
<br />
<strong>Opposites:</strong> <a href="#swapped.out">swapped out</a>.
<br />
<strong>See also:</strong> <a href="#swapping">swapping</a>.
</p></dd>

<dt><strong><a id="swapped.out" name="swapped.out">swapped out</a></strong></dt>
<dd><p>A process or <a href="p.html#page">page</a> is <em>swapped out</em> if it is not available in <a href="p.html#physical.memory-1">physical memory<sup><small>(1)</small></sup></a>.
This usually applies to the entire program image.</p>
<p><strong>Similar terms:</strong> <a href="p.html#paged.out">paged out</a>.
<br />
<strong>Opposites:</strong> <a href="#swapped.in">swapped in</a>.
<br />
<strong>See also:</strong> <a href="#swapping">swapping</a>.
</p></dd>

<dt><strong><a id="swapping" name="swapping">swapping</a></strong></dt>
<dd><p>Historically, swapping was the technique of moving entire program images to disk (or drum) and back into <a href="p.html#physical.memory-1">physical memory<sup><small>(1)</small></sup></a>, an early form of <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a>.  Nowadays, it is used as a synonym for <a href="p.html#paging">paging</a>.</p>
<p><strong>Similar terms:</strong> <a href="p.html#paging">paging</a>.
<br />
<strong>See also:</strong> <a href="#swapped.in">swapped in</a>;
    <a href="#swapped.out">swapped out</a>.
</p></dd>

<dt><strong><a id="sweeping" name="sweeping">sweeping</a></strong></dt>
<dd><p>Sweeping is the second phase ("the sweep phase") of the <a href="m.html#mark-sweep">mark-sweep</a> algorithm (q.v.).  It performs a sequential (address-order) pass over memory to <a href="r.html#recycle">recycle</a> unmarked blocks.</p>
<p>Sweeping typically gathers all unmarked blocks into one or more <a href="f.html#free.list">free lists</a>.</p>

<p><strong>See also:</strong> <a href="m.html#marking">marking</a>.
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
<a href="r.html">R</a>
<strong>S</strong>
<a href="t.html">T</a>
<a href="u.html">U</a>
<a href="v.html">V</a>
<a href="w.html">W</a>
X
Y
<a href="z.html">Z</a> - <a href="help.html">Help</a></p>

</body></html>