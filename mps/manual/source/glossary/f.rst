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
<link rel="prev" href="e.html" />
<link rel="next" href="g.html" />
<title>The Memory Management Glossary: F</title>
</head>
<body lang="en" bgcolor="#FFFFFF" text="#000000" link="#000099" vlink="#660066" alink="#FF0000">
<h1 align="center">The Memory Management Glossary<br />
<big>F</big></h1>
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
<strong>F</strong>
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
<dt><strong><a id="fencepost" name="fencepost">fencepost</a>, fence post</strong></dt>
<dd><p>A fencepost is spare <a href="m.html#memory-1">memory<sup><small>(1)</small></sup></a> between <a href="a.html#allocate">allocated</a> <a href="b.html#block">blocks</a> for checking purposes.</p>
<p>Some <a href="m.html#memory.management">memory management</a> systems leave spare memory between allocated blocks and store special values in it.  If a checking routine finds that these memory <a href="m.html#memory.location">locations</a> have been modified, this probably indicates an <a href="o.html#overwriting.error">overwriting error</a> in the application that was allocated the adjacent block.</p>

<p>Such checking is extremely useful in a memory manager, because it can help application programmers to find bugs that would otherwise be difficult to reproduce and track down.</p>

<p><strong>Similar terms:</strong> <a href="i.html#in-band.header">in-band header</a>.
</p></dd>

<dt><strong><a id="fencepost.error" name="fencepost.error">fencepost error</a>, fence post error</strong></dt>
<dd><p>The term <em>fencepost error</em> refers to errors arising from the fact that, to enclose <em>n</em> consecutive intervals, you need <em>n+1</em> end-points, from the number of posts required to support fence rails.</p>
<p>An example of a fencepost error would be, in <a href="../articles/lang.html#c">C</a>:</p>

<pre><code>
void f(void)
&#123;
  int i;
  int a&#91;10&#93;;
  for(i = 0; i &lt;= 10; i++)
    a&#91;i&#93; = 0;
&#125;
</code></pre>

<p>because the declaration <code>int a&#91;10&#93;;</code> creates an array of ten integers, with index from 0 to 9, but the <code>for</code> loop index <code>i</code> runs from 0 to 10.</p></dd>


<dt><strong><a id="fibonacci.buddies" name="fibonacci.buddies">Fibonacci buddies</a></strong></dt>
<dd><p>A common <a href="b.html#buddy.system">buddy system</a> <a href="a.html#allocation.mechanism">allocation mechanism</a>, in which block sizes
form a Fibonacci series (each block size is the sum of the two
previous sizes). Each block can therefore be <a href="s.html#split">split</a> to form two blocks
of valid sizes, and the sizes are more closely spaced than in <a href="b.html#binary.buddies">binary buddies</a>. However, if the same size is allocated repeatedly,
performance may suffer as the remainder blocks may have to be split
again (or become fragments).</p>
<p><strong>See also:</strong> <a href="b.html#buddy.system">buddy system</a>;
    <a href="a.html#allocation.mechanism">allocation mechanism</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil95">Paul R. Wilson, Mark S. Johnstone, Michael Neely, David Boles. 1995. <cite>Dynamic Storage Allocation: A Survey and Critical Review</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="fifo-ordered.first.fit" name="fifo-ordered.first.fit">FIFO-ordered first fit</a></strong></dt>
<dd><p>The <a href="a.html#allocation.policy">allocation policy</a> that always uses the least-recently <a href="#free-1">freed</a>
suitable <a href="#free.block">free block</a>. Commonly implemented by adding freed blocks to
the end of a <a href="#free.block.chain">free block chain</a>, and then using <a href="#first.fit">first fit</a> allocation
on this chain. <a href="#free-1">free<sup><small>(1)</small></sup></a> can be very quick, depending on the <a href="c.html#coalesce">coalescing</a>
policy.</p>
<p>According to <a href="../bib/f.html#wil95"><cite>Dynamic Storage Allocation: A Survey and Critical Review</cite></a>, this policy controls fragmentation quite well, better than <a href="l.html#lifo-ordered.first.fit">LIFO-ordered first fit</a> and as well as <a href="a.html#address-ordered.first.fit">address-ordered first fit</a> in some cases, although <a href="l.html#locality.of.reference">locality</a> may be worse.</p>

<p><strong>See also:</strong> <a href="#first.fit">first fit</a>;
    <a href="l.html#lifo-ordered.first.fit">LIFO-ordered first fit</a>;
    <a href="a.html#address-ordered.first.fit">address-ordered first fit</a>;
    <a href="a.html#allocation.policy">allocation policy</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil95">Paul R. Wilson, Mark S. Johnstone, Michael Neely, David Boles. 1995. <cite>Dynamic Storage Allocation: A Survey and Critical Review</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="file.mapping" name="file.mapping">file mapping</a></strong>
  (for full details, see <a href="m.html#memory.mapping">memory mapping</a>)</dt>
<dd><p><em>Memory mapping</em> is the technique of making a part of the <a href="a.html#address.space">address space</a> appear to contain an "object", such as a file or device, so that ordinary <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> accesses act on that object.</p></dd>

<dt><strong><a id="finalization" name="finalization">finalization</a></strong>
  (also known as termination)</dt>
<dd><p>In <a href="g.html#garbage.collection">garbage-collected</a> languages, it is often necessary to perform actions on some <a href="o.html#object">objects</a> after they are no longer in use and before their <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> can be <a href="r.html#recycle">recycled</a>.  These actions are known as <em>finalization</em> or <em>termination</em>.</p>
<p>A common use of finalization is to release resources when the corresponding "proxy" object dies.  For example, an open file might be represented by a stream object.  When this object has been proven <a href="d.html#dead">dead</a> by the <a href="c.html#collector-1">collector<sup><small>(1)</small></sup></a>, it is certain that the file is no longer in use by the program, and it can and should be closed before the stream is recycled.</p>

<p>Note that finalization is not, in general, guaranteed to be prompt, and this can cause problems if it is used to manage scarce operating system resources such as file descriptors.</p>

<p>Many object-oriented languages provide support for finalization, for example, Cedar, <a href="../articles/lang.html#java">Java</a>, <a href="../articles/lang.html#perl">Perl</a> 5, and <a href="../articles/lang.html#smalltalk">Smalltalk</a>.</p>

<p>The term <em>finalization</em> is sometimes used to refer to the use of <a href="d.html#destructor-1">destructors<sup><small>(1)</small></sup></a>, for example in Ada.</p></dd>


<dt><strong><a id="first.fit" name="first.fit">first fit</a></strong></dt>
<dd><p>First fit is a <a href="s.html#sequential.fit">sequential fit</a> <a href="a.html#allocation.mechanism">allocation mechanism</a>.
To quote <a href="../bib/f.html#wil95"><cite>Dynamic Storage Allocation: A Survey and Critical Review</cite></a>:</p>

<blockquote>First fit simply searches the <a href="#free.list">free list</a> from the beginning, and uses the first
<a href="#free.block">free block</a> large enough to satisfy the request.  If the block is
larger than necessary, it is split and the remainder is put on the free
list.</blockquote>
<p>The first fit mechanism provides a class of first fit <a href="a.html#allocation.policy">allocation policies</a>, depending on the order in which the free list is
stored. <a href="a.html#address-ordered.first.fit">Address-ordered first fit</a> stores the list in order of
(usually increasing) address.  <a href="l.html#lifo-ordered.first.fit">LIFO-ordered first fit</a> puts blocks on
the front of the free list when they are <a href="#free-1">freed</a>.  <a href="#fifo-ordered.first.fit">FIFO-ordered first fit</a> puts blocks on the end of the free list when they are <a href="#free-1">freed</a>.</p>

<p><strong>See also:</strong> <a href="a.html#address-ordered.first.fit">address-ordered first fit</a>;
    <a href="l.html#lifo-ordered.first.fit">LIFO-ordered first fit</a>;
    <a href="#fifo-ordered.first.fit">FIFO-ordered first fit</a>;
    <a href="s.html#sequential.fit">sequential fit</a>;
    <a href="n.html#next.fit">next fit</a>;
    <a href="b.html#best.fit">best fit</a>;
    <a href="w.html#worst.fit">worst fit</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil95">Paul R. Wilson, Mark S. Johnstone, Michael Neely, David Boles. 1995. <cite>Dynamic Storage Allocation: A Survey and Critical Review</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="flip" name="flip">flip</a></strong></dt>
<dd><p>The instant in a <a href="t.html#two-space.collector">two-space collector</a> when the roles of the two <a href="s.html#semi-space">semi-spaces</a> are reversed.  What was the <i>new</i> semi-space is now marked as <i>old</i> and <a href="t.html#threatened.set">condemned</a>.  What was the old semi-space becomes the site for all new <a href="a.html#allocate">allocations</a>.  Also used in a more general sense to mean the initiation of a new <a href="c.html#collection.cycle">collection cycle</a>.</p></dd>

<dt><strong><a id="floating.garbage" name="floating.garbage">floating garbage</a></strong></dt>
<dd><p>Floating garbage is <a href="g.html#garbage">garbage</a> that is not <a href="r.html#recycle">recycled</a> promptly due to some approximation or optimization in the <a href="g.html#garbage.collector">garbage collector</a>.</p>
<p>Floating garbage results from conservatively estimating an <a href="o.html#object">object</a> that is really <a href="u.html#unreachable">unreachable</a> to be <a href="r.html#reachable">reachable</a> for the purposes of a particular <a href="c.html#collection.cycle">collection cycle</a>.  Using estimates can have considerable performance benefits but also result in higher <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> consumption.</p>

<p>Typical estimates that cause floating garbage are:</p>
<ul>
  <li>Every register or <a href="a.html#activation.frame">activation frame</a> slot holds a reachable value: this is not always true, as objects stored in dead registers or slots may be otherwise unreachable.  This estimate can simplify the compiler as well as the interface between the compiler and the garbage collector.</li>
  <li>Every object in a <a href="r.html#remembered.set">remembered set</a> is reachable: this is not always true, because remembered objects can have become unreachable since they were added to the remembered set.  This estimate allows remembered sets to be effective; the alternative -- determining whether each remembered object is reachable -- is equivalent to a full garbage collection.</li>
  <li>Anything that looks like a <a href="r.html#reference">reference</a> is one: this is not generally true, because random data can have the same bit pattern as a pointer.  <a href="c.html#conservative.garbage.collection">Conservative garbage collectors</a> use this estimate.</li>
  <li>Any object referenced from another is reachable: this is not generally true, because garbage can reference other garbage.  <a href="r.html#reference.counting">Reference counting</a> collectors use this estimate, resulting in their not being able to reclaim self-referential structures.</li>
  <li>Any object reached during collection remains live until the next collection: this may not be true when the garbage collector runs interleaved with the mutator, as do <a href="i.html#incremental.garbage.collection">incremental</a> and <a href="p.html#parallel.garbage.collection">concurrent</a> collectors.</li>
</ul>

<p>A more subtle kind of floating garbage is an unreachable data structure that spans multiple regions that are never <a href="t.html#threatened.set">condemned</a> together.</p></dd>


<dt><strong><a id="format" name="format">format</a></strong></dt>
<dd><p>A format describes the representation of an <a href="o.html#object">object</a>; that is, how the object is laid out in memory.</p>
<p>A format usually specifies where the fields of the objects are located and what their type is.</p>

<p><strong>Relevance to memory management:</strong> If formats are provided by a language or the application program, <a href="e.html#exact.garbage.collection">exact garbage collection</a> can be used, because the <a href="c.html#collector-1">collector<sup><small>(1)</small></sup></a> can determine which fields are <a href="r.html#reference">references</a>.</p>

<p><strong>See also:</strong> <a href="c.html#conservative.garbage.collection">conservative garbage collection</a>.
</p></dd>

<dt><strong><a id="forwarding.pointer" name="forwarding.pointer">forwarding pointer</a></strong></dt>
<dd><p>Some <a href="g.html#garbage.collector">garbage collectors</a>  <a href="m.html#moving.garbage.collector">move</a> <a href="r.html#reachable">reachable</a> <a href="o.html#object">objects</a> into another space. They leave a <a href="#forwarding.pointer">forwarding pointer</a> -- that is, a special <a href="r.html#reference">reference</a> pointing to the new location -- in the old <a href="l.html#location">location</a>,  .</p>
<p><strong>Similar terms:</strong> <a href="b.html#broken.heart">broken heart</a>.
<br />
<strong>See also:</strong> <a href="c.html#copying.garbage.collection">copying garbage collection</a>;
    <a href="t.html#two-space.collector">two space collector</a>.
</p></dd>

<dt><strong><a id="fragmentation" name="fragmentation">fragmentation</a></strong></dt>
<dd><p>Fragmentation is the inability to use <a href="m.html#memory-1">memory<sup><small>(1)</small></sup></a> because of the arrangement of memory already in use.
It is usually divided into <a href="e.html#external.fragmentation">external fragmentation</a> and <a href="i.html#internal.fragmentation">internal fragmentation</a>.</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#jw98">Mark S. Johnstone, Paul R. Wilson. 1998. <cite>The Memory Fragmentation Problem: Solved?</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="frame" name="frame">frame</a></strong>
  (for full details, see <a href="i.html#in-band.header">in-band header</a>)</dt>
<dd><p>Some <a href="m.html#memory.manager">memory managers</a> <a href="a.html#allocate">allocate</a> a fixed amount more than is necessary for each <a href="b.html#block">block</a> and use it to store information such as the size of the block or a <a href="t.html#tag">tag</a>.  This extra memory is known as <em>an in-band header</em> or <em>a frame</em></p></dd>

<dt><strong><a id="free-1" name="free-1">free<sup><small>(1)</small></sup></a></strong>
  (also known as deallocate)</dt>
<dd><p>In <a href="m.html#manual.memory.management">manual memory management</a>, to free or deallocate an <a href="o.html#object">object</a> is to tell the <a href="m.html#memory.manager">memory manager</a> that it is no longer needed.  The <a href="m.html#memory-1">memory<sup><small>(1)</small></sup></a> may then be <a href="r.html#recycle">recycled</a> by being used for subsequent <a href="a.html#allocate">allocation</a>, or by being returned to the operating system.</p>
<p><strong>Opposites:</strong> <a href="a.html#allocate">allocate</a>.
<br />
<strong>See also:</strong> <a href="#free-2">free<sup><small>(2)</small></sup></a>;
    <a href="d.html#destructor-1">destructor<sup><small>(1)</small></sup></a>.
</p></dd>

<dt><strong><a id="free-2" name="free-2">free<sup><small>(2)</small></sup></a></strong></dt>
<dd><p>In <a href="../articles/lang.html#c">C</a>, the system function used for explicit <a href="#free-1">deallocation</a> is called <code>free</code>. </p></dd>

<dt><strong><a id="free-3" name="free-3">free<sup><small>(3)</small></sup></a></strong></dt>
<dd><p><a href="m.html#memory-2">Memory<sup><small>(2)</small></sup></a> is <i>free</i> if it is not currently <a href="a.html#allocate">allocated</a>.</p>
<p><strong>Historical note:</strong> The term <i>available</i> was commonly used to mean "free".</p>

<p><strong>Opposites:</strong> <a href="a.html#allocate">allocated</a>.
<br />
<strong>See also:</strong> <a href="a.html#allocate">allocate</a>;
    <a href="#free-1">free<sup><small>(1)</small></sup></a>.
</p></dd>

<dt><strong><a id="free-4" name="free-4">free<sup><small>(4)</small></sup></a></strong>
  (for full details, see <a href="u.html#unmapped">unmapped</a>)</dt>
<dd><p>A range of <a href="v.html#virtual.address">virtual addresses</a> is said to be <em>unmapped</em> (<em>free</em> on Windows&reg;) if there is no <a href="p.html#physical.memory-2">physical memory<sup><small>(2)</small></sup></a> associated with the range.</p></dd>

<dt><strong><a id="free.block" name="free.block">free block</a></strong></dt>
<dd><p>A single contiguous area of <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> available to satisfy an <a href="a.html#allocate">allocation</a> request.</p>
<p>For the purpose of discussing <a href="a.html#allocation.mechanism">allocation mechanisms</a>, two adjacent free blocks are not considered to be a single free block, until they are <a href="c.html#coalesce">coalesced</a>.  Free blocks may be <a href="s.html#split">split</a>.</p>

<p><strong>See also:</strong> <a href="a.html#allocation.mechanism">allocation mechanism</a>;
    <a href="#free.list">free list</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil95">Paul R. Wilson, Mark S. Johnstone, Michael Neely, David Boles. 1995. <cite>Dynamic Storage Allocation: A Survey and Critical Review</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="free.block.chain" name="free.block.chain">free block chain</a></strong></dt>
<dd><p>Some systems store the <a href="#free.list">free list</a> as a linked list, or chain. </p>
<p>Usually the links are stored within the <a href="#free-3">free<sup><small>(3)</small></sup></a> <a href="b.html#block">blocks</a>.  This means that all <a href="a.html#allocate">allocated</a> blocks must be large enough to store these, and implies a minimum size.</p>

<p>Sometimes, the free block chain is ordered by <a href="a.html#address">address</a>.  This makes <a href="c.html#coalesce">coalescence</a> considerably cheaper, but <a href="#free-1">deallocation</a> more expensive.</p>

<p><strong>See also:</strong> <a href="#free.list">free list</a>.
</p></dd>

<dt><strong><a id="free.list" name="free.list">free list</a>, free-list</strong></dt>
<dd><p>The free list is the set of <a href="#free.block">free blocks</a>.</p>
<p>Originally this term meant the single linked list of all free blocks, but as <a href="a.html#allocation.mechanism">allocation mechanisms</a> have become more varied, it has become more generic, and now may be implemented as a tree or other data
structure rather than a linked list.  If the implementation actually is a linked list of free blocks, this is called a <a href="#free.block.chain">free block chain</a> to distinguish it from the abstract term.</p>

<p>There may be several free lists, classed by size or other characteristic.  For instance, <a href="s.html#segregated.free.list">segregated free list</a> systems classify free lists by block size.</p>

<p><strong>See also:</strong> <a href="#free.block">free block</a>;
    <a href="#free.block.chain">free block chain</a>.
</p></dd>

<dt><strong><a id="free.store" name="free.store">free store</a></strong>
  (for full details, see <a href="h.html#heap">heap</a>)</dt>
<dd><p>The <em>heap</em> or <em>free store</em> is the <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> area managed by <a href="d.html#dynamic.allocation">dynamic allocation</a>.</p></dd>

<dt><strong><a id="freestore" name="freestore">freestore</a></strong>
  (for full details, see <a href="h.html#heap">heap</a>)</dt>
<dd><p>The <em>heap</em> or <em>free store</em> is the <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> area managed by <a href="d.html#dynamic.allocation">dynamic allocation</a>.</p></dd>

<dt><strong><a id="function.record" name="function.record">function record</a></strong>
  (for full details, see <a href="a.html#activation.record">activation record</a>)</dt>
<dd><p>An activation or function record is a data structure, associated with the invocation of a function, procedure or control block that stores the variables, temporaries and fixed-sized data local to the block, and the information required to return to the invoking context.  It is often stored on a <a href="s.html#stack">stack</a>.</p></dd>

</dl>
<p align="center"><a href="a.html">A</a>
<a href="b.html">B</a>
<a href="c.html">C</a>
<a href="d.html">D</a>
<a href="e.html">E</a>
<strong>F</strong>
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