<?xml version="1.0" encoding="iso-8859-1"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
        "http://www.w3.org/TR/2000/REC-xhtml1-20000126/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
<link rev="MADE" href="mailto:mm-web@ravenbrook.com" />
<link rel="Start" href="../" />
<link rel="Contents" href="../" />
<link rel="Glossary" href="" />
<meta name="DC.Date" content="2000-12-15T20:16:55" />
<meta name="description" content="Glossary of terms related to memory management" />
<meta name="keywords" content="dictionary, glossary, definition, memory management, garbage collection" />
<link rel="index" href="./" />
<link rel="help" href="help.html" />
<link rel="prev" href="a.html" />
<link rel="next" href="c.html" />
<title>The Memory Management Glossary: B</title>
</head>
<body lang="en" bgcolor="#FFFFFF" text="#000000" link="#000099" vlink="#660066" alink="#FF0000">
<h1 align="center">The Memory Management Glossary<br />
<big>B</big></h1>
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
<strong>B</strong>
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
<dt><strong><a id="backing.store" name="backing.store">backing store</a></strong></dt>
<dd><p>Backing <a href="s.html#store-2">store<sup><small>(2)</small></sup></a> is typically part of a hard disk that is used by a <a href="p.html#paging">paging</a> or <a href="s.html#swapping">swapping</a> system to store information not currently in <a href="m.html#main.memory">main memory</a>.  Backing store is slower and cheaper than main memory.</p>
<p>Other <a href="s.html#storage">storage</a> may, less commonly, be used in place of a hard disk (for instance, magnetic tape, floppy disk, or historically, magnetic drum).</p>

<p>In general, backing store may mean any locations used to store information when its preferred or natural location is otherwise being used -- for instance, memory used by a graphical interface to keep a copy of the contents of obscured windows.</p>

<p><strong>Similar terms:</strong> <a href="s.html#swap.space">swap space</a>.
</p></dd>

<dt><strong><a id="barrier-1" name="barrier-1">barrier<sup><small>(1)</small></sup></a></strong></dt>
<dd><p>A barrier is a block on reading from or writing to certain <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> <a href="m.html#memory.location">locations</a> by certain threads or processes.</p>
<p>Barriers can be implemented in either software or hardware.  Software barriers involve additional instructions around <a href="l.html#load">load</a> or <a href="s.html#store-1">store<sup><small>(1)</small></sup></a> operations, which would typically be added by a cooperative compiler.  Hardware barriers don't require compiler support, and may be implemented on common operating systems by using <a href="m.html#memory.protection">memory protection</a>.</p>

<p><strong>Relevance to memory management:</strong> Barriers are used for <a href="i.html#incremental.garbage.collection">incremental</a> or <a href="p.html#parallel.garbage.collection">concurrent</a> <a href="g.html#garbage.collection">garbage collection</a>.</p>

<p><strong>See also:</strong> <a href="r.html#read.barrier">read barrier</a>;
    <a href="w.html#write.barrier">write barrier</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#zorn90">Benjamin Zorn. 1990. <cite>Barrier Methods for Garbage Collection</cite>.</a></li>
  <li><a href="../bib/f.html#pirinen98">Pekka P. Pirinen. 1998. <cite>Barrier techniques for incremental tracing</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="barrier-2" name="barrier-2">barrier<sup><small>(2)</small></sup></a></strong></dt>
<dd><p>A memory barrier is an instruction on certain processor architectures that will ensure certain guarantees about the order of accesses to memory.</p>
<p>Some processor architectures, such as the Alpha AXP, make very few guarantees about the relative orders of <a href="l.html#load">load</a> and <a href="s.html#store-1">store<sup><small>(1)</small></sup></a> operations in the instruction stream and the actual order of accesses to <a href="m.html#main.memory">main memory</a>.  These architectures will often have special instructions that make stronger guarantees.</p>

<p>For example Alpha AXP has the MB instruction which will: </p>

<blockquote>Guarantee that all subsequent loads or stores will not access memory until after all previous loads and stores have accessed memory, as observed by other processors.</blockquote>

<p>These instructions are vital for certain synchronization operations.</p></dd>


<dt><strong><a id="base.pointer" name="base.pointer">base pointer</a></strong></dt>
<dd><p>A <em>base pointer</em> is a <a href="p.html#pointer">pointer</a> to the base or start of an <a href="o.html#object">object</a>.</p>
<p>This term is commonly used in opposition to <a href="d.html#derived.pointer">derived pointer</a>.</p>

<p>Note that in <a href="../bib/f.html#bc92a"><cite>A Proposal for Garbage-Collector-Safe C Compilation</cite></a>, Boehm and Chase define "base pointer" to be "any pointer value directly recognizable by the <a href="c.html#collector-1">collector<sup><small>(1)</small></sup></a>", and this may well include <a href="i.html#interior.pointer">interior pointers</a>.</p>

<p><strong>Opposites:</strong> <a href="d.html#derived.pointer">derived pointer</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#bc92a">Hans-J. Boehm, David Chase. 1992. <cite>A Proposal for Garbage-Collector-Safe C Compilation</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="best.fit" name="best.fit">best fit</a></strong></dt>
<dd><p>
The <a href="a.html#allocation.policy">allocation policy</a> that always allocates from the smallest
suitable <a href="f.html#free.block">free block</a>. Suitable <a href="a.html#allocation.mechanism">allocation mechanisms</a> include
<a href="s.html#sequential.fit">sequential fit</a> searching for a <a href="p.html#perfect.fit">perfect fit</a>, <a href="f.html#first.fit">first fit</a> on a
size-ordered <a href="f.html#free.block.chain">free block chain</a>, <a href="s.html#segregated.fit">segregated fits</a>, and <a href="i.html#indexed.fit">indexed fits</a>. Many <a href="g.html#good.fit">good fit</a> allocators are also described as <a href="#best.fit">best fit</a>.</p>

<p>In theory, best fit may exhibit bad <a href="f.html#fragmentation">fragmentation</a>, but in practice
this is not commonly observed.
</p>
<p><strong>See also:</strong> <a href="a.html#allocation.policy">allocation policy</a>;
    <a href="f.html#first.fit">first fit</a>;
    <a href="s.html#sequential.fit">sequential fit</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil95">Paul R. Wilson, Mark S. Johnstone, Michael Neely, David Boles. 1995. <cite>Dynamic Storage Allocation: A Survey and Critical Review</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="bibop" name="bibop">BIBOP</a></strong>
  (also known as big bag of pages)</dt>
<dd><p>BIBOP, or <em>BIg Bag Of Pages</em>, is a technique that encodes <a href="o.html#object">object</a> type in the high-order bits of their <a href="a.html#address">address</a>, by using a lookup table that maps from those bits to a type.</p>
<p>Despite the name, the blocks involved need not be the size of a <a href="p.html#page">page</a>.</p>

<p>BIBOP requires storing only objects of the same type in a block, but this has the same advantages as <a href="s.html#segregated.fit">segregated fits</a> in general.</p>

<p><strong>Historical note:</strong> This technique was invented for the PDP-10 MACLISP by JonL White and Stavros Macrakis.  It was an advance on earlier techniques that divided the <a href="a.html#address.space">address space</a> into contiguous blocks for each type.</p>

<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#baker79">Henry G. Baker. 1979. <cite>Optimizing Allocation and Garbage Collection of Spaces</cite>.</a></li>
  <li><a href="../bib/f.html#steele77">Guy L. Steele. 1977. <cite>Data Representation in PDP-10 MACLISP</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="big.bag.of.pages" name="big.bag.of.pages">big bag of pages</a></strong>
  (for full details, see <a href="#bibop">BIBOP</a>)</dt>
<dd><p>BIBOP, or <em>BIg Bag Of Pages</em>, is a technique that encodes <a href="o.html#object">object</a> type in the high-order bits of their <a href="a.html#address">address</a>, by using a lookup table that maps from those bits to a type.</p></dd>

<dt><strong><a id="binary.buddies" name="binary.buddies">binary buddies</a></strong></dt>
<dd><p>The most common <a href="#buddy.system">buddy system</a> <a href="a.html#allocation.mechanism">allocation mechanism</a>, in which all
block sizes are a power of two. Finding a block's buddy is then a
matter of flipping the appropriate bit in the block's address.</p>

<p><a href="i.html#internal.fragmentation">Internal fragmentation</a> is usually high, because objects are often
not a good fit for power-of-two sized blocks.</p>
<p><strong>See also:</strong> <a href="#buddy.system">buddy system</a>;
    <a href="a.html#allocation.mechanism">allocation mechanism</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil95">Paul R. Wilson, Mark S. Johnstone, Michael Neely, David Boles. 1995. <cite>Dynamic Storage Allocation: A Survey and Critical Review</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="bit-table" name="bit-table">bit-table</a></strong>
  (for full details, see <a href="#bitmap">bitmap</a>)</dt>
<dd><p>A table of bits.</p></dd>

<dt><strong><a id="bitmap" name="bitmap">bitmap</a></strong>
  (also known as bit-table)</dt>
<dd><p>A table of bits.</p>
<p><strong>Relevance to memory management:</strong> Bitmaps are sometimes used to represent the marks in a <a href="m.html#mark-sweep">mark-sweep</a> collector, or the used memory in a <a href="#bitmapped.fit">bitmapped fits</a> <a href="a.html#allocator">allocator</a>.</p></dd>


<dt><strong><a id="bitmapped.fit" name="bitmapped.fit">bitmapped fit</a></strong></dt>
<dd><p>A class of <a href="a.html#allocation.mechanism">allocation mechanisms</a> that use a <a href="#bitmap">bitmap</a> to represent
the usage of the <a href="h.html#heap">heap</a>. Each bit in the map corresponds to a
part of the heap, typically a <a href="w.html#word">word</a>, and is set if that
part is in use. Allocation is done by searching the bitmap
for a run of clear bits.</p>

<p>Bitmapped fit mechanisms have good <a href="l.html#locality.of.reference">locality</a>, as they avoid examining <a href="i.html#in-band.header">in-band headers</a> when allocating.
</p>
<p><strong>See also:</strong> <a href="a.html#allocation.mechanism">allocation mechanism</a>;
    <a href="s.html#sequential.fit">sequential fit</a>;
    <a href="i.html#indexed.fit">indexed fit</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil95">Paul R. Wilson, Mark S. Johnstone, Michael Neely, David Boles. 1995. <cite>Dynamic Storage Allocation: A Survey and Critical Review</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="black" name="black">black</a></strong></dt>
<dd><p>In a <a href="t.html#tri-color.marking">tri-color marking</a> scheme, black <a href="o.html#object">objects</a> are objects that have been <a href="s.html#scan">scanned</a>.</p>
<p>More precisely,  black objects have been noted <a href="r.html#reachable">reachable</a> and the <a href="c.html#collector-2">collector<sup><small>(2)</small></sup></a> has finished with them and need not visit them again (for the purposes of <a href="t.html#trace">tracing</a>).</p>

<p><strong>Opposites:</strong> <a href="w.html#white">white</a>;
    <a href="g.html#gray">gray</a>.
</p></dd>

<dt><strong><a id="blacklisting" name="blacklisting">blacklisting</a>, black-listing</strong></dt>
<dd><p>A <a href="c.html#conservative.garbage.collection">conservative garbage collector</a> can be made more effective by <em>blacklisting</em> values which resemble <a href="a.html#address">addresses</a> that may be <a href="a.html#allocate">allocated</a> at in the future, but are known not to be <a href="p.html#pointer">pointers</a> .  This list is then used to avoid allocation at those addresses.</p>
<p>For example, such values can be gathered by scanning the <a href="r.html#root">roots</a> before any <a href="o.html#object">objects</a> have been allocated.</p>

<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#boehm93">Hans-J. Boehm. 1993. <cite>Space Efficient Conservative Garbage Collection</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="block" name="block">block</a></strong></dt>
<dd><p>Block is a vague term for an (often contiguous) area of <a href="m.html#memory-1">memory<sup><small>(1)</small></sup></a>. Often used to describe <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> <a href="a.html#allocate">allocated</a> by an <a href="a.html#allocator">allocator</a> such as <code><a href="m.html#malloc">malloc</a></code>.</p></dd>

<dt><strong><a id="bounds.error" name="bounds.error">bounds error</a></strong>
  (for full details, see <a href="o.html#overwriting.error">overwriting error</a>)</dt>
<dd><p>An overwriting or bounds error occurs when the programmer intends his program to write to a particular <a href="#block">block</a> of <a href="m.html#memory-1">memory<sup><small>(1)</small></sup></a>, but a program error causes the program to write outside the bounds of that block.</p></dd>

<dt><strong><a id="boxed" name="boxed">boxed</a></strong></dt>
<dd><p>Boxed <a href="o.html#object">objects</a> are represented by a <a href="p.html#pointer">pointer</a> to a <a href="#block">block</a> of <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> that contains the object data.  Sometimes the pointer is <a href="t.html#tag">tagged</a> to distinguish it from an <a href="u.html#unboxed">unboxed</a> object, or to represent its type.  Only the pointer is duplicated when the object is passed around, so updates to the object are reflected everywhere.</p>
<p><strong>Opposites:</strong> <a href="u.html#unboxed">unboxed</a>.
<br />
<strong>See also:</strong> <a href="t.html#tag">tag</a>;
    <a href="#bibop">BIBOP</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#gudeman93">David Gudeman. 1993. <cite>Representing Type Information in Dynamically Typed Languages</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="break-table" name="break-table">break-table</a></strong></dt>
<dd><p>A break-table is a data structure used by a <a href="m.html#mark-compact">mark-compact</a> collector to store the <a href="r.html#relocation">relocation</a> information.</p>
<p><strong>See also:</strong> <a href="m.html#mark-compact">mark-compact</a>.
</p></dd>

<dt><strong><a id="brk" name="brk">brk</a></strong></dt>
<dd><p><code>brk</code> is a UNIX&reg; system call that sets the limit of the data segment.  This limit is known as the <em>break</em>.</p>
<p>The <a href="../articles/lang.html#c">C</a> library implementation of <code><a href="m.html#malloc">malloc</a></code> usually <a href="a.html#allocate">allocates</a> <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> for the <a href="h.html#heap">heap</a> by extending the data segment with <code>brk</code> or <code><a href="s.html#sbrk">sbrk</a></code>.</p>

<p>Unfortunately, most implementations of <code>malloc</code> never shrink the data segment, so the memory usage of a process never decreases.  In most UNIX systems, the data segment resides immediately above the program code (text segment) in the <a href="a.html#address.space">address space</a>.</p>

<p align="center"><em>A simplified view of the address space of a UNIX process</em><br /><img alt="Diagram: A simplified view of the address space of a UNIX process" src="../diagrams/brk.png" border="2" height="304" width="260" /></p></dd>


<dt><strong><a id="broken.heart" name="broken.heart">broken heart</a></strong></dt>
<dd><p><a href="c.html#copying.garbage.collection">Copying garbage collectors</a> <a href="m.html#moving.garbage.collector">move</a> <a href="r.html#reachable">reachable</a> <a href="o.html#object">objects</a> into another <a href="s.html#semi-space">semi-space</a>.  They leave a <a href="f.html#forwarding.pointer">forwarding pointer</a> in the old <a href="l.html#location">location</a>, pointing to the new.  The object at the old location is known as a broken heart.</p>
<p><strong>Similar terms:</strong> <a href="f.html#forwarding.pointer">forwarding pointer</a>.
</p></dd>

<dt><strong><a id="bucket" name="bucket">bucket</a></strong></dt>
<dd><p>In a <a href="g.html#generational.garbage.collection">generational garbage collector</a>, it is often desirable to divide <a href="g.html#generation">generations</a> by the age of the <a href="o.html#object">object</a>.  These divisions are known as buckets.</p>
<p><strong>See also:</strong> <a href="g.html#generational.garbage.collection">generational garbage collection</a>;
    <a href="a.html#aging.space">aging space</a>;
    <a href="c.html#creation.space">creation space</a>.
</p></dd>

<dt><strong><a id="buddy.system" name="buddy.system">buddy system</a></strong></dt>
<dd><p>Buddy systems are a subclass of <a href="s.html#strict.segregated.fit">strict segregated fit</a> <a href="a.html#allocation.mechanism">allocation mechanisms</a> which make <a href="s.html#split">splitting</a> and <a href="c.html#coalesce">coalescing</a> fast by pairing
each block with a unique adjacent <em>buddy</em> block.
</p>
<p>There is an array of <a href="f.html#free.list">free lists</a>, one for each allowable block
size. Allocation rounds up the requested size to an allowable size and
allocates from the corresponding free list. If the free list is empty,
a larger block is selected and split. A block may only be split into a
pair of buddies.</p>

<p>A block may only be coalesced with its buddy, and this is only
possible if the buddy has not been split into smaller blocks.</p>

<p>The advantage of buddy systems is that the buddy of a block being
freed can be quickly found by a simple address computation. The
disadvantage of buddy systems is that the restricted set of block
sizes leads to high <a href="i.html#internal.fragmentation">internal fragmentation</a>, as does the limited
ability to coalesce.</p>

<p>Different sorts of buddy system are distinguished by the available
block sizes and the method of splitting. They include <a href="#binary.buddies">binary buddies</a>
(the most common), <a href="f.html#fibonacci.buddies">Fibonacci buddies</a>, <a href="w.html#weighted.buddies">weighted buddies</a>, and
<a href="d.html#double.buddies">double buddies</a>.</p>

<p><strong>See also:</strong> <a href="a.html#allocation.mechanism">allocation mechanism</a>;
    <a href="s.html#segregated.free.list">segregated free lists</a>;
    <a href="s.html#segregated.fit">segregated fit</a>;
    <a href="s.html#strict.segregated.fit">strict segregated fit</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil95">Paul R. Wilson, Mark S. Johnstone, Michael Neely, David Boles. 1995. <cite>Dynamic Storage Allocation: A Survey and Critical Review</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="buffer" name="buffer">buffer</a></strong></dt>
<dd><p>A <em>buffer</em> is a large <a href="#block">block</a> of <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> from which blocks are <a href="a.html#allocate">allocated</a> contiguously, as a simple technique for fast <a href="a.html#allocate">allocation</a>.</p>
<p>By keeping only a <em>high-water</em> mark (that is, a <a href="p.html#pointer">pointer</a> to the start of unused memory), the buffer technique avoids expensive <a href="i.html#in-band.header">in-band headers</a> and the searching of <a href="f.html#free.block.chain">free block chains</a>. Buffers tend to, however, lead to <a href="e.html#external.fragmentation">external fragmentation</a>.</p>

<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#ael88">Andrew Appel, John R. Ellis, Kai Li. 1988. <cite>Real-time Concurrent Collection on Stock Multiprocessors</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="bus.error" name="bus.error">bus error</a></strong></dt>
<dd><p>Strictly speaking, <em>a bus error</em> is a fault on a hardware bus, such as when an invalid <a href="a.html#address">address</a> is issued.</p>
<p>Generally, any hardware exception caused by a <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> access (for example, <a href="l.html#load">loading</a> an <a href="u.html#unaligned">unaligned</a> <a href="w.html#word">word</a>) is termed a <em>bus error</em>.  The term is often used more loosely as a synonym for any memory access error.</p>

<p><strong>See also:</strong> <a href="s.html#segmentation.violation">segmentation violation</a>.
</p></dd>

<dt><strong><a id="byte-1" name="byte-1">byte<sup><small>(1)</small></sup></a></strong></dt>
<dd><p>A unit of storage measurement, equal to 8 bits.</p>
<p>It does not matter how the bits are arranged -- it is just a quantity.</p>

<p>This is the sense of byte used in the terms <a href="k.html#kilobyte">kilobyte</a>, <a href="m.html#megabyte">megabyte</a>, <a href="g.html#gigabyte">gigabyte</a>, <a href="t.html#terabyte">terabyte</a>, etc.  The prefixes in these terms derive from the SI prefixes for powers of 1000, but since powers of two are much more common in binary computers, they are used to denote powers of 1024 (= 2^10).</p>

<p><strong>See also:</strong> <a href="w.html#word">word</a>.
</p></dd>

<dt><strong><a id="byte-2" name="byte-2">byte<sup><small>(2)</small></sup></a></strong></dt>
<dd><p>A data type defined by a processor architecture.</p>
<p>For example, the smallest <a href="a.html#address">addressable</a> storage <a href="l.html#location">location</a> on the Intel x86
family is the 8-bit byte.</p>

<p>The PDP-10 has 36-bit <a href="w.html#word">words</a>, and defines "byte" to be a general sub-<a href="w.html#word">word</a>
bit-field.  (Compare this with <a href="#byte-3">byte<sup><small>(3)</small></sup></a>.)  On this machine it is commonplace
for characters to be packed four or five to a word using 9- or 7-bit bytes
respectively.</p>

<p><strong>See also:</strong> <a href="w.html#word">word</a>.
</p></dd>

<dt><strong><a id="byte-3" name="byte-3">byte<sup><small>(3)</small></sup></a></strong></dt>
<dd><p>A contiguous set of bits used to represent a range of values compactly.</p>
<p>The number of bits in a byte is a measure of the information content of the
byte.  An n-bit byte can represent 2^<sup>n</sup> distinct values.</p>

<p>Bytes may be packed into (or otherwise stored in bit-fields of) integers,
words, or other aligned values for space efficiency.</p></dd>


<dt><strong><a id="byte-4" name="byte-4">byte<sup><small>(4)</small></sup></a></strong></dt>
<dd><p>A data type or storage unit defined by a programming language.</p>
<p>In ANSI/ISO <a href="../articles/lang.html#c">C</a>, "the unit of data storage large enough to hold the basic character set of the execution environment".  In this sense, it is often used synonymously with the C type <code>char</code>.  C defines <code>sizeof(char)</code> to be 1.  Many architectures that run C programs equate this sense of byte and <a href="#byte-2">byte<sup><small>(2)</small></sup></a>.</p></dd>


</dl>
<p align="center"><a href="a.html">A</a>
<strong>B</strong>
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
<a href="s.html">S</a>
<a href="t.html">T</a>
<a href="u.html">U</a>
<a href="v.html">V</a>
<a href="w.html">W</a>
X
Y
<a href="z.html">Z</a> - <a href="help.html">Help</a></p>

</body></html>