<?xml version="1.0" encoding="iso-8859-1"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
        "http://www.w3.org/TR/2000/REC-xhtml1-20000126/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
<link rev="MADE" href="mailto:mm-web@ravenbrook.com" />
<link rel="Start" href="../" />
<link rel="Contents" href="../" />
<link rel="Glossary" href="" />
<meta name="DC.Date" content="2000-12-15T20:16:54" />
<meta name="description" content="Glossary of terms related to memory management" />
<meta name="keywords" content="dictionary, glossary, definition, memory management, garbage collection" />
<link rel="index" href="./" />
<link rel="help" href="help.html" />
<link rel="next" href="b.html" />
<title>The Memory Management Glossary: A</title>
</head>
<body lang="en" bgcolor="#FFFFFF" text="#000000" link="#000099" vlink="#660066" alink="#FF0000">
<h1 align="center">The Memory Management Glossary<br />
<big>A</big></h1>
<div align="center">&#160;<a href=".././">Contents</a> |
<a href="../news/">News</a> |
<a href="./">Glossary</a> |
<a href="../faq.html">FAQ</a> |
<a href="../articles/">Articles</a> |
<a href="../bib/">Bibliography</a> |
<a href="../links.html">Links</a> |
<a href="../feedback.html">Feedback</a></div>
<hr size="1" noshade="noshade" />
<p align="center"><strong>A</strong>
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
<dt><strong><a id="absolute.address" name="absolute.address">absolute address</a></strong>
  (for full details, see <a href="p.html#physical.address">physical address</a>)</dt>
<dd><p>Physical <a href="#address">addresses</a> are used to index into <a href="p.html#physical.memory-1">physical memory<sup><small>(1)</small></sup></a>.  On some systems, they are called <em>absolute addresses</em>.</p></dd>

<dt><strong><a id="activation.frame" name="activation.frame">activation frame</a></strong>
  (for full details, see <a href="#activation.record">activation record</a>)</dt>
<dd><p>An activation or function record is a data structure, associated with the invocation of a function, procedure or control block that stores the variables, temporaries and fixed-sized data local to the block, and the information required to return to the invoking context.  It is often stored on a <a href="s.html#stack">stack</a>.</p></dd>

<dt><strong><a id="activation.record" name="activation.record">activation record</a></strong>
  (also known as function record, activation frame)</dt>
<dd><p>An activation or function record is a data structure, associated with the invocation of a function, procedure or control block that stores the variables, temporaries and fixed-sized data local to the block, and the information required to return to the invoking context.  It is often stored on a <a href="s.html#stack">stack</a>.</p>
<p>In a register-based hardware architecture, the current activation record is typically partially stored in registers.</p>

<p><a href="c.html#closure">Closures</a> and <a href="c.html#continuation">continuations</a> are specializations of activation records in support of particular language features of <a href="../articles/lang.html#lisp">LISP</a>, <a href="../articles/lang.html#scheme">Scheme</a> and related languages.</p>

<p><strong>Relevance to memory management:</strong> The current activation record is part of the state of the <a href="m.html#mutator">mutator</a>, and is therefore a <a href="r.html#root">root</a> to the <a href="c.html#collector-2">collector<sup><small>(2)</small></sup></a>.  In languages that permit recursion, activation records have <a href="d.html#dynamic.extent">dynamic extent</a>.  In languages that permit closures or continuations, activation records may have <a href="i.html#indefinite.extent">indefinite extent</a>.  Although they may not be visible to the programmer, their <a href="s.html#storage">storage</a> must be managed by the language run-time support.  Because they are usually not visible to the programmer, they may be a source of inexplicable memory overhead.</p>

<p><strong>See also:</strong> <a href="s.html#stack.frame">stack frame</a>.
</p></dd>

<dt><strong><a id="activation.stack" name="activation.stack">activation stack</a></strong>
  (for full details, see <a href="c.html#control.stack">control stack</a>)</dt>
<dd><p>A <a href="s.html#stack">stack</a> that stores <a href="#activation.record">activation records</a>, particularly subroutine return information, is known as a <em>control stack</em>.</p></dd>

<dt><strong><a id="active" name="active">active</a></strong>
  (for full details, see <a href="l.html#live">live</a>)</dt>
<dd><p><a href="m.html#memory-2">Memory<sup><small>(2)</small></sup></a> or an <a href="o.html#object">object</a> is live if the program will read from it in future.  The term is often used more broadly to mean <a href="r.html#reachable">reachable</a>.</p></dd>

<dt><strong><a id="address" name="address">address</a></strong></dt>
<dd><p>An address is a specification of a <a href="l.html#location">location</a> in an <a href="#address.space">address space</a>.</p>
<p>An address is almost always represented as an unsigned integer stored in a single <a href="m.html#machine.word">machine word</a>.  The address is decoded by the hardware in order to access a location on a <a href="p.html#physical.memory-2">physical memory<sup><small>(2)</small></sup></a> device (such as a <a href="r.html#ram">RAM</a>) or some <a href="m.html#memory.mapping">memory-mapped</a> resource.</p>

<p align="center"><em>A simplified view of addresses, address space, and locations on a 32-bit architecture</em><br /><img alt="Diagram: A simplified view of addresses, address space, and locations on a 32-bit architecture" src="../diagrams/address.png" border="2" height="340" width="250" /></p>

<p><strong>Similar terms:</strong> <a href="p.html#pointer">pointer</a>.
</p></dd>

<dt><strong><a id="address.space" name="address.space">address space</a></strong></dt>
<dd><p>An <em>address space</em> is the set of possible <a href="#address">addresses</a>.
It can also be considered to be a partial function from addresses to <a href="m.html#memory.location">locations</a>.</p>
<p>Typically, addresses start at zero and run to 2^N-1, where N is the address width (for example, 15, 16, 24, 32, 64), which is usually the same as the width of the address bus.  This may not be true for <a href="s.html#segmented.addressing">segmented</a> architectures.</p>

<p>In modern systems, large parts of the whole address space may be reserved by the operating system or architecture, or not <a href="m.html#mapped">mapped</a> at any given time.  The mapped part of the address space may be discontiguous or sparse.</p>

<p><strong>See also:</strong> <a href="v.html#virtual.address.space">virtual address space</a>;
    <a href="p.html#physical.address.space">physical address space</a>.
</p></dd>

<dt><strong><a id="address.translation.cache" name="address.translation.cache">address translation cache</a></strong>
  (for full details, see <a href="t.html#tlb">TLB</a>)</dt>
<dd><p>The <em>translation lookaside buffer</em> or <em>address translation cache</em> is small piece of associative <a href="m.html#memory-1">memory<sup><small>(1)</small></sup></a> within a processor which caches part of the translation from <a href="v.html#virtual.address">virtual addresses</a> to <a href="p.html#physical.address">physical addresses</a>.</p></dd>

<dt><strong><a id="address-ordered.first.fit" name="address-ordered.first.fit">address-ordered first fit</a></strong></dt>
<dd><p>The <a href="#allocation.policy">allocation policy</a> that always uses the suitable <a href="f.html#free.block">free block</a>
with the lowest address. One of the most common allocation policies in
use. Commonly implemented by <a href="f.html#first.fit">first fit</a> on a single address-ordered
<a href="f.html#free.block.chain">free block chain</a>. Sometimes just called "first fit".</p>
<p><strong>See also:</strong> <a href="f.html#first.fit">first fit</a>;
    <a href="l.html#lifo-ordered.first.fit">LIFO-ordered first fit</a>;
    <a href="#address-ordered.first.fit">address-ordered first fit</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil95">Paul R. Wilson, Mark S. Johnstone, Michael Neely, David Boles. 1995. <cite>Dynamic Storage Allocation: A Survey and Critical Review</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="aging.space" name="aging.space">aging space</a></strong></dt>
<dd><p>In some <a href="g.html#generational.garbage.collection">generational garbage collection</a> systems, when <a href="g.html#generation">generations</a> are divided into <a href="b.html#bucket">buckets</a>, the aging space is where <a href="o.html#object">objects</a> which survive a <a href="c.html#collection">collection</a> stay until they are old enough to be <a href="p.html#promotion">promoted</a>.</p>
<p><strong>Opposites:</strong> <a href="c.html#creation.space">creation space</a>.
</p></dd>

<dt><strong><a id="algebraic.data.type" name="algebraic.data.type">algebraic data type</a></strong></dt>
<dd><p>Algebraic data types aggregate or alternate a number of dissimilarly-typed objects.  They are termed algebraic because they can be expressed as a sum-of-products: (a and b and c) or d.</p>
<p>Examples of algebraic data types include: structures, records, tuples, and unions.</p>

<p><strong>Relevance to memory management:</strong> Algebraic data types are usually represented using a <a href="h.html#heap">heap</a>.  Because of their non-uniformity, algebraic data types are more difficult to <a href="s.html#scan">scan</a>.</p>

<p><strong>See also:</strong> <a href="s.html#scalar.data.type">scalar data type</a>;
    <a href="v.html#vector.data.type">vector data type</a>;
    <a href="h.html#heap">heap</a>.
</p></dd>

<dt><strong><a id="alignment" name="alignment">alignment</a></strong></dt>
<dd><p>Alignment is a constraint on the <a href="#address">address</a> of an <a href="o.html#object">object</a> in <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a>. </p>
<p>The constraint is usually that the object's address must be a multiple of a
power of two, 2^N, and therefore that the least significant N bits of
the address must be zero.</p>

<p>The bus hardware of many modern processors cannot access multi-<a href="b.html#byte-2">byte<sup><small>(2)</small></sup></a>
objects at any memory address. Often <a href="w.html#word">word</a>-sized objects must be aligned
to word boundaries, double-words to double-word boundaries,
double-floats to 8-byte boundaries, and so on.  If a program attempts to access
an object that is incorrectly aligned, a <a href="b.html#bus.error">bus error</a> occurs.</p>

<p><strong>Relevance to memory management:</strong> A memory manager must take care to <a href="#allocate">allocate</a> memory with an appropriate
alignment for the object that is going to be stored there.
Implementations of <code><a href="m.html#malloc">malloc</a></code> have to allocate all <a href="b.html#block">blocks</a> at the largest
alignment that the processor architecture requires.</p>

<p>Other reasons for aligning objects include using the least significant
bits of the address for a <a href="t.html#tag">tag</a>.</p>

<p><strong>Opposites:</strong> <a href="u.html#unaligned">unaligned</a>.
<br />
<strong>See also:</strong> <a href="n.html#natural.alignment">natural alignment</a>.
</p></dd>

<dt><strong><a id="alive" name="alive">alive</a></strong>
  (for full details, see <a href="l.html#live">live</a>)</dt>
<dd><p><a href="m.html#memory-2">Memory<sup><small>(2)</small></sup></a> or an <a href="o.html#object">object</a> is live if the program will read from it in future.  The term is often used more broadly to mean <a href="r.html#reachable">reachable</a>.</p></dd>

<dt><strong><a id="allocate" name="allocate">allocate</a></strong>
  (also known as cons(2))</dt>
<dd><p><i>Allocation</i> is the process of assigning resources.  When requested to by the program, an application <a href="m.html#memory.manager">memory manager</a> or <a href="#allocator">allocator</a> <i>allocates</i> a <a href="b.html#block">block</a> of <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> for the program to store its data in. Allocation is also known as <i>consing</i>, from <a href="c.html#cons-1">cons<sup><small>(1)</small></sup></a>.</p>
<p>When faced with a request for memory from the program, a memory manager must choose a suitable block and hand it over, or fail.  The choices made by the memory manager at this point can have a significant effect on the future efficiency of the program.</p>

<p>Allocation is rarely a simple issue.  For example, programs usually allocate <a href="#activation.record">activation records</a> (<a href="#automatic.storage.duration">automatic variables</a>, and so on) for functions from a processor <a href="s.html#stack">stack</a> simply by subtracting a number from their stack <a href="p.html#pointer">pointer</a>.  However, in a <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> system, this may extend the stack onto a previously unused <a href="p.html#page">page</a>, in which case the operating system memory manager must carry out some quite complex operations in order to supply the program with <a href="b.html#backing.store">backing store</a> for the stack so that the program can continue.</p>

<p><strong>Historical note:</strong> The term <i>reserved</i> was often used to mean "allocated".</p>

<p><strong>Similar terms:</strong> <a href="m.html#malloc">malloc</a>.
<br />
<strong>Opposites:</strong> <a href="f.html#free-1">free<sup><small>(1)</small></sup></a>.
<br />
<strong>See also:</strong> <a href="c.html#constructor-1">constructor<sup><small>(1)</small></sup></a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil95">Paul R. Wilson, Mark S. Johnstone, Michael Neely, David Boles. 1995. <cite>Dynamic Storage Allocation: A Survey and Critical Review</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="allocation.mechanism" name="allocation.mechanism">allocation mechanism</a></strong></dt>
<dd><p>The algorithm by which an <a href="#allocator">allocator</a> chooses a <a href="f.html#free.block">free block</a> from which to satisfy an allocation request.  An allocation mechanism is the implementation of an <a href="#allocation.policy">allocation policy</a>.</p>
<p>A common mechanism is "<a href="f.html#first.fit">first fit</a> on an address-ordered <a href="f.html#free.block.chain">free block chain</a>, with eager <a href="c.html#coalesce">coalescing</a>". This implements the <a href="#address-ordered.first.fit">address-ordered first fit</a> policy, and commonly inherits that name, although there are many other mechanisms for implementing that policy (e.g. "leftmost fit on a balanced tree of free blocks ordered by address").</p>

<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil95">Paul R. Wilson, Mark S. Johnstone, Michael Neely, David Boles. 1995. <cite>Dynamic Storage Allocation: A Survey and Critical Review</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="allocation.policy" name="allocation.policy">allocation policy</a></strong>
  (also known as placement policy)</dt>
<dd><p>The concrete policy used by an <a href="#allocator">allocator</a> for choosing a <a href="f.html#free.block">free block</a> to satisfy an <a href="#allocate">allocation</a> request.</p>
<p>For instance, "always allocate from the largest free block" (<a href="w.html#worst.fit">worst fit</a>) or "use the most recently freed block suitable" (<a href="l.html#lifo-ordered.first.fit">LIFO-ordered first fit</a>).</p>

<p>Each allocation policy is motivated by an <a href="#allocation.strategy">allocation strategy</a> and implemented by an <a href="#allocation.mechanism">allocation mechanism</a>.</p>

<p><strong>See also:</strong> <a href="#address-ordered.first.fit">address-ordered first fit</a>;
    <a href="b.html#best.fit">best fit</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil95">Paul R. Wilson, Mark S. Johnstone, Michael Neely, David Boles. 1995. <cite>Dynamic Storage Allocation: A Survey and Critical Review</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="allocation.strategy" name="allocation.strategy">allocation strategy</a></strong></dt>
<dd><p>The high-level design motivation or strategy, of an <a href="#allocator">allocator</a>, which uses observations or theories about patterns of allocation requests to justify an <a href="#allocation.policy">allocation policy</a>.</p>
<p>For instance, "do not allow small long-lived <a href="o.html#object">objects</a> to fragment large <a href="f.html#free-3">free<sup><small>(3)</small></sup></a> areas", "allocate consecutive objects close together", and so on.  The allocation strategy motivates an <a href="#allocation.policy">allocation policy</a>, which is implemented by an <a href="#allocation.mechanism">allocation mechanism</a>.</p>

<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil95">Paul R. Wilson, Mark S. Johnstone, Michael Neely, David Boles. 1995. <cite>Dynamic Storage Allocation: A Survey and Critical Review</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="allocator" name="allocator">allocator</a></strong></dt>
<dd><p>The term <i>allocator</i> is often used to refer to the <a href="m.html#memory.manager">memory manager</a>, usually when it is a simple manual one.</p>
<p><strong>Similar terms:</strong> <a href="m.html#memory.manager">memory manager</a>.
<br />
<strong>See also:</strong> <a href="#allocate">allocation</a>.
</p></dd>

<dt><strong><a id="ambiguous.reference" name="ambiguous.reference">ambiguous reference</a></strong>
  (also known as unsure reference)</dt>
<dd><p>An ambiguous or unsure <a href="r.html#reference">reference</a> is a value that is potentially a reference, but the <a href="c.html#collector-1">collector<sup><small>(1)</small></sup></a> cannot prove that it is.</p>
<p>The presence of ambiguous references in a <a href="g.html#garbage.collection">garbage-collected</a> system requires the use of <a href="c.html#conservative.garbage.collection">conservative garbage collection</a>.</p>

<p><strong>Opposites:</strong> <a href="e.html#exact.reference">exact reference</a>.
<br />
<strong>See also:</strong> <a href="f.html#floating.garbage">floating garbage</a>.
</p></dd>

<dt><strong><a id="ambiguous.root" name="ambiguous.root">ambiguous root</a></strong></dt>
<dd><p>An ambiguous root is a <a href="r.html#root">root</a> containing <a href="#ambiguous.reference">ambiguous references</a>.</p>
<p><strong>Opposites:</strong> <a href="e.html#exact.root">exact root</a>.
<br />
<strong>See also:</strong> <a href="c.html#conservative.garbage.collection">conservative garbage collection</a>.
</p></dd>

<dt><strong><a id="arena" name="arena">arena</a></strong></dt>
<dd><p>The area of <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> used by <code><a href="m.html#malloc">malloc</a></code> for allocation.</p>
<p>So named from a semi-mythical "<samp>malloc: corrupted arena</samp>" message supposedly emitted when some early versions became terminally confused.</p>

<p><strong>See also:</strong> <a href="b.html#brk">brk</a>.
</p></dd>

<dt><strong><a id="atc" name="atc">ATC</a></strong>
  (for full details, see <a href="t.html#tlb">TLB</a>)</dt>
<dd><p>The <em>translation lookaside buffer</em> or <em>address translation cache</em> is small piece of associative <a href="m.html#memory-1">memory<sup><small>(1)</small></sup></a> within a processor which caches part of the translation from <a href="v.html#virtual.address">virtual addresses</a> to <a href="p.html#physical.address">physical addresses</a>.</p></dd>

<dt><strong><a id="atomic.object" name="atomic.object">atomic object</a></strong>
  (for full details, see <a href="l.html#leaf.object">leaf object</a>)</dt>
<dd><p>A leaf object is an <a href="o.html#object">object</a> that does not <a href="r.html#reference">reference</a> any other objects.</p></dd>

<dt><strong><a id="automatic.memory.management" name="automatic.memory.management">automatic memory management</a></strong></dt>
<dd><p>Automatic <a href="m.html#memory.management">memory management</a> is a general term for techniques that automatically <a href="r.html#recycle">recycle</a> unused <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a>.</p>
<p>It is not possible, in general, to automatically determine which <a href="o.html#object">objects</a> are still <a href="l.html#live">live</a>.    Even if it didn't depend on future input, there can be no general algorithm to prove that an object is live (cf. the Halting Problem).  However, effective approximations are possible.
In <a href="t.html#tracing.garbage.collection">tracing garbage collection</a>, the approximation is that an object can't be live unless it is <a href="r.html#reachable">reachable</a>.  In <a href="r.html#reference.counting">reference counting</a>, the approximation is that an object can't be live unless it is <a href="r.html#reference">referenced</a>.  Analysis of the program text can reveal where objects <a href="d.html#dead">die</a>; A notable technique in this vein is <a href="r.html#region.inference">region inference</a>.
Hybrid algorithms are also possible.</p>

<p><strong>Similar terms:</strong> <a href="g.html#garbage.collection">garbage collection</a>.
<br />
<strong>Opposites:</strong> <a href="m.html#manual.memory.management">manual memory management</a>.
</p></dd>

<dt><strong><a id="automatic.storage.duration" name="automatic.storage.duration">automatic storage duration</a></strong></dt>
<dd><p>In <a href="../articles/lang.html#c">C</a>, <a href="o.html#object">objects</a> that are declared with <em>automatic storage duration</em> are <a href="l.html#live">live</a> for the duration of a block of code.</p>
<p>In most implementations of C, objects with automatic storage duration are <a href="#allocate">allocated</a> on the <a href="s.html#stack">stack</a> when a function is entered, and <a href="f.html#free-1">deallocated</a> when it returns.</p>

<p><strong>Similar terms:</strong> <a href="s.html#stack.allocation">stack allocation</a>;
    <a href="d.html#dynamic.extent">dynamic extent</a>.
<br />
<strong>Opposites:</strong> <a href="s.html#static.storage.duration">static storage duration</a>.
</p></dd>

</dl>
<p align="center"><strong>A</strong>
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
<a href="s.html">S</a>
<a href="t.html">T</a>
<a href="u.html">U</a>
<a href="v.html">V</a>
<a href="w.html">W</a>
X
Y
<a href="z.html">Z</a> - <a href="help.html">Help</a></p>

</body></html>