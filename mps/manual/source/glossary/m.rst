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
<link rel="prev" href="l.html" />
<link rel="next" href="n.html" />
<title>The Memory Management Glossary: M</title>
</head>
<body lang="en" bgcolor="#FFFFFF" text="#000000" link="#000099" vlink="#660066" alink="#FF0000">
<h1 align="center">The Memory Management Glossary<br />
<big>M</big></h1>
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
<strong>M</strong>
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
<dt><strong><a id="machine.word" name="machine.word">machine word</a></strong>
  (for full details, see <a href="w.html#word">word</a>)</dt>
<dd><p>Almost all processor architectures have a characteristic data size that is handled most efficiently.  This is known as the <em>word size</em>, and data of that size are known as <em>words</em>.  The word size is usually a power of two multiple of <a href="b.html#byte-2">bytes<sup><small>(2)</small></sup></a>.</p></dd>

<dt><strong><a id="main.memory" name="main.memory">main memory</a></strong>
  (also known as memory(3), primary storage)</dt>
<dd><p>The <em>main memory</em> (or <em>primary storage</em>) of a computer is <a href="#memory-1">memory<sup><small>(1)</small></sup></a> that is wired directly to the processor, consisting of <a href="r.html#ram">RAM</a> and possibly <a href="r.html#rom">ROM</a>.</p>
<p>These terms are used in contrast to mass storage devices and <a href="c.html#cache.memory">cache memory</a> (although we may note that when a program accesses main memory, it is often actually interacting with a cache).</p>

<p>Main memory is the middle level of the <a href="#memory.hierarchy">memory hierarchy</a>: it is slower and cheaper than <a href="c.html#cache-1">caches<sup><small>(1)</small></sup></a>, but faster and more expensive than <a href="b.html#backing.store">backing store</a>.</p>

<p>It is common to refer only to the main memory of a computer; for example, "This box has 16 MB of memory" and "Word for Windows&reg; requires 32 MB".</p>

<p><strong>Historical note:</strong> Main memory used to be called <a href="c.html#core">core</a>, and is now likewise often called <a href="r.html#ram">RAM</a>.</p>

<p><strong>Similar terms:</strong> <a href="r.html#ram">RAM</a>;
    <a href="c.html#core">core</a>;
    <a href="p.html#physical.memory-1">physical memory<sup><small>(1)</small></sup></a>.
</p></dd>

<dt><strong><a id="malloc" name="malloc">malloc</a></strong></dt>
<dd><p>A function in the standard <a href="../articles/lang.html#c">C</a> library that performs <a href="d.html#dynamic.allocation">dynamic allocation</a> of <a href="#memory-2">memory<sup><small>(2)</small></sup></a>.</p>
<p>Many people use "malloc" as a verb to mean "allocate dynamically".</p>

<p><strong>Similar terms:</strong> <a href="a.html#allocate">allocate</a>.
<br />
<strong>Opposites:</strong> <a href="f.html#free-2">free<sup><small>(2)</small></sup></a>.
</p></dd>

<dt><strong><a id="manual.memory.management" name="manual.memory.management">manual memory management</a></strong></dt>
<dd><p>In some systems or languages, it is up to the application program to manage all the bookkeeping details of <a href="a.html#allocate">allocating</a> <a href="#memory-2">memory<sup><small>(2)</small></sup></a> from the <a href="h.html#heap">heap</a> and <a href="f.html#free-1">freeing</a> it when no longer required; this is known as manual <a href="#memory.management">memory management</a>.</p>
<p>Manual memory management may be appropriate for small programs, but it does not scale well in general, nor does it encourage modular or object-oriented programming.</p>

<p>To quote Ian Joyner's <cite>C++?? : A Critique of C++</cite>:</p>

<blockquote cite="http://www.elj.com/cppcv3/s3/#s03-36">This is the most difficult bookkeeping task C++
programmers face, that leads to two opposite problems: firstly, an
object can be <a href="f.html#free-1">deallocated</a> prematurely, while valid <a href="r.html#reference">references</a> still
exist (<a href="d.html#dangling.pointer">dangling pointers</a>); secondly, <a href="d.html#dead">dead</a> objects might not be
deallocated, leading to memory filling up with dead objects (<a href="#memory.leak">memory leaks</a>).  Attempts to correct either problem can lead to
overcompensation and the opposite problem occurring.  A correct system
is a fine balance.</blockquote>

<p><strong>Historical note:</strong> Manual memory management was common in early languages, but <a href="g.html#garbage.collection">garbage collection</a> has been around since the late 1950s, in languages like <a href="../articles/lang.html#lisp">Lisp</a>.  Most modern languages use <a href="a.html#automatic.memory.management">automatic memory management</a>, and some older languages have <a href="c.html#conservative.garbage.collection">conservative garbage collection</a> extensions.</p>

<p><strong>Opposites:</strong> <a href="a.html#automatic.memory.management">automatic memory management</a>.
</p></dd>

<dt><strong><a id="mapped" name="mapped">mapped</a></strong>
  (also known as committed)</dt>
<dd><p>A range of <a href="v.html#virtual.address">virtual addresses</a> is said to be <em>mapped</em> (<em>committed</em> on Windows&reg;) if there is <a href="p.html#physical.memory-2">physical memory<sup><small>(2)</small></sup></a> associated with the range.</p>
<p>Note that, in some circumstances, the <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> system could actually <a href="o.html#overcommit">overcommit</a> mapped memory.</p>

<p><strong>Opposites:</strong> <a href="u.html#unmapped">unmapped</a>.
<br />
<strong>See also:</strong> <a href="#mapping">mapping</a>;
    <a href="#memory.mapping">memory mapping</a>;
    <a href="#mmap">mmap</a>.
</p></dd>

<dt><strong><a id="mapping" name="mapping">mapping</a></strong></dt>
<dd><p>A <em>mapping</em> is a correspondence between a range of <a href="v.html#virtual.address">virtual addresses</a> and some <a href="#memory-1">memory<sup><small>(1)</small></sup></a> (or a <a href="#memory.mapping">memory-mapped</a> object).  The physical location of the memory will be managed by the <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> system.</p>
<p>Each <a href="p.html#page">page</a> in a mapping could be <a href="p.html#paged.out">paged out</a> or <a href="p.html#paged.in">paged in</a>, and the locations it occupies in <a href="#main.memory">main memory</a> and/or <a href="s.html#swap.space">swap space</a> might change over time.</p>

<p>The <a href="v.html#virtual.address.space">virtual address space</a> can contain of a complex set of mappings.  Typically, parts of the address space are <a href="#mapped">mapped</a> (have a mapping assigned), others are <a href="r.html#reserved">reserved</a> but unmapped, and most of it is entirely <a href="u.html#unmapped">unmapped</a>.</p>

<p align="center"><em>Virtual memory with different kinds of mappings</em><br /><img alt="Diagram: Virtual memory with different kinds of mappings" src="../diagrams/mapped.png" border="2" height="339" width="329" /></p>

<p><strong>See also:</strong> <a href="b.html#backing.store">backing store</a>.
</p></dd>

<dt><strong><a id="mark-compact" name="mark-compact">mark-compact</a></strong></dt>
<dd><p>Mark-compact collection is a kind of <a href="t.html#tracing.garbage.collection">tracing garbage collection</a> that operates by <a href="#marking">marking</a> <a href="r.html#reachable">reachable</a> <a href="o.html#object">objects</a>, then <a href="c.html#compaction">compacting</a> the marked objects (which must include all the <a href="l.html#live">live</a> objects).</p>
<p>The mark phase follows <a href="r.html#reference">reference</a> chains to mark all reachable objects; the compaction phase typically performs a number of sequential passes over <a href="#memory-2">memory<sup><small>(2)</small></sup></a> to move objects and update references.  As a result of compaction, all the marked objects are moved into a single contiguous <a href="b.html#block">block</a> of memory (or a small number of such blocks); the memory left unused after compaction is <a href="r.html#recycle">recycled</a>.</p>

<p>Mark-compact collection can be regarded as a variation of <a href="#mark-sweep">mark-sweep collection</a>, with extra effort spent to eliminate the resulting <a href="f.html#fragmentation">fragmentation</a>.  Compaction also allows the use of more efficient  <a href="a.html#allocation.mechanism">allocation mechanisms</a>, by making large free blocks available.</p>

<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#edwards">Daniel J. Edwards. n.d.. <cite>Lisp II Garbage Collector</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="mark-sweep" name="mark-sweep">mark-sweep</a>, mark-and-sweep</strong></dt>
<dd><p>Mark-sweep collection is a kind of <a href="t.html#tracing.garbage.collection">tracing garbage collection</a> that operates by <a href="#marking">marking</a> <a href="r.html#reachable">reachable</a> <a href="o.html#object">objects</a>, then <a href="s.html#sweeping">sweeping</a> over <a href="#memory-2">memory<sup><small>(2)</small></sup></a> and <a href="r.html#recycle">recycling</a> objects that are unmarked (which must be <a href="u.html#unreachable">unreachable</a>), putting them on a <a href="f.html#free.list">free list</a>.</p>
<p>The mark phase follows <a href="r.html#reference">reference</a> chains to mark all reachable objects; the sweep phase performs a sequential (<a href="a.html#address">address</a>-order) pass over memory to recycle all unmarked objects.  A mark-sweep <a href="c.html#collector-1">collector<sup><small>(1)</small></sup></a> doesn't move objects.</p>

<p><strong>Historical note:</strong> This was the first GC algorithm, devised by McCarthy for <a href="../articles/lang.html#lisp">Lisp</a>.</p>

<p><strong>See also:</strong> <a href="#mark-compact">mark-compact</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#mccarthy60">J. McCarthy. 1960. <cite>Recursive Functions of Symbolic Expressions and Their Computation by Machine</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="marking" name="marking">marking</a></strong></dt>
<dd><p>Marking is the first phase ("the mark phase") of the <a href="#mark-sweep">mark-sweep</a> algorithm or <a href="#mark-compact">mark-compact</a> algorithm.  It follows all <a href="r.html#reference">references</a> from a set of <a href="r.html#root">roots</a> to mark all the <a href="r.html#reachable">reachable</a> <a href="o.html#object">objects</a>.</p>
<p>Marking follows <a href="r.html#reference">reference</a> chains and makes some sort of mark for each object it reaches.</p>

<p>Marking is often achieved by setting a bit in the object, though any conservative representation of a predicate on the <a href="l.html#location">location</a> of the object can be used.  In particular, storing the mark bit within the object can lead to poor <a href="l.html#locality.of.reference">locality of reference</a>.</p>

<p><strong>See also:</strong> <a href="s.html#sweeping">sweep</a>;
    <a href="c.html#compaction">compact</a>.
</p></dd>

<dt><strong><a id="mb" name="mb">MB</a></strong>
  (for full details, see <a href="#megabyte">megabyte</a>)</dt>
<dd><p>A megabyte is 1024 <a href="k.html#kilobyte">kilobytes</a>, or 1048576 <a href="b.html#byte-1">bytes<sup><small>(1)</small></sup></a>.</p></dd>

<dt><strong><a id="megabyte" name="megabyte">megabyte</a></strong>
  (also known as MB)</dt>
<dd><p>A megabyte is 1024 <a href="k.html#kilobyte">kilobytes</a>, or 1048576 <a href="b.html#byte-1">bytes<sup><small>(1)</small></sup></a>.</p>
<p>See <a href="b.html#byte-1">byte<sup><small>(1)</small></sup></a> for general information on this and related quantities.</p></dd>


<dt><strong><a id="memoization" name="memoization">memoization</a></strong>
  (for full details, see <a href="c.html#caching-3">caching<sup><small>(3)</small></sup></a>)</dt>
<dd><p><i>Caching</i> is a heuristic that stores answers to questions asked in the past in a <i>cache</i> or a <i>table</i>, in order that they may be more quickly answered in the future.  This process is also called memoization and tabling (by the <a href="../articles/lang.html#prolog">Prolog</a> community).</p></dd>

<dt><strong><a id="memory-1" name="memory-1">memory<sup><small>(1)</small></sup></a></strong>
  (also known as storage, store(2))</dt>
<dd><p><i>memory</i> or <i>storage</i> (or <i>store</i>) is where data and instructions are stored.  For example, <a href="c.html#cache-1">caches<sup><small>(1)</small></sup></a>, <a href="#main.memory">main memory</a>, floppy and hard disks are all storage devices.
</p>
<p>These terms are also used for the capacity of a system to store data, and may be applied to the sum total of all the storage devices attached to a computer.</p>

<p><strong>Historical note:</strong> "Store" is old-fashioned, but survives in expressions such as "<a href="b.html#backing.store">backing store</a>".</p></dd>


<dt><strong><a id="memory-2" name="memory-2">memory<sup><small>(2)</small></sup></a></strong></dt>
<dd><p><em>Memory</em> refers to <a href="s.html#storage">storage</a> that can be accessed by the processor directly (using memory addressing instructions).</p>
<p>This could be <a href="r.html#real.memory-1">real memory<sup><small>(1)</small></sup></a> or <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a>.</p></dd>


<dt><strong><a id="memory-3" name="memory-3">memory<sup><small>(3)</small></sup></a></strong>
  (for full details, see <a href="#main.memory">main memory</a>)</dt>
<dd><p>The <em>main memory</em> (or <em>primary storage</em>) of a computer is <a href="#memory-1">memory<sup><small>(1)</small></sup></a> that is wired directly to the processor, consisting of <a href="r.html#ram">RAM</a> and possibly <a href="r.html#rom">ROM</a>.</p></dd>

<dt><strong><a id="memory-4" name="memory-4">memory<sup><small>(4)</small></sup></a></strong></dt>
<dd><p>A memory <a href="l.html#location">location</a>; for example,  "My watch has 256 memories."</p></dd>

<dt><strong><a id="memory.bandwidth" name="memory.bandwidth">memory bandwidth</a></strong></dt>
<dd><p>Memory bandwidth (by analogy with the term <i>bandwidth</i> from communication
theory) is a measure of how quickly information (expressed in terms of
bits) can be transferred between two places in a computer system.</p>
<p>Often the term is applied to a measure of how quickly the processor can obtain
information from the <a href="#main.memory">main memory</a> (for example, "My new bus design has a
bandwidth of over 400 Megabytes per second").</p></dd>


<dt><strong><a id="memory.cache" name="memory.cache">memory cache</a></strong>
  (for full details, see <a href="c.html#cache-1">cache<sup><small>(1)</small></sup></a>)</dt>
<dd><p>A processor's memory cache is a small piece of fast, but more expensive memory, usually <a href="s.html#static.memory-1">static memory<sup><small>(1)</small></sup></a>, used for copies of parts of <a href="#main.memory">main memory</a>.  The cache is automatically used by the processor for fast access to any data currently <a href="r.html#resident">resident</a> there.  Access to the cache typically takes only a few processor clock cycles, whereas access to <a href="#main.memory">main memory</a> may take tens or even hundreds of cycles.</p></dd>

<dt><strong><a id="memory.hierarchy" name="memory.hierarchy">memory hierarchy</a></strong>
  (for full details, see <a href="s.html#storage.hierarchy">storage hierarchy</a>)</dt>
<dd><p>A typical computer has several different <em>levels</em> of <a href="s.html#storage">storage</a>.
Each level of storage has a different speed, cost, and size.
The levels form a <em>storage hierarchy</em>, in which the topmost levels (those nearest the processor) are fastest, most expensive and smallest.</p></dd>

<dt><strong><a id="memory.leak" name="memory.leak">memory leak</a>, space-leak</strong>
  (also known as leak, space leak)</dt>
<dd><p>A memory leak is where <a href="a.html#allocate">allocated</a> <a href="#memory-2">memory<sup><small>(2)</small></sup></a> is not <a href="f.html#free-1">freed</a> although it is never used again.</p>
<p>In <a href="#manual.memory.management">manual memory management</a>, this usually occurs because <a href="o.html#object">objects</a> become <a href="u.html#unreachable">unreachable</a> without being <a href="f.html#free-1">freed</a>.</p>

<p>In <a href="t.html#tracing.garbage.collection">tracing garbage collection</a>, this happens when objects are <a href="r.html#reachable">reachable</a> but not <a href="l.html#live">live</a>.</p>

<p>In <a href="r.html#reference.counting">reference counting</a>, this happens when objects are <a href="r.html#reference">referenced</a> but not <a href="l.html#live">live</a>. (Such objects may or may not be <a href="r.html#reachable">reachable</a>.)</p>

<p>Repeated memory leaks cause the memory usage of a process to grow without bound.</p></dd>


<dt><strong><a id="memory.location" name="memory.location">memory location</a></strong>
  (also known as location)</dt>
<dd><p>Each separately-<a href="a.html#address">addressable</a> unit of <a href="#memory-2">memory<sup><small>(2)</small></sup></a> in which data can be stored is called a <em>memory location</em>.  Usually, these hold a <a href="b.html#byte-2">byte<sup><small>(2)</small></sup></a>, but the term can refer to <a href="w.html#word">words</a>.</p></dd>

<dt><strong><a id="memory.management" name="memory.management">memory management</a></strong>
  (also known as storage management)</dt>
<dd><p>Memory management is the art and the process of coordinating and controlling the use of <a href="#memory-1">memory<sup><small>(1)</small></sup></a> in a computer system.</p>
<p>Memory management can be divided into three areas:</p>

<ol>
  <li>Memory management hardware (<a href="#mmu">MMUs</a>, <a href="r.html#ram">RAM</a>, etc.);</li>
  <li>Operating system memory management (<a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a>, <a href="p.html#protection">protection</a>);</li>
  <li>Application memory management (<a href="a.html#allocate">allocation</a>, <a href="f.html#free-1">deallocation</a>, <a href="g.html#garbage.collection">garbage collection</a>).</li>
</ol>

<p>Memory management hardware consists of the electronic devices and associated circuitry that store the state of a computer.  These devices include RAM, MMUs (memory management units), <a href="c.html#cache-1">caches<sup><small>(1)</small></sup></a>, disks, and processor <a href="r.html#register">registers</a>.  The design of memory hardware is critical to the performance of modern computer systems.  In fact, <a href="#memory.bandwidth">memory bandwidth</a> is perhaps the main limiting factor on system performance.</p>

<p>Operating system memory management is concerned with using the memory management hardware to manage the resources of the <a href="s.html#storage.hierarchy">storage hierarchy</a> and allocating them to the various activities running on a computer.  The most significant part of this on many systems is <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a>, which creates the illusion that every process has more memory than is actually available.  OS memory management is also concerned with <a href="#memory.protection">memory protection</a> and security, which help to maintain the integrity of the operating system against accidental damage or deliberate attack.  It also protects user programs from errors in other programs.</p>

<p>Application memory management involves obtaining <a href="#memory-2">memory<sup><small>(2)</small></sup></a> from the operating system, and managing its use by an application program.  Application programs have dynamically changing storage requirements.
The application <a href="#memory.manager">memory manager</a> must cope with this while minimizing the total CPU overhead, interactive pause times, and the total memory used.</p>

<p>While the operating system may create the illusion of nearly infinite memory, it is a complex task to manage application memory so that the application can run most efficiently.
Ideally, these problems should be solved by tried and tested tools, tuned to a specific application.</p>

<p>The Memory Management Reference is mostly concerned with application memory management.</p>

<p><strong>See also:</strong> <a href="a.html#automatic.memory.management">automatic memory management</a>;
    <a href="#manual.memory.management">manual memory management</a>.
<br />
<strong>Other links:</strong> <a href="../articles/begin.html">Beginner's Guide</a>.
</p></dd>

<dt><strong><a id="memory.management.unit" name="memory.management.unit">Memory Management Unit</a></strong>
  (for full details, see <a href="#mmu">MMU</a>)</dt>
<dd><p>The MMU (Memory Management Unit) is a hardware device responsible for
handling <a href="#memory-2">memory<sup><small>(2)</small></sup></a> accesses requested by the main processor.</p></dd>

<dt><strong><a id="memory.manager" name="memory.manager">memory manager</a></strong></dt>
<dd><p>The memory manager is that part of the system that manages <a href="#memory-2">memory<sup><small>(2)</small></sup></a>, servicing <a href="a.html#allocate">allocation</a> requests, and <a href="r.html#recycle">recycling</a> memory, either <a href="#manual.memory.management">manually</a> or <a href="a.html#automatic.memory.management">automatically</a>.</p>
<p>The memory manager can have a significant effect on the efficiency of the program; it is not unusual for a program to spend 20% of its time managing memory.</p>

<p><strong>Similar terms:</strong> <a href="a.html#allocator">allocator</a>;
    <a href="c.html#collector-1">collector<sup><small>(1)</small></sup></a>.
<br />
<strong>See also:</strong> <a href="#memory.management">memory management</a>.
</p></dd>

<dt><strong><a id="memory.mapping" name="memory.mapping">memory mapping</a></strong>
  (also known as file mapping)</dt>
<dd><p><em>Memory mapping</em> is the technique of making a part of the <a href="a.html#address.space">address space</a> appear to contain an "object", such as a file or device, so that ordinary <a href="#memory-2">memory<sup><small>(2)</small></sup></a> accesses act on that object.</p>
<p>The object is said to be <em>mapped</em> to that range of addresses.  (The term "object" does not mean a program <a href="o.html#object">object</a>.  It comes from UNIX&reg; terminology on the <code><a href="#mmap">mmap</a></code>(2) man page.)</p>

<p align="center"><em>An address space with a range mapped to part of an object</em><br /><img alt="Diagram: An address space with a range mapped to part of an object" src="../diagrams/mapping.png" border="2" height="259" width="418" /></p>

<p>Memory mapping uses the same mechanism as <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> to "trap" accesses to parts of the <a href="a.html#address.space">address space</a>, so that data from the file or device can be <a href="p.html#paged.in">paged in</a> (and other parts <a href="p.html#paged.out">paged out</a>) before the access is completed.</p>

<p><strong>Historical note:</strong> File mapping is available on most modern UNIX&reg; systems, and also on recent versions of the Windows&reg; operating system such as Windows 95&reg; and Windows NT&reg;.  However, it has a much longer history.  In Multics, it was the primary way of accessing files.</p>

<p><strong>See also:</strong> <a href="#mapped">mapped</a>.
</p></dd>

<dt><strong><a id="memory.protection" name="memory.protection">memory protection</a></strong>
  (for full details, see <a href="p.html#protection">protection</a>)</dt>
<dd><p>Many operating systems support protection of <a href="#memory-2">memory<sup><small>(2)</small></sup></a> <a href="p.html#page">pages</a>.  Individual pages may be protected against a combination of read, write or execute accesses by a process. </p></dd>

<dt><strong><a id="misaligned" name="misaligned">misaligned</a></strong>
  (for full details, see <a href="u.html#unaligned">unaligned</a>)</dt>
<dd><p>An <a href="a.html#address">address</a> is unaligned or misaligned if it does not comply with some <a href="a.html#alignment">alignment</a> constraint on it.</p></dd>

<dt><strong><a id="miss" name="miss">miss</a></strong></dt>
<dd><p>A miss is a lookup failure in any form of <a href="c.html#caching-3">cache<sup><small>(3)</small></sup></a>, most commonly at some level of a <a href="s.html#storage.hierarchy">storage hierarchy</a>, such as a <a href="c.html#cache-1">cache<sup><small>(1)</small></sup></a> or <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> system.</p>
<p>The cost of a miss in a virtual memory system is considerable -- it may be five orders of magnitude more costly than a hit. In some systems, such as multi-process operating systems, other work may be done while a miss is serviced.</p>

<p><strong>Opposites:</strong> <a href="h.html#hit">hit</a>.
<br />
<strong>See also:</strong> <a href="#miss.rate">miss rate</a>.
</p></dd>

<dt><strong><a id="miss.rate" name="miss.rate">miss rate</a></strong></dt>
<dd><p>At any level of a <a href="s.html#storage.hierarchy">storage hierarchy</a>, the miss rate is the proportion of accesses which <a href="#miss">miss</a>.</p>
<p>Because misses are very costly, each level is designed to minimize the miss rate.  For instance, in <a href="c.html#cache-1">caches<sup><small>(1)</small></sup></a>, miss rates of about 0.01 may be acceptable, whereas in <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> systems, acceptable miss rates are much lower (say 0.00005).  If a system has a miss rate which is too high, it will spend most of its time servicing the misses, and is said to <a href="t.html#thrash">thrash</a>.</p>

<p>Miss rates may also be given as a number of misses per unit time, or per instruction.</p>

<p><strong>Opposites:</strong> <a href="h.html#hit.rate">hit rate</a>.
</p></dd>

<dt><strong><a id="mmap" name="mmap">mmap</a></strong></dt>
<dd><p><code>mmap</code> is a system call provided on many UNIX&reg; systems to create a <a href="#mapping">mapping</a> for a range of <a href="v.html#virtual.address">virtual addresses</a>.</p></dd>

<dt><strong><a id="mmu" name="mmu">MMU</a></strong>
  (also known as Memory Management Unit)</dt>
<dd><p>The MMU (Memory Management Unit) is a hardware device responsible for
handling <a href="#memory-2">memory<sup><small>(2)</small></sup></a> accesses requested by the main processor.</p>
<p>This typically involves translation of <a href="v.html#virtual.address">virtual addresses</a> to <a href="p.html#physical.address">physical addresses</a>, <a href="c.html#cache-1">cache<sup><small>(1)</small></sup></a> control, bus arbitration, <a href="#memory.protection">memory protection</a>, and the generation of
various exceptions.  Not all processors have an MMU.</p>

<p><strong>See also:</strong> <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a>;
    <a href="p.html#page.fault">page fault</a>;
    <a href="s.html#segmentation.violation">segmentation violation</a>.
</p></dd>

<dt><strong><a id="mostly-copying.garbage.collection" name="mostly-copying.garbage.collection">mostly-copying garbage collection</a>, mostly copying garbage collection</strong></dt>
<dd><p>A type of <a href="s.html#semi-conservative.garbage.collection">semi-conservative</a> <a href="t.html#tracing.garbage.collection">tracing garbage collection</a> which permits <a href="o.html#object">objects</a> to <a href="#moving.garbage.collector">move</a> if no <a href="a.html#ambiguous.reference">ambiguous references</a> point to them. </p>
<p>The techniques used are a hybrid of <a href="c.html#copying.garbage.collection">copying garbage collection</a> and <a href="#mark-sweep">mark-sweep</a>.</p>

<p>Mostly-copying garbage collectors share many of the benefits of copying collectors, including <a href="c.html#compaction">compaction</a>.  Since they support ambiguous references they are additionally suitable for use with uncooperative compilers, and may be an efficient choice for multi-threaded systems.</p>

<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#bartlett89">Joel F. Bartlett. 1989. <cite>Mostly-Copying Garbage Collection Picks Up Generations and C++</cite>.</a></li>
  <li><a href="../bib/f.html#yip91">G. May Yip. 1991. <cite>Incremental, Generational Mostly-Copying Garbage Collection in Uncooperative Environments</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="mostly-exact.garbage.collection" name="mostly-exact.garbage.collection">mostly-exact garbage collection</a></strong>
  (for full details, see <a href="s.html#semi-conservative.garbage.collection">semi-conservative garbage collection</a>)</dt>
<dd><p>A variant of <a href="c.html#conservative.garbage.collection">conservative garbage collection</a> which deals with <a href="e.html#exact.reference">exact references</a> as well as <a href="a.html#ambiguous.reference">ambiguous references</a>.</p></dd>

<dt><strong><a id="mostly-precise.garbage.collection" name="mostly-precise.garbage.collection">mostly-precise garbage collection</a></strong>
  (for full details, see <a href="s.html#semi-conservative.garbage.collection">semi-conservative garbage collection</a>)</dt>
<dd><p>A variant of <a href="c.html#conservative.garbage.collection">conservative garbage collection</a> which deals with <a href="e.html#exact.reference">exact references</a> as well as <a href="a.html#ambiguous.reference">ambiguous references</a>.</p></dd>

<dt><strong><a id="moving.garbage.collector" name="moving.garbage.collector">moving garbage collector</a></strong>
  (also known as moving memory manager)</dt>
<dd><p>A memory manager (often a <a href="g.html#garbage.collector">garbage collector</a>) is said to be moving if
<a href="a.html#allocate">allocated</a> <a href="o.html#object">objects</a> can move during their lifetimes. </p>
<p><strong>Relevance to memory management:</strong> In the garbage
collecting world this will apply to <a href="c.html#copying.garbage.collection">copying</a> collectors and to
<a href="#mark-compact">mark-compact</a> collectors. It may also refer to <a href="r.html#replicating.garbage.collector">replicating</a> collectors.</p>

<p><strong>Similar terms:</strong> <a href="c.html#copying.garbage.collection">copying garbage collection</a>.
</p></dd>

<dt><strong><a id="moving.memory.manager" name="moving.memory.manager">moving memory manager</a></strong>
  (for full details, see <a href="#moving.garbage.collector">moving garbage collector</a>)</dt>
<dd><p>A memory manager (often a <a href="g.html#garbage.collector">garbage collector</a>) is said to be moving if
<a href="a.html#allocate">allocated</a> <a href="o.html#object">objects</a> can move during their lifetimes. </p></dd>

<dt><strong><a id="mutable" name="mutable">mutable</a></strong></dt>
<dd><p>Any <a href="o.html#object">object</a> which may be changed by a program is <a href="#mutable">mutable</a>.
Opposite of <a href="i.html#immutable">immutable</a>.</p>
<p><strong>Opposites:</strong> <a href="i.html#immutable">immutable</a>.
</p></dd>

<dt><strong><a id="mutator" name="mutator">mutator</a></strong></dt>
<dd><p>In a <a href="g.html#garbage.collection">garbage-collected</a> system, the part that executes the user code, which <a href="a.html#allocate">allocates</a> <a href="o.html#object">objects</a> and
modifies, or <i>mutates</i>, them.</p>
<p>For purposes of describing <a href="i.html#incremental.garbage.collection">incremental garbage collection</a>, the system is divided into the <i>mutator</i> and the <i><a href="c.html#collector-2">collector<sup><small>(2)</small></sup></a></i>.  These can be separate threads of computation, or interleaved within the same thread.</p>

<p>The user code issues allocation requests, but the allocator code is usually considered part of the collector.  Indeed, one of the major ways of scheduling the other work of the collector is to perform a little of it at every allocation.</p>

<p>While the mutator mutates, it implicitly <a href="f.html#free-1">frees</a> <a href="s.html#storage">storage</a> by overwriting <a href="r.html#reference">references</a>.</p>

<p><strong>Historical note:</strong> This term is due to Dijkstra et al.</p>

<p><strong>Opposites:</strong> <a href="c.html#collector-2">collector<sup><small>(2)</small></sup></a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#dlmss76">E. W. Dijkstra, Leslie Lamport, A. J. Martin, C. S. Scholten, E. F. M. Steffens. 1976. <cite>On-the-fly Garbage Collection: An Exercise in Cooperation</cite>.</a></li>
</ul><br /></dd>

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
<strong>M</strong>
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