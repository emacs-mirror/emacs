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
<link rel="prev" href="h.html" />
<link rel="next" href="k.html" />
<title>The Memory Management Glossary: I</title>
</head>
<body lang="en" bgcolor="#FFFFFF" text="#000000" link="#000099" vlink="#660066" alink="#FF0000">
<h1 align="center">The Memory Management Glossary<br />
<big>I</big></h1>
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
<strong>I</strong>
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
<dt><strong><a id="immediate.data" name="immediate.data">immediate data</a></strong></dt>
<dd><p>Immediate data is the representation of a <a href="v.html#value.object">value object</a> as one or more machine <a href="w.html#word">words</a>, as a register, or as a field in an instruction.</p>
<p>Immediate data takes its name from the value of the object being immediately available, rather than requiring a <a href="l.html#load">load</a> or indirection through a <a href="r.html#reference">reference</a>.</p>

<p><strong>Similar terms:</strong> <a href="u.html#unboxed">unboxed</a>.
<br />
<strong>Opposites:</strong> <a href="b.html#boxed">boxed</a>;
    <a href="r.html#reference">reference</a>;
    <a href="p.html#pointer">pointer</a>.
</p></dd>

<dt><strong><a id="immune.set" name="immune.set">immune set</a></strong></dt>
<dd><p>The set of <a href="o.html#object">objects</a> which are not <a href="t.html#threatened.set">condemned</a>.</p>
<p><strong>Opposites:</strong> <a href="c.html#condemned.set">condemned set</a>.
</p></dd>

<dt><strong><a id="immutable" name="immutable">immutable</a></strong></dt>
<dd><p>In some programming languages, <a href="o.html#object">objects</a> of some types are immutable, that is, they cannot be modified.  For example, in Standard <a href="../articles/lang.html#ml">ML</a>, only arrays and refs are mutable; all other objects are immutable.</p>
<p>This property can be very useful for <a href="g.html#garbage.collection">garbage collection</a>.  For instance, no immutable object may contain a <a href="r.html#reference">reference</a> to an object younger than itself, and no immutable object will appear in a
<a href="r.html#remembered.set">remembered set</a>.  Garbage collectors for these languages often take advantage of this property.</p>

<p>In lazy languages, the evaluation of an expression may require an object of a different size, and adjustment of references may take place.  This means that, although objects might be immutable at the language level, they are not immutable at the implementation level, and may contain references to younger objects.</p>

<p><strong>Opposites:</strong> <a href="m.html#mutable">mutable</a>.
<br />
<strong>See also:</strong> <a href="g.html#generational.garbage.collection">generational garbage collection</a>.
</p></dd>

<dt><strong><a id="immutable.object" name="immutable.object">immutable object</a></strong>
  (for full details, see <a href="v.html#value.object">value object</a>)</dt>
<dd><p>A <em>value object</em> or <em>immutable object</em> is an <a href="o.html#object">object</a> whose identity depends solely upon its value or magnitude.</p></dd>

<dt><strong><a id="in-band.header" name="in-band.header">in-band header</a></strong>
  (also known as frame, header)</dt>
<dd><p>Some <a href="m.html#memory.manager">memory managers</a> <a href="a.html#allocate">allocate</a> a fixed amount more than is necessary for each <a href="b.html#block">block</a> and use it to store information such as the size of the block or a <a href="t.html#tag">tag</a>.  This extra memory is known as <em>an in-band header</em> or <em>a frame</em></p>
<p>This is a form of <a href="#internal.fragmentation">internal fragmentation</a>, although sometimes, <a href="a.html#alignment">alignment</a> requirements result in free space for the header.</p>

<p>Storing control information <em>in-band</em> often results in bad <a href="l.html#locality.of.reference">locality</a>, particularly for <a href="f.html#free-1">deallocation</a>.</p>

<p><strong>Opposites:</strong> <a href="o.html#out-of-band.header">out-of-band header</a>.
<br />
<strong>See also:</strong> <a href="s.html#stack.frame">stack frame</a>;
    <a href="a.html#activation.frame">activation frame</a>.
</p></dd>

<dt><strong><a id="incremental.garbage.collection" name="incremental.garbage.collection">incremental garbage collection</a></strong></dt>
<dd><p>Some <a href="t.html#tracing.garbage.collection">tracing garbage collection</a> algorithms can pause in the middle of a <a href="c.html#collection.cycle">collection cycle</a> while the <a href="m.html#mutator">mutator</a> continues, without ending up with inconsistent data.  Such collectors can operate incrementally and are suitable for use in an interactive system.</p>
<p>Primitive garbage <a href="g.html#garbage.collector">collectors<sup><small>(1)</small></sup></a>, once they start a <a href="c.html#collection.cycle">collection cycle</a>, must either finish the task, or abandon all their work so far.  This is often an appropriate restriction, but is unacceptable when the system must guarantee response times; for example, in systems with a user interface and in real-time hardware control systems.  Such systems might use incremental garbage collection so that the time-critical processing and the garbage collection can proceed effectively in parallel, without wasted effort.</p>

<p><strong>Similar terms:</strong> <a href="p.html#parallel.garbage.collection">parallel garbage collection</a>.
<br />
<strong>See also:</strong> <a href="t.html#tri-color.marking">tri-color marking</a>;
    <a href="b.html#barrier-1">barrier<sup><small>(1)</small></sup></a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#ael88">Andrew Appel, John R. Ellis, Kai Li. 1988. <cite>Real-time Concurrent Collection on Stock Multiprocessors</cite>.</a></li>
  <li><a href="../bib/f.html#bds91">Hans-J. Boehm, Alan J. Demers, Scott Shenker. 1991. <cite>Mostly Parallel Garbage Collection</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="incremental-update" name="incremental-update">incremental-update</a>, incremental update</strong></dt>
<dd><p>Incremental-update algorithms for <a href="t.html#trace">tracing</a>, <a href="#incremental.garbage.collection">incremental GC</a> note changes made by the <a href="m.html#mutator">mutator</a> to the <a href="g.html#graph">graph</a> of <a href="o.html#object">objects</a> and update the <a href="c.html#collector-2">collector<sup><small>(2)</small></sup></a> state to make it correctly trace the new graph.</p>
<p>In order for the collector to miss a <a href="r.html#reachable">reachable</a> <a href="o.html#object">object</a>, the following two conditions need to hold at some point during tracing:</p>

<ol>
<li>The mutator stores a <a href="r.html#reference">reference</a> to a <a href="w.html#white">white</a> object into a <a href="b.html#black">black</a> object.</li>
<li>All paths from any <a href="g.html#gray">gray</a> objects to that white object are destroyed.</li>
</ol>

<p>Incremental-update algorithms ensure the first condition cannot occur, by painting either the black or the white object gray (see <a href="../bib/f.html#pirinen98"><cite>Barrier techniques for incremental tracing</cite></a> for details).</p>

<p>They are so called because they incrementally update the collector's view of the graph to track changes made by the mutator.</p>

<p><strong>Historical note:</strong> This distinction between incremental-update and snapshot-at-the-beginning was first introduced for write-barrier algorithms, but it applies to any type of tracing algorithm.</p>

<p><strong>Opposites:</strong> <a href="s.html#snapshot-at-the-beginning">snapshot-at-the-beginning</a>.
<br />
<strong>See also:</strong> <a href="t.html#tri-color.marking">tri-color marking</a>;
    <a href="s.html#strong.tri-color.invariant">strong tri-color invariant</a>;
    <a href="b.html#barrier-1">barrier<sup><small>(1)</small></sup></a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil94">Paul R. Wilson. 1994. <cite>Uniprocessor Garbage Collection Techniques</cite>.</a></li>
  <li><a href="../bib/f.html#pirinen98">Pekka P. Pirinen. 1998. <cite>Barrier techniques for incremental tracing</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="indefinite.extent" name="indefinite.extent">indefinite extent</a></strong></dt>
<dd><p>An <a href="o.html#object">object</a> has indefinite extent if its <a href="l.html#lifetime">lifetime</a> is independent of the block or function-call structure of the program.</p>
<p>The <a href="l.html#lifetime">lifetime</a> of such an object can sometimes be determined by the programmer, and specified by <a href="f.html#free-1">freeing</a> the object explicitly.  This becomes harder to do correctly as the program becomes more complex, especially if objects are passed across module boundaries, or if higher-order functions are used.  In some languages it is impossible to determine the extent at compile-time.  In these situations, a <a href="g.html#garbage.collector">garbage collector</a> can be used to <a href="r.html#recycle">recycle</a> objects whose <a href="l.html#life">life</a> has come to an end.</p>

<p><strong>Opposites:</strong> <a href="d.html#dynamic.extent">dynamic extent</a>.
</p></dd>

<dt><strong><a id="indexed.fit" name="indexed.fit">indexed fit</a></strong></dt>
<dd><p>A class of <a href="a.html#allocation.mechanism">allocation mechanisms</a> that use an indexing data
structure, such as a tree or hash table, to identify suitable <a href="f.html#free.block">free blocks</a>, according to the <a href="a.html#allocation.policy">allocation policy</a>. For instance, a tree
ordered by block size may be used to implement the <a href="b.html#best.fit">best fit</a>
policy.</p>
<p><strong>See also:</strong> <a href="a.html#allocation.mechanism">allocation mechanism</a>;
    <a href="a.html#allocation.policy">allocation policy</a>;
    <a href="s.html#sequential.fit">sequential fit</a>;
    <a href="b.html#bitmapped.fit">bitmapped fit</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil95">Paul R. Wilson, Mark S. Johnstone, Michael Neely, David Boles. 1995. <cite>Dynamic Storage Allocation: A Survey and Critical Review</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="indirect.method" name="indirect.method">indirect method</a></strong></dt>
<dd><p>Indirect methods of <a href="a.html#automatic.memory.management">automatic memory management</a> are those in which the information necessary to determine whether an <a href="o.html#object">object</a> can be <a href="r.html#reclaim">reclaimed</a> is not stored in or associated with that object, but is derived from other objects.</p>
<p>Indirect methods detect <a href="g.html#garbage">garbage</a> by <a href="t.html#trace">tracing</a> <a href="r.html#reachable">reachable</a> objects.</p>

<p>Indirect methods cannot always reclaim <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> as soon as it becomes <a href="d.html#dead">dead</a>, because it may be necessary to inspect many other objects to determine this.  However, not having to store and update information on each object may reduce the overhead for the <a href="c.html#collector-1">collector<sup><small>(1)</small></sup></a>.  In <a href="d.html#distributed.garbage.collection">distributed garbage collection</a>, this can reduce the amount of communication between processors.</p>

<p><strong>Similar terms:</strong> <a href="t.html#tracing.garbage.collection">tracing garbage collection</a>.
<br />
<strong>Opposites:</strong> <a href="d.html#direct.method">direct method</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#jones96">Richard E. Jones, Rafael Lins. 1996. <cite>Garbage Collection: Algorithms for Automatic Dynamic Memory Management</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="infant.mortality" name="infant.mortality">infant mortality</a></strong>
  (for full details, see <a href="g.html#generational.hypothesis">generational hypothesis</a>)</dt>
<dd><p><em>Infant mortality</em> or <em>the generational hypothesis</em> is the observation that, in most cases, young <a href="o.html#object">objects</a> are much more likely to <a href="d.html#dead">die</a> than old objects.</p></dd>

<dt><strong><a id="inter-generational.pointer" name="inter-generational.pointer">inter-generational pointer</a></strong></dt>
<dd><p>An inter-generational pointer is a <a href="r.html#reference">reference</a> that is stored in an <a href="o.html#object">object</a> in one <a href="g.html#generation">generation</a> and references an object in another generation.</p>
<p>If the referent's generation is <a href="t.html#threatened.set">condemned</a> and the referrer's generation is not, then the reference is important in two ways.  First, the reference keeps the referent <a href="a.html#alive">alive</a>, so the referrer must be <a href="s.html#scan">scanned</a> during the <a href="c.html#collection">collection</a>.  Second, the reference must always refer to the referent, so if the referent is moved, then the referrer must be updated.</p>

<p>During a collection, the only objects in non-condemned areas that must be scanned are the ones that contain inter-generational pointers.  <a href="g.html#generational.garbage.collection">Generational garbage collectors</a> make use of <a href="w.html#write.barrier">write-barriers</a> and data structures like <a href="e.html#entry.table-2">entry tables<sup><small>(2)</small></sup></a>, <a href="e.html#exit.table">exit tables</a>, and <a href="r.html#remembered.set">remembered sets</a> to track those objects at run-time.</p>

<p>Inter-generational pointers can cause <a href="f.html#floating.garbage">floating garbage</a>: even if both referrer and referent die, the inter-generational pointer will stop the referent from being reclaimed until the referrer's generation is condemned.</p></dd>


<dt><strong><a id="interior.pointer" name="interior.pointer">interior pointer</a></strong>
  (also known as derived pointer)</dt>
<dd><p>An <em>interior pointer</em> is a pointer to <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> occupied by an <a href="o.html#object">object</a> which does not point to the start location.  Also called a <em>derived pointer</em> when it's derived from a <a href="b.html#base.pointer">base pointer</a>.</p>
<p>A <a href="p.html#pointer">pointer</a> to an object will usually take as its value the <a href="a.html#address">address</a> of the start of that object.</p>

<p>It is common to have interior pointers into string buffers or to embedded structures.  A <a href="s.html#suballocator">suballocator</a> may place a <a href="h.html#header">header</a> at the start of each object and pass on an interior pointer.</p>

<p><strong>Relevance to memory management:</strong> In a system where interior pointers are used, the <a href="g.html#garbage.collector">garbage collector</a> must be able to <a href="m.html#marking">mark</a> an object as <a href="r.html#reachable">reachable</a> without being told the start of the object.  In a system where interior pointers are not used, the collector should either ignore them (in particular, if it is <a href="s.html#scan">scanning</a> <a href="c.html#conservative.garbage.collection">conservatively</a>) and not retain <a href="g.html#garbage">garbage</a> because of them, or possibly report them as bugs.</p>

<p><strong>Opposites:</strong> <a href="b.html#base.pointer">base pointer</a>.
</p></dd>

<dt><strong><a id="internal.fragmentation" name="internal.fragmentation">internal fragmentation</a></strong></dt>
<dd><p>Internal <a href="f.html#fragmentation">fragmentation</a> is where the <a href="m.html#memory.manager">memory manager</a> <a href="a.html#allocate">allocates</a> more for
each allocation than is actually requested.  There are three reasons for
this: <a href="p.html#padding">padding</a>; <a href="b.html#buddy.system">buddy system</a>; <a href="#in-band.header">in-band headers</a>. </p>
<p><strong>See also:</strong> <a href="e.html#external.fragmentation">external fragmentation</a>.
</p></dd>

<dt><strong><a id="invalid.page.fault" name="invalid.page.fault">invalid page fault</a></strong></dt>
<dd><p>An exception when using <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> resulting from an access to a virtual memory location for which no translation is defined. </p>
<p>This is usually an error, often, anachronistically, known as a <a href="s.html#segmentation.violation">segmentation violation</a>.</p>

<p><strong>Similar terms:</strong> <a href="b.html#bus.error">bus error</a>.
<br />
<strong>See also:</strong> <a href="p.html#page.fault">page fault</a>.
</p></dd>

<dt><strong><a id="inverted.page.table" name="inverted.page.table">inverted page table</a>, inverted page-table</strong></dt>
<dd><p>In a <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> system, conventional <a href="p.html#page.table">page tables</a> have an entry for every <a href="p.html#page">page</a> in the <a href="v.html#virtual.address.space">virtual address space</a>.  An <em>inverted page table</em> has only as many entries as there are pages in <a href="p.html#physical.memory-1">physical memory<sup><small>(1)</small></sup></a>, and uses a hash lookup to translate <a href="v.html#virtual.address">virtual addresses</a> to <a href="p.html#physical.address">physical addresses</a> in nearly constant time.</p>
<p>The entire virtual address space of each process is described in an auxiliary structure, typically a B*-tree, that can efficiently store contiguous, sparse, or large <a href="a.html#address.space">address space</a> descriptions.  This auxiliary structure may itself be paged to avoid permanently consuming <a href="p.html#physical.memory-1">physical memory<sup><small>(1)</small></sup></a> resources.</p>

<p>Inverted page tables are ideal for schemes that store information about <a href="o.html#object">objects</a> in the high-order bits of their <a href="a.html#address">address</a>.  Such schemes may perform poorly with conventional page tables as the sparse address space may cause the page table structures to become so large as to compete with the program <a href="w.html#working.set">working set</a> for <a href="p.html#physical.memory-1">physical memory<sup><small>(1)</small></sup></a>.</p>

<p><strong>Historical note:</strong> The <a href="../articles/lang.html#lisp">Lisp</a> Machine was an early workstation that used an inverted page table with hardware lookup.  The Alpha, UltraSPARC&reg;, and PowerPC<sup><small class="tm-small">TM</small></sup> architectures all include inverted page tables.  Some implementations of these architectures have hardware-assisted lookup.</p></dd>


</dl>
<p align="center"><a href="a.html">A</a>
<a href="b.html">B</a>
<a href="c.html">C</a>
<a href="d.html">D</a>
<a href="e.html">E</a>
<a href="f.html">F</a>
<a href="g.html">G</a>
<a href="h.html">H</a>
<strong>I</strong>
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