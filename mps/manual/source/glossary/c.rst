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
<link rel="prev" href="b.html" />
<link rel="next" href="d.html" />
<title>The Memory Management Glossary: C</title>
</head>
<body lang="en" bgcolor="#FFFFFF" text="#000000" link="#000099" vlink="#660066" alink="#FF0000">
<h1 align="center">The Memory Management Glossary<br />
<big>C</big></h1>
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
<strong>C</strong>
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
<dt><strong><a id="cache-1" name="cache-1">cache<sup><small>(1)</small></sup></a></strong>
  (also known as memory cache, cache memory)</dt>
<dd><p>A processor's memory cache is a small piece of fast, but more expensive memory, usually <a href="s.html#static.memory-1">static memory<sup><small>(1)</small></sup></a>, used for copies of parts of <a href="m.html#main.memory">main memory</a>.  The cache is automatically used by the processor for fast access to any data currently <a href="r.html#resident">resident</a> there.  Access to the cache typically takes only a few processor clock cycles, whereas access to <a href="m.html#main.memory">main memory</a> may take tens or even hundreds of cycles.</p>
<p>What part of main memory is resident in a cache, and the mechanisms by which it is kept consistent, are quite varied.  See <a href="#cache.policy">cache policy</a>.</p>

<p>Some systems have more than one level of cache.  "level 1 cache" is the fastest, smallest <a href="s.html#storage.level">storage level</a>, "level 2" the next fastest, and so on.</p>

<p><strong>See also:</strong> <a href="s.html#storage.hierarchy">storage hierarchy</a>;
    <a href="#cache-2">cache<sup><small>(2)</small></sup></a>.
</p></dd>

<dt><strong><a id="cache-2" name="cache-2">cache<sup><small>(2)</small></sup></a></strong></dt>
<dd><p>A cache is any small, fast piece of <a href="s.html#storage">storage</a>, used for copies of data that normally reside in a larger, slower piece of storage.  The cache is used to speed up access to data <a href="r.html#resident">resident</a> in the slower storage.</p>
<p>In a typical cache, recently used data is <a href="r.html#resident">resident</a> in the cache (although the details of this depend on the <a href="#cache.policy">cache policy</a>).  A <a href="#cache-1">cache<sup><small>(1)</small></sup></a> is the most common example of a cache(2).</p>

<p><strong>See also:</strong> <a href="s.html#storage.hierarchy">storage hierarchy</a>.
</p></dd>

<dt><strong><a id="cache.memory" name="cache.memory">cache memory</a></strong>
  (for full details, see <a href="#cache-1">cache<sup><small>(1)</small></sup></a>)</dt>
<dd><p>A processor's memory cache is a small piece of fast, but more expensive memory, usually <a href="s.html#static.memory-1">static memory<sup><small>(1)</small></sup></a>, used for copies of parts of <a href="m.html#main.memory">main memory</a>.  The cache is automatically used by the processor for fast access to any data currently <a href="r.html#resident">resident</a> there.  Access to the cache typically takes only a few processor clock cycles, whereas access to <a href="m.html#main.memory">main memory</a> may take tens or even hundreds of cycles.</p></dd>

<dt><strong><a id="cache.policy" name="cache.policy">cache policy</a></strong></dt>
<dd><p>Any <a href="#caching-3">cache<sup><small>(3)</small></sup></a> uses a <em>cache policy</em> to decide which data to store.  A cache policy is an attempt to predict the future, so that the cache will provide swift responses to future requests.</p>
<p>Cache policy may be implemented in hardware, software, or a combination of both.  Some systems allow programs to influence cache policy, by giving hints or directions about future use of data.</p>

<p>There are three main aspects of cache behavior which the cache policy can affect:</p>

<dl>
  <dt>Fetch policy</dt>
  <dd><p>This determines which data is fetched into the cache, usually as a result of receiving a request for data that isn't cached.</p></dd>

<dt>Eviction policy</dt>
  <dd><p>This determines which data is discarded from the cache to provide space for newly fetched data.</p></dd>

<dt>Write policy</dt>
  <dd><p>This determines how and when modifications to cached data are synchronized with the underlying storage.</p></dd>
</dl>

<p><strong>See also:</strong> <a href="#cache-1">cache<sup><small>(1)</small></sup></a>;
    <a href="#cache-2">cache<sup><small>(2)</small></sup></a>;
    <a href="#caching-3">cache<sup><small>(3)</small></sup></a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#baker91">Henry G. Baker. 1991. <cite>Cache-Conscious Copying Collectors</cite>.</a></li>
  <li><a href="../bib/f.html#wlm92">Paul R. Wilson, Michael S. Lam, Thomas G. Moher. 1992. <cite>Caching Considerations for Generational Garbage Collection</cite>.</a></li>
  <li><a href="../bib/f.html#zorn91">Benjamin Zorn. 1991. <cite>The Effect of Garbage Collection on Cache Performance</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="caching-3" name="caching-3">caching<sup><small>(3)</small></sup></a></strong>
  (also known as memoization, tabling)</dt>
<dd><p><i>Caching</i> is a heuristic that stores answers to questions asked in the past in a <i>cache</i> or a <i>table</i>, in order that they may be more quickly answered in the future.  This process is also called memoization and tabling (by the <a href="../articles/lang.html#prolog">Prolog</a> community).</p>
<p>A "look-ahead cache" attempts to store answers to questions that will be asked soon.  A <a href="#cache-2">cache<sup><small>(2)</small></sup></a> is a common example of a cache(3).</p></dd>


<dt><strong><a id="cactus.stack" name="cactus.stack">cactus stack</a></strong>
  (also known as spaghetti stack)</dt>
<dd><p>A cactus stack is a <a href="s.html#stack">stack</a> with branches. When diagrammed, its shape resembles that of a <a href="http://www.azstarnet.com/%7Efosnp/factsaboutsaguaros.html">saguaro cactus</a>.</p>
<p>In languages that support <a href="#continuation">continuations</a>, <a href="a.html#activation.record">activation records</a> can have <a href="i.html#indefinite.extent">indefinite extent</a>.  One technique for implementing continuations is not to copy the activation records that are captured, rather to create a fork in the stack below the captured <a href="s.html#stack.frame">stack frames</a>, so that new frames appear as a parallel branch.  Often the process of forking is done lazily, captured frames are only duplicated if they are modified.</p></dd>


<dt><strong><a id="card" name="card">card</a></strong></dt>
<dd><p>A card is a division of memory, all cards being of equal size (in a
particular area of discourse).  A card is usually bigger than a <a href="w.html#word">word</a>
and smaller than a <a href="p.html#page">page</a>.  Cards are used in a technique called
<a href="#card.marking">card-marking</a> whereby <a href="d.html#dirty.bit">dirty bits</a> (which record which portions of old
generations have been written into) are maintained for each card.
Often the use of cards will also entail the use of a <a href="#crossing.map">crossing map</a>.</p></dd>

<dt><strong><a id="card.marking" name="card.marking">card marking</a>, card-marking</strong></dt>
<dd><p>A technique for managing <a href="p.html#pointer">pointer</a> <a href="s.html#store-1">stores</a> into old <a href="g.html#generation">generations</a> (which in turn is used to track <a href="i.html#inter-generational.pointer">inter-generational pointers</a>).  Each generation is divided into a number of equal-sized <a href="#card">cards</a>, and when a generation is written into, the particular card written to is recorded (often by using a <a href="b.html#bit-table">bit-table</a>).  Subsequently, when <a href="s.html#scan">scanning</a> an older generation in order to collect a younger generation, only the recorded cards (in the old generation) need to be scanned.</p>
<p><strong>See also:</strong> <a href="g.html#generational.garbage.collection">generational garbage collection</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#sobalvarro88">P. G. Sobalvarro. 1988. <cite>A Lifetime-based Garbage Collector for LISP Systems on General-Purpose Computers</cite>.</a></li>
  <li><a href="../bib/f.html#hh93">Antony L. Hosking, Richard L. Hudson. 1993. <cite>Remembered sets can also play cards</cite>.</a></li>
  <li><a href="../bib/f.html#akpy98">Alain Azagury, Elliot K. Kolodner, Erez Petrank, Zvi Yehudai. 1998. <cite>Combining Card Marking with Remembered Sets: How to Save Scanning Time</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="cell" name="cell">cell</a></strong>
  (for full details, see <a href="o.html#object">object</a>)</dt>
<dd><p>In <a href="m.html#memory.management">memory management</a>, we use the term <em>object</em> or <em>cell</em> to mean a contiguous <a href="b.html#block">block</a> of <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> forming a single logical structure.</p></dd>

<dt><strong><a id="cheney.collector" name="cheney.collector">Cheney collector</a></strong>
  (also known as Cheney scan)</dt>
<dd><p>A Cheney collector uses the new <a href="s.html#semi-space">semi-space</a> of a <a href="t.html#two-space.collector">two space collector</a> as a
queue of objects remaining to be <a href="s.html#scan">scanned</a>, thus eliminating the need for
recursion when <a href="t.html#trace">tracing</a> the <a href="g.html#graph">graph</a> of <a href="o.html#object">objects</a>.</p>
<p><strong>See also:</strong> <a href="t.html#two-space.collector">two space collector</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#cheney70">C. J. Cheney. 1970. <cite>A non-recursive list compacting algorithm</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="cheney.scan" name="cheney.scan">Cheney scan</a></strong>
  (for full details, see <a href="#cheney.collector">Cheney collector</a>)</dt>
<dd><p>A Cheney collector uses the new <a href="s.html#semi-space">semi-space</a> of a <a href="t.html#two-space.collector">two space collector</a> as a
queue of objects remaining to be <a href="s.html#scan">scanned</a>, thus eliminating the need for
recursion when <a href="t.html#trace">tracing</a> the <a href="g.html#graph">graph</a> of <a href="o.html#object">objects</a>.</p></dd>

<dt><strong><a id="closure" name="closure">closure</a></strong></dt>
<dd><p>A closure is a function or procedure that is saved along with the current bindings from enclosing blocks for later invocation.</p>
<p>Some programming languages, such as <a href="../articles/lang.html#algol">Algol</a>, permit nested blocks to access the local variables of enclosing blocks.  <a href="../articles/lang.html#lisp">Lisp</a>-like languages further permit such an inner block (in particular a function or procedure) to be saved for later invocation.  The act of saving such an inner block along with the current bindings of variables in the enclosing blocks that are referenced by the inner block, is called <em>closing over</em> or <em>capturing</em> those variables.  The object created is termed <em>a closure</em>.  A closure is invoked just like the function from which it was built, passing whatever parameters the function accepts, but when the function executes, the variables that belong to enclosing blocks will have the bindings that were in effect when the closure was created.</p>

<p><strong>Relevance to memory management:</strong> A closure is typically implemented by saving both the function and any <a href="a.html#activation.record">activation records</a> that contain variables referenced by the function.  The closure creates additional implicit <a href="r.html#reference">references</a> to the bindings closed over and hence must be accounted for in any memory management scheme.
The closure itself is an object that must be managed and may have either <a href="d.html#dynamic.extent">dynamic extent</a> or <a href="i.html#indefinite.extent">indefinite extent</a> depending on whether it is only used by inner blocks of the creating block or passed out of the creating block.</p>

<p><strong>See also:</strong> <a href="#continuation">continuation</a>.
</p></dd>

<dt><strong><a id="coalesce" name="coalesce">coalesce</a></strong></dt>
<dd><p>Coalescing is the act of merging two adjacent <a href="f.html#free.block">free blocks</a>.</p>
<p>Coalescing reduces <a href="e.html#external.fragmentation">external fragmentation</a>, but is not totally effective.</p>

<p>Coalescing can be done as soon as blocks are freed, or it can be deferred until some time later (known as <a href="d.html#deferred.coalescing">deferred coalescing</a>), or it might not be done at all.</p>

<p><a href="../bib/f.html#wil95"><cite>Dynamic Storage Allocation: A Survey and Critical Review</cite></a> has details about fragmentation, and which coalescing strategies are effective under what circumstances.</p>

<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil95">Paul R. Wilson, Mark S. Johnstone, Michael Neely, David Boles. 1995. <cite>Dynamic Storage Allocation: A Survey and Critical Review</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="collect" name="collect">collect</a></strong></dt>
<dd><p>An <a href="o.html#object">object</a> is collected when it is <a href="r.html#reclaim">reclaimed</a> by a <a href="g.html#garbage.collector">garbage collector</a>.</p>
<p><strong>Similar terms:</strong> <a href="r.html#reclaim">reclaim</a>.
</p></dd>

<dt><strong><a id="collection" name="collection">collection</a></strong>
  (for full details, see <a href="#collection.cycle">collection cycle</a>)</dt>
<dd><p>A collection cycle is a single complete execution of a <a href="t.html#tracing.garbage.collection">tracing garbage collection</a> algorithm.</p></dd>

<dt><strong><a id="collection.cycle" name="collection.cycle">collection cycle</a></strong>
  (also known as collection)</dt>
<dd><p>A collection cycle is a single complete execution of a <a href="t.html#tracing.garbage.collection">tracing garbage collection</a> algorithm.</p>
<p>Each collection cycle includes (not necessarily in strict order) choosing a <a href="#condemned.set">condemned set</a>; <a href="s.html#scan">scanning</a> <a href="r.html#root">roots</a> and <a href="o.html#object">objects</a> that have not been condemned; <a href="t.html#trace">tracing</a> the object graph to find all condemned objects that are <a href="r.html#reachable">reachable</a>; and <a href="r.html#reclaim">reclaiming</a> those that were not reachable.</p>

<p>In non-incremental garbage collection, the <a href="m.html#mutator">mutator</a> pauses at the start of a collection cycle and cannot continue until it is complete.  In <a href="i.html#incremental.garbage.collection">incremental</a> and <a href="p.html#parallel.garbage.collection">parallel</a> garbage collection, a collection cycle can be interleaved with, or simultaneous to, mutator activity.
</p></dd>


<dt><strong><a id="collector-1" name="collector-1">collector<sup><small>(1)</small></sup></a></strong>
  (for full details, see <a href="g.html#garbage.collector">garbage collector</a>)</dt>
<dd><p>A (garbage) collector is (an implementation of) a <a href="g.html#garbage.collection">garbage collection</a> algorithm.</p></dd>

<dt><strong><a id="collector-2" name="collector-2">collector<sup><small>(2)</small></sup></a></strong></dt>
<dd><p>In a <a href="g.html#garbage.collection">garbage-collected</a> system, the part that executes the garbage collection code, which discovers unused <a href="s.html#storage">storage</a> and <a href="r.html#reclaim">reclaims</a> it.</p>
<p>For purposes of describing <a href="i.html#incremental.garbage.collection">incremental garbage collection</a>, the system is divided into the <i><a href="m.html#mutator">mutator</a></i> and the <i>collector</i>.  These can be separate threads of computation, or interleaved within the same thread.</p>

<p><strong>Historical note:</strong> This term is due to Dijkstra et al.</p>

<p><strong>Opposites:</strong> <a href="m.html#mutator">mutator</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#dlmss76">E. W. Dijkstra, Leslie Lamport, A. J. Martin, C. S. Scholten, E. F. M. Steffens. 1976. <cite>On-the-fly Garbage Collection: An Exercise in Cooperation</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="color" name="color">color</a>, colour</strong></dt>
<dd><p>In a <a href="t.html#tri-color.marking">tri-color marking</a> scheme, each <a href="n.html#node">node</a> has a one of three colors: <a href="b.html#black">black</a>, <a href="w.html#white">white</a>, or <a href="g.html#gray">gray</a>.  In a <a href="t.html#treadmill">treadmill</a>, nodes may also be colored <a href="o.html#off-white">off-white</a>.</p></dd>

<dt><strong><a id="committed" name="committed">committed</a></strong>
  (for full details, see <a href="m.html#mapped">mapped</a>)</dt>
<dd><p>A range of <a href="v.html#virtual.address">virtual addresses</a> is said to be <em>mapped</em> (<em>committed</em> on Windows&reg;) if there is <a href="p.html#physical.memory-2">physical memory<sup><small>(2)</small></sup></a> associated with the range.</p></dd>

<dt><strong><a id="compactifying" name="compactifying">compactifying</a></strong>
  (for full details, see <a href="#compaction">compaction</a>)</dt>
<dd><p>Compaction is the process of <a href="m.html#moving.garbage.collector">moving</a> <a href="l.html#live">live</a> <a href="o.html#object">objects</a> to eliminate <a href="d.html#dead">dead</a> space between them.  Some people call this <i>compactifying</i>, to distinguish it from techniques for compressing data structures.</p></dd>

<dt><strong><a id="compaction" name="compaction">compaction</a></strong>
  (also known as compactifying)</dt>
<dd><p>Compaction is the process of <a href="m.html#moving.garbage.collector">moving</a> <a href="l.html#live">live</a> <a href="o.html#object">objects</a> to eliminate <a href="d.html#dead">dead</a> space between them.  Some people call this <i>compactifying</i>, to distinguish it from techniques for compressing data structures.</p>
<p>Compaction is used to avoid <a href="e.html#external.fragmentation">external fragmentation</a> and to increase <a href="l.html#locality.of.reference">locality of reference</a>.</p></dd>


<dt><strong><a id="composite.object" name="composite.object">composite object</a></strong></dt>
<dd><p>In the <a href="../articles/lang.html#postscript">PostScript</a>&reg; language, <i>composite objects</i> are the <a href="b.html#boxed">boxed</a> objects.</p>
<p>Unlike a <a href="s.html#simple.object">simple object</a>, the main data (what PostScript calls <i>the value</i>) in a composite object are stored separately, in <a href="v.html#vm-2">VM<sup><small>(2)</small></sup></a>.  Several composite objects can share the same value.</p>

<p><strong>Similar terms:</strong> <a href="b.html#boxed">boxed</a>.
<br />
<strong>Opposites:</strong> <a href="s.html#simple.object">simple object</a>.
</p></dd>

<dt><strong><a id="comprehensive" name="comprehensive">comprehensive</a></strong></dt>
<dd><p>A <a href="#collector-1">collector<sup><small>(1)</small></sup></a> is <i>comprehensive</i> if all <a href="g.html#garbage">garbage</a> (or, all <a href="u.html#unreachable">unreachable</a> <a href="o.html#object">objects</a>) is <a href="r.html#reclaim">reclaimed</a> in one <a href="#collection.cycle">collection cycle</a>.</p>
<p><strong>See also:</strong> <a href="g.html#garbage.collection">garbage collection</a>.
</p></dd>

<dt><strong><a id="concurrent.garbage.collection" name="concurrent.garbage.collection">concurrent garbage collection</a></strong>
  (for full details, see <a href="p.html#parallel.garbage.collection">parallel garbage collection</a>)</dt>
<dd><p>A parallel or concurrent <a href="#collector-2">collector<sup><small>(2)</small></sup></a> executes simultaneously with the <a href="m.html#mutator">mutator</a>, usually on a multi-processor machine.</p></dd>

<dt><strong><a id="condemned.set" name="condemned.set">condemned set</a></strong>
  (for full details, see <a href="t.html#threatened.set">threatened set</a>)</dt>
<dd><p><i>Condemned</i> <a href="o.html#object">objects</a> are those which are candidates for <a href="r.html#recycle">recycling</a> within a <a href="#collection.cycle">collection cycle</a>.</p></dd>

<dt><strong><a id="connected" name="connected">connected</a></strong></dt>
<dd><p><a href="o.html#object">Objects</a> are connected if and only if one contains a <a href="r.html#reference">reference</a> to the other.</p>
<p><strong>See also:</strong> <a href="g.html#graph">graph</a>.
</p></dd>

<dt><strong><a id="cons-1" name="cons-1">cons<sup><small>(1)</small></sup></a></strong></dt>
<dd><p>In <a href="../articles/lang.html#lisp">Lisp</a>, <code>cons</code> is a primitive operation creating a list element (from English "CONStruct").  By extension, a <em>cons</em> is the element created.</p>
<p><strong>Other links:</strong> <a href="http://www.xanalys.com/software_tools/reference/HyperSpec/Body/any_cons.html">CONS in Common Lisp HyperSpec</a>.
</p></dd>

<dt><strong><a id="cons-2" name="cons-2">cons<sup><small>(2)</small></sup></a></strong>
  (for full details, see <a href="a.html#allocate">allocate</a>)</dt>
<dd><p><i>Allocation</i> is the process of assigning resources.  When requested to by the program, an application <a href="m.html#memory.manager">memory manager</a> or <a href="a.html#allocator">allocator</a> <i>allocates</i> a <a href="b.html#block">block</a> of <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> for the program to store its data in. Allocation is also known as <i>consing</i>, from <a href="#cons-1">cons<sup><small>(1)</small></sup></a>.</p></dd>

<dt><strong><a id="conservative.garbage.collection" name="conservative.garbage.collection">conservative garbage collection</a></strong></dt>
<dd><p>In conservative <a href="g.html#garbage.collection">garbage collection</a>, the layout of <a href="o.html#object">objects</a> and <a href="r.html#root">roots</a> is not known, instead the <a href="#collector-1">collector<sup><small>(1)</small></sup></a> assumes that any field that looks like a <a href="p.html#pointer">pointer</a> <em>might</em> be a <a href="r.html#reference">reference</a>.</p>
<p>Conservative collectors can work with programs where information about the <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> layout is not available, because, for example, the language doesn't support <a href="g.html#gc">GC</a>.</p>

<p>A conservative collector doesn't need to know the <a href="f.html#format">format</a> of the objects, it just needs some idea of where the object boundaries are.  It regards any field value that looks like a pointer to an object
(or, sometimes, into the middle of one), as preventing the <a href="r.html#recycle">recycling</a> of that object.  It can't <a href="m.html#moving.garbage.collector">move</a> objects, because then the references to the moved objects would need to be updated, and such <a href="a.html#ambiguous.reference">ambiguous references</a> must not be modified, in case they weren't pointers after all.  Therefore, conservative collectors are usually <a href="m.html#mark-sweep">mark-sweep collectors</a>.</p>

<p>Because references are ambiguous, some objects may be retained despite being actually <a href="u.html#unreachable">unreachable</a>.  In practice, this happens rarely, and refinements such as <a href="b.html#blacklisting">black-listing</a> can further reduce the odds.</p>

<p><strong>Opposites:</strong> <a href="e.html#exact.garbage.collection">exact garbage collection</a>.
<br />
<strong>See also:</strong> <a href="a.html#ambiguous.root">ambiguous root</a>;
    <a href="s.html#semi-conservative.garbage.collection">semi-conservative garbage collection</a>;
    <a href="i.html#interior.pointer">interior pointer</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#bw88">Hans-J. Boehm, Mark Weiser. 1988. <cite>Garbage collection in an uncooperative environment</cite>.</a></li>
  <li><a href="../bib/f.html#boehm93">Hans-J. Boehm. 1993. <cite>Space Efficient Conservative Garbage Collection</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="constructor-1" name="constructor-1">constructor<sup><small>(1)</small></sup></a></strong></dt>
<dd><p>A constructor is a function or method that <a href="a.html#allocate">allocates</a> and initializes an <a href="o.html#object">object</a>.</p>
<p><strong>Opposites:</strong> <a href="d.html#destructor-1">destructor<sup><small>(1)</small></sup></a>.
</p></dd>

<dt><strong><a id="constructor-2" name="constructor-2">constructor<sup><small>(2)</small></sup></a></strong></dt>
<dd><p>In <a href="../articles/lang.html#cplusplus">C++</a>, a <em>constructor</em> is a member function that is used to initialize a newly-<a href="a.html#allocate">allocated</a> object.</p>
<p>The actual allocation of <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> is performed by <code>operator new</code> or the compiler (for <a href="s.html#static.allocation">static</a> and <a href="s.html#stack.allocation">stack allocation</a>), and the new <a href="b.html#block">block</a> is then passed to the appropriate constructor.</p>

<p><strong>See also:</strong> <a href="d.html#destructor-2">destructor<sup><small>(2)</small></sup></a>.
</p></dd>

<dt><strong><a id="continuation" name="continuation">continuation</a></strong></dt>
<dd><p>A continuation is the data required to restore an execution context after invocation of another context, typically as a subroutine.</p>
<p><strong>Relevance to memory management:</strong> If continuations can be represented as first-class objects, as in <a href="../articles/lang.html#scheme">Scheme</a>, the execution contexts can no longer be stored on a <a href="s.html#stack">stack</a>, instead, (at least some) <a href="a.html#activation.record">activation records</a> have to be <a href="h.html#heap.allocation">heap-allocated</a>.</p>

<p><strong>See also:</strong> <a href="#closure">closure</a>.
</p></dd>

<dt><strong><a id="control.stack" name="control.stack">control stack</a></strong>
  (also known as activation stack, execution stack)</dt>
<dd><p>A <a href="s.html#stack">stack</a> that stores <a href="a.html#activation.record">activation records</a>, particularly subroutine return information, is known as a <em>control stack</em>.</p>
<p>Typically the control stack is supported and used by the hardware architecture and the operating system, limiting the types and sizes of <a href="o.html#object">objects</a> that can be stored on it.  Often, only one type of object, a <a href="s.html#stack.frame">stack frame</a>, is permitted, and the layout of that is defined by the hardware architecture.</p>

<p><strong>Relevance to memory management:</strong> Theoretically, a control stack is simply an array of activation records, and hence just another object managed by the <a href="m.html#memory.manager">memory manager</a>.  In practice, the control stack is central to the performance of the hardware architecture and may require special treatment.  In particular, it may not be accessible as ordinary <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a>, or it may have its own <a href="#cache-2">cache<sup><small>(2)</small></sup></a> with specific updating requirements.</p>

<p><strong>Similar terms:</strong> <a href="s.html#stack">stack</a>.
<br />
<strong>See also:</strong> <a href="d.html#data.stack">data stack</a>.
</p></dd>

<dt><strong><a id="copying.garbage.collection" name="copying.garbage.collection">copying garbage collection</a></strong>
  (also known as scavenging garbage collection)</dt>
<dd><p>Copying garbage collection is a kind of <a href="t.html#tracing.garbage.collection">tracing garbage collection</a> that operates by <a href="r.html#relocation">relocating</a> <a href="r.html#reachable">reachable</a> <a href="o.html#object">objects</a> (this is sometimes called <em>scavenging</em>) and then <a href="r.html#reclaim">reclaiming</a> objects that are left behind, which must be <a href="u.html#unreachable">unreachable</a> and therefore <a href="d.html#dead">dead</a>.</p>
<p>A copying garbage collection relies on being able to find and correct all <a href="r.html#reference">references</a> to copied objects.</p>

<p align="center"><em>Copying garbage collection</em><br /><img alt="Diagram: Copying garbage collection" src="../diagrams/copying.png" border="2" height="249" width="373" /></p>

<p><strong>Similar terms:</strong> <a href="m.html#moving.garbage.collector">moving</a>.
<br />
<strong>See also:</strong> <a href="b.html#broken.heart">broken heart</a>;
    <a href="f.html#forwarding.pointer">forwarding pointer</a>;
    <a href="t.html#two-space.collector">two-space collector</a>.
</p></dd>

<dt><strong><a id="core" name="core">core</a></strong></dt>
<dd><p>A historical synonym for <a href="m.html#main.memory">main memory</a>, deriving from the <i>cores</i> or ferrite rings which were once the main technology used to implement main memory.</p>
<p><strong>Similar terms:</strong> <a href="m.html#main.memory">main memory</a>.
</p></dd>

<dt><strong><a id="creation.space" name="creation.space">creation space</a></strong></dt>
<dd><p>In <a href="g.html#generational.garbage.collection">generational garbage collection</a>, when <a href="g.html#generation">generations</a> are divided into <a href="b.html#bucket">buckets</a>, the creation space is where new <a href="o.html#object">objects</a> are created in each generation.</p>
<p>This term is sometimes used as a synonym for <a href="n.html#nursery.space">nursery space</a>.</p>

<p><strong>Opposites:</strong> <a href="a.html#aging.space">aging space</a>.
<br />
<strong>See also:</strong> <a href="g.html#generational.garbage.collection">generational garbage collection</a>.
</p></dd>

<dt><strong><a id="crossing.map" name="crossing.map">crossing map</a></strong></dt>
<dd><p>Where <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> has already been divided into some fixed-sized unit (for example, <a href="p.html#page">pages</a> or <a href="#card">cards</a>), a crossing map records where <a href="o.html#object">objects</a> lie across the boundaries of the fixed-sized units.  In other words, which fixed-sized units do not start with the beginning of an object.</p>
<p>A system which implements <a href="r.html#remembered.set">remembered sets</a> by <a href="p.html#page.marking">page-marking</a> or <a href="#card.marking">card-marking</a> needs to scan all the <a href="p.html#pointer">pointers</a> in the page or card.  If the system can not <a href="s.html#scan">scan</a> partial objects (or requires information in the object <a href="h.html#header">header</a> in order to scan a partial object), a crossing map is necessary to find the beginning of the first object in the unit.</p>

<p><strong>Relevance to memory management:</strong> In a sense, a crossing map is an optimization of <a href="t.html#tagged.architecture">tagged architecture</a>.  It represents the minimum information necessary to determine how to interpret any word of memory.</p></dd>


<dt><strong><a id="cyclic.data.structure" name="cyclic.data.structure">cyclic data structure</a></strong></dt>
<dd><p>A data structure is cyclic if some of its <a href="r.html#reference">references</a> form a loop; that is, there's an <a href="o.html#object">object</a> that can be reached by following references from itself.</p></dd>

</dl>
<p align="center"><a href="a.html">A</a>
<a href="b.html">B</a>
<strong>C</strong>
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