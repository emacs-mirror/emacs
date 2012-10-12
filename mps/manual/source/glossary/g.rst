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
<link rel="prev" href="f.html" />
<link rel="next" href="h.html" />
<title>The Memory Management Glossary: G</title>
</head>
<body lang="en" bgcolor="#FFFFFF" text="#000000" link="#000099" vlink="#660066" alink="#FF0000">
<h1 align="center">The Memory Management Glossary<br />
<big>G</big></h1>
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
<strong>G</strong>
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
<dt><strong><a id="garbage" name="garbage">garbage</a></strong></dt>
<dd><p>Garbage is <a href="o.html#object">objects</a> that are <a href="d.html#dead">dead</a>.</p>
<p>In <a href="t.html#tracing.garbage.collection">tracing garbage collection</a>, the term is sometimes used to mean objects that are known to be dead; that is, objects that are <a href="u.html#unreachable">unreachable</a>.</p></dd>


<dt><strong><a id="garbage.collection" name="garbage.collection">garbage collection</a></strong>
  (also known as GC)</dt>
<dd><p>Garbage collection (GC), also known as <em>automatic memory management</em>, is the automatic <a href="r.html#recycle">recycling</a> of <a href="h.html#heap.allocation">dynamically allocated</a> <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a>.  Garbage collection is performed by a <a href="#garbage.collector">garbage collector</a> which recycles memory that it can prove will never be used again.  Systems and languages which use garbage collection can be described as <em>garbage-collected</em>.</p>
<p>Garbage collection is a tried and tested memory management technique that has been in use since its invention in the 1950s.  GC avoids the need for the programmer to <a href="d.html#deallocate">deallocate</a> memory <a href="b.html#block">blocks</a> explicitly, thus avoiding a number of problems: <a href="m.html#memory.leak">memory leaks</a>, <a href="d.html#double.free">double frees</a>, and <a href="p.html#premature.free">premature frees</a>.  The burden on the programmer is reduced by not having to investigate such problems, thereby increasing productivity.</p>

<p>Garbage collection can also dramatically simplify programs, chiefly by allowing modules to present cleaner interfaces to each other: the management of object storage between modules is unnecessary.</p>

<p>It is not possible, in general, for a <a href="#garbage.collector">garbage collector</a> to determine exactly which <a href="o.html#object">objects</a> are still <a href="l.html#live">live</a>.  Even if it didn't depend on future input, there can be no general algorithm to prove that an object is live (cf. the Halting Problem).  All garbage collectors use some efficient approximation to liveness.  In <a href="t.html#tracing.garbage.collection">tracing garbage collection</a>, the approximation is that an object can't be live unless it is <a href="r.html#reachable">reachable</a>.  In <a href="r.html#reference.counting">reference counting</a>, the approximation is that an object can't be live unless it is <a href="r.html#reference">referenced</a>.  Hybrid algorithms are also possible.  Often the term <em>garbage collection</em> is used narrowly to mean only tracing garbage collection.</p>

<p>There is a large body of published work on particular and general GC algorithms.</p>

<p><strong>Historical note:</strong> Garbage collection was first invented by John McCarthy in 1958 as part of the implementation of <a href="../articles/lang.html#lisp">Lisp</a>.</p>

<p>Other significant languages offering GC include <a href="../articles/lang.html#java">Java</a>, <a href="../articles/lang.html#perl">Perl</a>, <a href="../articles/lang.html#modula-3">Modula-3</a>, <a href="../articles/lang.html#prolog">Prolog</a>, <a href="../articles/lang.html#ml">ML</a>, and <a href="../articles/lang.html#smalltalk">Smalltalk</a>.  Major applications using GC include Emacs and AutoCAD; usually, you can't tell whether an application does or not, but these have extension languages that expose the fact.</p>

<p><strong>Similar terms:</strong> <a href="a.html#automatic.memory.management">automatic memory management</a>.
<br />
<strong>Opposites:</strong> <a href="m.html#manual.memory.management">manual memory management</a>.
<br />
<strong>See also:</strong> <a href="c.html#conservative.garbage.collection">conservative garbage collection</a>;
    <a href="c.html#copying.garbage.collection">copying garbage collection</a>;
    <a href="d.html#distributed.garbage.collection">distributed garbage collection</a>;
    <a href="#generational.garbage.collection">generational garbage collection</a>;
    <a href="i.html#incremental.garbage.collection">incremental garbage collection</a>;
    <a href="p.html#parallel.garbage.collection">parallel garbage collection</a>.
<br />
<strong>Other links:</strong> <a href="../articles/begin.html">Beginner's Guide</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#mccarthy60">J. McCarthy. 1960. <cite>Recursive Functions of Symbolic Expressions and Their Computation by Machine</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="garbage.collector" name="garbage.collector">garbage collector</a></strong>
  (also known as collector(1))</dt>
<dd><p>A (garbage) collector is (an implementation of) a <a href="#garbage.collection">garbage collection</a> algorithm.</p>
<p>This term is often used when referring to particular implementations or algorithms, e.g., "the Boehm-Demers-Weiser <em>collector</em>".</p></dd>


<dt><strong><a id="gb" name="gb">GB</a></strong>
  (for full details, see <a href="#gigabyte">gigabyte</a>)</dt>
<dd><p>A gigabyte is 1024 <a href="m.html#megabyte">megabytes</a>, or 1073741824 <a href="b.html#byte-1">bytes<sup><small>(1)</small></sup></a>.</p></dd>

<dt><strong><a id="gc" name="gc">GC</a></strong>
  (for full details, see <a href="#garbage.collection">garbage collection</a>)</dt>
<dd><p>Garbage collection (GC), also known as <em>automatic memory management</em>, is the automatic <a href="r.html#recycle">recycling</a> of <a href="h.html#heap.allocation">dynamically allocated</a> <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a>.  Garbage collection is performed by a <a href="#garbage.collector">garbage collector</a> which recycles memory that it can prove will never be used again.  Systems and languages which use garbage collection can be described as <em>garbage-collected</em>.</p></dd>

<dt><strong><a id="general.protection.fault" name="general.protection.fault">General Protection Fault</a></strong>
  (also known as GPF)</dt>
<dd><p>A General Protection Fault on the 16-bit Windows<sup><small class="tm-small">TM</small></sup> platforms is the equivalent of a <a href="s.html#segmentation.violation">segmentation violation</a> on UNIX&reg;.</p></dd>

<dt><strong><a id="generation" name="generation">generation</a></strong></dt>
<dd><p>A generation is a set of <a href="o.html#object">objects</a> of similar <i>age</i>.</p>
<p>A <a href="#generational.garbage.collection">generational garbage collector</a> will typically divide the set of all objects into generations, and <a href="t.html#threatened.set">condemn</a> all the objects in a generation together. Rather than allowing whole generations to age, the <a href="c.html#collector-1">collector<sup><small>(1)</small></sup></a> can <a href="p.html#promotion">promote</a> objects into older generations as they survive successive <a href="c.html#collection.cycle">collection cycles</a>.</p>

<p>New objects are usually allocated in the youngest or <a href="n.html#nursery.generation">nursery generation</a>, but if we know that particular objects will be long-lived, we might want to allocate them directly in an older generation. Thus, more loosely, a generation is a set of objects which have similar expected <a href="l.html#lifetime">lifetimes</a>.</p>

<p><strong>See also:</strong> <a href="b.html#bucket">bucket</a>.
</p></dd>

<dt><strong><a id="generation.scavenging" name="generation.scavenging">generation scavenging</a></strong>
  (for full details, see <a href="#generational.garbage.collection">generational garbage collection</a>)</dt>
<dd><p>Generational garbage collection is <a href="t.html#tracing.garbage.collection">tracing garbage collection</a> that makes use of the
<a href="#generational.hypothesis">generational hypothesis</a>.  <a href="o.html#object">Objects</a> are gathered together in <a href="#generation">generations</a>.
New objects are allocated in the <i>youngest</i> or <i>nursery</i> generation, and
<a href="p.html#promotion">promoted</a> to <i>older</i> generations if they survive.  Objects in older
generations are <a href="t.html#threatened.set">condemned</a> less frequently, saving CPU time.</p></dd>

<dt><strong><a id="generational.garbage.collection" name="generational.garbage.collection">generational garbage collection</a></strong>
  (also known as generation scavenging)</dt>
<dd><p>Generational garbage collection is <a href="t.html#tracing.garbage.collection">tracing garbage collection</a> that makes use of the
<a href="#generational.hypothesis">generational hypothesis</a>.  <a href="o.html#object">Objects</a> are gathered together in <a href="#generation">generations</a>.
New objects are allocated in the <i>youngest</i> or <i>nursery</i> generation, and
<a href="p.html#promotion">promoted</a> to <i>older</i> generations if they survive.  Objects in older
generations are <a href="t.html#threatened.set">condemned</a> less frequently, saving CPU time.</p>
<p>It is typically rare for an object to refer to a younger object.
Hence, objects in one generation typically have few <a href="r.html#reference">references</a> to objects in younger generations.
This means that the <a href="s.html#scan">scanning</a> of old generations in the course of collecting younger generations can be done more efficiently by means of <a href="r.html#remembered.set">remembered sets</a>.</p>

<p>In some purely functional languages (that is, without update), all references are backwards in time, in which case remembered sets are unnecessary.</p>

<p><strong>See also:</strong> <a href="r.html#remembered.set">remembered set</a>.
</p></dd>

<dt><strong><a id="generational.hypothesis" name="generational.hypothesis">generational hypothesis</a></strong>
  (also known as infant mortality)</dt>
<dd><p><em>Infant mortality</em> or <em>the generational hypothesis</em> is the observation that, in most cases, young <a href="o.html#object">objects</a> are much more likely to <a href="d.html#dead">die</a> than old objects.</p>
<p>Strictly, the hypothesis is that the probability of death as a function of age falls faster than exponential decay (inverse hyper-exponential), but this strict condition is not always required for techniques such as <a href="#generational.garbage.collection">generational garbage collection</a> to be useful.</p></dd>


<dt><strong><a id="gigabyte" name="gigabyte">gigabyte</a></strong>
  (also known as GB)</dt>
<dd><p>A gigabyte is 1024 <a href="m.html#megabyte">megabytes</a>, or 1073741824 <a href="b.html#byte-1">bytes<sup><small>(1)</small></sup></a>.</p>
<p>See <a href="b.html#byte-1">byte<sup><small>(1)</small></sup></a> for general information on this and related quantities.</p></dd>


<dt><strong><a id="good.fit" name="good.fit">good fit</a></strong></dt>
<dd><p>The class of <a href="a.html#allocation.policy">allocation policies</a> which approximate <a href="b.html#best.fit">best fit</a>.
Strict best fit may be costly to implement (depending on the details
of the <a href="a.html#allocation.mechanism">allocation mechanism</a>), so some implementors approximate it,
choosing a block which is close in size to the allocation request.</p>
<p><strong>See also:</strong> <a href="b.html#best.fit">best fit</a>;
    <a href="a.html#allocation.policy">allocation policy</a>;
    <a href="n.html#next.fit">next fit</a>;
    <a href="w.html#worst.fit">worst fit</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil95">Paul R. Wilson, Mark S. Johnstone, Michael Neely, David Boles. 1995. <cite>Dynamic Storage Allocation: A Survey and Critical Review</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="gpf" name="gpf">GPF</a></strong>
  (for full details, see <a href="#general.protection.fault">General Protection Fault</a>)</dt>
<dd><p>A General Protection Fault on the 16-bit Windows<sup><small class="tm-small">TM</small></sup> platforms is the equivalent of a <a href="s.html#segmentation.violation">segmentation violation</a> on UNIX&reg;.</p></dd>

<dt><strong><a id="grain" name="grain">grain</a></strong></dt>
<dd><p>The grain of a platform is the smallest <a href="a.html#alignment">alignment</a> that is sufficient to
accommodate all data accesses on that platform.  Often this is a <a href="w.html#word">word</a> or
a small multiple of a word.  Double precision floating point numbers often
have the strictest alignment requirements.</p>
<p><strong>See also:</strong> <a href="a.html#alignment">alignment</a>;
    <a href="w.html#word">word</a>.
</p></dd>

<dt><strong><a id="graph" name="graph">graph</a></strong></dt>
<dd><p>A graph is a set of <a href="n.html#node">nodes</a> together with a set of <a href="e.html#edge">edges</a> connecting nodes. </p>
<p>If the edges have direction like arrows (for example, <a href="r.html#reference">references</a> in a graph of <a href="o.html#object">objects</a>), then the graph is said to be a <em>directed graph</em>.</p>

<p align="center"><em>Directed graph</em><br /><img alt="Diagram: Directed graph" src="../diagrams/graph.png" border="2" height="205" width="268" /></p>

<p><strong>Relevance to memory management:</strong> Graphs are used to model <a href="r.html#reachable">reachability</a> for <a href="t.html#tracing.garbage.collection">tracing garbage collection</a>.  The <a href="o.html#object">objects</a> are considered to form a graph, with the nodes of the graph being the objects and the edges of the graph being the references from one object to another.  Usually, there is a single, distinguished <a href="r.html#root">root</a> to which the <a href="m.html#mutator">mutator</a> has <em>direct</em> access, and the nodes strongly connected to it are the reachable modes.</p></dd>


<dt><strong><a id="gray" name="gray">gray</a>, grey</strong></dt>
<dd><p>In a <a href="t.html#tri-color.marking">tri-color marking</a> scheme, gray <a href="o.html#object">objects</a> are objects that are proved or assumed (see <a href="#generational.garbage.collection">generational</a> and <a href="t.html#threatened.set">condemn</a>) to be <a href="r.html#reachable">reachable</a>, but have not yet been <a href="s.html#scan">scanned</a>.</p>
<p>More precisely, gray objects have been noted reachable, but must still be visited by the <a href="c.html#collector-2">collector<sup><small>(2)</small></sup></a> in order to process their children.</p>

<p><strong>Similar terms:</strong> <a href="#gray.list">gray list</a>.
<br />
<strong>Opposites:</strong> <a href="b.html#black">black</a>;
    <a href="w.html#white">white</a>.
</p></dd>

<dt><strong><a id="gray.list" name="gray.list">gray list</a>, grey list</strong></dt>
<dd><p>The gray list is the set of <a href="o.html#object">objects</a> that a <a href="t.html#tracing.garbage.collection">tracing garbage collector</a> has noted <a href="r.html#reachable">reachable</a>,
but hasn't <a href="s.html#scan">scanned</a> yet.</p>
<p>The gray list is so called because it corresponds to the set of <a href="#gray">gray</a> objects in the <a href="t.html#tri-color.marking">tri-color marking</a> model of graph tracing.  The gray list changes as the garbage collector progresses.</p>

<p>Each gray object is <a href="s.html#scan">scanned</a>, and all <a href="w.html#white">white</a> objects referred to by it become gray and are added to the list.  Scanning a gray object turns it <a href="b.html#black">black</a>.  When the gray list is empty, the tracing is finished, and white objects may be <a href="r.html#reclaim">reclaimed</a>.</p>

<p>The representation of the gray list is a key part of garbage collector design.  The size of the list is potentially proportional to the size of the <a href="h.html#heap">heap</a>, and the operation of finding the next gray object to scan must be cheap.</p>

<p><strong>See also:</strong> <a href="c.html#cheney.scan">Cheney scan</a>.
</p></dd>

</dl>
<p align="center"><a href="a.html">A</a>
<a href="b.html">B</a>
<a href="c.html">C</a>
<a href="d.html">D</a>
<a href="e.html">E</a>
<a href="f.html">F</a>
<strong>G</strong>
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