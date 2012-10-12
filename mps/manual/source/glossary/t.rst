<?xml version="1.0" encoding="iso-8859-1"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
        "http://www.w3.org/TR/2000/REC-xhtml1-20000126/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
<link rev="MADE" href="mailto:mm-web@ravenbrook.com" />
<link rel="Start" href="../" />
<link rel="Contents" href="../" />
<link rel="Glossary" href="" />
<meta name="DC.Date" content="2000-12-15T20:17:00" />
<meta name="description" content="Glossary of terms related to memory management" />
<meta name="keywords" content="dictionary, glossary, definition, memory management, garbage collection" />
<link rel="index" href="./" />
<link rel="help" href="help.html" />
<link rel="prev" href="s.html" />
<link rel="next" href="u.html" />
<title>The Memory Management Glossary: T</title>
</head>
<body lang="en" bgcolor="#FFFFFF" text="#000000" link="#000099" vlink="#660066" alink="#FF0000">
<h1 align="center">The Memory Management Glossary<br />
<big>T</big></h1>
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
<a href="s.html">S</a>
<strong>T</strong>
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
<dt><strong><a id="tabling" name="tabling">tabling</a></strong>
  (for full details, see <a href="c.html#caching-3">caching<sup><small>(3)</small></sup></a>)</dt>
<dd><p><i>Caching</i> is a heuristic that stores answers to questions asked in the past in a <i>cache</i> or a <i>table</i>, in order that they may be more quickly answered in the future.  This process is also called memoization and tabling (by the <a href="../articles/lang.html#prolog">Prolog</a> community).</p></dd>

<dt><strong><a id="tag" name="tag">tag</a></strong></dt>
<dd><p>A tag is a piece of information associated with an <a href="o.html#object">object</a> or <a href="r.html#reference">reference</a> that allows the representation of the object to be determined.</p>
<p>Tags are often used to represent types in the implementation of a dynamically-typed language.
In statically-typed languages, types are usually implicit and not permitted to change at run-time, so tagging is rarely required.</p>

<p>One of the simplest forms of tag is a <a href="w.html#word">word</a> at the beginning of the object that points to a block of information about the object's <a href="f.html#format">format</a>.</p>

<p align="center"><em>Example of a tag-word at the start of an object</em><br /><img alt="Diagram: Example of a tag-word at the start of an object" src="../diagrams/tag-word.png" border="2" height="138" width="320" /></p>

<p>Another common form of tagging is to <a href="a.html#alignment">align</a> objects and keep information in the least significant bits of the <a href="a.html#address">address</a>.</p>

<p align="center"><em>Example of reference tagging, using the least significant bits</em><br /><img alt="Diagram: Example of reference tagging, using the least significant bits" src="../diagrams/tag-ref.png" border="2" height="191" width="294" /></p>

<p>In <a href="../articles/lang.html#c">C</a>, when a structure contains a union, it is common to add a field to the structure to indicate which union member is currently being used.  This field is known as a <em>discriminator</em>, and is a form of tag.  Analogues occur in other languages, sometimes with compiler or run-time support.</p>

<p><strong>See also:</strong> <a href="#tagged.architecture">tagged architecture</a>;
    <a href="h.html#header">header</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#gudeman93">David Gudeman. 1993. <cite>Representing Type Information in Dynamically Typed Languages</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="tagged.architecture" name="tagged.architecture">tagged architecture</a></strong></dt>
<dd><p>A tagged architecture is a hardware architecture where each memory <a href="w.html#word">word</a> is divided into a "data" and a <a href="#tag">tag</a> section.  The data section is sufficiently large to contain a memory <a href="a.html#address">address</a> and the tag section is used to describe how the data section is to be interpreted (that is, it encodes the type of the data).</p>
<p><strong>Relevance to memory management:</strong> Tagged architectures greatly simplify the implementation of a memory manager because each word of memory is self-describing.</p>

<p><strong>Historical note:</strong> The <a href="../articles/lang.html#lisp">Lisp</a> Machine is an example of a tagged architecture.</p></dd>


<dt><strong><a id="tb-1" name="tb-1">TB<sup><small>(1)</small></sup></a></strong>
  (for full details, see <a href="#terabyte">terabyte</a>)</dt>
<dd><p>A terabyte is 1024 <a href="g.html#gigabyte">gigabytes</a>, or 1099511627776 <a href="b.html#byte-1">bytes<sup><small>(1)</small></sup></a>.</p></dd>

<dt><strong><a id="tb-2" name="tb-2">TB<sup><small>(2)</small></sup></a></strong>
  (for full details, see <a href="#tlb">TLB</a>)</dt>
<dd><p>The <em>translation lookaside buffer</em> or <em>address translation cache</em> is small piece of associative <a href="m.html#memory-1">memory<sup><small>(1)</small></sup></a> within a processor which caches part of the translation from <a href="v.html#virtual.address">virtual addresses</a> to <a href="p.html#physical.address">physical addresses</a>.</p></dd>

<dt><strong><a id="tenuring" name="tenuring">tenuring</a></strong>
  (for full details, see <a href="p.html#promotion">promotion</a>)</dt>
<dd><p>Promotion or tenuring is the act of moving an <a href="o.html#object">object</a> from its current <a href="g.html#generation">generation</a> to an <em>older</em> one (one that contains objects that are expected to survive longer).</p></dd>

<dt><strong><a id="terabyte" name="terabyte">terabyte</a></strong>
  (also known as TB(1))</dt>
<dd><p>A terabyte is 1024 <a href="g.html#gigabyte">gigabytes</a>, or 1099511627776 <a href="b.html#byte-1">bytes<sup><small>(1)</small></sup></a>.</p>
<p>See <a href="b.html#byte-1">byte<sup><small>(1)</small></sup></a> for general information on this and related quantities.</p></dd>


<dt><strong><a id="termination" name="termination">termination</a></strong>
  (for full details, see <a href="f.html#finalization">finalization</a>)</dt>
<dd><p>In <a href="g.html#garbage.collection">garbage-collected</a> languages, it is often necessary to perform actions on some <a href="o.html#object">objects</a> after they are no longer in use and before their <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> can be <a href="r.html#recycle">recycled</a>.  These actions are known as <em>finalization</em> or <em>termination</em>.</p></dd>

<dt><strong><a id="thrash" name="thrash">thrash</a></strong></dt>
<dd><p>A <a href="c.html#cache-2">cache<sup><small>(2)</small></sup></a> is said to <a href="#thrash">thrash</a> when its <a href="m.html#miss.rate">miss rate</a> is too high, and it spends most of its time servicing <a href="m.html#miss">misses</a>.  Thrashing is bad for performance, particularly <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> thrashing, because the relative cost of a miss is so high: it may slow a machine down by a factor of a hundred or more.</p>
<p>Thrashing is typically caused by a process or system having a <a href="w.html#working.set">working set</a> which is larger than its <a href="c.html#cache-1">cache<sup><small>(1)</small></sup></a> or <a href="m.html#main.memory">main memory</a>.  It may also be caused by a failure of <a href="c.html#cache.policy">cache policy</a>.  A system with an inflexible cache policy may thrash even when the working set is quite small.</p>

<p>For instance, a virtual memory system which has four megabytes of <a href="p.html#physical.memory-1">physical memory<sup><small>(1)</small></sup></a> but which has a working set of ten megabytes will <a href="#thrash">thrash</a> badly.</p>

<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#denning68">P. J. Denning. 1968. <cite>Thrashing: Its Causes and Prevention</cite>.</a></li>
  <li><a href="../bib/f.html#denning70">P. J. Denning. 1970. <cite>Virtual Memory</cite>.</a></li>
  <li><a href="../bib/f.html#ds72">P. J. Denning, S. C. Schwartz. 1972. <cite>Properties of the Working-set Model</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="threatened.set" name="threatened.set">threatened set</a></strong>
  (also known as condemned set)</dt>
<dd><p><i>Condemned</i> <a href="o.html#object">objects</a> are those which are candidates for <a href="r.html#recycle">recycling</a> within a <a href="c.html#collection.cycle">collection cycle</a>.</p>
<p>At the start of a collection cycle, the <a href="c.html#collector-1">collector<sup><small>(1)</small></sup></a> may choose to condemn some objects (the <i>condemned set</i> or <i>threatened set</i>) but not to condemn others (the <a href="i.html#immune.set">immune set</a>). Objects that are not condemned are assumed to be <a href="a.html#alive">alive</a> and behave as <a href="r.html#root">roots</a> for the purposes of that collection cycle.</p>

<p>Many simple <a href="#tracing.garbage.collection">tracing garbage collection</a> algorithms begin by condemning all objects, but <a href="g.html#generational.garbage.collection">generational garbage collectors</a> will condemn individual <a href="g.html#generation">generations</a> or combinations of generations. Often young generations are condemned but older ones are not, because objects in older generations are less likely to have become <a href="u.html#unreachable">unreachable</a>.</p>

<p>In collectors using <a href="#tri-color.marking">tri-color marking</a>, at the start of a collection cycle the condemned set is exactly the set of objects that the collector colors <a href="w.html#white">white</a>.</p>

<p><strong>Opposites:</strong> <a href="i.html#immune.set">immune set</a>.
</p></dd>

<dt><strong><a id="tlb" name="tlb">TLB</a>, translation lookaside buffer</strong>
  (also known as TB(2), translation buffer, ATC, address translation cache)</dt>
<dd><p>The <em>translation lookaside buffer</em> or <em>address translation cache</em> is small piece of associative <a href="m.html#memory-1">memory<sup><small>(1)</small></sup></a> within a processor which caches part of the translation from <a href="v.html#virtual.address">virtual addresses</a> to <a href="p.html#physical.address">physical addresses</a>.</p>
<p>In a <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> system there is a translation from <a href="v.html#virtual.address">virtual addresses</a> to <a href="p.html#physical.address">physical addresses</a>.  This translation can often be very large and complex and the data structures that implement the translation (often a <a href="p.html#page.table">page-table</a>) can be too large to store efficiently on the processor.  Instead, a few elements of the translation are stored in the TLB; the processor can access the TLB extremely quickly.  If a required translation for a particular virtual address is not present in the TLB then <em>a TLB miss</em> is taken and the address is resolved using the more general mechanism.</p></dd>


<dt><strong><a id="trace" name="trace">trace</a></strong></dt>
<dd><p>In <a href="#tracing.garbage.collection">tracing garbage collection</a>, tracing is the process of following the <a href="g.html#graph">graph</a> from all <a href="r.html#root">roots</a>
 to all <a href="r.html#reachable">reachable</a> data.</p>
<p><strong>Similar terms:</strong> <a href="s.html#scan">scan</a>.
</p></dd>

<dt><strong><a id="tracing.garbage.collection" name="tracing.garbage.collection">tracing garbage collection</a></strong></dt>
<dd><p>Tracing garbage collection is <a href="g.html#garbage.collection">garbage collection</a> based on <a href="r.html#reachable">reachability</a>.</p>
<p>Tracing garbage collection relies on the fact that if an <a href="o.html#object">object</a> is not <a href="r.html#reachable">reachable</a>, there is no way the <a href="m.html#mutator">mutator</a> could ever access it, and therefore it cannot be <a href="a.html#alive">alive</a>. In each <a href="c.html#collection.cycle">collection cycle</a>, some or all of the objects are <a href="#threatened.set">condemned</a> and the <a href="g.html#graph">graph</a> is <a href="#trace">traced</a> to find which of the condemned objects are reachable. Those that were not reachable may be <a href="r.html#reclaim">reclaimed</a>.</p></dd>


<dt><strong><a id="translation.buffer" name="translation.buffer">translation buffer</a></strong>
  (for full details, see <a href="#tlb">TLB</a>)</dt>
<dd><p>The <em>translation lookaside buffer</em> or <em>address translation cache</em> is small piece of associative <a href="m.html#memory-1">memory<sup><small>(1)</small></sup></a> within a processor which caches part of the translation from <a href="v.html#virtual.address">virtual addresses</a> to <a href="p.html#physical.address">physical addresses</a>.</p></dd>

<dt><strong><a id="transport" name="transport">transport</a></strong></dt>
<dd><p>In a <a href="c.html#copying.garbage.collection">copying collector</a>, transporting is preventing an <a href="o.html#object">object</a> in the <a href="c.html#condemned.set">condemned set</a> from being collected by copying it and adjusting the <a href="r.html#reference">reference</a> by which it was discovered to point to the new copy.</p>
<p><strong>See also:</strong> <a href="c.html#copying.garbage.collection">scavenging</a>;
    <a href="s.html#snap-out">snap-out</a>.
</p></dd>

<dt><strong><a id="transport.snap-out" name="transport.snap-out">transport snap-out</a></strong>
  (for full details, see <a href="s.html#snap-out">snap-out</a>)</dt>
<dd><p>In a <a href="c.html#copying.garbage.collection">copying collector</a>, when there is a <a href="r.html#reference">reference</a> to an <a href="o.html#object">object</a> that was <a href="#threatened.set">condemned</a>, but has been <a href="#transport">transported</a>, snap-out is the adjustment of that reference to point to the preserved copy.</p></dd>

<dt><strong><a id="treadmill" name="treadmill">treadmill</a></strong></dt>
<dd><p>Henry Baker has devised an <a href="i.html#incremental.garbage.collection">incremental</a> non-<a href="m.html#moving.garbage.collector">moving</a> <a href="g.html#garbage.collector">garbage collector</a> that uses a circular doubly-linked list, called the treadmill, to implement <a href="#tri-color.marking">tri-color marking</a>.</p>
<p>Every <a href="o.html#object">object</a> is on the list.  The list has four sections corresponding to <a href="c.html#color">colors</a>. The <a href="b.html#black">black</a>, <a href="g.html#gray">gray</a> and <a href="w.html#white">white</a> sections are used for tri-color marking,  and an additional <a href="o.html#off-white">off-white</a> section is used for <a href="f.html#free-3">free<sup><small>(3)</small></sup></a> objects.  The color of an object is changed by unlinking it from the list and relinking it to a different part of the list.</p>

<p align="center"><em>A treadmill</em><br /><img alt="Diagram: A treadmill" src="../diagrams/treadmill.png" border="2" height="394" width="378" /></p>

<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#baker92c">Henry G. Baker. 1992. <cite>The Treadmill: Real-Time Garbage Collection Without Motion Sickness</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="tri-color.invariant" name="tri-color.invariant">tri-color invariant</a>, tri-colour invariant, tricolor invariant, tricolour invariant</strong></dt>
<dd><p>The term "tri-color invariant" is used to refer to any of a number of properties of a <a href="r.html#reference">reference</a> <a href="g.html#graph">graph</a> that are preserved throughout a <a href="#tri-color.marking">tri-color marking</a> algorithm to ensure the correctness.</p>
<p>There are two important ones: the <a href="s.html#strong.tri-color.invariant">strong tri-color invariant</a> and the <a href="w.html#weak.tri-color.invariant">weak tri-color invariant</a>.  When people say "the tri-color invariant" they probably mean the strong one.</p>

<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#pirinen98">Pekka P. Pirinen. 1998. <cite>Barrier techniques for incremental tracing</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="tri-color.marking" name="tri-color.marking">tri-color marking</a>, tri-colour marking, tricolor marking, tricolour marking</strong></dt>
<dd><p>Tri-color marking is a <a href="#tracing.garbage.collection">tracing garbage collection</a> algorithm that assigns a <a href="c.html#color">color</a> (<a href="b.html#black">black</a>, <a href="w.html#white">white</a>, or <a href="g.html#gray">gray</a>) to each <a href="n.html#node">node</a> in the <a href="g.html#graph">graph</a>.  It is basic to <a href="i.html#incremental.garbage.collection">incremental garbage collection</a>.</p>
<p>Initially all nodes are colored white.  The distinguished <a href="r.html#root.set">root set</a> is colored gray.  The <a href="c.html#collector-2">collector<sup><small>(2)</small></sup></a> proceeds to discover the <a href="r.html#reachable">reachable</a> nodes by finding an <a href="e.html#edge">edge</a> from a gray node to a white node and coloring the white node gray.  Hence each tracing step involves choosing a gray node and graying its white children.</p>

<p>When all the edges from a gray node lead only to other gray (or black) nodes, the node is colored black.  When no gray nodes remain, the reachable part of the graph has been discovered and any nodes that are still white may be <a href="r.html#recycle">recycled</a>.</p>

<p>The <a href="m.html#mutator">mutator</a> is free to access any part of the graph and allocate new nodes while the <a href="c.html#collector-2">collector<sup><small>(2)</small></sup></a> is determining the reachable nodes, provided the <a href="#tri-color.invariant">tri-color invariant</a> is maintained, by changing the colors of the nodes affected, if necessary.</p>

<p><strong>Historical note:</strong> "Tri-color marking" is the term used to describe an algorithm developed in 1975 by E. W. Dijkstra and others, as an exercise in proving cooperating programs correct.  They chose as their problem a <a href="p.html#parallel.garbage.collection">parallel garbage collector</a>, with the intent of illustrating cooperating sequential processes with a large shared data space but minimal exclusion and synchronization constraints.</p>

<p>Although the algorithm developed in the paper is not necessarily the most efficient algorithm for a <a href="c.html#collector-1">collector<sup><small>(1)</small></sup></a>, it has been generally accepted to be correct -- an important feature not all garbage collectors can claim.  A number of other garbage collection algorithms have been shown to be isomorphic to the tri-color marking algorithm and thus are also believed to be correct.</p>

<p><strong>See also:</strong> <a href="b.html#barrier-1">barrier<sup><small>(1)</small></sup></a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#dlmss76">E. W. Dijkstra, Leslie Lamport, A. J. Martin, C. S. Scholten, E. F. M. Steffens. 1976. <cite>On-the-fly Garbage Collection: An Exercise in Cooperation</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="two-space.collector" name="two-space.collector">two-space collector</a>, two space collector</strong>
  (also known as semi-space collector)</dt>
<dd><p>A two-space <a href="c.html#collector-1">collector<sup><small>(1)</small></sup></a> is a simple form of a <a href="c.html#copying.garbage.collection">copying garbage collector</a>.  The available <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> is divided into two halves, called <a href="s.html#semi-space">semi-spaces</a>.  <a href="o.html#object">Objects</a> are allocated in one semi-space until it is full.  The <a href="r.html#reachable">reachable</a> objects are then copied into the other semi-space (usually using a <a href="c.html#cheney.scan">Cheney scan</a>) and the old semi-space is <a href="r.html#reclaim">reclaimed</a>.  <a href="a.html#allocate">Allocation</a> continues in the new semi-space until it is full, at which point the process is repeated in reverse.</p>
<p>The main disadvantage of a two-space collector is that it only makes use of half of the available memory.  This can be tolerable in a <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> system if the <a href="g.html#garbage.collector">garbage collector</a> is written carefully to preserve <a href="l.html#locality.of.reference">locality of reference</a>.  Other forms of copying garbage collector, such as <a href="g.html#generational.garbage.collection">generational garbage collectors</a>, have much lower overheads.</p>

<p align="center"><em>Allocation</em><br /><img alt="Diagram: Allocation" src="../diagrams/two-space-1.png" border="2" height="159" width="201" /></p>

<p align="center"><em>Allocation space is full</em><br /><img alt="Diagram: Allocation space is full" src="../diagrams/two-space-2.png" border="2" height="133" width="187" /></p>

<p align="center"><em>Copying garbage collection</em><br /><img alt="Diagram: Copying garbage collection" src="../diagrams/two-space-3.png" border="2" height="133" width="187" /></p>

<p align="center"><em>Allocation continues</em><br /><img alt="Diagram: Allocation continues" src="../diagrams/two-space-4.png" border="2" height="133" width="187" /></p>

<p><strong>See also:</strong> <a href="f.html#flip">flip</a>.
</p></dd>

<dt><strong><a id="type-accurate.garbage.collection" name="type-accurate.garbage.collection">type-accurate garbage collection</a></strong>
  (for full details, see <a href="e.html#exact.garbage.collection">exact garbage collection</a>)</dt>
<dd><p><a href="g.html#garbage.collection">Garbage collection</a> is exact (or precise) if it deals only with <a href="e.html#exact.reference">exact references</a>.</p></dd>

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
<a href="s.html">S</a>
<strong>T</strong>
<a href="u.html">U</a>
<a href="v.html">V</a>
<a href="w.html">W</a>
X
Y
<a href="z.html">Z</a> - <a href="help.html">Help</a></p>

</body></html>