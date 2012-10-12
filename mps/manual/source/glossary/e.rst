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
<link rel="prev" href="d.html" />
<link rel="next" href="f.html" />
<title>The Memory Management Glossary: E</title>
</head>
<body lang="en" bgcolor="#FFFFFF" text="#000000" link="#000099" vlink="#660066" alink="#FF0000">
<h1 align="center">The Memory Management Glossary<br />
<big>E</big></h1>
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
<strong>E</strong>
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
<dt><strong><a id="edge" name="edge">edge</a></strong></dt>
<dd><p>In a <a href="g.html#graph">graph</a>, an edge is a connection between two <a href="n.html#node">nodes</a>. </p>
<p>In a directed graph (digraph), edges have a direction; otherwise the start and end nodes are interchangeable.
By convention, two directed edges between the same two nodes, but in different directions, are depicted as a bi-directional edge.</p>

<p>Typically an edge represents some relation between nodes.</p>

<p><strong>Relevance to memory management:</strong> In memory management, edges normally represent the fact that an <a href="o.html#object">object</a> holds a <a href="r.html#reference">reference</a> to another object.</p>

<p><strong>See also:</strong> <a href="g.html#graph">graph</a>.
</p></dd>

<dt><strong><a id="entry.table-1" name="entry.table-1">entry table<sup><small>(1)</small></sup></a></strong></dt>
<dd><p>An entry table is a table of <a href="r.html#reference">references</a> into a set of <a href="o.html#object">objects</a> used to indirect references from the outside.</p>
<p>The Lieberman-Hewitt <a href="c.html#collector-1">collector<sup><small>(1)</small></sup></a> represented references from older <a href="g.html#generation">generations</a> to younger ones by indirect pointers through an entry table in the younger generation that contained the actual <a href="a.html#address">address</a> of the young object.  This is fairly expensive without special hardware; other <a href="g.html#generational.garbage.collection">generational</a> collectors generally use <a href="r.html#remembered.set">remembered sets</a>.</p>

<p><strong>See also:</strong> <a href="g.html#generational.garbage.collection">generational garbage collection</a>;
    <a href="#exit.table">exit table</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#lh83">Henry Lieberman, Carl Hewitt. 1983. <cite>A real-time garbage collector based on the lifetimes of objects</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="entry.table-2" name="entry.table-2">entry table<sup><small>(2)</small></sup></a></strong></dt>
<dd><p>An entry table is an implementation of a <a href="r.html#remembered.set">remembered set</a>, where, for a given <a href="g.html#generation">generation</a>, there is a list of <a href="o.html#object">objects</a> in older generations which contain <a href="r.html#reference">references</a> into that generation.</p>
<p>One could also store the actual <a href="m.html#memory.location">locations</a> of the references, which would save time when <a href="s.html#scan">scanning</a>, but incur other costs.</p>

<p><strong>Similar terms:</strong> <a href="r.html#remembered.set">remembered set</a>.
<br />
<strong>See also:</strong> <a href="g.html#generational.garbage.collection">generational garbage collection</a>;
    <a href="#exit.table">exit table</a>.
</p></dd>

<dt><strong><a id="exact.garbage.collection" name="exact.garbage.collection">exact garbage collection</a></strong>
  (also known as precise garbage collection, type-accurate garbage collection)</dt>
<dd><p><a href="g.html#garbage.collection">Garbage collection</a> is exact (or precise) if it deals only with <a href="#exact.reference">exact references</a>.</p>
<p>An exact <a href="c.html#collector-1">collector<sup><small>(1)</small></sup></a> needs to know the <a href="f.html#format">format</a> of the <a href="o.html#object">objects</a> and the <a href="r.html#root">roots</a>, so that it can tell which fields are references.</p>

<p><strong>Opposites:</strong> <a href="c.html#conservative.garbage.collection">conservative garbage collection</a>.
</p></dd>

<dt><strong><a id="exact.reference" name="exact.reference">exact reference</a></strong>
  (also known as precise reference, sure reference)</dt>
<dd><p>An exact  or precise or sure reference is a value the <a href="c.html#collector-1">collector<sup><small>(1)</small></sup></a> knows is a <a href="r.html#reference">reference</a>.</p>
<p>This is the usual sort of reference.  The term is used to draw a contrast with <a href="a.html#ambiguous.reference">ambiguous reference</a>.</p>

<p><strong>Opposites:</strong> <a href="a.html#ambiguous.reference">ambiguous reference</a>.
</p></dd>

<dt><strong><a id="exact.root" name="exact.root">exact root</a></strong>
  (also known as precise root)</dt>
<dd><p>An exact or precise root is a <a href="r.html#root">root</a> that contains only <a href="#exact.reference">exact references</a>.</p>
<p><strong>Opposites:</strong> <a href="a.html#ambiguous.root">ambiguous root</a>.
<br />
<strong>See also:</strong> <a href="#exact.reference">exact reference</a>.
</p></dd>

<dt><strong><a id="exact.segregated.fit" name="exact.segregated.fit">exact segregated fit</a></strong></dt>
<dd><p>A <a href="s.html#segregated.fit">segregated fit</a> <a href="a.html#allocation.mechanism">allocation mechanism</a> which has a separate <a href="f.html#free.list">free list</a> for each possible block size. The array of free lists may be
represented sparsely. Large blocks may be treated separately.</p>
<p><strong>See also:</strong> <a href="s.html#segregated.fit">segregated fit</a>;
    <a href="s.html#segregated.free.list">segregated free list</a>;
    <a href="a.html#allocation.mechanism">allocation mechanism</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil95">Paul R. Wilson, Mark S. Johnstone, Michael Neely, David Boles. 1995. <cite>Dynamic Storage Allocation: A Survey and Critical Review</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="execution.stack" name="execution.stack">execution stack</a></strong>
  (for full details, see <a href="c.html#control.stack">control stack</a>)</dt>
<dd><p>A <a href="s.html#stack">stack</a> that stores <a href="a.html#activation.record">activation records</a>, particularly subroutine return information, is known as a <em>control stack</em>.</p></dd>

<dt><strong><a id="exit.table" name="exit.table">exit table</a></strong></dt>
<dd><p>An exit table is a table of all <a href="r.html#reference">references</a> from a set of <a href="o.html#object">objects</a> to objects outside the set.</p>
<p><strong>See also:</strong> <a href="#entry.table-1">entry table<sup><small>(1)</small></sup></a>;
    <a href="#entry.table-2">entry table<sup><small>(2)</small></sup></a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#lh83">Henry Lieberman, Carl Hewitt. 1983. <cite>A real-time garbage collector based on the lifetimes of objects</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="extent" name="extent">extent</a></strong>
  (for full details, see <a href="l.html#lifetime">lifetime</a>)</dt>
<dd><p>The lifetime or extent of an <a href="o.html#object">object</a> is the time for which the object is <a href="l.html#live">live</a>.</p></dd>

<dt><strong><a id="external.fragmentation" name="external.fragmentation">external fragmentation</a></strong></dt>
<dd><p>External <a href="f.html#fragmentation">fragmentation</a> is the inability to use <a href="m.html#memory-1">memory<sup><small>(1)</small></sup></a> because <a href="f.html#free-3">free<sup><small>(3)</small></sup></a> memory is divided into many small <a href="b.html#block">blocks</a>.</p>
<p>If <a href="l.html#live">live</a> <a href="o.html#object">objects</a> are scattered, the free blocks cannot be <a href="c.html#coalesce">coalesced</a>, and hence no large blocks can be <a href="a.html#allocate">allocated</a>.</p>

<p>Common solutions to external fragmentation include:</p>

<ul>
   <li><a href="m.html#moving.garbage.collector">Moving garbage collection</a>;</li>
   <li><a href="h.html#handle">Handles</a>;</li>
   <li>Making all your objects the same size.</li>
</ul>

<p><strong>See also:</strong> <a href="i.html#internal.fragmentation">internal fragmentation</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#jw98">Mark S. Johnstone, Paul R. Wilson. 1998. <cite>The Memory Fragmentation Problem: Solved?</cite>.</a></li>
</ul><br /></dd>

</dl>
<p align="center"><a href="a.html">A</a>
<a href="b.html">B</a>
<a href="c.html">C</a>
<a href="d.html">D</a>
<strong>E</strong>
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