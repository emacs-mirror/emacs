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
<link rel="prev" href="g.html" />
<link rel="next" href="i.html" />
<title>The Memory Management Glossary: H</title>
</head>
<body lang="en" bgcolor="#FFFFFF" text="#000000" link="#000099" vlink="#660066" alink="#FF0000">
<h1 align="center">The Memory Management Glossary<br />
<big>H</big></h1>
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
<strong>H</strong>
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
<dt><strong><a id="handle" name="handle">handle</a></strong></dt>
<dd><p>A handle is an object that represents a resource.</p>
<p>Handles are used when the resource cannot be represented directly.  For example, a file handle is an object passed between a process and the OS in order to access a file, because the file itself cannot be represented.</p>

<p><strong>Relevance to memory management:</strong> In memory management, a handle is an object that represents another <a href="o.html#object">object</a>.  Handles are usually used because the object itself needs to be <a href="m.html#moving.garbage.collector">moved</a> in <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a>, or even <a href="s.html#swapped.out">swapped out</a> to disk.  The program therefore cannot know the <a href="a.html#address">address</a> of the object.</p>

<p>For example, the Mac&reg; OS makes extensive use of handles in its <a href="http://developer.apple.com/techpubs/mac/Memory/Memory-11.html">heap management</a> to avoid problems due to <a href="f.html#fragmentation">fragmentation</a>.  If the Mac OS Memory Manager cannot satisfy a request for memory, it may try <a href="c.html#compaction">compacting</a> the <a href="#heap">heap</a> -- moving all the <a href="r.html#relocation">relocatable</a> objects together to squeeze out gaps.  It can do this because the program only has handles on the objects, and not their actual addresses.</p>

<p align="center"><em>Legend</em><br /><img alt="Diagram: Legend" src="../diagrams/handle-legend.png" border="2" height="151" width="163" /></p>

<p align="center"><em>Handle-based heap before compaction</em><br /><img alt="Diagram: Handle-based heap before compaction" src="../diagrams/handle-before.png" border="2" height="331" width="385" /></p>

<p align="center"><em>Handle-based heap after compaction</em><br /><img alt="Diagram: Handle-based heap after compaction" src="../diagrams/handle-after.png" border="2" height="331" width="385" /></p>

<p><strong>Similar terms:</strong> <a href="p.html#pointer">pointer</a>.
</p></dd>

<dt><strong><a id="header" name="header">header</a></strong>
  (for full details, see <a href="i.html#in-band.header">in-band header</a>)</dt>
<dd><p>Some <a href="m.html#memory.manager">memory managers</a> <a href="a.html#allocate">allocate</a> a fixed amount more than is necessary for each <a href="b.html#block">block</a> and use it to store information such as the size of the block or a <a href="t.html#tag">tag</a>.  This extra memory is known as <em>an in-band header</em> or <em>a frame</em></p></dd>

<dt><strong><a id="heap" name="heap">heap</a></strong>
  (also known as free store, freestore)</dt>
<dd><p>The <em>heap</em> or <em>free store</em> is the <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> area managed by <a href="d.html#dynamic.allocation">dynamic allocation</a>.</p>
<p>This use of <em>heap</em> is unconnected with the data structure used by the heapsort algorithm.</p></dd>


<dt><strong><a id="heap.allocation" name="heap.allocation">heap allocation</a></strong>
  (also known as dynamic allocation)</dt>
<dd><p><em>Heap allocation</em> or <em>dynamic allocation</em> means run-time <a href="a.html#allocate">allocation</a> and <a href="f.html#free-1">deallocation</a> of <a href="s.html#storage">storage</a> in arbitrary order.</p>
<p>Dynamic allocation is usually for <a href="o.html#object">objects</a> whose size, quantity, or <a href="l.html#lifetime">lifetime</a> could not be determined at compile-time.  It is necessary to implement modern data structures, such as recursive trees and full <a href="c.html#closure">closures</a>.</p>

<p>Objects on the <a href="#heap">heap</a> can be managed <a href="m.html#manual.memory.management">manually</a>, as in <a href="../articles/lang.html#c">C</a>, or <a href="a.html#automatic.memory.management">automatically</a>, as in <a href="../articles/lang.html#lisp">Lisp</a> and <a href="../articles/lang.html#java">Java</a><sup><small class="tm-small">TM</small></sup>.</p>

<p><strong>Opposites:</strong> <a href="s.html#stack.allocation">stack allocation</a>;
    <a href="s.html#static.allocation">static allocation</a>.
<br />
<strong>See also:</strong> <a href="i.html#indefinite.extent">indefinite extent</a>.
</p></dd>

<dt><strong><a id="hit" name="hit">hit</a></strong></dt>
<dd><p>A hit is a successful lookup in any form of <a href="c.html#caching-3">cache<sup><small>(3)</small></sup></a>, most commonly at some level of a <a href="s.html#storage.hierarchy">storage hierarchy</a>, such as a <a href="c.html#cache-1">cache<sup><small>(1)</small></sup></a> or <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> system.</p>
<p><strong>Opposites:</strong> <a href="m.html#miss">miss</a>.
</p></dd>

<dt><strong><a id="hit.rate" name="hit.rate">hit rate</a></strong></dt>
<dd><p>At any level of a <a href="s.html#storage.hierarchy">storage hierarchy</a>, the hit rate is the proportion of accesses which <a href="#hit">hit</a>.</p>
<p><strong>Opposites:</strong> <a href="m.html#miss.rate">miss rate</a>.
</p></dd>

</dl>
<p align="center"><a href="a.html">A</a>
<a href="b.html">B</a>
<a href="c.html">C</a>
<a href="d.html">D</a>
<a href="e.html">E</a>
<a href="f.html">F</a>
<a href="g.html">G</a>
<strong>H</strong>
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