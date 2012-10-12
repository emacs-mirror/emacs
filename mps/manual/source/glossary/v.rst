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
<link rel="prev" href="u.html" />
<link rel="next" href="w.html" />
<title>The Memory Management Glossary: V</title>
</head>
<body lang="en" bgcolor="#FFFFFF" text="#000000" link="#000099" vlink="#660066" alink="#FF0000">
<h1 align="center">The Memory Management Glossary<br />
<big>V</big></h1>
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
<a href="t.html">T</a>
<a href="u.html">U</a>
<strong>V</strong>
<a href="w.html">W</a>
X
Y
<a href="z.html">Z</a> - <a href="help.html">Help</a></p>
<p>Our aim is for these entries to be accurate, comprehensible, and useful, and also to have an entry for all common memory management terms.  If you can't find the term you're looking for, if our definition doesn't help you, or if you'd like to suggest corrections or additions, please let us know via our <a href="../feedback.html">feedback page</a>.</p>

<p>For an explanation of the structure of the entries, and information on how to link to definitions, please see the <a href="help.html">glossary help page</a>.</p>

<hr />
<dl>
<dt><strong><a id="value.object" name="value.object">value object</a></strong>
  (also known as immutable object)</dt>
<dd><p>A <em>value object</em> or <em>immutable object</em> is an <a href="o.html#object">object</a> whose identity depends solely upon its value or magnitude.</p>
<p>In a typed language, the compiler can often determine at compile time that certain types can be represented as value objects.  Usually these types are a <a href="s.html#scalar.data.type">scalar data type</a> with bounded magnitude.</p>

<p><strong>Relevance to memory management:</strong> If value objects can be identified, the compiler and the memory manager can make certain optimizations:  Value objects can be represented as <a href="i.html#immediate.data">immediate data</a> to minimize storage overhead, they can be replicated to improve <a href="l.html#locality.of.reference">locality</a>, and a <a href="#vector.data.type">vector data type</a> of value objects can be represented as a <a href="l.html#leaf.object">leaf object</a>.</p>

<p><strong>Historical note:</strong> Some programming languages expose representational details such as the use of value objects.  In <a href="../articles/lang.html#lisp">Lisp</a>, for example, numbers are often represented as value objects but not always as immediate data.  The <code>EQ</code> predicate of Lisp tests if two objects are the identical representation, whereas the <code>EQL</code> predicate tests if two objects represent the same type and value (are computationally identical).  Because the choice of representation is an optimization, exposing it at the language level could cause programs to behave differently under different compilers or optimization settings.  Modern languages, such as <a href="../articles/lang.html#dylan">Dylan</a><sup><small class="tm-small">TM</small></sup> hide this representational distinction, permitting the compiler greater freedom in optimization.</p>

<p><strong>Similar terms:</strong> <a href="i.html#immediate.data">immediate data</a>.
<br />
<strong>See also:</strong> <a href="i.html#immutable">immutable</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#baker93a">Henry G. Baker. 1993. <cite>Equal Rights for Functional Objects or, The More Things Change, The More They Are the Same</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="value.type" name="value.type">value type</a></strong></dt>
<dd><p>Definition not yet available.  Please see our <a href="../feedback.html#submissions">feedback page</a> for submission information.</p></dd>

<dt><strong><a id="vector.data.type" name="vector.data.type">vector data type</a></strong></dt>
<dd><p>A vector data type is an aggregate type of more than one dimension whose objects have a value for each dimension, where each dimension is of the same type.</p>
<p>Examples of vector data types include: strings, arrays, and lists.</p>

<p><strong>Relevance to memory management:</strong> Vector data types are seldom represented using <a href="#value.object">value objects</a>, but may be represented using <a href="l.html#leaf.object">leaf objects</a> if they are an aggregate of a type that can be represented by <a href="#value.object">value objects</a>.  <a href="s.html#scan">Scanning</a> information for vectors can be compactly encoded in terms of the aggregated type and the vector dimension.</p>

<p><strong>See also:</strong> <a href="s.html#scalar.data.type">scalar data type</a>;
    <a href="a.html#algebraic.data.type">algebraic data type</a>;
    <a href="#value.object">value object</a>;
    <a href="l.html#leaf.object">leaf object</a>.
</p></dd>

<dt><strong><a id="virtual.address" name="virtual.address">virtual address</a></strong>
  (also known as logical address)</dt>
<dd><p>In a <a href="#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> system, the <a href="a.html#address">addresses</a> that application programs deal with are known as <em>virtual addresses</em>.</p>
<p>The virtual addresses used by the application program are translated by the virtual memory system (often using <a href="t.html#tlb">TLB</a>s and <a href="p.html#page.table">page-tables</a>) to <a href="p.html#physical.address">physical addresses</a>.  It is the physical address that is used to retrieve the contents from the <a href="m.html#memory-3">memory<sup><small>(3)</small></sup></a>.</p>

<p><strong>Opposites:</strong> <a href="p.html#physical.address">physical address</a>.
</p></dd>

<dt><strong><a id="virtual.address.space" name="virtual.address.space">virtual address space</a></strong></dt>
<dd><p>The virtual <a href="a.html#address.space">address space</a> is the space of <a href="#virtual.address">virtual addresses</a>.</p>
<p>On <a href="#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a> systems, user processes see the virtual address space, and commonly have a separate virtual address space each, so that they map the same addresses to different data.  These systems often have <a href="s.html#shared.memory">shared memory</a> as well.</p>

<p><strong>Opposites:</strong> <a href="p.html#physical.address.space">physical address space</a>.
</p></dd>

<dt><strong><a id="virtual.memory-1" name="virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a></strong>
  (also known as VM(1))</dt>
<dd><p>In a <em>virtual memory</em> (<em><abbr title="Virtual Memory">VM</abbr></em>) system, the program code deals with <em><a href="#virtual.address">virtual addresses</a></em>.  Upon use, the virtual address is translated by the <a href="m.html#mmu">MMU</a> to obtain a <em><a href="p.html#physical.address">physical address</a></em> that is used to access <a href="p.html#physical.memory-1">physical memory<sup><small>(1)</small></sup></a>.</p>
<p>Some operating systems can simulate having more <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> than is available as <a href="m.html#main.memory">main memory</a>, by storing part of the data in <a href="b.html#backing.store">backing store</a>, typically on disk.  If the <a href="p.html#page">page</a> referenced by the virtual address is not currently in main memory, a <a href="p.html#page.fault">page fault</a> occurs, triggering an operating system handler that <a href="s.html#swapped.in">swaps in</a> the page.  Some other page might be <a href="s.html#swapped.out">swapped out</a> to make room.</p>

<p>Each process typically has its own separate <a href="#virtual.address.space">virtual address space</a> with its own <a href="m.html#mapping">mappings</a> and <a href="p.html#protection">protections</a>.</p>

<p align="center"><em>Example of the relationship between the virtual address spaces of two processes, physical memory, and backing store</em><br /><img alt="Diagram: Example of the relationship between the virtual address spaces of two processes, physical memory, and backing store" src="../diagrams/virtual-memory.png" border="2" height="507" width="454" /></p>

<p>Virtual memory technology can be used in many useful memory management techniques, such as <a href="b.html#barrier-1">barriers<sup><small>(1)</small></sup></a>, copy-on-write, and <a href="m.html#memory.mapping">memory mapping</a>.</p>

<blockquote>"Virtual" means never knowing where your next byte is coming from.</blockquote>

<p><strong>Opposites:</strong> <a href="r.html#real.memory-1">real memory<sup><small>(1)</small></sup></a>.
<br />
<strong>See also:</strong> <a href="p.html#paging">paging</a>;
    <a href="p.html#paged.in">paged in</a>;
    <a href="p.html#paged.out">paged out</a>;
    <a href="s.html#swapping">swapping</a>;
    <a href="s.html#swap.space">swap space</a>;
    <a href="m.html#mapped">mapped</a>;
    <a href="r.html#reserved">reserved</a>;
    <a href="u.html#unmapped">unmapped</a>;
    <a href="s.html#shared.memory">shared memory</a>.
</p></dd>

<dt><strong><a id="vm-1" name="vm-1">VM<sup><small>(1)</small></sup></a></strong>
  (for full details, see <a href="#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a>)</dt>
<dd><p>In a <em>virtual memory</em> (<em><abbr title="Virtual Memory">VM</abbr></em>) system, the program code deals with <em><a href="#virtual.address">virtual addresses</a></em>.  Upon use, the virtual address is translated by the <a href="m.html#mmu">MMU</a> to obtain a <em><a href="p.html#physical.address">physical address</a></em> that is used to access <a href="p.html#physical.memory-1">physical memory<sup><small>(1)</small></sup></a>.</p></dd>

<dt><strong><a id="vm-2" name="vm-2">VM<sup><small>(2)</small></sup></a></strong></dt>
<dd><p>In the <a href="../articles/lang.html#postscript">PostScript</a>&reg; language, <em>VM</em> is the <a href="s.html#storage">storage</a> where the values of the <a href="c.html#composite.object">composite objects</a> reside.</p>
<p>VM is short for "virtual memory", but this has nothing to do with the usual sense of the phrase (see <a href="#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a>).</p></dd>


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
<a href="t.html">T</a>
<a href="u.html">U</a>
<strong>V</strong>
<a href="w.html">W</a>
X
Y
<a href="z.html">Z</a> - <a href="help.html">Help</a></p>

</body></html>