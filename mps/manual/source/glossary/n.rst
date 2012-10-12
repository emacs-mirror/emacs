<?xml version="1.0" encoding="iso-8859-1"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
        "http://www.w3.org/TR/2000/REC-xhtml1-20000126/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
<link rev="MADE" href="mailto:mm-web@ravenbrook.com" />
<link rel="Start" href="../" />
<link rel="Contents" href="../" />
<link rel="Glossary" href="" />
<meta name="DC.Date" content="2000-12-15T20:16:58" />
<meta name="description" content="Glossary of terms related to memory management" />
<meta name="keywords" content="dictionary, glossary, definition, memory management, garbage collection" />
<link rel="index" href="./" />
<link rel="help" href="help.html" />
<link rel="prev" href="m.html" />
<link rel="next" href="o.html" />
<title>The Memory Management Glossary: N</title>
</head>
<body lang="en" bgcolor="#FFFFFF" text="#000000" link="#000099" vlink="#660066" alink="#FF0000">
<h1 align="center">The Memory Management Glossary<br />
<big>N</big></h1>
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
<strong>N</strong>
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
<dt><strong><a id="natural.alignment" name="natural.alignment">natural alignment</a></strong></dt>
<dd><p>Natural alignment is an <a href="a.html#alignment">alignment</a> constraint such that all <a href="o.html#object">objects</a> must be aligned to an address that is a multiple of their size.</p>
<p>Natural alignment is not usually required for objects larger than a <a href="w.html#word">word</a> or <a href="g.html#grain">grain</a>, which usually only need to be word- or grain-aligned.</p>

<p><strong>See also:</strong> <a href="a.html#alignment">alignment</a>;
    <a href="p.html#padding">padding</a>.
</p></dd>

<dt><strong><a id="nepotism" name="nepotism">nepotism</a></strong></dt>
<dd><p>In <a href="g.html#generational.garbage.collection">generational garbage collection</a> nepotism is the tendency for <a href="d.html#dead">dead</a> <a href="o.html#object">objects</a> in old <a href="g.html#generation">generations</a> to preserve younger dead objects that are referenced by them.  In other words, dead parents can cause their children to get promoted.</p>
<p>This happens when an object gets <a href="p.html#promotion">promoted</a> to an old generation and dies there, but does not get <a href="r.html#reclaim">reclaimed</a> because the generation it is in does not get considered for garbage collection very often.  The old object might refer to objects in younger generations that are also dead; until the old object is reclaimed the younger objects will be preserved by virtue of the <a href="r.html#reference">reference</a> from the older, assumed alive, object.</p>

<p>This is a form of <a href="f.html#floating.garbage">floating garbage</a> introduced by partitioning the objects into generations.</p></dd>


<dt><strong><a id="next.fit" name="next.fit">next fit</a></strong></dt>
<dd><p>A variant of the <a href="f.html#first.fit">first fit</a> <a href="a.html#allocation.mechanism">allocation mechanism</a> that uses a
<em>roving pointer</em> on a circular <a href="f.html#free.block.chain">free block chain</a>. The pointer is advanced along the chain when searching for a fit. Thus each allocation begins looking where the previous one finished.
The rationale is to avoid creating an accumulation of small
fragments at the head of the free block chain, which would have to be
examined on every allocation.
</p>
<p>There are several variants, according to the order of blocks on the
free block chain. The most common variant is address-ordered next fit.</p>

<p>This has a tendency to spread related objects out in memory, and also
gives quite poor <a href="l.html#locality.of.reference">locality</a> for the allocator (as the roving pointer
rotates around memory, the free blocks touched are those
least-recently used).
</p>

<p><strong>See also:</strong> <a href="f.html#first.fit">first fit</a>;
    <a href="a.html#allocation.mechanism">allocation mechanism</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil95">Paul R. Wilson, Mark S. Johnstone, Michael Neely, David Boles. 1995. <cite>Dynamic Storage Allocation: A Survey and Critical Review</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="node" name="node">node</a></strong></dt>
<dd><p>In a <a href="g.html#graph">graph</a>, a node is a representation of an <a href="o.html#object">object</a> at the
junction of zero or more <a href="e.html#edge">edges</a>.</p>
<p><strong>Opposites:</strong> <a href="e.html#edge">edge</a>.
<br />
<strong>See also:</strong> <a href="g.html#graph">graph</a>.
</p></dd>

<dt><strong><a id="nursery.generation" name="nursery.generation">nursery generation</a></strong>
  (for full details, see <a href="#nursery.space">nursery space</a>)</dt>
<dd><p>In <a href="g.html#generational.garbage.collection">generational garbage collection</a>, the <em>nursery <a href="g.html#generation">generation</a></em> or <em>space</em> is the area used for new <a href="a.html#allocate">allocation</a>.</p></dd>

<dt><strong><a id="nursery.space" name="nursery.space">nursery space</a></strong>
  (also known as nursery generation)</dt>
<dd><p>In <a href="g.html#generational.garbage.collection">generational garbage collection</a>, the <em>nursery <a href="g.html#generation">generation</a></em> or <em>space</em> is the area used for new <a href="a.html#allocate">allocation</a>.</p>
<p>The size of the nursery space must be chosen carefully.  Often it is related to the size of <a href="p.html#physical.memory-1">physical memory<sup><small>(1)</small></sup></a>.</p></dd>


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
<strong>N</strong>
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