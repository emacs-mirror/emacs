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
<link rel="prev" href="n.html" />
<link rel="next" href="p.html" />
<title>The Memory Management Glossary: O</title>
</head>
<body lang="en" bgcolor="#FFFFFF" text="#000000" link="#000099" vlink="#660066" alink="#FF0000">
<h1 align="center">The Memory Management Glossary<br />
<big>O</big></h1>
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
<strong>O</strong>
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
<dt><strong><a id="object" name="object">object</a></strong>
  (also known as cell)</dt>
<dd><p>In <a href="m.html#memory.management">memory management</a>, we use the term <em>object</em> or <em>cell</em> to mean a contiguous <a href="b.html#block">block</a> of <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> forming a single logical structure.</p>
<p>Objects are the units of <a href="a.html#allocate">allocation</a>, <a href="f.html#free-1">deallocation</a>, etc.  No connection to an object-oriented system is implied.</p></dd>


<dt><strong><a id="off-white" name="off-white">off-white</a></strong></dt>
<dd><p>In a <a href="t.html#treadmill">treadmill</a> <a href="g.html#garbage.collector">garbage collector</a>,  the <a href="c.html#color">color</a> off-white is used to describe <a href="#object">objects</a> which are <a href="f.html#free-3">free<sup><small>(3)</small></sup></a>.</p>
<p><strong>Opposites:</strong> <a href="w.html#white">white</a>;
    <a href="g.html#gray">gray</a>;
    <a href="b.html#black">black</a>.
<br />
<strong>See also:</strong> <a href="t.html#treadmill">treadmill</a>;
    <a href="c.html#color">color</a>.
</p></dd>

<dt><strong><a id="one-bit.reference.count" name="one-bit.reference.count">one-bit reference count</a></strong></dt>
<dd><p>The one-bit <a href="r.html#reference.counting">reference count</a> is a heuristic mechanism that lets a program test, at low cost, whether an <a href="#object">object</a> is <a href="d.html#dead">dead</a>.</p>
<p>The one-bit reference count is a special case of the <a href="l.html#limited-field.reference.count">limited-field reference count</a>.  A single bit in an object, called the MRB (Multiple Reference Bit), is cleared when the object is <a href="a.html#allocate">allocated</a>.  Whenever another <a href="r.html#reference">reference</a> to the object is created, the bit is set.  Thus, MRB=0 indicates that there is exactly one reference to the object, and MRB=1 indicates that there may be more than one reference to the object.</p>

<p>The MRB can be stored in the reference rather than in the object; doing so reduces the number of memory accesses due to MRB checking and setting.  When a reference is copied, the copy's MRB is set.  If the MRB in the old reference is 0, it also needs to be set.  Setting the MRB in the old reference requires that the program knows the location the old reference came from, and that it can prove that location has not since been overwritten with other data.</p>

<p>The one-bit reference count is used by a compiler to augment an object lifetime analysis.  When compile-time analysis predicts that a particular object may be dead (typically because the variable that references the object is dead), the compiler can generate code that will check the object's MRB at run-time.  If the MRB is 0, then the object is dead.</p>

<p>Using a one-bit reference count does have a cost: the MRB uses space that could sometimes be put to other use, and the MRB must be set every time the number of references to the object increases.  The one-bit reference count is cheaper than other kinds of reference counting, however, since the space cost is only one bit and the reference count is not adjusted when references are destroyed.</p>

<p><strong>Historical note:</strong> The one-bit reference count was suggested by Friedman and Wise <a href="../bib/f.html#fw77"><cite>The One-Bit Reference Count</cite></a>.  Storing the MRB in the reference was suggested by Stoye, Clarke, and Norman <a href="../bib/f.html#scn84"><cite>Some Practical Methods for Rapid Combinator Reduction</cite></a>.</p>

<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#jones96">Richard E. Jones, Rafael Lins. 1996. <cite>Garbage Collection: Algorithms for Automatic Dynamic Memory Management</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="out-of-band.header" name="out-of-band.header">out-of-band header</a></strong></dt>
<dd><p>In some <a href="m.html#memory.manager">memory managers</a>, each <a href="a.html#allocate">allocated</a> <a href="b.html#block">block</a> has additional information (such as the size of the block or a <a href="t.html#tag">tag</a>) stored in a separate block; this is called <em>an out-of-band header</em>.</p>
<p><strong>Opposites:</strong> <a href="i.html#in-band.header">in-band header</a>.
</p></dd>

<dt><strong><a id="overcommit" name="overcommit">overcommit</a></strong></dt>
<dd><p>In some circumstances, although a range of <a href="v.html#virtual.address">virtual addresses</a> has been <a href="m.html#mapped">mapped</a> as far as the user program is concerned, the <a href="p.html#physical.storage">physical storage</a> might not be allocated until it is accessed.  This is called <em>overcommitting</em>.</p>
<p>Overcommitting shares <a href="s.html#swap.space">swap space</a> resources more flexibly, especially when crude <a href="s.html#suballocator">suballocators</a> are involved, but it can lead to an out-of-resource error during a <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> access; few environments deal with this situation gracefully.</p>

<p>UNIX&reg; systems such as IRIX&reg; and AIX<sup><small class="tm-small">TM</small></sup> can do this on <code><a href="s.html#sbrk">sbrk</a></code> and <code><a href="m.html#mmap">mmap</a></code> calls.</p></dd>


<dt><strong><a id="overwriting.error" name="overwriting.error">overwriting error</a></strong>
  (also known as bounds error)</dt>
<dd><p>An overwriting or bounds error occurs when the programmer intends his program to write to a particular <a href="b.html#block">block</a> of <a href="m.html#memory-1">memory<sup><small>(1)</small></sup></a>, but a program error causes the program to write outside the bounds of that block.</p>
<p><strong>See also:</strong> <a href="f.html#fencepost">fencepost</a>.
</p></dd>

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
<strong>O</strong>
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