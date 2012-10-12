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
<link rel="prev" href="t.html" />
<link rel="next" href="v.html" />
<title>The Memory Management Glossary: U</title>
</head>
<body lang="en" bgcolor="#FFFFFF" text="#000000" link="#000099" vlink="#660066" alink="#FF0000">
<h1 align="center">The Memory Management Glossary<br />
<big>U</big></h1>
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
<strong>U</strong>
<a href="v.html">V</a>
<a href="w.html">W</a>
X
Y
<a href="z.html">Z</a> - <a href="help.html">Help</a></p>
<p>Our aim is for these entries to be accurate, comprehensible, and useful, and also to have an entry for all common memory management terms.  If you can't find the term you're looking for, if our definition doesn't help you, or if you'd like to suggest corrections or additions, please let us know via our <a href="../feedback.html">feedback page</a>.</p>

<p>For an explanation of the structure of the entries, and information on how to link to definitions, please see the <a href="help.html">glossary help page</a>.</p>

<hr />
<dl>
<dt><strong><a id="unaligned" name="unaligned">unaligned</a></strong>
  (also known as misaligned)</dt>
<dd><p>An <a href="a.html#address">address</a> is unaligned or misaligned if it does not comply with some <a href="a.html#alignment">alignment</a> constraint on it.</p>
<p>For example, typically double precision floating point numbers occupy 8 <a href="b.html#byte-1">bytes<sup><small>(1)</small></sup></a> and have an alignment of 4 bytes; that is, their address must be a multiple of four.  If a program tries to access such a number using an address that is not a multiple of four, a <a href="b.html#bus.error">bus error</a> will result.</p>

<p><strong>Opposites:</strong> <a href="a.html#alignment">aligned</a>.
<br />
<strong>See also:</strong> <a href="a.html#alignment">alignment</a>;
    <a href="b.html#bus.error">bus error</a>.
</p></dd>

<dt><strong><a id="unboxed" name="unboxed">unboxed</a></strong></dt>
<dd><p>Unboxed <a href="o.html#object">objects</a> are represented by an encoding of the data itself, and not by a <a href="p.html#pointer">pointer</a> to that data.</p>
<p>Representations are typically chosen so that unboxed values are the same size as the pointer part of a <a href="b.html#boxed">boxed</a> object.  Sometimes the value is <a href="t.html#tag">tagged</a> to distinguish it from a boxed object.  The entire object is duplicated when the object is passed around,  so updates to it,  if allowed, only affect one copy.</p>

<p><strong>Similar terms:</strong> <a href="i.html#immediate.data">immediate data</a>.
<br />
<strong>Opposites:</strong> <a href="b.html#boxed">boxed</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#gudeman93">David Gudeman. 1993. <cite>Representing Type Information in Dynamically Typed Languages</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="undead" name="undead">undead</a></strong></dt>
<dd><p>An undead object is an <a href="o.html#object">object</a> that cannot be proven to be <a href="d.html#dead">dead</a> by the <a href="g.html#garbage.collector">garbage collector</a>, but whose <a href="l.html#live">liveness</a> is dubious.</p>
<p>For example, an <a href="a.html#ambiguous.reference">ambiguous reference</a> to an object on a <a href="p.html#page">page</a> may mark the entire page as <a href="r.html#reachable">reachable</a>.  No further data is collected about that page.  The other objects on the page will survive, even though their reachability has not been determined.  They are <i>undead</i>.</p></dd>


<dt><strong><a id="unmapped" name="unmapped">unmapped</a></strong>
  (also known as free(4))</dt>
<dd><p>A range of <a href="v.html#virtual.address">virtual addresses</a> is said to be <em>unmapped</em> (<em>free</em> on Windows&reg;) if there is no <a href="p.html#physical.memory-2">physical memory<sup><small>(2)</small></sup></a> associated with the range.</p>
<p>An unmapped range may or may not be <a href="r.html#reserved">reserved</a>.</p>

<p><strong>Opposites:</strong> <a href="m.html#mapped">mapped</a>.
</p></dd>

<dt><strong><a id="unreachable" name="unreachable">unreachable</a></strong></dt>
<dd><p>An <a href="o.html#object">object</a> is unreachable if there is no <a href="r.html#reference">reference</a> chain to it from any <a href="r.html#root">root</a>.</p>
<p>An object will become unreachable when the <a href="m.html#mutator">mutator</a> overwrites its last (direct or indirect) reference to the object.</p>

<p><strong>Similar terms:</strong> <a href="d.html#dead">dead</a>.
<br />
<strong>Opposites:</strong> <a href="r.html#reachable">reachable</a>;
    <a href="l.html#live">live</a>.
<br />
<strong>See also:</strong> <a href="r.html#reachable">reachable</a>;
    <a href="g.html#garbage.collection">garbage collection</a>.
</p></dd>

<dt><strong><a id="unsure.reference" name="unsure.reference">unsure reference</a></strong>
  (for full details, see <a href="a.html#ambiguous.reference">ambiguous reference</a>)</dt>
<dd><p>An ambiguous or unsure <a href="r.html#reference">reference</a> is a value that is potentially a reference, but the <a href="c.html#collector-1">collector<sup><small>(1)</small></sup></a> cannot prove that it is.</p></dd>

<dt><strong><a id="unwrapped" name="unwrapped">unwrapped</a></strong>
  (also known as raw)</dt>
<dd><p>A value is <em>unwrapped</em> or <em>raw</em> if it is not encoded with type information.</p>
<p>In a dynamically-typed language, the compiler may sometimes be able to pick a more compact or efficient representation for a value if it can prove that the type can be determined at compile-time. This is a particularly useful optimization for numeric values such as integers or floats.</p>

<p><strong>Opposites:</strong> <a href="w.html#wrapped">wrapped</a>.
<br />
<strong>See also:</strong> <a href="b.html#boxed">boxed</a>;
    <a href="t.html#tag">tag</a>;
    <a href="v.html#value.object">value object</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#gudeman93">David Gudeman. 1993. <cite>Representing Type Information in Dynamically Typed Languages</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="use.after.free" name="use.after.free">use after free</a></strong>
  (for full details, see <a href="p.html#premature.free">premature free</a>)</dt>
<dd><p>A <i>premature free</i> or <i>use after free</i> occurs when <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> is <a href="f.html#free-1">deallocated</a>, but is later accessed.</p></dd>

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
<strong>U</strong>
<a href="v.html">V</a>
<a href="w.html">W</a>
X
Y
<a href="z.html">Z</a> - <a href="help.html">Help</a></p>

</body></html>