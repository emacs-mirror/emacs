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
<link rel="prev" href="v.html" />
<link rel="next" href="z.html" />
<title>The Memory Management Glossary: W</title>
</head>
<body lang="en" bgcolor="#FFFFFF" text="#000000" link="#000099" vlink="#660066" alink="#FF0000">
<h1 align="center">The Memory Management Glossary<br />
<big>W</big></h1>
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
<a href="v.html">V</a>
<strong>W</strong>
X
Y
<a href="z.html">Z</a> - <a href="help.html">Help</a></p>
<p>Our aim is for these entries to be accurate, comprehensible, and useful, and also to have an entry for all common memory management terms.  If you can't find the term you're looking for, if our definition doesn't help you, or if you'd like to suggest corrections or additions, please let us know via our <a href="../feedback.html">feedback page</a>.</p>

<p>For an explanation of the structure of the entries, and information on how to link to definitions, please see the <a href="help.html">glossary help page</a>.</p>

<hr />
<dl>
<dt><strong><a id="weak.reference-1" name="weak.reference-1">weak reference<sup><small>(1)</small></sup></a></strong></dt>
<dd><p>In <a href="t.html#tracing.garbage.collection">tracing garbage collection</a>, a weak reference is a <a href="r.html#reference">reference</a> that does not keep the <a href="o.html#object">object</a> it refers to <a href="a.html#alive">alive</a>.</p>
<p>A weak reference does not keep the referent alive, but it will continue to refer to the object as long as it remains otherwise alive.  When only weak references to the object remain, the weak references can be deleted ("splatted" or "cleared") and the object <a href="r.html#reclaim">reclaimed</a>.</p>

<p><a href="../articles/lang.html#java">Java</a><sup><small class="tm-small">TM</small></sup> offers three kinds of weak references, called <a href="s.html#soft.reference">soft references</a>, <a href="#weak.reference-2">weak references<sup><small>(2)</small></sup></a>, and <a href="p.html#phantom.reference">phantom references</a>, in order of increasing weakness.</p>

<p><strong>Opposites:</strong> <a href="s.html#strong.reference">strong reference</a>.
<br />
<strong>See also:</strong> <a href="#weak.root">weak root</a>.
</p></dd>

<dt><strong><a id="weak.reference-2" name="weak.reference-2">weak reference<sup><small>(2)</small></sup></a></strong></dt>
<dd><p>In <a href="../articles/lang.html#java">Java</a><sup><small class="tm-small">TM</small></sup> terminology, <em>weak reference</em> is used to mean a <a href="r.html#reference">reference</a> encapsulated in a <a href="r.html#reference.object">reference object</a> of class <code>WeakReference</code>.</p>
<p>Weak references form one of three kinds of <a href="#weak.reference-1">weak reference<sup><small>(1)</small></sup></a> in Java.  They are handy for associating extra data with objects when you cannot store it in the objects themselves.</p>

<p><strong>See also:</strong> <a href="#weakly.reachable">weakly reachable</a>.
<br />
<strong>Other links:</strong> <a href="http://java.sun.com/products/jdk/1.2/docs/api/java/lang/ref/WeakReference.html">Java spec for class WeakReference</a>;
    <a href="http://developer.java.sun.com/developer/technicalArticles/ALT/RefObj/index.html">Reference Objects and Garbage Collection</a>.
</p></dd>

<dt><strong><a id="weak.root" name="weak.root">weak root</a></strong></dt>
<dd><p>A weak root is a <a href="r.html#root">root</a>, such that all <a href="r.html#reference">references</a> in it are <a href="#weak.reference-1">weak references<sup><small>(1)</small></sup></a>;
that is, they do not affect the <a href="l.html#live">liveness</a> of the <a href="o.html#object">objects</a> referred to.</p>
<p><strong>Opposites:</strong> <a href="s.html#strong.root">strong root</a>.
</p></dd>

<dt><strong><a id="weak.tri-color.invariant" name="weak.tri-color.invariant">weak tri-color invariant</a>, weak tri-colour invariant, weak tricolor invariant, weak tricolour invariant</strong></dt>
<dd><p>The weak <a href="t.html#tri-color.invariant">tri-color invariant</a> is the property of a <a href="r.html#reference">reference</a> <a href="g.html#graph">graph</a> that  all <a href="#white">white</a> <a href="n.html#node">nodes</a> pointed to by a <a href="b.html#black">black</a> node are also <a href="r.html#reachable">reachable</a> from some <a href="g.html#gray">gray</a> node through a chain of white nodes.</p>
<p>By preserving this property throughout <a href="t.html#tri-color.marking">tri-color marking</a>, a <a href="t.html#trace">tracing</a> algorithm can ensure that the <a href="c.html#collector-2">collector<sup><small>(2)</small></sup></a> will not miss reachable objects, even if the <a href="m.html#mutator">mutator</a> manipulates the graph during the collection.  Mutator actions might need to change the <a href="c.html#color">color</a> of the nodes affected in order to preserve the invariant (see <a href="../bib/f.html#pirinen98"><cite>Barrier techniques for incremental tracing</cite></a> for details).</p>

<p>Algorithms using this invariant are <a href="s.html#snapshot-at-the-beginning">snapshot-at-the-beginning</a> algorithms.</p>

<p><strong>See also:</strong> <a href="b.html#barrier-1">barrier<sup><small>(1)</small></sup></a>;
    <a href="s.html#strong.tri-color.invariant">strong tri-color invariant</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#pirinen98">Pekka P. Pirinen. 1998. <cite>Barrier techniques for incremental tracing</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="weakly.reachable" name="weakly.reachable">weakly reachable</a></strong></dt>
<dd><p>In <a href="../articles/lang.html#java">Java</a><sup><small class="tm-small">TM</small></sup>, an object is <em>weakly reachable</em> if it is neither <a href="s.html#strongly.reachable">strongly</a> nor <a href="s.html#softly.reachable">softly reachable</a> and there is a path from the <a href="r.html#root">roots</a> to it that contains at least one <a href="#weak.reference-2">weak reference<sup><small>(2)</small></sup></a> but no <a href="p.html#phantom.reference">phantom references</a>.</p>
<p>When the Java <a href="c.html#collector-1">collector<sup><small>(1)</small></sup></a> determines that an object is weakly reachable, it clears all the weak references involved, and declares the object <a href="f.html#finalization">finalizable</a>.  (Operationally, finalization works as if it was implemented by a class of "final references" that stand between weak and phantom references.)  Also, the <a href="r.html#reference.object">reference objects</a> containing the weak references are enqueued, if they were registered with a queue.</p>

<p><strong>See also:</strong> <a href="r.html#reachable">reachability</a>;
    <a href="p.html#phantom.reachable">phantom reachable</a>.
<br />
<strong>Other links:</strong> <a href="http://java.sun.com/products/jdk/1.2/docs/api/java/lang/ref/WeakReference.html">Java spec for class WeakReference</a>;
    <a href="http://developer.java.sun.com/developer/technicalArticles/ALT/RefObj/index.html">Reference Objects and Garbage Collection</a>.
</p></dd>

<dt><strong><a id="weighted.buddies" name="weighted.buddies">weighted buddies</a></strong></dt>
<dd><p>A <a href="b.html#buddy.system">buddy system</a> <a href="a.html#allocation.mechanism">allocation mechanism</a> using two series of size
classes: <a href="b.html#binary.buddies">binary buddies</a> (2, 4, 8, ...) and three-times-power-of-two
(3, 6, 12, ...). A block that is in the latter series may be <a href="s.html#split">split</a> in
two different ways. Thus a block of size 12 may be split into two
blocks of size 6 or one block of size 4 and one block of size 8. The
same applies for <a href="c.html#coalesce">coalescing</a>. This gives this system more flexibility
than a regular buddy system.</p>
<p><strong>See also:</strong> <a href="b.html#buddy.system">buddy system</a>;
    <a href="a.html#allocation.mechanism">allocation mechanism</a>;
    <a href="b.html#binary.buddies">binary buddies</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil95">Paul R. Wilson, Mark S. Johnstone, Michael Neely, David Boles. 1995. <cite>Dynamic Storage Allocation: A Survey and Critical Review</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="weighted.reference.counting" name="weighted.reference.counting">weighted reference counting</a></strong></dt>
<dd><p>A technique for <a href="r.html#reference.counting">reference counting</a> which is in common use for <a href="d.html#distributed.garbage.collection">distributed garbage collection</a> because of the low level of inter-process communication it requires.</p>
<p>Inter-process <a href="r.html#reference">references</a> to <a href="o.html#object">objects</a> are counted, but instead of simply counting the number of references, each reference is given a weight. When an object is created, the initial pointer to it is assigned a weight, which is usually a power of 2 for easy division. The object records the sum of all the weights of all of its references. Whenever a reference is copied, its weight is divided equally between the new and original copies. Since this operation  preserves the weighted reference sum, there is no need for communication with the object at this time. When a reference is deleted, the weighted reference sum is decremented by the weight of the reference. This is communicated to the object by sending it a message. When the object detects that the weighted reference sum has dropped to zero, it may be <a href="r.html#reclaim">reclaimed</a>. The algorithm is tolerant of communication protocols which don't guarantee order of arrival of deletion messages.</p></dd>


<dt><strong><a id="white" name="white">white</a></strong></dt>
<dd><p>In a <a href="t.html#tri-color.marking">tri-color marking</a> scheme, white <a href="o.html#object">objects</a> are objects that were <a href="t.html#threatened.set">condemned</a> at the beginning of the <a href="c.html#collection.cycle">collection cycle</a> and have not been shown to be <a href="r.html#reachable">reachable</a>.  When <a href="t.html#trace">tracing</a> is complete, white objects will be subject to <a href="r.html#reclaim">reclamation</a>.</p>
<p><strong>Opposites:</strong> <a href="g.html#gray">gray</a>;
    <a href="b.html#black">black</a>.
</p></dd>

<dt><strong><a id="word" name="word">word</a></strong>
  (also known as machine word)</dt>
<dd><p>Almost all processor architectures have a characteristic data size that is handled most efficiently.  This is known as the <em>word size</em>, and data of that size are known as <em>words</em>.  The word size is usually a power of two multiple of <a href="b.html#byte-2">bytes<sup><small>(2)</small></sup></a>.</p>
<p>Often the platform's word size is used to characterize the architecture by quoting the number of bits in it.  For example, a 32-bit platform has a word size of four bytes and a 64-bit platform has eight-byte words (assuming 8-bit bytes).  Typically, <a href="p.html#pointer">pointers</a> are the size of a word, and traditionally this determined the word size.  Nowadays, word size is usually driven by the need for more accuracy and range in mathematical calculations.</p>

<p><strong>Historical note:</strong> In the past, the convenience of dealing with powers of two was not as significant, and word sizes such as 36- or 72-bits were not unknown.</p>

<p><strong>See also:</strong> <a href="a.html#alignment">alignment</a>;
    <a href="g.html#grain">grain</a>.
</p></dd>

<dt><strong><a id="working.set" name="working.set">working set</a></strong></dt>
<dd><p>The working set of a program or system is that <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> or set of <a href="a.html#address">addresses</a> which it will use in the near future.</p>
<p>This term is generally used when discussing <a href="m.html#miss.rate">miss rates</a> at some <a href="s.html#storage.level">storage level</a>; the time scale of "near future" depends upon the cost of a <a href="m.html#miss">miss</a>. The working set should fit in the storage level; otherwise the system may <a href="t.html#thrash">thrash</a>.</p>

<p><strong>See also:</strong> <a href="r.html#resident.set">resident set</a>;
    <a href="c.html#cache-2">cache<sup><small>(2)</small></sup></a>;
    <a href="s.html#storage.hierarchy">storage hierarchy</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#ds72">P. J. Denning, S. C. Schwartz. 1972. <cite>Properties of the Working-set Model</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="worst.fit" name="worst.fit">worst fit</a></strong></dt>
<dd><p>
The <a href="a.html#allocation.policy">allocation policy</a> that always allocates from the largest <a href="f.html#free.block">free block</a>. Commonly implemented using a size-ordered <a href="f.html#free.block.chain">free block chain</a>
(largest first).</p>

<p>In practice, this tends to work quite badly because it eliminates all
large blocks, so large requests cannot be met.</p>
<p><strong>See also:</strong> <a href="a.html#allocation.policy">allocation policy</a>;
    <a href="f.html#first.fit">first fit</a>;
    <a href="b.html#best.fit">best fit</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#wil95">Paul R. Wilson, Mark S. Johnstone, Michael Neely, David Boles. 1995. <cite>Dynamic Storage Allocation: A Survey and Critical Review</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="wrapped" name="wrapped">wrapped</a></strong></dt>
<dd><p>A value is wrapped if it is encoded with type information.</p>
<p><strong>Opposites:</strong> <a href="u.html#unwrapped">unwrapped</a>.
<br />
<strong>See also:</strong> <a href="#wrapper">wrapper</a>;
    <a href="b.html#boxed">boxed</a>;
    <a href="t.html#tag">tag</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#gudeman93">David Gudeman. 1993. <cite>Representing Type Information in Dynamically Typed Languages</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="wrapper" name="wrapper">wrapper</a></strong></dt>
<dd><p>A wrapper is that part of a <a href="#wrapped">wrapped</a> representation that is copied when the value is passed by value.</p>
<p>The wrapper does not include parts of the representation that are accessed indirectly, and are not copied when the value is passed.</p>

<p>For instance, a <a href="../articles/lang.html#lisp">Lisp</a> implementation might use the top two bits of a value representation as a <a href="t.html#tag">tag</a> to distinguish between integers and <a href="c.html#cons-1">cons<sup><small>(1)</small></sup></a> cells, setting these bits to 01 for a <a href="p.html#pointer">pointer</a> to a cons cell and 11 for an integer.  Then the wrapped value of the number 4 would have binary representation 11000...00100, and the wrapper for this number is the whole of this wrapped value.  The pointer to a cons cell stored at location 4 would have binary representation 01000...00100.  The wrapped value of the cons cell is the combination of this pointer and the cons cell in memory itself.  The wrapper of the cons cell is just the pointer; when the cons cell is passed as a function argument, just the pointer is passed.</p>

<p><strong>See also:</strong> <a href="#wrapped">wrapped</a>;
    <a href="b.html#boxed">boxed</a>.
</p>
<p><strong>Related publications:</strong></p>
<ul>
  <li><a href="../bib/f.html#gudeman93">David Gudeman. 1993. <cite>Representing Type Information in Dynamically Typed Languages</cite>.</a></li>
</ul><br /></dd>

<dt><strong><a id="write.barrier" name="write.barrier">write barrier</a>, write-barrier</strong></dt>
<dd><p>A write <a href="b.html#barrier-1">barrier<sup><small>(1)</small></sup></a> is a block on writing to certain <a href="m.html#memory-2">memory<sup><small>(2)</small></sup></a> <a href="m.html#memory.location">locations</a> by certain threads or processes.</p>
<p><strong>Relevance to memory management:</strong> Write barriers are used for <a href="i.html#incremental.garbage.collection">incremental</a> or <a href="p.html#parallel.garbage.collection">concurrent</a> <a href="g.html#garbage.collection">garbage collection</a>.  They are also used to maintain <a href="r.html#remembered.set">remembered sets</a> for <a href="g.html#generational.garbage.collection">generational</a> <a href="g.html#garbage.collector">collectors<sup><small>(1)</small></sup></a>.</p>

<p><strong>See also:</strong> <a href="r.html#read.barrier">read barrier</a>.
</p></dd>

<dt><strong><a id="write.fault" name="write.fault">write fault</a></strong></dt>
<dd><p>An exception which occurs when writing to an address in <a href="v.html#virtual.memory-1">virtual memory<sup><small>(1)</small></sup></a>.</p>
<p>This is probably either a <a href="p.html#page.fault">page fault</a>, an <a href="i.html#invalid.page.fault">invalid page fault</a> or a <a href="p.html#protection.fault">protection fault</a>.</p>

<p><strong>Similar terms:</strong> <a href="s.html#segmentation.violation">segmentation violation</a>.
<br />
<strong>See also:</strong> <a href="r.html#read.fault">read fault</a>.
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
<a href="o.html">O</a>
<a href="p.html">P</a>
<a href="q.html">Q</a>
<a href="r.html">R</a>
<a href="s.html">S</a>
<a href="t.html">T</a>
<a href="u.html">U</a>
<a href="v.html">V</a>
<strong>W</strong>
X
Y
<a href="z.html">Z</a> - <a href="help.html">Help</a></p>

</body></html>