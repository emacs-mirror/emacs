To do
=====

Outstanding
-----------

3.  Glossary entries need permalink markers. See Sphinx `issue 996`_.

    .. _issue 996:  https://bitbucket.org/birkenfeld/sphinx/issue/996/expose-glossary-entry-link-on-hover

5.  Fix the :ref:`genindex` so that glossary entries like "byte (2)"
    aren't interpreted as subentries? (Or is it OK as is?)

9.  Hyphenate long function names across line endings. It would be
    possible to do this by inserting soft hyphens but if you copied
    the function name you'd copy the soft hyphens too.

11. Support MMREF-style anchors to the glossary (``#garbage.collection``
    as well as ``#garbage-collection``).

37. Need a ``scheme-advanced.c`` to illustrate advanced topics:

    * Segregate objects into multiple pools (e.g. strings and integers
      into :ref:`pool-amcz`).
    * Global symbol table under MPS management.
    * Tagged references.
    * Unboxed values.
    * "Bumming cycles out of scan" using tags.
    * Weak hash table.

    Compare the performance with the plain ``scheme.c``. The advanced
    version better be faster!

76. Develop debugging examples that illustrate each of the warnings
    about consistency. Also to demonstrate the "garbage collect
    frequently to expose errors as soon as possible" advice.

95. Bring :ref:`lang` up to date. Add C#, Lua, Python.

124. In :ref:`guide-debug`, we need to talk about other debuggers.

     In particular, you can't really work with the MPS with LLDB on
     the Mac yet. Apple knows about this.

     There are a couple of magic commands to type when using WinDbg to
     get it to pass exceptions. We should document those and things
     that need doing with Visual Studio.

125. Need overview for people who already know about garbage
     collection. (RB needs to write this.)

126. Things that are "within reach", i.e. that we could do if people
     needed them. (RB needs to write this.)

140. Document MFS. Explain how it works (putting link in the free
     block) and how this is unlike other pools.

144. In AWL, what are the restrictions on the dependent object? Must
     it be another object in AWL? What are we allowed to do with it?
     What state will the object be in when we look at it? Does it have
     to be fixed?

145. Can you use the same generation chain with more than one pool?


Complete
--------

1.  Create a new `domain`_ for MPS stuff.

    .. _domain: http://sphinx.pocoo.org/ext/appapi.html#sphinx.domains.Domain

2.  Create new types of Sphinx objects for pools and topics.

    *Action:* doesn't seem necessary. Ordinary cross-references work fine.

4.  Make "(1)", "(2)" into superscripts.

6.  Identify ``:term:`` references that point to "see" cross-references.

7.  Re-do the diagrams in vector form and using the colour palette.

8.  Use a better bibliography extension and update the bibliography.
    See for example `sphinx-natbib`_.

    .. _sphinx-natbib: http://wnielson.bitbucket.org/projects/sphinx-natbib/

    I tried downloading and using this but it's clearly not ready yet.
    It uses Pybtex to parse BibTeX, but Pybtex is buggy (its
    ``CaseInsensitiveDict`` class can't be serialized).

    We could do this "by hand" as it were by using normal Sphinx
    cross-references. For example::

        * .. _WIL95:

          Paul R. Wilson, Mark S. Johnstone, Michael Neely, David
          Boles. 1995. "Dynamic Storage Allocation: A Survey and
          Critical Review".

10. Pluralize "Topic" to "Topics" and so on.

12. Superscripts in aka sections.

13. Does the object format description (:c:type:`mps_fmt_A_s` and so
    on) have to be static as in the Scheme example? Or is it OK to
    throw it away after calling :c:func:`mps_fmt_create`?

    Same question for the generation structure
    (:c:type:`mps_gen_param_s`) for a pool and the size classes
    (:c:type:`mps_sac_class_s`) for a segregated allocation cache?

    *Answer:* examination of the code shows that all of these structures
    are copied into internally allocated MPS control structures, and
    so it is OK for the client program to put these structures on the
    stack otherwise dispose of them.

14. What is the difference, if any, between :c:type:`mps_word_t` and
    :c:macro:`MPS_T_WORD`?

    *Answer:* :c:macro:`MPS_T_WORD` comes from ``mpstd.h`` which
    contains no C code (only macro definitions). It used to be the
    case that ``mpstd.h`` was included in ``mps.h`` but it isn't any
    more?  There's something to do here: why don't we include it?
    Because ``mpstd.h`` is a bit picky and could easily be
    broken. :c:macro:`MPS_T_WORD` could be eliminated from the
    external interface.

    *Action:* made `job003315`_.

    .. _job003315: https://info.ravenbrook.com/project/mps/issue/job003315/

15. How can I explain why the Scheme example uses
    ``sizeof(mps_word_t)`` as its alignment? Why not
    :c:macro:`MPS_PF_ALIGN` (or are client programs not supposed to
    look at ``mpstd.h``)? Why not something of its own manufacture, like
    ``sizeof(union {long, size_t, void*})``?

    *Answer:* This a puzzle! More thought needed.

    *Action:* made `job003316`_.

    .. _job003316: https://info.ravenbrook.com/project/mps/issue/job003316/

16. Why does the Scheme example have a copy method in its object
    format when the reference manual says it's obsolete?

    *Answer:* It wasn't obsolete when it was written. I removed it.

17. What is the difference between the "event stream" and the
    "telemetry stream"? Are these names for the same thing? Or is
    there a distinction (for example, "event stream" refers to the
    internal, unfiltered, stream of events and "telemetry stream"
    refers to the filtered stream)?

    *Answer:* the event stream is the implementation of the telemetry
    stream, so the user doc can refer to "telemetry stream".

18. The location dependency functions all take an arena as an
    argument. What is the role of this argument?

    *Answer:* if you have multiple arenas, you are going to have to
    have an :c:type:`mps_ld_t` for each arena, and add each address
    you are interested in for each arena. (If you happen to know which
    arena it's in, you can just specify it for that arena.) So for
    isstale, it better be the same arena as add.

19. What is the role of the third (``addr``) argument to
    :c:func:`mps_ld_isstale`?  ``LDIsStale`` says ``UNUSED(addr);`` so
    maybe it is unused.

    *Answer:* in theory it's the address you want to check, but in the
    implementation it tells you for all addresses. The LD functions
    have an intention that is not quite the same as the design
    documentation. (But what then, asks RB, is the point of
    :c:func:`mps_ld_add`?)  Perhaps in the case of
    :c:func:`mps_ld_isstale` it's "a piece of information that might
    be useful for debugging".

20. Is the material in the pool class comparison table at all accurate?

    *Answer:* It will be better to have a flowchart approach rather than
    a table of properties.

21. This code seems a bit confused about what to do::

        assert(0);
        fprintf(stderr, "Unexpected object on the heap\n");
        abort();
        return MPS_RES_FAIL;

    What should it say?

    *Answer:* the assertion and the return seem to be bogus, so I
    removed them.

22. How does fixing interact with tagged references? Do I need to
    remove the tag before fixing a reference? Do I need to restore the
    tag afterwards? I thought that both would be necessary but the
    `critical path`_ documentation has an example from OpenDylan with
    tagged references that does neither:

    .. _critical path: https://info.ravenbrook.com/project/mps/master/design/critical-path.txt

    *Answer:* we'll document that all references need to be
    decrypted/de-tagged. There ought to be some slack in practice but
    it needs thought.

    *Action:* made `job003317`_.

    .. _job003317: https://info.ravenbrook.com/project/mps/issue/job003317/

23. This code from ``mps_chat`` in the Scheme example is wrong::

        if (type == mps_message_type_gc_start()) {
          printf("Collection %lu started.\n", (unsigned long)mps_collections(arena));

    :c:func:`mps_collections` returns the total number of collections
    to date, not the number of the collection that posted the
    message. This means that if there have been multiple collections
    since the last time the message queue was emptied, the output will
    look like this::

        Collection 47 started.
          ...
        Collection 47 started.
          ...
        Collection 47 started.
          ...

    *Answer:* mis-feature. Update the scheme example.

    *Action:* updated the Scheme example. Made `job003318`_.

    .. _job003318: https://info.ravenbrook.com/project/mps/issue/job003318/

24. It seems "tricky" to re-use fowarding objects as padding objects
    by setting their forwarding pointer to ``NULL``. Wouldn't it be
    simpler to explain if we had ``TYPE_PAD`` for multiple-word
    padding objects? Things are difficult enough to explain as it is!

    *Answer:* It would be simpler, so I made this change.

25. The Scheme example says, "Adapting it to use the MPS took
    approximately two hours". I doubt this would be the common case,
    and it would be better to under-promise here and over-deliver.

    *Answer:* take it out.

26. I need to document the values in :c:type:`mps_gen_param_s`. I
    believe they are the capacity (size of the generation in
    kilobytes) and the mortality (the proportion of objects in this
    generation that are expected to die in a collection). But what do
    they mean to the MPS? And how should one go about choosing values?

    *Answer:* requires a whole document. Talk to RB. The intro
    document should say, "take these values on trust". There's a
    design document about this based on the `Lisp Machine`_.

    .. _Lisp Machine: https://info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mminfo/strategy/lisp-machine/

27. Wouldn't it make :c:func:`mps_amc_apply` easier to document if
    there were a typedef for the stepper function type, something like
    this::

        typedef void (*mps_amc_apply_stepper_t)(mps_addr_t object, void *p, size_t s)

    *Answer:* RB says I can make this change since it's
    backwards-compatible.

28. Wouldn't the Scheme example be better without TAB characters?

    *Answer:* maybe, but it would lead to merge conflicts. So no change.

29. The example code looks better (easier to see the structure) if I
    use an indentation of four spaces. There are also cases where the
    original code is inconsistent (compare the indentation of the case
    labels in ``print`` versus ``obj_scan``). I've made these consistent
    in the user guide.

    Should these changes be propagated back to the example code?

    *Answer:* no.

30. The Scheme example is inconsistent in its use of whitespace: for
    example sometimes there's a space after ``if`` and sometimes not.

    *Answer:* maybe so, but it would lead to merge conflicts. So no
    change.

31. What is a root mode and how do I explain it?

    *Answer:* currently a root mode has no effect. It tells the MPS
    whether it's OK for the MPS to put a barrier on the
    root. Recommend pass zero. For future expansion.

32. You create a marker on the stack and pass it to
    :c:func:`mps_root_create_reg` to tell it where the bottom of the
    stack is.  Fine. But then you are supposed to call your program
    via :c:func:`mps_tramp`. If the MPS is trampolining your whole
    program, why does the MPS need your help to work out where the
    stack is? It could work it out for itself surely?

    I guess this is some kind of separation of concerns --
    :c:func:`mps_tramp` is something to do with protection -- but it's
    hard for me to explain.

    *Answer:* :c:func:`mps_tramp` is needed for your program to work
    on Windows because a barrier hit generates a Structured Exception
    which has to be caught by a handler higher up the stack.

33. Status. At what point will the work be "good enough" to merge back
    to the master sources?

    *Answer:* After RB reviews it.

34. The generic example of using :c:func:`mps_tramp` needs to pass
    ``argv`` and ``argc``, and return an exit code, so maybe it would
    make sense to do that in the Scheme example, even though Scheme
    doesn't use these parameters.

    *Answer:* no.

35. There's a lot of stuff to explain here, and I think some of it
    could be simplified:

    a. The common trampoline case (passing ``argv`` and ``argc``, and
       returning an exit code).

    b. The common single-thread root registration case (e.g. putting a
       marker on the stack in :c:func:`mps_tramp`).

    *Answer:* leave it as it is.

36. The discussion in the Scheme example about :c:func:`mps_reserve`
    suggests that :c:func:`mps_alloc` doesn't require aligned
    sizes. Is that right? Needs to be added to :c:func:`mps_alloc`
    reference if so.

    *Answer:* in fact there's no rule about this. Depends on the pool
    class. "It doesn't unless the pool class says it does".

38. Document about interface conventions and interface policies. What
    do we guarantee about support for the external symbols?

    *Action:* added new topic :ref:`topic-interface`.

39. We don't support scanning the stack/registers except via
    mps_stack_scan_ambig? Document this?

    *Answer:* this is the only one we support at the moment.

40. The ``fragmentation_limit`` argument to :c:func:`mps_class_mvt` is
    an integer representing a percentage between 1 and 100. For
    consistency with mps_gen_param_s this should be a double between 0
    (exclusive) and 1. Can we change this?

    *Action:* made `job003319`_.

    .. _job003319: https://info.ravenbrook.com/project/mps/issue/job003319/

41. Move symbol reference from ``mpsio.h``, ``mpstd.h`` and
    ``mpslib.h`` to :ref:`topic-plinth`.

42. Move symbol references for the pool classes to the corresponding
    pool document.

43. In the "choosing a pool" procedure there's no mention of ambiguous
    references. I omitted them because the NB/RIT chart of pool
    properties seemed to suggest that no pools can contain ambiguous
    references. But is this right? Couldn't you allocate your block
    containing ambiguous references in a non-moving, non-scanning pool
    like MVT and register it as an ambiguous root?

    *Answer:* Ought to be a note to explain that there may more thing to
    take into account for experts. "For beginners". Ambiguous
    references not currently supported via scanning/automatic pools
    etc: use workaround.

44. In the "choosing a pool" procedure there's no mention of
    protection. Can we subsume this under "moving"?

    *Answer:* Use "movable and protectable".

45. Does :c:func:`mps_arena_step` offer any guarantees about how long
    it will pause for? (I presume not: I've written "makes every
    effort to return within interval seconds, but does not guarantee
    to do so".)

    *Answer:* No guarantee. "Since it calls your scanning code..."

46. RHSK's documentation for :c:func:`mps_arena_step` says "Note that
    :c:func:`mps_arena_step` will still step, even if the arena has
    been clamped. This is to allow a client to advance a collection
    only at these mps_arena_step points (but note that barrier hits
    will also cause collection work)." However, looking at the
    implementation, it seems to me that if :c:func:`mps_arena_step`
    does anything, then it releases the arena. Is the client program
    expected to call :c:func:`mps_arena_clamp` after
    :c:func:`mps_arena_step` returns, if they want to support this use
    case?

    Similar question if the arena is in the parked state.

    *Answer:* this is a bug.

    *Action:* made `job003320`_.

    .. _job003320: https://info.ravenbrook.com/project/mps/issue/job003320/

47. How does :c:func:`mps_arena_start_collect` interact with the arena
    state?  (It seems from looking at the implementation that it puts
    the arena into the unclamped state, like
    :c:func:`mps_arena_release`.)

    *Answer:* this is fine.

48. How do I explain the condition on :c:func:`mps_addr_pool` and
    :c:func:`mps_addr_fmt`?  It's clear what this is if the address is
    the address of an allocated block? But do we guarantee anything if
    it isn't? The implementation succeeds if the address points
    anywhere within a page allocated to a pool.

    *Answer:* don't specify result except in supported cases. Not
    false positive but never false negative.

49. Constraints on order of destructors. Is it an error to destroy an
    object format while there are still pools that refer to it?

    *Answer:* yes.

50. ``mps.h`` declares the type ``mps_shift_t`` for a "shift amount"
    but this is not used anywhere in the MPS. Remove?

    *Action:* removed in `change 179944`_.

    .. _change 179944: http://info.ravenbrook.com/infosys/cgi/perfbrowse.cgi?@describe+179944

51. The structures ``mps_sac_s`` and ``mps_sac_freelist_block_s`` are
    declared in mps.h. I presume that this is so for the benefit of
    the macros :c:func:`MPS_SAC_ALLOC_FAST` and
    :c:func:`MPS_SAC_FREE_FAST`, and the details of these structues
    are not actually part of the public interface.

    *Answer:* RB thinks the client doesn't need to know about these
    structures, so they ought to have names starting with
    underscores.

    *Action:* made `job003321`_.

    .. _job003321: https://info.ravenbrook.com/project/mps/issue/job003321/

52. ``mps_fmt_fixed_s`` is just like :c:type:`mps_fmt_A_s` but with no
    "skip". I presume it's for fixed-size pools. But the only
    fixed-size pool is MFS, which has no public header, so I presume
    it's for MPS internal use only. What should I say about this?

    *Answer:* leave it undocumented. Maybe it (and
    ``mps_fmt_create_fixed``) should be removed?

    *Action:* made `job003322`_.

    .. _job003322: https://info.ravenbrook.com/project/mps/issue/job003322/

53. It's rather unfortunate that :c:func:`mps_arena_create` take
    arguments in the order ``size``, ``base`` but
    :c:func:`mps_arena_extend` takes them in the order ``base``,
    ``size``. I guess there's nothing to be done about this now.

    *Answer:* RB plans to remove :c:func:`mps_arena_create` and
    other varargs functions.

    *Action:* made `job003323`_.

    .. _job003323: https://info.ravenbrook.com/project/mps/issue/job003323/

54. Need notes about performance. It's important to know that
    :c:func:`mps_addr_pool` is really quick and it's fine to call
    it. But other things are slow, e.g. walking over all objects.

    *Action:* made `job003324`_.

    .. _job003324: https://info.ravenbrook.com/project/mps/issue/job003324/

55. Mark all the allocation frame stuff as deprecated in the manual.

    *Action:* I marked them all as deprecated in version 1.111.

56. "since version 1.111" is wrong: "starting with version 1.111"
    would be right.

57. :c:func:`mps_collections` should of course have been called
    :c:func:`mps_arena_collections` but it's pretty meaningless
    because the collector is asynchronous (as we saw in the Scheme
    example). It might make more sense to deprecate it and add a new
    function :c:func:`mps_message_gc_start_collection` that returns the
    collection number for a :c:func:`mps_message_type_gc`.

    *Action:* added to `job003318`_.

    .. _job003318: https://info.ravenbrook.com/project/mps/issue/job003318/

58. After you destroy an allocation point, can you continue to use the
    objects you allocated on it?

    *Answer:* the objects you have committed are fine.

59. What's the condition for :c:func:`mps_thread_reg` when you have
    multiple arenas? Do you need to register each thread with each
    arena? Or is it acceptable to register a thread with only some of
    your arenas so long as you satisfy some condition? (e.g. that the
    thread promises only to access blocks allocated in the arenas it
    is registered with?)

    Similarly for :c:func:`mps_thread_dereg`: "after deregistration,
    the thread must not access any blocks allocated in that arena."

    *Answer:* a thread that never uses a pointer to an address in an
    automatically managed pool need not be registered. Note: recommend
    that the user register all threads and scan all their stacks.

60. What's the condition for needing to register a thread at all? If
    you have a single-threaded program does that thread still need to
    be registered? What if you have no moving pools? etc.

    *Answer:* see above.

61. Start adding :ref:`genindex` entries.

62. I documented the requirement for tags to be removed when calling
    :c:func:`MPS_FIX`, but does this apply to format auto-header?

    *Answer:* auto-header pool does the subtraction, so this is an
    exception to the rule.

63. How do you create an auto-header format with a class method?

    *Answer:* this is just missing functionality. Also, not clear what
    the class method is for.

    *Action:* made `job003325`_.

    .. _job003325: https://info.ravenbrook.com/project/mps/issue/job003325/

64. Doc for :c:func:`mps_arena_class_vmnz` says, "This class is
    similar to :c:func:`mps_arena_class_vm`, except that it has a
    simple placement policy (“no zones”) that makes it slightly
    faster." Presumably there's a corresponding disadvantage,
    otherwise why would you use :c:func:`mps_arena_class_vm` rather
    than this?

    *Answer:* There's a massive disadvantage: the lack of zones makes
    automatic memory management much slower. Remove the sentence. Not
    clear what :c:func:`mps_arena_class_vmnz` is for (maybe when you
    have no automatic pools?)

    *Action:* made `job003326`_.

    .. _job003326: https://info.ravenbrook.com/project/mps/issue/job003326/

65. Some of the :c:type:`mps_ap_s` structure is public. What are the
    use cases for the client program accessing these values other than
    via :c:func:`mps_reserve` and :c:func:`mps_commit`? Wouldn't they
    need to know about :c:func:`mps_ap_fill` and :c:func:`mps_ap_trip`
    if they were doing their own thing?  But these function have
    comments saying "should never be "called" directly by the client
    code."

    *Answer:* if you're not writing client code in C (e.g. writing a
    compiler, and you want to inline your allocation: you can't use
    the macros, you can generate the equivalent code). The comments in
    ``mpsi.c`` are simply wrong. See RB's `issue 235`_ on the OpenDylan bug
    tracker for advice about what's expected.

    .. _issue 235: https://github.com/dylan-lang/opendylan/issues/235

    *Action:* See what refers to these bogus comments and see if
    there's a reason. (There isn't.) If not: reverse the sense of the
    comments and refer to manual. (Done in `change 179971`_.)

    .. _change 179971: http://info.ravenbrook.com/infosys/cgi/perfbrowse.cgi?@describe+179971

66. What about reservoirs? Is the idea that the client's handler for
    low-memory situations (whether it's an error message or whatever)
    might need to allocate? And so you can ask the MPS to reserve a
    reservoir for this situation?

    *Answer:* leave it undocumented for now.

    *Action:* made `job003327`_.

    .. _job003327: https://info.ravenbrook.com/project/mps/issue/job003327/

67. Potential optimization of reserve/commit protocol: maybe we don't
    need to call :c:func:`mps_ap_trip`?

    *Action:* made `job003328`_.

    .. _job003328: https://info.ravenbrook.com/project/mps/issue/job003328/

68. What are we supposed to say about :c:func:`mps_ap_fill` and
    :c:func:`mps_ap_trip`?

    *Answer:* Say that they should only be called as part of the
    allocation point protocol.

69. What is the purpose of :c:func:`MPS_RESERVE_BLOCK`? It does the
    same thing as :c:func:`mps_reserve`, but can only be used as a
    statement, whereas the latter can also be used as an
    expression. So I can't say, "in such-and-such a circumstance use
    :c:func:`MPS_RESERVE_BLOCK`".

    *Answer:* it takes an lvalue instead of a pointer so it may
    generate better code.

70. "Reserve/commit protocol" or "allocation point protocol"?

    *Answer:* the latter.

71. What about :c:func:`mps_alert_collection_set`? Seems dodgy to me.

    *Answer:* leave it undocumented.

    *Action:* made `job003329`_.

    .. _job003329: https://info.ravenbrook.com/project/mps/issue/job003329/

72. When a ``.. note::`` block contains a numbered list with multiple
    items (as :ref:`here <guide-lang-scan>`) or multiple footnotes (as
    :ref:`here <pool-properties>`) the heading should say "Notes".

73. The names of the sectors on the "treadmill" diagram don't
    correspond exactly to the names in `Baker 1991`_.

    .. _Baker 1991: http://www.pipeline.com/~hbaker1/NoMotionGC.html

    *Answer:* these names come from the diagram in :ref:`Jones (1996)
    <JONES96>` and refer to concepts that are missing from the
    glossary: :term:`fromspace`, :term:`tospace` (with synonyms
    :term:`old space` and :term:`new space`).

    *Action:* add these concepts to the glossary. Label the treadmill
    diagram with "fromspace", "tospace", and so on, rather than just
    "from" and "to". Here and elsewhere, refer to :ref:`Jones (2012)
    <JONES12>` for preferred terminology. See in particular page 138.

74. ``eventcnv -b <bucket size>`` seems to be useless. See
    `job003331`_, `job003332`_, `job003333`_, `job003334`_, and
    `job003335`_. NB's work on eventsql suggests that maybe there's no
    point in trying to make it work again (because eventsql will be
    able to do this kind of aggregation and much more). So maybe best
    to deprecate or remove this feature? For the moment I've left it
    undocumented.

    .. _job003331: https://info.ravenbrook.com/project/mps/issue/job003331/
    .. _job003332: https://info.ravenbrook.com/project/mps/issue/job003332/
    .. _job003333: https://info.ravenbrook.com/project/mps/issue/job003333/
    .. _job003334: https://info.ravenbrook.com/project/mps/issue/job003334/
    .. _job003335: https://info.ravenbrook.com/project/mps/issue/job003335/

    *Answer:* ``eventcnv`` was always intended to be a simple tool that
    just translates events, not a report generator.

    *Action:* remove the -e, -b and -v options. (-v should be always
    turned on.) Also remove the label processing. Look at NB's branch
    in case he's done some of this work already. (He hasn't.)

75. Write something about telemetry labels in the telemetry chapter of
    the reference manual.

77. The debugging section on :ref:`guide-debug-underscanning` is not
    entirely convincing as to the utility of the telemetry. I'd like
    to have a more compelling example here.

    *Answer:* a better example is going to have to wait for better
    tools.

78. Should memory addresses increase going up the page, or down the
    page? The output of gdb's ``x`` command always has them going down
    the page so maybe we should follow that? It would involve turning
    a bunch of diagrams upside down.

    *Answer:* :ref:`Jones (2012) <JONES12>` has addresses increasing
    as they go down the page, so let's follow that.

79. Suppose that in the Scheme interpreter you wanted to return some
    statistics from the ``(gc)`` function, how would you do this? You
    could poll the message queue after calling
    :c:func:`mps_arena_collect` and before calling
    :c:func:`mps_arena_release`, I suppose.

    *Answer:* this might happen to work, but it's not supported.

80. Need to migrate the new Scheme functionality (new make_string, fix
    to append, new entry points) back to the "before the MPS" version.

    *Action:* done in `change 180069`_.

    .. _change 180069: http://info.ravenbrook.com/infosys/cgi/perfbrowse.cgi?@describe+180069

81. The Scheme interpreter could avoid printing "``#[undefined]``" at
    the REPL (like Python with  ``None``).

    *Answer:* not useful.

82. Is the message example correct?

    *Answer:* no, the MPS doesn't guarantee the timely delivery of
    messages, so this example is invalid. Remove it.

    *Action:* what we could do is return a collection ID from
    :c:func:`mps_arena_collect`, :c:func:`mps_arena_step` and
    :c:func:`mps_arena_park`, and provide a mechanism to get the
    collection ID from the collection message. I added this suggestion
    to `job003318`_.

83. If a block has been finalized, can you "resurrect" it and
    re-register it for finalization?

    *Answer:* make a note about resurrection in the MPS not being the
    same as the usual notion. It's fine to re-register after
    retrieving the message.

84. If a block has been registered for finalization more than once,
    what happens if you deregister it? Do you have to deregister it as
    many times as you registered it? Or do you only need to deregister
    it once?

    *Answer:* say nothing about this esoteric subject.

85. What exactly is our policy about support for the documented
    behaviour of public identifiers. "The documented behaviour of
    public identifiers will not be changed in a backward-incompatible
    fashion without a period of notice lasting at least one version."

    *Answer:* "We intend to support the existence and behaviour of
    documented symbols. We will only remove or change these on a
    version change (not between patch releases). Normally one
    version's notice."

86. In ``buffer.c`` it says::

        /* Assumes pun compatibility between Rank and mps_rank_t */
        /* Which is checked by mpsi_check in <code/mpsi.c> */

    but I see no such check in ``mpsi_check``.

    *Answer:* the comment was wrong: ranks are no longer part of the
    public interface. Removed in `change 180031`_.

    .. _change 180031: http://info.ravenbrook.com/infosys/cgi/perfbrowse.cgi?@describe+180031

87. Could simplify a lot of glossary references if plurals were
    handled automatically. That is, if a glossary entry for "bytes
    (1)" is found to be missing, then "byte (1)" should be tried
    instead. (Similarly for hyphens versus spaces.) 

88. Where glossary entries differ on in hyphens versus spaces there's
    no need to give both.

89. ``MPS_TELEMETRY_CONTROL=65535`` is ugly. Why not
    ``MPS_TELEMETRY_CONTROL=all``?

    *Action:* made `job003340`_.

    .. _job003340: https://info.ravenbrook.com/project/mps/issue/job003340/

90. Chapters in the User Guide need a conclusion of some kind. e.g. in
    debugging, what should you do if this hasn't helped?

    *Action:* added "What now?" section.

91. Chapter about the community resources. "Get in touch". See LLVM
    and Boost for examples of this kind of thing. Put it in the
    sidebar so it's always there.

    *Action:* added "Contact us" page and link in the sidebar of every
    page.

92. Extensibility: "Writing your own pool or need a pool that's not
    listed here: have a go, but get in touch." We can write new pool
    classes for you.

    *Action:* added a section to the Pool reference.

93. Multi-core is not supported (except in the case of multiple
    arenas). See the `Hacker News thread
    <http://news.ycombinator.com/item?id=4521988>`_. Could be done by
    running the MPS in a separate process but that might be horrible
    and heavyweight.

    There may be more documentation needed here (in
    :ref:`topic-thread`): if the mutator is multi-threaded, it needs
    to reserve ``SIGUSR1`` (or whatever the actual signal is) for use
    by the MPS. [It turned out that ``SIGBUS`` or ``SIGSEGV`` is used
    for this purpose, not ``SIGUSR1``.]

    Also ``SIGBUS`` (xc) or ``SIGSEGV`` (fr, li) or "first chance
    exception handler" (w3). Affects debugging. Also, if you handle your
    own SIGBUS etc then you need to give us a call.

    *Action:* added a section on "Signal handling issues" to
    :ref:`topic-thread`. (Not clear that this is the right place,
    since it isn't really thread-specific, but I couldn't find a
    better place and I can always move it later.)

94. Maybe target R4RS instead of R6RS? (R4RS is the "one true"
    Scheme.)

    *Action:* fixed in `change 180033`_.

    .. _change 180033: http://info.ravenbrook.com/infosys/cgi/perfbrowse.cgi?@describe+180033

96. Change "event" to "event category" in discussion of the telemetry
    filter.

97. In :ref:`guide-debug`, need to provide more context around each
    assertion, using GDB command ``list``.

98. In the "Cautions" section of :ref:`topic-finalization`, add
    something to effect of, "A block might be dead and yet not
    finalizable, because it is being kept alive by an ambiguous
    reference."

99. What does this mean in :ref:`topic-finalization`:
    "mps_pool_destroy() should therefore not be invoked on pools
    containing objects registered for finalization."

    *Answer:* This is clearly a misfeature if true, because how could
    you destroy a pool containing finalizable objects?

    *Action:* make `job003341`_

    .. _job003341: https://info.ravenbrook.com/project/mps/issue/job003341/

100. Try to find "GARBAGE COLLECTING... HERE ARE SOME INTERESTING
     STATISTICS" early Lisp anecdote and put it in
     :ref:`topic-telemetry`.

     The anecdote is at the end of :ref:`McCarthy (1979) <MCCARTHY79>`.

101. Are there any other use cases for the clamped and parked states?
     Are there any use cases that apply specifically to the parked
     state?

     *Answer:* clamp prevents a flip and so buffers can't be trapped,
     and so :c:func:`mps_commit` will always succeed, and so
     allocation will always run at max speed.

     It might be helpful for debugging, for example you might want to
     type it at the GDB prompt to ensure that nothing moves around
     while you are debugging.

102. It's kind of a shame that the MPS has two means for "committed".
     :term:`committed (1)` meaning "mapped to RAM", as in
     :c:func:`mps_arena_commit_limit`; and :term:`committed (2)`
     meaning "initialized and placed under management by the MPS", as
     in :c:func:`mps_commit`. Probably too late to do anything about
     this.

     *Answer:* too late to change.

103. The documentation for :c:func:`mps_arena_has_addr` says, "call
     this function and interpret the result while the arena is in the
     :term:`parked state`".  Similarly, :c:func:`mps_arena_roots_walk`
     says "This function may only be called when the arena is in the
     :term:`parked state`." What's wrong with the clamped state in
     these cases? (I can see that :c:func:`mps_arena_roots_walk`
     asserts if not in the parked state, but I guess I'd like an
     explanation.)

     *Answer:* there's still stuff going on in the clamped state.

104. Are there use cases for :c:func:`mps_arena_collect` other than
     development and testing?

     *Answer:* probably not. "You might think that..."

105. It's a shame that the names :c:func:`mps_arena_release` and
     "unclamped state" don't match. Could I call the "unclamped state"
     the "released state" in the documentation?

     *Answer:* too late to change.

106. After calling :c:func:`mps_arena_expose`, how do you restore the
     protection? Do you have to call :c:func:`mps_arena_release` or
     are there other functions that will do the job, e.g.
     :c:func:`mps_arena_collect`, :c:func:`mps_arena_start_collect`,
     or :c:func:`mps_arena_step`?

     *Answer:* the protection will arise naturally: no need to call
     anything specific.

107. :c:func:`mps_arena_unsafe_expose_remember_protection` and
     :c:func:`mps_arena_unsafe_restore_protection` are kind of hairy
     (as well as having absurd names). What's the use case? Exposing
     the MPS "is expected only to be useful for debugging" so why have
     special unsafe functions for optimizing the expose/restore
     procedure? Maybe these should be left undocumented?

     *Answer:* Deprecate them for the moment. Make a job to
     investigate the use case.

     *Action:* made `job003342`_

     .. _job003342: https://info.ravenbrook.com/project/mps/issue/job003342/

108. Have I correctly explained why the documentation is not very
     forthcoming about the set of result codes a function might
     return?

     *Answer:* the paragraph is fine, but there's a job here. For some
     functions we could say something along the lines of: "if you
     receive MPS_RES_X, that means Y".

     *Action:* made `job003343`_

     .. _job003343: https://info.ravenbrook.com/project/mps/issue/job003343/

109. NB's `MPS Format Protocol
     <http://info.ravenbrook.com/project/mps/doc/2002-06-18/obsolete-mminfo/mmdoc/protocol/mps/format/index.html>`_
     says that formats A and B are deprecated. Surely this isn't the
     case? That would leave only auto-header supported.

     *Answer:* ask NB, he wrote it.

110. Say something about assertions and varieties in the error chapter.

111. Need discussion in :ref:`topic-format` under
     :c:type:`mps_fmt_auto_header_s` about client pointers versus base
     pointers.

112. What's the purpose of allocation frames and the SNC pool class?
     Perhaps ask Pekka about how it's worked out for Global Graphics.

     *Action:* made `job003344`_

     .. _job003344: https://info.ravenbrook.com/project/mps/issue/job003344/

113. Note about sizes of generations in the Scheme example are
     deliberately chosen to be small so you can see it working.

114. What's the purpose of segregated allocation caches?

     *Answer:* There might be a paper about SACs. Run program once and
     write down the sizes of the objects you allocate. Change malloc
     into a macro that tests the size (because the size is almost
     always constant). Dramatic improvement in performance and
     reduction in fragmentation. Interface to a pool that allows it to
     exploit this pattern. Perhaps in use in ScriptWorks? Write to
     Pekka and ask him how this has worked out. (After 1.111.0.)

115. Need to associate paragraph of text with deprecation notice. (See
     for example :c:func:`mps_fix`.) Also, deprecation notices need
     nicer formatting.

116. "The maximum number of arenas that the system can support is
     around 10." Is this true? What goes wrong? (I tried creating lots
     of arenas and nothing seemed to go wrong.)

     *Answer:* this was probably a finger in the air. Remove it.

117. Consistent pluralization in headings. "Arena states". "Other
     notes" → "Ambiguous references". "You could use this to optimize
     the scan by avoiding the need to reassemble and store the updated
     reference after calling :c:func:`MPS_FIX2`.

118. Add comments giving links to sources.

119. This (from :ref:`topic-critical`) needs mentioning in various
     places: "it's important to give a good estimate of the amount of
     address space you will ever occupy with objects when you
     initialize the arena".

120. "Magic" needs explaining in :ref:`topic-location`.

     *Answer:* that's because we hit it: it didn't move.

121. Documentation needs copying into scheme.c.

122. What's a "class structure"? (See :c:func:`mps_sac_alloc` and
     :c:func:`mps_sac_free`.) Does it mean "exactly the same set of
     size classes in the same order"? Or even "exactly the same
     pointer to array of size classes"?

     *Answer:* probably the former. Check the code.

123. Needs to be a topic on low memory handling. How can you do this
     while still keeping the allocation fast and inline? How does the
     MPS behave when it's low on memory? Performance degrades (due to
     running out of zones) and then there are emergency collections.

     *Action:* created :ref:`topic-low`.

127. :c:func:`mps_arena_roots_walk` says, "A client-supplied function
     is called for every root reference which points to an object in
     any automatically managed pools". Is this right?

     *Answer:* the description seems to be right. What's the use case?
     If none, deprecate it.

128. From :ref:`topic-thread`: "A thread must be registered with an
     :term:`arena` if it ever uses a pointer to a location in an
     :term:`automatically managed <automatic memory management>`
     :term:`pool` belonging to that arena." This isn't quite right,
     and the real requirement is quite hard to document, so perhaps we
     need to make this a documented property of the pool ("whether or
     not threads that access memory allocated in the pool need to be
     registered"). Similarly in :c:func:`mps_thread_dereg`.

129. What happens if a thread is killed or otherwise terminates
     without being deregistered? Does the MPS go wrong?

     *Answer:* Probably all we do is try to send signals to it. What
     does pthreads do with this? It may be hard for the client program
     to deregister all their threads: better for the MPS to cope
     gracefully.

     *Action:* made `job003348`_.

     .. _job003348: https://info.ravenbrook.com/project/mps/issue/job003348/

130. readme.txt should contain a brief overview and pointers to more
     documentation, and should (only) duplicate other documentation.
     There's nothing wrong with it being somewhat redundant. Its
     supported platforms section doesn't exist elsewhere. Note that
     the platforms we build with (in build.txt) is not the same list.

131. Is :c:type:`mps_clock_t` a transparent alias for ``unsigned
     long``? I presume it must be: if it were platform-specific, or
     supplied by the plinth, then it wouldn't be defined in ``mps.h``,
     surely?

     *Answer:* this is a bug, fixed in change 180151.

132. :c:func:`mps_lib_get_stderr` appears in ``mpslib.h`` and
     ``mpsliban.c`` but is not called by the MPS (it uses
     :c:func:`mps_lib_assert_fail` for assertions). Should this be
     documented to reserve the option of using it, or should it be
     left out?

     *Action:* Keep stderr documented. Add a note that it's not used
     but for possible future use. Same thing about stdout.

133. What's the purpose of ``mps_SEH_filter`` and ``mps_SEH_handler``?
     Do they need to be documented?

     *Action:* made `job003349`_.

     .. _job003349: https://info.ravenbrook.com/project/mps/issue/job003349/

134. What's the use case for AMS? It's suitable when you have blocks
     that need to be automatically managed but can't be moved. But
     when does this happen? If foreign code maintains remembers their
     location then it seems unlikely that they can be automatically
     managed (how can these foreign references keep the blocks
     alive?).

     *Answer:* It's useful for a step in an integration because its
     automatic but non-moving. But not ready for production: best to
     plan to switch to AMC later on. Could be developed into a more
     solid mark-and-sweep pool. Contact us.

135. Does AMS use protection? I see that it calls ``ShieldExpose`` and
     ``ShieldCover`` when calling the format's skip method, but not
     otherwise (e.g. when calling the format's scan method). If it
     does I need to update the pool choice algorithm.

     Similar question for SNC? It calls ``ShieldExpose`` and
     ``ShieldCover`` when calling the format's pad method, but not
     otherwise.

     *Answer:* The rule is "if it needs to be scanned, either it gets
     scanned atomically on flip, or else it gets read-protected". So
     objects in both of these pools get protected.

136. It seems possible that MV should not be used. What should I say
     about this? Should we remove MV from the documentation
     altogether.

     *Action:* made `job003350`_.

     .. _job003350: https://info.ravenbrook.com/project/mps/issue/job003350/

137. What does the "No Checking" in "Stack No Checking" refer to?

     *Action:* Added to `job003344`_. Deprecated SNC.

138. The hash table implementation is a bit rubbish: it waits until
     the table is completely full before rehashing. Should ensure that
     it never gets more than 50% full. Also the constructor should be
     called ``make-eq-hashtable`` following R6RS.

139. It seems a shame that MVT doesn't support :c:func:`mps_alloc` as
     this would be fairly trivial via an internal :term:`allocation
     point`.

     *Action:* added to `job003350`_.

141. Do we support instruction emulation on x86-64? DL says that it's
     not in yet. What about OS X?

     *Action:* made `job003352`_.

     .. _job003352: https://info.ravenbrook.com/project/mps/issue/job003352/

142. Weakness: how to detect splatting. (In scan.) Cope with NULL. Do
     all references get splatted at the same time?

143. Document the purpose of AWL and explain that it's for OpenDylan.
     If you require more general implementation of weakness, contact
     us.
