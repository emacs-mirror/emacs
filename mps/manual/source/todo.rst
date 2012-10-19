To do, queries and suggestions
------------------------------

3.  Glossary entries need permalink markers. See Sphinx `issue 996`_.

    .. _issue 996:  https://bitbucket.org/birkenfeld/sphinx/issue/996/expose-glossary-entry-link-on-hover

5.  Fix the general index so that glossary entries like "byte (2)"
    aren't interpreted as subentries? (Or is it OK as is?)

7.  Re-do the diagrams in vector form and using the colour palette.

8.  Use a better bibliography extension. See for example the
    (unfortunately unfinished) `sphinx-natbib`_.

    .. _sphinx-natbib: http://wnielson.bitbucket.org/projects/sphinx-natbib/

9.  Hyphenate long function names across line endings (but what if you
    copy them?)

11. Support MMREF-style anchors to the glossary (``#garbage.collection``
    as well as ``#garbage-collection``).

27. Wouldn't it make :c:func:`mps_amc_apply` easier to document if
    there were a typedef for the stepper function type, something like
    this::

        typedef void (*mps_amc_apply_stepper_t)(mps_addr_t object, void *p, size_t s)

    *Answer:* Richard says I can make this change since it's
    backwards-compatible.

37. Some of the Scheme objects could be moved to a leaf-only pool
    (e.g. :ref:`pool-amcz`) in an "advanced topics" section of the
    user guide.

56. Deprecation warnings need nicer formatting. Also "since version
    1.111" is wrong: "starting with version 1.111" would be right.

61. Start adding index entries.



DONE, ANSWERED
--------------

 1. Create a new `domain`_ for MPS stuff.

    .. _domain: http://sphinx.pocoo.org/ext/appapi.html#sphinx.domains.Domain

 2. Create new types of Sphinx objects for pools and topics.

    *Action:* doesn't seem necessary. Ordinary cross-references work fine.

 4. Make "(1)", "(2)" into superscripts.

 6. Identify :term: references that point to "see" cross-references.

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

    *Answer:* This a puzzle! More thought needed. Make a job?

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
    it needs thought. Make a job.

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

    *Answer:* mis-feature. Update the scheme example. Make a job.

    *Action:* updated the Scheme example.

    *Action:* made `job003318`_.

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

    *Action:* done.

42. Move symbol references for the pool classes to the corresponding
    pool document.

    *Action:* done.

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

62. I documented the requirement for tags to be removed when calling
    :c:func:`MPS_FIX`, but does this apply to format auto_header?

    *Answer:* auto_header pool does the subtraction, so this is an
    exception to the rule.

63. How do you create an auto_header format with a class method?

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

    *Answer:* make a job.

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
