.. _topic-root:

=====
Roots
=====



<p><code><a href="#MPS_RM_CONST">MPS_RM_CONST</a></code> is a preprocessor macro defining a constant that can be OR'ed with other <code>MPS_RM_*</code> constants, and passed as the root mode argument to certain root creation functions (<code><a href="#mps_root_create">mps_root_create</a></code>, <code><a href="#mps_root_create_fmt">mps_root_create_fmt</a></code>, <code><a href="#mps_root_create_table">mps_root_create_table</a></code>, <code><a href="#mps_root_create_table_masked">mps_root_create_table_masked</a></code>, <code><a href="#mps_root_create_reg">mps_root_create_reg</a></code> ).</p>


from MPS_RM_PROT:

<p>No page may contain parts of two or more roots with <code><a
href="#MPS_RM_PROT">MPS_RM_PROT</a></code> [how does one prevent
that?]. You mustn't specify <code><a
href="#MPS_RM_PROT">MPS_RM_PROT</a></code> if the client program or
anything other than (this instance of) the MPS is going to protect or
unprotect the relevant pages.</p>

<h4>Internal Notes</h4>

<p>Future meaning: The MPS may place a hardware read and/or write barrier on any pages which any part of the root covers. Format methods and scanning functions (except for the one for this root) may not read or write data in this root. You may specify <code><a href="#MPS_RM_PROT">MPS_RM_PROT</a></code> on a root allocated from the MPS, as long as it's not from a GCd pool. - drj 1997-12-18</p>

<p>This feature is far too technical for most of our clients: we should think about producing some guidelines on how to use it. - pekka 1998-01-27</p>

<p>There may be problems if the client wants the OS to access the root. Lots of OSes can't cope with writing to protected pages. So we'll need to document that caveat too. drj 1998-05-20</p>



<h4>Example</h4>

<pre>
static mps_root_t mmRoot;

int main(void)
{
  mps_res_t res;

  /* ... */

  res = mps_root_create(&amp;mmRoot, arena, MPS_RANK_EXACT, (mps_rm_t)0,
                        &amp;rootScanner, NULL, 0);
  /* see doc of mps_root_scan_t for definition of rootScanner */
  if(res != MPS_RES_OK)
    exit(1);

  /* ... */
}
</pre>




    .. note::

        Unless the rank of the root is not :c:macro:`MPS_RANK_AMBIG`,
        the contents of the root have to be valid whenever a
        :term:`garbage collection` happens. That is, all the
        references fixed by the root scanning function have to be
        references to actual objects or null pointers. If you're using
        :term:`asynchronous` garbage collection, this could be as soon
        as the root is registered, so the root has to be valid when it
        is registered. As with an ordinary :term:`scan method`, a root
        scanning function is allowed to fix references which point to
        memory not managed by the MPS. These references will be
        ignored.




<h4>Example</h4>

<pre>
static mps_root_t mmRoot;
SegmentDescriptor DataSegment;

int main(void)
{
  mps_res_t res;

  /* ... */

  res = mps_root_create_fmt(&amp;mmRoot, arena, MPS_RANK_EXACT, (mps_rm_t)0,
    &amp;scan_objs,
    (mps_addr_t)DataSegment.base,
    (mps_addr_t) (DataSegment.base + SegmentLength) );

  /* see doc of mps_fmt_scan_t for definition of scan_objs */

  if(res != MPS_RES_OK)
    exit( EXIT_FAILURE );

  /* ... */
}
</pre>





<h4>Example</h4>

<pre>
typedef struct {
  mps_root_t mmRoot;
  mps_thr_t thread;
  /* ...  */
} ThreadLocals;

void InitThread(ThreadLocals *thr)
{
  /* This is a hack to find the bottom of the stack. */
  void *stackBottom=&amp;stackBottom;

  mps_thread_reg(&amp;thr-&gt;thread, arena);
  mps_root_create_reg(&amp;thr-&gt;mmRoot, arena, MPS_RANK_AMBIG, (mps_rm_t) 0,
    thr-&gt;thread, mps_stack_scan_ambig, stackBottom, 0);

  /* ...  */

}
</pre>






<h4>Example</h4>

<pre>
static mps_root_t mmRoot;
Object *Objects[rootCOUNT];

int main(void)
{
  mps_res_t res;

  /* ... */

  res = mps_root_create_table(&amp;mmRoot, arena, MPS_RANK_EXACT, (mps_rm_t)0,
                              (mps_addr_t) &amp;Objects, rootCOUNT );

  if(res != MPS_RES_OK)
    exit(1);

  /* ... */
}
</pre>




<h4>Example</h4>

<pre>
#define tagMASK 0x0003

static mps_root_t mmRoot;
Object *Objects[rootCOUNT];

int main(void)
{
  mps_res_t res;

  /* ... */

  res = mps_root_create_table_masked(&amp;mmRoot, arena, MPS_RANK_EXACT, (mps_rm_t)0,
                                     (mps_addr_t)&amp;Objects, rootCOUNT,
                                     (mps_word_t)tagMASK);
  if(res != MPS_RES_OK)
    exit(1);

  /* ... */
}
</pre>





<h4>Example</h4>

<pre>
static StackFrame *stackBottom;

/* root scanner for an imaginary interpreter for a stack-oriented language */
static mps_res_t rootScanner(mps_ss_t ss, void * p, size_t s)
{
  StackFrame *frame;
  size_t i;
  mps_res_t res;

  UNUSED(p);
  UNUSED(s);

  for(frame = stackBottom; frame != NULL; frame = frame-&gt;next)
    for(i = frame-&gt;size; i &gt; 0; --i) {
      res = mps_fix(ss, &amp;frame-&gt;locals[i]);
      if(res != MPS_RES_OK) return res;
    }

  return res;
}
</pre>





<h4>Example</h4>

<pre>
typedef struct {
  mps_root_t mmRoot;
  mps_thr_t thread;
  /* ... */
} ThreadLocals;

void InitThread(ThreadLocals *thr)
{
  /* This is a hack to find the bottom of the stack. */
  void *stackBottom=&amp;stackBottom;

  mps_thread_reg(&amp;thr-&gt;thread, arena);
  mps_root_create_reg(&amp;thr-&gt;mmRoot, arena, MPS_RANK_AMBIG, (mps_rm_t)0,
    thr-&gt;thread, mps_stack_scan_ambig, stackBottom, 0)

  /* ... */
}
</pre>



<h4>Example</h4>

<pre>
  mps_thr_t this_thread;
  mps_res_t res;

  res = mps_thread_reg(&amp;this_thread, space);
  if(res != MPS_RES_OK) return res;
</pre>
