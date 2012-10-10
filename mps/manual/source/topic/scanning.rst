.. _topic-scanning:

========
Scanning
========



<h4>Returned Values</h4>

<p>Returns a result code, see ERROR HANDLING.</p>

<p>If the reference rank of the object being scanned is not <code><a href="#MPS_RANK_AMBIG">MPS_RANK_AMBIG</a></code> then the reference pointed to by <code>ref_io</code> may be modified by <code><a href="#mps_fix">mps_fix</a></code>.</p>


<h4>Description</h4>

<p>This function is the part of the scanning protocol used to indicate references. Scanning functions apply it, or <code><a href="#MPS_FIX12">MPS_FIX12</a></code>, or <code><a href="#MPS_FIX1">MPS_FIX1</a></code> and <code><a href="#MPS_FIX2">MPS_FIX2</a></code> to the references in the object being scanned.</p>

<p>It may only be called from within a scanning function. If it is called within a <code><a href="#MPS_SCAN_BEGIN">MPS_SCAN_BEGIN</a></code> block, <code><a href="#MPS_FIX_CALL">MPS_FIX_CALL</a></code> must be used (yes, really).</p>

<p>This function does not perform any particular operation. The MPS may call scanning functions for a number of reasons, and <code><a href="#mps_fix">mps_fix</a></code> may take different actions depending on those reasons.</p>


<h4>Example</h4>

<pre>
mps_res_t scan_array(mps_ss_t ss, mps_addr_t object, size_t length)
{
  size_t i;

  mps_res_t res;
  mps_addr_t *array = (mps_addr_t *)object;
  for(i = 0; i &lt; length; ++i) {
    res = mps_fix(ss, &amp;array[i]);
    if(res != MPS_RES_OK) return res;
  }

  return res;
}
</pre>


<h4>Error Handling</h4>

<p>The function returns <code><a href="#MPS_RES_OK">MPS_RES_OK</a></code> if it was successful, in which case the scanning function should continue to scan the rest of the object, applying <code><a href="#mps_fix">mps_fix</a></code> to the remaining references. If <code><a href="#mps_fix">mps_fix</a></code> returns a value other than <code><a href="#MPS_RES_OK">MPS_RES_OK</a></code>, the scanning function must return that value, and may return without scanning further references. Generally, it is better if it returns as soon as possible.</p>





<h4>Example</h4>

<pre>
mps_res_t scan_array(mps_ss_t ss, Array object, size_t length)
{
  size_t i;

  mps_res_t res;
  mps_addr_t *array = (mps_addr_t *)object;
  MPS_SCAN_BEGIN(ss)
  for(i = 0; i &lt; length; ++i) {
    mps_addr_t ref = array[i];
    if(MPS_FIX1(ss, ref)) {
      /* if(((Object*)ref)-&gt;type == ScannableType) { */
      /* You can do something here, but in the end, you must call MPS_FIX2. */
      res = MPS_FIX2(ss, &amp;array[i]);
      if(res != MPS_RES_OK)
        return res;
      /* } */
    }
  }
  MPS_SCAN_END(ss);

  return res;
}
</pre>




<h4>Example</h4>

<pre>
mps_res_t scan_array(mps_ss_t ss, mps_addr_t object, size_t length) {
  size_t i;

  mps_res_t res;
  mps_addr_t *array = (mps_addr_t *)object;
  MPS_SCAN_BEGIN(ss)
  for(i = 0; i &lt; length; ++i) {
    res = MPS_FIX(ss, &amp;array[i]);
    if(res != MPS_RES_OK)
      return res;
  }
  MPS_SCAN_END(ss);

  return res;
}
</pre>


<h4>Error Handling</h4>

<p>The macro returns <code><a href="#MPS_RES_OK">MPS_RES_OK</a></code> if it was successful, in which case the scanning function should continue to scan the rest of the object, fixing the remaining references. If <code><a href="#MPS_FIX12">MPS_FIX12</a></code> returns a value other than <code><a href="#MPS_RES_OK">MPS_RES_OK</a></code>, the scanning function must return that value, and may return without scanning further references. Generally, it is better if it returns as soon as possible.</p>




<h4>Example</h4>

<pre>
mps_res_t scan_array(mps_ss_t ss, mps_addr_t object, size_t length)
{
  size_t i;
  mps_res_t res;
  mps_addr_t *array = (mps_addr_t *)object;

  MPS_SCAN_BEGIN(ss)
  for(i = 0; i &lt; length; ++i) {
    res = MPS_FIX12(ss, &amp;array[i]);
    if(res != MPS_RES_OK)
      return res;
  }
  MPS_SCAN_END(ss);

  return res;
}
</pre>






<h4>Example</h4>

<pre>
/* Scanner for a simple Scheme-like language with just two interesting types */

mps_res_t scan_objs(mps_ss_t ss, mps_addr_t base, mps_addr_t limit)
{
  mps_res_t res;
  mps_addr_t obj;

  MPS_SCAN_BEGIN(ss)
  for(obj = base; obj &lt; limit;) { /* obj maps over the objects to scan */
    switch(((Object*)obj)-&gt;type) {
    case ArrayType:
      {
        size_t i;
        Array *array = (Array *)obj;

        for(i = 0; i &lt; array-&gt;length; ++i) { /* fix each element */
          res = MPS_FIX12(ss, &amp;array-&gt;contents[i]);
          if(res != MPS_RES_OK) return res;
        }

        obj = AddrAdd(obj, ArraySize(array)); /* move to next object */
        break;
      }

    case StackFrameType:
      {
        StackFrame *frame = (StackFrame *)obj;
        for(i = frame-&gt;size; i &gt; 0; --i) { /* fix each local var */
          res = MPS_FIX12(ss, &amp;frame-&gt;locals[i]);
          if(res != MPS_RES_OK) return res;
        }

        res = MPS_FIX12(ss, &amp;frame-&gt;next);
        if(res != MPS_RES_OK) return res;
        obj = AddrAdd(obj, StackFrameSize(frame));
        break;
      }

    default: /* other types don't contain references */
      obj = AddrAdd(obj, DefaultSize(obj));
      break;

    }
  }
  MPS_SCAN_END(ss);

  return res;
}
</pre>
