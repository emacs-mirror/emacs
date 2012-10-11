.. _topic-error:

=============
Error handing
=============




<h4>Example</h4>

<pre>
  switch(mps_alloc(&amp;(mps_addr_t)object, pool, size)) {
  case MPS_RES_LIMIT:
    bomb("The MPS has reached an internal limit");
    break;

    /* ... */

  }
</pre>



<h4>Example</h4>

<pre>
  switch( res = mps_pool_create_v(&amp;pool, arena, class, params) ) {
  case MPS_RES_PARAM:
    bomb("Can't make a pool with those specifications");
    break;

    /* ... */

   }
</pre>




<h4>Example</h4>

<pre>
mps_addr_t p;
mps_res_t res;

res = mps_alloc(&amp;p, pool, sizeof(struct spong));
if(res != MPS_RES_OK) {
  handle_memory_error(res);
  abort();
}
</pre>

For more examples, s ee doc.mps.ref-man.if-conv.
