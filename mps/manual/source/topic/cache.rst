.. _topic-cache:

Allocation caches
=================



<pre>
  void *p;
  Foo *foo;

  res = mps_sac_alloc(&amp;p, sac, FooSIZE, is_in_panic);
  if (res != MPS_RES_OK) {
    printf("Failed to alloc foo!\n");
    exit(1);
  }
  foo = p;

  /* use foo */

  mps_sac_free(sac, p, FooSIZE);
</pre>


What does this mean? (from mps_sac_alloc):

    The client is responsible for synchronising the access to the
    cache, but if the cache decides to access the pool, the MPS will
    properly synchronize with any other threads that might be
    accessing the same pool.


<pre>
  void *p;
  Foo *foo;
  mps_res_t res;

  MPS_SAC_ALLOC_FAST(res, p, sac, FooSIZE, is_in_panic);
  if (res != MPS_RES_OK) {
    printf("Failed to alloc foo!\n");
    exit(1);
  }
  foo = p;

  /* use foo */

  MPS_SAC_FREE_FAST(sac, p, FooSIZE);
</pre>



<h4>Example</h4>

<pre>
  mps_sac_t sac;
  mps_sac_class_s classes[3] = { {8, 38, 1}, {136, 19, 3}, {512, 4, 1} };

#if (MPS_SAC_CLASS_LIMIT &lt; 3)
#  error "Too many classes!"
#endif

  res = mps_sac_create(&amp;sac, pool, 3, classes);
  if (res != MPS_RES_OK) {
    printf("Failed to create the allocation cache!");
    exit(1);
  }
</pre>




<h4>Example</h4>

<pre>
  void *p;
  Foo *foo;

  res = mps_sac_alloc(&amp;p, sac, FooSIZE, is_in_panic);
  if (res != MPS_RES_OK) {
    printf("Failed to alloc foo!\n");
    exit(1);
  }
  foo = p;

  /* use foo */

  mps_sac_free(sac, p, FooSIZE);
</pre>



<h4>Example</h4>

<pre>
  void *p;
  Foo *foo;
  mps_res_t res;

  MPS_SAC_ALLOC_FAST(res, p, sac, FooSIZE, is_in_panic);
  if (res != MPS_RES_OK) {
    printf("Failed to alloc foo!\n");
    exit(1);
  }
  foo = p;

  /* use foo */

  MPS_SAC_FREE_FAST(sac, p, FooSIZE);
</pre>
