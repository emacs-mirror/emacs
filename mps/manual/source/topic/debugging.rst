.. _topic-debugging:

===============
Debugging pools
===============


<h4>Example</h4>

<pre>
static mps_pool_debug_option_s debugOptions = { (void *)"postpost", 8 };
if(mps_pool_create(&amp;pool, arena, mps_class_ams_debug(),
                   &amp;debugOptions, 8192, 135, 8)
   != MPS_RES_OK) {
  printf("Error creating pool!"); exit(2);
}
</pre>

