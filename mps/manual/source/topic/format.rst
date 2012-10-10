.. _topic-format:

==============
Object formats
==============


<h4>Example</h4>

<pre>
mps_fmt_t create_format(mps_arena_t arena)
{
  mps_fmt my_format;
  mps_res_t res;
  mps_fmt_A_s my_format_A = { my_alignment, &amp;my_scan, &amp;my_skip, &amp;my_copy, &amp;my_fwd,
                              &amp;my_isfwd, &amp;my_pad };

  res = mps_fmt_create_A(&amp;my_format, arena, &amp;my_format_A);
  assert(res != MPS_RES_OK);

  return my_format;
}
</pre>




<h4>Example</h4>

<pre>
mps_fmt_t create_format(mps_arena_t arena)
{
  mps_fmt_B_s my_format_B = { my_alignment, &amp;my_scan, &amp;my_skip, &amp;my_copy,
                              &amp;my_fwd, &amp;my_isfwd, &amp;my_pad, &amp;my_class };
  mps_fmt my_format;
  mps_res_t res;

  res = mps_fmt_create_B(&amp;my_format, arena, &amp;my_format_B);
  assert(res != MPS_RES_OK);

  return my_format;
}
</pre>



<h4>Example</h4>

<pre>
mps_fmt_t create_format(mps_arena_t arena)
{
  mps_fmt format;
  mps_res_t res;
  mps_fmt_auto_header_s format_desc = { my_alignment, &amp;my_scan, &amp;my_skip, &amp;my_fwd,
                                        &amp;my_isfwd, &amp;my_pad, HEADER_SIZE };

  res = mps_fmt_create_auto_header(&amp;format, arena, &amp;format_desc);
  assert(res != MPS_RES_OK);

  return format;
}
</pre>




<h4>Example</h4>

<pre>
mps_addr_t my_class_method(mps_addr_t object) {
  my_object_generic_t generic_object = object;
  return (mps_addr_t)(generic_object.class);
}
</pre>




<h4>Example</h4>

<pre>
mps_fmt_t create_format(mps_arena_t arena)
{
  mps_fmt_A_s my_format_A = { my_alignment, &amp;my_scan, &amp;my_skip, &amp;my_copy,&amp;my_fwd,
    &amp;my_isfwd, &amp;my_pad };
  mps_fmt my_format;
  mps_res_t res;

  res = mps_fmt_create_A(&amp;my_format, arena, &amp;my_format_A);
  if(res != MPS_RES_OK) {
    fprintf(stderr, "Couldn't create format.\n");
    exit(1);
  }

  return my_format;
}
</pre>



<h4>Example</h4>

<pre>
mps_fmt_t create_format(mps_arena_t arena)
{
  mps_fmt_B_s my_format_B = { my_alignment, &amp;my_scan, &amp;my_skip, &amp;my_copy,
                              &amp;my_fwd, &amp;my_isfwd, &amp;my_pad, &amp;my_class };
  mps_fmt my_format;
  mps_res_t res;

  res = mps_fmt_create_B(&amp;my_format, arena, &amp;my_format_B);
  assert(res != MPS_RES_OK);

  return my_format;
}
</pre>


 

<h4>Example</h4>

<pre>
mps_fmt_t create_format(mps_arena_t arena)
{
  mps_fmt_auto_header_s format_desc = { my_alignment, &amp;my_scan, &amp;my_skip, &amp;my_fwd,
    &amp;my_isfwd, &amp;my_pad, HEADER_SIZE };
  mps_fmt format;
  mps_res_t res;

  res = mps_fmt_create_auto_header(&amp;format, arena, &amp;format_desc);
  assert(res != MPS_RES_OK);

  return format;
}
</pre>





<h4>Example</h4>

<pre>
/* define the function */

void example_fwd(mps_addr_t old, mps_addr_t new)
{
  /* ... */
}

/* also define example_scan, example_skip, etc */
/* store pointer to function in the format variant struct */
struct mps_fmt_B_s example_fmt_B = {
  4, /* align */
  example_scan,
  example_skip,
  example_copy,
  example_fwd,
  example_isfwd,
  example_pad,
  example_class
};

/* The (address of the) example_fmt_B object can now be passed to */
/* mps_fmt_create_B to create a format. */
</pre>




<h4>Example</h4>

<pre>
mps_addr_t my_skip_method(mps_addr_t object)
{
  char *p = (char *)object;
  my_object_t my_object = (my_object_t)object;
  return((mps_addr_t)(p + my_object-&gt;length));
}
</pre>




<h4>Example</h4>

<pre>
#include "mps.h"
#include "mpscamc.h"
#include &lt;stdlib.h&gt;

struct mps_fmt_A_s fmt_A_s = {
  (mps_align_t)4,
  scan, skip, copy, move, isMoved, pad
};

void go(mps_space_t space)
{
  mps_fmt_t format;
  mps_res_t res;
  mps_pool_t pool;

  res = mps_fmt_create_A(&amp;format, space, &amp;mps_fmt_A_s);
  if(res != MPS_RES_OK)
    abort();

  res = mps_pool_create(&amp;pool, space, mps_class_amc(), format);
  if(res != MPS_RES_OK)
    abort();

  /* do some stuff here */

  mps_pool_destroy(pool);
  mps_format_destroy(format);
}
</pre>
