.. _topic-message:

========
Messages
========




<h4>Example</h4>

<pre>
  mps_message_t message;
  mps_clock_t posted_at;

  if(mps_message_get(&amp;message, arena, mps_message_type_gc_start())) {
    posted_at = mps_message_clock(arena, message);
    printf("Collection started at %ul.\n", (unsigned long)posted_at);
  }
</pre>
