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



<h4>Example</h4>

<pre>
{
  mps_message_t message;
  if(mps_message_get(&amp;message, arena, mps_message_type_gc())) {
    size_t live, condemned, not_condemned;
    live = mps_message_gc_live_size(arena, message);
    condemned = mps_message_gc_condemned_size(arena, message);
    not_condemned = mps_message_gc_not_condemned_size(arena,message);
    mps_message_discard(arena, message);
    process_collection_stats(live, condemned, not_condemned);
  }
}
</pre>



<pre>
{
  mps_message_t message;
  if(mps_message_get(&amp;message, arena, mps_message_type_gc_start())) {
    printf("Collection started; reason: %s\n",
      mps_message_gc_start_why(arena, message));
  }
}
</pre>
