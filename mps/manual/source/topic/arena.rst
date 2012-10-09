.. _topic-arena:

======
Arenas
======


<h4>Example</h4>

<pre>
mps_arena_t arena;

int main(void)
{
  void *block;
  mps_res_t res;

  block = malloc(ARENA_SIZE);
  if(block == NULL) {
    printf("Not enough memory!");
    exit(1);
  }

  res = mps_arena_create(&amp;arena, mps_arena_class_cl(), ARENA_SIZE, block);
  if(res != MPS_RES_OK) {
    printf("ARENA_SIZE too small");
    exit(2);
  }

  /* rest of program */
}
</pre>



<h4>Example</h4>

<pre>
mps_arena_t arena;

int main(void)
{
  mps_res_t res;

  res = mps_arena_create(&amp;arena, mps_arena_class_vm(), ARENA_SIZE);
  if(res != MPS_RES_OK) {
    printf("Not enough memory!");
    exit(1);
  }

  /* rest of program */

}
</pre>



<h4>Example</h4>

<pre>
mps_arena_t arena;

int main(void)
{
  mps_res_t res;

  res = mps_arena_create(&amp;arena, mps_arena_class_vmnz(), ARENA_SIZE);
  if(res != MPS_RES_OK) {
    printf("Not enough memory!");
    exit(1);
  }

  /* rest of program */

}
</pre>


<h4>Example</h4>

<pre>
do {
  res = mps_arena_commit_limit_set(arena, limit - 100 * 1024);
  if(res != MPS_RES_OK)
    flush_caches();
} while(res != MPS_RES_OK);
</pre>


<pre>
mps_arena_t arena;

int main(void)
{
  mps_res_t res;

  res = mps_arena_create(&amp;arena, mps_arena_class_vm(), ARENA_SIZE);
  if(res != MPS_ RES_OK) {
    printf("Not enough memory!");
    exit(1);
  }

  /* rest of program */
}
</pre>
