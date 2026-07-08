  /* Add module environment functions newly added in Emacs 32 here.
     Before Emacs 32 is released, remove this comment and start
     module-env-33.h on master (see admin/release-branch.txt).  */

  /* Get pointer to the pixel buffer of CANVAS.  The buffer is in row
     major order and has the size width * height.  The pixel format is
     ARGB32 on all platforms.  The pointer will be valid as long as
     CANVAS is alive, and as long as its dimensions have not been
     changed.  Return NULL in case of error.   */

  uint32_t *(*canvas_data) (emacs_env *env, emacs_value canvas)
    EMACS_ATTRIBUTE_NONNULL(1);
