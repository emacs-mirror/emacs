  /* Returns whether a quit is pending.  */
  bool (*should_quit) (emacs_env *env)
    EMACS_ATTRIBUTE_NONNULL(1);
