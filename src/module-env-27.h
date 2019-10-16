  /* Processes pending input events and returns whether the module
     function should quit.  */
  enum emacs_process_input_result (*process_input) (emacs_env *env)
    EMACS_ATTRIBUTE_NONNULL (1);

  struct timespec (*extract_time) (emacs_env *env, emacs_value value)
    EMACS_ATTRIBUTE_NONNULL (1);

  emacs_value (*make_time) (emacs_env *env, struct timespec time)
    EMACS_ATTRIBUTE_NONNULL (1);

  void (*extract_big_integer) (emacs_env *env, emacs_value value,
                               struct emacs_mpz *result)
    EMACS_ATTRIBUTE_NONNULL (1, 3);

  emacs_value (*make_big_integer) (emacs_env *env,
                                   const struct emacs_mpz *value)
    EMACS_ATTRIBUTE_NONNULL (1, 2);
