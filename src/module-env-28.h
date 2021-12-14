  void (*(*EMACS_ATTRIBUTE_NONNULL (1)
            get_function_finalizer) (emacs_env *env,
                                     emacs_value arg)) (void *) EMACS_NOEXCEPT;

  void (*set_function_finalizer) (emacs_env *env, emacs_value arg,
                                  void (*fin) (void *) EMACS_NOEXCEPT)
    EMACS_ATTRIBUTE_NONNULL (1);

  int (*open_channel) (emacs_env *env, emacs_value pipe_process)
    EMACS_ATTRIBUTE_NONNULL (1);

  void (*make_interactive) (emacs_env *env, emacs_value function,
                            emacs_value spec)
    EMACS_ATTRIBUTE_NONNULL (1);

  /* Create a unibyte Lisp string from a string.  */
  emacs_value (*make_unibyte_string) (emacs_env *env,
				      const char *str, ptrdiff_t len)
    EMACS_ATTRIBUTE_NONNULL(1, 2);
