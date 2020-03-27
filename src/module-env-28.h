  /* Add module environment functions newly added in Emacs 28 here.
     Before Emacs 28 is released, remove this comment and start
     module-env-29.h on the master branch.  */

  void (*(*EMACS_ATTRIBUTE_NONNULL (1)
            get_function_finalizer) (emacs_env *env,
                                     emacs_value arg)) (void *) EMACS_NOEXCEPT;

  void (*set_function_finalizer) (emacs_env *env, emacs_value arg,
                                  void (*fin) (void *) EMACS_NOEXCEPT)
    EMACS_ATTRIBUTE_NONNULL (1);

  int (*open_channel) (emacs_env *env, emacs_value pipe_process)
    EMACS_ATTRIBUTE_NONNULL (1);
