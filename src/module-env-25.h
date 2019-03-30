  /* Structure size (for version checking).  */
  ptrdiff_t size;

  /* Private data; users should not touch this.  */
  struct emacs_env_private *private_members;

  /* Memory management.  */

  emacs_value (*make_global_ref) (emacs_env *env,
				  emacs_value any_reference)
    EMACS_ATTRIBUTE_NONNULL(1);

  void (*free_global_ref) (emacs_env *env,
			   emacs_value global_reference)
    EMACS_ATTRIBUTE_NONNULL(1);

  /* Non-local exit handling.  */

  enum emacs_funcall_exit (*non_local_exit_check) (emacs_env *env)
    EMACS_ATTRIBUTE_NONNULL(1);

  void (*non_local_exit_clear) (emacs_env *env)
    EMACS_ATTRIBUTE_NONNULL(1);

  enum emacs_funcall_exit (*non_local_exit_get)
    (emacs_env *env,
     emacs_value *non_local_exit_symbol_out,
     emacs_value *non_local_exit_data_out)
    EMACS_ATTRIBUTE_NONNULL(1, 2, 3);

  void (*non_local_exit_signal) (emacs_env *env,
				 emacs_value non_local_exit_symbol,
				 emacs_value non_local_exit_data)
    EMACS_ATTRIBUTE_NONNULL(1);

  void (*non_local_exit_throw) (emacs_env *env,
				emacs_value tag,
				emacs_value value)
    EMACS_ATTRIBUTE_NONNULL(1);

  /* Function registration.  */

  emacs_value (*make_function) (emacs_env *env,
				ptrdiff_t min_arity,
				ptrdiff_t max_arity,
				emacs_value (*function) (emacs_env *env,
							 ptrdiff_t nargs,
							 emacs_value args[],
							 void *)
				  EMACS_NOEXCEPT
                                  EMACS_ATTRIBUTE_NONNULL(1),
				const char *documentation,
				void *data)
    EMACS_ATTRIBUTE_NONNULL(1, 4);

  emacs_value (*funcall) (emacs_env *env,
                          emacs_value function,
                          ptrdiff_t nargs,
                          emacs_value args[])
    EMACS_ATTRIBUTE_NONNULL(1);

  emacs_value (*intern) (emacs_env *env,
                         const char *symbol_name)
    EMACS_ATTRIBUTE_NONNULL(1, 2);

  /* Type conversion.  */

  emacs_value (*type_of) (emacs_env *env,
			  emacs_value value)
    EMACS_ATTRIBUTE_NONNULL(1);

  bool (*is_not_nil) (emacs_env *env, emacs_value value)
    EMACS_ATTRIBUTE_NONNULL(1);

  bool (*eq) (emacs_env *env, emacs_value a, emacs_value b)
    EMACS_ATTRIBUTE_NONNULL(1);

  intmax_t (*extract_integer) (emacs_env *env, emacs_value value)
    EMACS_ATTRIBUTE_NONNULL(1);

  emacs_value (*make_integer) (emacs_env *env, intmax_t value)
    EMACS_ATTRIBUTE_NONNULL(1);

  double (*extract_float) (emacs_env *env, emacs_value value)
    EMACS_ATTRIBUTE_NONNULL(1);

  emacs_value (*make_float) (emacs_env *env, double value)
    EMACS_ATTRIBUTE_NONNULL(1);

  /* Copy the content of the Lisp string VALUE to BUFFER as an utf8
     NUL-terminated string.

     SIZE must point to the total size of the buffer.  If BUFFER is
     NULL or if SIZE is not big enough, write the required buffer size
     to SIZE and return true.

     Note that SIZE must include the last NUL byte (e.g. "abc" needs
     a buffer of size 4).

     Return true if the string was successfully copied.  */

  bool (*copy_string_contents) (emacs_env *env,
                                emacs_value value,
                                char *buffer,
                                ptrdiff_t *size_inout)
    EMACS_ATTRIBUTE_NONNULL(1, 4);

  /* Create a Lisp string from a utf8 encoded string.  */
  emacs_value (*make_string) (emacs_env *env,
			      const char *contents, ptrdiff_t length)
    EMACS_ATTRIBUTE_NONNULL(1, 2);

  /* Embedded pointer type.  */
  emacs_value (*make_user_ptr) (emacs_env *env,
				void (*fin) (void *) EMACS_NOEXCEPT,
				void *ptr)
    EMACS_ATTRIBUTE_NONNULL(1);

  void *(*get_user_ptr) (emacs_env *env, emacs_value uptr)
    EMACS_ATTRIBUTE_NONNULL(1);
  void (*set_user_ptr) (emacs_env *env, emacs_value uptr, void *ptr)
    EMACS_ATTRIBUTE_NONNULL(1);

  void (*(*get_user_finalizer) (emacs_env *env, emacs_value uptr))
    (void *) EMACS_NOEXCEPT EMACS_ATTRIBUTE_NONNULL(1);
  void (*set_user_finalizer) (emacs_env *env,
			      emacs_value uptr,
			      void (*fin) (void *) EMACS_NOEXCEPT)
    EMACS_ATTRIBUTE_NONNULL(1);

  /* Vector functions.  */
  emacs_value (*vec_get) (emacs_env *env, emacs_value vec, ptrdiff_t i)
    EMACS_ATTRIBUTE_NONNULL(1);

  void (*vec_set) (emacs_env *env, emacs_value vec, ptrdiff_t i,
		   emacs_value val)
    EMACS_ATTRIBUTE_NONNULL(1);

  ptrdiff_t (*vec_size) (emacs_env *env, emacs_value vec)
    EMACS_ATTRIBUTE_NONNULL(1);
