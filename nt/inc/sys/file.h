/*
 * sys\file.h doesn't exist on NT...rather than including it conditionally
 * in some of the source files, we just extend the include path so that the
 * compiler will pick up this empty header instead.
 */
