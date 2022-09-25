/* Detect the number of processors.

   Copyright (C) 2009-2022 Free Software Foundation, Inc.

   This file is free software: you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as
   published by the Free Software Foundation; either version 2.1 of the
   License, or (at your option) any later version.

   This file is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* Written by Glen Lenker and Bruno Haible.  */

/* Allow the use in C++ code.  */
#ifdef __cplusplus
extern "C" {
#endif

/* A "processor" in this context means a thread execution unit, that is either
   - an execution core in a (possibly multi-core) chip, in a (possibly multi-
     chip) module, in a single computer, or
   - a thread execution unit inside a core
     (hyper-threading, see <https://en.wikipedia.org/wiki/Hyper-threading>).
   Which of the two definitions is used, is unspecified.  */

enum nproc_query
{
  NPROC_ALL,                 /* total number of processors */
  NPROC_CURRENT,             /* processors available to the current process */
  NPROC_CURRENT_OVERRIDABLE  /* likewise, but overridable through the
                                OMP_NUM_THREADS environment variable */
};

/* Return the total number of processors.  The result is guaranteed to
   be at least 1.  */
extern unsigned long int num_processors (enum nproc_query query);

#ifdef __cplusplus
}
#endif /* C++ */
