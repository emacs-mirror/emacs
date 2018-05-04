#!/usr/bin/env python

# Module helper script.

# Copyright 2015-2018 Free Software Foundation, Inc.

# This file is part of GNU Emacs.

# GNU Emacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

import os
import string
import subprocess as sp
import argparse
import re

EMACS = os.path.join('..', 'src', 'emacs')

def find_modules():
    modpaths = []
    for (dirname, dirs, files) in os.walk('.'):
        if 'Makefile' in files:
            modpaths.append(dirname)
    return modpaths

def cmd_test(args):
    mods = args.module
    if not mods:
        mods = find_modules()

    make_cmd = ['make']
    if args.force:
        make_cmd.append('-B')

    failed = []
    for m in mods:
        print('[*] %s: ------- start -------' % m)
        print('[*] %s: running make' % m)
        r = sp.call(make_cmd, cwd=m)
        if r != 0:
            print('[E] %s: make failed' % m)
            failed += [m]
            continue

        print('[*] %s: running test' % m)
        testpath = os.path.join(m, 'test.el')
        if os.path.isfile(testpath):
            emacs_cmd = [EMACS, '-batch', '-L', '.', '-l', 'ert',
                         '-l', testpath, '-f', 'ert-run-tests-batch-and-exit']
            print(' '.join(emacs_cmd))
            r = sp.call(emacs_cmd)
            if r != 0:
                print('[E] %s: test failed' % m)
                failed += [m]
                continue
        else:
            print('[W] %s: no test to run' % m)

    print('\n[*] %d/%d MODULES OK' % (len(mods)-len(failed), len(mods)))
    for m in failed:
        print('\tfailed: %s' % m)

def to_lisp_sym(sym):
    sym = re.sub('[_ ]', '-', sym)
    return sym

def to_c_sym(sym):
    sym = re.sub('[- ]', '_', sym)
    return sym

def cmd_init(args):
    if os.path.exists(args.module):
        print("%s: file/dir '%s' already exists" % (__file__, args.module))
        return

    os.mkdir(args.module)

    template_vars = {
        'module': args.module,
        'func': args.fun,
        'c_file': '%s.c' % args.module,
        'c_func': 'F%s_%s' % (to_c_sym(args.module), to_c_sym(args.fun)),
        'lisp_func': '%s-%s' % (args.module,  to_lisp_sym(args.fun)),
    }

    for path, t in TEMPLATES.items():
        if isinstance(path, string.Template):
            path = path.substitute(template_vars)
        path = os.path.join(args.module, path)
        print("writing %s..." % path)
        with open(path, "w+") as f:
            f.write(t.substitute(template_vars))
    print("done! you can run %s test %s" % (__file__, args.module))


def main():
    # path always written relative to this file
    os.chdir(os.path.dirname(os.path.realpath(__file__)))

    mainp = argparse.ArgumentParser()
    subp = mainp.add_subparsers()

    testp = subp.add_parser('test', help='run tests')
    testp.add_argument('-f', '--force', action='store_true',
                       help='force regeneration (make -B)')
    testp.add_argument('module', nargs='*',
                       help='path to module to test (default all)')
    testp.set_defaults(func=cmd_test)

    initp = subp.add_parser('init', help='create a test module from a template')
    initp.add_argument('module', help='name of the new module')
    initp.add_argument('-f', '--fun', default='fun',
                       help='override name of the default function')
    initp.set_defaults(func=cmd_init)

    args = mainp.parse_args()
    args.func(args)


# double the $ to escape python template syntax
TEMPLATES = {
    'Makefile': string.Template('''
ROOT = ../..

CC      = gcc
LD      = gcc
CFLAGS  = -ggdb3 -Wall
LDFLAGS =

all: ${module}.so ${module}.doc

%.so: %.o
	$$(LD) -shared $$(LDFLAGS) -o $$@ $$<

%.o: %.c
	$$(CC) $$(CFLAGS) -I$$(ROOT)/src -fPIC -c $$<

'''),

    string.Template('${c_file}'): string.Template('''
#include <emacs-module.h>

int plugin_is_GPL_compatible;

static emacs_value
${c_func} (emacs_env *env, ptrdiff_t nargs, emacs_value args[], void *data)
{
  return env->intern (env, "t");
}

/* Bind NAME to FUN.  */
static void
bind_function (emacs_env *env, const char *name, emacs_value Sfun)
{
  emacs_value Qfset = env->intern (env, "fset");
  emacs_value Qsym = env->intern (env, name);
  emacs_value args[] = { Qsym, Sfun };

  env->funcall (env, Qfset, 2, args);
}

/* Provide FEATURE to Emacs.  */
static void
provide (emacs_env *env, const char *feature)
{
  emacs_value Qfeat = env->intern (env, feature);
  emacs_value Qprovide = env->intern (env, "provide");
  emacs_value args[] = { Qfeat };

  env->funcall (env, Qprovide, 1, args);
}

int
emacs_module_init (struct emacs_runtime *ert)
{
  emacs_env *env = ert->get_environment (ert);
  bind_function (env, "${lisp_func}",
                 env->make_function (env, 1, 1, ${c_func}, "doc", NULL));
  provide (env, "${module}");
  return 0;
}
'''),
    'test.el': string.Template('''
(require 'ert)
(require 'module-test-common)

;; #$$ works when loading, buffer-file-name when evaluating from emacs
(module-load (module-path (or #$$ (expand-file-name (buffer-file-name)))))

(ert-deftest ${lisp_func}-test ()
  (should (eq (${lisp_func} 42) t)))
''')
}

if __name__ == '__main__':
    main()
