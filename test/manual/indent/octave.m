## -*- mode: octave; coding: utf-8 -*-
0;				# Don't make this a function file
function res = tcomp (fn)

  global x y ...
         z1 z2
  persistent x y ...
             z1 z2
  global x y = 2 ...
         z1 z2                  # FIXME

  do
    something
  until x = ...
        y

  spmd                          #bug#36703
    something
  end

  %% res = tcomp (fn)
  %%     imports components and rearranges them.

  if nargin ~= 1
    print_usage()
  end

  data = dlmread(fn, 3, 0);

  enumeration
    first (1)
    second (2)
  end

  y = enumeration (x);          #Beware: "enumeration" can also be a function!
  y = foo(enumeration (x),
          2);          #Beware: "enumeration" can also be a function!

  x = data(:,2:end);
  y = 'hello';
  z = y';

  ## Bug#14399.
  vec = [...
          one;...
          two;...
          three];

  cnty = repmat(x(:,1)(:), 10, 1);
  x = ...
  12

  pop = x(:,1:10)(:);
    ## Here and below, we test if the indentation aligns with a previous
    ## fixindented line.  This is important so as to make it easier for the
    ## user to override some indentation somewhere, and also because it
    ## reflects the fact that the indentation decision is taken with a minimum
    ## amount of work (i.e. in the present case, without having to walk back
    ## until the `function' line).
    bir = x(:,11:20)(:);        # fixindent
    dth = x(:,21:30)(:);
    imig = x(:,31:40)(:);
    dmig = x(:,41:50)(:);
    gq = x(:,51:60)(:);

    yrs = repmat(2000:2009, 39, 1)(:);

    res = [yrs, cnty, pop, bir, dth, imig, dmig, gq];

endfunction

## Copyright (C) 2005, 2006, 2007, 2008, 2009 Søren Hauberg
##
## This file is part of Octave.
##
## Octave is free software; you can redistribute it and/or modify it
## under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3 of the License, or (at
## your option) any later version.
##
## Octave is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Octave; see the file COPYING.  If not, see
## <https://www.gnu.org/licenses/>.

## -*- texinfo -*-
## @deftypefn  {Command} pkg @var{command} @var{pkg_name}
## @deftypefnx {Command} pkg @var{command} @var{option} @var{pkg_name}
## This command interacts with the package manager.  Different actions will
## be taken depending on the value of @var{command}.
##
## @table @samp
## @item install
## Install named packages.  For example,
## @example
## pkg install image-1.0.0.tar.gz
## @end example
## @noindent
## installs the package found in the file @file{image-1.0.0.tar.gz}.
##
## The @var{option} variable can contain options that affect the manner
## in which a package is installed.  These options can be one or more of
##
## @table @code
## @item -nodeps
## The package manager will disable the dependency checking.  That way it
## is possible to install a package even if it depends on another package
## that's not installed on the system.  @strong{Use this option with care.}
##
## @item -noauto
## The package manager will not automatically load the installed package
## when starting Octave, even if the package requests that it is.
##
## @item -auto
## The package manager will automatically load the installed package when
## starting Octave, even if the package requests that it isn't.
##
## @item -local
## A local installation is forced, even if the user has system privileges.
##
## @item -global
## A global installation is forced, even if the user doesn't normally have
## system privileges
##
## @item -verbose
## The package manager will print the output of all of the commands that are
## performed.
## @end table
##
## @item uninstall
## Uninstall named packages.  For example,
## @example
## pkg uninstall image
## @end example
## @noindent
## removes the @code{image} package from the system.  If another installed
## package depends on the @code{image} package an error will be issued.
## The package can be uninstalled anyway by using the @code{-nodeps} option.
## @item load
## Add named packages to the path.  After loading a package it is
## possible to use the functions provided by the package.  For example,
## @example
## pkg load image
## @end example
## @noindent
## adds the @code{image} package to the path.  It is possible to load all
## installed packages at once with the command
## @example
## pkg load all
## @end example
## @item unload
## Removes named packages from the path.  After unloading a package it is
## no longer possible to use the functions provided by the package.
## This command behaves like the @code{load} command.
## @item list
## Show a list of the currently installed packages.  By requesting one or two
## output argument it is possible to get a list of the currently installed
## packages.  For example,
## @example
## installed_packages = pkg list;
## @end example
## @noindent
## returns a cell array containing a structure for each installed package.
## The command
## @example
## [@var{user_packages}, @var{system_packages}] = pkg list
## @end example
## @noindent
## splits the list of installed packages into those who are installed by
## the current user, and those installed by the system administrator.
## @item describe
## Show a short description of the named installed packages, with the option
## '-verbose' also list functions provided by the package, e.g.:
## @example
##  pkg describe -verbose all
## @end example
## @noindent
## will describe all installed packages and the functions they provide.
## If one output is requested a cell of structure containing the
## description and list of functions of each package is returned as
## output rather than printed on screen:
## @example
##  desc = pkg ("describe", "secs1d", "image")
## @end example
## @noindent
## If any of the requested packages is not installed, pkg returns an
## error, unless a second output is requested:
## @example
##  [ desc, flag] = pkg ("describe", "secs1d", "image")
## @end example
## @noindent
## @var{flag} will take one of the values "Not installed", "Loaded" or
## "Not loaded" for each of the named packages.
## @item prefix
## Set the installation prefix directory.  For example,
## @example
## pkg prefix ~/my_octave_packages
## @end example
## @noindent
## sets the installation prefix to @file{~/my_octave_packages}.
## Packages will be installed in this directory.
##
## It is possible to get the current installation prefix by requesting an
## output argument.  For example,
## @example
## p = pkg prefix
## @end example
##
## The location in which to install the architecture dependent files can be
## independent specified with an addition argument.  For example
##
## @example
## pkg prefix ~/my_octave_packages ~/my_arch_dep_pkgs
## @end example
## @item local_list
## Set the file in which to look for information on the locally
## installed packages.  Locally installed packages are those that are
## typically available only to the current user.  For example
## @example
## pkg local_list ~/.octave_packages
## @end example
## It is possible to get the current value of local_list with the following
## @example
## pkg local_list
## @end example
## @item global_list
## Set the file in which to look for, for information on the globally
## installed packages.  Globally installed packages are those that are
## typically available to all users.  For example
## @example
## pkg global_list /usr/share/octave/octave_packages
## @end example
## It is possible to get the current value of global_list with the following
## @example
## pkg global_list
## @end example
## @item rebuild
## Rebuilds the package database from the installed directories.  This can
## be used in cases where for some reason the package database is corrupted.
## It can also take the @code{-auto} and @code{-noauto} options to allow the
## autoloading state of a package to be changed.  For example
##
## @example
## pkg rebuild -noauto image
## @end example
##
## will remove the autoloading status of the image package.
## @item build
## Builds a binary form of a package or packages.  The binary file produced
## will itself be an Octave package that can be installed normally with
## @code{pkg}.  The form of the command to build a binary package is
##
## @example
## pkg build builddir image-1.0.0.tar.gz @dots{}
## @end example
##
## @noindent
## where @code{builddir} is the name of a directory where the temporary
## installation will be produced and the binary packages will be found.
## The options @code{-verbose} and @code{-nodeps} are respected, while
## the other options are ignored.
## @end table
## @end deftypefn

function [local_packages, global_packages] = pkg (varargin)
  ## Installation prefix (FIXME: what should these be on windows?)
  persistent user_prefix = false;
  persistent prefix = -1;
  persistent archprefix = -1;
  persistent local_list = tilde_expand (fullfile ("~", ".octave_packages"));
  persistent global_list = fullfile (OCTAVE_HOME (), "share", "octave",
				     "octave_packages");
  mlock ();

  global_install = issuperuser ();

  if (prefix == -1)
    if (global_install)
      prefix = fullfile (OCTAVE_HOME (), "share", "octave", "packages");
      archprefix = fullfile (octave_config_info ("libexecdir"),
			     "octave", "packages");
    else
      prefix = fullfile ("~", "octave");
      archprefix = prefix;
    endif
    prefix = tilde_expand (prefix);
    archprefix = tilde_expand (archprefix);
  endif

  available_actions = {"list", "install", "uninstall", "load", ...
		       "unload", "prefix", "local_list", ...
		       "global_list", "rebuild", "build","describe"};
  ## Handle input
  if (length (varargin) == 0 || ! iscellstr (varargin))
    print_usage ();
  endif
  files = {};
  deps = true;
  auto = 0;
  action = "none";
  verbose = false;
  for i = 1:length (varargin)
    switch (varargin{i})
      case "-nodeps"
	deps = false;
      case "-noauto"
	auto = -1;
      case "-auto"
	auto = 1;
      case "-verbose"
	verbose = true;
      case "-local"
	global_install = false;
	if (! user_prefix)
	  prefix = tilde_expand (fullfile ("~", "octave"));
	  archprefix = prefix;
	endif
      case "-global"
	global_install = true;
	if (! user_prefix)
	  prefix = fullfile (OCTAVE_HOME (), "share", "octave", "packages");
	  archprefix = fullfile (octave_config_info ("libexecdir"),
				 "octave", "packages");
	endif
      case available_actions
	if (strcmp (action, "none"))
	  action = varargin{i};
	else
	  error ("more than one action specified");
	endif
      otherwise
	files{end+1} = varargin{i};
    endswitch
  endfor

  ## Take action
  switch (action)
    case "list"
      if (nargout == 0)
	installed_packages (local_list, global_list);
      elseif (nargout == 1)
	local_packages = installed_packages (local_list, global_list);
      elseif (nargout == 2)
	[local_packages, global_packages] = installed_packages (local_list,
								global_list);
      else
	error ("too many output arguments requested");
      endif

    case "install"
      if (length (files) == 0)
	error ("you must specify at least one filename when calling 'pkg install'");
      endif
      install (files, deps, auto, prefix, archprefix, verbose, local_list,
	       global_list, global_install);

    case "uninstall"
      if (length (files) == 0)
	error ("you must specify at least one package when calling 'pkg uninstall'");
      endif
      uninstall (files, deps, verbose, local_list,
		 global_list, global_install);

    case "load"
      if (length (files) == 0)
	error ("you must specify at least one package, 'all' or 'auto' when calling 'pkg load'");
      endif
      load_packages (files, deps, local_list, global_list);

    case "unload"
      if (length (files) == 0)
	error ("you must specify at least one package or 'all' when calling 'pkg unload'");
      endif
      unload_packages (files, deps, local_list, global_list);

    case "prefix"
      if (length (files) == 0 && nargout == 0)
	printf ("Installation prefix:             %s\n", prefix);
	printf ("Architecture dependent prefix:   %s\n", archprefix);
      elseif (length (files) == 0 && nargout >= 1)
	local_packages = prefix;
	global_packages = archprefix;
      elseif (length (files) >= 1 && nargout <= 2 && ischar (files{1}))
	prefix = files{1};
	prefix = absolute_pathname (prefix);
	local_packages = prefix;
	user_prefix = true;
	if (length (files) >= 2 && ischar (files{2}))
	  archprefix = files{2};
	  try
	    archprefix = absolute_pathname (archprefix);
          catch
	    mkdir (archprefix);
	    warning ("creating the directory %s\n", archprefix);
	    archprefix = absolute_pathname (archprefix);
	  end_try_catch
	  global_packages = archprefix;
	endif
      else
	error ("you must specify a prefix directory, or request an output argument");
      endif

    case "local_list"
      if (length (files) == 0 && nargout == 0)
	disp (local_list);
      elseif (length (files) == 0 && nargout == 1)
	local_packages = local_list;
      elseif (length (files) == 1 && nargout == 0 && ischar (files{1}))
	try
	  local_list = absolute_pathname (files{1});
	catch
	  ## Force file to be created
	  fclose (fopen (files{1}, "wt"));
	  local_list = absolute_pathname (files{1});
	end_try_catch
      else
	error ("you must specify a local_list file, or request an output argument");
      endif

    case "global_list"
      if (length (files) == 0 && nargout == 0)
	disp(global_list);
      elseif (length (files) == 0 && nargout == 1)
	local_packages = global_list;
      elseif (length (files) == 1 && nargout == 0 && ischar (files{1}))
	try
	  global_list = absolute_pathname (files{1});
	catch
	  ## Force file to be created
	  fclose (fopen (files{1}, "wt"));
	  global_list = absolute_pathname (files{1});
	end_try_catch
      else
	error ("you must specify a global_list file, or request an output argument");
      endif

    case "rebuild"
      if (global_install)
	global_packages = rebuild (prefix, archprefix, global_list, files,
				   auto, verbose);
	global_packages = save_order (global_packages);
	save (global_list, "global_packages");
	if (nargout > 0)
	  local_packages = global_packages;
	endif
      else
	local_packages = rebuild (prefix, archprefix, local_list, files, auto,
				  verbose);
	local_packages = save_order (local_packages);
	save (local_list, "local_packages");
	if (nargout == 0)
	  clear ("local_packages");
	endif
      endif

    case "build"
      if (length (files) < 2)
	error ("you must specify at least the build directory and one filename\nwhen calling 'pkg build'");
      endif
      build (files, deps, auto, verbose);

    case "describe"
      if (length (files) == 0)
	error ("you must specify at least one package or 'all' when calling 'pkg describe'");
      endif
      ## FIXME: the name of the output variables is inconsistent
      ##            with their content
      switch (nargout)
	case 0
	  describe (files, verbose, local_list, global_list);
	case 1
	  pkg_desc_list = describe (files, verbose, local_list, ...
				    global_list);
	  local_packages = pkg_desc_list;
	case 2
	  [pkg_desc_list, flag] = describe (files, verbose, local_list, ...
					    global_list);
	  local_packages  = pkg_desc_list;
	  global_packages = flag;
	otherwise
	  error ("you can request at most two outputs when calling 'pkg describe'");
      endswitch

    otherwise
      error ("you must specify a valid action for 'pkg'. See 'help pkg' for details");
  endswitch
endfunction

function descriptions = rebuild (prefix, archprefix, list, files, auto, verbose)
  if (isempty (files))
    [dirlist, err, msg] = readdir (prefix);
    if (err)
      error ("couldn't read directory %s: %s", prefix, msg);
    endif
    ## the two first entries of dirlist are "." and ".."
    dirlist([1,2]) = [];
  else
    old_descriptions = installed_packages (list, list);
    wd = pwd ();
    unwind_protect
      cd (prefix);
      dirlist = glob (cellfun(@(x) cstrcat(x, '-*'), files, 'UniformOutput', 0));
    unwind_protect_cleanup
      cd (wd);
    end_unwind_protect
  endif
  descriptions = {};
  for k = 1:length (dirlist)
    descfile = fullfile (prefix, dirlist{k}, "packinfo", "DESCRIPTION");
    if (verbose)
      printf ("recreating package description from %s\n", dirlist{k});
    endif
    if (exist (descfile, "file"))
      desc = get_description (descfile);
      desc.dir = fullfile (prefix, dirlist{k});
      desc.archprefix = fullfile (archprefix, cstrcat (desc.name, "-",
						       desc.version));
      if (auto != 0)
	if (exist (fullfile (desc.dir, "packinfo", ".autoload"), "file"))
	  unlink (fullfile (desc.dir, "packinfo", ".autoload"));
	endif
        if (auto < 0)
	  desc.autoload = 0;
	elseif (auto > 0)
	  desc.autoload = 1;
	  fclose (fopen (fullfile (desc.dir, "packinfo", ".autoload"), "wt"));
	endif
      else
	if (exist (fullfile (desc.dir, "packinfo", ".autoload"), "file"))
	  desc.autoload = 1;
	else
	  desc.autoload = 0;
	endif
      endif
      descriptions{end + 1} = desc;
    elseif (verbose)
      warning ("directory %s is not a valid package", dirlist{k});
    endif
  endfor

  if (! isempty (files))
    ## We are rebuilding for a particular package(s) so we should take
    ## care to keep the other untouched packages in the descriptions
    descriptions = {descriptions{:}, old_descriptions{:}};

    dup = [];
    for i = 1:length (descriptions)
      if (find (dup, i))
	continue;
      endif
      for j = (i+1):length (descriptions)
	if (find (dup, j))
	  continue;
	endif
	if (strcmp (descriptions{i}.name, descriptions{j}.name))
	  dup = [dup, j];
	endif
      endfor
    endfor
    if (! isempty (dup))
      descriptions (dup) = [];
    endif
  endif
endfunction

function build (files, handle_deps, autoload, verbose)
  if (length (files) < 1)
    error ("insufficient number of files");
  endif
  builddir = files{1};
  if (! exist (builddir, "dir"))
    warning ("creating build directory %s", builddir);
    [status, msg] = mkdir (builddir);
    if (status != 1)
      error ("could not create installation directory: %s", msg);
    endif
  endif
  builddir = absolute_pathname (builddir);
  installdir = fullfile (builddir, "install");
  if (! exist (installdir, "dir"))
    [status, msg] = mkdir (installdir);
    if (status != 1)
      error ("could not create installation directory: %s", msg);
    endif
  endif
  files(1) = [];
  buildlist = fullfile (builddir, "octave_packages");
  install (files, handle_deps, autoload, installdir, installdir, verbose,
	   buildlist, "", false);
  unwind_protect
    repackage (builddir, buildlist);
  unwind_protect_cleanup
    unload_packages ({"all"}, handle_deps, buildlist, "");
    if (exist (installdir, "dir"))
      rm_rf (installdir);
    endif
    if (exist (buildlist, "file"))
      unlink (buildlist);
    endif
  end_unwind_protect
endfunction

function install (files, handle_deps, autoload, prefix, archprefix, verbose,
		  local_list, global_list, global_install)

  ## Check that the directory in prefix exist. If it doesn't: create it!
  if (! exist (prefix, "dir"))
    warning ("creating installation directory %s", prefix);
    [status, msg] = mkdir (prefix);
    if (status != 1)
      error ("could not create installation directory: %s", msg);
    endif
  endif

  ## Get the list of installed packages.
  [local_packages, global_packages] = installed_packages (local_list,
							  global_list);

  installed_pkgs_lst = {local_packages{:}, global_packages{:}};

  if (global_install)
    packages = global_packages;
  else
    packages = local_packages;
  endif

  ## Uncompress the packages and read the DESCRIPTION files.
  tmpdirs = packdirs = descriptions = {};
  try
    ## Warn about non existent files.
    for i = 1:length (files)
      if (isempty (glob(files{i})))
	warning ("file %s does not exist", files{i});
      endif
    endfor

    ## Unpack the package files and read the DESCRIPTION files.
    files = glob (files);
    packages_to_uninstall = [];
    for i = 1:length (files)
      tgz = files{i};

      if (exist (tgz, "file"))
	## Create a temporary directory.
	tmpdir = tmpnam ();
	tmpdirs{end+1} = tmpdir;
        if (verbose)
	  printf ("mkdir (%s)\n", tmpdir);
	endif
	[status, msg] = mkdir (tmpdir);
	if (status != 1)
	  error ("couldn't create temporary directory: %s", msg);
	endif

	## Uncompress the package.
	if (verbose)
	  printf ("untar (%s, %s)\n", tgz, tmpdir);
	endif
	untar (tgz, tmpdir);

	## Get the name of the directories produced by tar.
	[dirlist, err, msg] = readdir (tmpdir);
	if (err)
	  error ("couldn't read directory produced by tar: %s", msg);
	endif

	if (length (dirlist) > 3)
	  error ("bundles of packages are not allowed")
	endif
      endif

      ## The filename pointed to an uncompressed package to begin with.
      if (exist (tgz, "dir"))
	dirlist = {".", "..", tgz};
      endif

      if (exist (tgz, "file") || exist (tgz, "dir"))
	## The two first entries of dirlist are "." and "..".
	if (exist (tgz, "file"))
	  packdir = fullfile (tmpdir, dirlist{3});
	else
	  packdir = fullfile (pwd(), dirlist{3});
	endif
	packdirs{end+1} = packdir;

	## Make sure the package contains necessary files.
	verify_directory (packdir);

	## Read the DESCRIPTION file.
	filename = fullfile (packdir, "DESCRIPTION");
	desc = get_description (filename);

	## Verify that package name corresponds with filename.
	[dummy, nm] = fileparts (tgz);
	if ((length (nm) >= length (desc.name))
	    && ! strcmp (desc.name, nm(1:length(desc.name))))
	  error ("package name '%s' doesn't correspond to its filename '%s'",
		 desc.name, nm);
	endif

	## Set default installation directory.
	desc.dir = fullfile (prefix, cstrcat (desc.name, "-", desc.version));

	## Set default architecture dependent installation directory.
	desc.archprefix = fullfile (archprefix, cstrcat (desc.name, "-",
							 desc.version));

	## Save desc.
	descriptions{end+1} = desc;

	## Are any of the new packages already installed?
	## If so we'll remove the old version.
	for j = 1:length (packages)
	  if (strcmp (packages{j}.name, desc.name))
	    packages_to_uninstall(end+1) = j;
	  endif
	endfor
      endif
    endfor
  catch
    ## Something went wrong, delete tmpdirs.
    for i = 1:length (tmpdirs)
      rm_rf (tmpdirs{i});
    endfor
    rethrow (lasterror ());
  end_try_catch

  ## Check dependencies.
  if (handle_deps)
    ok = true;
    error_text = "";
    for i = 1:length (descriptions)
      desc = descriptions{i};
      idx2 = complement (i, 1:length(descriptions));
      if (global_install)
	## Global installation is not allowed to have dependencies on locally
	## installed packages.
	idx1 = complement (packages_to_uninstall,
			   1:length(global_packages));
	pseudo_installed_packages = {global_packages{idx1}, ...
				     descriptions{idx2}};
      else
	idx1 = complement (packages_to_uninstall,
			   1:length(local_packages));
	pseudo_installed_packages = {local_packages{idx1}, ...
				     global_packages{:}, ...
				     descriptions{idx2}};
      endif
      bad_deps = get_unsatisfied_deps (desc, pseudo_installed_packages);
      ## Are there any unsatisfied dependencies?
      if (! isempty (bad_deps))
	ok = false;
	for i = 1:length (bad_deps)
	  dep = bad_deps{i};
	  error_text = cstrcat (error_text, " ", desc.name, " needs ",
				dep.package, " ", dep.operator, " ",
				dep.version, "\n");
	endfor
      endif
    endfor

    ## Did we find any unsatisfied dependencies?
    if (! ok)
      error ("the following dependencies where unsatisfied:\n  %s", error_text);
    endif
  endif

  ## Prepare each package for installation.
  try
    for i = 1:length (descriptions)
      desc = descriptions{i};
      pdir = packdirs{i};
      prepare_installation (desc, pdir);
      configure_make (desc, pdir, verbose);
    endfor
  catch
    ## Something went wrong, delete tmpdirs.
    for i = 1:length (tmpdirs)
      rm_rf (tmpdirs{i});
    endfor
    rethrow (lasterror ());
  end_try_catch

  ## Uninstall the packages that will be replaced.
  try
    for i = packages_to_uninstall
      if (global_install)
	uninstall ({global_packages{i}.name}, false, verbose, local_list,
		   global_list, global_install);
      else
	uninstall ({local_packages{i}.name}, false, verbose, local_list,
		   global_list, global_install);
      endif
    endfor
  catch
    ## Something went wrong, delete tmpdirs.
    for i = 1:length (tmpdirs)
      rm_rf (tmpdirs{i});
    endfor
    rethrow (lasterror ());
  end_try_catch

  ## Install each package.
  try
    for i = 1:length (descriptions)
      desc = descriptions{i};
      pdir = packdirs{i};
      copy_files (desc, pdir, global_install);
      create_pkgadddel (desc, pdir, "PKG_ADD", global_install);
      create_pkgadddel (desc, pdir, "PKG_DEL", global_install);
      finish_installation (desc, pdir, global_install);
      generate_lookfor_cache (desc);
    endfor
  catch
    ## Something went wrong, delete tmpdirs.
    for i = 1:length (tmpdirs)
      rm_rf (tmpdirs{i});
    endfor
    for i = 1:length (descriptions)
      rm_rf (descriptions{i}.dir);
      rm_rf (getarchdir (descriptions{i}));
    endfor
    rethrow (lasterror ());
  end_try_catch

  ## Check if the installed directory is empty. If it is remove it
  ## from the list.
  for i = length (descriptions):-1:1
    if (dirempty (descriptions{i}.dir, {"packinfo", "doc"}) &&
	dirempty (getarchdir (descriptions{i})))
      warning ("package %s is empty\n", descriptions{i}.name);
      rm_rf (descriptions{i}.dir);
      rm_rf (getarchdir (descriptions{i}));
      descriptions(i) = [];
    endif
  endfor

  ## If the package requested that it is autoloaded, or the installer
  ## requested that it is, then mark the package as autoloaded.
  for i = length (descriptions):-1:1
    if (autoload > 0 || (autoload == 0 && isautoload (descriptions(i))))
      fclose (fopen (fullfile (descriptions{i}.dir, "packinfo",
			       ".autoload"), "wt"));
      descriptions{i}.autoload = 1;
    endif
  endfor

  ## Add the packages to the package list.
  try
    if (global_install)
      idx = complement (packages_to_uninstall, 1:length(global_packages));
      global_packages = save_order ({global_packages{idx}, descriptions{:}});
      save (global_list, "global_packages");
      installed_pkgs_lst = {local_packages{:}, global_packages{:}};
    else
      idx = complement (packages_to_uninstall, 1:length(local_packages));
      local_packages = save_order ({local_packages{idx}, descriptions{:}});
      save (local_list, "local_packages");
      installed_pkgs_lst = {local_packages{:}, global_packages{:}};
    endif
  catch
    ## Something went wrong, delete tmpdirs.
    for i = 1:length (tmpdirs)
      rm_rf (tmpdirs{i});
    endfor
    for i = 1:length (descriptions)
      rm_rf (descriptions{i}.dir);
    endfor
    if (global_install)
      printf ("error: couldn't append to %s\n", global_list);
    else
      printf ("error: couldn't append to %s\n", local_list);
    endif
    rethrow (lasterror ());
  end_try_catch

  ## All is well, let's clean up.
  for i = 1:length (tmpdirs)
    [status, msg] = rm_rf (tmpdirs{i});
    if (status != 1)
      warning ("couldn't clean up after my self: %s\n", msg);
    endif
  endfor

  ## Add the newly installed packages to the path, so the user
  ## can begin using them. Only load them if they are marked autoload.
  if (length (descriptions) > 0)
    idx = [];
    for i = 1:length (descriptions)
      if (isautoload (descriptions(i)))
	nm = descriptions{i}.name;
	for j = 1:length (installed_pkgs_lst)
	  if (strcmp (nm, installed_pkgs_lst{j}.name))
	    idx (end + 1) = j;
	    break;
	  endif
	endfor
      endif
    endfor
    load_packages_and_dependencies (idx, handle_deps, installed_pkgs_lst,
				    global_install);
  endif
endfunction

function uninstall (pkgnames, handle_deps, verbose, local_list,
		    global_list, global_install)
  ## Get the list of installed packages.
  [local_packages, global_packages] = installed_packages(local_list,
							 global_list);
  if (global_install)
    installed_pkgs_lst = {local_packages{:}, global_packages{:}};
  else
    installed_pkgs_lst = local_packages;
  endif

  num_packages = length (installed_pkgs_lst);
  delete_idx = [];
  for i = 1:num_packages
    cur_name = installed_pkgs_lst{i}.name;
    if (any (strcmp (cur_name, pkgnames)))
      delete_idx(end+1) = i;
    endif
  endfor

  ## Are all the packages that should be uninstalled already installed?
  if (length (delete_idx) != length (pkgnames))
    if (global_install)
      ## Try again for a locally installed package.
      installed_pkgs_lst = local_packages;

      num_packages = length (installed_pkgs_lst);
      delete_idx = [];
      for i = 1:num_packages
	cur_name = installed_pkgs_lst{i}.name;
	if (any (strcmp (cur_name, pkgnames)))
	  delete_idx(end+1) = i;
	endif
      endfor
      if (length (delete_idx) != length (pkgnames))
	## FIXME: We should have a better error message.
	warning ("some of the packages you want to uninstall are not installed");
      endif
    else
      ## FIXME: We should have a better error message.
      warning ("some of the packages you want to uninstall are not installed");
    endif
  endif

  ## Compute the packages that will remain installed.
  idx = complement (delete_idx, 1:num_packages);
  remaining_packages = {installed_pkgs_lst{idx}};

  ## Check dependencies.
  if (handle_deps)
    error_text = "";
    for i = 1:length (remaining_packages)
      desc = remaining_packages{i};
      bad_deps = get_unsatisfied_deps (desc, remaining_packages);

      ## Will the uninstallation break any dependencies?
      if (! isempty (bad_deps))
	for i = 1:length (bad_deps)
	  dep = bad_deps{i};
	  error_text = cstrcat (error_text, " ", desc.name, " needs ",
				dep.package, " ", dep.operator, " ",
				dep.version, "\n");
	endfor
      endif
    endfor

    if (! isempty (error_text))
      error ("the following dependencies where unsatisfied:\n  %s", error_text);
    endif
  endif

  ## Delete the directories containing the packages.
  for i = delete_idx
    desc = installed_pkgs_lst{i};
    ## If an 'on_uninstall.m' exist, call it!
    if (exist (fullfile (desc.dir, "packinfo", "on_uninstall.m"), "file"))
      wd = pwd ();
      cd (fullfile (desc.dir, "packinfo"));
      on_uninstall (desc);
      cd (wd);
    endif
    ## Do the actual deletion.
    if (desc.loaded)
      rmpath (desc.dir);
      if (exist (getarchdir (desc)))
	rmpath (getarchdir (desc));
      endif
    endif
    if (exist (desc.dir, "dir"))
      [status, msg] = rm_rf (desc.dir);
      if (status != 1)
	error ("couldn't delete directory %s: %s", desc.dir, msg);
      endif
      [status, msg] = rm_rf (getarchdir (desc));
      if (status != 1)
	error ("couldn't delete directory %s: %s", getarchdir (desc), msg);
      endif
      if (dirempty (desc.archprefix))
	rm_rf (desc.archprefix);
      endif
    else
      warning ("directory %s previously lost", desc.dir);
    endif
  endfor

  ## Write a new ~/.octave_packages.
  if (global_install)
    if (length (remaining_packages) == 0)
      unlink (global_list);
    else
      global_packages = save_order (remaining_packages);
      save (global_list, "global_packages");
    endif
  else
    if (length (remaining_packages) == 0)
      unlink (local_list);
    else
      local_packages = save_order (remaining_packages);
      save (local_list, "local_packages");
    endif
  endif

endfunction

function [pkg_desc_list, flag] = describe (pkgnames, verbose,
					   local_list, global_list)

  ## Get the list of installed packages.
  installed_pkgs_lst = installed_packages(local_list, global_list);
  num_packages = length (installed_pkgs_lst);


  describe_all = false;
  if (any (strcmp ("all", pkgnames)))
    describe_all = true;
    flag(1:num_packages) = {"Not Loaded"};
    num_pkgnames = num_packages;
  else
    num_pkgnames = length (pkgnames);
    flag(1:num_pkgnames) = {"Not installed"};
  endif

  for i = 1:num_packages
    curr_name = installed_pkgs_lst{i}.name;
    if (describe_all)
      name_pos = i;
    else
      name_pos = find(strcmp (curr_name, pkgnames));
    endif

    if (! isempty (name_pos))
      if (installed_pkgs_lst{i}.loaded)
	flag{name_pos} = "Loaded";
      else
	flag{name_pos} = "Not loaded";
      endif

      pkg_desc_list{name_pos}.name = installed_pkgs_lst{i}.name;
      pkg_desc_list{name_pos}.version = installed_pkgs_lst{i}.version;
      pkg_desc_list{name_pos}.description = installed_pkgs_lst{i}.description;
      pkg_desc_list{name_pos}.provides = parse_pkg_idx (installed_pkgs_lst{i}.dir);

    endif
  endfor

  non_inst = find (strcmp (flag, "Not installed"));
  if (! isempty (non_inst))
    if (nargout < 2)
      non_inst_str = sprintf (" %s ", pkgnames{non_inst});
      error ("some packages are not installed: %s", non_inst_str);
    else
      pkg_desc_list{non_inst} = struct ("name", {}, "description",
					{}, "provides", {});
    endif
  endif

  if (nargout == 0)
    for i = 1:num_pkgnames
      print_package_description (pkg_desc_list{i}.name,
				 pkg_desc_list{i}.version,
				 pkg_desc_list{i}.provides,
				 pkg_desc_list{i}.description,
				 flag{i}, verbose);
    endfor
  endif

endfunction

## AUXILIARY FUNCTIONS

## Read an INDEX file.
function [pkg_idx_struct] = parse_pkg_idx (packdir)

  index_file = fullfile (packdir, "packinfo", "INDEX");

  if (! exist (index_file, "file"))
    error ("could not find any INDEX file in directory %s, try 'pkg rebuild all' to generate missing INDEX files", packdir);
  endif


  [fid, msg] = fopen (index_file, "r");
  if (fid == -1)
    error ("the INDEX file %s could not be read: %s",
	   index_file, msg);
  endif

  cat_num = 1;
  pkg_idx_struct{1}.category = "Uncategorized";
  pkg_idx_struct{1}.functions = {};

  line = fgetl (fid);
  while (isempty (strfind (line, ">>")) && ! feof (fid))
    line = fgetl (fid);
  endwhile

  while (! feof (fid) || line != -1)
    if (! any (! isspace (line)) || line(1) == "#" || any (line == "="))
      ## Comments,  blank lines or comments about unimplemented
      ## functions: do nothing
      ## FIXME: probably comments and pointers to external functions
      ## could be treated better when printing to screen?
    elseif (! isempty (strfind (line, ">>")))
      ## Skip package name and description as they are in DESCRIPTION
      ## already.
    elseif (! isspace (line(1)))
      ## Category.
      if (! isempty (pkg_idx_struct{cat_num}.functions))
	pkg_idx_struct{++cat_num}.functions = {};
      endif
      pkg_idx_struct{cat_num}.category = deblank (line);
    else
      ## Function names.
      while (any (! isspace (line)))
	[fun_name, line] = strtok (line);
	pkg_idx_struct{cat_num}.functions{end+1} = deblank (fun_name);
      endwhile
    endif
    line = fgetl (fid);
  endwhile
  fclose (fid);
endfunction

function print_package_description (pkg_name, pkg_ver, pkg_idx_struct,
				    pkg_desc, status, verbose)

  printf ("---\nPackage name:\n\t%s\n", pkg_name);
  printf ("Version:\n\t%s\n", pkg_ver);
  printf ("Short description:\n\t%s\n", pkg_desc);
  printf ("Status:\n\t%s\n", status);
  if (verbose)
    printf ("---\nProvides:\n");
    for i = 1:length(pkg_idx_struct)
      if (! isempty (pkg_idx_struct{i}.functions))
	printf ("%s\n", pkg_idx_struct{i}.category);
	for j = 1:length(pkg_idx_struct{i}.functions)
	  printf ("\t%s\n", pkg_idx_struct{i}.functions{j});
	endfor
      endif
    endfor
  endif

endfunction


function pth = absolute_pathname (pth)
  [status, msg, msgid] = fileattrib (pth);
  if (status != 1)
    error ("could not find the file or path %s", pth);
  else
    pth = msg.Name;
  endif
endfunction

function repackage (builddir, buildlist)
  packages = installed_packages (buildlist, buildlist);

  wd = pwd();
  for i = 1 : length(packages)
    pack = packages{i};
    unwind_protect
      cd (builddir);
      mkdir (pack.name);
      mkdir (fullfile (pack.name, "inst"));
      copyfile (fullfile (pack.dir, "*"), fullfile (pack.name, "inst"));
      movefile (fullfile (pack.name, "inst","packinfo", "*"), pack.name);
      if (exist (fullfile (pack.name, "inst","packinfo", ".autoload"), "file"))
	unlink (fullfile (pack.name, "inst","packinfo", ".autoload"));
      endif
      rmdir (fullfile (pack.name, "inst", "packinfo"));
      if (exist (fullfile (pack.name, "inst", "doc"), "dir"))
	movefile (fullfile (pack.name, "inst", "doc"), pack.name);
      endif
      if (exist (fullfile (pack.name, "inst", "bin"), "dir"))
	movefile (fullfile (pack.name, "inst", "bin"), pack.name);
      endif
      archdir = fullfile (pack.archprefix, cstrcat (pack.name, "-",
						    pack.version), getarch ());
      if (exist (archdir, "dir"))
	if (exist (fullfile (pack.name, "inst", "PKG_ADD"), "file"))
	  unlink (fullfile (pack.name, "inst", "PKG_ADD"));
	endif
	if (exist (fullfile (pack.name, "inst", "PKG_DEL"), "file"))
	  unlink (fullfile (pack.name, "inst", "PKG_DEL"));
	endif
	if (exist (fullfile (archdir, "PKG_ADD"), "file"))
	  movefile (fullfile (archdir, "PKG_ADD"),
		    fullfile (pack.name, "PKG_ADD"));
	endif
	if (exist (fullfile (archdir, "PKG_DEL"), "file"))
	  movefile (fullfile (archdir, "PKG_DEL"),
		    fullfile (pack.name, "PKG_DEL"));
	endif
      else
	if (exist (fullfile (pack.name, "inst", "PKG_ADD"), "file"))
	  movefile (fullfile (pack.name, "inst", "PKG_ADD"),
		    fullfile (pack.name, "PKG_ADD"));
	endif
	if (exist (fullfile (pack.name, "inst", "PKG_DEL"), "file"))
	  movefile (fullfile (pack.name, "inst", "PKG_DEL"),
		    fullfile (pack.name, "PKG_DEL"));
	endif
      endif
      tfile = cstrcat (pack.name, "-", pack.version, ".tar");
      tar (tfile, pack.name);
      try
	gzip (tfile);
	unlink (tfile);
      catch
	warning ("failed to compress %s", tfile);
      end_try_catch
    unwind_protect_cleanup
      if (exist (pack.name, "dir"))
	rm_rf (pack.name);
      endif
      cd (wd);
    end_unwind_protect
  endfor
endfunction

function auto = isautoload (desc)
  auto = false;
  if (isfield (desc{1}, "autoload"))
    a = desc{1}.autoload;
    if ((isnumeric (a) && a > 0)
        || (ischar (a) && (strcmpi (a, "true")
			   || strcmpi (a, "on")
			   || strcmpi (a, "yes")
			   || strcmpi (a, "1"))))
      auto = true;
    endif
  endif
endfunction

function prepare_installation (desc, packdir)
  ## Is there a pre_install to call?
  if (exist (fullfile (packdir, "pre_install.m"), "file"))
    wd = pwd ();
    try
      cd (packdir);
      pre_install (desc);
      cd (wd);
    catch
      cd (wd);
      rethrow (lasterror ());
    end_try_catch
  endif

  ## If the directory "inst" doesn't exist, we create it.
  inst_dir = fullfile (packdir, "inst");
  if (! exist (inst_dir, "dir"))
    [status, msg] = mkdir (inst_dir);
    if (status != 1)
      rm_rf (desc.dir);
      error ("the 'inst' directory did not exist and could not be created: %s",
	     msg);
    endif
  endif
endfunction

function configure_make (desc, packdir, verbose)
  ## Perform ./configure, make, make install in "src".
  if (exist (fullfile (packdir, "src"), "dir"))
    src = fullfile (packdir, "src");
    ## Configure.
    if (exist (fullfile (src, "configure"), "file"))
      flags = "";
      if (isempty (getenv ("CC")))
        flags = cstrcat (flags, " CC=\"", octave_config_info ("CC"), "\"");
      endif
      if (isempty (getenv ("CXX")))
        flags = cstrcat (flags, " CXX=\"", octave_config_info ("CXX"), "\"");
      endif
      if (isempty (getenv ("AR")))
        flags = cstrcat (flags, " AR=\"", octave_config_info ("AR"), "\"");
      endif
      if (isempty (getenv ("RANLIB")))
        flags = cstrcat (flags, " RANLIB=\"", octave_config_info ("RANLIB"), "\"");
      endif
      [status, output] = shell (strcat ("cd '", src, "'; ./configure --prefix=\"",
                                        desc.dir, "\"", flags));
      if (status != 0)
	rm_rf (desc.dir);
	error ("the configure script returned the following error: %s", output);
      elseif (verbose)
	printf("%s", output);
      endif

    endif

    ## Make.
    if (exist (fullfile (src, "Makefile"), "file"))
      [status, output] = shell (cstrcat ("export INSTALLDIR=\"", desc.dir,
					 "\"; make -C '", src, "'"));
      if (status != 0)
	rm_rf (desc.dir);
	error ("'make' returned the following error: %s", output);
      elseif (verbose)
	printf("%s", output);
      endif
    endif

    ## Copy files to "inst" and "inst/arch" (this is instead of 'make
    ## install').
    files = fullfile (src, "FILES");
    instdir = fullfile (packdir, "inst");
    archdir = fullfile (packdir, "inst", getarch ());

    ## Get file names.
    if (exist (files, "file"))
      [fid, msg] = fopen (files, "r");
      if (fid < 0)
	error ("couldn't open %s: %s", files, msg);
      endif
      filenames = char (fread (fid))';
      fclose (fid);
      if (filenames(end) == "\n")
	filenames(end) = [];
      endif
      filenames = split_by (filenames, "\n");
      delete_idx =  [];
      for i = 1:length (filenames)
	if (! all (isspace (filenames{i})))
	  filenames{i} = fullfile (src, filenames{i});
	else
	  delete_idx(end+1) = i;
	endif
      endfor
      filenames(delete_idx) = [];
    else
      m = dir (fullfile (src, "*.m"));
      oct = dir (fullfile (src, "*.oct"));
      mex = dir (fullfile (src, "*.mex"));

      filenames = cellfun (@(x) fullfile (src, x),
			   {m.name, oct.name, mex.name},
			   "UniformOutput", false);
    endif

    ## Split into architecture dependent and independent files.
    if (isempty (filenames))
      idx = [];
    else
      idx = cellfun (@is_architecture_dependent, filenames);
    endif
    archdependent = filenames (idx);
    archindependent = filenames (!idx);

    ## Copy the files.
    if (! all (isspace ([filenames{:}])))
	if (! exist (instdir, "dir")) # fixindent
	  mkdir (instdir);
	endif
	if (! all (isspace ([archindependent{:}])))
	  if (verbose)
	    printf ("copyfile");
	    printf (" %s", archindependent{:});
	    printf ("%s\n", instdir);
	  endif
	  [status, output] = copyfile (archindependent, instdir);
	  if (status != 1)
	    rm_rf (desc.dir);
	    error ("Couldn't copy files from 'src' to 'inst': %s", output);
	  endif
        endif
	if (! all (isspace ([archdependent{:}])))
	  if (verbose)
	    printf ("copyfile");
	    printf (" %s", archdependent{:});
	    printf (" %s\n", archdir);
	  endif
	  if (! exist (archdir, "dir"))
	    mkdir (archdir);
	  endif
	  [status, output] = copyfile (archdependent, archdir);
	  if (status != 1)
	    rm_rf (desc.dir);
	    error ("Couldn't copy files from 'src' to 'inst': %s", output);
	  endif
        endif
    endif
  endif
endfunction

function pkg = extract_pkg (nm, pat)
  fid = fopen (nm, "rt");
  pkg = "";
  if (fid >= 0)
    while (! feof (fid))
      ln = fgetl (fid);
      if (ln > 0)
	t = regexp (ln, pat, "tokens");
	if (! isempty (t))
          pkg = cstrcat (pkg, "\n", t{1}{1});
	endif
      endif
    endwhile
    if (! isempty (pkg))
      pkg = cstrcat (pkg, "\n");
    endif
    fclose (fid);
  endif
endfunction

function create_pkgadddel (desc, packdir, nm, global_install)
  instpkg = fullfile (desc.dir, nm);
  instfid = fopen (instpkg, "wt");
  ## If it is exists, most of the  PKG_* file should go into the
  ## architecture dependent directory so that the autoload/mfilename
  ## commands work as expected. The only part that doesn't is the
  ## part in the main directory.
  archdir = fullfile (getarchprefix (desc), cstrcat (desc.name, "-",
						     desc.version), getarch ());
  if (exist (getarchdir (desc, global_install), "dir"))
    archpkg = fullfile (getarchdir (desc, global_install), nm);
    archfid = fopen (archpkg, "at");
  else
    archpkg = instpkg;
    archfid = instfid;
  endif

  if (archfid >= 0 && instfid >= 0)
    ## Search all dot-m files for PKG commands.
    lst = dir (fullfile (packdir, "inst", "*.m"));
    for i = 1:length (lst)
      nam = fullfile (packdir, "inst", lst(i).name);
      fwrite (instfid, extract_pkg (nam, ['^[#%][#%]* *' nm ': *(.*)$']));
    endfor

    ## Search all C++ source files for PKG commands.
    lst = dir (fullfile (packdir, "src", "*.cc"));
    for i = 1:length (lst)
      nam = fullfile (packdir, "src", lst(i).name);
      fwrite (archfid, extract_pkg (nam, ['^//* *' nm ': *(.*)$']));
      fwrite (archfid, extract_pkg (nam, ['^/\** *' nm ': *(.*) *\*/$']));
    endfor

    ## Add developer included PKG commands.
    packdirnm = fullfile (packdir, nm);
    if (exist (packdirnm, "file"))
      fid = fopen (packdirnm, "rt");
      if (fid >= 0)
        while (! feof (fid))
          ln = fgets (fid);
          if (ln > 0)
            fwrite (archfid, ln);
          endif
        endwhile
        fclose (fid);
      endif
    endif

    ## If the files is empty remove it.
    fclose (instfid);
    t = dir (instpkg);
    if (t.bytes <= 0)
      unlink (instpkg);
    endif

    if (instfid != archfid)
      fclose (archfid);
      t = dir (archpkg);
      if (t.bytes <= 0)
        unlink (archpkg);
      endif
    endif
  endif
endfunction

function copy_files (desc, packdir, global_install)
  ## Create the installation directory.
  if (! exist (desc.dir, "dir"))
    [status, output] = mkdir (desc.dir);
    if (status != 1)
      error ("couldn't create installation directory %s : %s",
	     desc.dir, output);
    endif
  endif

  octfiledir = getarchdir (desc);

  ## Copy the files from "inst" to installdir.
  instdir = fullfile (packdir, "inst");
  if (! dirempty (instdir))
    [status, output] = copyfile (fullfile (instdir, "*"), desc.dir);
    if (status != 1)
      rm_rf (desc.dir);
      error ("couldn't copy files to the installation directory");
    endif
    if (exist (fullfile (desc.dir, getarch ()), "dir") &&
	! strcmp (fullfile (desc.dir, getarch ()), octfiledir))
      if (! exist (octfiledir, "dir"))
        ## Can be required to create upto three levels of dirs.
        octm1 = fileparts (octfiledir);
        if (! exist (octm1, "dir"))
          octm2 = fileparts (octm1);
          if (! exist (octm2, "dir"))
            octm3 = fileparts (octm2);
            if (! exist (octm3, "dir"))
              [status, output] = mkdir (octm3);
              if (status != 1)
                rm_rf (desc.dir);
                error ("couldn't create installation directory %s : %s",
                       octm3, output);
              endif
            endif
            [status, output] = mkdir (octm2);
            if (status != 1)
              rm_rf (desc.dir);
              error ("couldn't create installation directory %s : %s",
                     octm2, output);
            endif
          endif
          [status, output] = mkdir (octm1);
          if (status != 1)
            rm_rf (desc.dir);
            error ("couldn't create installation directory %s : %s",
                   octm1, output);
          endif
        endif
        [status, output] = mkdir (octfiledir);
        if (status != 1)
          rm_rf (desc.dir);
          error ("couldn't create installation directory %s : %s",
		 octfiledir, output);
        endif
      endif
      [status, output] = movefile (fullfile (desc.dir, getarch (), "*"),
				   octfiledir);
      rm_rf (fullfile (desc.dir, getarch ()));

      if (status != 1)
        rm_rf (desc.dir);
        rm_rf (octfiledir);
        error ("couldn't copy files to the installation directory");
      endif
    endif

  endif

  ## Create the "packinfo" directory.
  packinfo = fullfile (desc.dir, "packinfo");
  [status, msg] = mkdir (packinfo);
  if (status != 1)
    rm_rf (desc.dir);
    rm_rf (octfiledir);
    error ("couldn't create packinfo directory: %s", msg);
  endif

  ## Copy DESCRIPTION.
  [status, output] = copyfile (fullfile (packdir, "DESCRIPTION"), packinfo);
  if (status != 1)
    rm_rf (desc.dir);
    rm_rf (octfiledir);
    error ("couldn't copy DESCRIPTION: %s", output);
  endif

  ## Copy COPYING.
  [status, output] = copyfile (fullfile (packdir, "COPYING"), packinfo);
  if (status != 1)
    rm_rf (desc.dir);
    rm_rf (octfiledir);
    error ("couldn't copy COPYING: %s", output);
  endif

  ## If the file ChangeLog exists, copy it.
  changelog_file = fullfile (packdir, "ChangeLog");
  if (exist (changelog_file, "file"))
    [status, output] = copyfile (changelog_file, packinfo);
    if (status != 1)
      rm_rf (desc.dir);
      rm_rf (octfiledir);
      error ("couldn't copy ChangeLog file: %s", output);
    endif
  endif

  ## Is there an INDEX file to copy or should we generate one?
  index_file = fullfile (packdir, "INDEX");
  if (exist(index_file, "file"))
    [status, output] = copyfile (index_file, packinfo);
    if (status != 1)
      rm_rf (desc.dir);
      rm_rf (octfiledir);
      error ("couldn't copy INDEX file: %s", output);
    endif
  else
    try
      write_index (desc, fullfile (packdir, "inst"),
		   fullfile (packinfo, "INDEX"), global_install);
    catch
      rm_rf (desc.dir);
      rm_rf (octfiledir);
      rethrow (lasterror ());
    end_try_catch
  endif

  ## Is there an 'on_uninstall.m' to install?
  fon_uninstall = fullfile (packdir, "on_uninstall.m");
  if (exist (fon_uninstall, "file"))
    [status, output] = copyfile (fon_uninstall, packinfo);
    if (status != 1)
      rm_rf (desc.dir);
      rm_rf (octfiledir);
      error ("couldn't copy on_uninstall.m: %s", output);
    endif
  endif

  ## Is there a doc/ directory that needs to be installed?
  docdir = fullfile (packdir, "doc");
  if (exist (docdir, "dir") && ! dirempty (docdir))
    [status, output] = copyfile (docdir, desc.dir);
  endif

  ## Is there a bin/ directory that needs to be installed?
  ## FIXME: Need to treat architecture dependent files in bin/
  bindir = fullfile (packdir, "bin");
  if (exist (bindir, "dir") && ! dirempty (bindir))
    [status, output] = copyfile (bindir, desc.dir);
  endif
endfunction

function finish_installation (desc, packdir, global_install)
  ## Is there a post-install to call?
  if (exist (fullfile (packdir, "post_install.m"), "file"))
    wd = pwd ();
    try
      cd (packdir);
      post_install (desc);
      cd (wd);
    catch
      cd (wd);
      rm_rf (desc.dir);
      rm_rf (getarchdir (desc), global_install);
      rethrow (lasterror ());
    end_try_catch
  endif
endfunction

function generate_lookfor_cache (desc)
  dirs = split_by (genpath (desc.dir), pathsep ());
  for i = 1 : length (dirs)
    gen_doc_cache (fullfile (dirs{i}, "doc-cache"), dirs{i});
  endfor
endfunction

## Make sure the package contains the essential files.
function verify_directory (dir)
  needed_files = {"COPYING", "DESCRIPTION"};
  for f = needed_files
    if (! exist (fullfile (dir, f{1}), "file"))
      error ("package is missing file: %s", f{1});
    endif
  endfor
endfunction

## Parse the DESCRIPTION file.
function desc = get_description (filename)
  [fid, msg] = fopen (filename, "r");
  if (fid == -1)
    error ("the DESCRIPTION file %s could not be read: %s", filename, msg);
  endif

  desc = struct ();

  line = fgetl (fid);
  while (line != -1)
    if (line(1) == "#")
      ## Comments, do nothing.
    elseif (isspace(line(1)))
      ## Continuation lines
      if (exist ("keyword", "var") && isfield (desc, keyword))
	desc.(keyword) = cstrcat (desc.(keyword), " ", rstrip(line));
      endif
    else
      ## Keyword/value pair
      colon = find (line == ":");
      if (length (colon) == 0)
	disp ("skipping line");
      else
	colon = colon(1);
	keyword = tolower (strip (line(1:colon-1)));
	value = strip (line (colon+1:end));
	if (length (value) == 0)
	  fclose (fid);
	  error ("the keyword %s has an empty value", desc.keywords{end});
	endif
	desc.(keyword) = value;
      endif
    endif
    line = fgetl (fid);
  endwhile
  fclose (fid);

  ## Make sure all is okay.
  needed_fields = {"name", "version", "date", "title", ...
		   "author", "maintainer", "description"};
  for f = needed_fields
    if (! isfield (desc, f{1}))
      error ("description is missing needed field %s", f{1});
    endif
  endfor
  desc.version = fix_version (desc.version);
  if (isfield (desc, "depends"))
    desc.depends = fix_depends (desc.depends);
  else
    desc.depends = "";
  endif
  desc.name = tolower (desc.name);
endfunction

## Make sure the version string v is a valid x.y.z version string
## Examples: "0.1" => "0.1.0", "monkey" => error(...).
function out = fix_version (v)
  dots = find (v == ".");
  if (length (dots) == 1)
    major = str2num (v(1:dots-1));
    minor = str2num (v(dots+1:end));
    if (length (major) != 0 && length (minor) != 0)
      out = sprintf ("%d.%d.0", major, minor);
      return;
    endif
  elseif (length (dots) == 2)
    major = str2num (v(1:dots(1)-1));
    minor = str2num (v(dots(1)+1:dots(2)-1));
    rev = str2num (v(dots(2)+1:end));
    if (length (major) != 0 && length (minor) != 0 && length (rev) != 0)
      out = sprintf ("%d.%d.%d", major, minor, rev);
      return;
    endif
  endif
  error ("bad version string: %s", v);
endfunction

## Make sure the depends field is of the right format.
## This function returns a cell of structures with the following fields:
##   package, version, operator
function deps_cell = fix_depends (depends)
  deps = split_by (tolower (depends), ",");
  deps_cell = cell (1, length (deps));

  ## For each dependency.
  for i = 1:length (deps)
    dep = deps{i};
    lpar = find (dep == "(");
    rpar = find (dep == ")");
    ## Does the dependency specify a version
    ## Example: package(>= version).
    if (length (lpar) == 1 && length (rpar) == 1)
      package = tolower (strip (dep(1:lpar-1)));
      sub = dep(lpar(1)+1:rpar(1)-1);
      parts = strsplit (sub, " ", true);
      if (length (parts) != 2)
	error ("incorrect syntax for dependency `%s' in the DESCRIPTION file\n",
	       dep);
      endif
      operator = parts{1};
      if (! any (strcmp (operator, {">", ">=", "<=", "<", "=="})))
	error ("unsupported operator: %s", operator);
      endif
      version  = fix_version (parts{2});

      ## If no version is specified for the dependency
      ## we say that the version should be greater than
      ## or equal to "0.0.0".
    else
      package = tolower (strip (dep));
      operator = ">=";
      version  = "0.0.0";
    endif
    deps_cell{i} = struct ("package", package, "operator", operator,
			   "version", version);
  endfor
endfunction

## Strip the text of spaces from the right
## Example: "  hello world  " => "  hello world"
## FIXME -- is this the same as deblank?
function text = rstrip (text)
  chars = find (! isspace (text));
  if (length (chars) > 0)
    ## FIXME: shouldn't it be text = text(1:chars(end));
    text = text (chars(1):end);
  else
    text = "";
  endif
endfunction

## Strip the text of spaces from the left and the right.
## Example: "  hello world  " => "hello world"
function text = strip (text)
  chars = find (! isspace (text));
  if (length (chars) > 0)
    text = text(chars(1):chars(end));
  else
    text = "";
  endif
endfunction

## Split the text into a cell array of strings by sep.
## Example: "A, B" => {"A", "B"} (with sep = ",")
function out = split_by (text, sep)
  out = strtrim (strsplit (text, sep));
endfunction

## Create an INDEX file for a package that doesn't provide one.
##   'desc'  describes the package.
##   'dir'   is the 'inst' directory in temporary directory.
##   'index_file' is the name (including path) of resulting INDEX file.
function write_index (desc, dir, index_file, global_install)
  ## Get names of functions in dir
  [files, err, msg] = readdir (dir);
  if (err)
    error ("couldn't read directory %s: %s", dir, msg);
  endif

  ## Check for architecture dependent files.
  tmpdir = getarchdir (desc);
  if (exist (tmpdir, "dir"))
    [files2, err, msg] = readdir (tmpdir);
    if (err)
      error ("couldn't read directory %s: %s", tmpdir, msg);
    endif
    files = [files; files2];
  endif

  functions = {};
  for i = 1:length (files)
    file = files{i};
    lf = length (file);
    if (lf > 2 && strcmp (file(end-1:end), ".m"))
      functions{end+1} = file(1:end-2);
    elseif (lf > 4 && strcmp (file(end-3:end), ".oct"))
      functions{end+1} = file(1:end-4);
    endif
  endfor

  ## Does desc have a categories field?
  if (! isfield (desc, "categories"))
    error ("the DESCRIPTION file must have a Categories field, when no INDEX file is given");
  endif
  categories = split_by (desc.categories, ",");
  if (length (categories) < 1)
    error ("the Category field is empty");
  endif

  ## Write INDEX.
  fid = fopen (index_file, "w");
  if (fid == -1)
    error ("couldn't open %s for writing.", index_file);
  endif
  fprintf (fid, "%s >> %s\n", desc.name, desc.title);
  fprintf (fid, "%s\n", categories{1});
  fprintf (fid, "  %s\n", functions{:});
  fclose (fid);
endfunction

function bad_deps = get_unsatisfied_deps (desc, installed_pkgs_lst)
  bad_deps = {};

  ## For each dependency.
  for i = 1:length (desc.depends)
    dep = desc.depends{i};

    ## Is the current dependency Octave?
    if (strcmp (dep.package, "octave"))
      if (! compare_versions (OCTAVE_VERSION, dep.version, dep.operator))
        bad_deps{end+1} = dep;
      endif
      ## Is the current dependency not Octave?
    else
      ok = false;
      for i = 1:length (installed_pkgs_lst)
	cur_name = installed_pkgs_lst{i}.name;
	cur_version = installed_pkgs_lst{i}.version;
	if (strcmp (dep.package, cur_name)
	    && compare_versions (cur_version, dep.version, dep.operator))
	  ok = true;
	  break;
	endif
      endfor
      if (! ok)
        bad_deps{end+1} = dep;
      endif
    endif
  endfor
endfunction

function [out1, out2] = installed_packages (local_list, global_list)
  ## Get the list of installed packages.
  try
    local_packages = load (local_list).local_packages;
  catch
    local_packages = {};
  end_try_catch
  try
    global_packages = load (global_list).global_packages;
  catch
    global_packages = {};
  end_try_catch
  installed_pkgs_lst = {local_packages{:}, global_packages{:}};

  ## Eliminate duplicates in the installed package list.
  ## Locally installed packages take precedence.
  dup = [];
  for i = 1:length (installed_pkgs_lst)
    if (find (dup, i))
      continue;
    endif
    for j = (i+1):length (installed_pkgs_lst)
      if (find (dup, j))
	continue;
      endif
      if (strcmp (installed_pkgs_lst{i}.name, installed_pkgs_lst{j}.name))
	dup = [dup, j];
      endif
    endfor
  endfor
  if (! isempty(dup))
    installed_pkgs_lst(dup) = [];
  endif

  ## Now check if the package is loaded.
  tmppath = strrep (path(), "\\", "/");
  for i = 1:length (installed_pkgs_lst)
    if (findstr (tmppath, strrep (installed_pkgs_lst{i}.dir, "\\", "/")))
      installed_pkgs_lst{i}.loaded = true;
    else
      installed_pkgs_lst{i}.loaded = false;
    endif
  endfor
  for i = 1:length (local_packages)
    if (findstr (tmppath, strrep (local_packages{i}.dir, "\\", "/")))
      local_packages{i}.loaded = true;
    else
      local_packages{i}.loaded = false;
    endif
  endfor
  for i = 1:length (global_packages)
    if (findstr (tmppath, strrep (global_packages{i}.dir, "\\", "/")))
      global_packages{i}.loaded = true;
    else
      global_packages{i}.loaded = false;
    endif
  endfor

  ## Should we return something?
  if (nargout == 2)
    out1 = local_packages;
    out2 = global_packages;
    return;
  elseif (nargout == 1)
    out1 = installed_pkgs_lst;
    return;
  endif

  ## We shouldn't return something, so we'll print something.
  num_packages = length (installed_pkgs_lst);
  if (num_packages == 0)
    printf ("no packages installed.\n");
    return;
  endif

  ## Compute the maximal lengths of name, version, and dir.
  h1 = "Package Name";
  h2 = "Version";
  h3 = "Installation directory";
  max_name_length = length (h1);
  max_version_length = length (h2);
  names = cell (num_packages, 1);
  for i = 1:num_packages
    max_name_length = max (max_name_length,
			   length (installed_pkgs_lst{i}.name));
    max_version_length = max (max_version_length,
			      length (installed_pkgs_lst{i}.version));
    names{i} = installed_pkgs_lst{i}.name;
  endfor
  max_dir_length = terminal_size()(2) - max_name_length - ...
		   max_version_length - 7;
  if (max_dir_length < 20)
    max_dir_length = Inf;
  endif

  h1 = postpad (h1, max_name_length + 1, " ");
  h2 = postpad (h2, max_version_length, " ");

  ## Print a header.
  header = sprintf("%s | %s | %s\n", h1, h2, h3);
  printf (header);
  tmp = sprintf (repmat ("-", 1, length(header)-1));
  tmp(length(h1)+2) = "+";
  tmp(length(h1)+length(h2)+5) = "+";
  printf ("%s\n", tmp);

  ## Print the packages.
  format = sprintf ("%%%ds %%1s| %%%ds | %%s\n", max_name_length,
		    max_version_length);
  [dummy, idx] = sort (names);
  for i = 1:num_packages
    cur_name = installed_pkgs_lst{idx(i)}.name;
    cur_version = installed_pkgs_lst{idx(i)}.version;
    cur_dir = installed_pkgs_lst{idx(i)}.dir;
    if (length (cur_dir) > max_dir_length)
      first_char = length (cur_dir) - max_dir_length + 4;
      first_filesep = strfind (cur_dir(first_char:end), filesep());
      if (! isempty (first_filesep))
        cur_dir = cstrcat ("...",
			   cur_dir((first_char + first_filesep(1) - 1):end));
      else
        cur_dir = cstrcat ("...", cur_dir(first_char:end));
      endif
    endif
    if (installed_pkgs_lst{idx(i)}.loaded)
      cur_loaded = "*";
    else
      cur_loaded = " ";
    endif
    printf (format, cur_name, cur_loaded, cur_version, cur_dir);
  endfor
endfunction

function load_packages (files, handle_deps, local_list, global_list)
  installed_pkgs_lst = installed_packages (local_list, global_list);
  num_packages = length (installed_pkgs_lst);

  ## Read package names and installdirs into a more convenient format.
  pnames = pdirs = cell (1, num_packages);
  for i = 1:num_packages
    pnames{i} = installed_pkgs_lst{i}.name;
    pdirs{i} = installed_pkgs_lst{i}.dir;
  endfor

  ## Load all.
  if (length (files) == 1 && strcmp (files{1}, "all"))
    idx = [1:length(installed_pkgs_lst)];
    ## Load auto.
  elseif (length (files) == 1 && strcmp (files{1}, "auto"))
    idx = [];
    for i = 1:length (installed_pkgs_lst)
      if (exist (fullfile (pdirs{i}, "packinfo", ".autoload"), "file"))
	idx (end + 1) = i;
      endif
    endfor
    ## Load package_name1 ...
  else
    idx = [];
    for i = 1:length (files)
      idx2 = find (strcmp (pnames, files{i}));
      if (! any (idx2))
	error ("package %s is not installed", files{i});
      endif
      idx (end + 1) = idx2;
    endfor
  endif

  ## Load the packages, but take care of the ordering of dependencies.
  load_packages_and_dependencies (idx, handle_deps, installed_pkgs_lst, true);
endfunction

function unload_packages (files, handle_deps, local_list, global_list)
  installed_pkgs_lst = installed_packages (local_list, global_list);
  num_packages = length (installed_pkgs_lst);

  ## Read package names and installdirs into a more convenient format.
  pnames = pdirs = cell (1, num_packages);
  for i = 1:num_packages
    pnames{i} = installed_pkgs_lst{i}.name;
    pdirs{i} = installed_pkgs_lst{i}.dir;
    pdeps{i} = installed_pkgs_lst{i}.depends;
  endfor

  ## Get the current octave path.
  p = split_by (path(), pathsep ());

  if (length (files) == 1 && strcmp (files{1}, "all"))
    ## Unload all.
    dirs = pdirs;
    desc = installed_pkgs_lst;
  else
    ## Unload package_name1 ...
    dirs = {};
    desc = {};
    for i = 1:length (files)
      idx = strcmp (pnames, files{i});
      if (! any (idx))
	error ("package %s is not installed", files{i});
      endif
      dirs{end+1} = pdirs{idx};
      desc{end+1} = installed_pkgs_lst{idx};
    endfor
  endif

  ## Check for architecture dependent directories.
  archdirs = {};
  for i = 1:length (dirs)
    tmpdir = getarchdir (desc{i});
    if (exist (tmpdir, "dir"))
      archdirs{end+1} = dirs{i};
      archdirs{end+1} = tmpdir;
    else
      archdirs{end+1} = dirs{i};
    endif
  endfor

  ## Unload the packages.
  for i = 1:length (archdirs)
    d = archdirs{i};
    idx = strcmp (p, d);
    if (any (idx))
      rmpath (d);
      ## FIXME: We should also check if we need to remove items from
      ## EXEC_PATH.
    endif
  endfor
endfunction

function [status_out, msg_out] = rm_rf (dir)
  if (exist (dir))
    crr = confirm_recursive_rmdir ();
    unwind_protect
      confirm_recursive_rmdir (false);
      [status, msg] = rmdir (dir, "s");
    unwind_protect_cleanup
      confirm_recursive_rmdir (crr);
    end_unwind_protect
  else
    status = 1;
    msg = "";
  endif
  if (nargout > 0)
    status_out = status;
  endif
  if (nargout > 1)
    msg_out = msg;
  endif
endfunction

function emp = dirempty (nm, ign)
  if (exist (nm, "dir"))
    if (nargin < 2)
      ign = {".", ".."};
    else
      ign = [{".", ".."}, ign];
    endif
    l = dir (nm);
    for i = 1:length (l)
      found = false;
      for j = 1:length (ign)
        if (strcmp (l(i).name, ign{j}))
          found = true;
          break;
        endif
      endfor
      if (! found)
        emp = false;
        return
      endif
    endfor
    emp = true;
  else
    emp = true;
  endif
endfunction

function arch = getarch ()
  persistent _arch = cstrcat (octave_config_info("canonical_host_type"), ...
			      "-", octave_config_info("api_version"));
  arch = _arch;
endfunction

function archprefix = getarchprefix (desc, global_install)
  if ((nargin == 2 && global_install) || (nargin < 2 && issuperuser ()))
    archprefix = fullfile (octave_config_info ("libexecdir"), "octave",
			   "packages", cstrcat(desc.name, "-", desc.version));
  else
    archprefix = desc.dir;
  endif
endfunction

function archdir = getarchdir (desc)
  archdir = fullfile (desc.archprefix, getarch());
endfunction

function s = issuperuser ()
  if ((ispc () && ! isunix ()) || (geteuid() == 0))
    s = true;
  else
    s = false;
  endif
endfunction

function [status, output] = shell (cmd)
  persistent have_sh;

  cmd = strrep (cmd, "\\", "/");
  if (ispc () && ! isunix ())
    if (isempty(have_sh))
      if (system("sh.exe -c \"exit\""))
        have_sh = false;
      else
        have_sh = true;
      endif
    endif
    if (have_sh)
      [status, output] = system (cstrcat ("sh.exe -c \"", cmd, "\""));
    else
      error ("Can not find the command shell")
    endif
  else
    [status, output] = system (cmd);
  endif
endfunction

function newdesc = save_order (desc)
  newdesc = {};
  for i = 1 : length(desc)
    deps = desc{i}.depends;
    if (isempty (deps) || (length (deps) == 1 &&
			   strcmp(deps{1}.package, "octave")))
      newdesc {end + 1} = desc{i};
    else
      tmpdesc = {};
      for k = 1 : length (deps)
        for j = 1 : length (desc)
          if (strcmp (desc{j}.name, deps{k}.package))
            tmpdesc{end+1} = desc{j};
	    break;
          endif
        endfor
      endfor
      if (! isempty (tmpdesc))
        newdesc = {newdesc{:}, save_order(tmpdesc){:}, desc{i}};
      else
        newdesc{end+1} = desc{i};
      endif
    endif
  endfor
  ## Eliminate the duplicates.
  idx = [];
  for i = 1 : length (newdesc)
    for j = (i + 1) : length (newdesc)
      if (strcmp (newdesc{i}.name, newdesc{j}.name))
        idx (end + 1) = j;
      endif
    endfor
  endfor
  newdesc(idx) = [];
endfunction

function load_packages_and_dependencies (idx, handle_deps, installed_pkgs_lst,
					 global_install)
  idx = load_package_dirs (idx, [], handle_deps, installed_pkgs_lst);
  dirs = {};
  execpath = EXEC_PATH ();
  for i = idx;
    ndir = installed_pkgs_lst{i}.dir;
    dirs{end+1} = ndir;
    if (exist (fullfile (dirs{end}, "bin"), "dir"))
      execpath = cstrcat (fullfile (dirs{end}, "bin"), ":", execpath);
    endif
    tmpdir = getarchdir (installed_pkgs_lst{i});
    if (exist (tmpdir, "dir"))
      dirs{end + 1} = tmpdir;
      if (exist (fullfile (dirs{end}, "bin"), "dir"))
        execpath = cstrcat (fullfile (dirs{end}, "bin"), ":", execpath);
      endif
    endif
  endfor

  ## Load the packages.
  if (length (dirs) > 0)
    addpath (dirs{:});
  endif

  ## Add the binaries to exec_path.
  if (! strcmp (EXEC_PATH, execpath))
    EXEC_PATH (execpath);
  endif
endfunction

function idx = load_package_dirs (lidx, idx, handle_deps, installed_pkgs_lst)
  for i = lidx
    if (isfield (installed_pkgs_lst{i}, "loaded") &&
	installed_pkgs_lst{i}.loaded)
      continue;
    else
      if (handle_deps)
        deps = installed_pkgs_lst{i}.depends;
        if ((length (deps) > 1) || (length (deps) == 1 &&
	  			    ! strcmp(deps{1}.package, "octave")))
          tmplidx = [];
          for k = 1 : length (deps)
            for j = 1 : length (installed_pkgs_lst)
              if (strcmp (installed_pkgs_lst{j}.name, deps{k}.package))
                tmplidx (end + 1) = j;
	        break;
              endif
            endfor
          endfor
          idx = load_package_dirs (tmplidx, idx, handle_deps,
				   installed_pkgs_lst);
        endif
      endif
      if (isempty (find(idx == i)))
        idx (end + 1) = i;
      endif
    endif
  endfor
endfunction

function dep = is_architecture_dependent (nm)
  persistent archdepsuffix = {".oct",".mex",".a",".lib",".so",".so.*",".dll","dylib"};

  dep = false;
  for i = 1 : length (archdepsuffix)
    ext = archdepsuffix{i};
    if (ext(end) == "*")
      isglob = true;
      ext(end) = [];
    else
      isglob = false;		# I am a test
				#%% me too
### I shall align to column 0
    endif
    pos = findstr (nm, ext);
    if (pos)
      if (! isglob && (length(nm) - pos(end) != length(ext) - 1))
	continue;
      endif
      dep = true;
      break;
    endif
  endfor
endfunction

%!assert(norm(logm([1 -1;0 1]) - [0 -1; 0 0]) < 1e-5);
%!assert(norm(expm(logm([-1 2 ; 4 -1])) - [-1 2 ; 4 -1]) < 1e-5);
%!assert(logm([1 -1 -1;0 1 -1; 0 0 1]), [0 -1 -1.5; 0 0 -1; 0 0 0], 1e-5);
%!assert (logm (expm ([0 1i; -1i 0])), [0 1i; -1i 0], 10 * eps)

%% Test input validation
%!error logm ();
%!error logm (1, 2, 3);
%!error <logm: A must be a square matrix> logm([1 0;0 1; 2 2]);

%!assert (logm (10), log (10))
%!assert (full (logm (eye (3))), logm (full (eye (3))))
%!assert (full (logm (10*eye (3))), logm (full (10*eye (3))), 8*eps)
