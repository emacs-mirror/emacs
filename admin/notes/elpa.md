# NOTES ON THE EMACS PACKAGE ARCHIVE

The Emacs package archives at `elpa.gnu.org` (GNU ELPA and NonGNU ELPA)
are managed using two Git repositories named `gnu.git` and `nongnu.git`
hosted in the `elpa` group on Savannah.
To check them out:

    git clone https://git.savannah.gnu.org/git/elpa/gnu.git
    cd gnu
    make setup

resp.

    git clone https://git.savannah.gnu.org/git/elpa/nongnu.git
    cd nongnu
    make setup

That leaves the `(non)gnu/packages` directory empty; you must check out the
ones you want.

If you wish to check out all the packages into the packages directory,
you can run the command:

    make worktrees

You can check out a specific package <pkgname> into the packages
directory with:

    make packages/<pkgname>

Changes to this repository propagate to `elpa.gnu.org` via a
"deployment" script run daily.  This script generates the content
visible at https://elpa.gnu.org/packages and https://elpa.nongnu.org/nongnu

A new package is released as soon as the "version number" of that
package is changed (as found in the `;; Version:` header of the main
ELisp file of the package).  So you can use `elpa/(non)gnu.git` to work
on a package without fear of releasing those changes prematurely.
And once the code is ready, just bump the version number to make a new
release of the package.

It is easy to use these repositories to deploy a "local" copy of the
package archive.  For details, see the README file after cloning them.
