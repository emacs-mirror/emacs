@rem w3i6mv.bat -- set up and invoke build from raw CMD on Windows
@rem This can be invoked from a plain CMD from the MPS workspace
@rem like this::
@rem   code\w3i6mv.bat
@rem or from Git Bash in Travis CI build machines like this::
@rem   MSYS2_ARG_CONV_EXCL='*' cmd /k 'code\w3i6mv.bat'
@rem which is how it's invoked from .travis.yml
call "C:\Program Files (x86)\Microsoft Visual Studio\2017\BuildTools\VC\Auxiliary\Build\vcvarsall.bat" x64
cd code
nmake /f w3i6mv.nmk
