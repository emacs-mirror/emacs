#include <stdio.h>
#include "Util.hh"

//
// This file contains the impl for the symbols in inc/Util.hh
// and this is the destination of some symref jumps.
//
namespace play { namespace prod {

    // The symbol in namespaces.
    int myUtilFunc(const int param) {
      printf("Inside myUtilFunc(%d).\n", param);
      return param;
    }

  }}

// The symbol outside of namespaces.
int myUtilFuncNoNS(const int param) {
  printf("Inside myUtilFuncNoNS(%d).\n", param);
  return param;
}
