#pragma once
//
// This file contains the symbols that are searched from in the globalref test.
//

namespace play { namespace prod {

    // This sym is in some namespaces.
    int myUtilFunc(const int param);

}}

// This sym is outside of all namespaces.
int myUtilFuncNoNS(const int param);

//End
