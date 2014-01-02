#include "Util.hh"
#include "Manager.hh"

//
// This contains source using namespaces that are used in the test,
// but with an impl that we aren't searching for.
//
namespace play { namespace prod {

Manager(const int param) {
   printf("Inside Manager ctor. myUtilFunc returned: %d\n", play::prod::myUtilFunc(param));
}

}}
