#include "Util.hh"
#include "Manager.hh"
#include "ManagerTest.hh"

//
// This fcn uses the namespaces used in the test, but should not be
// a search destination.
//
namespace play { namespace test {

    ManagerTest() {
      printf("Inside ManagerTest ctor.\n");
      play::prod::Manager manager(666);
    }

}}
