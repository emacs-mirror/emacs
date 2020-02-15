// testsubclass.hh --- unit test for analyzer and complex C++ inheritance

// Copyright (C) 2007-2020 Free Software Foundation, Inc.

// Author: Eric M. Ludlam <zappo@gnu.org>

// This file is part of GNU Emacs.

// GNU Emacs is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// GNU Emacs is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

//#include <cmath>
// #include <stdio.h>

#ifndef TESTSUBCLASS_HH
#define TESTSUBCLASS_HH

namespace animal {

  class moose {
  public:
    moose() : fFeet(0),
	      fIsValid(false)
    { }

    virtual void setFeet(int);
    int getFeet();

    void doNothing();

    enum moose_enum {
      NAME1, NAME2, NAME3 };


  protected:

    bool fIsValid;
    int fIsProtectedInt;

  private:
    int fFeet; // Usually 2 or 4.
    bool fIsPrivateBool;

  }; // moose

  int two_prototypes();
  int two_prototypes();

  class quadruped {
  public:
    quadruped(int a) : fQuadPrivate(a)
    { }

    int fQuadPublic;

  protected:
    int fQuadProtected;

  private:
    int fQuadPrivate;

  };

}


namespace deer {

  class moose : public animal::moose {
  public:
    moose() : fAntlers(false)
    { }

    void setAntlers(bool);
    bool getAntlers();

    void doSomething();

  protected:

    bool fSomeField;

  private:
    bool fAntlers;

  };

} // deer

// A second namespace of the same name will test the
// namespace merging needed to resolve deer::alces
namespace deer {

  class alces : public animal::moose {
  public:
    alces(int lat) : fLatin(lat)
    { }

    void setLatin(bool);
    bool getLatin();

    void doLatinStuff(moose moosein); // for completion testing

    moose createMoose(); // for completion testing.

  protected:
    bool fAlcesBool;
    int fAlcesInt;

  private:
    bool fLatin;
    int fGreek;
  };

};

// A third namespace with classes that does protected and private inheritance.
namespace sneaky {

  class antelope : public animal::quadruped {

  public:
    antelope(int a) : animal::quadruped(),
		      fAntyProtected(a)
    {}

    int fAntyPublic;

    bool testAccess();

  protected:
    int fAntyProtected;

  private :
    int fAntyPrivate;

  };

  class jackalope : protected animal::quadruped {

  public:
    jackalope(int a) : animal::quadruped(),
		       fBunny(a)
    {}

    int fBunnyPublic;

    bool testAccess();

  protected:
    bool fBunnyProtected;

  private :
    bool fBunnyPrivate;

  };

  // Nothing specified means private.
  class bugalope : /* private*/  animal::quadruped {

  public:
    bugalope(int a) : animal::quadruped(),
		       fBug(a)
    {}

    int fBugPublic;

    bool testAccess();
  protected:
    bool fBugProtected;

  private :
    bool fBugPrivate;

  };


};

#endif
