//  testjavacomp.java --- Semantic unit test for Java

// Copyright (C) 2009-2020 Free Software Foundation, Inc.

//  Author: Eric M. Ludlam <zappo@gnu.org>

//  This file is part of GNU Emacs.

//  GNU Emacs is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.

//  GNU Emacs is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.

//  You should have received a copy of the GNU General Public License
//  along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

package tests.testjavacomp;

class secondClass {
    private void scFuncOne() {    }
    public void scFuncOne() {    }

    int package_protected_field;
    public int public_protected_field;
    private int private_protected_field;
}


public class testjavacomp {

    private int funcOne() {    }
    private int funcTwo() {    }
    private char funcThree() {    }

    class nestedClass {
	private void ncFuncOne() {	}
	public void ncFuncOne() {	}
    }

    public void publicFunc() {

	int i;

	i = fu// -1-
	    // #1# ( "funcOne" "funcTwo" )
	    ;

	fu// -2-
	    // #2# ( "funcOne" "funcThree" "funcTwo" )
	    ;

	secondClass SC;

	SC.s//-3-
	    // #3# ( "scFuncOne" )
	    ;

	// @TODO - to make this test complete, we need an import
	//         with a package protected field that is excluded
	//         from the completion list.
	SC.p//-4-
	    // #4# ( "package_protected_field" "public_protected_field" )

	nestedClass NC;

	// @todo - need to fix this?  I don't know if  this is legal java.
	NC.// - 5-
	    // #5# ( "ncFuncOne" )
	    ;
    }

} // testjavacomp
