# A Test Makefile. -*-makefile-*-

# This test is for a file in this test directory.  Just a random one.
FILES=testdoub # -1-
# #1# ("testdoublens.cpp" "testdoublens.hpp" )

all: optional

# This one completes on a variable name.
optional: $FIL # -2-
	# #2# ("FILES")
	compile $@

notoptional: opt # -3-
	# #3# ("optional")
	echo "Done."

#end
