package lang;

/***************************************************************************
 * Atoms
 ***************************************************************************/

// keep all atoms
//atom                        6.036 MB (18), 55760 ( 8) (  55760 requested ( 1)),  108 B average

// current implementation with gcAtomTerminator
//atom                        2.780 MB (10), 26881 ( 4) (  78863 requested ( 2)),  103 B average
// ~ 27000 atoms after startup

/**
 * All atoms.
 */
export _set<str>* atoms = null;
//export setP<str>* atoms = null;


/**
 * Atomize string 's'.
 */
export inline str atomize(str s) {
    str a = atoms->get(s);
    if (!a) {
	gcBlock() {
	    // We do not know from what context atomize has been called.
	    // If a string is given as actual argument to a formal atom argument,
	    // atomize will run with the subsequent posiblility of gc.collect()
	    // being called if we do not block. This will free objects which are
	    // being constructed. Bad bad bad ... like German people.
	    *atoms << (a = atomCopy(s));
	}
    }
    return a;
}


#ifdef CM_DEBUG

/**
 * Check atoms.
 */
export void checkAtoms() {
    if (liveGcEnabled) {
	forSet(a, *atoms, str) {
	    if (a && !gc.alive(a)) {
		once() { printf("\n\n"); }
		printf("not live %S %d\n", a, a);

		forList(p, gc.tenured, gcHeader*) {
		    gcClass* c = gcClasses[p->classId];
		    if (c == gc_atom) {
			ptr userP = (byte*)p + sizeof(gcHeader);
			if (eq(str(userP), a)) printf("Found tenured %S %d\n", a, userP);
		    }
		}

		forSet(b, *liveGc, ptr) {
		    if (a == b) printf("Found in liveGc set %d, a %d, b %d, live %d %d\n",
				       b,
				       liveGc->contains(a),
				       liveGc->contains(b),
				       gc.alive(a),
				       gc.alive(b));
		}

		debugBreak();
	    }
	    
	    nat bi = atoms->bucketIndex(a);
	    if (bi != _i) {
		printf("\n  atom bucket index failure %d %d\n", bi, _i);
	    }
	}
    }
}

#endif // CM_DEBUG


/***************************************************************************
 * Atom terminator
 ***************************************************************************/

/**
 * Atom terminator. 
 */
export void gcAtomTerminator(ptr p) {
    //printf("Not called! (disabled in gcTypes.m initGc())\n");

    str z = (str)p;

   #ifdef CM_DEBUG
    int a = atoms->count;
   #endif // CM_DEBUG

    atoms->remove(z);

   #ifdef CM_DEBUG
    if (liveGcEnabled) {
	if (a - atoms->count != 1) {
	    forSet(x, *atoms, str) {
		if (eq(x, z)) {
		    printf("   gcAtomTerminator: found %S, z %d, x %d\n", x,
			   atoms->contains(z),
			   atoms->contains(x));
		    printf("  map array index %d, bucket index %d\n", _i,
			   atoms->bucketIndex(x));
		    printf("  map array index hash %d, bi %d\n", hash(x),
			   (hash(x))&atoms->capacityMinus1);;
		}
	    }
	    debugBreak();
	}
    }
   #endif // CM_DEBUG
}


/***************************************************************************
 * Atom class
 ***************************************************************************/

/**
 * Class atom for shared strings.
 */
class atom {
public:
    /**
     * String.
     */
    str s;

    /**
     * Return a new atom.
     */
    constructor inline atom()
      : s(null) {}
    

    /**
     * Return a new atom.
     */
    constructor inline atom(str s)
      : s(atomize(s)) {}
    

    /**
     * Return a new atom.
     */
    constructor inline atom(const atom& w)
      : s(w.s) {}
    

    /**
     * Return a new atom.
     */
    constructor inline atom(byteStream& b) {
	gcBlock() {
	    str a;
	    b >> a;
	    s = atomize(a);
	}
    }
    

    /**
     * Implicit conversion to str.
     */
    final operator str() const { return s; }


    /**
     * Prevent the use of the new operator for this by-value class.
     */
    final void* operator new(size_t s) {
	bug; return null;
    }


    /**
     * Convert to String.
     */
    final inline str toS() const {
	return s;
    }
}


/**
 * Append to strBuf.
 */
export inline strBuf& operator<<(strBuf& z, const atom& v) {
    z << v.s;
    return z;
}


/**
 * Append a atom to byteStream.
 */
export inline byteStream& operator<<(byteStream& b, const atom& z) {
    b << z.s;
    return b;
}


/**
 * Get a atom from byteStream.
 */
export byteStream& operator>>(byteStream& b, atom& z) {
    gcBlock() {
	b >> z.s;
	//if (!gc.alive(z.s)) printf("read from bs (%S) %d\n", z.s, gc.alive(z.s));
	//str s = atoms->get(z.s);
	z.s = atomize(z.s);
	//if (!gc.alive(z.s)) printf(">> A read from bs (%S) %d\n", s, gc.alive(s));
	//if (!gc.alive(z.s)) printf(">> B read from bs (%S) %d\n", z.s, gc.alive(z.s));
    }

    return b;
}


/**
 * Return the hash value of a atom.
 */
export inline int hash(const atom& z) {
    // gives much better distribution than the easy one int(z.s)
    return int(z.s) >> 2;
}


/**
 * Return true if 'a' == 'b'.
 */
export inline bool eq(const atom& a, const atom& b) {
    return (ptr(a.s) == ptr(b.s));
}


/**
 * Return 0 if a and b are equal,
 * -1 if a < b,
 *  1 if a > b.
 */
export inline int compare(const atom& a, const atom& b) {
    return compare(a.s, b.s);
}


/**
 * Clear the atom.
 */
export inline void clear(atom& a) {
    a.s = null;
}


/***************************************************************************
 * Some atoms
 ***************************************************************************/

/**
 * Atom callback.
 */
export atom callbackA;


/**
 * Atom close.
 */
export atom closeA;


/**
 * Atom finalize.
 */
export atom finalizeA;


/**
 * Atom last.
 */
export atom lastA;


/**
 * Atom deepCopy.
 */
export atom deepCopyA;


/**
 * Atom shallowCopy.
 */
export atom shallowCopyA;


/**
 * Atom parent.
 */
export atom parentA;


/**
 * Atom super.
 */
export atom superA;

/**
 * Atom this.
 */
export atom thisA;


/**
 * Init atoms.
 */
export void initAtoms() {
    atoms = new _set<str>(16380);

    callbackA = atom(T("callback"));
    gcGlobal(callbackA);

    closeA = atom(T("close"));
    gcGlobal(closeA);

    finalizeA = atom(T("finalize"));
    gcGlobal(finalizeA);

    lastA = atom(T("last"));
    gcGlobal(lastA);

    deepCopyA = atom(T("deepCopy"));
    gcGlobal(deepCopyA);

    shallowCopyA = atom(T("shallowCopy"));
    gcGlobal(shallowCopyA);

    parentA = atom(T("parent"));
    gcGlobal(parentA);

    superA = atom(T("super"));
    gcGlobal(superA);

    thisA = atom(T("this"));
    gcGlobal(thisA);
}
