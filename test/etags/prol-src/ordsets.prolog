/* Copyright(C) 1988, Swedish Institute of Computer Science */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   File   : ORDSETS.PL							      %
%   Author : Lena Flood							      %
%   Updated: 9 September 1988						      %
%   Purpose: Ordered set manipulation utilities				      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(ordsets, [
	is_ordset/1,
	list_to_ord_set/2,
	ord_add_element/3,
	ord_del_element/3,
	ord_disjoint/2,
	ord_intersect/2,
	ord_intersection/3,
	ord_intersection/4,
	ord_intersection/2,
	ord_member/2,
	ord_seteq/2,
	ord_setproduct/3,
	ord_subset/2,
	ord_subtract/3,
	ord_symdiff/3,
	ord_union/3,
	ord_union/4,
	ord_union/2
		   ]).

%   Adapted from shared code written by Richard A O'Keefe. 

%   In this package, sets are represented by ordered lists with no
%   duplicates.	 Thus {c,r,a,f,t} would be [a,c,f,r,t].	 The ordering
%   is defined by the @< family of term comparison predicates, which
%   is the ordering used by sort/2 and setof/3.

%   The benefit of the ordered representation is that the elementary
%   set operations can be done in time proportional to the Sum of the
%   argument sizes rather than their Product.  



%   is_ordset(+Set)
%   is true when Set is an ordered set.

is_ordset(X) :- var(X), !, fail.
is_ordset([]).
is_ordset([Head|Tail]) :-
	is_ordset(Tail, Head).

is_ordset(X, _) :- var(X), !, fail.
is_ordset([], _).
is_ordset([Head|Tail], Left) :-
	Left @< Head,
	is_ordset(Tail, Head).


%   list_to_ord_set(+List, ?Set)
%   is true when Set is the ordered representation of the set represented
%   by the unordered representation List.  

list_to_ord_set(List, Set) :-
	sort(List, Set).


%   ord_add_element(+Set1, +Element -Set2)
%   is true when Set2 is Set1 with Element inserted in it, preserving
%   the order.

ord_add_element([], Element, [Element]).
ord_add_element([Head|Tail], Element, Set) :-
	compare(Order, Head, Element),
	ord_add_element(Order, Head, Tail, Element, Set).

ord_add_element(<, Head, Tail, Element, [Head|Set]) :-
	ord_add_element(Tail, Element, Set).
ord_add_element(=, Head, Tail, _, [Head|Tail]).
ord_add_element(>, Head, Tail, Element, [Element,Head|Tail]).


%   ord_del_element(+Set1, +Element, ?Set2)
%   is true when Set2 is Set1 but with Element removed.

ord_del_element([], _, []).
ord_del_element([Head|Tail], Element, Set) :-
	compare(Order, Head, Element),
	ord_del_element(Order, Head, Tail, Element, Set).

ord_del_element(<, Head, Tail, Element, [Head|Set]) :-
	ord_del_element(Tail, Element, Set).
ord_del_element(=, _, Tail, _, Tail).
ord_del_element(>, Head, Tail, _, [Head|Tail]).



%   ord_disjoint(+Set1, +Set2)
%   is true when the two ordered sets have no element in common.  

ord_disjoint(Set1, Set2) :-
	\+ ord_intersect(Set1, Set2).



%   ord_intersect(+Set1, +Set2)
%   is true when the two ordered sets have at least one element in common.

ord_intersect([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_intersect(Order, Head1, Tail1, Head2, Tail2).

ord_intersect(<, _, [Head1|Tail1], Head2, Tail2) :-
	compare(Order, Head1, Head2),
	ord_intersect(Order, Head1, Tail1, Head2, Tail2).
ord_intersect(=, _, _, _, _).
ord_intersect(>, Head1, Tail1, _, [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_intersect(Order, Head1, Tail1, Head2, Tail2).



%   ord_intersection(+Set1, +Set2, ?Intersection)
%   is true when Intersection is the intersecton of Set1
%   and Set2, provided that Set1 and Set2 are ordered sets.

ord_intersection([], _, []).
ord_intersection([Head1|Tail1], Set2, Intersection) :-
	ord_intersection3(Set2, Head1, Tail1, Intersection).

ord_intersection3(<, _, Set1, Head2, Tail2, Intersection) :-
	ord_intersection3(Set1, Head2, Tail2, Intersection).
ord_intersection3(=, Head, Tail1, _, Tail2, [Head|Intersection]) :-
	ord_intersection(Tail1, Tail2, Intersection).
ord_intersection3(>, Head1, Tail1, _, Set2, Intersection) :-
	ord_intersection3(Set2, Head1, Tail1, Intersection).

% could be a disjunction, but is used in three places
ord_intersection3([], _, _, []).
ord_intersection3([Head2|Tail2], Head1, Tail1, Intersection) :-
	compare(Order, Head1, Head2),
	ord_intersection3(Order, Head1, Tail1, Head2, Tail2, Intersection).



%   ord_intersection(+Set1, +Set2, ?Intersection, ?Difference)
%   is true when Intersection is the intersection of Set1 and Set2, 
%   and Differens is Set2 \ Set1 (like in ord_union/4),
%    provided that Set1 and Set2 are ordered sets.

ord_intersection([], Set2, [], Set2).
ord_intersection([Head1|Tail1], Set2, Intersection, Difference) :-
	ord_intersection4(Set2, Head1, Tail1, Intersection, Difference).

ord_intersection4(<, _, Set1, Head2, Tail2, Intersection, Difference) :-
	(   Set1 = [], Intersection = [], Difference = [Head2|Tail2]
	;   Set1 = [Head1|Tail1],
	    compare(Order, Head1, Head2),
	    ord_intersection4(Order, Head1, Tail1, Head2, Tail2, Intersection, Difference)
	).
ord_intersection4(=, Head, Tail1, _, Tail2, [Head|Intersection], Difference) :-
	ord_intersection(Tail1, Tail2, Intersection, Difference).
ord_intersection4(>, Head1, Tail1, Head2, Set2, Intersection, [Head2|Difference]) :-
	ord_intersection4(Set2, Head1, Tail1, Intersection, Difference).

ord_intersection4([], _, _, [], []).
ord_intersection4([Head2|Tail2], Head1, Tail1, Intersection, Difference) :-
	compare(Order, Head1, Head2),
	ord_intersection4(Order, Head1, Tail1, Head2, Tail2, Intersection, Difference).



%   ord_intersection(+Sets, ?Intersection)
%   is true when Intersection is the ordered set representation of the
%   intersection of all the sets in Sets.

ord_intersection(Sets, Intersection) :- 
	length(Sets, NumberOfSets),
	NumberOfSets > 0,
	ord_intersection2(NumberOfSets, Sets, Intersection, []).

ord_intersection2(1, [Set|Sets], Set0, Sets0) :- !,
	Set = Set0,
	Sets = Sets0.
ord_intersection2(2, [Set,Set2|Sets], Intersection, Sets0) :- !,
	Sets = Sets0,
	ord_intersection2(Set, Set2, Intersection).
ord_intersection2(N, Sets0, Intersection, Sets) :-
%	N > 2,
	A is N>>1,
	Z is N-A,
	ord_intersection2(A, Sets0, X, Sets1),
	ord_intersection2(Z, Sets1, Y, Sets),
	ord_intersection(X, Y, Intersection).



%   ord_member(+Elt, +Set)
%   is true when Elt is a member of Set.  Suggested by Mark Johnson.

ord_member(X, [E|Es]) :-
        compare(C, X, E),
        ord_member(C, X, Es).

ord_member(=, _X, _Es).
ord_member(>, X, [E|Es]) :-
        compare(C, X, E),
        ord_member(C, X, Es).



%   ord_seteq(+Set1, +Set2)
%   is true when the two arguments represent the same set.  Since they
%   are assumed to be ordered representations, they must be identical.


ord_seteq(Set1, Set2) :-
	Set1 == Set2.


%   ord_setproduct(+Set1, +Set2, ?SetProduct)
%   is true when SetProduct is the cartesian product of Set1 and Set2. The
%   product is represented as pairs Elem1-Elem2, where Elem1 is an element
%   from Set1 and Elem2 is an element from Set2.

ord_setproduct([], _, []).
ord_setproduct([Head|Tail], Set, SetProduct)  :-
	ord_setproduct(Set, Head, SetProduct, Rest),
	ord_setproduct(Tail, Set, Rest).

ord_setproduct([], _, Set, Set).
ord_setproduct([Head|Tail], X, [X-Head|TailX], Tl) :-
	ord_setproduct(Tail, X, TailX, Tl).



%   ord_subset(+Set1, +Set2)
%   is true when every element of the ordered set Set1 appears in the
%   ordered set Set2.

ord_subset([], _).
ord_subset([Head1|Tail1], [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_subset(Order, Head1, Tail1, Tail2).

ord_subset(=, _, Tail1, Tail2) :-
	ord_subset(Tail1, Tail2).
ord_subset(>, Head1, Tail1, [Head2|Tail2]) :-
	compare(Order, Head1, Head2),
	ord_subset(Order, Head1, Tail1, Tail2).



%   ord_subtract(+Set1, +Set2, ?Difference)
%   is true when Difference contains all and only the elements of Set1
%   which are not also in Set2, i.e. Set1 \ Set2.

ord_subtract(Set1, Set2, Union) :-
	prolog:subtract(Set1, Set2, Union).



%   ord_symdiff(+Set1, +Set2, ?Difference)
%   is true when Difference is the symmetric difference of Set1 and Set2.

ord_symdiff([], Set2, Set2).
ord_symdiff([Head1|Tail1], Set2, Symdiff) :-
	ord_symdiff(Set2, Head1, Tail1, Symdiff).

ord_symdiff(<, Head1, Set1, Head2, Tail2, [Head1|Symdiff]) :-
	ord_symdiff(Set1, Head2, Tail2, Symdiff).
ord_symdiff(=, _, Tail1, _, Tail2, Symdiff) :-
	ord_symdiff(Tail1, Tail2, Symdiff).
ord_symdiff(>, Head1, Tail1, Head2, Set2, [Head2|Symdiff]) :-
	ord_symdiff(Set2, Head1, Tail1, Symdiff).

% could be a disjunction, but is used in three places
ord_symdiff([], Head1, Tail1, [Head1|Tail1]).
ord_symdiff([Head2|Tail2], Head1, Tail1, Symdiff) :-
	compare(Order, Head1, Head2),
	ord_symdiff(Order, Head1, Tail1, Head2, Tail2, Symdiff).



%   ord_union(+Set1, +Set2, ?Union)
%   is true when Union is the union of Set1 and Set2.  Note that when
%   something occurs in both sets, we want to retain only one copy.

ord_union(Set1, Set2, Union) :-
	prolog:merge(Set1, Set2, Union).



%   ord_union(+Set1, +Set2, ?Union, ?New)
%   is true when Union is the union of Set1 and Set2, and New is
%   Set2 \ Set1.  This is useful if you
%   are accumulating members of a set and you want to process new
%   elements as they are added to the set.

ord_union([], Set2, Set2, Set2).
ord_union([Head1|Tail1], Set2, Union, Difference) :-
	ord_union4(Set2, Head1, Tail1, Union, Difference).

ord_union4(<, Head, Set1, Head2, Tail2, [Head|Union], Difference) :-
	(   Set1 = [], Union = [Head2|Tail2], Difference = [Head2|Tail2]
	;   Set1 = [Head1|Tail1],
	    compare(Order, Head1, Head2),
	    ord_union4(Order, Head1, Tail1, Head2, Tail2, Union, Difference)
	).
ord_union4(=, Head, Tail1, _, Tail2, [Head|Union], Difference) :-
	ord_union(Tail1, Tail2, Union, Difference).
ord_union4(>, Head1, Tail1, Head2, Set2, [Head2|Union], [Head2|Difference]) :-
	ord_union4(Set2, Head1, Tail1, Union, Difference).

ord_union4([], Head1, Tail1, [Head1|Tail1], []).
ord_union4([Head2|Tail2], Head1, Tail1, Union, Difference) :-
	compare(Order, Head1, Head2),
	ord_union4(Order, Head1, Tail1, Head2, Tail2, Union, Difference).



%   ord_union(+Sets, ?Union) 
%   is true when Union is the union of all the sets in Sets. 

ord_union([], Union) :- !, Union = [].
ord_union(Sets, Union) :-
	length(Sets, NumberOfSets),
	ord_union_all(NumberOfSets, Sets, Union, []).

ord_union_all(1, [Set|Sets], Set, Sets) :- !.
ord_union_all(2, [Set,Set2|Sets], Union, Sets) :- !,
	ord_union(Set, Set2, Union).
ord_union_all(N, Sets0, Union, Sets) :-
	A is N>>1,
	Z is N-A,
	ord_union_all(A, Sets0, X, Sets1),
	ord_union_all(Z, Sets1, Y, Sets),
	ord_union(X, Y, Union).
