% $Id: natded.pl,v 1.7 2001/04/26 12:22:56 geertk Exp geertk $
% NATURAL DEDUCTION CG PARSER WITH SEMANTICS
% ========================================================================= 
% Bob CARPENTER
% Computational Linguistics Program, Department of Philosophy
% Carnegie Mellon University, Pittsburgh, PA  15213
% Net: carp+@cmu.edu  
% Voice: (412) 268-8043      Fax: (412) 268-1440

% Copyright 1995, Bob Carpenter

% Written: 12 March 1993
% Revised: 4 February 1994
% Further Revised: 2 May 1994
% Revised for CGI: 16 November 1995
% Revised for Lambek notation: ? Novemeber 1995
% Revised again: 30 November 1995


% Library Includes
% ========================================================================= 

:- use_module(library(system)).
% :- use_module(library(random)).


% Data Types
% ========================================================================= 

% <lambda_term> ::=  <lambda_var>
%                 |  <lambda_con>
%                 |  <lambda_term>@<lambda_term>
%                 |  <lambda_var>^<lambda_term>

% <lambda_var> ::= var(<prolog_var>)

% <lambda_con> ::= con(<prolog_atom>)

% <tree> ::= tree(<rule>,<cat>,<list(<tree>)>)
%          | ass(<syn>,<var>,<index>)
%          | leaf(<word>)

% <rule> ::= <prolog_atom>

% <cat> ::= <syn> : <lambda_term>

% <syn> ::= <basic_syn>
%         | <syn> / <syn>  |  <syn> \ <syn> 
%         | scop(<syn>,<syn>) 
%         | <syn> - <syn>

% <basic_syn> ::= bas(<prolog_term>)

% <grammar> ::= <sequence(<lex_entry>)> 
%               <sequence(<empty_category>)>
%               <sequence(<grammar_rule>)>

% <lex_entry> ::=  <word> ==> <cat>.

% <empty_category> ::= empty <cat>.

% <grammar_rule> ::= <cat> ===> <list(<cat>)> if <prolog_goal>.

% <index> ::= <integer>

% <word> ::= <prolog_atom>

% <chart_edge> ::= edge(<int>, <int>, <cat>)

% Operator Declarations
% ========================================================================= 

  :-op(150,yfx,@).      % function application
% :-op(200,xfy,^).      % lambda abstraction
% :-op(400,yfx,/).      % forward slash
  :-op(350,yfx,\).      % backward slash
  :-op(500,xfx,:).      % category constructor
  :-op(600,xfx,==>).    % lexical rewriting
  :-op(600,xfx,===>).   % grammar rule
  :-op(600,fx,empty).   % empty categories
  :- op(600,xfx,macro). % lexical macros
  :- op(600,xfx,means). % meaning postulates
  :-op(1200,xfx,if).    % conditions on rule schemes

:- dynamic edge/3.
:- dynamic emptyedge/1.
:- dynamic active/3.




% Lambda Calculus
% ========================================================================= 

% expandmng(+M:<term>, -MExp:<term>)
% ----------------------------------------------------------------------
% MExp is the result of recursively replacing constants with their
% definitions in M; disallows non-determinism
% ----------------------------------------------------------------------
expandmng(var(V),var(V)).
expandmng(con(C),MExp):-
  con(C) means M, !,
  expandmng(M,MExp).
expandmng(con(C),con(C)).
expandmng(V^M,V^MExp):-
  expandmng(M,MExp).
expandmng(M@N,MExp@NExp):-
  expandmng(M,MExp),
  expandmng(N,NExp).


% normalize(+M:<term>, -MNorm:<term>)
% ----------------------------------------------------------------------
% MNorm is the normal form of M; all bound variables renamed
% ----------------------------------------------------------------------
normalize(M,MNorm):-
  fresh_vars(M,MFr),
  normalize_fresh(MFr,MNorm).

% fresh_vars(+M:<term>, -MFr:<term>)
% ----------------------------------------------------------------------
% MFr is the result of renaming all bound variables
% in M to fresh instances, using alpha-reduction
% ----------------------------------------------------------------------
fresh_vars(var(V),var(V)).
fresh_vars(con(C),con(C)).
fresh_vars(M@N,MFr@NFr):-
  fresh_vars(M,MFr),
  fresh_vars(N,NFr).
fresh_vars(X^M,var(Y)^MFr):-
  subst(M,X,var(Y),M2),
  fresh_vars(M2,MFr).

% substitute(+M:<term>, +X:<var>, +N:<term>, -L:<term>)
% ----------------------------------------------------------------------
% L = M[X |--> N]
% ----------------------------------------------------------------------
subst(var(Y),var(X),M,N):-
  ( X == Y
    -> N=M
  ; N = var(Y)
  ).
subst(con(C),_,_,con(C)).
subst(M@L,X,N,M2@L2):-
  subst(M,X,N,M2),
  subst(L,X,N,L2).
subst(Y^M,X,N,Y^M2):-
  ( Y == X
    -> M2 = M
  ; subst(M,X,N,M2)
  ).

% normalize_fresh(+M:<term>, -N:<term>)
% ----------------------------------------------------------------------
% M is normalized to N 
% -- all bound variables are made fresh
% -- cut corresponds to leftmost normalization
% ----------------------------------------------------------------------
normalize_fresh(M,N):-
  reduce_subterm(M,L),
  !, normalize_fresh(L,N).
normalize_fresh(M,M).

% reduce_subterm(+M:<term>, -N:<term>)
% ----------------------------------------------------------------------
% N is the result of performing one beta- or 
% eta-reduction on some subterm of M;
% -- reduces leftmost subterm first, but provides 
%    all reductions on backtracking
% ----------------------------------------------------------------------
reduce_subterm(M,M2):-
  reduce(M,M2).
reduce_subterm(M@N,M2@N):-
  reduce_subterm(M,M2).
reduce_subterm(M@N,M@N2):-
  reduce_subterm(N,N2).
reduce_subterm(X^M,X^N):-
  reduce_subterm(M,N).

% reduce(+M:<term>, -N:<term>)
% ----------------------------------------------------------------------
% reduces M to N using beta- or eta-reduction
% -- assumes no variable clashes
% ----------------------------------------------------------------------
reduce((X^M)@N,L):-     % beta reduction
  subst(M,X,N,L).
reduce(X^(M@Y),M):-     % eta reduction
  X == Y,
  \+ ( free_var(M,Z),
       Z == X ).

% free_var(+M:<term>, -X:<var>)
% ----------------------------------------------------------------------
% X is free in M
% ----------------------------------------------------------------------
free_var(var(V),var(V)).
free_var(M@N,X):-
  ( free_var(M,X)
  ; free_var(N,X)
  ).
free_var(X^M,Y):-
  free_var(M,Y),
  Y \== X.

% free_for(+N:<term>, +X:<var>, +M:<term>)
% ----------------------------------------------------------------------
% M is free for X in N
% ----------------------------------------------------------------------
free_for(var(_),_,_).
free_for(con(_),_,_).
free_for(L@K,X,M):-
  free_for(L,X,M),
  free_for(K,X,M).
free_for(Y^L,X,M):-
  free_for(L,X,M),
  ( \+ free_var(L,X)
  ; \+ free_var(M,Y)
  ).


% Right-Left, Bottom-Up Dynamic Chart Parser (after ALE)
% =========================================================================

% Lexical Compiler
% ----------------------------------------------------------------------

% compile_lex(+File:<file>)
% ----------------------------------------------------------------------
% compiles lexical entries into file 
% ----------------------------------------------------------------------
compile_lex(File):-
  tell(File),
  write('% Lexical Entries'), nl,
  write('% ---------------'), nl, nl,
  lex(W,Syn,Sem),
  numbervars(lexentry(W,Syn,Sem),0,_),
  write('lexentry(\''), write(W), write('\','),
  write(Syn),write(','), write(Sem), write(').'), nl,
  fail.
compile_lex(File):-
  told,
  compile(File).

% consult_lex  
% ----------------------------------------------------------------------
% consults lexicon in place
% ----------------------------------------------------------------------
consult_lex:-
  retractall(lexentry(_,_,_)),
  lex(W,Syn,Sem),
  assert(lexentry(W,Syn,Sem)),
  fail.
consult_lex.

% lex(?W:<word>, ?Syn:<syn>, ?Sem:<lambda_term>)
% ----------------------------------------------------------------------
% word W has syntactic category Syn and smenantic term Sem
% ----------------------------------------------------------------------
lex(W,SynOut,Sem):-
  W ==> Syn : Sem,
  expandsyn(Syn,SynOut).

% expandsyn(+SynIn:<syn>, ?SynOut:<syn>)
% ----------------------------------------------------------------------
% the category SynIn is macro expanded recursively to SynOut 
% ----------------------------------------------------------------------
expandsyn(Syn,Syn):-
  var(Syn), !.
expandsyn(SynIn,SynOut):-
  macro(SynIn,SynMid),   % cut means unique macro expansion
  !, expandsyn(SynMid,SynOut).
expandsyn(Syn1/Syn2,Syn1Out/Syn2Out):-
  !, expandsyn(Syn1,Syn1Out), 
  expandsyn(Syn2,Syn2Out).
expandsyn(Syn1\Syn2,Syn1Out\Syn2Out):-
  !, expandsyn(Syn1,Syn1Out), 
  expandsyn(Syn2,Syn2Out).
expandsyn(Syn1-Syn2,Syn1Out-Syn2Out):-
  !, expandsyn(Syn1,Syn1Out), 
  expandsyn(Syn2,Syn2Out).
expandsyn(q(Syn1,Syn2,Syn3),q(Syn1Out,Syn2Out,Syn3Out)):-
  !, expandsyn(Syn1,Syn1Out), 
  expandsyn(Syn2,Syn2Out),
  expandsyn(Syn3,Syn3Out).
expandsyn(Syn,Syn):-
  bas_syn(Syn).

% bas_syn(?Syn:<syn>)
% ----------------------------------------------------------------------
% Syn is a basic syntactic category
% ----------------------------------------------------------------------
bas_syn(n(_)).
bas_syn(np(_,_)).
bas_syn(s(_)).
bas_syn(coor).
bas_syn(sc(_)).
bas_syn(ex(_)).




% Empty Edge Compilation
% ----------------------------------------------------------------------

% compile_empty
% ----------------------------------------------------------------------
% compiles empty categories, asserting all active and inactive edges
% they can produce by themselves; always succeeds
% ----------------------------------------------------------------------
compile_empty:-
  retractall(emptyedge(_)), retractall(active(_,_,_)),
  empty SynIn:Sem,
  expandsyn(SynIn,Syn),
  complete(cat(Syn,Sem,[],[],empty(Syn,Sem))).
compile_empty:-
  bagof(C,emptyedge(C),Cs),
  length(Cs,N),  
  nl, write(N), write(' complete empty edges'), nl,
  bagof(D-Ds,G^active(Ds,D,G),Es),
  length(Es,M),  
  write(M), write(' active rules with empty starts'), nl.

% complete_cat(Cat:+<cat>)
% ----------------------------------------------------------------------
% Cat is asserted as empty, and all current active edges are tested to
% see if Cat can extend them; fails for looping
% ----------------------------------------------------------------------
complete(Cat):-
  assert(emptyedge(Cat)),
  ( (CatM ===> [Cat|Cats] if Goal)
  ; active(CatM,[Cat|Cats],Goal)
  ),
  add_active(Cats,CatM,Goal).

% add_active(Cats:+<list(<cat>)>, +Cat:<cat>, +Goal:<goal>)
% ----------------------------------------------------------------------
% the active edge Cat --> . Cats is asserted, and any extensions
% computed and themselves asserted;  fails for looping
% ----------------------------------------------------------------------
add_active([],Cat,Goal):-
  call(Goal),
  assert(emptyedge(Cat)),
  complete(Cat).
add_active([Cat|Cats],CatM,Goal):-
  assert(active([Cat|Cats],CatM,Goal)),
  emptyedge(Cat),
  add_active(Cats,CatM,Goal).

% parse(Ws:+<list(<word>)>, Cat:?<cat>)
% ----------------------------------------------------------------------
% Cat can be derived from Ws
% ----------------------------------------------------------------------
parse(Ws,Cat):-
  derived_analyses(Ws,WsMid),
  retractall(edge(_,_,_)),
  reverse(WsMid,[],WsRev),
  build(WsRev,0,Length),
  edge(Length,0,Cat).

% derived_analyses(WsIn:+<list(<word>)>, WsOut:-<list(<word>)>)
% ----------------------------------------------------------------------
% computes subderivations of WsIn
% ----------------------------------------------------------------------
derived_analyses([],[]).
derived_analyses([der(Ws)|Ws2],[der(Ws,Ass,Syn,Sem)|DerWs2]):-
  !, parse(Ws,cat(Syn,Sem,Ass,[],_)),
  \+ member(abs(_,_,_),Ass),
  derived_analyses(Ws2,DerWs2).
derived_analyses([W|Ws],[W|DerWs]):-
  derived_analyses(Ws,DerWs).

% build(Ws:+<list(<word>)>, Right:+<int>, Left:-<int>)
% ----------------------------------------------------------------------
% finishes building chart with Ws as remaing word, starting from
% right position Right and finishing on left position Left
%      -- counts backwards, so Left > Right
% ----------------------------------------------------------------------
build([],Left,Left).
build([W|Ws],Right,FinalLeft):-
  RightPlus1 is Right+1,
  ( buildact(W,Right,RightPlus1)
  ; build(Ws,RightPlus1,FinalLeft)
  ).

% build_act(+W:<inputword>, +Left:<int>, +Right:<int>)
% ----------------------------------------------------------------------
% take action basedon whether input W is:
%       [SynCat]     assume hypothetical category with syntax SynCat
%       der(WsSub,Ass,Syn,Sem)  add derived result
%       W            treat as input word
% ----------------------------------------------------------------------
buildact([SynIn],Right,RightPlus1):-
  mapsyn(SynIn,Syn),   % add unspecified features
  !,     add_edge(RightPlus1,Right,cat(Syn,var(X),[abs(Syn,var(X),N)],[],
                                  ass(Syn,var(X),N))).
buildact(der(WsSub,Ass,Syn,Sem),Right,RightPlus1):-
  !,  add_edge(RightPlus1,Right,cat(Syn,Sem,Ass,[],
                                  tree(der,Syn:Sem,[ders(WsSub)]))).
buildact(W,Right,RightPlus1):-
  lexentry(W,Syn,Sem),
  add_edge(RightPlus1,Right,cat(Syn,Sem,[l],[],tree(lex,Syn:Sem,[leaf(W)]))).
buildact(W,_,_):-
    \+ (W ==> _), 
    nl, write('Input not recognized: '), write(W), write('<br>').

% mapsyn(+SynCat:<syncat>, -SynCatOut:<syncat)
% ----------------------------------------------------------------------
% SynCatOut is result of adding default features to subcategories of
% SynCat if any are missing;  allows [SynCat] to specify cats without 
% features for input;  ones with features will be passed along
% ----------------------------------------------------------------------
mapsyn(A/B,AM/BM):-
  mapsyn(A,AM), mapsyn(B,BM).
mapsyn(A\B,AM\BM):-
  mapsyn(A,AM), mapsyn(B,BM).
mapsyn(A-B,AM-BM):-
  mapsyn(A,AM), mapsyn(B,BM).
mapsyn(scop(A,B),scop(AM,BM)):-
  mapsyn(A,AM), mapsyn(B,BM).
mapsyn(q(A,B,C),q(AM,BM,CM)):-
  mapsyn(A,AM), mapsyn(B,BM), mapsyn(C,CM).
mapsyn(s,s(_)).
mapsyn(n,n(ind(sng))).
mapsyn(np,np(ind(sng),nm(_))).
mapsyn(np(X,Y),np(X,Y)).
mapsyn(n(X),n(X)).
mapsyn(s(X),s(X)).

% add_edge(Left:+<int>, Right:+<int>, Cat:+<cat>)
% ----------------------------------------------------------------------
% asserts edge into chart and then tries to extend it in all possible ways
%     -- always fails to force backgracking
% ----------------------------------------------------------------------
add_edge(Left,Right,Cat):-
  asserta(edge(Left,Right,Cat)),
  ( (MotherCat ===> [Cat|Cats] if Goal)
  ; active([Cat|Cats],MotherCat,Goal)
  ),
  findcats(Cats,Right,NewRight),
  call(Goal),
  add_edge(Left,NewRight,MotherCat).

% findcats(Left:+<int>, Cats:+<cats>, Right:-<int>)
% ----------------------------------------------------------------------
% Cats is a list of categories spanning Left to Right
% ----------------------------------------------------------------------
findcats([],Left,Left).
findcats([Cat|Cats],Left,Right):-
  ( edge(Left,Mid,Cat),
    findcats(Cats,Mid,Right)
  ; emptyedge(Cat),
    findcats(Cats,Left,Right)
  ).

% edge(Left:?<nat>, Right:?<nat>, Cat:?<cat>)                     (dynamic)
% ----------------------------------------------------------------------
% There is an edge with category Cat from Left to Right;
% ----------------------------------------------------------------------

% normalize_tree(+TreeIn:<tree>, -TreeOut:<tree>)
% ----------------------------------------------------------------------
% TreeOut is isomorphic to TreeIn, with normalized semantics at
% every node
% ----------------------------------------------------------------------
normalize_tree(tree(Rule,Syn:Sem,Trees),
               tree(Rule,Syn:SemNorm,TreesNorm)):-
  normalize_fresh(Sem,SemNorm),
  normalize_trees(Trees,TreesNorm).
normalize_tree(ass(Syn,Var,Index),ass(Syn,Var,Index)).
normalize_tree(leaf(Word),leaf(Word)).
normalize_tree(ders(Word),ders(Word)).
normalize_tree(empty(Syn,Sem),empty(Syn,SemNorm)):-
  normalize_fresh(Sem,SemNorm).

normalize_trees([],[]).
normalize_trees([T|Ts],[TNorm|TsNorm]):-
  normalize_tree(T,TNorm),
  normalize_trees(Ts,TsNorm).


% expandmng_tree(+TreeIn:<tree>, -TreeOut:<tree>)
% ----------------------------------------------------------------------
% TreeOut is isomorphic to TreeIn, with expanded semantics
% every node
% ----------------------------------------------------------------------
expandmng_tree(tree(Rule,Syn:Sem,Trees),
               tree(Rule,Syn:SemNorm,TreesNorm)):-
  expandmng(Sem,SemNorm),
  expandmng_trees(Trees,TreesNorm).
expandmng_tree(ass(Syn,Var,Index),ass(Syn,Var,Index)).
expandmng_tree(leaf(Word),leaf(Word)).
expandmng_tree(ders(Word),ders(Word)).
expandmng_tree(empty(Syn,Sem),empty(Syn,SemNorm)):-
  expandmng(Sem,SemNorm).

expandmng_trees([],[]).
expandmng_trees([T|Ts],[TExp|TsExp]):-
  expandmng_tree(T,TExp),
  expandmng_trees(Ts,TsExp).


% Grammar Rules
% ========================================================================= 

% C:<-cat> ===> Cs:<+list(<cat>)>
% ----------------------------------------------------------------------
% C can be composed of Cs; may be conditions

% / elimination
% -------------
cat(A, Alpha@Beta, Ass3, Qs3, tree(fe,A:Alpha@Beta,[T1,T2]))
===>
[ cat(A/B, Alpha, Ass1, Qs1, T1),
  cat(B, Beta, Ass2, Qs2, T2)
] if
     append(Ass1,Ass2,Ass3),
     append(Qs1,Qs2,Qs3).

% \ elimination
% -------------
cat(A, Alpha@Beta, Ass3, Qs3, tree(be,A:Alpha@Beta,[T1,T2]))
===>
[ cat(B, Beta, Ass1, Qs1, T1),
  cat(B\A, Alpha, Ass2, Qs2, T2)
] if
     append(Ass1,Ass2,Ass3),
     append(Qs1,Qs2,Qs3).

% \ introduction
% --------------
cat(B\A, X^Alpha, Ass, Qs, tree(bi(N),B\A:X^Alpha,[T1]))
===>
[  cat(A, Alpha, [abs(B,X,N)|Ass], Qs, T1)
] if
    \+ T1 = tree(be,_,[_,ass(_,_,N)]),   % normal
    at_least_one_member(l,Ass),  % non-empty condition
     \+ ( subtree(tree(AssumeM,_,Ts),T1),      % properly nested
          member(TMid,Ts), 
          subtree(ass(_,_,'$VAR'(J)),TMid), 
          J == N,
          hypothetical_mem(AssumeM,Ass,Qs) ).

% / introduction
% --------------
cat(A/B, X^Alpha, Ass2, Qs, tree(fi(N),A/B:X^Alpha,[T1]))
===>
[ cat(A,Alpha,Ass1,Qs,T1)
] if
     \+ T1 = tree(fe,_,[_,ass(_,_,N)]),    % normal
     at_least_one_member(l,Ass1), % non-empty condition
     select_last(Ass1,abs(B,X,N),Ass2),
     \+ ( subtree(tree(AssumeM,_,Ts),T1),   % properly nested
          member(TMid,Ts), 
          subtree(ass(_,_,'$VAR'(J)),TMid), 
          J == N,
          hypothetical_mem(AssumeM,Ass1,Qs) ).

% - introduction
% --------------
cat(A-B, X^Alpha, Ass2, Qs, tree(gi(N),(A-B):X^Alpha,[T1]))
===>
[ cat(A, Alpha, Ass1, Qs, T1)
] if
     at_least_one_member(l,Ass1),  % non-empty condition
     select(abs(B,X,N),Ass1,Ass2),
     \+ ( subtree(tree(AssumeM,_,Ts),T1),  % normalized?
          member(TMid,Ts), 
          subtree(ass(_,_,'$VAR'(J)),TMid), 
          J == N,
          hypothetical_mem(AssumeM,Ass1,Qs) ).


% q quantifier pushing (q-elimination part 1)
% ----------------------------------------------------------------------
cat(C, var(X), Ass, [gq(B,A,Q,var(X),N)|Qs],
          tree(qqpush(N),C:var(X),[T1]))
===>
[ cat(q(C,B,A), Q, Ass, Qs, T1)
] if
  \+ T1 = tree(qqi,_,_).     % normal

% q quantifier popping (q-elimination part 2)
% ----------------------------------------------------------------------
cat(A, Q@(X^Alpha), Ass, Qs2, tree(qqpop(N),A:Q@(X^Alpha),[T1]))
===>
[ cat(B,Alpha,Ass,Qs1,T1)
] if
     select(gq(B,A,Q,X,N),Qs1,Qs2),
     \+ ( subtree(tree(AssumeM,_,Ts),T1),
          member(TMid,Ts), 
          subtree(tree(qqpush(J),_,_),TMid), 
          J == N,
          hypothetical_mem(AssumeM,Ass,Qs1) ).

% q quantifier introduction [restricted to q(np,s,s)]
% ----------------------------------------------------------------------
% restricted to A = s(_), B=np case for termination
cat(q(np(ind(Num),Case),s(VF),s(VF)), var(P)^(var(P)@Alpha), Ass, Qs1,
    tree(qqi,q(np(ind(Num),Case),s(VF),s(VF)):var(P)^var(P)@Alpha,[T1]))
===>
[ cat(np(ind(Num),Case),Alpha,Ass,Qs1,T1) 
] if 
  true.

% coordination elimination
% ----------------------------------------------------------------------
cat(C, Sem, [], [], tree(coel,C:Sem,[T1,T2,T3]))
===>
[ cat(C, Sem1, Ass1, [], T1),
  cat(coor, Alpha, Ass2, [],T2),
  cat(C, Sem2, Ass3, [], T3)
] if
     \+ member(abs(_,_,_),Ass1),        % coordination condition
     \+ member(abs(_,_,_),Ass2),
     \+ member(abs(_,_,_),Ass3),
     \+ T1 = tree(coel,_,_),
     \+ T2 = tree(coel,_,_),
     make_coor(C,Alpha,Sem1,Sem2,Sem).

% non-boolean coordination
% ----------------------------------------------------------------------
%cat(np(pl,-), con(union)@Alpha1P@Alpha3P, [], [],
%    tree(nbc,np(pl,-):con(union)@Alpha1P@Alpha3P,[T1,T2,T3]))
%===>
%[ cat(NP1, Alpha1, Ass1, [], T1),
%  cat(coor, nbc, Ass2, [],T2),
%  cat(NP3, Alpha3, Ass3, [], T3)
% ]:-  
%      \+ member(abs(_,_,_),Ass1),        % coordination condition
%      \+ member(abs(_,_,_),Ass2),
%      \+ member(abs(_,_,_),Ass3),
%      make_nb_coor(NP1,Alpha1,Alpha1P),
%      make_nb_coor(NP3,Alpha3,Alpha3P).
% 
% make_nb_coor(np,Alpha,con(singleton)@Alpha).
% make_nb_coor(np(pl,+),Alpha,con(singleton)@Alpha).
% make_nb_coor(np(pl,-),Alpha,Alpha).


% subtree(-TSub:<tree>, +T:<tree>)
% ----------------------------------------------------------------------
% TSub is a subtree of T
% ----------------------------------------------------------------------
subtree(T,T).
subtree(T,tree(_,_,Ts)):-
  member(T2,Ts),
  subtree(T,T2).

% hypothetical_mem(Rule,Assumptions,Qs)
% ----------------------------------------------------------------------
% Rule is a member of the assumptions
% ----------------------------------------------------------------------
hypothetical_mem(fi(N),Ass,_):-
  member(abs(_,_,M),Ass), N == M.
hypothetical_mem(bi(N),Ass,_):-
  member(abs(_,_,M),Ass), N == M.
hypothetical_mem(gi(N),Ass,_):-
  member(abs(_,_,M),Ass), N == M.
hypothetical_mem(qqpush(N),_,Qs):-
  member(gq(_,_,_,_,M),Qs), N == M.

% make_coor(Cat,CoorSem,Sem1,Sem2,SemOut)
% ----------------------------------------------------------------------
% generalized coordination semantics CoorSem is applied to
% Sem1 and Sem2 of type Cat, with result SemOut
% ----------------------------------------------------------------------
make_coor(s(_),Alpha,Sem1,Sem2,Alpha@Sem1@Sem2).
make_coor(n(_),Alpha,Sem1,Sem2,var(X)^Alpha@(Sem1@var(X))@(Sem2@var(X))).
make_coor(A/_,Alpha,Sem1,Sem2,var(X)^Sem):-
  make_coor(A,Alpha,Sem1@var(X),Sem2@var(X),Sem).
make_coor(_\A,Alpha,Sem1,Sem2,var(X)^Sem):-
  make_coor(A,Alpha,Sem1@var(X),Sem2@var(X),Sem).
make_coor(A-_,Alpha,Sem1,Sem2,var(X)^Sem):-
  make_coor(A,Alpha,Sem1@var(X),Sem2@var(X),Sem).
make_coor(q(_,_,A),Alpha,Sem1,Sem2,var(X)^Sem):-
  make_coor(A,Alpha,Sem1@var(X),Sem2@var(X),Sem).


% General CGI Handling
% ========================================================================= 

% start_up
% ----------------------------------------------------------------------
% executed when saved state is restarted;
% tokenizes, parses and sends off input for handling;
% halts on termination
% ----------------------------------------------------------------------
start_up:-
%    getenv('QUERY_STRING', Arg),
    prolog_flag(argv,[Arg]),

                            %           write('<p>'), write(Arg), nl, ttyflush,
  ( tokenizeatom(Arg,TokenList)
        %                               ,write('<p>'), write(TokenList), ttyflush
  ; write('Input '), write(Arg), write(' could not be tokenized'), ttyflush, halt
  ),
  ( parse_cgi(TokenList,KeyVals)
         %                              , write('<p>'), write(KeyVals), ttyflush
  ; write('Tokens '), write(TokenList), write(' could not be parsed'), halt
  ),
  ( action(KeyVals)
  ; told, write('Action '), write(KeyVals), write(' could not be executed')
  ),
  halt.

% tokenizeatom(+Input:<atom>, -Tokens:<list(<token>)>)  
% ----------------------------------------------------------------------
% breaks input Input into list of tokens;  
% ----------------------------------------------------------------------
tokenizeatom(Atom,Ws):-
  name(Atom,Cs),
  tokenize(Cs,Xs-Xs,Ws).
 
% tokenize(+Chars:<list(<char>)>, +CharsSoFar:<d_list(<char>)>,
%          -Tokens:<list(<token>)>)
% ----------------------------------------------------------------------
% Tokens is the list of tokens retrieved from Chars; ChrsSoFar 
% accumulates prefixes of atoms being recognized
% ----------------------------------------------------------------------
tokenize([C1,C2,C3|Cs],Xs-Ys,TsResult):-     % special symbol
  name('%',[C1]),
  specialsymbol(C2,C3,SpecialSymbol),
  !, 
  ( Xs = []
    -> TsResult = [SpecialSymbol|TsOut]
  ; Ys = [],
    name(CsAtom,Xs),
    TsResult = [CsAtom,SpecialSymbol|TsOut]
  ), 
  tokenize(Cs,Zs-Zs,TsOut).
tokenize([C|Cs],Xs-Ys,TsResult):-           % one-character operator
  isoperator(C),
  !, name(OpToken,[C]),
  ( Xs = []
    -> TsResult = [OpToken|Ts]
  ; Ys = [],
    name(CsAtom,Xs),
    TsResult = [CsAtom,OpToken|Ts]
  ),
  tokenize(Cs,Zs-Zs,Ts).
tokenize([C|Cs],Xs-[C|Ys],Ts):-             % more of string
  tokenize(Cs,Xs-Ys,Ts).
tokenize([],Xs-_,[]):-                      % no more input; nothing accum.
  Xs = [], !.
tokenize([],Xs-[],[CsAtom]):-               % no more input; stringg accum.
  name(CsAtom,Xs).

% isoperator(+Char:<char>)
% ----------------------------------------------------------------------
% Char is the name of an operator character
% ----------------------------------------------------------------------
isoperator(Char):-
  name(Op,[Char]),
  isoptab(Op).

isoptab('%').
isoptab('+').
isoptab('&').
isoptab('=').

% specialsymbol(+C1:<char>, +C2:<char>, -S:<token>)
% ----------------------------------------------------------------------
% C1 and C2 are the names of characters completing a % special symbol
% ----------------------------------------------------------------------
specialsymbol(C1,C2,S):-
  name(N1,[C1]), name(N2,[C2]),
  ( sstab(N1,N2,S), !
  ; S = spec(N1,N2)
  ).

sstab(2,'C',',').
sstab(2,'F','/').
sstab(2,8,'(').
sstab(2,9,')').
sstab(5,'B','[').
sstab(5,'C','\\').
sstab(5,'D',']').
sstab(3,'D','=').
sstab(3,'E','>').


% parse_cgi(+TokenList:<list(<token>)>, -KeyVals:<list(<keyval>)>)
% ----------------------------------------------------------------------
% KeyVals is Key/Val list resulting from parsing TokenList using
% the compiled DCG to perform a top-down parse
% ----------------------------------------------------------------------
parse_cgi(TokenList,KeyVals):-
    keyvalseq(KeyVals,TokenList,[]).

% Grammar for Parser
% ----------------------------------------------------------------------
keyvalseq([KeyVal|KeyVals]) --> 
   keyval(KeyVal), andkeyvalseq(KeyVals). 
keyvalseq([]) --> [].

andkeyvalseq(KeyVals) --> ['&'], keyvalseq(KeyVals).
andkeyvalseq([]) --> [].

keyval(key(Key,Val)) --> [Key,'='], valseq(Val).

% valseq(rec(Ws,Cat)) --> valseq(Ws), as(Cat).

% as('$ANY') --> [].
% as(Cat) --> optplus, ['=','>'], optplus, val(Cat).

% valseq([]) --> [].   % subsumed by plusvalseq([]) --> []
valseq([Val|Vals]) --> val(Val), plusvalseq(Vals).
valseq(Vals) --> plusvalseq(Vals).

plusvalseq([]) --> [].
plusvalseq(Vals) --> ['+'], valseq(Vals).

optplus --> [].
optplus --> ['+'].

val(X) --> ['['], valseq(X), [']'].
val(der(X)) --> [der,'('], valseq(X), [')'].
val(X) --> atomval(X).
val(X/Y) --> atomval(X), ['/'], atomval(Y).
val(Y\X) --> atomval(Y), ['\\'], atomval(X).
val(X-Y) --> atomval(Y), ['-'], atomval(X).
val(Term) --> atom(Fun), ['('], argvals(Args), [')'],   {Term =.. [Fun|Args]}.

argvals([]) --> [].
argvals([Arg|Args]) -->
  val(Arg), commaargvals(Args).

commaargvals(Args) -->
  [','], argvals(Args).
commaargvals([]) -->
  [].

atomval(X) --> atom(X).
atomval(X) --> ['('], val(X), [')'].

atom(X) --> [X], {atomic(X)}.


% Specific CGI Query Handling
% ========================================================================= 

% action(+KeyVals:<list(<keyval>)>)
% ----------------------------------------------------------------------
% take an action based on list of KeyVals
% ----------------------------------------------------------------------
action(KeyVals):-
    retractall(keyvalscgi(_)),
    assert(keyvalscgi(KeyVals)),
    member(key(inputfrom,[InputFrom]),KeyVals),
    ( InputFrom = 'Typing'
      -> member(key(parsestringone,Ws),KeyVals)
    ; InputFrom = 'Corpus' 
      -> member(key(parsestringtwo,Ws),KeyVals)
    ), 
%                                 write('<p>'), write(Ws), nl,
    nl, write('P<font size=-1>ARSE</font> R<font size=-1>ESULTS FOR:</font> <cite>'),
    writelist(Ws),
    write('</cite><br><br>'), nl,
    member(key(outputform,[OutForm]),KeyVals),
    member(key(outputsyn,OutSynSym),KeyVals),
    outsyn(OutSynSym,OutSyn),
    act(OutForm,OutSyn,Ws).
  
keyvalcgi(Key,Val):-
keyvalscgi(KeyVals),
  member(key(Key,Val),KeyVals).  

outsyn(['Any'],_).
outsyn(['Finite','S'],s(fin)).
outsyn(['Noun','Phrase'],np(_,_)).

% act(+Form:<form>, ?Syn:<syn>, +Ws:<list(<word>)>)
% ----------------------------------------------------------------------
% the input Ws is parsed and output in form Form;
% ----------------------------------------------------------------------
act(OutForm,OutSyn,Ws):-
    findall(Tree, ( parse(Ws,cat(OutSyn,_,Ass,[],Tree)),
                   \+ member(abs(_,_,_),Ass) ),     Trees),  % all parses
    ( Trees = [],
      !, write('<BR> No Parses Found')                       % none found
    ; ( keyvalcgi(expandmng,['Yes']),
        !, expandmng_trees(Trees,Trees2)
      ; Trees2 = Trees
      ),
      ( keyvalcgi(normalize,['Yes']),
        !, normalize_trees(Trees2,Trees3)
      ; Trees3 = Trees2
      ),
        write('parse('),
        write_term(Ws,[quoted(true)]),
        write(',Cat).<br>'), nl,
      actout(OutForm,Trees3)
   ).



% actout(+Form:<form>, +Ts:<list(<tree>)>)
% ----------------------------------------------------------------------
% return output for list of trees Ts in form Form
% ----------------------------------------------------------------------
actout('Text',Trees):-
  write('<PRE>'), nl,  
  texttreelist(Trees),
  nl, write('</PRE>').
actout('Prawitz',Ts):-
  htmltreelist(Ts).  
actout('Fitch',Ts):-
  fitchtreelist(Ts).



texttreelist([]).
texttreelist([T|Ts]):-
  pp_tree(T),
  nl, write('<BR>'), nl,
  texttreelist(Ts).

htmltreelist([]).
htmltreelist([T|Ts]):-
  pp_html_table_tree(T),
  nl, write('<BR>'), nl,
  htmltreelist(Ts).  

fitchtreelist([]).
fitchtreelist([T|Ts]):-
  pp_html_table_fitch_tree(T),
  nl, write('<BR>'), nl,
  fitchtreelist(Ts).


% PRETTY PRINTING ROUTINES
% ======================================================================

% pp_html_table_tree(+Tree:<tree>)
% ----------------------------------------------------------------------
% Tree is output as an HTML table;  first numbered
% ----------------------------------------------------------------------
pp_html_table_tree(T):-
  numbervars(T),
%   nl, 
%   write_term(T,[quoted(true)]), 
%   nl, write('<P>'),
  pp_html_tree(T).

% pp_html_tree(+Tree:<tree>)
% ----------------------------------------------------------------------
% Tree is output as an HTML table;  assume numbered
% ----------------------------------------------------------------------
pp_html_tree(ass(Syn,V,'$VAR'(N))):-
  write('['), pp_cat(Syn:V), write(']<sup>'), write(N), write('</sup>').
pp_html_tree(leaf(Word)):-
  pp_word(Word).
pp_html_tree(ders(Words)):-
  pp_word_list(Words).
pp_html_tree(empty(Syn,Sem)):-
  nl, write('<TABLE BORDER=1>'), nl,
  write('<TR VALIGN=bottom>
         <TD ALIGN=CENTER>-</TD>
         <TD ROWSPAN=2 ALIGN=CENTER>Nil</TD>
         </TR>'), 
  nl,
  write('<TR VALIGN=bottom>
         <TD ALIGN=CENTER>'), 
  pp_cat(Syn:Sem), 
  write('</TD></TR>'),
  nl,
  write('</TABLE>').
pp_html_tree(tree(Rule,Root,SubTrees)):-
  nl, write('<TABLE BORDER=1>'), nl,
  write('<TR VALIGN=bottom>'), nl,
  pp_html_trees(SubTrees,0,N),
  nl, 
  ( Rule = lex 
    -> true
  ; write('<TD ROWSPAN=2 ALIGN=CENTER>'), pp_rule(Rule), write('</TD>')
  ),
  write('</TR>'),
  write('<TR VALIGN=bottom><TD ALIGN=CENTER COLSPAN='), write(N), write('>'), 
  pp_cat(Root),
  write('</TD></TR>'),
  nl, write('</TABLE>').

% pp_html_trees(+Trees: <list(<tree>)>,+N:<int>,-M:<int>)
% ----------------------------------------------------------------------
% prints the trees in Trees, where (M-N) is the length of the list (N
% acts as an accumulator, initialized to 0
% ----------------------------------------------------------------------
pp_html_trees([T|Ts],N,M):-
  write('<TD ALIGN=center>'), pp_html_tree(T),
  write('</TD>'),
  K is N+1,
  pp_html_trees(Ts,K,M).
pp_html_trees([],N,N).

% pp_html_table_fitch_tree(+T:<tree>)
% ----------------------------------------------------------------------
% T is numbered and output as a table Fitch-style
% ----------------------------------------------------------------------
pp_html_table_fitch_tree(T):-
  numbervars(T),
  nl, write('<TABLE BORDER=1>'), 
  pp_html_fitch_tree(T,1,_,_,_,[],_),
  nl, write('</TABLE>').

% pp_html_fitch_tree(+Tree:<tree>, +Start:<int>, -Next:<int>, -Me:<int>,
%                    +Exp:<exp>, 
%                    +AssIn:<list(<assgn>)>, -AssOut:<list(<assgn>)>)
% ----------------------------------------------------------------------
% the rows of the table for Tree are printed;
% Start is where the numbering begins; Next is the next available number
% after last one used; Me is the row representing the output of the 
% derivation;  Exp is the expression corresponding to Tree;
% AssIn are existing assignments coming in and AssOut are assignments 
% going out (an <assgn> is a pair ass(M,X) where M is a row number on the
% table and X is the abstracted variable)
% ----------------------------------------------------------------------
pp_html_fitch_tree(tree(der,Root,[ders(Words)]),M,N,M,Exp,Ass,Ass):-
  !, nl, write('<TR><TD>'), 
  write(M), write('</TD><TD>'), 
  map_word(Words,Exp), pp_exp(Exp), 
  write('-'), pp_cat(Root), 
  write('</TD><TD>'), write('Der'), write('</TD></TR>'), nl,
  N is M+1.
pp_html_fitch_tree(tree(lex,Root,[leaf(Word)]),M,N,M,Word,Ass,Ass):-
  !, nl, write('<TR><TD>'), 
  write(M), write('</TD><TD>'), pp_exp(Word), write('-'), pp_cat(Root), 
  write('</TD><TD>'), write('Lex'), write('</TD></TR>'), nl,
  N is M+1.
pp_html_fitch_tree(tree(fe,Root,[T1,T2]),M,N,L,Exp1+Exp2,AssIn,AssOut):-
  !, pp_html_fitch_tree(T1,M,K,Source1,Exp1,AssIn,AssMid),
  pp_html_fitch_tree(T2,K,L,Source2,Exp2,AssMid,AssOut),
  nl, write('<TR><TD>'), 
  write(L), write('</TD><TD>'), pp_exp(Exp1+Exp2), write('-'), pp_cat(Root), 
  write('</TD><TD>'), write('E/ '), write((Source1,Source2)), write('</TD></TR>'), nl, 
  N is L + 1.
pp_html_fitch_tree(tree(be,Root,[T1,T2]),M,N,L,Exp1+Exp2,AssIn,AssOut):-
  !, pp_html_fitch_tree(T1,M,K,Source1,Exp1,AssIn,AssMid),
  pp_html_fitch_tree(T2,K,L,Source2,Exp2,AssMid,AssOut),
  nl, write('<TR><TD>'), 
  write(L), write('</TD><TD>'), pp_exp(Exp1+Exp2), write('-'), pp_cat(Root), 
  write('</TD><TD>'), write('E\\ '), write((Source1,Source2)), write('</TD></TR>'), nl,
  N is L + 1.
pp_html_fitch_tree(tree(qqi,Root,[T]),M,Next,Me,Exp,AssIn,AssOut):-
  !, pp_html_fitch_tree(T,M,Me,Source,Exp,AssIn,AssOut),
  nl, write('<TR><TD>'), 
  write(Me), write('</TD><TD>'), pp_exp(Exp), write('-'), pp_cat(Root), 
  write('</TD><TD>'), write('q I '), write(Source), write('</TD></TR>'), nl,
  Next is Me+1.
pp_html_fitch_tree(tree(coel,Root,[T1,T2,T3]),M,N,L,Exp1+Exp2+Exp3,AssIn,AssOut):-
  !, pp_html_fitch_tree(T1,M,K,Source1,Exp1,AssIn,AssMid),
  pp_html_fitch_tree(T2,K,L1,Source2,Exp2,AssMid,AssMid2),
  pp_html_fitch_tree(T3,L1,L,Source3,Exp3,AssMid2,AssOut),
  nl, write('<TR><TD>'), 
  write(L), write('</TD><TD>'), pp_exp(Exp1+Exp2+Exp3), write('-'), pp_cat(Root), 
  write('</TD><TD>'), write('E co '), write((Source1,Source2,Source3)), write('</TD></TR>'), nl,
  N is L + 1.
pp_html_fitch_tree(tree(fi(_),(C1/C2):(var(X)^Sem),[T]),M,Q,N,ExpNew,AssIn,AssOut):-
    K is M+1,
    write('<TR><TD COLSPAN=3><TABLE BORDER=1>'),
    write('<TR><TD>'), write(M), write('</TD><TD>'),
    X = '$VAR'(Num), 
  cat_atoms(Num,'</sub>',ExpMid),
  cat_atoms('e<sub>',ExpMid,ExpNum),
    pp_exp(ExpNum),  write(' - '),
    pp_cat(C2:var(X)), write('</TD><TD>'), write('Assume</TD></TR>'),
    pp_html_fitch_tree(T,K,N,L, Exp, [ass(M,X)|AssIn],AssOut),
    write('<TR><TD>'), write(N), write('</TD><TD>'),
    removeexp(ExpNum,Exp,ExpNew),
    pp_exp(ExpNew), write(' - '), pp_cat(C1/C2:var(X)^Sem), write('</TD><TD>'),
    write('/I '), write((M,L)), write('</TD></TR>'),
    write('</TD></TR></TABLE>'),
    Q is N+1.
pp_html_fitch_tree(tree(bi(_),(C2\C1):(var(X)^Sem),[T]),M,Q,N,ExpNew,AssIn,AssOut):-
    K is M+1,
    write('<TR><TD COLSPAN=3><TABLE BORDER=1>'),
    write('<TR><TD>'), write(M), write('</TD><TD>'),
    X = '$VAR'(Num), 
  cat_atoms(Num,'</sub>',ExpMid),
  cat_atoms('e<sub>',ExpMid,ExpNum),
    pp_exp(ExpNum),  write(' - '),
    pp_cat(C2:var(X)), write('</TD><TD>'), write('Assume</TD></TR>'),
    pp_html_fitch_tree(T,K,N,L, Exp, [ass(M,X)|AssIn],AssOut),
    write('<TR><TD>'), write(N), write('</TD><TD>'),
    removeexp(ExpNum,Exp,ExpNew),
    pp_exp(ExpNew), write(' - '), pp_cat(C2\C1:var(X)^Sem), write('</TD><TD>'),
    write('/I '), write((M,L)), write('</TD></TR>'),
    write('</TD></TR></TABLE>'),
    Q is N+1.
pp_html_fitch_tree(tree(gi(_),(C1-C2):var(X)^Sem,[T]),M,Q,N,ExpNew,AssIn,AssOut):-
  K is M+1,
    write('<TR><TD COLSPAN=3><TABLE BORDER=1>'),
    write('<TR><TD>'), write(M), write('</TD><TD>'),
    X = '$VAR'(Num), 
  cat_atoms(Num,'</sub>',ExpMid),
  cat_atoms('e<sub>',ExpMid,ExpNum),
    pp_exp(ExpNum),  write(' - '),
    pp_cat(C2:var(X)), write('</TD><TD>'), write('Assume</TD></TR>'),
    pp_html_fitch_tree(T,K,N,L,Exp, [ass(M,X)|AssIn],AssOut),
    write('<TR><TD>'), write(N), write('</TD><TD>'),
    splitexp(ExpNum,Exp,ExpNew),
    pp_exp(ExpNew), write(' - '),
    pp_cat((C1-C2):var(X)^Sem), write('</TD><TD>'), 
    write('I- '), write((M,L)), write('</TD></TR>'),
    write('</TD></TR></TABLE>'),
    Q is N+1.
% pp_html_fitch_tree(tree(qqpop(N),A:(Q@(X^Alpha)),[T1]),M,N,K,Exp,Ass,Ass):-
%   !, replace_qtree(qqpush(N),T1,T1Mid,T1Extract),
%   pp_html_fitch_tree(T1Extract,M,L,J,_,_,_),
%   pp_html_fitch_tree(T1Mid,L,P,I,_,_,_),
%   write('<TR><TD>'), write(P), write('</TD><TD>'),
%   pp_exp(Exp), write(' - '), 
%   pp_cat(A:(Q@(X^Alpha))), write('</TD><TD>'),
%   write(' '). 
pp_html_fitch_tree(empty(Syn,Sem),M,N,M,[],Ass,Ass):-
  !, nl, write('<TR><TD>'), 
  write(M), write('</TD><TD>'), write('NIL'), write(' '), pp_cat(Syn:Sem), 
  write('</TD><TD>'), write('Empty'), write('</TD></TR>'), nl,
  N is M+1.
pp_html_fitch_tree(ass(_Syn,var(Var),_),N,N,M,Exp,Ass,Ass):-
  member(ass(M,Var),Ass),
  Var = '$VAR'(Num),
  cat_atoms(Num,'</sub>',ExpMid),
  cat_atoms('e<sub>',ExpMid,Exp).

% removexp(+ExpRem:<exp>,+Exp:<exp>,-ExpOut:<exp>)
% ----------------------------------------------------------------------
% he expression ExpRem is removed from Exp with result ExpOut
% ----------------------------------------------------------------------
removeexp(E,E,'NIL'):-!.
removeexp(E,E+E2,E2):-!.
removeexp(E,E2+E,E2):-!.
removeexp(E,E2+E3,E2New+E3New):-
  !, removeexp(E,E2,E2New),
  removeexp(E,E3,E3New).
removeexp(_,E2,E2).

% splitexp(+ExpRem:<exp>, +Exp:<exp>, -ExpOut:<exp>)
% ----------------------------------------------------------------------
% ExpRem is removed from Exp with ExpOut left over;  the extraction
% site is represented as a split point
% ----------------------------------------------------------------------
splitexp(E,E,('NIL','NIL')):-!.
splitexp(E,E+E2,('NIL',E2)):-!.
splitexp(E,E2+E,(E2,'NIL')):-!.
splitexp(E,E1+E2,(E3,E4+E2)):-
  splitexp(E,E1,(E3,E4)), !.
splitexp(E,E1+E2,(E1+E3,E4)):-
  splitexp(E,E2,(E3,E4)).

% pp_exp(+Exp:<exp>)
% ----------------------------------------------------------------------
% the expression Exp is output;  concatenations are represented as
% spaces and split points by (_,_) and empty by '0'
% ----------------------------------------------------------------------
pp_exp('NIL'):-
  !, write(0).
pp_exp(A+'NIL'):-
  !, pp_exp(A).
pp_exp(B+'NIL'):-
  !, pp_exp(B).
pp_exp(A+B):-
  !, pp_exp(A), write(' '), pp_exp(B).
pp_exp((A,B)):-
  !, write('('), pp_exp(A), write(', '), pp_exp(B), write(')').
pp_exp(A):-
  pp_word(A).

map_word([[_]|Ws],Exp):-
  !, map_word(Ws,Exp).
map_word([W|Ws],Exp):-
  map_word(Ws,W,Exp).
map_word([],'NIL').

map_word(Ws,[_],W):-
 !, map_word(Ws,W).
map_word([],W,W).
map_word([W|Ws],W1,W1+Exp):-
  map_word(Ws,W,Exp).

pp_exps([]).
pp_exps([Exp|Exps]):-
  pp_exp(Exp), write('+'), pp_exp(Exps).

% pp_tree(+T:<tree>)
% ----------------------------------------------------------------------
% tree T is output in indented list notation; first number
% ----------------------------------------------------------------------
pp_tree(T):-
  numbervars(T),
  pp_tree(T,0).

% pp_tree(+T:<tree>, +Col:<int>)
% ----------------------------------------------------------------------
% print tree T beginning at column Col
% ----------------------------------------------------------------------
pp_tree(empty(Syn,Sem),Col):-
  nl, tab(Col), pp_cat(Syn:Sem), write(' via empty').
pp_tree(ass(Syn,V,'$VAR'(N)),Column):-
  nl, tab(Column), write('['), pp_cat(Syn:V), write(']'), 
  write('<SUP>'), write(N), write('</SUP>').
pp_tree(leaf(Word),Column):-
  nl, tab(Column), pp_word(Word).
pp_tree(ders(Words),Column):-
  nl, tab(Column), pp_word_list(Words).
pp_tree(tree(Rule,Root,SubTrees),Column):-
  nl, tab(Column),
  pp_cat(Root),
  write(' via '), pp_rule(Rule),
  NewColumn is Column + 2,
  pp_trees(SubTrees,NewColumn).

% pp_trees(+Ts:<list(<tree>)>, +Col:<int>)
% ----------------------------------------------------------------------
% print tree list Ts beginning at column Col
% ----------------------------------------------------------------------
pp_trees([T|Ts],Column):-
  pp_tree(T,Column),
  pp_trees(Ts,Column).
pp_trees([],_).

% pp_word_list(+Ws:<list(<word>)>)
% ----------------------------------------------------------------------
% the list of words Ws is output, ignoring non-atoms
% ----------------------------------------------------------------------
pp_word_list([]).
pp_word_list([W|Ws]):-
  atom(W), !, pp_word(W), pp_word_list_rest(Ws).
pp_word_list([_|Ws]):-
  pp_word_list(Ws).

pp_word(W):-
  write('<I>'), write(W), write('</I>').

% pp_word_list_rest(+Ws:<list(<word>)>)
% ----------------------------------------------------------------------
% word list Ws is output with an initial blank if Ws is non-empty
% ----------------------------------------------------------------------
pp_word_list_rest([]).
pp_word_list_rest([W|Ws]):-
  atom(W), !, write(' '), pp_word(W), pp_word_list_rest(Ws).
pp_word_list_rest([_|Ws]):-
  pp_word_list_rest(Ws).

% pp_cat(Cat:<cat>)
% ----------------------------------------------------------------------
% pretty print category Cat
% ----------------------------------------------------------------------
pp_cat(Syn:Sem):-
  pp_lam(Sem), write(' : '), pp_syn(Syn).

% pp_syn(SynCat:<syncat>)
% ----------------------------------------------------------------------
% pretty print syntactic category
% ----------------------------------------------------------------------
pp_syn(A/B):-
  !, pp_syn(A), write('/'), pp_syn_paren(B).
pp_syn(A-B):-
  !, pp_syn(A), write('-'), pp_syn_paren(B).
pp_syn(B\A):-
  !, pp_syn_paren(B), write('\\'), pp_syn_back(A).
pp_syn(q(A,B,B)):-
  !, pp_syn(scop(A,B)).
pp_syn(q(A,B,C)):-
  !, write('q('), pp_syn(A), write(','), pp_syn(B), write(','), 
  pp_syn(C), write(')'). 
pp_syn(scop(A,B)):-
  !, pp_syn(A), write('^^'), pp_syn(B).
pp_syn(C):-
  pp_bas_cat(C).

% pp_syn_paren(SynCat:<syncat>)
% ----------------------------------------------------------------------
% pretty print syntactic category with enclosing parens if it
% is functional (used for arguments)
% ----------------------------------------------------------------------
pp_syn_paren(A/B):-
  !, pp_paren(A/B).
pp_syn_paren(A-B):-
  !, pp_paren(A-B).
pp_syn_paren(B\A):-
  !, pp_paren(B\A).
pp_syn_paren(q(A,B,B)):-
  !, pp_paren(q(A,B,B)).
pp_syn_paren(q(A,B,C)):-
  !, pp_syn(q(A,B,C)).
pp_syn_paren(C):-
  pp_bas_cat(C).

% pp_paren(+C:<cat>)
% ----------------------------------------------------------------------
% category Cat is pretty printed with surrounding parens
% ----------------------------------------------------------------------
pp_paren(C):-
  write('('), pp_syn(C), write(')').

% pp_syn_back(+Cat:<cat>)
% ----------------------------------------------------------------------
% Cat is pretty printed as the result of a backward functor
% ----------------------------------------------------------------------
pp_syn_back(A/B):-
  !, pp_syn_paren(A/B).
pp_syn_back(A-B):-
  !, pp_syn_paren(A-B).
pp_syn_back(A):-
  pp_syn(A).

% pp_bas_cat(+BasCat:<bascat>)
% ----------------------------------------------------------------------
% the basic category BasCat is pretty printed
% ----------------------------------------------------------------------
pp_bas_cat(Cat):-
  writecat(Cat,Atom,Subs,Sups),
  write(Atom),
  writesubs(Subs),
  writesups(Sups).
  
% writecat(+BasCat:<bascat>,-Root:<atom>,-Subs:<list>,-Sups:<list>)
% ----------------------------------------------------------------------
% basic category BasCat is printed as Root with superscripts Sups
% and subscripts Subs
% ----------------------------------------------------------------------
writecat(np(ind(sng),nm(_)),np,[],[]):-!.
writecat(np(ind(sng),pp(C)),np,[C],[]):-!.
writecat(np(ind(plu),nm(_)),np,[p],[]):-!.
writecat(np(ind(plu),pp(C)),np,[p,C],[]):-!.
writecat(np(ind(_),nm(_)),np,[],[]):-!.
writecat(np(set,nm(_)),np,[p],['*']):-!.
writecat(np(set,pp(C)),np,[p,C],['*']):-!.
writecat(np(_,_),np,[],[]):-!.
writecat(s(fin),s,[],[]):-!.
writecat(s('$VAR'(_)),s,[],[]):-!.
writecat(s(V),s,[V],[]):-!.
writecat(n(ind(plu)),n,[p],[]):-!.
writecat(n(set),n,[p],['*']):-!.
writecat(n(ind(sng)),n,[],[]):-!.
writecat(n(_),n,[],[]):-!.
writecat(sc(th(fin)),sc,[th,fin],[]):-!.
writecat(sc(th(bse)),sc,[th,bse],[]):-!.
writecat(sc(wh),sc,[wh],[]):-!.
writecat(sc(if),sc,[if],[]):-!.
writecat(sc(_),sc,[],[]):-!.
writecat(ex(it),ex,[it],[]):-!.
writecat(ex(th(_)),ex,[th],[]):-!.
writecat(ex(_),ex,[],[]):-!.
writecat(C,C,[],[]).

% writesubs(+List:<list>)
% ----------------------------------------------------------------------
% List is output as a subscript
% ----------------------------------------------------------------------
writesubs([]).
writesubs([X|Xs]):-
  write('<SUB>'),
  writelistsubs(Xs,X),
  write('</SUB>').

% writesups(+List:<list>)
% ----------------------------------------------------------------------
% List is output as a superscript
% ----------------------------------------------------------------------
writesups([]).
writesups([X|Xs]):-
  write('<SUP>'),
  writelistsubs(Xs,X),
  write('</SUP>').

% writelistsubs(+Xs:<list>, +X:<term>)
% ----------------------------------------------------------------------
% Xs is written as a list with commas as separators
% ----------------------------------------------------------------------
writelistsubs([],X):-
 write(X).
writelistsubs([X|Xs],Y):-
  write(Y), write(' ,'), writelistsubs(Xs,X).

% pp_lam(+Term:<lambdaterm>)
% ----------------------------------------------------------------------
% lambda term Term is pretty printed
% ----------------------------------------------------------------------
pp_lam(Var^Alpha):-
  !, pp_lam(Var), write('<B>. </B>'), pp_lam(Alpha). 
pp_lam(con(and)@Alpha@Beta):-
  !, pp_lam_paren(Alpha), write(' &amp '), pp_lam_paren(Beta).
pp_lam(con(or)@Alpha@Beta):-
  !, pp_lam_paren(Alpha), write(' <b>or</b> '), pp_lam_paren(Beta).
pp_lam(con(not)@Alpha):-
  !, write(' &#172 '), write('('), pp_lam_paren(Alpha), write(')').
pp_lam(Alpha@Beta):-
  !, pp_lam_bracket(Alpha),
  write('('),
  pp_lam(Beta),
  write(')').
pp_lam(var('$VAR'(N))):-
  !, write('<I>'), write(x), write('<SUB>'), write(N), write('</SUB></I>').
pp_lam(con(Con)):-
  write('<B>'), write(Con), write('</B>').

pp_lam_bracket(A^B):-
  !, write('('), pp_lam(A^B), write(')').
pp_lam_bracket(A):-
  pp_lam(A).

% pp_lam_paren(+Term:<lambdaterm>)
% ----------------------------------------------------------------------
% lambda term Term is pretty printed
% ----------------------------------------------------------------------
pp_lam_paren(Var^Alpha):-
  !, pp_lam(Var), write('<B>. </B>'), pp_lam(Alpha). 
pp_lam_paren(con(and)@Alpha@Beta):-
  !, write('('), pp_lam_paren(Alpha), write(' &amp '), pp_lam_paren(Beta), write(')').
pp_lam_paren(con(or)@Alpha@Beta):-
  !, write('('), pp_lam_paren(Alpha), write(' <b>or</b> '), pp_lam_paren(Beta), write(')').
pp_lam_paren(con(not)@Alpha):-
  !, write(' &#172 '), write('('), pp_lam_paren(Alpha), write(')').
pp_lam_paren(Alpha@Beta):-
  !, pp_lam(Alpha),
  write('('),
  pp_lam(Beta),
  write(')').
pp_lam_paren(var('$VAR'(N))):-
  !, write('<I>'), write(x), write('<SUB>'), write(N), write('</SUB></I>').
pp_lam_paren(con(Con)):-
  write('<B>'), write(Con), write('</B>').

% pp_rule(+Rule:<rulename>)
% ----------------------------------------------------------------------
% rule Rule is pretty printed
% ----------------------------------------------------------------------
pp_rule(fe):-write('/E').
pp_rule(be):-write('\\E').
pp_rule(fi('$VAR'(N))):-write('/I<sup>'), write(N), write('</sup>').
pp_rule(bi('$VAR'(N))):-write('\\I<sup>'), write(N), write('</sup>').
pp_rule(gi('$VAR'(N))):-write('-I<sup>'), write(N), write('</sup>').
pp_rule(qqpush('$VAR'(N))):-write('qE<sup>'), write(N), write('</sup>').
pp_rule(qqpop('$VAR'(N))):-write(N).
pp_rule(qqi):-write(qI).
pp_rule(coel):-write('coE').
pp_rule(lex):-write('L').
pp_rule(der):-write('D').
pp_rule(nbc):-write('NBC').
pp_rule(qi):-write('qI').


% Standard Utilities
% ======================================================================

member(X,[X|_]).
member(X,[_|Xs]):-
  member(X,Xs).

append_list([],[]).
append_list([Xs|Xss],Ys):-
  append(Xs,Zs,Ys),
  append_list(Xss,Zs).

append([],Xs,Xs).
append([X|Xs],Ys,[X|Zs]):-
  append(Xs,Ys,Zs).

at_least_one_member(X,[X|_]):-!.
at_least_one_member(X,[_|Xs]):-
  at_least_one_member(X,Xs).

numbervars(X):-
  numbervars(X,0,_).

reverse([],Ws,Ws).
reverse([W|Ws],WsAcc,WsRev):-
  reverse(Ws,[W|WsAcc],WsRev).

select(X,[X|Xs],Xs).
select(X,[Y|Xs],[Y|Zs]):-
  select(X,Xs,Zs).

select_last([X],X,[]).
select_last([X|Xs],Y,[X|Zs]):-
  select_last(Xs,Y,Zs).

cat_atoms(A1,A2,A3):-
  name(A1,L1),
  name(A2,L2),
  append(L1,L2,L3),
  name(A3,L3).

writelist([der(Ws)|Ws2]):-
  !, writelist(Ws), write(' '), writelist(Ws2).
writelist([W|Ws]):-
  write(W), write(' '),
  writelist(Ws).
writelist([]).

write_lex_cat(File):-
  tell(File),
  write('<HTML><HEAD><TITLE>Natural Deduction CG Parser</TITLE></HEAD><BODY><b> L<FONT SIZE = -1>EXICON</FONT> </b><br><br><FONT SIZE=-1>'), nl, nl,
  setof(lexe(W,Syn:Sem),lexentry(W,Syn,Sem),Ws),
  !,   writebreaklex(Ws),
  nl, write('</FONT></HEAD></HTML>'), nl,
  told.
	
writebreaklex([]).
writebreaklex([W|Ws]):-
  writebreaklex(Ws,W).

writebreaklex([],lexe(W,Cat)):-
  write(W), write(' ==> '), 
  pp_cat(Cat), nl.
writebreaklex([W2|Ws],lexe(W,Cat)):-
  write(W), write(' ==> '), 
  pp_cat(Cat), 
  write(' <BR> '), nl,
  writebreaklex(Ws,W2).

write_lex(File):-
  tell(File),
  write('<HTML><HEAD><TITLE>Natural Deduction CG Parser</TITLE></HEAD><BODY><b> L<FONT SIZE = -1>EXICON</FONT> </b><br><FONT SIZE=-1><BR>'), nl,
  setof(W,C^(W==>C),Ws),
  !,   writebreak(Ws),
  nl, write('</FONT></HEAD></HTML>'), nl,
  told.

writebreak([]).
writebreak([W|Ws]):-
  writebreak(Ws,W).

writebreak([],W):-
  write(W), nl.
writebreak([W2|Ws],W):-
  write(W), write(' <BR> '), nl,
  writebreak(Ws,W2).

tt:-
  consult(natded), consult(lexicon), consult_lex, compile_empty.

mt:-
  consult(natded), consult(lexicon), consult_lex, compile_empty, save(test3), start_up.

cmt:-
  compile(natded), compile(lexicon), compile_lex('compilelex.pl'), compile_empty, save(test3), start_up.


%%% Local Variables:
%%% mode: prolog
%%% prolog-indent-width: 2
%%% tab-width: 2
%%% End:
