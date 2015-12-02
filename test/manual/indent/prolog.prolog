%% -*- mode: prolog; coding: utf-8; fill-column: 78 -*-

%% bug#21526
test21526_1 :-
    (   a ->
        (   a ->
            b
        ;   c
        )
    ;   % Toto
        c ->
        d
    ).

test21526_2 :-
    (    a
    ->   (   a,
             b
         ;   c
         ),
         b2
    ;    c1,
         c2
    ).

test21526_3 :-
    X \= Y,
    \+ a,
    b,
    \+ \+ c,
    d.

test21526_4 :-
    (   \+ a ->
        b
    ;   \+ c,
        \+ d
    ).


test21526_5 :-
    (a;
     b ->
         c).

test21526_predicate(c) :- !,
    test_goal1,
    test_goal2.

%% Testing correct tokenizing.
foo(X) :- 0'= = X.
foo(X) :- 8'234 = X.
foo(X) :- '\x45\' = X.
foo(X) :- 'test 0'=X.
foo(X) :- 'test 8'=X.

%% wf(+E)
%% Vérifie que E est une expression syntaxiquement correcte.
wf(X) :- atom(X); integer(X); var(X).         %Une variable ou un entier.
wf(lambda(X, T, B)) :- atom(X), wf(T), wf(B). %Une fonction.
wf(app(E1, E2)) :- wf(E1), wf(E2).            %Un appel de fonction.
wf(pi(X, T, B)) :- atom(X), wf(T), wf(B).     %Le type d'une fonction.

%% Éléments additionnels utilisés dans le langage source.
wf(lambda(X, B)) :- atom(X), wf(B).
wf(let(X, E1, E2)) :- atom(X), wf(E1), wf(E2).
wf(let(X, T, E1, E2)) :- atom(X), wf(T), wf(E1), wf(E2).
wf((T1 -> T2)) :- wf(T1), wf(T2).
wf(forall(X, T, B)) :- atom(X), wf(T), wf(B).
wf(fix(X,T,E1,E2)) :- atom(X), wf(T), wf(E1), wf(E2).
wf(fix(X,E1,E2)) :- atom(X), wf(E1), wf(E2).
wf(app(E1,E2,E3)) :- wf(E1), wf(E2), wf(E3).
wf(app(E1,E2,E3,E4)) :- wf(E1), wf(E2), wf(E3), wf(E4).

%% subst(+X, +V, +FV, +Ei, -Eo)
%% Remplace X par V dans Ei.  Les variables qui apparaissent libres dans
%% V et peuvent aussi apparaître dans Ei doivent toutes être inclues
%% dans l'environnement FV.
subst(X, V, _, X, E) :- !, E = V.
subst(_, _, _, Y, Y) :- atom(Y); integer(Y).
%% Residualize the substitution when applied to an uninstantiated variable.
%% subst(X, V, _, Y, app(lambda(X,_,Y),V)) :- var(Y).
%% Rather than residualize and leave us with unifications that fail, let's
%% rather assume that Y will not refer to X.
subst(X, V, _, Y, Y) :- var(Y).
subst(X, V, FV, lambda(Y, Ti, Bi), lambda(Y1, To, Bo)) :-
    subst(X, V, FV, Ti, To),
    (X = Y ->
         %% If X is equal to Y, X is shadowed, so no subst can take place.
         Y1 = Y, Bo = Bi;
     (member((Y, _), FV) ->
          %% If Y appears in FV, it can appear in V, so we need to
          %% rename it to avoid name capture.
          new_atom(Y, Y1),
          subst(Y, Y1, [], Bi, Bi1);
      Y1 = Y, Bi1 = Bi),
     %% Perform substitution on the body.
     subst(X, V, FV, Bi1, Bo)),
    (	X = Y
    %% If X is equal to Y, X is shadowed, so no subst can take place.
    ->	Y1 = Y, Bo = Bi
    ;	(member((Y, _), FV)
	%% If Y appears in FV, it can appear in V, so we need to
	%% rename it to avoid name capture.
	-> new_atom(Y, Y1),
	   subst(Y, Y1, [], Bi, Bi1)
	; Y1 = Y, Bi1 = Bi),
	%% Perform substitution on the body.
	subst(X, V, FV, Bi1, Bo)
    ).
subst(X, V, FV, pi(Y, Ti, Bi), pi(Y1, To, Bo)) :-
    subst(X, V, FV, lambda(Y, Ti, Bi), lambda(Y1, To, Bo)).
subst(X, V, FV, forall(Y, Ti, Bi), forall(Y1, To, Bo)) :-
    subst(X, V, FV, lambda(Y, Ti, Bi), lambda(Y1, To, Bo)).
subst(X, V, FV, app(E1i, E2i), app(E1o, E2o)) :-
    subst(X, V, FV, E1i, E1o), subst(X, V, FV, E2i, E2o).

%% apply(+F, +Arg, +Env, -E)
apply(lambda(X, _, B), Arg, Env, E) :- \+ var(B), subst(X, Arg, Env, B, E).
apply(app(plus, N1), N2, _, N) :- integer(N1), integer(N2), N is N1 + N2.
apply(app(minus, N1), N2, _, N) :- integer(N1), integer(N2), N is N1 - N2.


%% normalize(+E1, +Env, -E2)
%% Applique toutes les réductions possibles sur E1.
normalize(X, _, X) :- integer(X); var(X); atom(X).
%% normalize(X, Env, E) :- atom(X), member((X, E), Env).
normalize(lambda(X, T, B), Env, lambda(X, Tn, Bn)) :-
    normalize(T, [(X,T)|Env], Tn), normalize(B, [(X,T)|Env], Bn).
normalize(pi(X, T, B), Env, pi(X, Tn, Bn)) :-
    normalize(T, [(X,T)|Env], Tn), normalize(B, [(X,T)|Env], Bn).
normalize(forall(X, T, B), Env, forall(X, Tn, Bn)) :-
    normalize(T, [(X,T)|Env], Tn), normalize(B, [(X,T)|Env], Bn).
normalize(app(E1, E2), Env, En) :-
    normalize(E1, Env, E1n),
    normalize(E2, Env, E2n),
    (apply(E1n, E2n, Env, E) ->
         normalize(E, Env, En);
     En = app(E1n, E2n)).

%% infer(+E, +Env, -T)
%% Infère le type de E dans Env.  On essaie d'être permissif, dans le sens
%% que l'on présume que l'expression est typée correctement.
infer(X, _, int) :- integer(X).
infer(X, _, _) :- var(X).            %Une expression encore inconnue.
infer(X, Env, T) :-
    atom(X),
    (member((X, T1), Env) ->
         %% X est déjà dans Env: vérifie que le type est correct.
         T = T1;
     %% X est une variable libre.
     true).
infer(lambda(X,T,B), Env, pi(Y,T,TB)) :-
    infer(B, [(X,T)|Env], TBx),
    (var(Y) ->
         Y = X, TB = TBx;
     subst(X, Y, Env, TBx, TB)).
infer(app(E1, E2), Env, Tn) :-
    infer(E1, Env, T1),
    (T1 = pi(X,T2,B); T1 = forall(X,T2,B)),
    infer(E2, Env, T2),
    subst(X, E2, Env, B, T),
    normalize(T, Env, Tn).
infer(pi(X,T1,T2), Env, type) :-
    infer(T1, Env, type),
    infer(T2, [(X,T1)|Env], type).
infer(forall(X,T1,T2), Env, type) :-
    infer(T1, Env, type),
    infer(T2, [(X,T1)|Env], type).

%% freevars(+E, +Env, -Vs)
%% Renvoie les variables libres de E.  Vs est une liste associative
%% où chaque élément est de la forme (X,T) où X est une variable et T est
%% son type.
freevars(X, _, []) :- integer(X).
freevars(X, Env, Vs) :-
    atom(X),
    (member((X,_), Env) ->
         %% Variable liée.
         Vs = [];
     %% Variable libre.  Type inconnu :-(
     Vs = [(X,_)]).
%% Les variables non-instanciées peuvent être remplacées par des paramètres
%% qui seront liés par `closetype' selon le principe de Hindley-Milner.
freevars(X, _, [(X, _)]) :- var(X), new_atom(X).
freevars(app(E1, E2), Env, Vs) :-
    freevars(E1, Env, Vs1),
    append(Vs1, Env, Env1),
    freevars(E2, Env1, Vs2),
    append(Vs1, Vs2, Vs).
freevars(lambda(X, T, B), Env, Vs) :-
    freevars(T, Env, TVs),
    append(TVs, Env, Env1),
    freevars(B, [(X,T)|Env1], BVs),
    append(TVs, BVs, Vs).
freevars(pi(X, T, B), Env, Vs)     :- freevars(lambda(X, T, B), Env, Vs).
freevars(forall(X, T, B), Env, Vs) :- freevars(lambda(X, T, B), Env, Vs).

%% close(+Eo, +To, +Vs, -Ec, -Tc)
%% Ferme un type ouvert To en liant chaque variable libre (listées dans Vs)
%% avec `forall'.
closetype(E, T, [], E, T).
closetype(Eo, To, [(X,T)|Vs], lambda(X, T, Ec), forall(X, T, Tc)) :-
    closetype(Eo, To, Vs, Ec, Tc).

%% elab_type(+Ee, +Te, +Env, -Eg, -Tg)
%% Ajoute les arguments implicites de E:T.
generalize(Ee, Te, Env, Eg, Tg) :-
    freevars(Te, Env, Vs),
    append(Vs, Env, EnvX),
    %% Essaie d'instancier les types des paramètres que `generalize' vient
    %% d'ajouter.
    infer(Te, EnvX, type),
    closetype(Ee, Te, Vs, Eg, Tg).

%% instantiate(+X, +T, -E)
%% Utilise la variable X de type T.  Le résultat E est X auquel on ajoute
%% tous les arguments implicites (de valeur inconnue).
instantiate(X, T, X) :- var(T), !.
instantiate(X, forall(_, _, T), app(E, _)) :- !, instantiate(X, T, E).
instantiate(X, _, X).

%% elaborate(+E1, +Env, -E2)
%% Transforme E1 en une expression E2 où le sucre syntaxique a été éliminé
%% et où les arguments implicites ont été rendus explicites.
elaborate(X, _, X) :- integer(X); var(X).
elaborate(X, Env, E) :-
    atom(X),
    (member((X, T), Env) ->
         instantiate(X, T, E);
     %% Si X n'est pas dans l'environnement, c'est une variable libre que
     %% l'on voudra probablement généraliser.
     X = E).
elaborate(lambda(X, T, B), Env, lambda(X, Te, Be)) :-
    elaborate(T, Env, Te),
    elaborate(B, [(X,Te)|Env], Be).
elaborate(pi(X, T, B), Env, pi(X, Te, Be)) :-
    elaborate(T, Env, Te),
    elaborate(B, [(X,Te)|Env], Be).
elaborate(app(E1, E2), Env, app(E1e, E2e)) :-
    elaborate(E1, Env, E1e),
    elaborate(E2, Env, E2e).
elaborate(let(X, T, E1, E2), Env, app(lambda(X, Tg, E2e), E1g)) :-
    elaborate(E1, Env, E1e),
    elaborate(T, Env, Te),
    infer(E1e, Env, Te),
    generalize(E1e, Te, Env, E1g, Tg),
    elaborate(E2, [(X,Te)|Env], E2e).
%% Expansion du sucre syntaxique.
elaborate((T1 -> T2), Env, Ee) :-
    new_atom(X), elaborate(pi(X, T1, T2), Env, Ee).
elaborate(app(E1, E2, E3, E4), Env, Ee) :-
    elaborate(app(app(E1,E2,E3),E4), Env, Ee).
elaborate(app(E1, E2, E3), Env, Ee) :- elaborate(app(app(E1,E2),E3), Env, Ee).
elaborate(lambda(X, B), Env, Ee) :- elaborate(lambda(X, _, B), Env, Ee).
elaborate(let(X, E1, E2), Env, Ee) :- elaborate(let(X, _, E1, E2), Env, Ee).
elaborate(fix(F,B,E), Env, Ee) :- elaborate(fix(F,_,B,E), Env, Ee).
elaborate(fix(F,T,B,E), Env, Ee) :-
    elaborate(let(F,T,app(fix,lambda(F,T,B)),E), Env, Ee).

%% elab_bindings(+TS, +Env, -TS).
%% Applique `elaborate' sur l'environnement de type TS.
elab_tenv([], _, []).
elab_tenv([(X,T)|TS], Env, [(X, Tg)|TSe]) :-
    elaborate(T, Env, Te),
    infer(Te, Env, type),
    generalize(_, Te, Env, _, Tg),
    elab_tenv(TS, [(X, Tg)|Env], TSe).


%% elaborate(+E1, -E2)
%% Comme le `elaborate' ci-dessus, mais avec un environnement par défaut.
elaborate(SRC, E) :-
    elab_tenv([(int, type),
               (fix, ((t -> t) -> t)),
               %% list: type → int → type
               (list, (type -> int -> type)),
               %% plus: int → int → int
               (plus, (int -> int -> int)),
               %% minus: int → int → int
               (minus, (int -> int -> int)),
               %% nil: list t 0
               (nil, app(app(list,t),0)),
               %% cons: t -> list t n → list t (n + 1)
               (cons, (t -> app(app(list,t),n) ->
                            app(app(list,t), app(app(plus,n),1)))) %fixindent
              ],
              [(type,type)],
              Env),
    elaborate(SRC, Env, E).
