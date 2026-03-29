%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1999-2000,2002-2007, 2009-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Module: accumulator.m.
% Main authors: petdr.
%
% Attempts to transform a single proc to a tail recursive form by
% introducing accumulators. The algorithm can do this if the code after
% the recursive call has either the order independent state update or
% associative property.
%
% /* Order independent State update property */
% :- promise all [A,B,S0,S]
%   (
%       (some[SA] (update(A, S0, SA), update(B, SA, S)))
%   <=>
%       (some[SB] (update(B, S0, SB), update(A, SB, S)))
%   ).
%
% /* Associativity property */
% :- promise all [A,B,C,ABC]
%   (
%       (some[AB] (assoc(A, B, AB), assoc(AB, C, ABC)))
%   <=>
%       (some[BC] (assoc(B, C, BC), assoc(A, BC, ABC)))
%   ).
%
% XXX What about exceptions and non-termination?
%
% The promise declarations above only provide promises about the declarative
% semantics, but in order to apply this optimization, we ought to check that
% it will preserve the operational semantics (modulo whatever changes are
% allowed by the language semantics options).
%
% Currently we check and respect the --fully-strict option, but not the
% --no-reorder-conj option. XXX we should check --no-reorder-conj!
% If --no-reorder-conj was set, it would still be OK to apply this
% transformation, but ONLY in cases where the goals which get reordered
% are guaranteed not to throw any exceptions.
%
% The algorithm implemented is a combination of the algorithms from
% "Making Mercury Programs Tail Recursive" and
% "State Update Transformation", which can be found at
% <http://www.cs.mu.oz.au/research/mercury/information/papers.html>.
%
% Note that currently "State Update Transformation" paper only resides
% in CVS papers archive in the directory update, but has been submitted
% to PPDP '00.
%
% The transformation recognizes predicates in the form
%
% p(In, OutUpdate, OutAssoc) :-
%   minimal(In),
%   initialize(OutUpdate),
%   base(OutAssoc).
% p(In, OutUpdate, OutAssoc) :-
%   decompose(In, Current, Rest),
%   p(Rest, OutUpdate0, OutAssoc0),
%   update(Current, OutUpdate0, OutUpdate),
%   assoc(Current, OutAssoc0, OutAssoc).
%
% which can be transformed by the algorithm in "State Update Transformation" to
%
% p(In, OutUpdate, OutAssoc) :-
%   initialize(AccUpdate),
%   p_acc(In, OutUpdate, OutAssoc, AccUpdate).
%
% p_acc(In, OutUpdate, OutAssoc, AccUpdate) :-
%   minimal(In),
%   base(OutAssoc),
%   OutUpdate = AccUpdate.
% p_acc(In, OutUpdate, OutAssoc, AccUpdate0) :-
%   decompose(In, Current, Rest),
%   update(Current, AccUpdate0, AccUpdate),
%   p_acc(Rest, OutUpdate, OutAssoc0, AccUpdate),
%   assoc(Current, OutAssoc0, OutAssoc).
%
% we then apply the algorithm from "Making Mercury Programs Tail Recursive"
% to p_acc to obtain
%
% p_acc(In, OutUpdate, OutAssoc, AccUpdate) :-
%   minimal(In),
%   base(OutAssoc),
%   OutUpdate = AccUpdate.
% p_acc(In, OutUpdate, OutAssoc, AccUpdate0) :-
%   decompose(In, Current, Rest),
%   update(Current, AccUpdate0, AccUpdate),
%   p_acc2(Rest, OutUpdate, OutAssoc, AccUpdate, Current).
%
% p_acc2(In, OutUpdate, OutAssoc, AccUpdate0, AccAssoc0) :-
%   minimal(In),
%   base(Base),
%   assoc(AccAssoc0, Base, OutAssoc),
%   OutUpdate = AccUpdate0.
% p_acc2(In, OutUpdate, OutAssoc, AccUpdate0, AccAssoc0) :-
%   decompose(In, Current, Rest),
%   update(Current, AccUpdate0, AccUpdate),
%   assoc(AccAssoc0, Current, AccAssoc),
%   p_acc2(Rest, OutUpdate, OutAssoc, AccUpdate, AccAssoc).
%
% p_acc is no longer recursive and is only ever called from p, so we
% inline p_acc into p to obtain the final schema.
%
% p(In, OutUpdate, OutAssoc) :-
%   minimal(In),
%   base(OutAssoc),
%   initialize(AccUpdate),
%   OutUpdate = AccUpdate.
% p(In, OutUpdate, OutAssoc) :-
%   decompose(In, Current, Rest),
%   initialize(AccUpdate0),
%   update(Current, AccUpdate0, AccUpdate),
%   p_acc2(Rest, OutUpdate, OutAssoc, AccUpdate, Current).
%
% p_acc2(In, OutUpdate, OutAssoc, AccUpdate0, AccAssoc0) :-
%   minimal(In),
%   base(Base),
%   assoc(AccAssoc0, Base, OutAssoc),
%   OutUpdate = AccUpdate0.
% p_acc2(In, OutUpdate, OutAssoc, AccUpdate0, AccAssoc0) :-
%   decompose(In, Current, Rest),
%   update(Current, AccUpdate0, AccUpdate),
%   assoc(AccAssoc0, Current, AccAssoc),
%   p_acc2(Rest, OutUpdate, OutAssoc, AccUpdate, AccAssoc).
%
% The only real difficulty in this new transformation is identifying the
% initialize/1 and base/1 goals from the original base case.
%
% Note that if the recursive clause contains multiple calls to p, the
% transformation attempts to move each recursive call to the end
% until one succeeds. This makes the order of independent recursive
% calls in the body irrelevant.
%
% XXX Replace calls to can_reorder_goals with calls to the version that
% use the intermodule-analysis framework.
%
%---------------------------------------------------------------------------%

:- module transform_hlds.accumulator.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- import_module univ.

    % Attempt to transform a procedure into accumulator recursive form.
    % If we succeed, we will add the recursive version of the procedure
    % to the module_info. However, we may also encounter errors, which
    % we will add to the list of error_specs in the univ accumulator.
    %
:- pred accu_transform_proc(pred_proc_id::in, pred_info::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out,
    univ::in, univ::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.assertion.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_error_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_promise.
:- import_module hlds.instmap.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module hlds.status.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_util.
:- import_module parse_tree.set_of_var.
:- import_module transform_hlds.goal_store.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module solutions.
:- import_module string.
:- import_module term.
:- import_module varset.

%---------------------------------------------------------------------------%

    % The form of the goal around the base and recursive cases.
    %
:- type top_level
    --->    switch_base_rec
    ;       switch_rec_base
    ;       disj_base_rec
    ;       disj_rec_base
    ;       ite_base_rec
    ;       ite_rec_base.

    % An accu_goal_id represents a goal. The first field says which conjunction
    % the goal came from (the base case or the recursive case), and the second
    % gives the location of the goal in that conjunction.
    %
:- type accu_goal_id
    --->    accu_goal_id(accu_case, int).

:- type accu_case
    --->    accu_base
    ;       accu_rec.

    % The goal_store associates a goal with each goal_id.
    %
:- type accu_goal_store == goal_store(accu_goal_id).

    % A substitution from the first variable name to the second.
    %
:- type accu_subst == map(prog_var, prog_var).

:- type accu_warning
    --->    accu_warn(prog_context, pred_id, prog_var, prog_var).
            % Warn that two prog_vars in a call to pred_id at the given context
            % were swapped, which may cause an efficiency problem.

%---------------------------------------------------------------------------%

accu_transform_proc(proc(PredId, ProcId), PredInfo, !ProcInfo, !ModuleInfo,
        !Cookie) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    DoLCMC = OptTuple ^ ot_opt_lcmc_accumulator,
    globals.lookup_bool_option(Globals, fully_strict, FullyStrict),
    ( if
        should_attempt_accu_transform(!ModuleInfo, PredId, ProcId, PredInfo,
            !ProcInfo, FullyStrict, DoLCMC, Warnings)
    then
        globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
        (
            VeryVerbose = yes,
            trace [io(!IO)] (
                module_info_get_name(!.ModuleInfo, ModuleName),
                get_progress_output_stream(Globals, ModuleName,
                    ProgressStream, !IO),
                PredStr = pred_id_to_string(!.ModuleInfo, PredId),
                io.format(ProgressStream,
                    "%% Accumulators introduced into %s\n", [s(PredStr)], !IO)
            )
        ;
            VeryVerbose = no
        ),

        (
            Warnings = []
        ;
            Warnings = [_ | _],
            pred_info_get_context(PredInfo, Context),
            PredPieces = describe_one_pred_name(!.ModuleInfo,
                should_module_qualify, PredId),
            InPieces = [words("In") | PredPieces] ++ [suffix(":"), nl],
            InMsg = simple_msg(Context,
                [option_is_set(warn_accumulator_swaps, yes,
                    [always(InPieces)])]),

            proc_info_get_varset(!.ProcInfo, VarSet),
            generate_warnings(!.ModuleInfo, VarSet, Warnings, WarnMsgs),
            (
                Warnings = [_],
                EnsurePieces = [words("Please ensure that this"),
                    words("argument rearrangement does not introduce"),
                    words("performance problems.")]
            ;
                Warnings = [_, _ | _],
                EnsurePieces = [words("Please ensure that these"),
                    words("argument rearrangements do not introduce"),
                    words("performance problems.")]
            ),
            SuppressPieces =
                [words("These warnings can be suppressed by"),
                quote("--no-warn-accumulator-swaps"), suffix(".")],
            VerbosePieces = [words("If a predicate has been declared"),
                words("associative"),
                words("via a"), quote("promise"), words("declaration,"),
                words("the compiler will rearrange the order of"),
                words("the arguments in calls to that predicate,"),
                words("if by so doing it makes the containing predicate"),
                words("tail recursive. In such situations, the compiler"),
                words("will issue this warning. If this reordering"),
                words("changes the performance characteristics"),
                words("of the call to the predicate, use"),
                quote("--no-accumulator-introduction"),
                words("to turn the optimization off, or "),
                quote("--no-warn-accumulator-swaps"),
                words("to turn off the warnings.")],
            EnsureSuppressMsg = simple_msg(Context,
                [option_is_set(warn_accumulator_swaps, yes,
                    [always(EnsurePieces), always(SuppressPieces)]),
                verbose_only(verbose_once, VerbosePieces)]),
            Severity = severity_conditional(warn_accumulator_swaps, yes,
                severity_warning, no),
            Msgs = [InMsg | WarnMsgs] ++ [EnsureSuppressMsg],
            Spec = error_spec($pred, Severity, phase_accumulator_intro, Msgs),

            det_univ_to_type(!.Cookie, Specs0),
            Specs = [Spec | Specs0],
            type_to_univ(Specs, !:Cookie)
        )
    else
        true
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred generate_warnings(module_info::in, prog_varset::in,
    list(accu_warning)::in, list(error_msg)::out) is det.

generate_warnings(_, _, [], []).
generate_warnings(ModuleInfo, VarSet, [Warning | Warnings], [Msg | Msgs]) :-
    generate_warning(ModuleInfo, VarSet, Warning, Msg),
    generate_warnings(ModuleInfo, VarSet, Warnings, Msgs).

:- pred generate_warning(module_info::in, prog_varset::in, accu_warning::in,
    error_msg::out) is det.

generate_warning(ModuleInfo, VarSet, Warning, Msg) :-
    Warning = accu_warn(Context, PredId, VarA, VarB),
    PredPieces = describe_one_pred_name(ModuleInfo, should_module_qualify,
        PredId),

    varset.lookup_name(VarSet, VarA, VarAName),
    varset.lookup_name(VarSet, VarB, VarBName),

    Pieces = [words("warning: the call to")] ++ PredPieces ++
        [words("has had the location of the variables"),
        quote(VarAName), words("and"), quote(VarBName),
        words("swapped to allow accumulator introduction."), nl],
    Msg = simplest_msg(Context, Pieces).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % should_attempt_accu_transform is only true iff the current proc
    % has been transformed to call the newly created accumulator proc.
    %
:- pred should_attempt_accu_transform(module_info::in, module_info::out,
    pred_id::in, proc_id::in, pred_info::in, proc_info::in, proc_info::out,
    bool::in, maybe_opt_lcmc_accumulator::in,
    list(accu_warning)::out) is semidet.

should_attempt_accu_transform(!ModuleInfo, PredId, ProcId, PredInfo,
        !ProcInfo, FullyStrict, DoLCMC, Warnings) :-
    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_headvars(!.ProcInfo, HeadVars),
    proc_info_get_initial_instmap(!.ModuleInfo, !.ProcInfo, InitialInstMap),
    accu_standardize(Goal0, Goal),
    identify_goal_type(PredId, ProcId, Goal, InitialInstMap,
        TopLevel, Base, BaseInstMap, Rec, RecInstMap),

    C = initialize_goal_store(Rec, RecInstMap, Base, BaseInstMap),
    identify_recursive_calls(PredId, ProcId, C, RecCallIds),
    list.length(Rec, M),

    should_attempt_accu_transform_2(!ModuleInfo, PredId, PredInfo, !ProcInfo,
        HeadVars, InitialInstMap, TopLevel, FullyStrict, DoLCMC,
        RecCallIds, C, M, Rec, Warnings).

    % should_attempt_accu_transform_2 takes a list of locations of the
    % recursive calls, and attempts to introduce accumulator into each of the
    % recursive calls, stopping at the first one that succeeds.
    % This catches the following case, as selecting the first recursive call
    % allows the second recursive call to be moved before it, and
    % OutA is in the correct spot in list.append.
    %
    %   p(InA, OutA),
    %   p(InB, OutB),
    %   list.append(OutB, OutA, Out)
    %
:- pred should_attempt_accu_transform_2(module_info::in, module_info::out,
    pred_id::in, pred_info::in, proc_info::in, proc_info::out,
    list(prog_var)::in, instmap::in, top_level::in, bool::in,
    maybe_opt_lcmc_accumulator::in,
    list(accu_goal_id)::in, accu_goal_store::in, int::in, list(hlds_goal)::in,
    list(accu_warning)::out) is semidet.

should_attempt_accu_transform_2(!ModuleInfo, PredId, PredInfo, !ProcInfo,
        HeadVars, InitialInstMap, TopLevel, FullyStrict, DoLCMC,
        [Id | Ids], C, M, Rec, Warnings) :-
    proc_info_get_vartypes(!.ProcInfo, VarTypes0),
    identify_out_and_out_prime(!.ModuleInfo, VarTypes0, InitialInstMap,
        Id, Rec, HeadVars, Out, OutPrime, HeadToCallSubst, CallToHeadSubst),
    ( if
        accu_stage1(!.ModuleInfo, VarTypes0, FullyStrict, DoLCMC, Id, M, C,
            Sets),
        accu_stage2(!.ModuleInfo, !.ProcInfo, Id, C, Sets, OutPrime, Out,
            VarSet, VarTypes, Accs, BaseCase, BasePairs, Substs, CS,
            WarningsPrime),
        accu_stage3(Id, Accs, VarSet, VarTypes, C, CS, Substs,
            HeadToCallSubst, CallToHeadSubst, BaseCase, BasePairs, Sets, Out,
            TopLevel, PredId, PredInfo, !ProcInfo, !ModuleInfo)
    then
        Warnings = WarningsPrime
    else
        should_attempt_accu_transform_2(!ModuleInfo, PredId, PredInfo,
            !ProcInfo, HeadVars, InitialInstMap, TopLevel, FullyStrict, DoLCMC,
            Ids, C, M, Rec, Warnings)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Transform the goal into a standard form that is amenable to
    % introducing accumulators.
    %
    % At the moment all this does is remove any extra disj/conj wrappers
    % around the top level goal.
    %
    % Future work is for this code to rearrange code with multiple base
    % and recursive cases into a single base and recursive case.
    %
:- pred accu_standardize(hlds_goal::in, hlds_goal::out) is det.

accu_standardize(Goal0, Goal) :-
    ( if
        Goal0 = hlds_goal(GoalExpr0, _),
        (
            GoalExpr0 = conj(plain_conj, [Goal1])
        ;
            GoalExpr0 = disj([Goal1])
        )
    then
        accu_standardize(Goal1, Goal)
    else
        Goal = Goal0
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % This predicate takes the original goal and identifies the `shape'
    % of the goal around the recursive and base cases.
    %
    % Note that the base case can contain a recursive call, as the
    % transformation doesn't depend on what is in the base case.
    %
:- pred identify_goal_type(pred_id::in, proc_id::in, hlds_goal::in,
    instmap::in, top_level::out, list(hlds_goal)::out, instmap::out,
    list(hlds_goal)::out, instmap::out) is semidet.

identify_goal_type(PredId, ProcId, Goal, InitialInstMap, Type,
        Base, BaseInstMap, Rec, RecInstMap) :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    (
        GoalExpr = switch(_Var, _CanFail, Cases),
        ( if
            Cases = [case(_IdA, [], GoalA), case(_IdB, [], GoalB)],
            goal_to_conj_list(GoalA, GoalAList),
            goal_to_conj_list(GoalB, GoalBList)
        then
            ( if is_recursive_case(GoalAList, proc(PredId, ProcId)) then
                Type = switch_rec_base,
                Base = GoalBList,
                Rec = GoalAList
            else if is_recursive_case(GoalBList, proc(PredId, ProcId)) then
                Type = switch_base_rec,
                Base = GoalAList,
                Rec = GoalBList
            else
                fail
            ),
            BaseInstMap = InitialInstMap,
            RecInstMap = InitialInstMap
        else
            fail
        )
    ;
        GoalExpr = disj(Goals),
        ( if
            Goals = [GoalA, GoalB],
            goal_to_conj_list(GoalA, GoalAList),
            goal_to_conj_list(GoalB, GoalBList)
        then
            ( if is_recursive_case(GoalAList, proc(PredId, ProcId)) then
                Type = disj_rec_base,
                Base = GoalBList,
                Rec = GoalAList
            else if is_recursive_case(GoalBList, proc(PredId, ProcId)) then
                Type = disj_base_rec,
                Base = GoalAList,
                Rec = GoalBList
            else
                fail
            ),
            BaseInstMap = InitialInstMap,
            RecInstMap = InitialInstMap
        else
            fail
        )
    ;
        GoalExpr = if_then_else(_Vars, Cond, Then, Else),
        Cond = hlds_goal(_CondGoalExpr, CondGoalInfo),
        CondInstMapDelta = goal_info_get_instmap_delta(CondGoalInfo),

        goal_to_conj_list(Then, GoalAList),
        goal_to_conj_list(Else, GoalBList),
        ( if is_recursive_case(GoalAList, proc(PredId, ProcId)) then
            Type = ite_rec_base,
            Base = GoalBList,
            Rec = GoalAList,

            BaseInstMap = InitialInstMap,
            apply_instmap_delta(CondInstMapDelta, InitialInstMap, RecInstMap)
        else if is_recursive_case(GoalBList, proc(PredId, ProcId)) then
            Type = ite_base_rec,
            Base = GoalAList,
            Rec = GoalBList,

            RecInstMap = InitialInstMap,
            apply_instmap_delta(CondInstMapDelta, InitialInstMap, BaseInstMap)
        else
            fail
        )
    ).

    % is_recursive_case(Gs, Id) is true iff the list of goals, Gs,
    % contains a call to the procedure specified by Id, where the call
    % is located in a position that can be used by the transformation
    % (i.e. not hidden in a compound goal).
    %
:- pred is_recursive_case(list(hlds_goal)::in, pred_proc_id::in) is semidet.

is_recursive_case(Goals, proc(PredId, ProcId)) :-
    list.append(_Initial, [RecursiveCall | _Final], Goals),
    RecursiveCall = hlds_goal(plain_call(PredId, ProcId, _, _, _, _), _).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % The store info is folded over the list of goals which
    % represent the base and recursive case conjunctions.
:- type store_info
    --->    store_info(
                store_loc       :: int,
                                % The location of the goal in the conjunction.
                store_instmap   :: instmap,
                store_goals     :: accu_goal_store
            ).

    % Initialise the goal_store, which will hold the C_{a,b} goals.
    %
:- func initialize_goal_store(list(hlds_goal), instmap,
    list(hlds_goal), instmap) = accu_goal_store.

initialize_goal_store(Rec, RecInstMap, Base, BaseInstMap) = C :-
    goal_store_init(C0),
    list.foldl3(accu_store(accu_rec), Rec,
        1, _, RecInstMap, _, C0, C1),
    list.foldl3(accu_store(accu_base), Base,
        1, _, BaseInstMap, _, C1, C).

:- pred accu_store(accu_case::in, hlds_goal::in,
    int::in, int::out, instmap::in, instmap::out,
    accu_goal_store::in, accu_goal_store::out) is det.

accu_store(Case, Goal, !N, !InstMap, !GoalStore) :-
    Id = accu_goal_id(Case, !.N),
    goal_store_det_insert(Id, stored_goal(Goal, !.InstMap), !GoalStore),

    !:N = !.N + 1,
    Goal = hlds_goal(_, GoalInfo),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
    apply_instmap_delta(InstMapDelta, !InstMap).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Determine the k's which are recursive calls.
    % Note that this doesn't find recursive calls which are `hidden'
    % in compound goals, this is not a problem as currently we can't use
    % these to do transformation.
    %
:- pred identify_recursive_calls(pred_id::in, proc_id::in,
    accu_goal_store::in, list(accu_goal_id)::out) is det.

identify_recursive_calls(PredId, ProcId, GoalStore, Ids) :-
    P =
        ( pred(Key::out) is nondet :-
            goal_store_member(GoalStore, Key, stored_goal(Goal, _InstMap)),
            Key = accu_goal_id(accu_rec, _),
            Goal = hlds_goal(plain_call(PredId, ProcId, _, _, _, _), _)
        ),
    solutions.solutions(P, Ids).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Determine the variables which are members of the sets Out and Out',
    % and initialize the substitutions between the two sets.
    %
    % This is done by identifying those variables whose instantiatedness change
    % in the goals after the recursive call and are headvars.
    %
    % Note that we are only identifying the output variables which will need
    % to be accumulated, as there may be other output variables which are
    % produced prior to the recursive call.
    %
:- pred identify_out_and_out_prime(module_info::in, vartypes::in, instmap::in,
    accu_goal_id::in, list(hlds_goal)::in,
    list(prog_var)::in, list(prog_var)::out, list(prog_var)::out,
    accu_subst::out, accu_subst::out) is det.

identify_out_and_out_prime(ModuleInfo, VarTypes, InitialInstMap, GoalId,
        Rec, HeadVars, Out, OutPrime, HeadToCallSubst, CallToHeadSubst) :-
    GoalId = accu_goal_id(_Case, K),
    ( if
        list.take(K, Rec, InitialGoals),
        list.drop(K-1, Rec, FinalGoals),
        FinalGoals = [hlds_goal(plain_call(_, _, Args, _, _, _), _) | Rest]
    then
        goal_list_instmap_delta(InitialGoals, InitInstMapDelta),
        apply_instmap_delta( InitInstMapDelta,
            InitialInstMap, InstMapBeforeRest),

        goal_list_instmap_delta(Rest, InstMapDelta),
        apply_instmap_delta(InstMapDelta, InstMapBeforeRest, InstMapAfterRest),

        instmap_changed_vars(ModuleInfo, VarTypes,
            InstMapBeforeRest, InstMapAfterRest, ChangedVars),

        assoc_list.from_corresponding_lists(HeadVars, Args, HeadArg0),

        Member =
            ( pred(M::in) is semidet :-
                M = HeadVar - _,
                set_of_var.member(ChangedVars, HeadVar)
            ),
        list.filter(Member, HeadArg0, HeadArg),
        list.map(fst, HeadArg, Out),
        list.map(snd, HeadArg, OutPrime),

        map.from_assoc_list(HeadArg, HeadToCallSubst),

        list.map((pred(X-Y::in, Y-X::out) is det), HeadArg, ArgHead),
        map.from_assoc_list(ArgHead, CallToHeadSubst)
    else
        unexpected($pred, "test failed")
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % For each goal after the recursive call, we place that goal
    % into a set according to what properties that goal has.
    % For the definition of what goes into each set, inspect the documentation
    % for the functions named before, assoc, and so on.
    %
:- type accu_sets
    --->    accu_sets(
                as_before           ::  set(accu_goal_id),
                as_assoc            ::  set(accu_goal_id),
                as_construct_assoc  ::  set(accu_goal_id),
                as_construct        ::  set(accu_goal_id),
                as_update           ::  set(accu_goal_id),
                as_reject           ::  set(accu_goal_id)
            ).

    % Stage 1 is responsible for identifying which goals are associative,
    % which can be moved before the recursive call and so on.
    %
:- pred accu_stage1(module_info::in, vartypes::in, bool::in,
    maybe_opt_lcmc_accumulator::in, accu_goal_id::in, int::in,
    accu_goal_store::in, accu_sets::out) is semidet.

accu_stage1(ModuleInfo, VarTypes, FullyStrict, DoLCMC, GoalId, M, GoalStore,
        Sets) :-
    GoalId = accu_goal_id(Case, K),
    NextGoalId = accu_goal_id(Case, K + 1),
    accu_sets_init(Sets0),
    accu_stage1_2(ModuleInfo, VarTypes, FullyStrict, NextGoalId, K, M,
        GoalStore, Sets0, Sets1),
    Sets1 = accu_sets(Before, Assoc,
        ConstructAssoc, Construct, Update, Reject),
    Sets = accu_sets(Before `set.union` set_upto(Case, K - 1), Assoc,
        ConstructAssoc, Construct, Update, Reject),

    % Continue the transformation only if the set reject is empty and
    % the set assoc or update contains something that needs to be moved
    % before the recursive call.
    set.is_empty(Reject),
    (
        not set.is_empty(Assoc)
    ;
        not set.is_empty(Update)
    ),
    (
        DoLCMC = do_not_opt_lcmc_accumulator,
        % If LCMC is not turned on, then there must be no construction
        % unifications after the recursive call.
        set.is_empty(Construct),
        set.is_empty(ConstructAssoc)
    ;
        DoLCMC = opt_lcmc_accumulator
    ).

    % For each goal after the recursive call decide which set
    % the goal belongs to.
    %
:- pred accu_stage1_2(module_info::in, vartypes::in, bool::in,
    accu_goal_id::in, int::in, int::in, accu_goal_store::in,
    accu_sets::in, accu_sets::out) is det.

accu_stage1_2(ModuleInfo, VarTypes, FullyStrict, GoalId, K, M, GoalStore,
        !Sets) :-
    GoalId = accu_goal_id(Case, I),
    NextGoalId = accu_goal_id(Case, I + 1),
    ( if I > M then
        true
    else
        ( if
            accu_before(ModuleInfo, VarTypes, FullyStrict, GoalId, K,
                GoalStore, !.Sets)
        then
            !Sets ^ as_before := set.insert(!.Sets ^ as_before, GoalId),
            accu_stage1_2(ModuleInfo, VarTypes, FullyStrict, NextGoalId, K, M,
                GoalStore, !Sets)
        else if
            accu_assoc(ModuleInfo, VarTypes, FullyStrict, GoalId, K,
                GoalStore, !.Sets)
        then
            !Sets ^ as_assoc := set.insert(!.Sets ^ as_assoc, GoalId),
            accu_stage1_2(ModuleInfo, VarTypes, FullyStrict, NextGoalId, K, M,
                GoalStore, !Sets)
        else if
            accu_construct(ModuleInfo, VarTypes, FullyStrict, GoalId, K,
                GoalStore, !.Sets)
        then
            !Sets ^ as_construct := set.insert(!.Sets ^ as_construct, GoalId),
            accu_stage1_2(ModuleInfo, VarTypes, FullyStrict, NextGoalId, K, M,
                GoalStore, !Sets)
        else if
            accu_construct_assoc(ModuleInfo, VarTypes, FullyStrict, GoalId, K,
                GoalStore, !.Sets)
        then
            !Sets ^ as_construct_assoc :=
                set.insert(!.Sets ^ as_construct_assoc, GoalId),
            accu_stage1_2(ModuleInfo, VarTypes, FullyStrict, NextGoalId, K, M,
                GoalStore, !Sets)
        else if
            accu_update(ModuleInfo, VarTypes, FullyStrict, GoalId, K,
                GoalStore, !.Sets)
        then
            !Sets ^ as_update := set.insert(!.Sets ^ as_update, GoalId),
            accu_stage1_2(ModuleInfo, VarTypes, FullyStrict, NextGoalId, K, M,
                GoalStore, !Sets)
        else
            !Sets ^ as_reject := set.insert(!.Sets ^ as_reject, GoalId)
        )
    ).

%---------------------------------------------------------------------------%

:- pred accu_sets_init(accu_sets::out) is det.

accu_sets_init(Sets) :-
    set.init(EmptySet),
    Before = EmptySet,
    Assoc = EmptySet,
    ConstructAssoc = EmptySet,
    Construct = EmptySet,
    Update = EmptySet,
    Reject = EmptySet,
    Sets = accu_sets(Before, Assoc, ConstructAssoc, Construct, Update, Reject).

    % set_upto(Case, K) returns the set
    % {accu_goal_id(Case, 1) .. accu_goal_id(Case, K)}.
    %
:- func set_upto(accu_case, int) = set(accu_goal_id).

set_upto(Case, K) = Set :-
    ( if K =< 0 then
        set.init(Set)
    else
        Set0 = set_upto(Case, K - 1),
        set.insert(accu_goal_id(Case, K), Set0, Set)
    ).

%---------------------------------------------------------------------------%

    % A goal is a member of the before set iff the goal only depends on goals
    % which are before the recursive call or can be moved before the recursive
    % call (member of the before set).
    %
:- pred accu_before(module_info::in, vartypes::in, bool::in,
    accu_goal_id::in, int::in, accu_goal_store::in, accu_sets::in) is semidet.

accu_before(ModuleInfo, VarTypes, FullyStrict, GoalId, K, GoalStore, Sets) :-
    GoalId = accu_goal_id(Case, _I),
    Before = Sets ^ as_before,
    goal_store_lookup(GoalStore, GoalId, stored_goal(LaterGoal, LaterInstMap)),
    (
        member_lessthan_goalid(GoalStore, GoalId, LessThanGoalId,
            stored_goal(EarlierGoal, EarlierInstMap)),
        not can_reorder_goals_old(ModuleInfo, VarTypes, FullyStrict,
            EarlierInstMap, EarlierGoal, LaterInstMap, LaterGoal)
    )
    =>
    (
        set.member(LessThanGoalId, set_upto(Case, K - 1) `union` Before)
    ).

    % A goal is a member of the assoc set iff the goal only depends on goals
    % upto and including the recursive call and goals which can be moved
    % before the recursive call (member of the before set) AND the goal
    % is associative.
    %
:- pred accu_assoc(module_info::in, vartypes::in, bool::in,
    accu_goal_id::in, int::in, accu_goal_store::in, accu_sets::in) is semidet.

accu_assoc(ModuleInfo, VarTypes, FullyStrict, GoalId, K, GoalStore, Sets) :-
    GoalId = accu_goal_id(Case, _I),
    Before = Sets ^ as_before,
    goal_store_lookup(GoalStore, GoalId, stored_goal(LaterGoal, LaterInstMap)),
    LaterGoal = hlds_goal(plain_call(PredId, _, Args, _, _, _), _),
    accu_is_associative(ModuleInfo, PredId, Args, _),
    (
        % XXX LessThanGoalId was _N - J, not N - J: it ignored the case.
        % See the diff with the previous version.
        member_lessthan_goalid(GoalStore, GoalId, LessThanGoalId,
            stored_goal(EarlierGoal, EarlierInstMap)),
        not can_reorder_goals_old(ModuleInfo, VarTypes, FullyStrict,
            EarlierInstMap, EarlierGoal, LaterInstMap, LaterGoal)
    )
    =>
    (
        set.member(LessThanGoalId, set_upto(Case, K) `union` Before)
    ).

    % A goal is a member of the construct set iff the goal only depends
    % on goals upto and including the recursive call and goals which
    % can be moved before the recursive call (member of the before set)
    % AND the goal is construction unification.
    %
:- pred accu_construct(module_info::in, vartypes::in, bool::in,
    accu_goal_id::in, int::in, accu_goal_store::in, accu_sets::in) is semidet.

accu_construct(ModuleInfo, VarTypes, FullyStrict, GoalId, K, GoalStore,
        Sets) :-
    GoalId = accu_goal_id(Case, _I),
    Before = Sets ^ as_before,
    Construct = Sets ^ as_construct,
    goal_store_lookup(GoalStore, GoalId, stored_goal(LaterGoal, LaterInstMap)),
    LaterGoal = hlds_goal(unify(_, _, _, Unify, _), _GoalInfo),
    Unify = construct(_, _, _, _, _, _, _),
    (
        % XXX LessThanGoalId was _N - J, not N - J: it ignored the case.
        % See the diff with the previous version.
        member_lessthan_goalid(GoalStore, GoalId, LessThanGoalId,
            stored_goal(EarlierGoal, EarlierInstMap)),
        not can_reorder_goals_old(ModuleInfo, VarTypes, FullyStrict,
            EarlierInstMap, EarlierGoal, LaterInstMap, LaterGoal)
    )
    =>
    (
        set.member(LessThanGoalId,
            set_upto(Case, K) `union` Before `union` Construct)
    ).

    % A goal is a member of the construct_assoc set iff the goal depends only
    % on goals upto and including the recursive call and goals which can be
    % moved before the recursive call (member of the before set) and goals
    % which are associative AND the goal is construction unification AND
    % there is only one member of the assoc set which the construction
    % unification depends on AND the construction unification can be expressed
    % as a call to the member of the assoc set which the construction
    % unification depends on.
    %
:- pred accu_construct_assoc(module_info::in, vartypes::in, bool::in,
    accu_goal_id::in, int::in, accu_goal_store::in, accu_sets::in) is semidet.

accu_construct_assoc(ModuleInfo, VarTypes, FullyStrict,
        GoalId, K, GoalStore, Sets) :-
    GoalId = accu_goal_id(Case, _I),
    Before = Sets ^ as_before,
    Assoc = Sets ^ as_assoc,
    ConstructAssoc = Sets ^ as_construct_assoc,
    goal_store_lookup(GoalStore, GoalId, stored_goal(LaterGoal, LaterInstMap)),
    LaterGoal = hlds_goal(unify(_, _, _, Unify, _), _GoalInfo),
    Unify = construct(_, ConsId, _, _, _, _, _),

    goal_store_all_ancestors(GoalStore, GoalId, VarTypes, ModuleInfo,
        FullyStrict, Ancestors),

    set.is_singleton(Assoc `intersect` Ancestors, AssocId),
    goal_store_lookup(GoalStore, AssocId,
        stored_goal(AssocGoal, _AssocInstMap)),
    AssocGoal = hlds_goal(plain_call(PredId, _, _, _, _, _), _),

    is_associative_construction(ModuleInfo, PredId, ConsId),
    (
        % XXX LessThanGoalId was _N - J, not N - J: it ignored the case.
        % See the diff with the previous version.
        member_lessthan_goalid(GoalStore, GoalId, LessThanGoalId,
            stored_goal(EarlierGoal, EarlierInstMap)),
        not can_reorder_goals_old(ModuleInfo, VarTypes, FullyStrict,
            EarlierInstMap, EarlierGoal, LaterInstMap, LaterGoal)
    )
    =>
    (
        set.member(LessThanGoalId,
            set_upto(Case, K) `union` Before `union` Assoc
            `union` ConstructAssoc)
    ).

    % A goal is a member of the update set iff the goal only depends
    % on goals upto and including the recursive call and goals which
    % can be moved before the recursive call (member of the before set)
    % AND the goal updates some state.
    %
:- pred accu_update(module_info::in, vartypes::in, bool::in,
    accu_goal_id::in, int::in, accu_goal_store::in, accu_sets::in) is semidet.

accu_update(ModuleInfo, VarTypes, FullyStrict, GoalId, K, GoalStore, Sets) :-
    GoalId = accu_goal_id(Case, _I),
    Before = Sets ^ as_before,
    goal_store_lookup(GoalStore, GoalId, stored_goal(LaterGoal, LaterInstMap)),
    LaterGoal = hlds_goal(plain_call(PredId, _, Args, _, _, _), _),
    accu_is_update(ModuleInfo, PredId, Args, _),
    (
        % XXX LessThanGoalId was _N - J, not N - J: it ignored the case.
        % See the diff with the previous version.
        member_lessthan_goalid(GoalStore, GoalId, LessThanGoalId,
            stored_goal(EarlierGoal, EarlierInstMap)),
        not can_reorder_goals_old(ModuleInfo, VarTypes, FullyStrict,
            EarlierInstMap, EarlierGoal, LaterInstMap, LaterGoal)
    )
    =>
    (
        set.member(LessThanGoalId, set_upto(Case, K) `union` Before)
    ).

    % member_lessthan_goalid(GS, IdA, IdB, GB) is true iff the goal_id, IdB,
    % and its associated goal, GB, is a member of the goal_store, GS,
    % and IdB is less than IdA.
    %
:- pred member_lessthan_goalid(accu_goal_store::in,
    accu_goal_id::in, accu_goal_id::out, stored_goal::out) is nondet.

member_lessthan_goalid(GoalStore, GoalId, LessThanGoalId, LessThanGoal) :-
    goal_store_member(GoalStore, LessThanGoalId, LessThanGoal),
    GoalId = accu_goal_id(Case, I),
    LessThanGoalId = accu_goal_id(Case, J),
    J < I.

%---------------------------------------------------------------------------%

:- type accu_assoc
    --->    accu_assoc(
                set_of_progvar,     % the associative input args
                prog_var,           % the corresponding output arg
                bool                % is the predicate commutative?
            ).

    % If accu_is_associative is true, it returns the two arguments which are
    % associative and the variable which depends on those two arguments,
    % and an indicator of whether or not the predicate is commutative.
    %
:- pred accu_is_associative(module_info::in, pred_id::in, list(prog_var)::in,
    accu_assoc::out) is semidet.

accu_is_associative(ModuleInfo, PredId, Args, Result) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_assertions(PredInfo, Assertions),
    AssertionsList = set.to_sorted_list(Assertions),
    associativity_assertion(ModuleInfo, AssertionsList, Args,
        AssociativeVarsOutputVar),
    ( if
        commutativity_assertion(ModuleInfo, AssertionsList, Args,
            _CommutativeVars)
    then
        IsCommutative = yes
    else
        IsCommutative = no
    ),
    AssociativeVarsOutputVar =
        associative_vars_output_var(AssociativeVars, OutputVar),
    Result = accu_assoc(AssociativeVars, OutputVar, IsCommutative).

    % Does there exist one (and only one) associativity assertion for the
    % current predicate?
    % The 'and only one condition' is required because we currently
    % do not handle the case of predicates which have individual parts
    % which are associative, because then we do not know which variable
    % is descended from which.
    %
:- pred associativity_assertion(module_info::in, list(assert_id)::in,
    list(prog_var)::in, associative_vars_output_var::out) is semidet.

associativity_assertion(ModuleInfo, [AssertId | AssertIds], Args0,
        AssociativeVarsOutputVar) :-
    ( if
        assertion.is_associativity_assertion(ModuleInfo, AssertId,
            Args0, AssociativeVarsOutputVarPrime)
    then
        AssociativeVarsOutputVar = AssociativeVarsOutputVarPrime,
        not associativity_assertion(ModuleInfo, AssertIds, Args0, _)
    else
        associativity_assertion(ModuleInfo, AssertIds, Args0,
            AssociativeVarsOutputVar)
    ).

    % Does there exist one (and only one) commutativity assertion for the
    % current predicate?
    % The 'and only one condition' is required because we currently
    % do not handle the case of predicates which have individual
    % parts which are commutative, because then we do not know which variable
    % is descended from which.
    %
:- pred commutativity_assertion(module_info::in,list(assert_id)::in,
    list(prog_var)::in, set_of_progvar::out) is semidet.

commutativity_assertion(ModuleInfo, [AssertId | AssertIds], Args0,
        CommutativeVars) :-
    ( if
        assertion.is_commutativity_assertion(ModuleInfo, AssertId,
            Args0, CommutativeVarsPrime)
    then
        CommutativeVars = CommutativeVarsPrime,
        not commutativity_assertion(ModuleInfo, AssertIds, Args0, _)
    else
        commutativity_assertion(ModuleInfo, AssertIds, Args0,
            CommutativeVars)
    ).

%---------------------------------------------------------------------------%

    % Does the current predicate update some state?
    %
:- pred accu_is_update(module_info::in, pred_id::in, list(prog_var)::in,
    state_update_vars::out) is semidet.

accu_is_update(ModuleInfo, PredId, Args, ResultStateVars) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_assertions(PredInfo, Assertions),
    list.filter_map(
        ( pred(AssertId::in, StateVars::out) is semidet :-
            assertion.is_update_assertion(ModuleInfo, AssertId,
                PredId, Args, StateVars)
        ),
        set.to_sorted_list(Assertions), Result),
    % XXX Maybe we should just match on the first result,
    % just in case there are duplicate promises.
    Result = [ResultStateVars].

%---------------------------------------------------------------------------%

    % Can the construction unification be expressed as a call to the
    % specified predicate.
    %
:- pred is_associative_construction(module_info::in, pred_id::in, cons_id::in)
    is semidet.

is_associative_construction(ModuleInfo, PredId, ConsId) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    pred_info_get_assertions(PredInfo, Assertions),
    list.filter(
        ( pred(AssertId::in) is semidet :-
            assertion.is_construction_equivalence_assertion(ModuleInfo,
                AssertId, ConsId, PredId)
        ),
        set.to_sorted_list(Assertions), Result),
    Result = [_ | _].

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- type accu_substs
    --->    accu_substs(
                acc_var_subst       :: accu_subst,
                rec_call_subst      :: accu_subst,
                assoc_call_subst    :: accu_subst,
                update_subst        :: accu_subst
            ).

:- type accu_base
    --->    accu_base(
                % goals which initialize update
                init_update         :: set(accu_goal_id),

                % goals which initialize assoc
                init_assoc          :: set(accu_goal_id),

                % other goals
                other               :: set(accu_goal_id)
            ).

    % Stage 2 is responsible for identifying the substitutions which
    % are needed to mimic the unfold/fold process that was used as
    % the justification of the algorithm in the paper.
    % It is also responsible for ensuring that the reordering of arguments
    % doesn't worsen the big-O complexity of the procedure.
    % It also divides the base case into goals that initialize the
    % variables used by the update goals, and those used by the assoc
    % goals and then all the rest.
    %
:- pred accu_stage2(module_info::in, proc_info::in,
    accu_goal_id::in, accu_goal_store::in, accu_sets::in,
    list(prog_var)::in, list(prog_var)::in, prog_varset::out, vartypes::out,
    list(prog_var)::out, accu_base::out, list(pair(prog_var))::out,
    accu_substs::out, accu_goal_store::out, list(accu_warning)::out)
    is semidet.

accu_stage2(ModuleInfo, ProcInfo0, GoalId, GoalStore, Sets, OutPrime, Out,
        !:VarSet, !:VarTypes, Accs, BaseCase, BasePairs, !:Substs,
        CS, Warnings) :-
    Sets = accu_sets(Before0, Assoc, ConstructAssoc, Construct, Update, _),
    GoalId = accu_goal_id(Case, K),
    Before = Before0 `union` set_upto(Case, K-1),

    % Note Update set is not placed in the after set, as the after set is used
    % to determine the variables that need to be accumulated for the
    % associative calls.
    After = Assoc `union` ConstructAssoc `union` Construct,

    P =
        ( pred(Id::in, Set0::in, Set::out) is det :-
            goal_store_lookup(GoalStore, Id, stored_goal(Goal, _InstMap)),
            Goal = hlds_goal(_GoalExpr, GoalInfo),
            NonLocals = goal_info_get_nonlocals(GoalInfo),
            set_of_var.union(NonLocals, Set0, Set)
        ),
    list.foldl(P, set.to_sorted_list(Before),
        set_of_var.init, BeforeNonLocals),
    list.foldl(P, set.to_sorted_list(After),
        set_of_var.init, AfterNonLocals),
    InitAccs = set_of_var.intersect(BeforeNonLocals, AfterNonLocals),

    proc_info_get_varset(ProcInfo0, !:VarSet),
    proc_info_get_vartypes(ProcInfo0, !:VarTypes),

    accu_substs_init(set_of_var.to_sorted_list(InitAccs), !VarSet, !VarTypes,
        !:Substs),

    set_of_var.list_to_set(OutPrime, OutPrimeSet),
    accu_process_assoc_set(ModuleInfo, GoalStore, set.to_sorted_list(Assoc),
        OutPrimeSet, !Substs, !VarSet, !VarTypes, CS, Warnings),

    accu_process_update_set(ModuleInfo, GoalStore, set.to_sorted_list(Update),
        OutPrimeSet, !Substs, !VarSet, !VarTypes, UpdateOut, UpdateAccOut,
        BasePairs),

    Accs = set_of_var.to_sorted_list(InitAccs) ++ UpdateAccOut,

    accu_divide_base_case(ModuleInfo, !.VarTypes, GoalStore, UpdateOut, Out,
        UpdateBase, AssocBase, OtherBase),

    BaseCase = accu_base(UpdateBase, AssocBase, OtherBase).

%---------------------------------------------------------------------------%

:- pred accu_substs_init(list(prog_var)::in, prog_varset::in, prog_varset::out,
    vartypes::in, vartypes::out, accu_substs::out) is det.

accu_substs_init(InitAccs, !VarSet, !VarTypes, Substs) :-
    map.init(Subst),
    acc_var_subst_init(InitAccs, !VarSet, !VarTypes, AccVarSubst),
    RecCallSubst = Subst,
    AssocCallSubst = Subst,
    UpdateSubst = Subst,
    Substs = accu_substs(AccVarSubst, RecCallSubst, AssocCallSubst,
        UpdateSubst).

    % Initialise the acc_var_subst to be from Var to A_Var where Var is a
    % member of InitAccs and A_Var is a fresh variable of the same type of Var.
    %
:- pred acc_var_subst_init(list(prog_var)::in,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    accu_subst::out) is det.

acc_var_subst_init([], !VarSet, !VarTypes, map.init).
acc_var_subst_init([Var | Vars], !VarSet, !VarTypes, Subst) :-
    create_new_var(Var, "A_", AccVar, !VarSet, !VarTypes),
    acc_var_subst_init(Vars, !VarSet, !VarTypes, Subst0),
    map.det_insert(Var, AccVar, Subst0, Subst).

    % Create a fresh variable which is the same type as the old variable
    % and has the same name except that it begins with the prefix.
    %
:- pred create_new_var(prog_var::in, string::in, prog_var::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

create_new_var(OldVar, Prefix, NewVar, !VarSet, !VarTypes) :-
    varset.lookup_name(!.VarSet, OldVar, OldName),
    string.append(Prefix, OldName, NewName),
    varset.new_named_var(NewName, NewVar, !VarSet),
    lookup_var_type(!.VarTypes, OldVar, Type),
    add_var_type(NewVar, Type, !VarTypes).

%---------------------------------------------------------------------------%

    % For each member of the assoc set determine the substitutions needed,
    % and also check the efficiency of the procedure isn't worsened
    % by reordering the arguments to a call.
    %
:- pred accu_process_assoc_set(module_info::in, accu_goal_store::in,
    list(accu_goal_id)::in, set_of_progvar::in,
    accu_substs::in, accu_substs::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    accu_goal_store::out, list(accu_warning)::out) is semidet.

accu_process_assoc_set(_ModuleInfo, _GS, [], _OutPrime, !Substs,
        !VarSet, !VarTypes, CS, []) :-
    goal_store_init(CS).
accu_process_assoc_set(ModuleInfo, GS, [Id | Ids], OutPrime, !Substs,
        !VarSet, !VarTypes, CS, Warnings) :-
    !.Substs = accu_substs(AccVarSubst, RecCallSubst0, AssocCallSubst0,
        UpdateSubst),

    lookup_call(GS, Id, stored_goal(Goal, InstMap)),

    Goal = hlds_goal(plain_call(PredId, _, Args, _, _, _), GoalInfo),
    accu_is_associative(ModuleInfo, PredId, Args, AssocInfo),
    AssocInfo = accu_assoc(Vars, AssocOutput, IsCommutative),
    OutPrimeVars = set_of_var.intersect(Vars, OutPrime),
    set_of_var.is_singleton(OutPrimeVars, DuringAssocVar),
    set_of_var.is_singleton(set_of_var.difference(Vars, OutPrimeVars),
        BeforeAssocVar),

    map.lookup(AccVarSubst, BeforeAssocVar, AccVar),
    create_new_var(BeforeAssocVar, "NewAcc_", NewAcc, !VarSet, !VarTypes),

    map.det_insert(DuringAssocVar, AccVar, AssocCallSubst0, AssocCallSubst1),
    map.det_insert(AssocOutput, NewAcc, AssocCallSubst1, AssocCallSubst),
    map.det_insert(DuringAssocVar, AssocOutput, RecCallSubst0, RecCallSubst1),
    map.det_insert(BeforeAssocVar, NewAcc, RecCallSubst1, RecCallSubst),

    !:Substs = accu_substs(AccVarSubst, RecCallSubst, AssocCallSubst,
        UpdateSubst),

    % ONLY swap the order of the variables if the goal is
    % associative and not commutative.
    (
        IsCommutative = yes,
        CSGoal = stored_goal(Goal, InstMap),
        CurWarnings = []
    ;
        IsCommutative = no,

        % Ensure that the reordering doesn't cause a efficiency problem.
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        ModuleName = pred_info_module(PredInfo),
        PredName = pred_info_name(PredInfo),
        Arity = pred_info_orig_arity(PredInfo),
        ( if accu_has_heuristic(ModuleName, PredName, Arity) then
            % Only do the transformation if the accumulator variable is
            % *not* in a position where it will control the running time
            % of the predicate.
            accu_heuristic(ModuleName, PredName, Arity, Args,
                PossibleDuringAssocVars),
            set_of_var.member(PossibleDuringAssocVars, DuringAssocVar),
            CurWarnings = []
        else
            ProgContext = goal_info_get_context(GoalInfo),
            CurWarnings = [accu_warn(ProgContext, PredId, BeforeAssocVar,
                DuringAssocVar)]
        ),
        % Swap the arguments.
        [A, B] = set_of_var.to_sorted_list(Vars),
        map.from_assoc_list([A - B, B - A], Subst),
        rename_some_vars_in_goal(Subst, Goal, SwappedGoal),
        CSGoal = stored_goal(SwappedGoal, InstMap)
    ),

    accu_process_assoc_set(ModuleInfo, GS, Ids, OutPrime, !Substs,
        !VarSet, !VarTypes, CS0, Warnings0),
    goal_store_det_insert(Id, CSGoal, CS0, CS),
    Warnings = Warnings0 ++ CurWarnings.

:- pred accu_has_heuristic(module_name::in, string::in, arity::in) is semidet.

accu_has_heuristic(unqualified("list"), "append", 3).

    % heuristic returns the set of which head variables are important
    % in the running time of the predicate.
    %
:- pred accu_heuristic(module_name::in, string::in, arity::in,
    list(prog_var)::in, set_of_progvar::out) is semidet.

accu_heuristic(unqualified("list"), "append", 3, [_Typeinfo, A, _B, _C],
        Set) :-
    set_of_var.make_singleton(A, Set).

%---------------------------------------------------------------------------%

    % For each member of the update set determine the substitutions needed
    % (creating the accumulator variables when needed).
    % Also associate with each Output variable which accumulator variable
    % to get the result from.
    %
:- pred accu_process_update_set(module_info::in, accu_goal_store::in,
    list(accu_goal_id)::in, set_of_progvar::in,
    accu_substs::in, accu_substs::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out,
    list(prog_var)::out, list(prog_var)::out, list(pair(prog_var))::out)
    is semidet.

accu_process_update_set(_ModuleInfo, _GS, [], _OutPrime, !Substs,
        !VarSet, !VarTypes, [], [], []).
accu_process_update_set(ModuleInfo, GS, [Id | Ids], OutPrime, !Substs,
        !VarSet, !VarTypes, StateOutputVars, Accs, BasePairs) :-
    !.Substs = accu_substs(AccVarSubst0, RecCallSubst0, AssocCallSubst,
        UpdateSubst0),
    lookup_call(GS, Id, stored_goal(Goal, _InstMap)),

    Goal = hlds_goal(plain_call(PredId, _, Args, _, _, _), _GoalInfo),
    accu_is_update(ModuleInfo, PredId, Args, StateVars),
    StateVars = state_update_vars(StateVarA, StateVarB),

    ( if set_of_var.member(OutPrime, StateVarA) then
        StateInputVar = StateVarA,
        StateOutputVar = StateVarB
    else
        StateInputVar = StateVarB,
        StateOutputVar = StateVarA
    ),

    create_new_var(StateInputVar, "Acc_", Acc0, !VarSet, !VarTypes),
    create_new_var(StateOutputVar, "Acc_", Acc, !VarSet, !VarTypes),

    map.det_insert(StateInputVar, Acc0, UpdateSubst0, UpdateSubst1),
    map.det_insert(StateOutputVar, Acc, UpdateSubst1, UpdateSubst),
    map.det_insert(StateInputVar, StateOutputVar, RecCallSubst0, RecCallSubst),
    map.det_insert(Acc, Acc0, AccVarSubst0, AccVarSubst),
    !:Substs = accu_substs(AccVarSubst, RecCallSubst, AssocCallSubst,
        UpdateSubst),

    accu_process_update_set(ModuleInfo, GS, Ids, OutPrime, !Substs,
        !VarSet, !VarTypes, StateOutputVars0, Accs0, BasePairs0),

    % Rather then concatenating to start of the list we concatenate to the end
    % of the list. This allows the accumulator introduction to be applied
    % as the heuristic will succeed (remember after transforming the two
    % input variables will have their order swapped, so they must be in the
    % inefficient order to start with)

    StateOutputVars = StateOutputVars0 ++ [StateOutputVar],
    Accs = Accs0 ++ [Acc],
    BasePairs = BasePairs0 ++ [StateOutputVar - Acc0].

%---------------------------------------------------------------------------%

    % divide_base_case(UpdateOut, Out, U, A, O) is true iff given the output
    % variables which are instantiated by update goals, UpdateOut, and all
    % the variables that need to be accumulated, Out, divide the base case up
    % into three sets, those base case goals which initialize the variables
    % used by update calls, U, those which initialize variables used by
    % assoc calls, A, and the rest of the goals, O. Note that the sets
    % are not necessarily disjoint, as the result of a goal may be used
    % to initialize a variable in both U and A, so both U and A will contain
    % the same goal_id.
    %
:- pred accu_divide_base_case(module_info::in, vartypes::in,
    accu_goal_store::in, list(prog_var)::in, list(prog_var)::in,
    set(accu_goal_id)::out, set(accu_goal_id)::out, set(accu_goal_id)::out)
    is det.

accu_divide_base_case(ModuleInfo, VarTypes, C, UpdateOut, Out,
        UpdateBase, AssocBase, OtherBase) :-
    list.delete_elems(Out, UpdateOut, AssocOut),

    list.map(accu_related(ModuleInfo, VarTypes, C), UpdateOut, UpdateBaseList),
    list.map(accu_related(ModuleInfo, VarTypes, C), AssocOut, AssocBaseList),
    UpdateBase = set.power_union(set.list_to_set(UpdateBaseList)),
    AssocBase = set.power_union(set.list_to_set(AssocBaseList)),

    Set = base_case_ids_set(C) `difference` (UpdateBase `union` AssocBase),
    set.to_sorted_list(Set, List),

    list.map(
        ( pred(GoalId::in, Ancestors::out) is det :-
            goal_store_all_ancestors(C, GoalId, VarTypes,
                ModuleInfo, no, Ancestors)
        ), List, OtherBaseList),

    OtherBase = set.list_to_set(List) `union`
        (base_case_ids_set(C) `intersect`
        set.power_union(set.list_to_set(OtherBaseList))).

    % accu_related(ModuleInfo, VarTypes, GoalStore, Var, Related):
    %
    % From GoalStore, return all the goal_ids, Related, which are needed
    % to initialize Var.
    %
:- pred accu_related(module_info::in, vartypes::in, accu_goal_store::in,
    prog_var::in, set(accu_goal_id)::out) is det.

accu_related(ModuleInfo, VarTypes, GoalStore, Var, Related) :-
    solutions.solutions(
        ( pred(Key::out) is nondet :-
            goal_store_member(GoalStore, Key, stored_goal(Goal, InstMap0)),
            Key = accu_goal_id(accu_base, _),
            Goal = hlds_goal(_GoalExpr, GoalInfo),
            InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
            apply_instmap_delta(InstMapDelta, InstMap0, InstMap),
            instmap_changed_vars(ModuleInfo, VarTypes,
                InstMap0, InstMap, ChangedVars),
            set_of_var.is_singleton(ChangedVars, Var)
        ), Ids),
    (
        Ids = [],
        unexpected($pred, "no Id")
    ;
        Ids = [Id],
        goal_store_all_ancestors(GoalStore, Id, VarTypes, ModuleInfo, no,
            Ancestors),
        list.filter((pred(accu_goal_id(accu_base, _)::in) is semidet),
            set.to_sorted_list(set.insert(Ancestors, Id)), RelatedList),
        Related = set.list_to_set(RelatedList)
    ;
        Ids = [_, _ | _],
        unexpected($pred, "more than one Id")
    ).

%---------------------------------------------------------------------------%

:- inst stored_goal_plain_call for goal_store.stored_goal/0
    --->    stored_goal(goal_plain_call, ground).

    % Do a goal_store_lookup where the result is known to be a call.
    %
:- pred lookup_call(accu_goal_store::in, accu_goal_id::in,
    stored_goal::out(stored_goal_plain_call)) is det.

lookup_call(GoalStore, Id, stored_goal(Call, InstMap)) :-
    goal_store_lookup(GoalStore, Id, stored_goal(Goal, InstMap)),
    ( if
        Goal = hlds_goal(GoalExpr, GoalInfo),
        GoalExpr = plain_call(_, _, _, _, _, _)
    then
        Call = hlds_goal(GoalExpr, GoalInfo)
    else
        unexpected($pred, "not a call")
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % accu_stage3 creates the accumulator version of the predicate using
    % the substitutions determined in stage2. It also redefines the
    % original procedure to call the accumulator version of the procedure.
    %
:- pred accu_stage3(accu_goal_id::in, list(prog_var)::in, prog_varset::in,
    vartypes::in, accu_goal_store::in, accu_goal_store::in,
    accu_substs::in, accu_subst::in, accu_subst::in,
    accu_base::in, list(pair(prog_var))::in, accu_sets::in,
    list(prog_var)::in, top_level::in, pred_id::in, pred_info::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out) is det.

accu_stage3(RecCallId, Accs, VarSet, VarTypes, C, CS, Substs,
        HeadToCallSubst, CallToHeadSubst, BaseCase, BasePairs, Sets, Out,
        TopLevel, OrigPredId, OrigPredInfo, !OrigProcInfo, !ModuleInfo) :-
    acc_proc_info(Accs, VarSet, VarTypes, Substs, !.OrigProcInfo,
        AccTypes, AccProcInfo),
    acc_pred_info(AccTypes, Out, AccProcInfo, OrigPredId, OrigPredInfo,
        AccProcId, AccPredInfo),
    AccName = unqualified(pred_info_name(AccPredInfo)),

    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    predicate_table_insert(AccPredInfo, AccPredId, PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, !ModuleInfo),
    accu_create_goal(RecCallId, Accs, AccPredId, AccProcId, AccName, Substs,
        HeadToCallSubst, CallToHeadSubst, BaseCase, BasePairs, Sets, C, CS,
        OrigBaseGoal, OrigRecGoal, AccBaseGoal, AccRecGoal),

    proc_info_get_goal(!.OrigProcInfo, OrigGoal0),
    accu_top_level(TopLevel, OrigGoal0, OrigBaseGoal, OrigRecGoal,
        AccBaseGoal, AccRecGoal, OrigGoal, AccGoal),

    proc_info_set_goal(OrigGoal, !OrigProcInfo),
    proc_info_set_varset(VarSet, !OrigProcInfo),
    proc_info_set_vartypes(VarTypes, !OrigProcInfo),

    requantify_proc_general(ordinary_nonlocals_no_lambda, !OrigProcInfo),
    update_accumulator_pred(AccPredId, AccProcId, AccGoal, !ModuleInfo).

%---------------------------------------------------------------------------%

    % Construct a proc_info for the introduced predicate.
    %
:- pred acc_proc_info(list(prog_var)::in, prog_varset::in, vartypes::in,
    accu_substs::in, proc_info::in, list(mer_type)::out, proc_info::out)
    is det.

acc_proc_info(Accs0, VarSet, VarTypes, Substs, OrigProcInfo,
        AccTypes, AccProcInfo) :-
    % ProcInfo Stuff that must change.
    proc_info_get_headvars(OrigProcInfo, HeadVars0),
    proc_info_get_argmodes(OrigProcInfo, HeadModes0),

    proc_info_get_inst_varset(OrigProcInfo, InstVarSet),
    proc_info_get_inferred_determinism(OrigProcInfo, Detism),
    proc_info_get_goal(OrigProcInfo, Goal),
    proc_info_get_context(OrigProcInfo, Context),
    proc_info_get_rtti_varmaps(OrigProcInfo, RttiVarMaps),
    proc_info_get_is_address_taken(OrigProcInfo, IsAddressTaken),
    proc_info_get_has_parallel_conj(OrigProcInfo, HasParallelConj),
    proc_info_get_var_name_remap(OrigProcInfo, VarNameRemap),

    Substs = accu_substs(AccVarSubst, _RecCallSubst, _AssocCallSubst,
        _UpdateSubst),
    list.map(map.lookup(AccVarSubst), Accs0, Accs),

    % We place the extra accumulator variables at the start, because placing
    % them at the end breaks the convention that the last variable of a
    % function is the output variable.
    HeadVars = Accs ++ HeadVars0,

    % XXX we don't want to use the inst of the var as it can be more specific
    % than it should be. ie int_const(1) when it should be any integer.
    % However this will no longer handle partially instantiated data
    % structures.
    Inst = ground(shared, none_or_default_func),
    inst_lists_to_mode_list([Inst], [Inst], Mode),
    list.duplicate(list.length(Accs), list.det_head(Mode), AccModes),
    HeadModes = AccModes ++ HeadModes0,

    lookup_var_types(VarTypes, Accs, AccTypes),

    SeqNum = item_no_seq_num,
    proc_info_create(Context, SeqNum, VarSet, VarTypes, HeadVars,
        InstVarSet, HeadModes, detism_decl_none, Detism, Goal, RttiVarMaps,
        IsAddressTaken, HasParallelConj, VarNameRemap, AccProcInfo).

%---------------------------------------------------------------------------%

    % Construct the pred_info for the introduced predicate.
    %
:- pred acc_pred_info(list(mer_type)::in, list(prog_var)::in, proc_info::in,
    pred_id::in, pred_info::in, proc_id::out, pred_info::out) is det.

acc_pred_info(NewTypes, OutVars, NewProcInfo, OrigPredId, OrigPredInfo,
        NewProcId, NewPredInfo) :-
    % PredInfo stuff that must change.
    pred_info_get_arg_types(OrigPredInfo, TypeVarSet, ExistQVars, Types0),

    ModuleName = pred_info_module(OrigPredInfo),
    Name = pred_info_name(OrigPredInfo),
    PredOrFunc = pred_info_is_pred_or_func(OrigPredInfo),
    pred_info_get_context(OrigPredInfo, PredContext),
    pred_info_get_markers(OrigPredInfo, Markers),
    pred_info_get_class_context(OrigPredInfo, ClassContext),
    pred_info_get_origin(OrigPredInfo, OldOrigin),
    pred_info_get_var_name_remap(OrigPredInfo, VarNameRemap),

    set.init(Assertions),

    proc_info_get_context(NewProcInfo, Context),
    term.context_line(Context, Line),
    Counter = 0,

    Types = NewTypes ++ Types0,

    make_pred_name_with_context(ModuleName, "AccFrom", PredOrFunc, Name,
        Line, Counter, SymName),

    OutVarNums = list.map(term.var_to_int, OutVars),
    Origin = origin_transformed(transform_accumulator(OutVarNums),
        OldOrigin, OrigPredId),
    GoalType = goal_not_for_promise(np_goal_type_none),
    pred_info_create(ModuleName, SymName, PredOrFunc, PredContext, Origin,
        pred_status(status_local), Markers, Types, TypeVarSet,
        ExistQVars, ClassContext, Assertions, VarNameRemap, GoalType,
        NewProcInfo, NewProcId, NewPredInfo).

%---------------------------------------------------------------------------%

    % create_goal creates the new base and recursive case of the
    % original procedure (OrigBaseGoal and OrigRecGoal) and the base
    % and recursive cases of accumulator version (AccBaseGoal and
    % AccRecGoal).
    %
:- pred accu_create_goal(accu_goal_id::in, list(prog_var)::in,
    pred_id::in, proc_id::in, sym_name::in, accu_substs::in,
    accu_subst::in, accu_subst::in, accu_base::in,
    list(pair(prog_var))::in, accu_sets::in,
    accu_goal_store::in, accu_goal_store::in,
    hlds_goal::out, hlds_goal::out, hlds_goal::out, hlds_goal::out) is det.

accu_create_goal(RecCallId, Accs, AccPredId, AccProcId, AccName, Substs,
        HeadToCallSubst, CallToHeadSubst, BaseIds, BasePairs,
        Sets, C, CS, OrigBaseGoal, OrigRecGoal, AccBaseGoal, AccRecGoal) :-
    lookup_call(C, RecCallId, stored_goal(OrigCall, _InstMap)),
    Call = create_acc_call(OrigCall, Accs, AccPredId, AccProcId, AccName),
    create_orig_goal(Call, Substs, HeadToCallSubst, CallToHeadSubst,
        BaseIds, Sets, C, OrigBaseGoal, OrigRecGoal),
    create_acc_goal(Call, Substs, HeadToCallSubst, BaseIds, BasePairs,
        Sets, C, CS, AccBaseGoal, AccRecGoal).

    % create_acc_call takes the original call and generates a call to the
    % accumulator version of the call, which can have the substitutions
    % applied to it easily.
    %
:- func create_acc_call(hlds_goal::in(goal_plain_call), list(prog_var)::in,
    pred_id::in, proc_id::in, sym_name::in) = (hlds_goal::out(goal_plain_call))
    is det.

create_acc_call(OrigCall, Accs, AccPredId, AccProcId, AccName) = Call :-
    OrigCall = hlds_goal(OrigCallExpr, GoalInfo),
    OrigCallExpr = plain_call(_PredId, _ProcId, Args, Builtin, Context, _Name),
    CallExpr = plain_call(AccPredId, AccProcId, Accs ++ Args, Builtin,
        Context, AccName),
    Call = hlds_goal(CallExpr, GoalInfo).

    % Create the goals which are to replace the original predicate.
    %
:- pred create_orig_goal(hlds_goal::in, accu_substs::in,
    accu_subst::in, accu_subst::in, accu_base::in, accu_sets::in,
    accu_goal_store::in, hlds_goal::out, hlds_goal::out) is det.

create_orig_goal(Call, Substs, HeadToCallSubst, CallToHeadSubst,
        BaseIds, Sets, C, OrigBaseGoal, OrigRecGoal) :-
    Substs = accu_substs(_AccVarSubst, _RecCallSubst, _AssocCallSubst,
        UpdateSubst),

    BaseIds = accu_base(UpdateBase, _AssocBase, _OtherBase),
    Before = Sets ^ as_before,
    Update = Sets ^ as_update,

    U = create_new_orig_recursive_goals(UpdateBase, Update,
        HeadToCallSubst, UpdateSubst, C),

    rename_some_vars_in_goal(CallToHeadSubst, Call, BaseCall),
    Cbefore = accu_goal_list(set.to_sorted_list(Before), C),
    Uupdate = accu_goal_list(set.to_sorted_list(UpdateBase) ++
        set.to_sorted_list(Update), U),
    Cbase = accu_goal_list(base_case_ids(C), C),
    calculate_goal_info(conj(plain_conj, Cbefore ++ Uupdate ++ [BaseCall]),
        OrigRecGoal),
    calculate_goal_info(conj(plain_conj, Cbase), OrigBaseGoal).

    % Create the goals which are to go in the new accumulator version
    % of the predicate.
    %
:- pred create_acc_goal(hlds_goal::in, accu_substs::in, accu_subst::in,
    accu_base::in, list(pair(prog_var))::in, accu_sets::in,
    accu_goal_store::in, accu_goal_store::in,
    hlds_goal::out, hlds_goal::out) is det.

create_acc_goal(Call, Substs, HeadToCallSubst, BaseIds, BasePairs, Sets,
        C, CS, AccBaseGoal, AccRecGoal) :-
    Substs = accu_substs(AccVarSubst, RecCallSubst, AssocCallSubst,
        UpdateSubst),

    BaseIds = accu_base(_UpdateBase, AssocBase, OtherBase),
    Sets = accu_sets(Before, Assoc, ConstructAssoc, Construct, Update,
        _Reject),

    rename_some_vars_in_goal(RecCallSubst, Call, RecCall),

    Cbefore = accu_goal_list(set.to_sorted_list(Before), C),

    % Create the goals which will be used in the new recursive case.
    R = create_new_recursive_goals(Assoc, Construct `union` ConstructAssoc,
        Update, AssocCallSubst, AccVarSubst, UpdateSubst, C, CS),

    Rassoc = accu_goal_list(set.to_sorted_list(Assoc), R),
    Rupdate = accu_goal_list(set.to_sorted_list(Update), R),
    Rconstruct = accu_goal_list(set.to_sorted_list(Construct `union`
        ConstructAssoc), R),

    % Create the goals which will be used in the new base case.
    B = create_new_base_goals(Assoc `union` Construct `union`
        ConstructAssoc, C, AccVarSubst, HeadToCallSubst),
    Bafter = set.to_sorted_list(Assoc `union`
        Construct `union` ConstructAssoc),

    BaseCase = accu_goal_list(set.to_sorted_list(AssocBase `union` OtherBase)
        ++ Bafter, B),

    list.map(acc_unification, BasePairs, UpdateBase),

    calculate_goal_info(conj(plain_conj, Cbefore ++ Rassoc ++ Rupdate
        ++ [RecCall] ++ Rconstruct), AccRecGoal),
    calculate_goal_info(conj(plain_conj, UpdateBase ++ BaseCase), AccBaseGoal).

    % Create the U set of goals (those that will be used in the original
    % recursive case) by renaming all the goals which are used to initialize
    % the update state variable using the head_to_call followed by the
    % update_subst, and rename all the update goals using the update_subst.
    %
:- func create_new_orig_recursive_goals(set(accu_goal_id), set(accu_goal_id),
    accu_subst, accu_subst, accu_goal_store) = accu_goal_store.

create_new_orig_recursive_goals(UpdateBase, Update, HeadToCallSubst,
        UpdateSubst, C)
        = accu_rename(set.to_sorted_list(Update), UpdateSubst, C, Ubase) :-
    Ubase = accu_rename(set.to_sorted_list(UpdateBase),
        chain_subst(HeadToCallSubst, UpdateSubst), C, goal_store_init).

    % Create the R set of goals (those that will be used in the new
    % recursive case) by renaming all the members of assoc in CS
    % using assoc_call_subst and all the members of (construct U
    % construct_assoc) in C with acc_var_subst.
    %
:- func create_new_recursive_goals(set(accu_goal_id), set(accu_goal_id),
    set(accu_goal_id), accu_subst, accu_subst, accu_subst,
    accu_goal_store, accu_goal_store) = accu_goal_store.

create_new_recursive_goals(Assoc, Constructs, Update,
        AssocCallSubst, AccVarSubst, UpdateSubst, C, CS)
        = accu_rename(set.to_sorted_list(Constructs), AccVarSubst, C, RBase) :-
    RBase0 = accu_rename(set.to_sorted_list(Assoc), AssocCallSubst, CS,
        goal_store_init),
    RBase = accu_rename(set.to_sorted_list(Update), UpdateSubst, C, RBase0).

    % Create the B set of goals (those that will be used in the new base case)
    % by renaming all the base case goals of C with head_to_call and all the
    % members of (assoc U construct U construct_assoc) of C with acc_var_subst.
    %
:- func create_new_base_goals(set(accu_goal_id), accu_goal_store,
    accu_subst, accu_subst) = accu_goal_store.

create_new_base_goals(Ids, C, AccVarSubst, HeadToCallSubst)
        = accu_rename(set.to_sorted_list(Ids), AccVarSubst, C, Bbase) :-
    Bbase = accu_rename(base_case_ids(C), HeadToCallSubst, C, goal_store_init).

    % acc_unification(O-A, G):
    %
    % is true if G represents the assignment unification Out = Acc.
    %
:- pred acc_unification(pair(prog_var)::in, hlds_goal::out) is det.

acc_unification(Out - Acc, Goal) :-
    UnifyMode = unify_modes_li_lf_ri_rf(free, ground_inst,
        ground_inst, ground_inst),
    Context = unify_context(umc_explicit, []),
    Expr = unify(Out, rhs_var(Acc), UnifyMode, assign(Out,Acc), Context),
    set_of_var.list_to_set([Out, Acc], NonLocalVars),
    InstMapDelta = instmap_delta_bind_var(Out),
    goal_info_init(NonLocalVars, InstMapDelta, detism_det, purity_pure, Info),
    Goal = hlds_goal(Expr, Info).

%---------------------------------------------------------------------------%

    % Given the top level structure of the goal create new version
    % with new base and recursive cases plugged in.
    %
:- pred accu_top_level(top_level::in, hlds_goal::in,
    hlds_goal::in, hlds_goal::in, hlds_goal::in,
    hlds_goal::in, hlds_goal::out, hlds_goal::out) is det.

accu_top_level(TopLevel, Goal, OrigBaseGoal, OrigRecGoal,
        NewBaseGoal, NewRecGoal, OrigGoal, NewGoal) :-
    (
        TopLevel = switch_base_rec,
        ( if
            Goal = hlds_goal(switch(Var, CanFail, Cases0), GoalInfo),
            Cases0 = [case(IdA, [], _), case(IdB, [], _)]
        then
            OrigCases = [case(IdA, [], OrigBaseGoal),
                case(IdB, [], OrigRecGoal)],
            OrigGoal = hlds_goal(switch(Var, CanFail, OrigCases), GoalInfo),

            NewCases = [case(IdA, [], NewBaseGoal), case(IdB, [], NewRecGoal)],
            NewGoal = hlds_goal(switch(Var, CanFail, NewCases), GoalInfo)
        else
            unexpected($pred, "not the correct top level")
        )
    ;
        TopLevel = switch_rec_base,
        ( if
            Goal = hlds_goal(switch(Var, CanFail, Cases0), GoalInfo),
            Cases0 = [case(IdA, [], _), case(IdB, [], _)]
        then
            OrigCases = [case(IdA, [], OrigRecGoal),
                case(IdB, [], OrigBaseGoal)],
            OrigGoal = hlds_goal(switch(Var, CanFail, OrigCases), GoalInfo),

            NewCases = [case(IdA, [], NewRecGoal), case(IdB, [], NewBaseGoal)],
            NewGoal = hlds_goal(switch(Var, CanFail, NewCases), GoalInfo)
        else
            unexpected($pred, "not the correct top level")
        )
    ;
        TopLevel = disj_base_rec,
        ( if
            Goal = hlds_goal(disj(Goals), GoalInfo),
            Goals = [_, _]
        then
            OrigGoals = [OrigBaseGoal, OrigRecGoal],
            OrigGoal = hlds_goal(disj(OrigGoals), GoalInfo),

            NewGoals = [NewBaseGoal, NewRecGoal],
            NewGoal = hlds_goal(disj(NewGoals), GoalInfo)
        else
            unexpected($pred, "not the correct top level")
        )
    ;
        TopLevel = disj_rec_base,
        ( if
            Goal = hlds_goal(disj(Goals), GoalInfo),
            Goals = [_, _]
        then
            OrigGoals = [OrigRecGoal, OrigBaseGoal],
            OrigGoal = hlds_goal(disj(OrigGoals), GoalInfo),

            NewGoals = [NewRecGoal, NewBaseGoal],
            NewGoal = hlds_goal(disj(NewGoals), GoalInfo)
        else
            unexpected($pred, "not the correct top level")
        )
    ;
        TopLevel = ite_base_rec,
        ( if Goal = hlds_goal(if_then_else(Vars, Cond, _, _), GoalInfo) then
            OrigGoal = hlds_goal(if_then_else(Vars, Cond,
                OrigBaseGoal, OrigRecGoal), GoalInfo),
            NewGoal = hlds_goal(if_then_else(Vars, Cond,
                NewBaseGoal, NewRecGoal), GoalInfo)
        else
            unexpected($pred, "not the correct top level")
        )
    ;
        TopLevel = ite_rec_base,
        ( if Goal = hlds_goal(if_then_else(Vars, Cond, _, _), GoalInfo) then
            OrigGoal = hlds_goal(if_then_else(Vars, Cond,
                OrigRecGoal, OrigBaseGoal), GoalInfo),
            NewGoal = hlds_goal(if_then_else(Vars, Cond,
                NewRecGoal, NewBaseGoal), GoalInfo)
        else
            unexpected($pred, "not the correct top level")
        )
    ).

%---------------------------------------------------------------------------%

    % Place the accumulator version of the predicate in the HLDS.
    %
:- pred update_accumulator_pred(pred_id::in, proc_id::in,
    hlds_goal::in, module_info::in, module_info::out) is det.

update_accumulator_pred(NewPredId, NewProcId, AccGoal, !ModuleInfo) :-
    module_info_pred_proc_info(!.ModuleInfo, NewPredId, NewProcId,
        PredInfo, ProcInfo0),
    proc_info_set_goal(AccGoal, ProcInfo0, ProcInfo1),
    requantify_proc_general(ordinary_nonlocals_no_lambda, ProcInfo1, ProcInfo),
    module_info_set_pred_proc_info(NewPredId, NewProcId,
        PredInfo, ProcInfo, !ModuleInfo).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % accu_rename(Ids, Subst, From, Initial):
    %
    % Return a goal_store, Final, which is the result of looking up each
    % member of set of goal_ids, Ids, in the goal_store, From, applying
    % the substitution and then storing the goal into the goal_store, Initial.
    %
:- func accu_rename(list(accu_goal_id), accu_subst,
    accu_goal_store, accu_goal_store) = accu_goal_store.

accu_rename(Ids, Subst, From, Initial) = Final :-
    list.foldl(
        ( pred(Id::in, GS0::in, GS::out) is det :-
            goal_store_lookup(From, Id, stored_goal(Goal0, InstMap)),
            rename_some_vars_in_goal(Subst, Goal0, Goal),
            goal_store_det_insert(Id, stored_goal(Goal, InstMap), GS0, GS)
        ), Ids, Initial, Final).

    % Return all the goal_ids which belong in the base case.
    %
:- func base_case_ids(accu_goal_store) = list(accu_goal_id).

base_case_ids(GS) = Base :-
    solutions.solutions(
        ( pred(Key::out) is nondet :-
            goal_store_member(GS, Key, _Goal),
            Key = accu_goal_id(accu_base, _)
        ), Base).

:- func base_case_ids_set(accu_goal_store) = set(accu_goal_id).

base_case_ids_set(GS) = set.list_to_set(base_case_ids(GS)).

    % Given a list of goal_ids, return the list of hlds_goals from
    % the goal_store.
    %
:- func accu_goal_list(list(accu_goal_id), accu_goal_store) = list(hlds_goal).

accu_goal_list(Ids, GS) = Goals :-
    list.map(
        ( pred(Key::in, G::out) is det :-
            goal_store_lookup(GS, Key, stored_goal(G, _))
        ), Ids, Goals).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- pred calculate_goal_info(hlds_goal_expr::in, hlds_goal::out) is det.

calculate_goal_info(GoalExpr, hlds_goal(GoalExpr, GoalInfo)) :-
    ( if GoalExpr = conj(plain_conj, GoalList) then
        goal_list_nonlocals(GoalList, NonLocals),
        goal_list_instmap_delta(GoalList, InstMapDelta),
        goal_list_determinism(GoalList, Detism),

        goal_info_init(NonLocals, InstMapDelta, Detism, purity_pure, GoalInfo)
    else
        unexpected($pred, "not a conj")
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- func chain_subst(accu_subst, accu_subst) = accu_subst.

chain_subst(AtoB, BtoC) = AtoC :-
    map.keys(AtoB, Keys),
    chain_subst_2(Keys, AtoB, BtoC, AtoC).

:- pred chain_subst_2(list(A)::in, map(A, B)::in, map(B, C)::in,
    map(A, C)::out) is det.

chain_subst_2([], _, _, AtoC) :-
    map.init(AtoC).
chain_subst_2([A | As], AtoB, BtoC, AtoC) :-
    chain_subst_2(As, AtoB, BtoC, AtoC0),
    map.lookup(AtoB, A, B),
    ( if map.search(BtoC, B, C) then
        map.det_insert(A, C, AtoC0, AtoC)
    else
        AtoC = AtoC0
    ).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.accumulator.
%---------------------------------------------------------------------------%

:- some [T] pred unravel_univ(univ::in, T::out) is det.
:- pragma foreign_export("C", unravel_univ(in, out), "ML_unravel_univ").
:- pragma foreign_export("C#", unravel_univ(in, out), "ML_unravel_univ").
:- pragma foreign_export("Java", unravel_univ(in, out), "ML_unravel_univ").

unravel_univ(Univ, X) :-
    univ_value(Univ) = X.
