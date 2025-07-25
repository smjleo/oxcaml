(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Flambda.Import
module CSE = Common_subexpression_elimination
module K = Flambda_kind
module BP = Bound_parameter
module T = Flambda2_types
module TE = Flambda2_types.Typing_env

type resolver =
  Compilation_unit.t -> Flambda2_types.Typing_env.Serializable.t option

type get_imported_names = unit -> Name.Set.t

type get_imported_code = unit -> Exported_code.t

module Disable_inlining_reason = struct
  type t =
    | Stub
    | Speculative_inlining

  let print ppf = function
    | Stub -> Format.fprintf ppf "Stub"
    | Speculative_inlining -> Format.fprintf ppf "Speculative_inlining"
end

module Disable_inlining = struct
  type t =
    | Disable_inlining of Disable_inlining_reason.t
    | Do_not_disable_inlining

  let print ppf = function
    | Disable_inlining reason ->
      Format.fprintf ppf "@[<hov 1>(Disable_inlining %a)@]"
        Disable_inlining_reason.print reason
    | Do_not_disable_inlining -> Format.fprintf ppf "Do_not_disable_inlining"
end

type t =
  { round : int;
    typing_env : TE.t;
    inlined_debuginfo : Inlined_debuginfo.t;
    disable_inlining : Disable_inlining.t;
    inlining_state : Inlining_state.t;
    propagating_float_consts : bool;
    at_unit_toplevel : bool;
    unit_toplevel_return_continuation : Continuation.t;
    unit_toplevel_exn_continuation : Continuation.t;
    variables_defined_at_toplevel : Variable.Set.t;
    cse : CSE.t;
    comparison_results : Comparison_result.t Variable.Map.t;
    are_rebuilding_terms : Are_rebuilding_terms.t;
    closure_info : Closure_info.t;
    get_imported_code : unit -> Exported_code.t;
    all_code : Code.t Code_id.Map.t;
    inlining_history_tracker : Inlining_history.Tracker.t;
    loopify_state : Loopify_state.t;
    replay_history : Replay_history.t;
        (* Replay history for the current continuation handler (or toplevel) *)
    specialization_cost : Specialization_cost.t;
        (* Accumulator to record whether the handler of the current continuation
           can be specialized, or whether there are reasons why it could not (or
           rather why it would not be beneficial) *)
    defined_variables_by_scope : Lifted_cont_params.t list;
        (* Stack of variables defined. The first element of the list refers to
           variables defined by the current continuation, and the last element
           of the list to variables that are defined at toplevel. We use the
           type [Lifted_cont_params.t] because the current only use of this
           field is the continuation lifting process. *)
    lifted : Variable.Set.t;
        (* This field provides a fast access to the set of parameters (as
           variables) added to the current continuation by the continuation
           lifting process. For these variables, it's important that we do not
           generate a fresh [Lifted_cont_param] when we execute
           [define_variable]. Note that this set will always be a subset of the
           head of the defined_variables_by_scope field. *)
    cost_of_lifting_continuations_out_of_current_one : int
        (* This cost is the number of parameters that would have to be created
           if we lifted all continuations that are defined in the current
           continuation's handler. *)
  }

let [@ocamlformat "disable"] print ppf { round; typing_env;
                inlined_debuginfo; disable_inlining;
                inlining_state; propagating_float_consts;
                at_unit_toplevel; unit_toplevel_exn_continuation;
                variables_defined_at_toplevel; cse; comparison_results;
                are_rebuilding_terms; closure_info;
                unit_toplevel_return_continuation; all_code;
                get_imported_code = _; inlining_history_tracker = _;
                loopify_state; replay_history; specialization_cost; defined_variables_by_scope;
                lifted = _; cost_of_lifting_continuations_out_of_current_one;
              } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(round@ %d)@]@ \
      @[<hov 1>(typing_env@ %a)@]@ \
      @[<hov 1>(inlined_debuginfo@ %a)@]@ \
      @[<hov 1>(disable_inlining@ %a)@]@ \
      @[<hov 1>(inlining_state@ %a)@]@ \
      @[<hov 1>(propagating_float_consts@ %b)@]@ \
      @[<hov 1>(at_unit_toplevel@ %b)@]@ \
      @[<hov 1>(unit_toplevel_return_continuation@ %a)@]@ \
      @[<hov 1>(unit_toplevel_exn_continuation@ %a)@]@ \
      @[<hov 1>(variables_defined_at_toplevel@ %a)@]@ \
      @[<hov 1>(cse@ @[<hov 1>%a@])@]@ \
      @[<hov 1>(comparison_results@ @[<hov 1>%a@])@]@ \
      @[<hov 1>(are_rebuilding_terms@ %a)@]@ \
      @[<hov 1>(closure_info@ %a)@]@ \
      @[<hov 1>(all_code@ %a)@]@ \
      @[<hov 1>(loopify_state@ %a)@]@ \
      @[<hov 1>(binding_histories@ %a)@]@ \
      @[<hov 1>(specialization_cost@ %a)@]@ \
      @[<hov 1>(defined_variables_by_scope@ %a)@]@ \
      @[<hov 1>(cost_of_lifting_continuation_out_of_current_one %d)@]\
      )@]"
    round
    TE.print typing_env
    Inlined_debuginfo.print inlined_debuginfo
    Disable_inlining.print disable_inlining
    Inlining_state.print inlining_state
    propagating_float_consts
    at_unit_toplevel
    Continuation.print unit_toplevel_return_continuation
    Continuation.print unit_toplevel_exn_continuation
    Variable.Set.print variables_defined_at_toplevel
    CSE.print cse
    (Variable.Map.print Comparison_result.print) comparison_results
    Are_rebuilding_terms.print are_rebuilding_terms
    Closure_info.print closure_info
    (Code_id.Map.print Code.print) all_code
    Loopify_state.print loopify_state
    Replay_history.print replay_history
    Specialization_cost.print specialization_cost
    (Format.pp_print_list ~pp_sep:Format.pp_print_space Lifted_cont_params.print) defined_variables_by_scope
    cost_of_lifting_continuations_out_of_current_one

let define_continuations t conts =
  let replay_history =
    Replay_history.define_continuations conts t.replay_history
  in
  { t with replay_history }

let define_variable0 ~extra t var kind =
  let replay_history =
    if extra
    then t.replay_history
    else Replay_history.define_variable (Bound_var.var var) t.replay_history
  in
  let defined_variables_by_scope =
    if extra || Variable.Set.mem (Bound_var.var var) t.lifted
    then t.defined_variables_by_scope
    else
      match t.defined_variables_by_scope with
      | [] -> Misc.fatal_errorf "Empty stack of defined variables in denv."
      | variables_defined_in_current_continuation :: r ->
        let kind = Flambda_kind.With_subkind.anything kind in
        let variables_defined_in_current_continuation =
          Lifted_cont_params.new_param ~replay_history
            variables_defined_in_current_continuation
            (Bound_parameter.create (Bound_var.var var) kind)
        in
        variables_defined_in_current_continuation :: r
  in
  let typing_env =
    let var = Bound_name.create_var var in
    TE.add_definition t.typing_env var kind
  in
  let variables_defined_at_toplevel =
    if t.at_unit_toplevel
    then Variable.Set.add (Bound_var.var var) t.variables_defined_at_toplevel
    else t.variables_defined_at_toplevel
  in
  { t with
    typing_env;
    replay_history;
    variables_defined_at_toplevel;
    defined_variables_by_scope
  }

let define_variable t var kind =
  (define_variable0 [@inlined hint]) ~extra:false t var kind

let define_extra_variable t var kind =
  (define_variable0 [@inlined hint]) ~extra:true t var kind

let create ~round ~(resolver : resolver)
    ~(get_imported_names : get_imported_names)
    ~(get_imported_code : get_imported_code) ~propagating_float_consts
    ~unit_toplevel_exn_continuation ~unit_toplevel_return_continuation
    ~toplevel_my_region ~toplevel_my_ghost_region =
  let typing_env = TE.create ~resolver ~get_imported_names in
  let t =
    { round;
      typing_env;
      inlined_debuginfo = Inlined_debuginfo.none;
      disable_inlining = Do_not_disable_inlining;
      inlining_state = Inlining_state.default ~round;
      propagating_float_consts;
      at_unit_toplevel = true;
      unit_toplevel_return_continuation;
      unit_toplevel_exn_continuation;
      variables_defined_at_toplevel = Variable.Set.empty;
      cse = CSE.empty;
      comparison_results = Variable.Map.empty;
      are_rebuilding_terms = Are_rebuilding_terms.are_rebuilding;
      closure_info = Closure_info.not_in_a_closure;
      all_code = Code_id.Map.empty;
      get_imported_code;
      inlining_history_tracker =
        Inlining_history.Tracker.empty (Compilation_unit.get_current_exn ());
      loopify_state = Loopify_state.do_not_loopify;
      replay_history = Replay_history.first_pass;
      specialization_cost = Specialization_cost.can_specialize;
      defined_variables_by_scope = [Lifted_cont_params.empty];
      lifted = Variable.Set.empty;
      cost_of_lifting_continuations_out_of_current_one = 0
    }
  in
  define_variable
    (define_variable t
       (Bound_var.create toplevel_my_region Name_mode.normal)
       K.region)
    (Bound_var.create toplevel_my_ghost_region Name_mode.normal)
    K.region

let all_code t = t.all_code

let resolver t = TE.resolver t.typing_env

let typing_env t = t.typing_env

let round t = t.round

let get_continuation_scope t = TE.current_scope t.typing_env

let disable_inlining t = t.disable_inlining

let propagating_float_consts t = t.propagating_float_consts

let unit_toplevel_exn_continuation t = t.unit_toplevel_exn_continuation

let unit_toplevel_return_continuation t = t.unit_toplevel_return_continuation

let at_unit_toplevel t = t.at_unit_toplevel

let set_at_unit_toplevel_state t at_unit_toplevel = { t with at_unit_toplevel }

let is_defined_at_toplevel t var =
  Variable.Set.mem var t.variables_defined_at_toplevel

let defined_variables_by_scope t = t.defined_variables_by_scope

let get_inlining_state t = t.inlining_state

let set_inlining_state t inlining_state = { t with inlining_state }

let inlining_history_tracker t = t.inlining_history_tracker

let relative_history t =
  Inlining_history.Tracker.relative t.inlining_history_tracker

let set_inlining_history_tracker inlining_history_tracker t =
  { t with inlining_history_tracker }

let increment_continuation_scope t =
  { t with typing_env = TE.increment_scope t.typing_env }

let bump_current_level_scope t =
  { t with typing_env = TE.bump_current_level_scope t.typing_env }

let enter_set_of_closures
    { round;
      typing_env;
      inlined_debuginfo = _;
      disable_inlining = _;
      inlining_state;
      propagating_float_consts;
      at_unit_toplevel = _;
      unit_toplevel_return_continuation;
      unit_toplevel_exn_continuation;
      variables_defined_at_toplevel;
      cse = _;
      comparison_results = _;
      are_rebuilding_terms;
      closure_info = _;
      get_imported_code;
      all_code;
      inlining_history_tracker;
      loopify_state = _;
      replay_history = _;
      specialization_cost = _;
      defined_variables_by_scope = _;
      lifted = _;
      cost_of_lifting_continuations_out_of_current_one = _
    } disable_inlining =
  { round;
    typing_env = TE.closure_env typing_env;
    inlined_debuginfo = Inlined_debuginfo.none;
    disable_inlining;
    inlining_state;
    propagating_float_consts;
    at_unit_toplevel = false;
    unit_toplevel_return_continuation;
    unit_toplevel_exn_continuation;
    variables_defined_at_toplevel;
    cse = CSE.empty;
    comparison_results = Variable.Map.empty;
    are_rebuilding_terms;
    closure_info = Closure_info.in_a_set_of_closures;
    get_imported_code;
    all_code;
    inlining_history_tracker;
    loopify_state = Loopify_state.do_not_loopify;
    replay_history = Replay_history.first_pass;
    specialization_cost = Specialization_cost.can_specialize;
    defined_variables_by_scope = [Lifted_cont_params.empty];
    lifted = Variable.Set.empty;
    cost_of_lifting_continuations_out_of_current_one = 0
  }

let define_symbol t sym kind =
  let typing_env =
    let sym = Bound_name.create (Name.symbol sym) Name_mode.normal in
    TE.add_definition t.typing_env sym kind
  in
  { t with typing_env }

let define_name t name kind =
  Name.pattern_match (Bound_name.name name)
    ~var:(fun [@inline] var ->
      (define_variable [@inlined hint]) t
        (Bound_var.create var (Bound_name.name_mode name))
        kind)
    ~symbol:(fun [@inline] sym -> (define_symbol [@inlined hint]) t sym kind)

let add_variable0 ~extra t var ty =
  let t = (define_variable0 [@inlined hint]) ~extra t var (T.kind ty) in
  { t with
    typing_env = TE.add_equation t.typing_env (Name.var (Bound_var.var var)) ty
  }

let add_variable t var ty =
  (add_variable0 [@inlined hint]) ~extra:false t var ty

let add_symbol t sym ty =
  let t = (define_symbol [@inlined hint]) t sym (T.kind ty) in
  { t with typing_env = TE.add_equation t.typing_env (Name.symbol sym) ty }

let add_name t name ty =
  Name.pattern_match (Bound_name.name name)
    ~var:(fun [@inline] var ->
      add_variable t (Bound_var.create var (Bound_name.name_mode name)) ty)
    ~symbol:(fun [@inline] sym -> add_symbol t sym ty)

let add_equation_on_variable t var ty =
  let typing_env = TE.add_equation t.typing_env (Name.var var) ty in
  { t with typing_env }

let mem_name t name = TE.mem t.typing_env name

let mem_variable t var = TE.mem t.typing_env (Name.var var)

let add_equation_on_symbol t sym ty =
  let typing_env =
    let sym = Name.symbol sym in
    TE.add_equation t.typing_env sym ty
  in
  { t with typing_env }

let mem_symbol t sym = mem_name t (Name.symbol sym)

let find_symbol t sym = TE.find (typing_env t) (Name.symbol sym) (Some K.value)

let add_symbol_projection t var proj =
  { t with typing_env = TE.add_symbol_projection t.typing_env var proj }

let find_symbol_projection t var = TE.find_symbol_projection t.typing_env var

let define_name_if_undefined t name kind =
  if TE.mem t.typing_env (Bound_name.name name)
  then t
  else define_name t name kind

let add_equation_on_name t name ty =
  let typing_env = TE.add_equation t.typing_env name ty in
  { t with typing_env }

let define_parameters ~extra t ~params =
  List.fold_left
    (fun t param ->
      let var = Bound_var.create (BP.var param) Name_mode.normal in
      define_variable0 ~extra t var (K.With_subkind.kind (BP.kind param)))
    t
    (Bound_parameters.to_list params)

let add_parameters ~extra ?(name_mode = Name_mode.normal) t params ~param_types
    =
  let params' = params in
  let params = Bound_parameters.to_list params in
  if List.compare_lengths params param_types <> 0
  then
    Misc.fatal_errorf
      "Mismatch between number of [params] and [param_types]:@ (%a)@ and@ %a"
      Bound_parameters.print params'
      (Format.pp_print_list ~pp_sep:Format.pp_print_space T.print)
      param_types;
  List.fold_left2
    (fun t param param_type ->
      let var = Bound_var.create (BP.var param) name_mode in
      add_variable0 ~extra t var param_type)
    t params param_types

let add_parameters_with_unknown_types ~extra ?alloc_modes ?name_mode t params =
  let params' = params in
  let params = Bound_parameters.to_list params in
  let alloc_modes =
    match alloc_modes with
    | Some alloc_modes ->
      if List.compare_lengths alloc_modes params <> 0
      then
        Misc.fatal_errorf "Params and alloc modes do not match up:@ %a"
          Bound_parameters.print params';
      alloc_modes
    | None -> List.map (fun _ -> Alloc_mode.For_types.unknown ()) params
  in
  let param_types =
    ListLabels.map2 params alloc_modes ~f:(fun param alloc_mode ->
        T.unknown_with_subkind ~alloc_mode (BP.kind param))
  in
  add_parameters ~extra ?name_mode t params' ~param_types

let mark_parameters_as_toplevel t params =
  let variables_defined_at_toplevel =
    Variable.Set.union t.variables_defined_at_toplevel
      (Bound_parameters.var_set params)
  in
  { t with variables_defined_at_toplevel }

let extend_typing_environment t env_extension =
  (* There doesn't seem any need to augment [t.variables_defined_at_toplevel]
     here for the existential variables, since they will have [In_types]
     mode. *)
  let typing_env =
    TE.add_env_extension_with_extra_variables t.typing_env env_extension
  in
  { t with typing_env }

let with_typing_env t typing_env = { t with typing_env }

let map_typing_env t ~f = with_typing_env t (f t.typing_env)

let check_name_is_bound t name =
  if not (TE.mem t.typing_env name)
  then
    Misc.fatal_errorf "Unbound name %a in environment:@ %a" Name.print name
      print t

let check_simple_is_bound t (simple : Simple.t) =
  Simple.pattern_match simple
    ~name:(fun name ~coercion:_ -> check_name_is_bound t name)
    ~const:(fun _ -> ())

let mem_code t id =
  Code_id.Map.mem id t.all_code || Exported_code.mem id (t.get_imported_code ())

let find_code_exn t id =
  match Code_id.Map.find id t.all_code with
  | code -> Code_or_metadata.create code
  | exception Not_found ->
    (* We might have already loaded the metadata, from another unit that
       references it. However we force loading of the corresponding .cmx to make
       sure that we will have access to the actual code (assuming the .cmx isn't
       missing). *)
    let (_ : TE.Serializable.t option) =
      TE.resolver t.typing_env (Code_id.get_compilation_unit id)
    in
    Exported_code.find_exn (t.get_imported_code ()) id

let define_code t ~code_id ~code =
  if not
       (Code_id.in_compilation_unit code_id
          (Compilation_unit.get_current_exn ()))
  then
    Misc.fatal_errorf "Cannot define code ID %a as it is from another unit:@ %a"
      Code_id.print code_id Code.print code;
  if not (Code_id.equal code_id (Code.code_id code))
  then
    Misc.fatal_errorf "Code ID %a does not match code ID in@ %a" Code_id.print
      code_id Code.print code;
  let typing_env =
    TE.add_to_code_age_relation t.typing_env ~new_code_id:code_id
      ~old_code_id:(Code.newer_version_of code)
  in
  let all_code = Code_id.Map.add code_id code t.all_code in
  { t with typing_env; all_code }

let cse t = t.cse

let comparison_results t = t.comparison_results

let add_cse t prim ~bound_to =
  let scope = get_continuation_scope t in
  let cse = CSE.add t.cse prim ~bound_to scope in
  let comparison_results =
    let prim = Flambda_primitive.Eligible_for_cse.to_primitive prim in
    match
      ( Comparison_result.create ~prim ~comparison_results:t.comparison_results,
        Simple.must_be_var bound_to )
    with
    | None, _ | _, None -> t.comparison_results
    | Some comp, Some (var, _) -> Variable.Map.add var comp t.comparison_results
  in
  { t with cse; comparison_results }

let find_cse t prim = CSE.find t.cse prim

let find_comparison_result t var =
  Variable.Map.find_opt var t.comparison_results

let with_cse t cse = { t with cse }

let set_do_not_rebuild_terms_and_disable_inlining t disable_inlining_reason =
  { t with
    are_rebuilding_terms = Are_rebuilding_terms.are_not_rebuilding;
    disable_inlining = Disable_inlining disable_inlining_reason
  }

let set_disable_inlining t reason =
  { t with disable_inlining = Disable_inlining reason }

let set_rebuild_terms t =
  { t with are_rebuilding_terms = Are_rebuilding_terms.are_rebuilding }

let are_rebuilding_terms t = t.are_rebuilding_terms

let enter_closure code_id ~return_continuation ~exn_continuation ~my_closure t =
  { t with
    closure_info =
      Closure_info.in_a_closure code_id ~return_continuation ~exn_continuation
        ~my_closure
  }

let closure_info t = t.closure_info

let inlining_arguments { inlining_state; _ } =
  Inlining_state.arguments inlining_state

let set_inlining_arguments arguments t =
  { t with
    inlining_state = Inlining_state.with_arguments arguments t.inlining_state
  }

(* CR mshinwell/gbury: we might be dropping [Enter_inlined_apply] context here
   when mixing code compiled in classic and Simplify modes *)

let set_inlined_debuginfo t ~from =
  { t with inlined_debuginfo = from.inlined_debuginfo }

let merge_inlined_debuginfo t ~from_apply_expr =
  { t with
    inlined_debuginfo =
      Inlined_debuginfo.merge t.inlined_debuginfo ~from_apply_expr
  }

let add_inlined_debuginfo t dbg =
  Inlined_debuginfo.rewrite t.inlined_debuginfo dbg

let enter_inlined_apply ~called_code ~apply ~was_inline_always t =
  let arguments =
    Inlining_state.arguments t.inlining_state
    |> Inlining_arguments.meet (Code.inlining_arguments called_code)
    |> Inlining_arguments.meet (Apply.inlining_arguments apply)
  in
  let inlining_state =
    (* The depth limit for [@inline always] and [@inlined always] is really to
       make sure the compiler terminates if user code containing implicit
       recursion turns into explicit recursion within Flambda. We want to honour
       the user's requests for these attributes basically all the time. As such
       inlining when these attributes are in effect affects the depth limit much
       less than in other scenarios. *)
    Inlining_state.with_arguments arguments
      (if Code.stub called_code
      then t.inlining_state
      else
        let by =
          if was_inline_always
          then 1
          else Flambda_features.Inlining.depth_scaling_factor
        in
        Inlining_state.increment_depth t.inlining_state ~by)
  in
  let inlined_debuginfo =
    Inlined_debuginfo.create ~called_code_id:(Code.code_id called_code)
      ~apply_dbg:(Apply.dbg apply)
  in
  { t with
    inlined_debuginfo;
    inlining_state;
    inlining_history_tracker =
      Inlining_history.Tracker.enter_inlined_apply
        ~callee:(Code.absolute_history called_code)
        ~dbg:(Apply.dbg apply)
        ~apply_relative_history:(Apply.relative_history apply)
        t.inlining_history_tracker
  }

let generate_phantom_lets t =
  Flambda_features.debug ()
  && Flambda_features.Expert.phantom_lets ()
  (* It would be a waste of time generating phantom lets when not rebuilding
     terms, since they have no effect on cost metrics. *)
  && Are_rebuilding_terms.do_rebuild_terms (are_rebuilding_terms t)

let loopify_state t = t.loopify_state

let set_loopify_state loopify_state t = { t with loopify_state }

let with_code_age_relation code_age_relation t =
  { t with
    typing_env = TE.with_code_age_relation t.typing_env code_age_relation
  }

let with_replay_history replay t =
  (* CR gbury: should we try and have a "dummy" replay history for the temporary
     envs created during join points ? *)
  let replay_history =
    match replay with
    | None -> Replay_history.first_pass
    | Some (previous_history, always_inline) ->
      Replay_history.replay ~always_inline previous_history
  in
  { t with replay_history }

let enter_continuation_handler lifted_params t =
  let lifted =
    Lifted_cont_params.fold ~init:Variable.Set.empty
      ~f:(fun bp set -> Variable.Set.add (Bound_parameter.var bp) set)
      lifted_params
  in
  { t with
    lifted;
    defined_variables_by_scope = lifted_params :: t.defined_variables_by_scope;
    cost_of_lifting_continuations_out_of_current_one = 0
  }

let variables_defined_in_current_continuation t =
  match t.defined_variables_by_scope with
  | [] -> Misc.fatal_errorf "Empty stack of defined_variables_by_scope in denv"
  | res :: _ -> res

let cost_of_lifting_continuations_out_of_current_one t =
  t.cost_of_lifting_continuations_out_of_current_one

let add_lifting_cost cost t =
  if cost < 0
  then Misc.fatal_errorf "Negative cost of lifting continuation"
  else
    { t with
      cost_of_lifting_continuations_out_of_current_one =
        t.cost_of_lifting_continuations_out_of_current_one + cost
    }

let must_inline t = Replay_history.must_inline t.replay_history

let replay_history t = t.replay_history

let map_specialization_cost ~f t =
  { t with specialization_cost = f t.specialization_cost }

let specialization_cost t = t.specialization_cost

let denv_for_lifted_continuation ~denv_for_join ~denv =
  (* At this point, we are lifting a continuation k' with handler [handler], out
     of a continuation k, and:

     - [denv_for_join] is the denv just before the let_cont for k

     - [denv] is the denv just before the let_cont for k'

     And we need to decide which parts of denv to use to simplify the handler of
     k' after they are lifted out from the handler of k. *)
  { (* denv *)
    inlined_debuginfo = denv.inlined_debuginfo;
    disable_inlining = denv.disable_inlining;
    inlining_state = denv.inlining_state;
    all_code = denv.all_code;
    inlining_history_tracker = denv.inlining_history_tracker;
    (* denv_for_join *)
    typing_env = denv_for_join.typing_env;
    at_unit_toplevel = denv_for_join.at_unit_toplevel;
    variables_defined_at_toplevel = denv_for_join.variables_defined_at_toplevel;
    cse = denv_for_join.cse;
    comparison_results = denv_for_join.comparison_results;
    replay_history = denv_for_join.replay_history;
    specialization_cost = denv_for_join.specialization_cost;
    defined_variables_by_scope = denv_for_join.defined_variables_by_scope;
    lifted = denv_for_join.lifted;
    cost_of_lifting_continuations_out_of_current_one =
      denv_for_join.cost_of_lifting_continuations_out_of_current_one;
    (* For the following fields, both denvs should have the same value of these
       fields *)
    round = denv.round;
    propagating_float_consts = denv.propagating_float_consts;
    unit_toplevel_return_continuation = denv.unit_toplevel_return_continuation;
    unit_toplevel_exn_continuation = denv.unit_toplevel_exn_continuation;
    are_rebuilding_terms = denv.are_rebuilding_terms;
    closure_info = denv.closure_info;
    get_imported_code = denv.get_imported_code;
    loopify_state = denv.loopify_state
  }
