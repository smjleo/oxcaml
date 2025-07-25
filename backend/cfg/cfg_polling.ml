[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
module List = ListLabels
module String = Misc.Stdlib.String
module DLL = Oxcaml_utils.Doubly_linked_list

let function_is_assumed_to_never_poll func =
  String.begins_with ~prefix:"caml_apply" func
  || String.begins_with ~prefix:"caml_send" func

let is_disabled fun_name =
  (not Config.poll_insertion)
  || !Oxcaml_flags.disable_poll_insertion
  || function_is_assumed_to_never_poll fun_name

(* These are used for the poll error annotation later on*)
type polling_point =
  | Alloc
  | Poll
  | Function_call
  | External_call

type error = Poll_error of Debuginfo.t * (polling_point * Debuginfo.t) list

exception Error of error

(* "Might_not_poll" means there exists a path from the function entry to a
   Potentially Recursive Tail Call (an Itailcall_ind or Itailcall_imm to a
   forward function) that does not go through an Ialloc or Ipoll instruction.

   "Always_polls", therefore, means the function always polls (via Ialloc or
   Ipoll) before doing a PRTC. *)

type polls_before_prtc =
  | Might_not_poll
  | Always_polls

module Polls_before_prtc = struct
  type t = polls_before_prtc

  let bot = Always_polls

  let join t1 t2 =
    match t1, t2 with
    | Might_not_poll, Might_not_poll
    | Might_not_poll, Always_polls
    | Always_polls, Might_not_poll ->
      Might_not_poll
    | Always_polls, Always_polls -> Always_polls

  let lessequal t1 t2 =
    match t1, t2 with
    | Always_polls, Always_polls
    | Always_polls, Might_not_poll
    | Might_not_poll, Might_not_poll ->
      true
    | Might_not_poll, Always_polls -> false
end

(* Error report *)

let instr_type p =
  match p with
  | Poll -> "inserted poll"
  | Alloc -> "allocation"
  | Function_call -> "function call"
  | External_call -> "external call that allocates"

let report_error ppf = function
  | Poll_error (_fun_dbg, instrs) ->
    let num_inserted_polls =
      List.fold_left
        ~f:(fun s (p, _) ->
          s
          +
          match p with Poll -> 1 | Alloc | Function_call | External_call -> 0)
        ~init:0 instrs
    in
    let num_user_polls = List.length instrs - num_inserted_polls in
    if num_user_polls = 0
    then
      Format.fprintf ppf
        "Function with poll-error attribute contains polling points (inserted \
         by the compiler)\n"
    else
      Format.fprintf ppf
        "Function with poll-error attribute contains polling points:\n";
    List.iter
      ~f:(fun (p, dbg) ->
        match p with
        | Poll | Alloc | Function_call | External_call ->
          Format.fprintf ppf "\t%s" (instr_type p);
          if not (Debuginfo.is_none dbg)
          then (
            Format.fprintf ppf " at ";
            Location.print_loc ppf (Debuginfo.to_location dbg));
          Format.fprintf ppf "\n")
      (List.sort
         ~cmp:(fun (_, left) (_, right) -> Debuginfo.compare left right)
         instrs)

let () =
  Location.register_error_of_exn (function
    | Error err -> (
      match err with
      | Poll_error (fun_dbg, _instrs) ->
        let loc = Debuginfo.to_location fun_dbg in
        Some (Location.error_of_printer ~loc report_error err))
    | _ -> None)

(* Compututation of the "safe" map, which is a map from labels to booleans where
   `true` indicates the block contains a safe point such as a poll or an alloc
   instruction. *)

(* CR-soon xclerc for xclerc: given how we use the safe map below, it is not
   clear taking into accounts terminator makes a difference; maybe matching over
   the terminator to always return `false` would be better. *)

let is_safe_basic : Cfg.basic Cfg.instruction -> bool =
 fun instr -> Cfg.is_alloc instr || Cfg.is_poll instr

let is_safe_terminator : Cfg.terminator Cfg.instruction -> bool =
 fun term ->
  match term.desc with
  | Never -> assert false
  | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
  | Switch _ ->
    false
  | Raise _ -> false
  | Tailcall_self _ | Tailcall_func _ | Return -> true
  | Call_no_return _ | Call _ | Prim _ -> false

let is_safe_block : Cfg.basic_block -> bool =
 fun block ->
  is_safe_terminator block.terminator || DLL.exists block.body ~f:is_safe_basic

let safe_map_of_cfg : Cfg.t -> bool Label.Tbl.t =
 fun cfg ->
  Cfg.fold_blocks cfg
    ~init:(Label.Tbl.create (Label.Tbl.length cfg.blocks))
    ~f:(fun label block acc ->
      Label.Tbl.replace acc label (is_safe_block block);
      acc)

(* Detection of functions that can loop via a tail-call without going through a
   poll point. *)

(* We use a backwards dataflow analysis to compute a single value: either
   "Might_not_poll" or "Always_polls".

   "Might_not_poll" means there exists a path from the function entry to a
   Potentially Recursive Tail Call (a Tailcall_self of Tailcall_func which is
   either indirect or to a forward function) that does not go through an Alloc
   or Poll instruction.

   "Always_polls", therefore, means the function always polls (via Alloc or
   Poll) before doing a PRTC. *)

module Polls_before_prtc_domain = struct
  type t = Polls_before_prtc.t

  let bot = Polls_before_prtc.bot

  let join = Polls_before_prtc.join

  let less_equal = Polls_before_prtc.lessequal
end

module Polls_before_prtc_transfer = struct
  type domain = Polls_before_prtc_domain.t

  type context =
    { future_funcnames : String.Set.t;
      optimistic_prologue_poll_instr_id : InstructionId.t
    }

  type error = |

  let basic :
      domain -> Cfg.basic Cfg.instruction -> context -> (domain, error) result =
   fun dom instr { future_funcnames = _; optimistic_prologue_poll_instr_id } ->
    match instr.desc with
    | Op Poll ->
      if InstructionId.equal instr.id optimistic_prologue_poll_instr_id
      then Ok dom
      else Ok Always_polls
    | Op (Alloc _) -> Ok Always_polls
    | Op
        ( Move | Spill | Reload | Opaque | Begin_region | End_region | Dls_get
        | Pause | Const_int _ | Const_float32 _ | Const_float _ | Const_symbol _
        | Const_vec128 _ | Const_vec256 _ | Const_vec512 _ | Stackoffset _
        | Load _
        | Store (_, _, _)
        | Intop _
        | Intop_imm (_, _)
        | Intop_atomic _
        | Floatop (_, _)
        | Csel _ | Reinterpret_cast _ | Static_cast _ | Probe_is_enabled _
        | Specific _ | Name_for_debugger _ )
    | Reloadretaddr | Pushtrap _ | Poptrap _ | Prologue | Stack_check _ ->
      Ok dom

  let terminator :
      domain ->
      exn:domain ->
      Cfg.terminator Cfg.instruction ->
      context ->
      (domain, error) result =
   fun dom ~exn term { future_funcnames; optimistic_prologue_poll_instr_id = _ } ->
    match term.desc with
    | Never -> assert false
    | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
    | Switch _ ->
      Ok dom
    | Raise _ -> Ok exn
    | Tailcall_self _ | Tailcall_func Indirect -> Ok Might_not_poll
    | Tailcall_func (Direct func) ->
      if String.Set.mem func.sym_name future_funcnames
         || function_is_assumed_to_never_poll func.sym_name
      then Ok Might_not_poll
      else Ok Always_polls
    | Return -> Ok Always_polls
    | Call_no_return _ | Call _ | Prim _ ->
      if Cfg.can_raise_terminator term.desc
      then Ok (Polls_before_prtc_domain.join dom exn)
      else Ok dom

  let exception_ : domain -> context -> (domain, error) result =
   fun dom { future_funcnames = _; optimistic_prologue_poll_instr_id = _ } ->
    Ok dom
end

let potentially_recursive_tailcall :
    future_funcnames:String.Set.t ->
    optimistic_prologue_poll_instr_id:InstructionId.t ->
    Cfg.t ->
    Polls_before_prtc_domain.t =
 fun ~future_funcnames ~optimistic_prologue_poll_instr_id cfg ->
  let module PTRCAnalysis =
    Cfg_dataflow.Backward
      (Polls_before_prtc_domain)
      (Polls_before_prtc_transfer)
  in
  let init : Polls_before_prtc_domain.t = Polls_before_prtc_domain.bot in
  match
    PTRCAnalysis.run ~init ~map:PTRCAnalysis.Block cfg
      { future_funcnames; optimistic_prologue_poll_instr_id }
  with
  | Ok res -> (
    match Label.Tbl.find_opt res cfg.entry_label with
    | None ->
      Misc.fatal_errorf
        "Cfg_polling.potentially_recursive_tailcall: missing entry label %a"
        Label.print cfg.entry_label
    | Some res -> res)
  | Aborted _ -> .
  | Max_iterations_reached ->
    Misc.fatal_error
      "Cfg_polling.potentially_recursive_tailcall has been interrupted"

(* Insertion of poll instruction onto back edges: a poll instruction is needed
   if there is an unsafe path from the header of the loop to the back edge. An
   unsafe path is simply a path such that all blocks are unsafe. *)

let exists_unsafe_path :
    Cfg.t -> safe_map:bool Label.Tbl.t -> from:Label.t -> to_:Label.t -> bool =
 fun cfg ~safe_map ~from ~to_ ->
  let exception Found in
  try
    let open_ = ref (Label.Set.singleton from) in
    let closed = ref Label.Set.empty in
    while not (Label.Set.is_empty !open_) do
      let label = Label.Set.choose !open_ in
      if Label.equal label to_ then raise Found;
      open_ := Label.Set.remove label !open_;
      closed := Label.Set.add label !closed;
      match Label.Tbl.find_opt safe_map label with
      | None ->
        Misc.fatal_errorf
          "Cfg_polling.exists_unsafe_path: missing safety information for \
           block %a"
          Label.format label
      | Some true -> ()
      | Some false ->
        let block = Cfg.get_block_exn cfg label in
        let successor_labels =
          Cfg.successor_labels ~normal:true ~exn:true block
        in
        Label.Set.iter
          (fun successor_label ->
            match Label.Set.mem successor_label !closed with
            | true -> ()
            | false -> open_ := Label.Set.add successor_label !open_)
          successor_labels
    done;
    false
  with Found -> true

let instr_cfg_with_layout :
    Cfg_with_layout.t ->
    safe_map:bool Label.Tbl.t ->
    back_edges:Cfg_edge.Set.t ->
    bool =
 fun cfg_with_layout ~safe_map ~back_edges ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let next_instruction_id () =
    InstructionId.get_and_incr cfg.next_instruction_id
  in
  Cfg_edge.Set.fold
    (fun { Cfg_edge.src; dst } added_poll ->
      let needs_poll =
        (not (Label.Tbl.find safe_map src))
        && exists_unsafe_path cfg ~safe_map ~from:dst ~to_:src
      in
      if needs_poll
      then (
        let after = Cfg.get_block_exn cfg src in
        let poll =
          { after.terminator with
            Cfg.id = next_instruction_id ();
            Cfg.desc = Cfg.Op Poll
          }
        in
        (match
           ( Label.Set.cardinal
               (Cfg.successor_labels after ~normal:true ~exn:false),
             after.exn )
         with
        | 1, None -> DLL.add_end after.body poll
        | _ ->
          let before = Some (Cfg.get_block_exn cfg dst) in
          let instrs = DLL.of_list [poll] in
          let inserted_blocks =
            Cfg_with_layout.insert_block cfg_with_layout instrs ~after ~before
              ~next_instruction_id
          in
          (* All the inserted blocks are safe since they contain a poll
             instruction *)
          List.iter inserted_blocks ~f:(fun block ->
              Label.Tbl.replace safe_map block.Cfg.start true));
        true)
      else added_poll)
    back_edges false

type polling_points = (polling_point * Debuginfo.t) list

let add_poll_or_alloc_basic :
    Cfg.basic Cfg.instruction -> polling_points -> polling_points =
 fun instr points ->
  match instr.desc with
  | Op op -> (
    match op with
    | Move | Spill | Reload | Const_int _ | Const_float32 _ | Const_float _
    | Const_symbol _ | Const_vec128 _ | Const_vec256 _ | Const_vec512 _
    | Stackoffset _ | Load _ | Store _ | Intop _ | Intop_imm _ | Intop_atomic _
    | Floatop _ | Csel _ | Reinterpret_cast _ | Static_cast _
    | Probe_is_enabled _ | Opaque | Begin_region | End_region | Specific _
    | Name_for_debugger _ | Dls_get | Pause ->
      points
    | Poll -> (Poll, instr.dbg) :: points
    | Alloc _ -> (Alloc, instr.dbg) :: points)
  | Reloadretaddr | Pushtrap _ | Poptrap _ | Prologue | Stack_check _ -> points

let add_calls_terminator :
    Cfg.terminator Cfg.instruction -> polling_points -> polling_points =
 fun term points ->
  match term.desc with
  | Never -> assert false
  | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
  | Switch _ | Return | Raise _ ->
    points
  | Tailcall_self _ | Tailcall_func _ -> (Function_call, term.dbg) :: points
  | Call _ -> (Function_call, term.dbg) :: points
  | Call_no_return
      { alloc = false;
        func_symbol = _;
        ty_res = _;
        ty_args = _;
        stack_ofs = _;
        stack_align = _;
        effects = _
      }
  | Prim
      { op =
          External
            { alloc = false;
              func_symbol = _;
              ty_res = _;
              ty_args = _;
              stack_ofs = _;
              stack_align = _;
              effects = _
            };
        label_after = _
      } ->
    points
  | Call_no_return
      { alloc = true;
        func_symbol = _;
        ty_res = _;
        ty_args = _;
        stack_ofs = _;
        stack_align = _;
        effects = _
      }
  | Prim
      { op =
          External
            { alloc = true;
              func_symbol = _;
              ty_res = _;
              ty_args = _;
              stack_ofs = _;
              stack_align = _;
              effects = _
            };
        label_after = _
      } ->
    (External_call, term.dbg) :: points
  | Prim { op = Probe _; label_after = _ } -> points

let find_poll_alloc_or_calls : Cfg.t -> polling_points =
 fun cfg ->
  Cfg.fold_blocks cfg ~init:[] ~f:(fun _label block acc ->
      let acc =
        DLL.fold_right ~init:acc ~f:add_poll_or_alloc_basic block.body
      in
      let acc = add_calls_terminator block.terminator acc in
      acc)

let contains_polls : Cfg.t -> bool =
 fun cfg ->
  let exception Found in
  try
    Cfg.iter_blocks cfg ~f:(fun _label block ->
        let has_poll_instr = DLL.exists block.body ~f:Cfg.is_poll in
        if has_poll_instr then raise Found);
    false
  with Found -> true

let instrument_fundecl :
    future_funcnames:Misc.Stdlib.String.Set.t ->
    Cfg_with_layout.t ->
    Cfg_with_layout.t =
 fun ~future_funcnames:_ cfg_with_layout ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  if is_disabled cfg.fun_name
  then cfg_with_layout
  else
    let safe_map = safe_map_of_cfg cfg in
    (* CR-soon xclerc for xclerc: consider using `Cfg_with_infos` to cache the
       computations *)
    let doms = Cfg_dominators.build cfg in
    let back_edges = Cfg_loop_infos.compute_back_edges cfg doms in
    let added_poll =
      instr_cfg_with_layout cfg_with_layout ~safe_map ~back_edges
    in
    (match cfg.fun_poll with
    | Error_poll -> (
      match find_poll_alloc_or_calls cfg with
      | [] -> ()
      | poll_error_instrs ->
        let poll_error_instrs =
          List.sort
            ~cmp:(fun left right -> Debuginfo.compare (snd left) (snd right))
            poll_error_instrs
        in
        raise (Error (Poll_error (cfg.fun_dbg, poll_error_instrs))))
    | Default_poll -> ());
    let new_contains_calls =
      (* `added_poll` is used to avoid iterating over the CFG if we have added a
         Poll instruction *)
      cfg.fun_contains_calls || added_poll || contains_polls cfg
    in
    let cfg = { cfg with fun_contains_calls = new_contains_calls } in
    Cfg_with_layout.create cfg ~layout:(Cfg_with_layout.layout cfg_with_layout)

let requires_prologue_poll :
    future_funcnames:Misc.Stdlib.String.Set.t ->
    fun_name:string ->
    optimistic_prologue_poll_instr_id:InstructionId.t ->
    Cfg.t ->
    bool =
 fun ~future_funcnames ~fun_name ~optimistic_prologue_poll_instr_id cfg ->
  if is_disabled fun_name
  then false
  else
    match
      potentially_recursive_tailcall ~future_funcnames
        ~optimistic_prologue_poll_instr_id cfg
    with
    | Might_not_poll -> true
    | Always_polls -> false
