open Gillian.Utils.Syntaxes.Option
open Gillian.Utils
open Gillian.Debugger
open Gillian.Debugger.Lifter
open Gillian.Debugger.Utils.Exec_map
open LifterTypes
open Javert_utils.Js_branch_case
open Javert_utils
module PC = Js2jsil_lib.JS2GIL_ParserAndCompiler

module HelperFuncs
    (Gil : Gillian.Debugger.Lifter.Gil_fallback_lifter.Gil_lifter_with_state)
    (V : Gillian.Abstraction.Verifier.S with type annot = PC.Annot.t)
    (State : State.S with type gil_state_t = Gil.Lifter.t)
    (Utils : sig
      val package_case : Js_branch_case.t -> Packaged.branch_case * string
      val path_of_id : id -> State.t -> Gil_syntax.Branch_case.path
    end)
    (Types : sig
      type cmd_report = V.SAInterpreter.Logging.ConfigReport.t
      [@@deriving yojson]

      type exec_data = cmd_report executed_cmd_data [@@deriving yojson]
      type _ Effect.t += Step : step_args -> exec_data Effect.t
    end)
    (InitOrHandle : sig
      val f :
        state:State.t ->
        ?prev_id:id ->
        ?gil_case:Gil_syntax.Branch_case.t ->
        Types.exec_data ->
        ( id * Gil_syntax.Branch_case.t option,
          (id, Js_branch_case.t, cmd_data, branch_data) node )
        Either.t
    end) =
struct
  (* open Gil *)
  open Types
  open State
  open Utils

  let previous_step id { map; _ } =
    let+ id, case = (get_exn map id).data.prev in
    let case = case |> Option.map package_case in
    (id, case)

  let is_breakpoint ~start ~current =
    let b =
      let+ file, line = current.data.loc in
      let- () =
        let* file', line' = start.data.loc in
        if file = file' && line = line' then Some false else None
      in
      Effect.perform (IsBreakpoint (file, [ line ]))
    in
    Option.value b ~default:false

  let select_case nexts =
    let result =
      List.fold_left
        (fun acc (_, (next, id_case)) ->
          match (next, acc) with
          | _, Some (Either.Right _) -> acc
          | None, _ -> Some (Either.Right id_case)
          | Some id, None -> Some (Either.Left id)
          | _ -> acc)
        None nexts
    in
    Option.get result

  (* If a FinalCmd is in a function call, get the caller ID
     and the relevant branch case for stepping forward,
     while checking that it actually exists. *)
  let get_next_from_end state { callers; func_return_label; _ } =
    let* caller_id = List_utils.hd_opt callers in
    let* label, ix = func_return_label in
    let case = Case (FuncExit label, ix) in
    let* _ =
      match (get_exn state.map caller_id).next with
      | Some (Branch nexts) -> List.assoc_opt case nexts
      | _ -> None
    in
    Some (caller_id, Some case)

  let request_next state id case =
    let rec aux id case =
      let path = path_of_id id state in
      let exec_data = Effect.perform (Step (Some id, case, path)) in
      match InitOrHandle.f ~state ~prev_id:id ?gil_case:case exec_data with
      | Either.Left (id, case) -> aux id case
      | Either.Right map -> map.data.id
    in
    aux id case

  let rec find_next state id case =
    let node = get_exn state.map id in
    match (node.next, case, node.data.submap) with
    | (None | Some (Single _)), Some _, _ ->
        failwith "HORROR - tired to step case for non-branch cmd"
    | ( Some (Branch [ (FuncExitPlaceholder, _) ]),
        Some FuncExitPlaceholder,
        Submap submap_id ) -> Either.left submap_id
    | Some (Single (None, _)), None, _ ->
        let id = List.hd (List.rev node.data.all_ids) in
        let case =
          match Gil.Lifter.cases_at_id id state.gil_state with
          | [] -> None
          | [ case ] -> Some case
          | _ ->
              Fmt.failwith
                "find_next: id %a has multiple cases - not sure where to step!"
                L.Report_id.pp id
        in
        Either.Right (id, case)
    | Some (Single (Some next, _)), None, _ -> Either.left next
    | Some (Branch nexts), None, _ -> select_case nexts
    | Some (Branch nexts), Some case, _ -> (
        match List.assoc_opt case nexts with
        | None -> failwith "case not found"
        | Some (None, id_case) -> Either.Right id_case
        | Some (Some next, _) -> Either.Left next)
    | None, None, _ -> (
        match get_next_from_end state node.data with
        | Some (id, case) -> find_next state id case
        | None -> Either.Left id)

  let step state id case =
    let () =
      Logging.log (fun m ->
          m "Stepping %a %a" pp_id id (pp_option Js_branch_case.pp) case)
    in
    match find_next state id case with
    | Either.Left next -> next
    | Either.Right (id, case) -> request_next state id case

  let step_all ~start state id case =
    let cmd = get_exn state.map id in
    let stack_depth = List.length cmd.data.callers in
    let rec aux ends = function
      | [] -> List.rev ends
      | (id, case) :: rest ->
          let next_id = step state id case in
          let node = get_exn state.map next_id in
          let ends, nexts =
            let- () =
              if is_breakpoint ~start ~current:node then
                Some (node :: ends, rest)
              else None
            in
            match node.next with
            | Some (Single _) -> (ends, (next_id, None) :: rest)
            | Some (Branch nexts) ->
                let new_nexts =
                  nexts |> List.map (fun (case, _) -> (next_id, Some case))
                in
                (ends, new_nexts @ rest)
            | None ->
                let stack_depth' = List.length node.data.callers in
                if stack_depth' < stack_depth then
                  failwith "Stack depth too small!"
                else if stack_depth' > stack_depth then
                  match get_next_from_end state node.data with
                  | Some next -> (ends, next :: rest)
                  | None -> (node :: ends, rest)
                else (node :: ends, rest)
          in
          aux ends nexts
    in
    match (case, cmd.next) with
    | _, None -> [ cmd ]
    | None, Some (Branch nexts) ->
        let first_steps =
          nexts |> List.map (fun (case, _) -> (id, Some case))
        in
        aux [] first_steps
    | _, _ -> aux [] [ (id, case) ]
end

module Make
    (Gil : Gillian.Debugger.Lifter.Gil_fallback_lifter.Gil_lifter_with_state)
    (V : Gillian.Abstraction.Verifier.S with type annot = PC.Annot.t)
    (State : State.S with type gil_state_t = Gil.Lifter.t)
    (Utils : sig
      val package_case : Js_branch_case.t -> Packaged.branch_case * string
      val path_of_id : id -> State.t -> Gil_syntax.Branch_case.path
    end)
    (Types : sig
      type cmd_report = V.SAInterpreter.Logging.ConfigReport.t
      [@@deriving yojson]

      type exec_data = cmd_report executed_cmd_data [@@deriving yojson]
      type _ Effect.t += Step : step_args -> exec_data Effect.t
    end)
    (InitOrHandle : sig
      val f :
        state:State.t ->
        ?prev_id:id ->
        ?gil_case:Gil_syntax.Branch_case.t ->
        Types.exec_data ->
        ( id * Gil_syntax.Branch_case.t option,
          (id, Js_branch_case.t, cmd_data, branch_data) node )
        Either.t
    end) =
struct
  open State
  include HelperFuncs (Gil) (V) (State) (Utils) (Types) (InitOrHandle)

  let step_branch state id case =
    (* let { map; _ } = state in *)
    let case =
      let+ json = case in
      json |> Js_branch_case.of_yojson |> Result.get_ok
    in
    let cmd = get_exn state.map id in
    (* Bodge: step in if on func exit placeholder *)
    let- () =
      match (case, cmd.data.submap) with
      | Some FuncExitPlaceholder, Submap submap_id ->
          Some (submap_id, Debugger_utils.Step)
      | _ -> None
    in
    let next_id = step state id case in
    (next_id, Debugger_utils.Step)

  let step_in state id =
    let cmd = get_exn state.map id in
    (* Only BranchCmds should have submaps *)
    let- () =
      match cmd.data.submap with
      | NoSubmap | Proc _ -> None
      | Submap submap_id -> Some (submap_id, Debugger_utils.Step)
    in
    step_branch state id None

  let step_over state id =
    let node = get_exn state.map id in
    let () =
      let () =
        match (node.next, node.data.submap) with
        | Some (Branch nexts), (NoSubmap | Proc _) ->
            if List.mem_assoc FuncExitPlaceholder nexts then
              step state id (Some FuncExitPlaceholder) |> ignore
        | _ -> ()
      in
      let node = get_exn state.map id in
      let> submap_id =
        match node.data.submap with
        | NoSubmap | Proc _ -> None
        | Submap m -> Some m
      in
      let _ = step_all ~start:node state submap_id None in
      ()
    in
    let node = get_exn state.map id in
    (* Failsafe in case of error paths in submap *)
    match node.next with
    | Some (Branch [ (FuncExitPlaceholder, _) ]) -> (id, Debugger_utils.Step)
    | _ -> step_branch state id None

  let step_back state id =
    let cmd = get_exn state.map id in
    let id =
      match cmd.data.prev with
      | Some (id, _) -> id
      | None -> id
    in
    (id, Debugger_utils.Step)

  let continue state id =
    let start = get_exn state.map id in
    let rec aux ends = function
      | [] -> ends
      | (id, case) :: rest ->
          let ends' = step_all ~start state id case in
          let ends', nexts =
            ends'
            |> List.partition_map (fun { data; _ } ->
                   match get_next_from_end state data with
                   | Some (id, case) -> Either.Right (id, case)
                   | None -> Either.Left data.id)
          in
          aux (ends @ ends') (nexts @ rest)
    in
    let ends = aux [] [ (id, None) ] in
    let id = List.hd ends in
    (id, Debugger_utils.Step)

  let step_out state id =
    match (get_exn state.map id).data.callers with
    | [] -> continue state id
    | caller_id :: _ -> step_over state caller_id

  let continue_back state id =
    let start = get_exn state.map id in
    let rec aux node =
      let { id; callers; _ } = node.data in
      if is_breakpoint ~start ~current:node then (id, Debugger_utils.Breakpoint)
      else
        match previous_step id state with
        | None -> (
            match callers with
            | [] -> (id, Debugger_utils.Step)
            | caller_id :: _ -> aux (get_exn state.map caller_id))
        | Some (id, _) -> aux (get_exn state.map id)
    in
    aux start
end
