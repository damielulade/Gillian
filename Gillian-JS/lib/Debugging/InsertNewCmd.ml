open Gillian.Debugger
open Gillian.Debugger.Utils.Exec_map
open Gillian.Debugger.Lifter
open Gillian.Utils
open Gillian.Utils.Prelude
open Gillian.Utils.Syntaxes.Option

(* open PartialTypes *)
open Javert_utils
open Javert_utils.Js_branch_case
open LifterUtils
open LifterTypes

module Make
    (Gil : Gillian.Debugger.Lifter.Gil_fallback_lifter.Gil_lifter_with_state)
    (State : State.S with type gil_state_t = Gil.Lifter.t)
    (PartialCmds : sig
      (* type finished [@@deriving yojson] *)
      (* val finished_to_yojson : finished -> Packaged.branch_case *)
    end)
    (Utils : sig
      val package_node :
        ('a, Js_branch_case.t, cmd_data, 'b) node ->
        ('a, Packaged.branch_case, Packaged.cmd_data, string) node
    end) =
struct
  open State
  open Utils

  let failwith ~state ~finished_partial msg =
    Logging.failwith
      (fun () ->
        [
          ("state", to_yojson state);
          ("finished_partial", PartialTypes.finished_to_yojson finished_partial);
        ])
      ("JSLifter.insert_new_cmd: " ^ msg)

  let with_prev prev { data; next } =
    let data = { data with prev } in
    { data; next }

  let make_new_cmd ~func_return_label finished_partial =
    let PartialTypes.
          {
            all_ids;
            id;
            display;
            matches;
            errors;
            prev;
            next_kind;
            callers;
            loc;
            _;
          } =
      finished_partial
    in
    let data =
      {
        all_ids;
        id;
        display;
        matches;
        errors;
        submap = NoSubmap;
        prev;
        callers;
        func_return_label;
        loc;
      }
    in
    let next =
      match next_kind with
      | Zero -> None
      | One bdata -> Some (Single (None, bdata))
      | Many ends ->
          let nexts =
            List.map (fun (case, bdata) -> (case, (None, bdata))) ends
          in
          Some (Branch nexts)
    in
    { data; next }

  let insert_as_submap ~state ~parent_id new_id =
    let++ parent =
      map_node_extra_exn state.map parent_id (fun parent ->
          match parent.data.submap with
          | Proc _ | Submap _ -> (parent, Error "duplicate submaps!")
          | NoSubmap ->
              let data = { parent.data with submap = Submap new_id } in
              let parent = { parent with data } in
              (parent, Ok parent))
    in
    let () =
      Effect.perform (Node_updated (parent_id, Some (package_node parent)))
    in
    ()

  let insert_as_next ~state ~prev_id ?case new_id =
    let++ new_prev =
      map_node_extra_exn state.map prev_id (fun prev ->
          let new_next =
            let** next =
              match prev.next with
              | Some next -> Ok next
              | None -> Error "trying to insert next of a final command!"
            in
            match (next, case) with
            | Single _, Some _ ->
                Error "tyring to insert into a non-branch cmd with branch"
            | Branch _, None ->
                Error "trying to insert into branch cmd with no branch"
            | Single (Some _, _), _ -> Error "duplicate insertion"
            | Single (None, bdata), None ->
                Ok (Some (Single (Some new_id, bdata)))
            | Branch nexts, Some case -> (
                match List.assoc_opt case nexts with
                | None -> Error "case not found"
                | Some (Some _, _) -> Error "duplicate insertion"
                | Some (None, bdata) ->
                    let nexts =
                      List_utils.assoc_replace case (Some new_id, bdata) nexts
                    in
                    Ok (Some (Branch nexts)))
          in
          match new_next with
          | Ok next ->
              let prev = { prev with next } in
              (prev, Ok prev)
          | Error e ->
              (prev, Fmt.error "insert_as_next (%a) - %s" pp_id prev_id e))
    in
    let () =
      Effect.perform (Node_updated (prev_id, Some (package_node new_prev)))
    in
    ()

  let insert_to_empty_map ~state ~prev ~stack_direction new_cmd =
    let- () =
      match state.map.root with
      | Some _ -> Some None
      | None -> None
    in
    let r =
      match (stack_direction, prev) with
      | Some _, _ -> Error "stepping in or out with an empty map!"
      | _, Some _ -> Error "inserting into an empty map with prev!"
      | None, None ->
          let new_cmd = new_cmd |> with_prev None in
          let () = state.map.root <- Some new_cmd.data.id in
          Ok new_cmd
    in
    Some r

  let insert_cmd ~state ~prev ~stack_direction new_cmd =
    let- () = insert_to_empty_map ~state ~prev ~stack_direction new_cmd in
    match (stack_direction, prev) with
    | _, None -> Error "inserting into non-empty map with no prev"
    | Some In, Some (parent_id, Some FuncExitPlaceholder)
    | Some In, Some (parent_id, None) ->
        let new_cmd = new_cmd |> with_prev None in
        let++ () = insert_as_submap ~state ~parent_id new_cmd.data.id in
        new_cmd
    | Some In, Some (_, Some _) -> Error "stepping in with branch case!"
    | None, Some (prev_id, case) ->
        let++ () = insert_as_next ~state ~prev_id ?case new_cmd.data.id in
        new_cmd
    | Some (Out prev_id), Some (inner_prev_id, _) ->
        let** case =
          let func_return_label =
            (get_exn state.map inner_prev_id).data.func_return_label
          in
          match func_return_label with
          | Some (label, ix) -> Ok (Case (FuncExit label, ix))
          | None -> Error "stepping out without function return label!"
        in
        let new_cmd = new_cmd |> with_prev (Some (prev_id, Some case)) in
        let++ () = insert_as_next ~state ~prev_id ~case new_cmd.data.id in
        new_cmd

  let new_function_return_label caller_id state =
    state.func_return_count <- state.func_return_count + 1;
    let label = int_to_letters state.func_return_count in
    let count = ref 0 in
    Hashtbl.add state.func_return_map caller_id (label, count);
    (label, count)

  let update_caller_branches ~caller_id ~cont_id (label, ix) state =
    let result =
      map_node_extra state.map caller_id (fun node ->
          let new_next =
            match node.next with
            | Some (Branch nexts) ->
                let nexts = List.remove_assoc FuncExitPlaceholder nexts in
                let case = Case (FuncExit label, ix) in
                let bdata = (cont_id, None) in
                let nexts = nexts @ [ (case, (None, bdata)) ] in
                Ok (Some (Branch nexts))
            | None | Some (Single _) ->
                Fmt.error "update_caller_branches - caller %a does not branch"
                  pp_id caller_id
          in
          match new_next with
          | Ok next ->
              let node = { node with next } in
              (node, Ok node)
          | Error e -> (node, Error e))
    in
    match result with
    | Some r ->
        let++ new_node = r in
        let () =
          Effect.perform
            (Node_updated (caller_id, Some (package_node new_node)))
        in
        ()
    | None ->
        Fmt.error "update_caller_branches - caller %a not found" pp_id caller_id

  let resolve_func_branches ~state finished_partial =
    let PartialTypes.{ all_ids; next_kind; callers; has_return; _ } =
      finished_partial
    in
    match (next_kind, has_return, callers) with
    | Zero, true, caller_id :: _ ->
        let label, count =
          match Hashtbl.find_opt state.func_return_map caller_id with
          | Some (label, count) -> (label, count)
          | None -> new_function_return_label caller_id state
        in
        incr count;
        let label = (label, !count) in
        let cont_id = all_ids |> List.rev |> List.hd in
        let** () = update_caller_branches ~caller_id ~cont_id label state in
        Ok (Some label)
    | _ -> Ok None

  let f ~state finished_partial =
    let r =
      let PartialTypes.{ id; all_ids; prev; stack_direction; _ } =
        finished_partial
      in
      let** func_return_label = resolve_func_branches ~state finished_partial in
      let new_cmd = make_new_cmd ~func_return_label finished_partial in
      let** new_cmd = insert_cmd ~state ~prev ~stack_direction new_cmd in
      let () = insert state.map ~id ~all_ids new_cmd in
      let () =
        Effect.perform (Node_updated (id, Some (package_node new_cmd)))
      in
      Ok new_cmd
    in
    Result_utils.or_else (failwith ~state ~finished_partial) r
end
