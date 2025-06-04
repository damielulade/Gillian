open Javert_utils
open Js2jsil_lib
open Jsil_syntax
open Jsil_syntax.JS_Annot
open LifterTypes
open LifterUtils
open Gillian.Utils
open Gillian.Utils.Syntaxes.Option
module DL = Debugger_log
open Gillian.Debugger.Lifter
open Gillian.Debugger.Utils

module Update
    (V : Gillian.Abstraction.Verifier.S
           with type annot = JS2GIL_ParserAndCompiler.Annot.t)
    (Types : sig
      type cmd_report = V.SAInterpreter.Logging.ConfigReport.t
      [@@deriving yojson]

      type exec_data = cmd_report executed_cmd_data [@@deriving yojson]
    end) =
struct
  open PartialTypes
  open Types

  let get_is_end ({ cmd_kind; _ } : JS2GIL_ParserAndCompiler.Annot.t) =
    match cmd_kind with
    | Normal b -> Ok b
    | Context b -> Ok b
    | Hidden | Internal -> Ok false
    | Return | Exception -> Ok true
    | Unknown -> Error "HORROR - unknown cmd kind"

  let resolve_case
      ?gil_case
      (kind : Js_branch_case.kind option)
      (prev_case : Js_branch_case.case) =
    match (kind, prev_case) with
    | None, prev_case -> Ok prev_case
    | Some prev_kind, Unknown -> (
        match prev_kind with
        | IfElseKind | WhileLoopKind -> (
            match gil_case with
            | Some (Gil_syntax.Branch_case.GuardedGoto b) -> Ok (IfElse b)
            | _ -> Error "IfElseKind expects a GuardedGoto gil case"))
    | Some _, _ -> Error "HORROR - branch kind is set with pre-existing case!"

  let get_stack_info ~(partial : partial_data) (exec_data : exec_data) =
    match partial.prev with
    | None -> Ok ([], None)
    | Some (prev_id, _, prev_callers) -> (
        let cs = exec_data.cmd_report.callstack in
        let depth_change =
          assert ((List.hd cs).pid = exec_data.cmd_report.proc_name);
          let prev_depth = List.length prev_callers in
          let new_depth = List.length cs - 1 in
          (* @TODO look at this where -2 is used in C2 for the harness *)
          new_depth - prev_depth
        in
        match depth_change with
        | 0 -> Ok (prev_callers, None)
        | 1 -> Ok (prev_id :: prev_callers, Some In)
        | -1 -> (
            match prev_callers with
            | [] -> Error "HORROR - stepping out when prev_callers is empty"
            | hd :: tl -> Ok (tl, Some (Out hd)))
        | _ ->
            let callers =
              List.map (fun c -> V.SAInterpreter.Call_stack.(c.pid)) cs
            in
            Fmt.error "Horror - too great a stack depth change! (%d)\n[%a]"
              depth_change
              (Fmt.list ~sep:(Fmt.any ", ") Fmt.string)
              callers)

  let update_return_cmd_info
      ~id
      ~callers
      ~stack_direction
      (partial : partial_data) =
    partial.canonical_data <-
      Some
        {
          id;
          display = "<end of func>";
          callers;
          stack_direction;
          nest_kind = None;
          loc = None;
          cmd_kind = JS_Annot.Return;
        };
    Ok ()

  let update_funcall_kind
      ~(prog : tl_ast)
      ~(exec_data : exec_data)
      ~(annot : JS_Annot.t)
      (partial : partial_data) =
    let cmd_report = exec_data.cmd_report in
    let> () =
      match (partial.funcall_kind, annot.cmd_kind) with
      | Some Evaluated_funcall, _ | _, (Unknown | Hidden | Internal) -> None
      | _ -> Some ()
    in
    let> pid =
      match V.SAInterpreter.Logging.ConfigReport.(cmd_report.cmd) with
      | Call (_, Lit (String pid), _, _, _)
      | ECall (_, (Lit (String pid) | PVar pid), _, _) -> Some pid
      | _ -> None
    in
    let kind =
      let _ = prog in
      if List.mem pid JS2JSIL_Helpers.unevaluated_funcs then Unevaluated_funcall
      else Evaluated_funcall
    in
    let () = partial.funcall_kind <- Some kind in
    ()

  let update_paths ~is_end ~exec_data ~branch_case ~annot ~branch_kind partial =
    let ({ id; next_kind; _ } : exec_data) = exec_data in
    let { ends; unexplored_paths; _ } = partial in
    match (JS_Annot.(annot.cmd_kind), next_kind, is_end) with
    | _, Zero, _ -> Ok ()
    | Return, _, _ ->
        partial.has_return <- true;
        Ok ()
    | _, One (), false ->
        Stack.push (id, None) unexplored_paths;
        Ok ()
    | _, Many cases, false ->
        cases
        |> List.iter (fun (gil_case, ()) ->
               Stack.push (id, Some gil_case) unexplored_paths);
        Ok ()
    | _, One (), true ->
        Ext_list.add (branch_case, (id, None)) ends;
        Ok ()
    | _, Many cases, true ->
        cases
        |> List_utils.iter_results (fun (gil_case, ()) ->
               let++ case = resolve_case ~gil_case branch_kind branch_case in
               Ext_list.add (case, (id, Some gil_case)) ends)

  let update_canonical_data
      ~id
      ~(annot : JS_Annot.t)
      ~exec_data
      (partial : partial_data) =
    let- () =
      match annot.cmd_kind with
      | Return ->
          let result =
            let** callers, stack_direction =
              get_stack_info ~partial exec_data
            in
            update_return_cmd_info ~id ~callers ~stack_direction partial
          in
          Some result
      | _ -> None
    in
    match (annot.cmd_kind, partial.canonical_data, annot.display) with
    | Unknown, _, _ ->
        Fmt.error "HORROR - trying to get display of %a" pp_cmd_kind
          annot.cmd_kind
    | (Hidden | Internal), _, _ -> Ok ()
    | _, None, Some display ->
        let** callers, stack_direction = get_stack_info ~partial exec_data in
        let JS_Annot.{ nest_kind; cmd_kind; _ } = annot in
        let loc =
          let+ loc =
            annot.origin_loc |> Option.map location_to_display_location
          in
          (loc.loc_source, loc.loc_start.pos_line)
        in
        partial.canonical_data <-
          Some
            { id; display; callers; stack_direction; nest_kind; loc; cmd_kind };
        Ok ()
    | _ -> Ok ()

  let insert_id_and_case ~prev_id ~exec_data ~id ({ all_ids; _ } : partial_data)
      =
    let annot, gil_case =
      let { cmd_report; _ } = exec_data in
      V.SAInterpreter.Logging.ConfigReport.
        (cmd_report.annot, cmd_report.branch_case)
    in
    let prev_kind_case =
      let* prev_id = prev_id in
      Ext_list.assoc_opt prev_id all_ids
    in
    let kind = annot.branch_kind in
    let++ case =
      match prev_kind_case with
      | Some (prev_kind, prev_case) ->
          resolve_case ?gil_case prev_kind prev_case
      | None -> Ok Js_branch_case.(Unknown)
    in
    Ext_list.add (id, (kind, case)) all_ids;
    (kind, case)

  let f ~finish ~prog ~prev_id exec_data partial =
    let { id; cmd_report; matches; errors; _ } = exec_data in
    let annot = V.SAInterpreter.Logging.ConfigReport.(cmd_report.annot) in
    let** is_end = get_is_end annot in
    let** branch_kind, branch_case =
      insert_id_and_case ~prev_id ~exec_data ~id partial
    in
    let () = Ext_list.add_all matches partial.matches in
    let () = Ext_list.add_all errors partial.errors in
    let** () = update_canonical_data ~id ~annot ~exec_data partial in
    let** () =
      update_paths ~is_end ~exec_data ~branch_case ~annot ~branch_kind partial
    in
    let () = update_funcall_kind ~prog ~exec_data ~annot partial in

    (* Finish or continue *)
    match Stack.pop_opt partial.unexplored_paths with
    | None -> finish partial
    | Some (id, branch_case) -> StepAgain (id, branch_case) |> Result.ok
end

module Make
    (V : Gillian.Abstraction.Verifier.S
           with type annot = JS2GIL_ParserAndCompiler.Annot.t)
    (Types : sig
      type cmd_report = V.SAInterpreter.Logging.ConfigReport.t
      [@@deriving yojson]

      type exec_data = cmd_report executed_cmd_data [@@deriving yojson]
    end) =
struct
  open Types
  include PartialTypes
  module Update = Update (V) (Types)

  let ends_to_cases
      ~is_unevaluated_funcall
      ~nest_kind
      (ends : (Js_branch_case.case * branch_data) list) =
    let- () =
      match (nest_kind, ends) with
      | Some (FunCall _), [ (Unknown, bdata) ] ->
          if is_unevaluated_funcall then None
          else Some (Ok [ (Js_branch_case.FuncExitPlaceholder, bdata) ])
      | Some (FunCall _), _ ->
          Some (Error "Unexpected branching in cmd with FunCall nest")
      | _ -> None
    in
    let counts = Hashtbl.create 0 in
    let () =
      ends
      |> List.iter (fun (case_kind, _) ->
             let total, _ =
               Hashtbl.find_opt counts case_kind |> Option.value ~default:(0, 0)
             in
             Hashtbl.replace counts case_kind (total + 1, 0))
    in
    ends
    |> List.map (fun (kind, branch_data) ->
           let total, count = Hashtbl.find counts kind in
           let ix =
             match (kind, total) with
             | (Js_branch_case.IfElse _ | Js_branch_case.WhileLoop _), 1 -> -1
             | _ -> count
           in
           let () = Hashtbl.replace counts kind (total, count + 1) in
           (Js_branch_case.Case (kind, ix), branch_data))
    |> Result.ok

  let make_canonical_data_if_error
      ~(canonical_data : canonical_cmd_data option)
      ~ends
      ~errors
      ~all_ids
      ~prev : (canonical_cmd_data * partial_end list) option =
    let/ () = canonical_data |> Option.map (fun c -> (c, ends)) in
    match errors with
    | [] -> None
    | _ ->
        let _, _, callers = Option.get prev in
        let c =
          {
            id = all_ids |> List.rev |> List.hd;
            display = "<error>";
            callers;
            stack_direction = None;
            nest_kind = None;
            loc = None;
            cmd_kind = JS_Annot.Unknown;
          }
        in
        Some (c, [])

  let finish partial =
    let ({
           prev;
           canonical_data;
           funcall_kind;
           all_ids;
           ends;
           matches;
           errors;
           has_return;
           _;
         }
          : partial_data) =
      partial
    in
    let all_ids = all_ids |> Ext_list.to_list |> List.map fst in
    let matches = matches |> Ext_list.to_list in
    let errors = errors |> Ext_list.to_list in
    let ends = ends |> Ext_list.to_list in
    let canonical_data =
      make_canonical_data_if_error ~canonical_data ~ends ~errors ~all_ids ~prev
    in
    let** ( { id; display; callers; stack_direction; nest_kind; loc; cmd_kind },
            ends ) =
      Result_utils.of_option
        ~none:"Trying to finish partial with no canonical data!" canonical_data
    in
    let prev =
      let+ id, branch, _ = prev in
      (id, branch)
    in
    let++ next_kind =
      let is_unevaluated_funcall =
        match funcall_kind with
        | Some Unevaluated_funcall -> true
        | _ -> false
      in
      let++ cases = ends_to_cases ~is_unevaluated_funcall ~nest_kind ends in
      match cases with
      | [] -> Exec_map.Zero
      | [ (Case (Unknown, _), _) ] ->
          One (Option.get (List_utils.last all_ids), None)
      | _ -> Many cases
    in

    match cmd_kind with
    | Context _ ->
        FinishedContext
          {
            cmd_kind;
            branch_case = None;
            id;
            prev;
            stack_direction;
            all_ids;
            display;
            matches;
            errors;
            next_kind;
            callers;
            loc;
            (* has_return; *)
          }
    | _ ->
        FinishedCommand
          {
            prev;
            id;
            all_ids;
            display;
            matches;
            errors;
            next_kind;
            callers;
            stack_direction;
            loc;
            has_return;
          }

  let update = Update.f ~finish
  let init () = Hashtbl.create 0

  let create_partial ~prev =
    {
      prev;
      all_ids = Ext_list.make ();
      unexplored_paths = Stack.create ();
      ends = Ext_list.make ();
      matches = Ext_list.make ();
      errors = Ext_list.make ();
      canonical_data = None;
      funcall_kind = None;
      has_return = false;
    }

  let get_or_create_partial ~partials ~get_prev prev_id =
    let partial =
      let* prev_id = prev_id in
      Hashtbl.find_opt partials prev_id
    in
    match partial with
    | Some p -> Ok p
    | None ->
        let++ prev = get_prev () in
        create_partial ~prev

  let failwith ~exec_data ?partial ~partials msg =
    DL.failwith
      (fun () ->
        [
          ("exec_data", exec_data_to_yojson exec_data);
          ("partial_data", opt_to_yojson partial_data_to_yojson partial);
          ("partials_state", to_yojson partials);
        ])
      ("JSLifter.PartialCmds.handle: " ^ msg)

  let handle ~prog ~(partials : t) ~get_prev ~prev_id exec_data =
    let partial =
      get_or_create_partial ~partials ~get_prev prev_id
      |> Result_utils.or_else (fun e -> failwith ~exec_data ~partials e)
    in
    Hashtbl.replace partials exec_data.id partial;
    let result =
      update ~prog ~prev_id exec_data partial
      |> Result_utils.or_else (fun e ->
             failwith ~exec_data ~partial ~partials e)
    in
    (* DL.log (fun m ->
        m
          ~json:[ ("partial_result", result |> partial_result_to_yojson) ]
          "PartialCommands.handle: partial result gotten"); *)
    let () =
      match result with
      | FinishedCommand _ | FinishedContext _ ->
          partial.all_ids
          |> Ext_list.iter (fun (id, _) -> Hashtbl.remove_all partials id)
      | _ -> ()
    in
    result
end
