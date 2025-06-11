open Gillian.Debugger.Utils.Exec_map
open Gillian.Utils.Syntaxes.Result_of_option
open Gillian.Debugger.Lifter
open Javert_utils
open LifterTypes
module PC = Js2jsil_lib.JS2GIL_ParserAndCompiler
module DL = Debugger_log

module Make
    (Gil : Gillian.Debugger.Lifter.Gil_fallback_lifter.Gil_lifter_with_state)
    (V : Gillian.Abstraction.Verifier.S with type annot = PC.Annot.t)
    (State : State.S with type gil_state_t = Gil.Lifter.t)
    (Types : sig
      type cmd_report = V.SAInterpreter.Logging.ConfigReport.t
      type exec_data = cmd_report executed_cmd_data [@@deriving yojson]
    end)
    (PartialCommands : sig
      val handle :
        prog:tl_ast ->
        partials:PartialCommandTypes.t ->
        get_prev:
          (init_data ->
          ((id * JS_branch_case.t option * id list) option, string) result) ->
        prev_id:id option ->
        Types.exec_data ->
        PartialCommandTypes.partial_result
    end)
    (NewCommandGroup : sig
      val f :
        state:State.t ->
        (PartialCommandTypes.finished, PartialCommandTypes.context) Either.t ->
        (id, JS_branch_case.t, cmd_data, branch_data) node
    end) =
struct
  open State
  open Types

  let get_prev ~state ~gil_case ~prev_id () =
    let { map; _ } = state in
    let=* prev_id = Ok prev_id in
    let=* prev =
      match get map prev_id with
      | None -> (
          match map.root with
          | None ->
              Ok None
              (* It's okay to not have a prev if we're still in the harness *)
          | _ -> Error "couldn't find map at prev_id!")
      | map -> Ok map
    in
    let { id; callers; _ } = prev.data in
    match prev.next with
    | None | Some (Single _) -> Ok (Some (id, None, callers))
    | Some (Branch nexts) -> (
        let case =
          List.find_map
            (fun (case, (_, (prev_id', gil_case'))) ->
              if prev_id' = prev_id && gil_case' = gil_case then Some case
              else None)
            nexts
        in
        match case with
        | Some case -> Ok (Some (id, Some case, callers))
        | None -> Error "couldn't find prev in branches!")

  let f ~state ?prev_id ?gil_case (exec_data : Types.exec_data) =
    let annot = exec_data.cmd_report.annot in
    let { partial_cmds = partials; tl_ast = prog; _ } = state in
    match annot.cmd_kind with
    | Unknown ->
        let json () =
          [
            ("state", to_yojson state);
            ("gil_case", opt_to_yojson Gil_syntax.Branch_case.to_yojson gil_case);
            ("exec_data", exec_data_to_yojson exec_data);
          ]
        in
        DL.failwith json "JSLifter: Encountered unknown cmd kind"
    | Context _ | Exception | Normal _ | Hidden | Return | Internal -> (
        let get_prev = get_prev ~state ~gil_case ~prev_id in
        let partial_result =
          PartialCommands.handle ~prog ~get_prev ~partials ~prev_id exec_data
        in
        match partial_result with
        | FinishedCommand finished ->
            let cmd = NewCommandGroup.f ~state (Either.Left finished) in
            Either.Right cmd
        | FinishedContext context ->
            let cmd = NewCommandGroup.f ~state (Either.Right context) in
            (* Either.Left (cmd.data.id, None) *)
            Either.Right cmd
        | StepAgain (id, case) -> Either.Left (id, case))
end
