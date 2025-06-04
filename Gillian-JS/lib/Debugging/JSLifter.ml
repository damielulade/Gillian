open Debugger.Utils
open Syntaxes.Option
module PC = Js2jsil_lib.JS2GIL_ParserAndCompiler
module DL = Debugger_log

module Make
    (Gil : Gillian.Debugger.Lifter.Gil_fallback_lifter.Gil_lifter_with_state)
    (V : Gillian.Abstraction.Verifier.S with type annot = PC.Annot.t) =
struct
  module Types = struct
    include LifterTypes.Make (V)
    include LifterTypes
  end

  module State = State.Make (Gil)
  module LifterUtils = LifterUtils.Make (Gil) (State)
  module Insert_new_cmd = InsertNewCmd.Make (Gil) (State) (LifterUtils)
  module PartialCommands = PartialCommands.Make (V) (Types)

  module Init_or_handle =
    InitOrHandle.Make (Gil) (V) (State) (Types) (PartialCommands)
      (Insert_new_cmd)

  include
    StepFuncs.Make (Gil) (V) (State) (LifterUtils) (Types) (Init_or_handle)

  include Types
  include State
  include VariableHandling.Make (Gil)

  let init ~proc_name ~all_procs:_ tl_ast prog =
    let gil_state = Gil.get_state () in
    let+ tl_ast = tl_ast in
    let partial_cmds = PartialCommands.init () in
    let state =
      {
        proc_name;
        gil_state;
        tl_ast;
        prog;
        partial_cmds;
        map = Exec_map.make ();
        func_return_map = Hashtbl.create 0;
        func_return_count = 0;
      }
    in
    let finish_init () =
      let rec aux id_case =
        let id, case, path =
          match id_case with
          | Some (id, case) ->
              let path = LifterUtils.path_of_id id state in
              (Some id, case, path)
          | None -> (None, None, [])
        in
        let exec_data = Effect.perform (Step (id, case, path)) in
        match Init_or_handle.f ~state ?prev_id:id ?gil_case:case exec_data with
        | Either.Left (id, case) -> aux (Some (id, case))
        | Either.Right map -> map.data.id
      in
      let id = aux None in
      (id, Debugger_utils.Step)
    in
    (state, finish_init)

  let init_exn ~proc_name ~all_procs tl_ast prog =
    match init ~proc_name ~all_procs tl_ast prog with
    | None -> failwith "init: JSLifter needs a tl_ast!"
    | Some x -> x

  let dump = to_yojson
  let pp_expr _ = Gil_syntax.Expr.pp
  let pp_asrt _ = Gil_syntax.Asrt.pp_atom
  let get_matches_at_id id { map; _ } = (Exec_map.get_exn map id).data.matches

  let memory_error_to_exception_info _info : exception_info =
    { id = "unknown"; description = Some "Error lifting not supported yet!" }

  let parse_and_compile_files ~entrypoint files =
    let entrypoint, _override =
      if Utils.(!Config.current_exec_mode = Exec_mode.Symbolic) then
        (Js2jsil_lib.JS2JSIL_Helpers.main_fid, true)
      else (entrypoint, false)
    in
    PC.parse_and_compile_files files |> Result.map (fun r -> (r, entrypoint))

  let _handle_cmd prev_id gil_case exec_data state =
    Init_or_handle.f ~state ~prev_id ?gil_case exec_data
end
