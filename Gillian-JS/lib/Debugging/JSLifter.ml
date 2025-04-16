open Debugger.Utils
open Syntaxes.Option
module PC = Js2jsil_lib.JS2GIL_ParserAndCompiler

module Make
    (Gil : Gillian.Debugger.Lifter.Gil_fallback_lifter.Gil_lifter_with_state)
    (V : Gillian.Abstraction.Verifier.S with type annot = PC.Annot.t) =
struct
  module Types = struct
    include LifterTypes.Make (V)
    include LifterTypes
  end

  module State = State.Make (Gil)
  module Utils = LifterUtils.Make (Gil) (State)
  module Partial_cmds = PartialCmds.Make
  include Types
  include State
  include VariableHandling.Make (Utils)
  include StepFuncs.Make (Gil) (State) (Utils)
  open Exec_map

  let init ~proc_name ~all_procs:_ tl_ast prog =
    let gil_state = Gil.get_state () in
    let+ tl_ast = tl_ast in
    let partial_cmds = Partial_cmds.init () in
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
    let finish_init = failwith proc_name in
    (state, finish_init)

  let init_exn ~proc_name ~all_procs tl_ast prog =
    match init ~proc_name ~all_procs tl_ast prog with
    | None -> failwith "init: JSLifter needs a tl_ast!"
    | Some x -> x

  let dump = to_yojson
  let get_matches_at_id id { map; _ } = (get_exn map id).data.matches

  let memory_error_to_exception_info _info : exception_info =
    { id = "unknown"; description = Some "Error lifting not supported yet!" }

  let parse_and_compile_files ~entrypoint files =
    PC.parse_and_compile_files files |> Result.map (fun r -> (r, entrypoint))
end
