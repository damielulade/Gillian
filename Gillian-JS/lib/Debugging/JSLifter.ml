open Debugger.Utils
module PC = Js2jsil_lib.JS2GIL_ParserAndCompiler

module Make
    (Gil : Gillian.Debugger.Lifter.Gil_fallback_lifter.Gil_lifter_with_state)
    (V : Gillian.Abstraction.Verifier.S with type annot = PC.Annot.t) =
struct
  module Partial_cmds = PartialCmd
  module Types = Types.Make (Gil) (V) (Partial_cmds)
  include Types
  include StepFuncs
  include VarFuncs
  include UtilFuncs
  open Exec_map

  let init ~proc_name ~all_procs:_ _tl_ast _prog = failwith proc_name

  let init_exn ~proc_name ~all_procs tl_ast prog =
    match init ~proc_name ~all_procs tl_ast prog with
    | None -> failwith "init: JSLifter needs a tl_ast!"
    | Some x -> x

  let get_matches_at_id id { map; _ } = (get_exn map id).data.matches

  let memory_error_to_exception_info _info : exception_info =
    { id = "unknown"; description = Some "Error lifting not supported yet!" }

  let parse_and_compile_files ~entrypoint files =
    PC.parse_and_compile_files files |> Result.map (fun r -> (r, entrypoint))
end
