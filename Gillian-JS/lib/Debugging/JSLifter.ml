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

  let init ~proc_name ~all_procs:_ _tl_ast _prog = failwith proc_name

  let init_exn ~proc_name ~all_procs tl_ast prog =
    match init ~proc_name ~all_procs tl_ast prog with
    | None -> failwith "init: JSLifter needs a tl_ast!"
    | Some x -> x

  let get_matches_at_id = failwith "undefined"
  let memory_error_to_exception_info = failwith "undefined"
  let parse_and_compile_files = failwith "undefined"
end
