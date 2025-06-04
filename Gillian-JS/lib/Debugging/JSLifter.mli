open Gillian
module PC = Js2jsil_lib.JS2GIL_ParserAndCompiler

module Make
    (Gil : Gillian.Debugger.Lifter.Gil_fallback_lifter.Gil_lifter_with_state)
    (V : Abstraction.Verifier.S with type annot = PC.Annot.t) :
  Debugger.Lifter.S
    with type memory = Semantics.Symbolic.t
     and type memory_error = Semantics.Symbolic.err_t
     and type tl_ast = PC.tl_ast
     and type cmd_report = V.SAInterpreter.Logging.ConfigReport.t
     and type annot = PC.Annot.t
     and type init_data = PC.init_data
     and type pc_err = PC.err
