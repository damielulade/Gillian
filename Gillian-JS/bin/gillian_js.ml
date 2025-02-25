module Gillian_JS_lifter =
  Gillian.Debugger.Lifter.Gil_fallback_lifter.Make
    (Semantics.Symbolic)
    (Js2jsil_lib.JS2GIL_ParserAndCompiler)
    (Debugging.JSLifter.Make)

module CLI =
  Gillian.Command_line.Make
    (Gillian.General.Init_data.Dummy)
    (Semantics.Concrete)
    (Semantics.Symbolic)
    (Js2jsil_lib.JS2GIL_ParserAndCompiler)
    (Semantics.External)
    (struct
      let runners : Gillian.Bulk.Runner.t list =
        [ (module Test262.Test262_runner); (module CosetteRunner) ]
    end)
    (Gillian_JS_lifter)

let () = CLI.main ()
