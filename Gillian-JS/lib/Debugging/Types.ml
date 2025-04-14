module Lifter = Gillian.Debugger.Lifter
module SMemory = Semantics.Symbolic
module PC = Js2jsil_lib.JS2GIL_ParserAndCompiler
module Branch_case = Gil_syntax.Branch_case
module Prog = Gil_syntax.Prog
module Report_id = Gillian.Logging.Report_id
module Exec_map = Gillian.Debugger.Utils.Exec_map
module Match_map = Gillian.Debugger.Utils.Match_map
module Hashtbl = Gillian.Utils.Prelude.Hashtbl

type memory = SMemory.t
type memory_error = SMemory.err_t
type tl_ast = PC.tl_ast
type init_data = PC.init_data
type annot = PC.Annot.t
type pc_err = PC.err
type id = Report_id.t [@@deriving yojson, show]
type branch_data = id * Branch_case.t option [@@deriving yojson]
type stack_direction = In | Out of id [@@deriving yojson]
type step_args = id option * Branch_case.t option * Branch_case.path

type cmd_data = {
  id : id;
  all_ids : id list;
  display : string;
  matches : Match_map.matching list;
  errors : string list;
  submap : id Exec_map.submap;
  prev : (id * Branch_case.t option) option;
  callers : id list;
  func_return_label : (string * int) option;
  loc : (string * int) option;
}
[@@deriving yojson]

type map = (id, Branch_case.t, cmd_data, branch_data) Exec_map.map
[@@deriving yojson]

module Make
    (Gil : Gillian.Debugger.Lifter.Gil_fallback_lifter.Gil_lifter_with_state)
    (V : Gillian.Abstraction.Verifier.S with type annot = PC.Annot.t) =
struct
  module Cmd_Report = V.SAInterpreter.Logging.ConfigReport
  module Fallback_Gil_lifter = Gil.Lifter

  type cmd_report = Cmd_Report.t [@@deriving yojson]
  type exec_data = cmd_report Lifter.executed_cmd_data [@@deriving yojson]
  type _ Effect.t += Step : step_args -> exec_data Effect.t

  type t = {
    proc_name : string;
    gil_state : Fallback_Gil_lifter.t; [@to_yojson Fallback_Gil_lifter.dump]
    tl_ast : tl_ast; [@to_yojson fun _ -> `Null]
    prog : (annot, int) Prog.t; [@to_yojson fun _ -> `Null]
    partial_cmds : PartialCmds.t;
    map : map;
    func_return_map : (id, string * int ref) Hashtbl.t;
    mutable func_return_count : int;
  }
  [@@deriving to_yojson]
end
