open Semantics
open Js2jsil_lib
open Gillian.Logging
open Gil_syntax
open Gillian.Debugger.Utils
open Javert_utils
open Gillian.Debugger.Lifter

type memory = Symbolic.t
type memory_error = Symbolic.err_t
type tl_ast = JS2GIL_ParserAndCompiler.tl_ast
type init_data = JS2GIL_ParserAndCompiler.init_data
type annot = JS2GIL_ParserAndCompiler.Annot.t
type pc_err = JS2GIL_ParserAndCompiler.err
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
  prev : (id * Js_branch_case.t option) option;
  callers : id list;
  func_return_label : (string * int) option;
  loc : (string * int) option;
}
[@@deriving yojson]

type map = (id, Js_branch_case.t, cmd_data, branch_data) Exec_map.map
[@@deriving yojson]

module Make (CmdReport : sig
  type t [@@deriving yojson]
end) =
struct
  type cmd_report = CmdReport.t [@@deriving yojson]
  type exec_data = cmd_report executed_cmd_data [@@deriving yojson]
  type _ Effect.t += Step : step_args -> exec_data Effect.t
end
