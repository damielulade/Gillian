open LifterTypes
open Gillian.Utils
open Gillian.Debugger.Utils
open Gil_syntax
open Javert_utils

type partial_end = Js_branch_case.case * (id * Branch_case.t option)
[@@deriving to_yojson]

open Js2jsil_lib

type canonical_cmd_data = {
  id : id;
  display : string;
  callers : id list;
  stack_direction : stack_direction option;
  nest_kind : JS2GIL_ParserAndCompiler.Annot.nest_kind option;
  loc : (string * int) option;
}
[@@deriving to_yojson]

type funcall_kind = Evaluated_funcall | Unevaluated_funcall
[@@deriving to_yojson]

type partial_data = {
  prev : (id * Js_branch_case.t option * id list) option;
      (* Where to put the finished CMD in the map. *)
  all_ids :
    (id * (Js_branch_case.kind option * Js_branch_case.case)) Ext_list.t;
      (* All the Gil CMD IDs that build into this one (and the relevant branch case info). *)
  unexplored_paths : (id * Branch_case.t option) Stack.t;
      (* All paths that haven't been explored yet *)
  ends : partial_end Ext_list.t;
      (* All the end points; there may be multiple if the CMD branches. *)
  matches : Match_map.matching Ext_list.t;
      (* Unifications contained in this CMD. *)
  errors : string Ext_list.t; (* Errors occurring during this CMD. *)
  mutable canonical_data : canonical_cmd_data option;
  mutable funcall_kind : funcall_kind option;
  mutable has_return : bool;
}
[@@deriving to_yojson]

type t = (id, partial_data) Hashtbl.t [@@deriving to_yojson]

type finished = {
  prev : (id * Js_branch_case.t option) option;
  id : id;
  all_ids : id list;
  display : string;
  matches : Match_map.matching list;
  errors : string list;
  next_kind : (Js_branch_case.t, branch_data) Exec_map.next_kind;
  callers : id list;
  stack_direction : stack_direction option;
  loc : (string * int) option;
  has_return : bool;
}
[@@deriving yojson]

type partial_result =
  | Finished of finished
  | StepAgain of (id * Branch_case.t option)
[@@deriving yojson]
