open Gil_syntax
open Debugger.Utils
module PC = Js2jsil_lib.JS2GIL_ParserAndCompiler

(** Logging report ID *)
type id = Gillian.Logging.Report_id.t [@@deriving yojson, show]

(**)
type stack_direction = In | Out of id [@@deriving yojson]

(** Tuples of command identifier and ooptional branch case *)
type branch_data = id * Branch_case.t option [@@deriving yojson]

module Make
    (Gil : Gillian.Debugger.Lifter.Gil_fallback_lifter.Gil_lifter_with_state)
    (V : Gillian.Abstraction.Verifier.S with type annot = PC.Annot.t)
    (Partial_cmds : sig
      type t
    end) =
struct
  (* The map of the execution of a function *)
  open Exec_map
  module CmdReport = V.SAInterpreter.Logging.ConfigReport

  (** Fallback GIL lifter *)
  module Gil_lifter = Gil.Lifter

  type memory = Semantics.Symbolic.t
  type memory_error = Semantics.Symbolic.err_t
  type tl_ast = PC.tl_ast
  type annot = PC.Annot.t
  type cmd_report = CmdReport.t [@@deriving yojson]
  type init_data = PC.init_data
  type pc_err = PC.err

  (**  *)
  type cmd_data = {
    id : id;
    all_ids : id list;
    display : string;
    matches : Match_map.matching list;
    errorss : string list;
    submap : id submap;
    prev : (id * Branch_case.t option) option;
    callers : id list;
    func_return_label : (string * int) option;
    loc : (string * int) option;
  }
  [@@deriving yojson]

  (** Map *)
  type map = (id, Branch_case.t, cmd_data, branch_data) Exec_map.map
  [@@deriving yojson]

  (** Lifter type (or state), not complete. *)
  type t = {
    proc_name : string;
    gil_state : Gil_lifter.t; [@to_yojson Gil_lifter.dump]
    tl_ast : tl_ast; [@to_yojson fun _ -> `Null]
    partial_cmds : Partial_cmds.t; [@to_yojson fun _ -> `Null]
    map : map;
    mutable is_loop_func : bool;
    prog : (annot, int) Prog.t; [@to_yojson fun _ -> `Null]
    func_return_map : (id, string * int ref) Hashtbl.t;
    mutable func_return_count : int;
  }
  [@@deriving to_yojson]

  (** Execution data *)
  type exec_data = cmd_report Gillian.Debugger.Lifter.executed_cmd_data
  [@@deriving yojson]

  (** Step arguments: optional ID, optional branch case type, and path of branch types taken so far *)
  type step_args = id option * Branch_case.t option * Branch_case.path

  (**)
  type _ Effect.t += Step : step_args -> exec_data Effect.t
end
