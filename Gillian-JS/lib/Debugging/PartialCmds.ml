open Gillian.Utils.Prelude
open Gillian.Debugger.Utils
module Ext_list = Gillian.Utils.Ext_list
module Gil_branch_case = Gil_syntax.Branch_case
module Branch_case = Javert_utils.Js_branch_case
module PC = Js2jsil_lib.JS2GIL_ParserAndCompiler

module type Type_dependencies = sig
  type id [@@deriving yojson, show]
  type branch_data [@@deriving yojson]
  type stack_direction [@@deriving yojson]
end

module Types (Lifter_types : Type_dependencies) = struct
  open Lifter_types

  type partial_end = Branch_case.case * (id * Gil_branch_case.t option)
  [@@deriving to_yojson]

  type canonical_cmd_data = {
    id : id;
    display : string;
    callers : id list;
    stack_direction : stack_direction option;
    nest_kind : PC.Annot.nest_kind option;
    loc : (string * int) option;
  }
  [@@deriving to_yojson]

  type funcall_kind = Evaluated_funcall | Unevaluated_funcall
  [@@deriving to_yojson]

  type partial_data = {
    prev : (id * Branch_case.t option * id list) option;
        (* Where to put the finished CMD in the map. *)
    all_ids : (id * (Branch_case.kind option * Branch_case.case)) Ext_list.t;
        (* All the Gil CMD IDs that build into this one (and the relevant branch case info). *)
    unexplored_paths : (id * Gil_branch_case.t option) Stack.t;
        (* All paths that haven't been explored yet *)
    ends : partial_end Ext_list.t;
        (* All the end points; there may be multiple if the CMD branches. *)
    matches : Match_map.matching Ext_list.t;
        (* Unifications contained in this CMD. *)
    errors : string Ext_list.t; (* Errors occurring during this CMD. *)
    mutable conanical_data : canonical_cmd_data option;
    mutable funcall_kind : funcall_kind option;
    mutable has_return : bool;
  }
  [@@deriving to_yojson]

  type t = (id, partial_data) Hashtbl.t [@@deriving to_yojson]

  type finished = {
    prev : (id * Branch_case.t option) option;
    id : id;
    all_ids : id list;
    display : string;
    matches : Match_map.matching list;
    errors : string list;
    next_kind : (Branch_case.t, branch_data) Exec_map.next_kind;
    callers : id list;
    stack_direction : stack_direction option;
    loc : (string * int) option;
    has_return : bool;
  }
  [@@deriving yojson]

  type partial_result =
    | Finished of finished
    | StepAgain of (id * Gil_branch_case.t option)
  [@@deriving yojson]
end

module Update = struct
  let resolve_case = ()
  let get_stack_info = ()
  let update_return_cmd_info = ()
  let update_paths = ()
  let update_funcall_kind = ()
  let update_canonical_data = ()
  let get_is_end = ()
  let insert_id_and_case = ()
  let f = ()
end

module Make (Lifter_types : Type_dependencies) = struct
  include Types (Lifter_types)
  (* open Update *)
  (* open UtilFuncs *)

  let update = Update.f
  let ends_to_cases = ()
  let make_canonical_data_if_error = ()
  let finish = ()
  let init () = Hashtbl.create 0
  let init_partial = ()
  let find_or_init = ()
  let failwith = ()
  let handle = ()
end
