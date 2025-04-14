(* open UtilFuncs *)
(* open Types *)
(* open Jsil_syntax.JS_Annot *)

type t = unit [@@deriving to_yojson]

(* Canonical command data
   type conanical_cmd_data = {
     id : id;
     display : string;
     nest_kind : nest_kind option;
   }
   [@@deriving to_yojson]

   type partial_result *)

(* open Gil_syntax
   open Debugger.Utils
   module DL = Debugger_log
   module Gil_branch_case = Gil_syntax.Branch_case

   module Branch_case = struct
     type t = unit [@@deriving yojson]
     type kind = unit [@@deriving yojson]
     type case = unit [@@deriving yojson]
   end

   open Types
   open Exec_map
   open Utils
   open Jsil_syntax.JS_Annot

   (** Logging report ID *)
   type id = Gillian.Logging.Report_id.t [@@deriving yojson, show]

   type canonical_cmd_data = {
     id : id;
     display : string;
     stack_info : id list * stack_direction option;
     is_loop_end : bool;
     loc : string * int;
   }
   [@@deriving yojson]

   type partial_data = {
     prev : (id * Branch_case.t option * id list) option;
     all_ids : (id * (Branch_case.kind option * Branch_case.case)) Ext_list.t;
     unexplored_paths : (id * Gil_branch_case.t option) Stack.t;
     ends : (Branch_case.case * branch_data) Ext_list.t;
     matches : Match_map.matching Ext_list.t;
     errors : string Ext_list.t;
     mutable canonical_data : canonical_cmd_data option;
     mutable nest_kind : nest_kind option;
     mutable has_return : bool;
   }
   [@@deriving yojson]

   let init () = Hashtbl.create 0

   let init_partial ~prev =
     {
       prev;
       all_ids = Ext_list.make ();
       unexplored_paths = Stack.create ();
       ends = Ext_list.make ();
       matches = Ext_list.make ();
       errors = Ext_list.make ();
       canonical_data = None;
       nest_kind = None;
       has_return = false;
     }

   type t = (id, partial_data) Hashtbl.t [@@deriving yojson]

   type finished = {
     prev : (id * Branch_case.t option) option;
     id : id;
     all_ids : id list;
     display : string;
     matches : Match_map.matching list;
     errors : string list;
     next_kind : (Branch_case.t, branch_data) next_kind;
     submap : id submap;
     callers : id list;
     stack_direction : stack_direction option;
     loc : string * int;
     has_return : bool;
   }
   [@@deriving to_yojson]

   type partial_result =
     | Finished of finished
     | StepAgain of (id * Gil_branch_case.t option)
   [@@deriving to_yojson]

   type partial_end = Branch_case.case * (id * Gil_branch_case.t option)
   [@@deriving to_yojson]

   (* let failwith = DL.failwith
   let handle = failwith "undefined" *)
*)
