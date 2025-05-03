type cmd_kind =
  | Normal of bool  (** Is this the final JSIL command for the JS statement? *)
  | Hidden
  | Return
[@@deriving yojson, eq, show]

type nest_kind = FunCall of string [@@deriving yojson, eq]

type t = {
  origin_loc : Gillian.Utils.Location.t option;
  loop_info : string list;
  cmd_kind : cmd_kind; [@default Normal false]
  nest_kind : nest_kind option;
  branch_kind : Javert_utils.Js_branch_case.kind option;
  display : string option;
}
[@@deriving yojson, make, eq]

(** For basic commands *)
let make_basic ?origin_loc ?loop_info () =
  make ?origin_loc ?loop_info ~cmd_kind:Hidden ()

let get_origin_loc { origin_loc; _ } = origin_loc
let get_loop_info { loop_info; _ } = loop_info
let set_loop_info loop_info annot = { annot with loop_info }

let is_hidden { cmd_kind; _ } =
  match cmd_kind with
  | Hidden -> true
  | _ -> false
