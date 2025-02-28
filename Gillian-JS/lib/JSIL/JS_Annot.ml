type cmd_kind =
  | Normal of bool  (** Is this the final JSIL command for the JS statement? *)
  | Hidden
[@@deriving yojson, eq, show]

type t = {
  origin_loc : Gil_syntax.Location.t option;
  loop_info : string list;
  cmd_kind : cmd_kind; [@default Normal false]
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
