type cmd_kind =
  | Hidden
  | Normal of bool  (** Is this the final JSIL command for the JS statement? *)
  | Exception  (** Is this command raising an exception? *)
  | Return
  | Context of bool
      (** Is this command the last in a seq of evnironment changes? *)
  | Internal  (** Is this command from an internal/imported JSIL file? *)
  | Unknown
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

let make_internal ?origin_loc ?loop_info ?display () =
  make ?origin_loc ?loop_info ~cmd_kind:Internal ?display ()

let make_multi ?origin_loc ?loop_info ?display () =
  make ?origin_loc ?loop_info ~cmd_kind:Hidden ?display ()

let get_origin_loc { origin_loc; _ } = origin_loc
let get_loop_info { loop_info; _ } = loop_info
let set_loop_info loop_info annot = { annot with loop_info }
let get_display { display; _ } = display

let is_hidden { cmd_kind; _ } =
  match cmd_kind with
  | Hidden -> true
  | _ -> false
