open Gil_syntax
module Types = LifterTypes

module type S = sig
  type gil_state_t

  type t = {
    proc_name : string;
    gil_state : gil_state_t;
    tl_ast : Types.tl_ast;
    prog : (Types.annot, int) Prog.t;
    partial_cmds : PartialCommandTypes.t;
    map : Types.map;
    func_return_map : (Types.id, string * int ref) Hashtbl.t;
    mutable func_return_count : int;
  }
  [@@deriving to_yojson]
end

module Make
    (Gil : Gillian.Debugger.Lifter.Gil_fallback_lifter.Gil_lifter_with_state) =
struct
  type gil_state_t = Gil.Lifter.t

  type t = {
    proc_name : string;
    gil_state : gil_state_t; [@to_yojson Gil.Lifter.dump]
    tl_ast : Types.tl_ast; [@to_yojson fun _ -> `Null]
    prog : (Types.annot, int) Prog.t; [@to_yojson fun _ -> `Null]
    partial_cmds : PartialCommandTypes.t;
    map : Types.map;
    func_return_map : (Types.id, string * int ref) Hashtbl.t;
    mutable func_return_count : int;
  }
  [@@deriving to_yojson]
end
