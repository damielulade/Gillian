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
    (Gil : Gillian.Debugger.Lifter.Gil_fallback_lifter.Gil_lifter_with_state) :
  S with type gil_state_t = Gil.Lifter.t
