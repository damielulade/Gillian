open Gillian.Utils.Syntaxes.Option
open Gillian.Debugger.Utils.Exec_map

(* open Gil_syntax *)
open Javert_utils

module HelperFuncs
    (Gil : Gillian.Debugger.Lifter.Gil_fallback_lifter.Gil_lifter_with_state)
    (State : State.S with type gil_state_t = Gil.Lifter.t)
    (Utils : sig
      val package_case : Js_branch_case.t -> Packaged.branch_case * string
    end) =
struct
  (* open Gil *)
  open State
  open Utils

  let previous_step id { map; _ } =
    let+ id, case = (get_exn map id).data.prev in
    let case = case |> Option.map package_case in
    (id, case)

  (* let is_breakpoint ~start ~current = ()
     let get_next_from_end state { callers; func_return_label; _ } =
        let* caller_id = List_utils.hd_opt callers in
        let* label, ix = func_return_label in
        let case = ()
     let select_case nexts = ()

     let step_all ~start _state _id _case = failwith "undefined"
     let request_next state id case
     let rec find_next state id case
     let _step _state _id _case = failwith "undefined" *)
end

module Make
    (Gil : Gillian.Debugger.Lifter.Gil_fallback_lifter.Gil_lifter_with_state)
    (State : State.S with type gil_state_t = Gil.Lifter.t)
    (Utils : sig
      val package_case : Js_branch_case.t -> Packaged.branch_case * string
    end) =
struct
  include HelperFuncs (Gil) (State) (Utils)

  let step_branch _state _id _case = failwith "undefined"
  let step_in _state _id = failwith "undefined"
  let step_over _state _id = failwith "undefined"
  let step_back _state _id = failwith "undefined"
  let continue _state _id = failwith "undefined"
  let step_out _state _id = failwith "undefined"
  let continue_back _state _id = failwith "undefined"
  (* let start = get_exn state.map id in
     let rec aux node =
       let { id; callers; _ } = node.data in
       if is_breakpoint ~start ~current:node then (id, Debugger_utils.Breakpoint)
       else
         match previous_step id state *)
end
