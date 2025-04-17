module Make
    (Gil : Gillian.Debugger.Lifter.Gil_fallback_lifter.Gil_lifter_with_state)
    (State : State.S with type gil_state_t = Gil.Lifter.t)
    (Types : sig
      type exec_data
    end) =
struct
  (* open State *)
  open Types

  let get_prev = ()

  let f ~state ?prev_id ?gil_case (exec_data : exec_data) =
    let _state = state in
    let _prev_id = prev_id in
    let _gil_case = gil_case in
    let _exec = exec_data in
    failwith "proc_name"
end
