(* open Syntaxes.Option
   open Gillian.Utils
   open Types *)

let _step _state _id _case = failwith "undefined"
let step_branch _state _id _case = failwith "undefined"
let step_in _state _id = failwith "undefined"
let step_over _state _id = failwith "undefined"
let step_back _state _id = failwith "undefined"
let continue _state _id = failwith "undefined"
let step_out _state _id = failwith "undefined"
let continue_back _state _id = failwith "undefined"

(* let step_all ~start _state _id _case = failwith "undefined" *)
(* let is_breakpoint ~start ~current *)
(* let request_next state id case *)
(* let rec find_next state id case *)

(* let select_case nexts *)
(* let previous_step id { map; _ } *)

(* let get_next_from_end state { callers; func_return_label; _ } =
   let* caller_id = List_utils.hd_opt callers in
   let* label, ix = func_return_label in
   let case = *)
