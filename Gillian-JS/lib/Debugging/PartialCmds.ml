(* open PartialTypes *)

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

module Make = struct
  (* open Update *)
  (* open UtilFuncs *)
  include PartialTypes

  let update = Update.f
  let ends_to_cases = ()
  let make_canonical_data_if_error = ()
  let finish = ()
  let init () = Hashtbl.create 0
  let init_partial = ()
  let find_or_init = ()
  (* let failwith = () *)

  let handle ~prog ~(partials : t) ~get_prev ~prev_id exec_data =
    let _a = prog in
    let _a = partials in
    let _a = get_prev in
    let _a = prev_id in
    let _a = exec_data in
    failwith "undefined"
end
