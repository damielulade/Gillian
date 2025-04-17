open Javert_utils
open Gillian.Debugger.Utils.Exec_map
open LifterTypes
(* open InsertNewCmd *)

let ( let++ ) f o = Result.map o f
let ( let** ) o f = Result.bind o f
let to_str pp = Fmt.to_to_string (Fmt.hbox pp)

let rec int_to_letters ?(acc = "") = function
  | 0 -> acc
  | i ->
      let i = i - 1 in
      let remainder = i mod 26 in
      let char = Char.chr (65 + remainder) |> Char.escaped in
      let acc = acc ^ char in
      int_to_letters ~acc (i / 26)

module Make
    (Gil : Gillian.Debugger.Lifter.Gil_fallback_lifter.Gil_lifter_with_state)
    (State : State.S with type gil_state_t = Gil.Lifter.t) =
struct
  open Gil
  open State

  let path_of_id id { gil_state; _ } = Lifter.path_of_id id gil_state

  let package_case case : Packaged.branch_case * string =
    let json = Js_branch_case.to_yojson case in
    let display = Js_branch_case.display case in
    (json, display)

  let package_data { id; all_ids; display; matches; errors; submap; _ } =
    Packaged.{ id; all_ids; display; matches; errors; submap }

  let package_node { data : cmd_data; next } =
    let data = package_data data in
    let next =
      match next with
      | None -> None
      | Some (Single (next, _)) -> Some (Single (next, ""))
      | Some (Branch nexts) ->
          let nexts =
            nexts
            |> List.map (fun (case, (next, _)) ->
                   let case, bdata = package_case case in
                   (case, (next, bdata)))
          in
          Some (Branch nexts)
    in
    { data; next }
end
