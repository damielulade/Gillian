type kind = IfElseKind | ForLoopKind | WhileLoopKind [@@deriving yojson, eq]

type case =
  | IfElse of bool
  | ForLoop of bool
  | WhileLoop of bool
  | FuncExit of string
  | Unknown
[@@deriving yojson, show]

let bool_kind_to_case kind b =
  match kind with
  | IfElseKind -> IfElse b
  | ForLoopKind -> ForLoop b
  | WhileLoopKind -> WhileLoop b

type t = Case of case * int | FuncExitPlaceholder [@@deriving yojson]

let pp fmt = function
  | Case (Unknown, i) -> Fmt.pf fmt "%d" i
  | Case ((IfElse b | WhileLoop b | ForLoop b), -1) -> Fmt.pf fmt "%B" b
  | Case ((IfElse b | WhileLoop b | ForLoop b), i) -> Fmt.pf fmt "%B - %d" b i
  | Case (FuncExit label, i) -> Fmt.pf fmt "%s-%d" label i
  | FuncExitPlaceholder -> Fmt.pf fmt "<step in>"

let display = Fmt.str "%a" pp
