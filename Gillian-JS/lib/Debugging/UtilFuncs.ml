let ( let++ ) f o = Result.map o f
let ( let** ) o f = Result.bind o f
let dump = to_yojson
let to_str pp = Fmt.to_to_string (Fmt.hbox pp)
