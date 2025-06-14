{
  open Lexing

  module Make (Annot : Annot.S) = struct
    exception Syntax_error of string

    module GIL_Parser = GIL_Parser.Make (Annot)
    let keyword_table = Hashtbl.create 307

    let _ = List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
    [
        (* Type literals *)
        "Undefined", GIL_Parser.UNDEFTYPELIT;
        "Null",      GIL_Parser.NULLTYPELIT;
        "Empty",     GIL_Parser.EMPTYTYPELIT;
        "None",      GIL_Parser.NONETYPELIT;
        "Bool",      GIL_Parser.BOOLTYPELIT;
        "Int",       GIL_Parser.INTTYPELIT;
        "Num",       GIL_Parser.NUMTYPELIT;
        "Str",       GIL_Parser.STRTYPELIT;
        "Obj",       GIL_Parser.OBJTYPELIT;
        "List",      GIL_Parser.LISTTYPELIT;
        "Type",      GIL_Parser.TYPETYPELIT;
        "Set",       GIL_Parser.SETTYPELIT;

        (* Literals *)
        "undefined", GIL_Parser.UNDEFINED;
        "null",      GIL_Parser.NULL;
        "empty",     GIL_Parser.EMPTY;
        "true",      GIL_Parser.TRUE;
        "True",      GIL_Parser.TRUE;
        "false",     GIL_Parser.FALSE;
        "False",     GIL_Parser.FALSE;
        "nan",       GIL_Parser.NAN;
        "inf",       GIL_Parser.INFINITY;
        "nil",       GIL_Parser.LSTNIL;

        (* Binary operators *)
        "and",     GIL_Parser.AND;
        "or",      GIL_Parser.OR;
        "m_atan2", GIL_Parser.M_ATAN2;

        (* Unary operators *)
        "not",           GIL_Parser.NOT;
        "isNaN",         GIL_Parser.M_ISNAN;
        "m_abs",         GIL_Parser.M_ABS;
        "m_acos",        GIL_Parser.M_ACOS;
        "m_asin",        GIL_Parser.M_ASIN;
        "m_atan",        GIL_Parser.M_ATAN;
        "m_ceil",        GIL_Parser.M_CEIL;
        "m_cos",         GIL_Parser.M_COS;
        "m_exp",         GIL_Parser.M_EXP;
        "m_floor",       GIL_Parser.M_FLOOR;
        "m_log",         GIL_Parser.M_LOG;
        "m_round",       GIL_Parser.M_ROUND;
        "m_sgn",         GIL_Parser.M_SGN;
        "m_sin",         GIL_Parser.M_SIN;
        "m_sqrt",        GIL_Parser.M_SQRT;
        "m_tan",         GIL_Parser.M_TAN;
        "as_int",        GIL_Parser.NUMTOINT;
        "as_num",        GIL_Parser.INTTONUM;
        "num_to_string", GIL_Parser.TOSTRING;
        "num_to_int",    GIL_Parser.TOINT;
        "num_to_uint16", GIL_Parser.TOUINT16;
        "num_to_int32",  GIL_Parser.TOINT32;
        "num_to_uint32", GIL_Parser.TOUINT32;
        "string_to_num", GIL_Parser.TONUMBER;
        "car",           GIL_Parser.CAR;
        "cdr",           GIL_Parser.CDR;
        "set_to_list",   GIL_Parser.SETTOLIST;

        (* Expression keywords *)
        "typeOf", GIL_Parser.TYPEOF;
        "exists", GIL_Parser.EXISTS;


        (* Command keywords *)
        "skip",         GIL_Parser.SKIP;
        "args",         GIL_Parser.ARGUMENTS;
        "goto",         GIL_Parser.GOTO;
        "with",         GIL_Parser.WITH;
        "apply",        GIL_Parser.APPLY;
        "PHI",          GIL_Parser.PHI;
        "return",       GIL_Parser.RETURN;
        "throw",        GIL_Parser.THROW;
        "extern",       GIL_Parser.EXTERN;

        (* Logical expressions: most match with the program expressions *)
        "none", GIL_Parser.LNONE;

        (* Logic assertions *)
        "emp",          GIL_Parser.LEMP;
        "types",        GIL_Parser.LTYPES;
        "forall",       GIL_Parser.LFORALL;

        (* Logic predicates *)
        "abstract", GIL_Parser.ABSTRACT;
        "pure", GIL_Parser.PURE;
        "pred", GIL_Parser.PRED;
        "nounfold", GIL_Parser.NOUNFOLD;
        "facts", GIL_Parser.FACTS;
        "guard", GIL_Parser.PUGUARD;

        (* Logic commands *)
        "fold",         GIL_Parser.FOLD;
        "unfold",       GIL_Parser.UNFOLD;
        "unfold_all",   GIL_Parser.UNFOLDALL;
        "symb_exec",    GIL_Parser.SYMBEXEC;
        "if",           GIL_Parser.LIF;
        "then",         GIL_Parser.LTHEN;
        "else",         GIL_Parser.LELSE;
        "macro",        GIL_Parser.MACRO;
        "invariant",    GIL_Parser.INVARIANT;
        "consume",      GIL_Parser.CONSUME;
        "produce",      GIL_Parser.PRODUCE;
        "assert",       GIL_Parser.ASSERT;
        "assume",       GIL_Parser.ASSUME;
        "assume_type",  GIL_Parser.ASSUME_TYPE;
        "fresh_svar",   GIL_Parser.FRESH_SVAR;
        "bind",         GIL_Parser.BIND;
        "existentials", GIL_Parser.EXISTENTIALS;
        "sep_assert",   GIL_Parser.SEPASSERT;
        "branch",       GIL_Parser.BRANCH;
        "use_subst",    GIL_Parser.USESUBST;

        (* Procedure specification keywords *)
        "axiomatic",    GIL_Parser.AXIOMATIC;
        "incomplete",   GIL_Parser.INCOMPLETE;
        "lemma",        GIL_Parser.LEMMA;
        "variant",      GIL_Parser.VARIANT;
        "spec",         GIL_Parser.SPEC;
        "bispec",       GIL_Parser.BISPEC;
        "normal",       GIL_Parser.NORMAL;
        "error",        GIL_Parser.ERROR;
        "fail",         GIL_Parser.FAIL;
        "trusted",      GIL_Parser.TRUSTED;

        (* Procedure definition keywords *)
        "proc", GIL_Parser.PROC;

        (* Others *)
        "import", GIL_Parser.IMPORT;
        "verify", GIL_Parser.VERIFY;
      ]
}

let digit = ['0'-'9']
let letter = ['a'-'z''A'-'Z']
let identifier = letter(letter|digit|'_')*

let float = '-'? digit+ ('.' digit*)?
let int = '-'? digit+ 'i'

let var2 = "_pvar_" (letter|digit|'_')*
let lvar = '#' (letter|digit|'_'|'$')*
let lvar2 = "#lvar_" (letter|digit|'_')*
let normalised_lvar = "##NORMALISED_LVAR" (letter|digit|'_'|'$')*
let loc = "$l" (letter|digit|'_')*
let aloc = "_$l_" (letter|digit|'_')*
let normalised_aloc = "_$l_#" (letter|digit|'_')*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read = parse
  | white                { read lexbuf }
  | newline              { new_line lexbuf; read lexbuf }

  | "_"                  { GIL_Parser.UNDERSCORE    }

  (* Literals *)
  | "{{"                 { GIL_Parser.LSTOPEN   }
  | "}}"                 { GIL_Parser.LSTCLOSE  }

(* Constants *)
  | "$$min_float"        { GIL_Parser.MIN_FLOAT     }
  | "$$max_float"        { GIL_Parser.MAX_FLOAT     }
  | "$$random"           { GIL_Parser.RANDOM        }
  | "$$pi"               { GIL_Parser.PI            }
  | "$$UTCTime"          { GIL_Parser.UTCTIME       }
  | "$$LocalTime"        { GIL_Parser.LOCALTIME     }

(* Binary operators *)
  | "==>"                { GIL_Parser.LIMPLIES   }
  | "=="
  | "="                  { GIL_Parser.EQ     }
  | "-*"                 { GIL_Parser.WAND   }

  | "i<#"
  | "i<"                 { GIL_Parser.ILT    }
  | "i>"                 { GIL_Parser.IGT    }
  | "i<=#"
  | "i<="                { GIL_Parser.ILE    }
  | "i>="                { GIL_Parser.IGE    }
  | "i+"                 { GIL_Parser.IPLUS  }
  | "i-"                 { GIL_Parser.IMINUS }
  | "i*"                 { GIL_Parser.ITIMES }
  | "i/"                 { GIL_Parser.IDIV   }
  | "i%"                 { GIL_Parser.IMOD   }

  | "<#"
  | "<"                  { GIL_Parser.FLT    }
  | ">"                  { GIL_Parser.FGT    }
  | "<=#"
  | "<="                 { GIL_Parser.FLE    }
  | ">="                 { GIL_Parser.FGE    }
  | "+"                  { GIL_Parser.FPLUS  }
  | "-"                  { GIL_Parser.FMINUS }
  | "*"                  { GIL_Parser.FTIMES }
  | "/"                  { GIL_Parser.FDIV   }
  | "%"                  { GIL_Parser.FMOD   }

  | "s<#"
  | "s<"                 { GIL_Parser.SLT           }
  | "&"                  { GIL_Parser.BITWISEAND    }
  | "|"                  { GIL_Parser.BITWISEOR     }
  | "^"                  { GIL_Parser.BITWISEXOR    }
  | "<<"                 { GIL_Parser.LEFTSHIFT     }
  | ">>"                 { GIL_Parser.SIGNEDRIGHTSHIFT }
  | ">>>"                { GIL_Parser.UNSIGNEDRIGHTSHIFT }
  | "&l"                 { GIL_Parser.BITWISEANDL   }
  | "|l"                 { GIL_Parser.BITWISEORL    }
  | "^l"                 { GIL_Parser.BITWISEXORL   }
  | "<<l"                { GIL_Parser.LEFTSHIFTL    }
  | ">>l"                { GIL_Parser.SIGNEDRIGHTSHIFTL }
  | ">>>l"               { GIL_Parser.UNSIGNEDRIGHTSHIFTL }
  | "**"                 { GIL_Parser.M_POW         }
  | "l+"                 { GIL_Parser.LSTCAT        }
  | "++"                 { GIL_Parser.STRCAT        }
  | "-u-"                { GIL_Parser.SETUNION      }
  | "-i-"                { GIL_Parser.SETINTER      }
  | "-d-"                { GIL_Parser.SETDIFF       }
  | "--e--"
  | "-e-"                { GIL_Parser.SETMEM        }
  | "--s--"
  | "-s-"                { GIL_Parser.SETSUB        }
  | "-{"                 { GIL_Parser.SETOPEN       }
  | "}-"                 { GIL_Parser.SETCLOSE      }
(* Unary operators *)
  (* Unary minus uses the same symbol as binary minus, token MINUS *)
  | "~"                  { GIL_Parser.BITWISENOT    }
  | "l-len"              { GIL_Parser.LSTLEN }
  | "l-rev"              { GIL_Parser.LSTREV }
  | "l-sub"              { GIL_Parser.LSTSUB }
  | "s-len"              { GIL_Parser.STRLEN }
(* Expression keywords *)
  | "l-nth"              { GIL_Parser.LSTNTH }
  | "l-repeat"           { GIL_Parser.LSTREPEAT }
  | "s-nth"              { GIL_Parser.STRNTH }
(* Command keywords *)
  | ":="                 { GIL_Parser.DEFEQ }
(* Logic assertions *)
  | "[["                 { GIL_Parser.OASSERT }
  | "]]"                 { GIL_Parser.CASSERT }
  | "/\\"                { GIL_Parser.LAND }
  | "\\/"                { GIL_Parser.LOR }
  | "!"                  { GIL_Parser.LNOT }
  | "is-int"             { GIL_Parser.ISINT }
  (* Separating conjunction uses the same symbol as product, token TIMES *)
(* Logic commands *)
  | "[*"                 { GIL_Parser.OLCMD     }
  | "*]"                 { GIL_Parser.CLCMD     }
  | "package"            { GIL_Parser.PACKAGE   }
  | "unfold*"            { GIL_Parser.RECUNFOLD }
  (**
    macro, assert are elsewhere
  *)
(* Separators *)
  | "(*"                 { read_comment lexbuf   }
  | '.'                  { GIL_Parser.DOT       }
  | ','                  { GIL_Parser.COMMA     }
  | ':'                  { GIL_Parser.COLON     }
  | ';'                  { GIL_Parser.SCOLON    }
  | '('                  { GIL_Parser.LBRACE    }
  | ')'                  { GIL_Parser.RBRACE    }
  | '['                  { GIL_Parser.LBRACKET  }
  | ']'                  { GIL_Parser.RBRACKET  }
  | '{'                  { GIL_Parser.CLBRACKET }
  | '}'                  { GIL_Parser.CRBRACKET }
(* Literals (cont.) *)
  | int                  { let s = Lexing.lexeme lexbuf in
                           let s_n = String.sub s 0 ((String.length s) - 1) in
                           let n = Z.of_string s_n in
                           GIL_Parser.INTEGER n }
  | float                { let n = float_of_string (Lexing.lexeme lexbuf) in
                           GIL_Parser.FLOAT n }
  | '"'                  { read_string (Buffer.create 32) lexbuf }
  | loc                  { GIL_Parser.LOC (Lexing.lexeme lexbuf) }
  | aloc                 { GIL_Parser.ALOC (Lexing.lexeme lexbuf) }
  | normalised_aloc      { GIL_Parser.ALOC (Lexing.lexeme lexbuf) }
(* Directives *)
  | "@nopath"            { GIL_Parser.NO_PATH }
  | "@internal"          { GIL_Parser.INTERNAL }
  | "#internal"          { GIL_Parser.INTERNAL_FILE }
  | "#begin_init_data"   { read_init_data (Buffer.create 1024) lexbuf }
(* Variables *)
  | identifier           { let candidate = Lexing.lexeme lexbuf in
                           match (Hashtbl.mem keyword_table candidate) with
                           | true  -> Hashtbl.find keyword_table candidate
                           | false -> GIL_Parser.VAR (Lexing.lexeme lexbuf) }
  | var2                 { GIL_Parser.VAR (Lexing.lexeme lexbuf) }
(* Logic variables *)
  | lvar                 { GIL_Parser.LVAR (Lexing.lexeme lexbuf) }
  | lvar2                { GIL_Parser.LVAR (Lexing.lexeme lexbuf) }
  | normalised_lvar      { GIL_Parser.LVAR (Lexing.lexeme lexbuf) }
(* EOF *)
  | eof                  { GIL_Parser.EOF }
  | _                    { raise (Syntax_error ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and read_string buf =
  parse
  | '"'                  { GIL_Parser.STRING (Buffer.contents buf) }
  | '\\' '/'             { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\'            { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'             { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'             { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'             { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'             { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'             { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | '\\' '\"'            { Buffer.add_char buf '\"'; read_string buf lexbuf }
  | [^ '"' '\\']+        {
                           Buffer.add_string buf (Lexing.lexeme lexbuf);
                           read_string buf lexbuf
                         }
  | _                    { raise (Syntax_error ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof                  { raise (Syntax_error ("String is not terminated")) }

and read_comment =
  parse
  | newline              { new_line lexbuf; read_comment lexbuf }
  | "*)"                 { read lexbuf }
  | eof                  { raise (Syntax_error ("Comment is not terminated")) }
  | _                    { read_comment lexbuf }

and read_init_data buf =
  parse
  | newline { new_line lexbuf; Buffer.add_char buf '\n'; read_init_data buf lexbuf }
  | eof     { raise (Syntax_error ("Init_data is not terminated")) }
  | "#end_init_data" { GIL_Parser.INIT_DATA (Yojson.Safe.from_string (Buffer.contents buf)) }
  | _        { Buffer.add_string buf (Lexing.lexeme lexbuf); read_init_data buf lexbuf }

{
  end
}
