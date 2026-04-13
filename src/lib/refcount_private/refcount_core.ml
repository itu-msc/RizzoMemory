open! Ast
module StringSet = Collections.StringSet
module StringMap = Collections.StringMap

type primitive =
  | Const of Ast.const
  | Var of string

type rexpr =
  | RConst of Ast.const
  | RCall of string * primitive list
  | RPartialApp of string * primitive list
  | RVarApp of string * primitive
  | RCtor of ctor
  | RProj of int * string
  | RReset of string
  | RReuse of string * ctor

and ctor =
  | Ctor of { tag: int; fields: primitive list }
  | Signal of { head: primitive; tail: primitive }

type fn_body =
  | FnRet of primitive
  | FnLet of string * rexpr * fn_body
  | FnCase of string * case_arm list
  | FnInc of string * fn_body
  | FnDec of string * fn_body

and case_arm = {
  tag: int option;
  num_fields: int option;
  body: fn_body;
}

type rc_fun = Fun of string list * fn_body

type program =
  | RefProg of { functions: (string * rc_fun) list; globals: (string * fn_body) list }

let get_cases = List.map (fun { body; _ } -> body)

type ownership =
  | Owned
  | Borrowed

let is_owned = function
  | Owned -> true
  | Borrowed -> false

type beta_env = ownership StringMap.t

let lookup env x =
  match StringMap.find_opt x env with
  | None -> Owned
  | Some b -> b

type parameter_ownership = ownership list StringMap.t

let lookup_params (b : parameter_ownership) (c : string) : ownership list =
  match StringMap.find_opt c b with
  | Some xs -> xs
  | None -> failwith (Printf.sprintf "unknown function in beta: '%s'" c)