open! Rizzoc
open Rizzoc.RefCount
open! Examples


let () = 
  let program: Ast.program = [
    TLet("id", EFun(["x"], EVar "x"));
    TLet("garbage_sig", EFun(["x"], 
      ELet("my_sig", EBinary(SigCons,
        ECase (EVar "x", [EVar "x"; ELet ("y", EApp (EVar "id", [EVar "x"]), EVar "y")]), 
        EApp (EVar("id"), [EVar "x"])),
      EApp (EVar "id", [EVar "my_sig"])
      )
    ));
    TLet("_", EFun(["x"], 
      EApp (EVar "garbage_sig", [EVar "x"])
    ))
  ] in
  (* how can entry borrow x? *)
  let beta, program_rc = Transformations.auto_ref_count program in
  Examples.print_parameter_ownerships beta;
  program_rc 
  |> List.iter (fun (c, Fun (_, b)) ->
    Format.printf "!!%s:\n%a\n!!\n" c pp_fnbody b
  )