open Arg

let get_term nbprog poly_tvar int_tvar filename =
  let inBuffer = open_in filename in
  let lineBuffer = Lexing.from_channel inBuffer in
  try let expr = Parser.prog Lexer.token lineBuffer in
    Debug.print_debug (nbprog ^ " program: " ^ Syntax.string_of_exprML expr);
    let ty = Type_checker.typing_full poly_tvar int_tvar expr in
    Debug.print_debug ("His type is: " ^ Syntax.string_of_typeML ty);
    (expr,ty)
  with
  | Lexer.SyntaxError msg -> failwith ("Parsing Error in the " ^ nbprog
                                       ^ " program:" ^ msg)
  | Parser.Error ->
    failwith ("Syntax Error in the " ^ nbprog ^ " program:"
              ^ " at position "
              ^ (string_of_int (Lexing.lexeme_start lineBuffer)))
  | Type_checker.TypingError msg -> failwith ("Type error :" ^ msg)

let () =
  let number_filename = ref 0 in
  let filename1 = ref "" in
  let filename2 = ref "" in
  let si_j = ref 0 in
  let si_k = ref 0 in
  let bounded_checking = ref false in
  let asym_unfold = ref false in
  let ext_reason = ref true in
  let print_cf = ref false in
  let print_sts = ref false in
  let file_sts = ref "" in
  let print_dg = ref false in
  let print_chc = ref false in
  let check_chc = ref false in
  let file_chc = ref "" in
  let int_tvar = ref false in
  let poly_tvar = ref false in
  let speclist =
    [("-cf",Set print_cf,"Print the temporal characteristic formula");
     ("-dg",Set print_dg,"Print the derivation graph");
     ("-debug",Set Debug.debug_mode,"Debug mode");
     ("-j",Set_int si_j,"Fix the left step-index to n in order to control unfolding of recursive calls");
     ("-k",Set_int si_k,"Fix the right step-index to n in order to control unfolding of recursive calls");
     ("-bc",Set bounded_checking, "Enable bounded checking");
     ("-au",Set asym_unfold, "Enable asymmetric unfolding of recursive calls");
     ("-smtm",Set print_sts, "Print the Structured-Memory Transition Machine");
     ("-file-smtm",Set_string file_sts, "Specify the file where to print the SMTM");
     ("-chc", Set print_chc,"Print the translation of the reachability of failed states as a Constrained Horn Clause");
     ("-check-chc", Set check_chc,"Check the generated Constrained Horn Clause with z3 (Experimental)");
     ("-file-chc",Set_string file_chc, "Specify the file where to print the Constrained Horn Clause");
     ("-int-type", Set int_tvar,"Substitute unconstrained type variables by Int rather than Unit");
     ("-poly", Set poly_tvar,"Allow polymorphic reasoning (Experimental)");
     ("-no-ext-reason", Clear ext_reason, "Forbid the initial extensional reasoning on values");
    ] in
  let usage_msg = "Usage: syteci filename1 filename2 [options]" in
  let get_filename str =
    match !number_filename with
    | 0 -> filename1 := str; number_filename := !number_filename+1;
    | 1 -> filename2 := str; number_filename := !number_filename+1;
    | _ -> Error.fail_error ("Error: too many filenames have been provided. \n"^ usage_msg);
  in
  let check_number_filenames () =
    if !number_filename < 2 then
      Error.fail_error ("Error: two filenames containing the programs to be compared "
        ^ "should have been provided. "^ usage_msg);
  in
  parse speclist get_filename usage_msg;
  check_number_filenames ();
  let (expr1,ty1) = get_term "first" !poly_tvar !int_tvar !filename1 in
  let (expr2,ty2) = get_term "second" !poly_tvar !int_tvar !filename2 in
  if ty1 <> ty2 then 
    Error.fail_error ("Error: the first program is of type "
      ^ (Syntax.string_of_typeML ty1)
      ^ " while the second program is of type "
      ^ (Syntax.string_of_typeML ty2) ^ ".");
  let tc = Tcstruct.build_tc !ext_reason !asym_unfold ty1 expr1 expr2 !si_j !si_k  in
  if !print_dg then begin
    print_endline ("Inference Graph:");
    print_endline (Tcstruct.string_of_tc tc)
  end;
  if !print_cf then begin
    let temp_form =
      Logic.iter 10 Templogic.simplify_temp_formula
        (Templogic.tformula_of_tc !bounded_checking tc) in
    print_endline ("Temporal Formula:");
    print_endline (Templogic.string_of_temp_formula temp_form)
  end;
  Debug.print_debug "Computing the SMTM";
  let sr = Wts.build_sr !bounded_checking tc in
  Debug.print_debug "Computing the closure of the SMTM";
  let sr' = Wts_closure.sr_closure sr in
  if !print_sts || (!file_sts <>"") then begin
    Printer.refresh_file !file_sts;
    Wts_to_dot.dot_from_sr !file_sts sr';
  end;
  Debug.print_debug "Computing the Constrained Horn Clause";
  if !print_chc || (!file_chc <> "") || !check_chc then begin
    let full_chc = Chc.visit_sr_full sr' in
    if !print_chc || (!file_chc <> "") then begin
      Printer.refresh_file !file_chc;
      Chc.print_full_chc !file_chc full_chc;
    end;
    if !check_chc then begin
      let str = Logic_to_z3.get_chc_z3_str full_chc in
      print_endline (Logic_to_z3.check_sat_chc_str str)
    end
  end