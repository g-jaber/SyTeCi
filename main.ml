open Arg


let get_term filename = 
 let inBuffer = open_in filename in
 let lineBuffer = Lexing.from_channel inBuffer in
 try let expr = Parser.prog Lexer.token lineBuffer in
     Debug.print_debug (Syntax.string_of_exprML expr);
     let (ty,_) = Type_checker.typing [] [] expr in
     Debug.print_debug (Syntax.string_of_typeML ty);
     (expr,ty)
 with
   | Lexer.SyntaxError msg -> failwith ("Parsing Error :" ^ msg)
   | Parser.Error -> failwith ("Syntax Error at position "^ (string_of_int (Lexing.lexeme_start lineBuffer)))
   | Type_checker.TypingError msg -> failwith ("Type error :" ^ msg)   
   
let () =
        let number_filename = ref 0 in
        let filename1 = ref "" in
        let filename2 = ref "" in
        let si_j = ref 0 in
        let si_k = ref 0 in
        let bounded_checking = ref false in
        let asym_unfold = ref false in
        let compute_char_form = ref false in
        let compute_sts = ref true in
        let print_dg = ref false in
        let speclist = 
          [("-cf",Set compute_char_form,"Compute the temporal characteristic formula");
           ("-dg",Set print_dg,"Print the computed derivation graph");
           ("-debug",Set Debug.debug_mode,"Debug mode"); 
           ("-j",Set_int si_j,"Fixe the left step-index to n in order to control unfolding of recursive calls");
           ("-k",Set_int si_k,"Fixe the right step-index to n in order to control unfolding of recursive calls");
           ("-bc",Set bounded_checking, "Enable bounded checking");
           ("-au",Set asym_unfold, "Enable asymmetric unfolding of recursive calls");           
           ("-nosts",Clear compute_sts, "Do not compute the STS")
          ] in
        let usage_msg = "Usage: ./syteci filename1 filename2 [options] where the options are:" in          
        let get_filename str =
          match !number_filename with
            | 0 -> filename1 := str; number_filename := !number_filename+1;
            | 1 -> filename2 := str; number_filename := !number_filename+1;
            | _ -> failwith ("Error: too many filenames have been provided. \n"^ usage_msg);
        in
        let check_number_filenames () =
          if !number_filename < 2 then failwith ("Error: two filenames containing the programs to be compared should have been provided. \n"^ usage_msg);
        in
        parse speclist get_filename usage_msg;
        check_number_filenames ();
        let (expr1,ty1) = get_term !filename1 in
        let (expr2,_) = get_term !filename2 in
        let tc = Tcstruct.build_tc !asym_unfold ty1 expr1 expr2 !si_j !si_k  in
        if !print_dg then begin 
           print_endline ("Inference Graph:");
           print_endline (Tcstruct.string_of_tc tc)
        end;   
        if !compute_char_form then begin
          let temp_form = Logic.iter 10 Templogic.simplify_temp_formula (Templogic.tformula_of_tc !bounded_checking tc) in   
          print_endline ("Temporal Formula:");
          print_endline (Templogic.string_of_temp_formula temp_form)
        end;
        if !compute_sts then begin
          let sr = Wts.build_sr !bounded_checking tc in     
          let sr' = Wts_closure.sr_closure sr in
          Wts_to_dot.dot_from_sr sr'
        end  
