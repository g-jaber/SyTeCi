let get_arguments () = 
  let size_args = Array.length Sys.argv in
   if (size_args <= 2) then failwith ("Error: you should provide two files containing the programs you want to compare.");
   if (not (Sys.file_exists Sys.argv.(1))) then failwith ("Error: the file " ^ Sys.argv.(1) ^ " does not exist.");
   if (not (Sys.file_exists Sys.argv.(2))) then failwith ("Error: the file " ^ Sys.argv.(2) ^ " does not exist.");
   let step1 = if (size_args <= 3) then 0 else (int_of_string Sys.argv.(3)) in
   let step2 = if (size_args <= 4) then 0 else (int_of_string Sys.argv.(4)) in   
   (Sys.argv.(1),Sys.argv.(2),step1,step2)


let get_term filename = 
 let inBuffer = open_in filename in
 let lineBuffer = Lexing.from_channel inBuffer in
 try let expr = Parser.prog Lexer.token lineBuffer in
(*     print_string (Syntax.string_of_exprML expr);
     print_newline (); *)
     let ty = Type_checker.typing [] [] expr in
(*     print_string (Syntax.string_of_typeML ty);
     print_newline ();  *)   
     (expr,ty)
 with
   | Lexer.SyntaxError msg -> failwith ("Parsing Error :" ^ msg)
   | Parser.Error -> failwith ("Syntax Error at position "^ (string_of_int (Lexing.lexeme_start lineBuffer)))
   | Type_checker.TypingError msg -> failwith ("Type error :" ^ msg)   
   
let () =
        let (filename1,filename2,step1,step2) = get_arguments () in
        let (expr1,ty1) = get_term filename1 in
        let (expr2,_) = get_term filename2 in       
        let tc = Tcstruct.build_tc ty1 expr1 expr2 step1 step2 in 
        let temp_form = Logic.iter 10 Templogic.simplify_temp_formula (Templogic.tformula_of_tc tc) in   
        print_endline ("Temporal Formula:");
        print_endline (Templogic.string_of_temp_formula temp_form);
        print_endline ("WTS:");
        let sr = Wts.build_sr tc in     
        let sr' = Wts_closure.sr_closure sr in
        Wts_to_dot.dot_from_sr sr';
