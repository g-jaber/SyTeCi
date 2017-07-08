open Symb_red
open Parser
open Skor
open Tcstruct
open Logic
open Templogic
(*open Wts*)

let get_filename () = 
   if (Array.length Sys.argv <= 2) then failwith ("Error: you should provide two files containing the programs you want to compare.");
   if (not (Sys.file_exists Sys.argv.(1))) then failwith ("Error: the file " ^ Sys.argv.(1) ^ " does not exist.");
   if (not (Sys.file_exists Sys.argv.(2))) then failwith ("Error: the file " ^ Sys.argv.(2) ^ " does not exist.");
   (Sys.argv.(1),Sys.argv.(2))

let get_term filename = 
 let inBuffer = open_in filename in
 let lineBuffer = Lexing.from_channel inBuffer in
 try let expr = Parser.prog Lexer.token lineBuffer in
     print_string (Syntax.string_of_exprML expr);
     print_newline (); 
     let ty = Type_checker.typing [] [] expr in
     print_string (Syntax.string_of_typeML ty);
     print_newline ();     
     (expr,ty)
 with
   | Lexer.SyntaxError msg -> failwith ("Parsing Error :" ^ msg)
   | Parser.Error -> failwith ("Syntax Error at position "^ (string_of_int (Lexing.lexeme_start lineBuffer)))
   | Type_checker.TypingError msg -> failwith ("Type error :" ^ msg)   
   
let () =
        let (filename1,filename2) = get_filename () in
        print_string ("First Program");
        print_newline ();
        let (expr1,ty1) = get_term filename1 in
        print_string ("Second Program");
        print_newline ();
        let (expr2,ty2) = get_term filename2 in
        let tc = build_tc_rule [] (emptyctx_sequent (RelE (ty1,[],expr1,expr2,[],[])) 0 0) in (*(int_of_string Sys.argv.(3))) in *)        
        let tc' = extract_temporal tc in
        let temp_form = iter 10 simplify_temp_formula (tformula_of_tc tc') in   
        print_endline ("Temporal Formula:");
        print_string (string_of_temp_formula temp_form);
        print_newline ();
(*        print_endline ("WTS:");
        let (sr,isVal,isSquare,isSquareP,preds) = Wts.build_esr tc' in   
        Wts.print_sr sr;*)
(*        print_endline ("Closed WTS:");        
        let (sr',_,_) = iter 3 esr_closure (sr,isSquare,isSquareP) in
        Wts.print_sr sr';   *)     
(*        print_endline ("Reachability:");
        let paths = backward_sr sr' in
        List.iter (fun (lists,x) -> ((List.iter (fun s -> print_string ((string_of_state s) ^" ")) lists); (print_endline (string_of_arith_pred x)))) paths*)
(*        let result = Symb_red.symbred_trans [] [] expr1 [] in
        List.iter (fun (expr', env, heap, _) -> print_string ("Expr: " ^ (Syntax.string_of_exprML expr')); print_newline ();
                                                print_string ("Env: " ^ (Symb_red.string_of_env env)); print_newline ();
                                                print_string ("Heap: " ^ (Logic.string_of_heap heap)); print_newline ())
                        result;*)
