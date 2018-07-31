let refresh_file = function
  | "" -> ()
  | file -> let channel = open_out file in close_out channel


let print_to_file file str = match file with
  | "" -> print_endline str
  | _ ->
    let channel = open_out_gen [Open_append] 600 file in
    output_string channel str;
    output_char channel '\n';
    close_out channel (* TODO: Deal with exceptions and finalizers *)
