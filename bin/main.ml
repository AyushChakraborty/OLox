let run source = 
	let tokens = Olox.Scanner.scan_tokens source in

	List.iter (fun token -> print_endline (Olox.Token.to_string token)) tokens;

	!Olox.Error.had_error

let runFile path = 
	let ch = open_in_bin path in
	let len = in_channel_length ch in
	let source = really_input_string ch len in
	close_in ch;

	let had_error = run source in
	if had_error then exit 65


let rec run_prompt () = 
	print_string "> ";
	flush stdout;
	try
		let line = read_line () in
		let _had_error = run line in   (*ignore the had_error returned since line wise errors can be ignored in REPL form. Ignoring is same as setting the state to true in oops paradigm*)
		Olox.Error.had_error := false;
		run_prompt ()
	with End_of_file -> 
		print_endline "end of input.."


(*entry point*)
let () = 
	let len = Array.length Sys.argv in
	if len > 2 then (
		print_endline "Usage: olox [script]";
		exit 64
	) else if len = 2 then
		runFile Sys.argv.(1)
	else
		run_prompt ()
