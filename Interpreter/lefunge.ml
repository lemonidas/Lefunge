let is_digit x = x>='0' && x <= '9';;

type dir_t = Up | Down | Left | Right;;

let is_digit x = (x>= Char.code '0' && x<= Char.code '9');;

let max a b = if (a>b) then a else b;;

let main = 
	let in_channel = open_in Sys.argv.(1) in
	let rec loop max_len acc i= 
		try
			let line = input_line in_channel in
			let len = String.length line in
			loop (max max_len len) (line::acc) (i+1)
		with 
			End_of_file -> (List.rev acc, max_len, i)
	in let (code_list, width, height) = loop 0 [] 0 in
	let code = Array.make_matrix height width 32 in
	let rec loop_list i j str_list =
		match str_list with
		|[] -> ()
		|(h::t) ->  
			let len = String.length h in
			insert_to_array i j h t len 
	and insert_to_array i j str tail len =
		for j = 0 to len-1 do
			code.(i).(j) <- (Char.code str.[j])
		done;
		loop_list (i+1) j tail 
	in loop_list 0 0 code_list;
	(*Array.iter (Array.iter (Printf.printf "%d\n")) code;*)
	let stack = Stack.create () in
	let pop_2_and_apply op =
		let a = Stack.pop stack in
		let b = Stack.pop stack in
		op b a
	in
	let next_dir (i,j) direction=
			match direction with
			|Up -> (i-1, j)
			|Down -> (i+1, j)
			|Left -> (i, j-1)
			|Right -> (i, j+1)
	in
	let push x = Stack.push x stack in
	let pop () = Stack.pop stack in
	let rec simulate (i,j) dir=
	 	(*Printf.printf "Simulating..(%d, %d)\n" i j;
		Printf.printf "Code: %c\n" code.(i).[j];*)
		match code.(i).(j) with 
		|x when is_digit x ->
			push (x-(Char.code '0'));
			simulate (next_dir (i,j) dir) dir
		|43 -> (* + *)
			push (pop_2_and_apply (+));
			simulate (next_dir (i,j) dir) dir
		|45 -> (* - *)
			push (pop_2_and_apply (-));
			simulate (next_dir (i,j) dir) dir
		|42-> (* * *)
			push (pop_2_and_apply ( * ));
			simulate (next_dir (i,j) dir) dir
		|47 -> (* / *)
			push (pop_2_and_apply ( /));
			simulate (next_dir (i,j) dir) dir
		|37 -> (* % *)
			push (pop_2_and_apply ( mod ));
			simulate (next_dir (i,j) dir) dir
		|97 -> (* 'a' *)
			push (pop_2_and_apply (land));
			simulate (next_dir (i,j) dir) dir
		|111 -> (* 'o' *)
			push (pop_2_and_apply (lor));
			simulate (next_dir (i,j) dir) dir
		|120 -> (* 'x' *)
			push (pop_2_and_apply (lxor));
			simulate (next_dir (i,j) dir) dir
		|33 -> (* ! *)
			let a = pop () in
			push (if a=0 then 1 else 0);
			simulate (next_dir (i,j) dir) dir
		|96 -> (* ` *)
			push (if (pop_2_and_apply (>)) then 1 else 0);
			simulate (next_dir (i,j) dir) dir
		|62 -> simulate (next_dir (i,j)  Right) Right
		|60 -> simulate (next_dir (i,j) Left) Left
		|94 -> simulate (next_dir (i,j) Up) Up
		|118 -> simulate (next_dir (i,j) Down) Down
		|95 -> (* _ *)
			 let new_dir = if (pop () = 0) then Right else Left in
			 simulate (next_dir (i,j) new_dir) new_dir
		|124-> (* | *)
			 let new_dir = if (pop () = 0) then Down else Up in
			 simulate (next_dir (i,j) new_dir) new_dir
		|34 ->
			push_string (next_dir (i,j) dir) dir
		|58 -> (* : *)
			let a = pop () in
				push a; push a;
				simulate (next_dir (i,j) dir) dir;
		|92 -> (* \ *)
			let a = pop () in
			let b = pop () in
			push a;
			push b;
			simulate (next_dir (i,j) dir) dir;
		|36 -> (* $ *)
			ignore (pop ());
			simulate (next_dir (i,j) dir) dir
		|46 -> (* . *)
			let a = pop () in
			Printf.printf "%d" a;
			simulate (next_dir (i,j) dir) dir
		|44 -> (* , *)
			let a = pop () in	
			Printf.printf "%c" (Char.chr a);
			simulate (next_dir (i,j) dir) dir
		|35 ->  (* # *)
			let skip = next_dir (i,j) dir in
			let new_dir =  next_dir skip dir in
			simulate new_dir dir
		|112 -> (* p *)
			let y = pop () in
			let x = pop () in
			let v = pop () in
			code.(x).(y) <- v;
			simulate (next_dir (i,j) dir) dir
		|103 -> (*q *)
			let y = pop () in
			let x = pop () in
			push ( code.(x).(y));
			simulate (next_dir (i,j) dir) dir 
		|38 -> (* & *)
			let r = Scanf.scanf "%d" (fun r -> r) in
			push r;
			simulate (next_dir (i,j) dir) dir
		|126 -> (* ~ *)
			let c = Scanf.scanf "%c" (fun c -> c) in
			push (Char.code c);
			simulate (next_dir (i,j) dir) dir
		|64 -> () (* @ *)
		|32 -> simulate (next_dir (i,j) dir) dir (*' ' *)
		|x -> Printf.printf "Unknown character %c at line %d, position %d\n" (Char.chr x) i j
	and push_string (i,j) dir = 
(*		Printf.printf "Pushing string (%d, %d)\n" i j;*)
		match code.(i).(j) with
		|34 -> simulate (next_dir (i,j) dir) dir 
		| x -> 
			push (x);
			push_string (next_dir (i,j) dir) dir
in simulate (0,0) Right
;;	
