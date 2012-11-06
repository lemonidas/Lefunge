exception End_of_code;;

let is_digit x = x>='0' && x <= '9';;

type dir_t = Up | Down | Left | Right;;

let is_digit x = (x>= Char.code '0' && x<= Char.code '9');;

let max a b = if (a>b) then a else b;;

let in_channel = open_in Sys.argv.(1);;

let rec loop max_len acc i= 
	try
		let line = input_line in_channel in
		let len = String.length line in
		loop (max max_len len) (line::acc) (i+1)
	with 
		End_of_file -> (List.rev acc, max_len, i)
;;

let (code_list, width, height) = loop 0 [] 0;;

let code = Array.make_matrix height width 32;;

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
in loop_list 0 0 code_list
;;


let stack = Stack.create ();;
let _ =
	Stack.push 0 stack;
;;

let render_code loc_x loc_y=
	let spacing = 18. in
	let start_x = 20. in
	let start_y = 580. in
	let rec loop i j cur_x cur_y =
		if (i>=height) then ()
			else if (j>=width) then
				loop (i+1) 0 (start_x) (cur_y -. spacing)
			else (
				if (i=loc_y && j = loc_x) then 
					GlDraw.color (1.,0.,0.)
				else GlDraw.color (1., 1., 1.);		
				GlPix.raster_pos ~x:cur_x ~y:cur_y ();
				Glut.bitmapCharacter ~font:Glut.BITMAP_HELVETICA_18 ~c:(code.(i).(j));
				loop i (j+1) (cur_x +. spacing) cur_y
			)
	in
	loop 0 0 start_x start_y
;;

let render_stack () =
	let sx_pos = 700. in
	let x_pos = ref 700. in
	let y_pos = ref 500. in
	let spacing = 15. in
	let render_digit_at_position num =
		GlPix.raster_pos ~x:!x_pos ~y:!y_pos ();
		Glut.bitmapCharacter ~font:Glut.BITMAP_HELVETICA_18 ~c:(Char.code num);
		x_pos := !x_pos+.spacing;
	in let render_num_at_position num =
		let str = string_of_int num in
		String.iter (render_digit_at_position) str;
		x_pos := sx_pos;
		y_pos := !y_pos -. spacing;		
	in Stack.iter (render_num_at_position) stack
;;

	
let width = ref 800 and height = ref 600;;


let pop_2_and_apply op =
	let a = Stack.pop stack in
	let b = Stack.pop stack in
	op b a
;;

let push x = Stack.push x stack;;
let pop () = Stack.pop stack;;

let i = ref 0;;
let j = ref 0;;
let dir = ref Right ;;

let next_dir ()=
		match !dir with
		|Up -> i := !i - 1
		|Down -> incr i
		|Left -> j := !j -1
		|Right -> incr j
;;

type mode_t = Normal | String;;

let mode = ref Normal;;

let last = ref 0. ;;
let interval = ref 1.;;

let rec simulate ()=
 (*	print_float (Sys.time());*)
(*	print_newline();*)
	if (Sys.time () > !interval +. !last) then
	(
	last := Sys.time ();
	(
	match !mode with
	|Normal -> (
		match code.(!i).(!j) with 
		|x when is_digit x ->
			push (x-(Char.code '0'));
			next_dir()
		|43 -> (* + *)
			push (pop_2_and_apply (+));
			next_dir()
		|45 -> (* - *)
			push (pop_2_and_apply (-));
			next_dir()
		|42-> (* * *)
			push (pop_2_and_apply ( * ));
			next_dir()
		|47 -> (* / *)
			push (pop_2_and_apply ( /));
			next_dir()
		|37 -> (* % *)
			push (pop_2_and_apply ( mod ));
			next_dir()
		|97 -> (* 'a' *)
			push (pop_2_and_apply (land));
			next_dir()
		|111 -> (* 'o' *)
			push (pop_2_and_apply (lor));
			next_dir()
		|120 -> (* 'x' *)
			push (pop_2_and_apply (lxor));
			next_dir()
		|33 -> (* ! *)
			let a = pop () in
			push (if a=0 then 1 else 0);
			next_dir()
		|96 -> (* ` *)
			push (if (pop_2_and_apply (>)) then 1 else 0);
			next_dir()
		|62 -> dir := Right; next_dir()
		|60 -> dir := Left;	next_dir()
		|94 -> dir := Up; next_dir()
		|118 -> dir := Down; next_dir()
		|95 -> (* _ *)
			 let new_dir = if (pop () = 0) then Right else Left in
			 dir := new_dir; next_dir()
		|124-> (* | *)
			 let new_dir = if (pop () = 0) then Down else Up in
			 dir := new_dir; next_dir()
		|34 ->
			next_dir(); mode := String
		|58 -> (* : *)
			let a = pop () in
			push a; push a;
			next_dir()
		|92 -> (* \ *)
			let a = pop () in
			let b = pop () in
			push a;
			push b;
			next_dir()
		|36 -> (* $ *)
			ignore (pop ());
			next_dir()
		|46 -> (* . *)
			let a = pop () in
			Printf.printf "%d" a;
			next_dir()
		|44 -> (* , *)
			let a = pop () in	
			Printf.printf "%c" (Char.chr a);
			next_dir()
		|35 ->  (* # *)
			next_dir(); next_dir()
		|112 -> (* p *)
			let y = pop () in
			let x = pop () in
			let v = pop () in
			code.(x).(y) <- v;
			next_dir()
		|103 -> (* g *)
			let y = pop () in
			let x = pop () in
			push ( code.(x).(y));
			next_dir()
		|38	-> (* & *)
			let r = Scanf.scanf "%d" (fun r -> r) in
			push r;
			next_dir()
		|126 -> (* ~ *)
			let c = Scanf.scanf "%c" (fun c -> c) in
			push (Char.code c);
			next_dir()
		|64 -> raise Exit (* @ *)
		|32	-> next_dir() (*' ' *)
		|x -> Printf.printf "Unknown character %c at line %d, position %d\n" (Char.chr x) !i !j; raise Exit
	)
	|String -> (
		match code.(!i).(!j) with
		|34 -> next_dir(); mode := Normal;
		| x -> push (x); next_dir()
	);
	);
	Glut.postRedisplay();
	)
	else ();
;;

let display () =
   	GlClear.clear [ `color ];
	GlMat.mode `modelview;
	GlMat.load_identity();
	render_code !j !i;
	render_stack ();
	Glut.swapBuffers ()
;;

(*OpenGL Specific stuff*)
let set_projection w h =
	GlMat.mode `projection;
	GlMat.load_identity ();
	GlMat.ortho ~x:(0., float w) ~y:(0., float h) ~z:(0., 1.)
;;

let reshape ~w ~h =
	GlDraw.viewport 0 0 w h;
	set_projection w h;
	width := w;
	height := h;
	display ()
;;


let gl_main =
   	ignore( Glut.init Sys.argv );
   	Glut.initDisplayMode ~double_buffer:true ();
	Glut.initWindowSize ~w:!width ~h:!height;
   	ignore (Glut.createWindow ~title:"Lefunge graphical interpreter");
	Glut.reshapeFunc ~cb:reshape;
   	Glut.displayFunc ~cb:display;
  	Glut.idleFunc ~cb:(Some simulate);
   	Glut.mainLoop ()
;;

