open Facile
open Easy

let data =
	[|(3,[|2;1;1;1;1;1|]);
	(19,[|10;9;7;6;4;4;3;3;3;3;3;2;2;2;1;1;1;1;1;1|]);
	(112,[|50;42;37;35;33;29;27;25;24;19;18;17;16;15;11;9;8;7;6;4;2|]);
	(175,[|81;64;56;55;51;43;39;38;35;33;31;30;29;20;18;16;14;9;8;5;4;3;2;1|]);
	|]

let fprint = fun filename t x y -> (* printing for GNUplot *)
  let ch = open_out filename in
  Array.iteri
    (fun i ti ->
      let xi = Fd.elt_value x.(i) and yi = Fd.elt_value y.(i) in
      Printf.fprintf ch "%d %d\n%d %d\n%d %d\n%d %d\n%d %d\n\n"
        xi yi (xi+ti) yi (xi+ti) (yi+ti) xi (yi+ti) xi yi)
    t;
  close_out ch


let solve = fun size t ->
	let x = Array.map (fun ti -> Fd.interval 0 (size-ti)) t in
	let y = Array.map (fun ti -> Fd.interval 0 (size-ti)) t in
	let n = Array.length t in	

	
	for i=0 to n-1 do
		for j=i+1 to n-1 do
			Cstr.post ((fd2e x.(i) +~ i2e t.(i) <=~ fd2e x.(j)) ||~~
						(fd2e y.(i) +~ i2e t.(i) <=~ fd2e y.(j)) ||~~
 						(fd2e x.(j) +~ i2e t.(j) <=~ fd2e x.(i)) ||~~
 						(fd2e y.(j) +~ i2e t.(j) <=~ fd2e y.(i)))
		done
	done;
	for v=0 to size-1 do
		let w = Array.mapi (fun i ti -> Interval.is_member x.(i) (v-ti+1) v) t in
		let z = Array.mapi (fun i ti -> Interval.is_member y.(i) (v-ti+1) v) t in
		Cstr.post (Arith.scalprod_fd t w =~ i2e size);
		Cstr.post (Arith.scalprod_fd t z =~ i2e size)
	done;

	(*let goal = Goals.Array.labeling x &&~ Goals.Array.labeling y in*)
	let minmin = Goals.Array.choose_index (fun v1 v2 -> Fd.min v1 < Fd.min v2) in
	let goal = Goals.Array.forall ~select:minmin Goals.assign x &&~ Goals.Array.forall ~select:minmin Goals.assign y in
	let print_bt = fun bt -> Printf.printf "\r%d%!\n" bt in
  	if Goals.solve ~control:print_bt goal then
    	fprint "solutions" t x y
  	else Printf.printf "No solution found\n"

let () =
	let idx = int_of_string Sys.argv.(1) in	
	let (size, t) = data.(idx) in
	
	solve size t

	
