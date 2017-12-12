open Facile
open Easy

let read = fun file ->
  let chin = open_in file in
  let (m, n) = Scanf.fscanf chin " %d %d " (fun m n -> (m, n)) in
  let flights = Array.make_matrix m n 0 in
  let costs = Array.make n 0 in
  for j = 0 to n-1 do
    let (cj, nb1) =
      Scanf.fscanf chin " %d %d %d " (fun cj nb1 _ -> (cj, nb1)) in
    costs.(j) <- cj;
    for k = 0 to nb1-1 do
      let i = Scanf.fscanf chin " %d " (fun i -> i) in
      flights.(i-1).(j) <- 1
    done
  done;
  (costs, flights)
  
let solve = fun costs flights ->
	(* Variables *)
	let nb_rot = Array.length costs in
	let nb_fl = Array.length flights in
	let x = Fd.array nb_rot 0 1 in
	
	(* Constraintes *)

	Array.iter (fun fi -> Cstr.post (Arith.sum_fd (Array.init nb_rot (fun i -> x.(i)*fi.(i))) =~ 1)) flights;

	(* Goals *)
	let cost = Arith.sum_fd (fun i -> costs.(i)*x.(i)) in
	let print_bt = fun bt -> Printf.printf "\r%d%!" bt in
	let labeling = Goals.Array.labeling x in
	let start = Sys.time () in
	let solution = fun c -> Printf.printf "x:%a cost:%d\n" Fd.fprint_array x c in
	let opti = Goals.minimize labeling cost solution in
	ignore (Goals.solve ~control:print_bt opti)
	
let () =
	solve (read "toyrotations.ml")