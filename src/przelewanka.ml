(************************************************************)
(* Author: Kacper Kramarz-Fernandez, index: 429629, gr: IV *)
(* Reviewer: Bartłomiej Kucypera gr: I *)
(************************************************************)

(* Computes the GCD of an array, ignoring elements that are zero. *)
let array_gcd (arr: int array) : int = 
    let gcd a b =
        let (aa,bb) = if a > b then (a,b) else (b,a) in
        let rec aux x y =
            if x = 0 then y
            else aux (y mod x) x
        in aux aa bb in
    Array.fold_left (fun res x -> if x > 0 then gcd res x else res) 0 arr
;;

(* (x,y) - (capacity, final fill) *)
let przelewanka (arr: (int*int) array) : int =
    let pp_arr = (* pre-processed input array with 0-capacity glasses removed *)
        let lst = Array.to_list arr in (* these conversions are memory inefficient, but hey - at least we're using higher order functions *)
        let fltr = List.filter (fun (cap, _) -> cap > 0) lst in
        Array.of_list fltr in
    let n = Array.length pp_arr in
    if n = 0 then 0 (* no positive capacity glasses - result is 0 *)
    else 
        let capacities = Array.map fst pp_arr in
        let final_state = Array.map snd pp_arr in
        let arr_gcd = array_gcd capacities in
        if not (Array.for_all (fun (_,fill) -> fill mod arr_gcd = 0) pp_arr) then -1 (* all final fills must be divisible by the array's gcd *)
        else if not (Array.exists (fun (cap, fill) -> (cap = fill || fill = 0)) pp_arr) then -1 (* the final state must contain at least one empty or full glass *)
        else if (Array.exists (fun (cap, fill) -> fill > cap) pp_arr) then -1 (* no glass can be filled beyond its capacity *)
        else if (Array.for_all (fun (_, fill) -> fill = 0) pp_arr) then 0 (* no steps required *)
        else
            let q = Queue.create () in
            let (htbl : (int array, int) Hashtbl.t) = Hashtbl.create 100000 in
            let add_state (state, steps) =
                if not (Hashtbl.mem htbl state) then
                begin
                    Hashtbl.add htbl state steps;
                    Queue.add (state, steps) q
                end in

            (* legal operations on the cups *)
            let pour_in (state, steps) i = (* fills the i-th cup *)
                if not (state.(i) = capacities.(i)) then
                begin
                    let after_pour = Array.copy state in
                    after_pour.(i) <- capacities.(i);
                    add_state (after_pour, steps + 1)
                end in 
            let pour_out (state, steps) i = (* empties the i-th cup *)
                if not (state.(i) = 0) then
                begin
                    let after_pour = Array.copy state in
                    after_pour.(i) <- 0;
                    add_state (after_pour, steps + 1)
                end in
            let pour_to (state, steps) i j = (* pours water from the i-th cup to the j-th cup *)
                if not (state.(i) = 0 || state.(j) = capacities.(j)) then
                begin
                    let after_pour = Array.copy state in
                    after_pour.(j) <- min (capacities.(j)) (state.(j) + state.(i));
                    after_pour.(i) <- after_pour.(i) - (after_pour.(j) - state.(j));
                    add_state (after_pour, steps + 1)
                end in

            (* the actual BFS *)
            begin
                let initial_state = (Array.make n 0, 0) in
                Queue.add initial_state q;
                while not (Queue.is_empty q) && not (Hashtbl.mem htbl final_state) do 
                    let (state, steps) = Queue.pop q in
                    for i = 0 to n-1 do
                        pour_in (state, steps) i;
                        pour_out (state, steps) i;
                        for j = 0 to n-1 do
                            if i <> j then pour_to (state, steps) i j
                        done
                    done
                done
            end;
            try Hashtbl.find htbl final_state with 
            | Not_found -> -1
;;  
