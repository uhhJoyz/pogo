open Equiv

(* For Live Variant: *)
(* Can prune when score is less than half of max found *)
(* Can prune when 3 characters without progress *)

(* O(min(n, m)) where n is len of key and m is len of value *)
let greedy_match key value =
  let rev_seq str =
    let ind = String.length str - 1 in
    let rec aux ind () =
      if ind < 0 then Seq.Nil else Seq.Cons (str.[ind], aux (ind - 1))
    in
    aux ind
  in
  let seqa_f, seqa_r = String.to_seq key, rev_seq key in
  let seqb_f, seqb_r = String.to_seq value, rev_seq value in
  let rec aux acc p a b =
    if p >= 3
    then acc
    else (
      match Seq.uncons a, Seq.uncons b with
      | None, _ -> acc
      | _, None -> acc
      | Some (x, isa), Some (y, isb) ->
        (match congruent (x |> Char.lowercase_ascii) (y |> Char.lowercase_ascii) with
         | Equivalent -> Equivalent, 0, isa, isb
         | Congruent -> Congruent, 0, isa, isb
         | Divergent -> Divergent, p + 1, seqa_f, isb)
        |> fun (v, pat, sa, sb) -> aux (acc + Equiv.to_int v) pat sa sb)
  in
  let apply = aux (-Bool.to_int (String.length key != String.length value)) 0 in
  max (apply seqa_f seqb_f) (apply seqa_r seqb_r)
;;

let find_best_opt key l =
  Array.mapi (fun i v -> i, greedy_match key v) l
  |> Array.fold_left (fun ((_, a) as p1) ((_, b) as p2) -> if a > b then p1 else p2) (0, 0)
  |> fun (i, v) -> if v <= 0 then None else Some i
;;
