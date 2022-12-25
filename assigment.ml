let l_num idx_j list=
  let rec mricxveli idx_j idx_k x list1 = (
    match list1 with
    |[] -> 1.
    |(xa,yb)::t -> if (idx_k == idx_j) then mricxveli idx_j (idx_k+1) x t 
        else (x -. xa ) *. mricxveli idx_j (idx_k+1) x t
  )
  in mricxveli idx_j 0 list ;;



let  mnishvneli idx_j list=
  let rec helper idx_j idx_k list1 = 
    match list1 with
    | [] -> 1.
    | (xa,yb)::t -> if (idx_k!=idx_j) then ((fst (List.nth list idx_j )) -. xa )*. (helper idx_j (idx_k+1) t )  
        else helper idx_j (idx_k+1) t
  in helper idx_j 0 list ;;




let devider idx_j list x = 
  ((l_num idx_j x list) /. (mnishvneli idx_j list)) ;;



let lagrange list1=
  (fun x-> 
     let rec lagrange1 lst1 idx_j = (
       match lst1 with
       |[] -> 0.
       |(xa,yb)::t -> (yb *. devider idx_j list1 x) +. lagrange1 t (idx_j+1)
     )
     in lagrange1 list1 0
  );; 
