(*
  Cours "Sémantique et Application à la Vérification de programmes"

  Ecole normale supérieure, Paris, France / CNRS / INRIA
*)

(*
  Signature of abstract domains representing sets of integers
  (for instance: constants or intervals).
 *)

 open Frontend
 open Abstract_syntax_tree
 
 
type sgn = Zero | Minus | Plus | StMinus | StPlus | SBot | STop 


let sign_minus s = match s with
  | Zero -> Zero
  | Plus -> Minus
  | Minus -> Plus
  | StPlus -> StMinus
  | StMinus -> StPlus
  | _ -> s

let is_minus s = match s with
   | Minus | StMinus -> true
   | _ -> false

let is_plus s = match s with
  | Plus | StPlus -> true
  | _ -> false


let contains_zero s = match s with
| STop | Zero | Plus | Minus -> true
| _ -> false
  
 
 module SIGN_DOMAIN : Value_domain.VALUE_DOMAIN =
   struct
     
 
     (* type of abstract elements *)
     (* an element of type t abstracts a set of integers *)
 
     type t = sgn
 
     (* unrestricted value: [-oo,+oo] *)
 
     (* bottom value: empty set *)
     let bottom = SBot
 
     (* constant: {c} *)
     let const n = if n=Z.zero then Zero
     else if n<Z.zero then StMinus
     else StPlus

    
     (* interval: [a,b] *)
     let rand a b = 
      if a>b
      then SBot
      else if a<Z.zero
      then if b<Z.zero
        then StMinus
        else if b=Z.zero
        then Minus
        else STop
      else if a=Z.zero
        then if b=Z.zero
          then Zero
          else Plus
        else StPlus
 
     let top = STop
 
 
     (* unary operation *)
     let unary a op = match op with
        | AST_UNARY_PLUS -> a
        | AST_UNARY_MINUS -> sign_minus a
 
     (* binary operation *)
     let rec binary a b op = match op,a,b with
        | _,SBot,_ | _,_,SBot -> SBot
        | _,STop,_ | _,_,STop -> STop
        | AST_PLUS,Minus,StMinus | AST_PLUS,StMinus,Minus | AST_PLUS,StMinus,StMinus -> StMinus
        | AST_PLUS,Plus,StPlus | AST_PLUS,StPlus,Plus | AST_PLUS,StPlus,StPlus -> StPlus
        | AST_PLUS,Plus,Plus -> Plus
        | AST_PLUS,Minus,Minus -> Minus
        | AST_PLUS,_,_ -> STop
        | AST_MINUS,_,_ -> binary a (sign_minus b) AST_PLUS
        | AST_MULTIPLY,Zero,_ | AST_MULTIPLY,_,Zero | AST_DIVIDE,Zero,_ -> Zero
        | AST_MULTIPLY,StMinus,StMinus | AST_MULTIPLY,StPlus,StPlus -> StPlus
        | AST_MULTIPLY,StMinus,StPlus | AST_MULTIPLY,StPlus,StMinus -> StMinus
        | AST_MULTIPLY,a,b | AST_DIVIDE,a,b | AST_MODULO,a,b when is_minus a && is_plus b || (is_plus a && is_minus b) -> Minus
        | AST_MULTIPLY,_,_ | AST_DIVIDE,_,_ | AST_MODULO,_,_ -> Plus
 
     
 
 
 
 
     (* set-theoretic operations *)
     let join a b =
      match a, b with
      | STop, _ | _, STop -> STop
      | a, b when is_minus a && is_plus b -> STop
      | b, a when is_minus a && is_plus b -> STop
      | a, b when a = b -> a
      | Zero, a when is_minus a -> Minus
      | a, Zero when is_minus a -> Minus
      | Zero, a when is_plus a -> Plus
      | a, Zero when is_plus a -> Plus
      | _ -> failwith "il manque un cas dans les join"
    
      


     let meet a b =
      match a, b with
      | SBot, _ | _, SBot -> STop
      | a, b when a = b -> a
      | Zero,a when contains_zero a -> Zero
      | a,Zero when contains_zero a -> Zero
      | Plus,Minus | Minus,Plus -> Zero
      | a, b when is_minus a && is_plus b -> SBot
      | b,a when is_minus a && is_plus b -> SBot
      | _ -> failwith "il manque un cas dans les meet"



 (* narrowing *)
    let narrow a b = match a,b with
      | SBot,_ | _,STop -> SBot
      | a,SBot -> a
      | a,b when a=b -> SBot
      | STop,a when not (contains_zero a) -> join (sign_minus a) Zero
      | STop,Minus -> StPlus
      | STop,Plus -> StMinus
      | STop,_ -> failwith "normalement n'arrive pas dans narrow"
      | Plus,Zero | Plus,Minus -> StPlus
      | Minus,Zero | Minus,Plus -> StMinus
      | Plus,StPlus -> Zero
      | Minus,StMinus -> Zero
      | Plus,StMinus | Minus,StPlus -> a
      | _ -> failwith "il manque des cas dans narrow"




(* comparison *)
     (* [compare x y op] returns (x',y') where
        - x' abstracts the set of v  in x such that v op v' is true for some v' in y
        - y' abstracts the set of v' in y such that v op v' is true for some v  in x
        i.e., we filter the abstract values x and y knowing that the test is true
 
        a safe, but not precise implementation, would be:
        compare x y op = (x,y)
      *)



     let compare x y op = match op,x,y with
      | AST_EQUAL,_,_ -> (meet x y, meet x y) 
      | _ -> failwith "non"


 
     (* widening *)
     let widen a b = STop
 
  


     (* subset inclusion of concretizations *)
     let subset a b = narrow a b = SBot
 
     (* check the emptiness of the concretization *)
     let is_bottom a = a = SBot
 
     (* backards unary operation *)
     (* [bwd_unary x op r] return x':
        - x' abstracts the set of v in x such as op v is in r
        i.e., we fiter the abstract values x knowing the result r of applying
        the operation on x
      *)
     let bwd_unary x op r = match x with
      | Const n when subset (Const (apply_int_un_op op n)) r -> x
      | Const _ -> Bottom
      | _ -> meet x r
 
     (* backward binary operation *)
     (* [bwd_binary x y op r] returns (x',y') where
       - x' abstracts the set of v  in x such that v op v' is in r for some v' in y
       - y' abstracts the set of v' in y such that v op v' is in r for some v  in x
       i.e., we filter the abstract values x and y knowing that, after
       applying the operation op, the result is in r
       *)
     let bwd_binary x y op r =
      let aux n nr b = match op with
      | AST_PLUS -> Const (Z.(-) nr n)
      | AST_MINUS when b -> Const(Z.(-) n nr )
      | AST_MINUS -> Const(Z.(+) n nr )
      | AST_MULTIPLY when n<>Z.zero && Z.(mod) nr n = Z.zero -> Const(Z.(/) nr n)
      | AST_MULTIPLY -> Bottom
      | AST_MODULO when not b && Z.abs nr>= Z.abs n -> Bottom     (*a%b<b*)
      | AST_MODULO when b && Z.abs nr > Z.abs n -> Bottom          (*a/b<=a*)
      | _ -> Top in
      match x,y,r with
      | Const(n1),Const(n2),_ when subset (Const (apply_int_bin_op op n1 n2)) r -> (x,y)
      | Const(_),Const(_),_ -> (Bottom,Bottom)
      | Const(n),Top,Const(nr) -> let a = aux n nr true in if a=Bottom then (Bottom,Bottom) else Const(n),a
      | Top,Const(n),Const(nr) -> let a = aux n nr false in if a=Bottom then (Bottom,Bottom) else a,Const(n)
      | Const(n),Top,Top -> Const(n),Top
      | Top,Const(n),Top -> Top,Const(n)
      | _,_,Bottom | _,Bottom,_ | Bottom,_,_ -> Bottom,Bottom
      | Top,Top,_ -> Top,Top
 
     (* print abstract element *)
     let print fmt a = match a with
     | Top -> Format.fprintf fmt "Top";
     | Bottom -> Format.fprintf fmt "Bottom";
     | Const n -> Format.fprintf fmt "%i" (Z.to_int n);
 
 end
 
 
 