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


     let to_string a = match a with
     | STop -> "Top"
     | SBot -> "Bot"
     | Plus -> "Plus"
     | Minus -> "Minus"
     | StPlus -> "StPlus"
     | StMinus -> "StMinus"
     | Zero -> "Zero"
 
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
     | AST_DIVIDE,_,b | AST_MODULO,_,b when contains_zero b -> raise DivisionByZero
     | _ -> match op,a,b with
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
      | Zero,b when not (contains_zero b) -> SBot
      | a,Zero when not(contains_zero a) -> SBot
      | _ -> failwith ("il manque un cas dans les meet"^(to_string a)^(to_string b))



 (* narrowing *)
    let narrow a b = match a,b with
      | SBot,_ | _,STop -> SBot
      | a,SBot -> a
      | a,b when a=b -> SBot
      | STop,a when not (contains_zero a) -> join (sign_minus a) Zero
      | STop,Minus -> StPlus
      | STop,Plus -> StMinus
      | STop,Zero -> STop
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



     let rec compare x y op = match op,x,y with
      | _,SBot,_ | _,_,SBot -> SBot,SBot
      | AST_EQUAL,_,_ -> (meet x y, meet x y) 
      | AST_NOT_EQUAL,_,_ -> if x = Zero && y = Zero then SBot,SBot else x,y
      | AST_GREATER,_,_ -> let a,b = compare y x AST_LESS in (b,a)
      | AST_GREATER_EQUAL,_,_ -> let a,b = compare y x AST_LESS_EQUAL in (b,a)
      | AST_LESS,_,STop | AST_LESS_EQUAL,_,STop -> (x,STop)
      | AST_LESS,STop,_ | AST_LESS_EQUAL,STop,_ -> (y,y)
      | (AST_LESS_EQUAL,x,y) when x = y -> (x,y)
      | AST_LESS,Minus,Zero | AST_LESS,StMinus,Zero | AST_LESS_EQUAL,StMinus,Zero -> StMinus,Zero
      | AST_LESS_EQUAL,Minus,Zero -> Minus,Zero
      | AST_LESS_EQUAL,Zero,y when not (is_minus y) -> Zero,y
      | AST_LESS,Zero,y when is_plus y -> Zero,StPlus
      | _,Plus,StPlus | _,StPlus,Plus -> StPlus,StPlus
      | AST_LESS,Plus,Minus -> Zero,Zero
      | AST_LESS,Minus,_ -> Minus,y
      | AST_LESS,StMinus,_ -> StMinus,y
      | AST_LESS,Plus,_ | AST_LESS,StPlus,_ | AST_LESS,Zero,_ -> x,meet y StPlus
      | _ -> x,y


 
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
     let bwd_unary x op r = 
      meet (unary x op) r
 
     (* backward binary operation *)
     (* [bwd_binary x y op r] returns (x',y') where
       - x' abstracts the set of v  in x such that v op v' is in r for some v' in y
       - y' abstracts the set of v' in y such that v op v' is in r for some v  in x
       i.e., we filter the abstract values x and y knowing that, after
       applying the operation op, the result is in r
       *)
     let bwd_binary x y op r = match op,x,y with
     | AST_DIVIDE,_,b | AST_MODULO,_,b when contains_zero b -> raise DivisionByZero
     | _ ->
      let repx = ref SBot in
      let repy = ref SBot in
      let l = [|StMinus;Zero;StPlus|] in
      for i=0 to 2 do
        if subset (binary l.(i) y op) r
        then repx := join !repx l.(i)
      done;
      for i=0 to 2 do
        if subset (binary x l.(i) op) r
        then repy := join !repy l.(i)
      done;
      (!repx,!repy)

 
     (* print abstract element *)
     let print fmt a = match a with
     | STop -> Format.fprintf fmt "Top"
     | SBot -> Format.fprintf fmt "Bot"
     | Plus -> Format.fprintf fmt "Plus"
     | Minus -> Format.fprintf fmt "Minus"
     | StPlus -> Format.fprintf fmt "StPlus"
     | StMinus -> Format.fprintf fmt "StMinus"
     | Zero -> Format.fprintf fmt "Zero"

 
 end
 
 
 