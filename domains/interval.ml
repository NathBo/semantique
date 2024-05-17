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
 
 
type interval = ITop | IBottom | Inter of Z.t*Z.t


let contains_zero a = match a with
  | ITop -> true
  | Inter (a,b) when a <= Z.zero && b>=Z.zero -> true
  | _ -> false


let rec minList l = match l with
  | [] -> failwith "non"
  | x::[] -> x
  | x::q -> Z.min x (minList q)

let rec maxList l = match l with
  | [] -> failwith "non"
  | x::[] -> x
  | x::q -> Z.max x (maxList q)
  

 
 module CONSTANTDOMAIN : Value_domain.VALUE_DOMAIN =
   struct
     
 
     (* type of abstract elements *)
     (* an element of type t abstracts a set of integers *)
 
     type t = interval

     let to_string a = match a with
     | ITop -> "Top"
     | IBottom -> "Bottom"
     | Inter (a,b) -> "["^string_of_int (Z.to_int a)^","^string_of_int (Z.to_int a)^"]"
 
     (* unrestricted value: [-oo,+oo] *)
 
     (* bottom value: empty set *)
     let bottom = IBottom
 
     (* constant: {c} *)
     let const n = Inter (n,n)
 
     (* interval: [a,b] *)
     let rand a b = Inter(a,b)
 
     let top = ITop 
 
 
     (* unary operation *)
     let unary a op = match a,op with
      | Inter(x,y),AST_UNARY_MINUS -> Inter(Z.(~-) y,Z.(~-) x)
      | _ -> a
 
     (* binary operation *)
     let binary a b op = match op,a,b with
     | AST_DIVIDE,_,b | AST_MODULO,_,b when contains_zero b -> raise DivisionByZero
     | _,IBottom,_ | _,_,IBottom -> IBottom
     | _,ITop,_ | _,_,ITop -> ITop
     | AST_PLUS,Inter(a,b),Inter(c,d) -> Inter(Z.(+) a c,Z.(+) b d)
     | AST_MINUS,Inter(a,b),Inter(c,d) -> Inter(Z.(-) a d,Z.(-) b c)
     | AST_MULTIPLY,Inter(a,b),Inter(c,d) | AST_DIVIDE,Inter(a,b),Inter(c,d) -> let l = [apply_int_bin_op op a c; apply_int_bin_op op a d; apply_int_bin_op op b c; apply_int_bin_op op c d] in
      Inter(minList l, maxList l) 
     | AST_MODULO,_,_ -> ITop
 
 
     (* comparison *)
     (* [compare x y op] returns (x',y') where
        - x' abstracts the set of v  in x such that v op v' is true for some v' in y
        - y' abstracts the set of v' in y such that v op v' is true for some v  in x
        i.e., we filter the abstract values x and y knowing that the test is true
 
        a safe, but not precise implementation, would be:
        compare x y op = (x,y)
      *)
     let compare x y op = match (x,y) with
      | IBottom,_ | _,IBottom -> (IBottom,IBottom)
      | Const(a),Const(b) when apply_compare_op op a b -> (x,y)
      | Const(_),Const(_) -> (print_endline "ya plus personne";(Bottom,Bottom))
      | _ -> (Top,Top)
 
 
 
 
     (* set-theoretic operations *)
     let join a b = match a,b with
      | Top,_ | _,Top -> Top
      | Const(n1),Const(n2) when n1 = n2 -> Const(n1)
      | Const(_),Const(_) -> Top
      | Const(n),Bottom | Bottom,Const(n) -> Const(n)
      | _ -> Bottom


     let meet a b = match a,b with
     | Bottom,_ | _,Bottom -> Bottom
     | Const(n1),Const(n2) when n1 = n2 -> Const(n1)
     | Const(_),Const(_) -> Bottom
     | Const(n),Top | Top,Const(n) -> Const(n)
     | _ -> Top
 
     (* widening *)
     let widen a b = Top
 
     (* narrowing *)
     let narrow a b = match a,b with
     | Bottom,_ | _,Top -> Bottom
     | Top,_ -> Top
     | a,Bottom -> (print_endline "on a rien change dans :";print_endline (to_string a);a)
     | Const(n1),Const(n2) when n1 = n2 -> Bottom
     | Const(_),Const(_) -> a


     (* subset inclusion of concretizations *)
     let subset a b = match a,b with
     | Bottom,_ | _,Top -> true
     | Top,_ -> false
     | _,Bottom -> false
     | Const(n1),Const(n2) when n1 = n2 -> true
     | Const(_),Const(_) -> false
 
     (* check the emptiness of the concretization *)
     let is_bottom a = a = Bottom
 
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
     let bwd_binary x y op r = match op,x,y with
     | AST_DIVIDE,_,b | AST_MODULO,_,b when constains_zero b -> raise DivisionByZero
     | _ ->
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
     | Top -> Format.fprintf fmt "Top"
     | Bottom -> Format.fprintf fmt "Bottom"
     | Const n -> Format.fprintf fmt "%i" (Z.to_int n)

 
 end
 
 
 