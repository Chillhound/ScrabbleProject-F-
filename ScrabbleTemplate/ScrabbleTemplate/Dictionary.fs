// Insert your Dictionary.fsi file here. All modules must be internal.

module internal Dictionary

type Dict =
       | Node of bool * Map<char,Dict>

let empty (u:unit) = Node(false, Map.empty);;

let rec splitWord (s:string) = Seq.toList s;;

let orEmpty =
   function
   |Some t -> t
   |None -> empty;;
 
let insert (s:string) (dict:Dict) = 
   let rec aux  = 
       function
       | ([], Node(b,l)) -> Node(true,l)
       | (d::ds, Node(b,l)) ->
       match Map.tryFind d l with
           |Some v -> Node(b, l.Add(d,aux(ds, v)))
           |None -> Node(b, l.Add(d, aux(ds, Node(false, Map.empty))))
   aux (splitWord s, dict);;


let lookup (s:string) (dict:Dict) =
   let rec aux =
       function
       |([],Node(b,_)) -> b
       |(d::ds, Node(_,l)) ->
           match Map.tryFind d l with
               |Some v -> aux(ds,v)
               |None -> false
   aux (splitWord s, dict);;

let step (c:char) (dict:Dict) = 
       match dict with
       |Node(_,l) ->
           match Map.tryFind c l with
               |Some v -> match v with 
                           |Node(s,_) -> Some (s,v)
               |None -> None;;


    // Only implement reverse if you have made a Gaddag
    let reverse : Dict -> (bool * Dict) option = fun _ -> failwith "Not implemented"