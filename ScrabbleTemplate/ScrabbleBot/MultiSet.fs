// Insert your MultiSet.fs file here. All modules must be internal

module internal MultiSet

    type MultiSet<'a when 'a : comparison> = 
        |MS of Map<'a, uint32>
        override s.ToString() =
            match s with
            |MS s -> let str = Map.fold(fun acc key value -> acc + ("(" + key.ToString() + ", #" + value.ToString() + "), ")) "" s 
                     "{" + str.Remove(str.Length-2) + "}"

    let empty = MS(Map.empty);;
    let isEmpty (MS s) = if s |> Seq.sumBy(fun a -> (int) a.Value) = 0 then true else false
        

    let size (MS s) = s |> Seq.sumBy(fun a -> a.Value);;

    let contains a (MS s) = s.ContainsKey(a);;
        
    let numItems a (MS s) = 
        let k = s.TryFind a 
        if k <> None then k.Value
        else uint32(0);;

    let add a n (MS s) =
        let k = s.TryFind a 
        if k <> None then MS(s.Add(a,n+k.Value))
        else MS(s.Add(a,n));;
   
    let addSingle a (MS s) = 
        let k = s.TryFind a 
        if k <> None then MS(s.Add(a,k.Value+1u))
        else MS(s.Add(a,1u));;
        
    let remove a n (MS s) =
        let k = s.TryFind a 
        if k <> None && int(k.Value)- int(n) >= 0 then MS(s.Add(a,k.Value-n))
        else MS(s.Add(a,0u));;

    let removeSingle a (MS s) =
        let k = s.TryFind a 
        if k <> None && int(k.Value)-1 >= 0 then MS(s.Add(a,k.Value-1u))
        else MS(s.Add(a,0u));;

    let fold f acc (MS s) = Map.fold f acc s;;
       
    let foldBack f (MS s) acc  = Map.foldBack f s acc;;   
       
    let map f (MS s) = fold (fun acc k v -> add(f k) v acc) empty (MS s);;
    
    let ofList (lst: 'a list) = List.fold (fun acc a -> addSingle a acc) empty lst;;
    
    let toList (s:MultiSet<'a>) = fold(fun acc key value -> acc@(List.replicate (int value) key)) [] s;;
                
    let union (s1:MultiSet<'a>) (s2:MultiSet<'a>) = fold(fun acc key value -> acc) s1 s2;;

    let sum (MS s1) (s2:MultiSet<'a>) = MS(fold(fun acc key value -> Map.add key (Map.find key acc+value) acc) s1 s2);;
      
    let subtract (MS s1) (s2:MultiSet<'a>) = MS(fold(fun acc key value -> Map.add key (Map.find key acc-value) acc) s1 s2);;
   
    let intersection (s1:MultiSet<'a>) (MS s2) = MS(fold(fun acc key value -> if s2.ContainsKey key then Map.add key (Map.find key s2) acc else acc) Map.empty s1);;
