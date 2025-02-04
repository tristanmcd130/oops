type 'a t = {bindings: (string, 'a) Hashtbl.t; parent: 'a t option}

let create bindings parent = {bindings = bindings |> List.to_seq |> Hashtbl.of_seq; parent = parent}
let rec lookup env name = 
  match Hashtbl.find_opt env.bindings name with
  | Some v -> v
  | None ->
    match env.parent with
    | Some p -> lookup p name
    | None -> failwith (name ^ " not found")
let bind env name value = Hashtbl.replace env.bindings name value