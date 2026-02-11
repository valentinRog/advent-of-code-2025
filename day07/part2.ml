let raw =
  In_channel.input_all stdin |> String.trim |> String.to_seq
  |> Seq.filter (fun c -> c != '\r')
  |> String.of_seq

module Point = struct
  type t = int * int

  let compare = compare
end

module PointMap = Map.Make (Point)
module PointSet = Set.Make (Point)

let m =
  raw |> String.split_on_char '\n' |> List.to_seq
  |> Seq.mapi (fun y line ->
         line |> String.to_seqi |> Seq.map (fun (x, c) -> ((x, y), c)))
  |> Seq.concat |> PointMap.of_seq

let p0 =
  m |> PointMap.to_seq |> Seq.find (fun (_, c) -> c = 'S') |> Option.get |> fst

let res =
  let rec dfs (x, y) cache =
    match PointMap.find_opt (x, y) cache with
    | Some n -> (n, cache)
    | None ->
        let n, cache =
          match PointMap.find_opt (x, y + 1) m with
          | None -> (1, cache)
          | Some '.' -> dfs (x, y + 1) cache
          | Some '^' ->
              let n1, cache = dfs (x - 1, y + 1) cache in
              let n2, cache = dfs (x + 1, y + 1) cache in
              (n1 + n2, cache)
          | _ -> assert false
        in
        (n, cache |> PointMap.add (x, y) n)
  in
  dfs p0 PointMap.empty |> fst

let () = Printf.printf "%d\n" res
