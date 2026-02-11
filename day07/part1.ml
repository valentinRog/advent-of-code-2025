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

let advance_beam (x, y) =
  match PointMap.find_opt (x, y + 1) m with
  | None -> None
  | Some '.' -> Some [ (x, y + 1) ]
  | Some '^' -> Some [ (x - 1, y + 1); (x + 1, y + 1) ]
  | _ -> assert false

let rec compute_beams acc hs =
  let l = hs |> PointSet.to_seq |> List.of_seq in
  if advance_beam (List.hd l) |> Option.is_none then acc
  else
    let l = l |> List.map advance_beam |> List.map Option.get in
    let n = l |> List.filter (fun l -> List.length l = 2) |> List.length in
    compute_beams (acc + n) (l |> List.flatten |> PointSet.of_list)

let res = compute_beams 0 (PointSet.of_list [ p0 ])
let () = Printf.printf "%d\n" res
