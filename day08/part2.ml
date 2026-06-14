let raw =
  In_channel.input_all stdin |> String.trim |> String.to_seq
  |> Seq.filter (fun c -> c != '\r')
  |> String.of_seq

module Point = struct
  type t = { x : int; y : int; z : int }

  let compare = compare

  let distance { x = x1; y = y1; z = z1 } { x = x2; y = y2; z = z2 } =
    let square n = n * n in
    float_of_int (square (x1 - x2) + square (y1 - y2) + square (z1 - z2))
end

module PointSet = Set.Make (Point)
module PointMap = Map.Make (Point)

let data =
  raw |> String.split_on_char '\n'
  |> List.map (fun s ->
         let l = s |> String.split_on_char ',' |> List.map int_of_string in
         let nth i = List.nth l i in
         Point.{ x = nth 0; y = nth 1; z = nth 2 })

let con =
  data
  |> List.map (fun p -> (p, PointSet.empty))
  |> List.to_seq |> PointMap.of_seq

let data =
  let rec f acc = function
    | [] -> acc
    | hd :: tl -> f ((data |> List.map (fun p -> (hd, p))) :: acc) tl
  in
  f [] data |> List.flatten
  |> List.filter (fun (p1, p2) -> p1 <> p2)
  |> List.sort (fun (p11, p12) (p21, p22) ->
         Float.compare (Point.distance p11 p12) (Point.distance p21 p22))
  |> List.filteri (fun i _ -> i mod 2 = 1)
  |> List.to_seq

let connect (p1, p2) con =
  let f p1 p2 con =
    let v = PointMap.find p1 con in
    let v = PointSet.add p2 v in
    PointMap.add p1 v con
  in
  f p2 p1 (f p1 p2 con)

let make_con n =
  List.fold_left
    (fun acc points -> connect points acc)
    con
    (data |> Seq.take n |> List.of_seq)

let extract_group p con =
  let rec f p acc =
    if PointSet.mem p acc then acc
    else
      let v = PointMap.find p con in
      let acc = PointSet.add p acc in
      PointSet.to_seq v |> Seq.fold_left (fun acc pp -> f pp acc) acc
  in
  f p PointSet.empty

let remove_network hs con =
  let rec f acc = function
    | [] -> acc
    | hd :: tl -> f (PointMap.remove hd acc) tl
  in
  f con (hs |> PointSet.to_seq |> List.of_seq)

let make_networks con =
  let rec f acc con =
    if PointMap.is_empty con then acc
    else
      let network = extract_group (fst (PointMap.choose con)) con in
      let con = remove_network network con in
      f (network :: acc) con
  in
  f [] con

let res =
  let rec f n1 n2 =
    let n = (n2 + n1 + 1) / 2 in
    let con = make_con n in
    let networks = make_networks con in
    match List.length networks with
    | 1 when n2 != n1 + 1 -> f n1 n
    | 1 -> data |> Seq.take n |> List.of_seq |> List.rev |> List.hd
    | _ -> f n n2
  in
  let p1, p2 = f 1 (List.of_seq data |> List.length) in
  p1.x * p2.x

let () = Printf.printf "%d\n" res
