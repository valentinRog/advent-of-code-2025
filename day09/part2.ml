let raw =
  In_channel.input_all stdin |> String.trim |> String.to_seq
  |> Seq.filter (fun c -> c != '\r')
  |> String.of_seq

module Point = struct
  type t = { x : int; y : int }

  let compare = compare

  let area { x = x1; y = y1 } { x = x2; y = y2 } =
    (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)
end

module PointMap = Map.Make (Point)

let data =
  raw |> String.split_on_char '\n'
  |> List.map (fun s ->
         let l = s |> String.split_on_char ',' |> List.map int_of_string in
         let nth i = List.nth l i in
         Point.{ x = nth 0; y = nth 1 })

let h_shrinked_map =
  let open Point in
  let rec f acc x = function
    | [] -> acc
    | hd :: tl ->
        let x0 = hd.x in
        let rec ff acc = function
          | [] -> (acc, [])
          | hd :: tl when hd.x = x0 ->
              let acc = PointMap.add hd Point.{ x; y = hd.y } acc in
              ff acc tl
          | l -> (acc, l)
        in
        let acc, tl = ff acc (hd :: tl) in
        f acc (x + 1) tl
  in
  f PointMap.empty 0
    (List.sort (fun (p1 : Point.t) p2 -> Int.compare p1.x p2.x) data)

let vh_shrinked_map =
  let open Point in
  let rec f acc y = function
    | [] -> acc
    | hd :: tl ->
        let y0 = hd.y in
        let rec ff acc = function
          | [] -> (acc, [])
          | h :: t when h.y = y0 ->
              let p = PointMap.find h acc in
              let updated = { p with y } in
              let acc = PointMap.add h updated acc in
              ff acc t
          | rest -> (acc, rest)
        in
        let acc, rest = ff acc (hd :: tl) in
        f acc (y + 1) rest
  in
  f h_shrinked_map 0
    (List.sort (fun (p : Point.t) q -> Int.compare p.y q.y) data)

let shrinked_point_to_old_point =
  vh_shrinked_map |> PointMap.to_seq
  |> Seq.fold_left (fun acc (k, v) -> PointMap.add v k acc) PointMap.empty

let points =
  shrinked_point_to_old_point |> PointMap.to_seq
  |> Seq.map (fun (k, _) -> k)
  |> List.of_seq

module PointSet = Set.Make (Point)

let make_border points =
  let open Point in
  let closest_on_axis p l =
    let dist p1 p2 = abs (p1.x - p2.x) + abs (p1.y - p2.y) in
    let rec f best best_dist = function
      | [] -> best
      | q :: rest ->
          let d = dist p q in
          if d < best_dist then f q d rest else f best best_dist rest
    in
    let hd = List.hd l in
    f hd (dist p hd) (List.tl l)
  in
  let add_vertical acc p =
    let same_x = List.filter (fun q -> q.x = p.x && q <> p) points in
    let nearest = closest_on_axis p same_x in
    let y_min = min p.y nearest.y in
    let y_max = max p.y nearest.y in
    let rec loop y acc =
      if y > y_max then acc else loop (y + 1) (PointSet.add { x = p.x; y } acc)
    in
    loop y_min acc
  in

  let add_horizontal acc p =
    let same_y = List.filter (fun q -> q.y = p.y && q <> p) points in
    let nearest = closest_on_axis p same_y in
    let x_min = min p.x nearest.x in
    let x_max = max p.x nearest.x in
    let rec loop x acc =
      if x > x_max then acc else loop (x + 1) (PointSet.add { x; y = p.y } acc)
    in
    loop x_min acc
  in

  let hs = List.fold_left add_vertical PointSet.empty points in
  let hs = List.fold_left add_horizontal hs points in
  hs

let hs =
  make_border (PointMap.fold (fun _ v acc -> v :: acc) vh_shrinked_map [])

let hs =
  let open Point in
  let ymin =
    hs |> PointSet.to_seq |> Seq.map (fun p -> p.y) |> Seq.fold_left min max_int
  in
  let x =
    hs |> PointSet.to_seq
    |> Seq.filter (fun p -> p.y = ymin)
    |> Seq.map (fun p -> p.x)
    |> Seq.fold_left min max_int
  in
  let p = { x = x + 1; y = ymin + 1 } in
  let rec f acc p =
    let acc = PointSet.add p acc in
    let add p acc = if PointSet.mem p acc then acc else f acc p in
    let { x; y } = p in
    let acc = add { x = x + 1; y } acc in
    let acc = add { x = x - 1; y } acc in
    let acc = add { x; y = y + 1 } acc in
    let acc = add { x; y = y - 1 } acc in
    acc
  in
  f hs p

let is_rectangle_in_bound p1 p2 =
  let open Point in
  let x0 = min p1.x p2.x in
  let x1 = max p1.x p2.x in
  let y0 = min p1.y p2.y in
  let y1 = max p1.y p2.y in
  let rec in_bound_x = function
    | x when x > x1 -> true
    | x when not (PointSet.mem { x; y = y0 } hs) -> false
    | x when not (PointSet.mem { x; y = y1 } hs) -> false
    | x -> in_bound_x (x + 1)
  in
  let rec in_bound_y = function
    | y when y > y1 -> true
    | y when not (PointSet.mem { x = x0; y } hs) -> false
    | y when not (PointSet.mem { x = x1; y } hs) -> false
    | y -> in_bound_y (y + 1)
  in
  in_bound_x x0 && in_bound_y y0

let res =
  let area p1 p2 =
    let get p = PointMap.find p shrinked_point_to_old_point in
    let open Point in
    let p1 = get p1 in
    let p2 = get p2 in
    Point.area p1 p2
  in
  points
  |> List.map (fun p -> points |> List.map (fun pp -> (p, pp)))
  |> List.flatten
  |> List.filter (fun (p1, p2) -> is_rectangle_in_bound p1 p2)
  |> List.fold_left (fun acc (p1, p2) -> max acc (area p1 p2)) 0

let () = Printf.printf "%d\n" res
