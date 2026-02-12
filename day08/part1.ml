let raw =
  In_channel.input_all stdin |> String.trim |> String.to_seq
  |> Seq.filter (fun c -> c != '\r')
  |> String.of_seq

module Point = struct
  type t = { x : int; y : int; z : int }

  let compare = compare

  let distance { x = x1; y = y1; z = z1 } { x = x2; y = y2; z = z2 } =
    let square n = n * n in
    sqrt (float_of_int (square (x1 - x2) + square (y1 - y2) + square (z1 - z2)))

  let print { x; y; z } = Printf.printf "%d %d %d\n" x y z
end

module PointSet = Set.Make (Point)

let data =
  raw |> String.split_on_char '\n'
  |> List.map (fun s ->
         let l = s |> String.split_on_char ',' |> List.map int_of_string in
         let nth i = List.nth l i in
         Point.{ x = nth 0; y = nth 1; z = nth 2 })

let data =
  let rec f acc = function
    | [] -> acc
    | hd :: tl -> f ((data |> List.map (fun p -> (hd, p))) :: acc) tl
  in
  f [] data |> List.flatten
  |> List.filter (fun (p1, p2) -> p1 <> p2)
  |> List.sort (fun (p11, p12) (p21, p22) ->
         Float.compare (Point.distance p11 p12) (Point.distance p21 p22))
  |> List.to_seq |> Seq.take 10 |> List.of_seq
