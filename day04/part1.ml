let raw =
  In_channel.input_all stdin |> String.trim |> String.to_seq
  |> Seq.filter (fun c -> c != '\r')
  |> String.of_seq

module Point = struct
  type t = int * int

  let compare = compare

  let around (x, y) =
    [
      (x, y - 1);
      (x + 1, y - 1);
      (x + 1, y);
      (x + 1, y + 1);
      (x, y + 1);
      (x - 1, y + 1);
      (x - 1, y);
      (x - 1, y - 1);
    ]
    |> List.to_seq
end

module PointSet = Set.Make (Point)

let m =
  raw |> String.split_on_char '\n' |> List.to_seq
  |> Seq.mapi (fun y line ->
         line |> String.to_seqi |> Seq.map (fun (x, c) -> ((x, y), c)))
  |> Seq.concat
  |> Seq.filter (fun (_, c) -> c == '@')
  |> Seq.map (fun (p, _) -> p)
  |> PointSet.of_seq

let is_accessible p =
  Point.around p |> Seq.filter (fun p -> PointSet.mem p m) |> Seq.length < 4

let res = m |> PointSet.to_seq |> Seq.filter is_accessible |> Seq.length
let () = Printf.printf "%d\n" res
