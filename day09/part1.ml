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

let data =
  raw |> String.split_on_char '\n'
  |> List.map (fun s ->
         let l = s |> String.split_on_char ',' |> List.map int_of_string in
         let nth i = List.nth l i in
         Point.{ x = nth 0; y = nth 1 })

let res =
  let max_area_with_point p =
    let rec f acc = function
      | [] -> acc
      | hd :: tl -> f (max acc (Point.area p hd)) tl
    in
    f 0 data
  in
  let rec f acc = function
    | [] -> acc
    | hd :: tl -> f (max acc (max_area_with_point hd)) tl
  in
  f 0 data

let () = Printf.printf "%d\n" res