let raw =
  In_channel.input_all stdin |> String.trim |> String.to_seq
  |> Seq.filter (fun c -> c != '\r')
  |> String.of_seq

let data =
  let rec f acc = function
    | [] -> acc
    | hd :: tl ->
        f (List.combine hd acc |> List.map (fun (hd, tl) -> hd :: tl)) tl
  in
  let l =
    let is_not_empty s = not (String.equal s "") in
    raw |> String.split_on_char '\n'
    |> List.map (fun s ->
           s |> String.split_on_char ' ' |> List.filter is_not_empty)
  in
  f (Seq.repeat [] |> Seq.take (l |> List.hd |> List.length) |> List.of_seq) l

let compute_column l =
  let op =
    match String.get (List.hd l) 0 with
    | '*' -> fun n1 n2 -> n1 * n2
    | '+' -> fun n1 n2 -> n1 + n2
    | _ -> assert false
  in
  let rec f acc = function [] -> acc | hd :: tl -> f (op acc hd) tl in
  let l = List.tl l |> List.map int_of_string in
  f (List.hd l) (List.tl l)

let res = data |> List.fold_left (fun acc l -> acc + compute_column l) 0
let () = Printf.printf "%d\n" res
