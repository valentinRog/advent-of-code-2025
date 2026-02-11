let raw =
  In_channel.input_all stdin |> String.trim |> String.to_seq
  |> Seq.filter (fun c -> c != '\r')
  |> String.of_seq

module IntSet = Set.Make (Int)

let data =
  let lines = raw |> String.split_on_char '\n' in
  let len = List.length lines in
  let symboles_line = List.nth lines (len - 1) in
  let symboles_indexes =
    symboles_line |> String.to_seqi
    |> Seq.filter (fun (_, c) -> c = '*' || c = '+')
    |> Seq.map (fun (i, _) -> i)
    |> IntSet.of_seq
  in
  let format_line line =
    line |> String.to_seqi
    |> Seq.map (fun (i, c) ->
           match (i, c) with
           | i, ' ' when not (IntSet.mem (i + 1) symboles_indexes) -> '0'
           | _ -> c)
    |> String.of_seq
  in
  let lines =
    let lines =
      lines |> List.to_seq
      |> Seq.take (len - 1)
      |> Seq.map format_line |> List.of_seq
    in
    List.append lines [ symboles_line ]
  in
  let rec f acc = function
    | [] -> acc
    | hd :: tl ->
        f (List.combine hd acc |> List.map (fun (hd, tl) -> hd :: tl)) tl
  in
  let l =
    let is_not_empty s = not (String.equal s "") in
    lines
    |> List.map (fun s ->
           s |> String.split_on_char ' ' |> List.filter is_not_empty)
  in
  f (Seq.repeat [] |> Seq.take (l |> List.hd |> List.length) |> List.of_seq) l

let parse_numbers l =
  let rec parse_last_digit acc = function
    | [] -> int_of_string acc
    | hd :: tl -> (
        match hd mod 10 with
        | 0 -> parse_last_digit acc tl
        | n -> parse_last_digit (acc ^ string_of_int n) tl)
  in
  let rec f acc l =
    if List.exists (fun n -> n != 0) l then
      f (parse_last_digit "" l :: acc) (l |> List.map (fun n -> n / 10))
    else acc
  in
  f [] l |> List.rev

let compute_column l =
  let op =
    match String.get (List.hd l) 0 with
    | '*' -> fun n1 n2 -> n1 * n2
    | '+' -> fun n1 n2 -> n1 + n2
    | _ -> assert false
  in
  let l = l |> List.tl |> List.rev |> List.map int_of_string |> parse_numbers in
  let rec f acc = function [] -> acc | hd :: tl -> f (op acc hd) tl in
  f (List.hd l) (List.tl l)

let res = data |> List.fold_left (fun acc l -> acc + compute_column l) 0
let () = Printf.printf "%d\n" res
