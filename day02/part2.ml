let get_raw =
  In_channel.input_all stdin |> String.trim |> String.to_seq
  |> Seq.filter (fun c -> c != '\r')
  |> String.of_seq

let data =
  get_raw |> String.split_on_char ','
  |> List.map (fun s ->
         let l =
           s |> String.split_on_char '-' |> List.map (fun s -> int_of_string s)
         in
         (List.hd l, List.nth l 1))

let () =
  let is_invalid n =
    let rec is_only_pattern pattern =
      let len = String.length pattern in
      function
      | "" -> true
      | s when String.length s < len -> false
      | s when not (String.equal (String.sub s 0 len) pattern) -> false
      | s -> is_only_pattern pattern (String.sub s len (String.length s - len))
    in
    let s = Int.to_string n in
    let len = String.length s in
    let rec f = function
      | i when i > len / 2 -> false
      | i when is_only_pattern (String.sub s 0 i) s -> true
      | i -> f (i + 1)
    in
    f 1
  in
  let rec compute acc = function
    | (n0, n1) :: tl ->
        let acc =
          Seq.ints n0
          |> Seq.take_while (fun n -> n <= n1)
          |> Seq.filter is_invalid
          |> Seq.fold_left (fun acc n -> acc + n) acc
        in
        compute acc tl
    | [] -> acc
  in
  Printf.printf "%d\n" (compute 0 data)
