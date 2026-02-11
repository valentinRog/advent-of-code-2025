let raw =
  In_channel.input_all stdin |> String.trim |> String.to_seq
  |> Seq.filter (fun c -> c != '\r')
  |> String.of_seq

let data =
  raw |> String.split_on_char ','
  |> List.map (fun s ->
         let l =
           s |> String.split_on_char '-' |> List.map (fun s -> int_of_string s)
         in
         (List.hd l, List.nth l 1))

let () =
  let is_invalid n =
    let s = Int.to_string n in
    let len = String.length s in
    if len mod 2 == 0 then
      let s1 = String.sub s 0 (len / 2) in
      let s2 = String.sub s (len / 2) (len / 2) in
      String.equal s1 s2
    else false
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
