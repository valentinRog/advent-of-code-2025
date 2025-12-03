let get_raw =
  In_channel.input_all stdin |> String.trim |> String.to_seq
  |> Seq.filter (fun c -> c != '\r')
  |> String.of_seq

let () =
  let target_len = 12 in
  let rec compute s = function
    | acc when String.length acc == target_len -> acc |> int_of_string
    | acc ->
        let n =
          s
          |> Seq.take (Seq.length s - (target_len - 1 - String.length acc))
          |> Seq.fold_left (fun e acc -> max acc e) 0
        in
        let s = s |> Seq.drop_while (fun e -> e != n) |> Seq.drop 1 in
        compute s (acc ^ string_of_int n)
  in
  let res =
    get_raw |> String.split_on_char '\n'
    |> List.map (fun s ->
           let s =
             s |> String.to_seq
             |> Seq.map (fun c -> int_of_char c - int_of_char '0')
           in
           compute s "")
    |> List.fold_left (fun n acc -> acc + n) 0
  in
  Printf.printf "%d\n" res
