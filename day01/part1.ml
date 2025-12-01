let get_raw =
  In_channel.input_all stdin |> String.trim |> String.to_seq
  |> Seq.filter (fun c -> c != '\r')
  |> String.of_seq

let () =
  let data =
    get_raw |> String.split_on_char '\n'
    |> List.map (fun s ->
           (s.[0], String.sub s 1 (String.length s - 1) |> int_of_string))
  in
  let rec f n acc = function
    | (d, nn) :: tl ->
        let nn = match d with 'L' -> -nn | 'R' -> nn | _ -> assert false in
        let n = n + nn in
        let acc = if n mod 100 == 0 then acc + 1 else acc in
        f n acc tl
    | [] -> acc
  in
  let n = f 50 0 data in
  Printf.printf "%d\n" n
