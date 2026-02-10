let raw =
  In_channel.input_all stdin |> String.trim |> String.to_seq
  |> Seq.filter (fun c -> c != '\r')
  |> String.of_seq

let split_on_word w s =
  let rec f res acc l =
    let remove_word =
      let rec f w_l l =
        match (w_l, l) with
        | [], _ -> Some l
        | w_hd :: w_tl, hd :: tl when w_hd == hd -> f w_tl tl
        | _ -> None
      in
      f (w |> String.to_seq |> List.of_seq) l
    in
    let push_acc () =
      (acc |> List.rev |> List.to_seq |> String.of_seq) :: res
    in
    match (l, acc, remove_word) with
    | [], [], _ -> res
    | [], _, _ -> push_acc ()
    | _, _, Some l -> f (push_acc ()) [] l
    | hd :: tl, _, None -> f res (hd :: acc) tl
  in
  f [] [] (s |> String.to_seq |> List.of_seq) |> List.rev

let l =
  split_on_word "\n\n" raw |> List.map (fun s -> String.split_on_char '\n' s)

let ranges =
  List.hd l
  |> List.map (fun s ->
         let l = s |> String.split_on_char '-' |> List.map Int64.of_string in
         (List.hd l, List.nth l 1))

let ids = List.nth l 1 |> List.map Int64.of_string
let is_valid id = ranges |> List.exists (fun (n0, n1) -> id >= n0 && id <= n1)
let res = ids |> List.to_seq |> Seq.filter is_valid |> Seq.length
let () = Printf.printf "%d\n" res
