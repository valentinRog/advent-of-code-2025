let raw =
  In_channel.input_all stdin |> String.trim |> String.to_seq
  |> Seq.filter (fun c -> c != '\r')
  |> String.of_seq

let remove_all charset s =
  s |> String.to_seq
  |> Seq.filter (fun c -> not (String.contains charset c))
  |> String.of_seq

module Queue = struct
  type 'a t = { front : 'a list; back : 'a list }

  let empty = { front = []; back = [] }
  let push e { front; back } = { front; back = e :: back }

  let rec pop { front; back } =
    match front with
    | e :: tl -> (e, { front = tl; back })
    | [] ->
        assert (not (List.is_empty back));
        pop { front = back |> List.rev; back = [] }
end

module IntSet = Set.Make (Int)

type light = IntSet.t
type button = int list

module IntSetSet = Set.Make (IntSet)

module Problem = struct
  type t = { light : light; buttons : button list }

  let parse line =
    let a =
      line |> remove_all "[]()" |> String.split_on_char ' ' |> List.rev
      |> List.to_seq |> Seq.drop 1 |> List.of_seq |> List.rev
    in
    let light =
      List.hd a |> String.to_seq
      |> Seq.fold_lefti
           (fun acc i c -> match c with '#' -> acc |> IntSet.add i | _ -> acc)
           IntSet.empty
    in
    let parse_button s =
      s |> String.split_on_char ',' |> List.map int_of_string
    in
    let buttons : button list = a |> List.tl |> List.map parse_button in
    { light; buttons }

  let apply_button button light =
    button
    |> List.fold_left
         (fun acc n ->
           if IntSet.mem n light then IntSet.remove n acc else IntSet.add n acc)
         light

  let bfs { light = target; buttons } =
    let rec f q seen =
      let (light, n), q = Queue.pop q in
      if IntSet.equal light target then n
      else
        let q, seen =
          buttons
          |> List.fold_left
               (fun (acc_q, acc_seen) button ->
                 let new_light = apply_button button light in
                 if IntSetSet.mem new_light acc_seen then (acc_q, acc_seen)
                 else
                   ( Queue.push (new_light, n + 1) acc_q,
                     IntSetSet.add new_light acc_seen ))
               (q, seen)
        in
        f q seen
    in
    let initial_seen = IntSetSet.singleton IntSet.empty in
    f (Queue.empty |> Queue.push (IntSet.empty, 0)) initial_seen
end

let res =
  raw |> String.split_on_char '\n' |> List.map Problem.parse
  |> List.map Problem.bfs
  |> List.fold_left (fun acc n -> acc + n) 0

let () = Printf.printf "%d\n" res
