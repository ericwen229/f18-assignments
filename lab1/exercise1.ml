open Core

exception Unimplemented

let main () =
  let rec gcd (m : int) (n : int) : int =
    match n with
    | 0 -> m
    | _ -> gcd n (m mod n)
  in

  assert (gcd 5 2 = 1);
  assert (gcd 10 2 = 2);
  assert (gcd 48 18 = 6);

  let fizz_buzz (n : int) : unit =
    let rec fizz_buzz_iter (i : int) : unit =
      if i < n
      then
        ((match (i mod 3 = 0, i mod 5 = 0) with
        | (true, true) -> Printf.printf "fizzbuzz\n"
        | (true, false) -> Printf.printf "fizz\n"
        | (false, true) -> Printf.printf "buzz\n"
        | _ -> ());
        fizz_buzz_iter (i + 1))
      else ()
    in fizz_buzz_iter 0
  in

  fizz_buzz 20;

  let read_line () : string =
    match In_channel.input_line In_channel.stdin with
    | Some s -> s
    | None -> assert false
  in

  let rec read_password (password : string) : unit =
    if read_line () = password
    then ()
    else read_password password
  in

  let substring_match (pattern : string) (source : string) : int option =
    let pattern_length = String.length pattern in
    let source_length = String.length source in
    let rec match_iter (i : int) : int option =
      if (i + pattern_length) > source_length
      then None
      else if pattern = String.slice source i (i + pattern_length)
      then Some i
      else match_iter (i + 1)
    in match_iter 0
  in

  assert (substring_match "foo" "foobar" = Some 0);
  assert (substring_match "foo" "barfoo" = Some 3);
  assert (substring_match "z" "foobar" = None);

  ()

let () = main ()
