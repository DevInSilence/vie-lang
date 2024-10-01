module UTF8 = Camomile.UTF8

let uchar_to_string (u : Camomile.UChar.t) = Camomile.UTF8.init 1 (fun _ -> u)

let utf8_sub source start_idx len =
  let rec collect_chars idx acc count =
    if count = len then String.concat "" (List.rev acc)
    else
      let char = UTF8.get source idx in
      let next_idx = UTF8.next source idx in
      collect_chars next_idx (uchar_to_string char :: acc) (count + 1)
  in
  collect_chars start_idx [] 0
