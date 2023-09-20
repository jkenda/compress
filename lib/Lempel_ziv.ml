type seq = {
    offset : int;
    len : int
}

type lz =
    | Char of char
    | Seq of seq

let rec ( ^ ) a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = a^(n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

let lempel_ziv string =
    let rec lz' i acc =
        (* find the longest matching sequence starting at i1 and i2 *)
        let seq_len i1 i2 =
            let rec seq_len' len =
                if i1 + len >= String.length string
                || string.[i1 + len] != string.[i2 + len] then
                    len
                else
                    seq_len' (len + 1)
            in
            seq_len' 0
        in
        (* find a match for the sequence starting at j *)
        let rec next_seq j ({ offset = _; len = best_len } as best) =
            if j < 0 || i - j >= 2^16 then best
            else
                let len = seq_len i j in
                next_seq (j - 1) @@
                    if len > best_len then { offset = j - i; len }
                    else best
        in

        if i >= String.length string then List.rev acc
        else
            match next_seq (i - 1) { offset = 0; len = 1 } with
            | { offset = _; len = 1 } -> lz' (i + 1) (Char string.[i] :: acc)
            | seq -> lz' (i + seq.len) (Seq seq :: acc)
    in
    lz' 0 []

let%test "empty" = lempel_ziv "" = []
let%test "a" = lempel_ziv "a" = [Char 'a']
let%test "aa" = lempel_ziv "aa" = [Char 'a'; Char 'a']
let%test "aba" = lempel_ziv "aba" = [Char 'a'; Char 'b'; Char 'a']
let%test "abab" = lempel_ziv "abab" = [Char 'a'; Char 'b'; Seq { offset = -2; len = 2 }]
let%test "abba" =
    lempel_ziv "abba"
    = [Char 'a'; Char 'b'; Char 'b'; Char 'a']

