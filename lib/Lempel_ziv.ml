let string_of_char char =
    String.make 1 char

let compress input =
    let dict = Hashtbl.create 256 in
    for i = 0 to 255 do
        Hashtbl.add dict (String.make 1 (Char.chr i)) i
    done;

    let rec compress' i w output =
        if i >= String.length input then
            List.rev @@
                if w = "" then output
                else Hashtbl.find dict w :: output
        else
            let c = string_of_char input.[i] in
            let wc = w ^ c in
            
            if Hashtbl.mem dict wc then
                compress' (i + 1) wc output
            else (
                Hashtbl.add dict wc (Hashtbl.length dict);
                compress' (i + 1) c (Hashtbl.find dict w :: output))
    in
    let compressed = compress' 0 "" [] in
    (compressed, dict)


let test_encode input expected =
    let rec print = function
        | [] -> ()
        | id :: tl ->
                (if id < 256 then
                    print_char @@ Char.chr id
                else
                    Format.printf "[%d]" id);
                print tl
    in
    let (output, _) = compress input in
    let pass = output = expected in
    if not pass then (print output; print_newline ());
    pass

let%test _ = test_encode ""         []
let%test _ = test_encode "a"        [Char.code 'a']
let%test _ = test_encode "aa"       [Char.code 'a'; Char.code 'a']
let%test _ = test_encode "aba"      [Char.code 'a'; Char.code 'b'; Char.code 'a']
let%test _ = test_encode "abab"     [Char.code 'a'; Char.code 'b'; 256]
let%test _ = test_encode "abba"     [Char.code 'a'; Char.code 'b'; Char.code 'b'; Char.code 'a']

let%test _ = test_encode "AABBA" 
[Char.code 'A'; Char.code 'A'; Char.code 'B'; Char.code 'B'; Char.code 'A']

let%test _ = test_encode "AABBAABB"
[Char.code 'A'; Char.code 'A'; Char.code 'B'; Char.code 'B'; 256; 258]



let decompress input =
    let dict = Hashtbl.create 256 in
    for i = 0 to 255 do
        Hashtbl.add dict i (string_of_char (Char.chr i))
    done;

    let rec decompress' w output = function
        | [] -> output
        | id :: tl ->
                let entry =
                    match Hashtbl.find_opt dict id with
                    | Some str -> str
                    | None when id = Hashtbl.length dict ->
                                w ^ (string_of_char w.[0])
                    | _ -> raise (Failure "bad compression")
                in
                Hashtbl.add dict (Hashtbl.length dict) (w ^ (string_of_char entry.[0]));

                decompress' entry (output ^ entry) tl
    in
    match input with
    | [] -> ""
    | hd :: tl ->
            let w = string_of_char @@ Char.chr @@ hd in
            decompress' w w tl

let test_decode input expected =
    let output = decompress input in
    let pass = output = expected in
    if not pass then print_endline output;
    pass

let%test _ = test_decode []                                                           ""
let%test _ = test_decode [Char.code 'a']                                              "a"
let%test _ = test_decode [Char.code 'a'; Char.code 'a']                               "aa"
let%test _ = test_decode [Char.code 'a'; Char.code 'b'; Char.code 'a']                "aba"
let%test _ = test_decode [Char.code 'a'; Char.code 'b'; 256]                          "abab"
let%test _ = test_decode [Char.code 'a'; Char.code 'b'; Char.code 'b'; Char.code 'a'] "abba"
let%test _ = test_decode 
[Char.code 'A'; Char.code 'A'; Char.code 'B'; Char.code 'B'; Char.code 'A']
"AABBA"

let test_both expected =
    let (output, _) = compress expected in
    let input = decompress output in
    let pass = input = expected in
    if not pass then print_endline input;
    pass


let%test _ = test_both ""
let%test _ = test_both "a"
let%test _ = test_both "aa"
let%test _ = test_both "aba"
let%test _ = test_both "abab"
let%test _ = test_both "abba"
let%test _ = test_both "AABBA"
let%test _ = test_both "AABBAABBA"

open Tools

let%test _ =
    let (compressed, _) = compress lorem_ipsum in
    decompress compressed = lorem_ipsum

