let string_of_char char =
    String.make 1 char

let byte_limit = 256

let make_dict () =
    let open Hashtbl in
    let dict = create byte_limit in
    for i = 0 to byte_limit - 1 do
        add dict (String.make 1 (Char.chr i)) i
    done;
    add dict "č" (length dict);
    add dict "š" (length dict);
    add dict "ž" (length dict);
    add dict "Č" (length dict);
    add dict "Š" (length dict);
    add dict "Ž" (length dict);

    add dict "th" (length dict);
    add dict "he" (length dict);
    add dict "in" (length dict);
    add dict "er" (length dict);

    dict

let make_dict' () =
    let open Hashtbl in
    let dict = create byte_limit in
    make_dict ()
    |> iter (fun seq code -> add dict code seq);
    dict

let compress input =
    let dict = make_dict () in

    let compress' (w, output) c =
        let cs = string_of_char c in
        let wc = w ^ cs in
        
        if Hashtbl.mem dict wc then
            wc, output
        else (
            Hashtbl.add dict wc (Hashtbl.length dict);
            cs, (Hashtbl.find dict w :: output))
    in
    
    let compressed =
        let w, output = String.fold_left compress' ("", []) input in
            List.rev @@
                if w = "" then output
                else Hashtbl.find dict w :: output
    in
    compressed, Hashtbl.length dict


let test_encode input expected =
    let rec print = function
        | [] -> ()
        | id :: tl ->
                (if id < byte_limit then
                    print_char @@ Char.chr id
                else
                    Format.printf "[%d]" id);
                print tl
    in
    let (output, _) = compress input in
    let pass = output = expected in
    if not pass then (print output; Format.printf "\n");
    pass

let%test _ = test_encode ""         []
let%test _ = test_encode "a"        [Char.code 'a']
let%test _ = test_encode "aa"       [Char.code 'a'; Char.code 'a']
let%test _ = test_encode "aba"      [Char.code 'a'; Char.code 'b'; Char.code 'a']
let%test _ = test_encode "abab"     [Char.code 'a'; Char.code 'b'; 256]
let%test _ = test_encode "abba"     [Char.code 'a'; Char.code 'b'; Char.code 'b'; Char.code 'a']

let%test _ = test_encode "až"
(String.to_seq "až" |> List.of_seq |> List.map (fun x -> Char.code x))

let%test _ = test_encode "AABBA" 
[Char.code 'A'; Char.code 'A'; Char.code 'B'; Char.code 'B'; Char.code 'A']

let%test _ = test_encode "AABBAABB"
[Char.code 'A'; Char.code 'A'; Char.code 'B'; Char.code 'B'; 256; 258]



let decompress input =
    let dict = make_dict' () in

    let decompress' (w, output) code =
        let entry =
            match Hashtbl.find_opt dict code with
            | Some str -> str
            | None when code = Hashtbl.length dict ->
                        w ^ string_of_char w.[0]
            | _ -> raise @@ Failure ("Bad compression: " ^ string_of_int code)
        in
        let code = Hashtbl.length dict in
        Hashtbl.add dict code (w ^ (string_of_char entry.[0]));
        entry, output ^ entry
    in
    match input with
    | [] -> ""
    | hd :: tl ->
            let w = string_of_char @@ Char.chr hd in
            let _, output = List.fold_left decompress' (w, w) tl in
            output

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
(String.to_seq "až" |> List.of_seq |> List.map (fun x -> Char.code x))
"až"

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
let%test _ = test_both "až"
let%test _ = test_both "0 = ∞; ∞ = 0"

open Tools

let%test _ =
    let (compressed, _) = compress lorem_ipsum in
    decompress compressed = lorem_ipsum

