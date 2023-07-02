type 'a node =
    | Leaf of int * 'a
    | Node of int * 'a node * 'a node

let freq = function
    | Leaf (fr, _)
    | Node (fr, _, _) -> fr

let huffman freqs =
    (* sort list of (char, freq) in ascending order *)
    let sort =
        List.sort
        (fun (_, f1) (_, f2) -> f1 - f2)
    in
    (* transform list of (char, freq) tuples to list of nodes *)
    let rec make_nodes = function
        | [] -> []
        | (ch, fr) :: tl -> Leaf (fr, ch) :: make_nodes tl
    in
    (* build tree *)
    let rec build_tree list =
        (* make node from first two nodes in the list *)
        let combine = function
            | a :: b :: tl -> Node (freq a + freq b, a, b), tl
            | _ -> raise (Failure "unreachable: always at least 2 nodes")
        in
        (* insert node at the appropriate position *)
        let rec insert (node, list) =
            match list with
            | [] -> [node]
            | hd :: _ as ls when freq node < freq hd -> node :: ls
            | hd :: tl -> hd :: insert (node, tl)
        in

        if List.length list = 1 then List.hd list
        else
            list
                |> combine
                |> insert
                |> build_tree
    in
    (* transform tree to list of huffman codes *)
    let to_huffman nodes =
        let rec aux code = function
            | Leaf (_, ch) -> [(ch, code)]
            | Node (_, lc, rc) -> aux (code ^ "0") lc @ aux (code ^ "1") rc
        in
        aux "" nodes
    in

    freqs
        |> sort
        |> make_nodes
        |> build_tree
        |> to_huffman

(* Encode mesage with huffman *)
let encode message =
    (* calculate frequencies of characters *)
    let freqs string =
        let insert list char =
            let rec aux = function
                | [] -> [(char, ref 1)]
                | (ch, cnt) :: _ as ls when ch = char -> cnt := !cnt + 1; ls
                | hd :: tl -> hd :: aux tl
            in
            aux list
        in
        Bytes.fold_left insert [] string
        |> List.map (fun (ch, cnt) -> (ch, !cnt))
    in
    (* put most common characters first *)
    let sort =
        List.sort
        (fun (_, c1) (_, c2) -> String.length c1 - String.length c2)
    in
    (* buffer for building strings *)
    let out_buffer = Bitv.create (Bytes.length message * 8) false in
    let i = ref 0 in
    (* encode mesage with huffman *)
    let to_code huffman =
        let add_code char =
            let add_bit c =
                Bitv.unsafe_set out_buffer !i (c = '1');
                i := !i + 1 
            in
            let rec aux = function
                | (ch, code) :: _ when ch = char -> String.iter add_bit code
                | _ :: tl -> aux tl
                | [] -> raise (Failure "unreachable: code for char always exists")
            in
            aux huffman
        in
        Bytes.iter add_code message;
        (Bitv.sub out_buffer 0 !i |> Bitv.to_bytes), !i
    in

    let huffman = message
                |> freqs
                |> huffman
                |> sort
    in
    (huffman, to_code huffman)

(* decode huffman encoded message *)
let decode (dict, (bytes, len)) =
    (* buffer for building strings *)
    let in_buffer  = Bitv.of_bytes bytes in
    let out_buffer = Buffer.create len in
    (* build string from code *)
    (* find a code that matches the front of the buffer, then
        - add the corresponding char to the out buffer,
        - clear the bit buffer *)
    let rec code_to_string start =
        (* check if the in_buffer[start..] starts with the code *)
        let rec starts_with code i = 
            let j = i - start in
            if j >= String.length code then true
            else
            if i >= Bitv.length in_buffer then false
            else
                match Bitv.get in_buffer i, String.get code j with
                | false, '0' | true, '1' -> starts_with code (i + 1)
                | _ -> false
        in
        (* find char that matches the code *)
        let rec find_char dict start =
            match dict with
            | (char, code) :: _ when starts_with code start -> char, String.length code
            | _ :: tl -> find_char tl start
            | _ -> raise (Failure "unreachable: there is always a match")
        in

        if start < len then
            let char, len = find_char dict start in 
            Buffer.add_char out_buffer char;
            code_to_string (start + len)
    in

    code_to_string 0;
    Buffer.contents out_buffer

