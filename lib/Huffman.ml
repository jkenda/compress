open Tools
let time = time

let bitv_of_string str =
    Bitv.init (String.length str) (fun i -> str.[i] = '1')

let string_of_bitv bitv =
    String.init (Bitv.length bitv) (fun i -> if Bitv.unsafe_get bitv i then '1' else '0')

let bitv_of_int num len =
    Bitv.init len (fun i -> (num lsr (len - 1 - i)) land 1 = 1)

type 'a node =
    | Leaf of int * 'a
    | Node of int * 'a node * 'a node

let freq = function
    | Leaf (freq, _)
    | Node (freq, _, _) -> freq

(* transform (char, freq) list to a (char, huffman code) list *)
let huffman freqs =
    (* build huffman tree *)
    let rec build_tree list =
        (* make node from first two nodes in the list *)
        let combine = function
            | a :: b :: tl -> Node (freq a + freq b, a, b), tl
            | _ -> raise (Unreachable "always at least 2 nodes")
        in
        (* insert node at the appropriate position *)
        let insert (node, list) =
            let rec aux = function
                | [] -> [node]
                | hd :: _ as ls when freq node < freq hd -> node :: ls
                | hd :: tl -> hd :: aux tl
            in aux list
        in

        (* keep building the tree until only one node is left (the root) *)
        match list with
        | [] -> None
        | [root] -> Some root
        | _ -> list
                |> combine
                |> insert
                |> build_tree
    in
    (* transform tree to list of huffman codes *)
    let to_huffman tree =
        let rec aux len code = function
            | Leaf (_, char) -> [char, bitv_of_int code len]
            | Node (_, lc, rc) -> aux (len + 1) (2 * code + 0) lc
                                @ aux (len + 1) (2 * code + 1) rc
        in
        match tree with
        | None -> []
        | Some Leaf (_, char) -> [char, bitv_of_int 0 1]
        | Some tree -> aux 0 0 tree
    in

    freqs
        |> List.sort (fun (_, f1) (_, f2) -> f1 - f2)
        |> List.map (fun (char, freq) -> Leaf (freq, char))
        |> build_tree
        |> to_huffman

(* Encode mesage with huffman *)
let encode message =
    (* calculate frequencies of characters *)
    let freqs string =
        let array = Array.make 256 0 in
        Bytes.iter (fun c -> array.(Char.code c) <- array.(Char.code c) + 1) string;
        let rec aux = function
            | 256 -> []
            | n -> if array.(n) > 0 then (Char.chr n, array.(n)) :: aux (n + 1) else aux (n + 1)
        in
        aux 0
    in
    (* buffer for building strings *)
    let out_buffer = Bitv.create (Bytes.length message * 8) false in
    let i = ref 0 in
    (* encode mesage with huffman *)
    let to_code dict =
        let add_code char =
            let rec find_code = function
                | (ch, code) :: _ when ch = char ->
                        let len = Bitv.length code in
                        Bitv.blit code 0 out_buffer !i len;
                        i := !i + len
                | _ :: tl -> find_code tl
                | [] -> raise (Failure "code for char always exists")
            in
            find_code dict
        in
        Bytes.iter add_code message;
        (Bitv.sub out_buffer 0 !i |> Bitv.to_bytes), !i
    in

    let dict = message
                |> time "encode.freqs" freqs
                |> time "encode.huffman" huffman
                |> time "encode.sort" List.sort (fun (_, c1) (_, c2) -> Bitv.length c1 - Bitv.length c2)
    in
    let (bytes, _) as encoded = time "encode.to_code" to_code dict in
    Format.printf "raw size: %d B, compressed size: %d B, compression ratio: %f\n"
        (Bytes.length message)
        (Bytes.length bytes)
        (Float.of_int (Bytes.length message) /. Float.of_int (Bytes.length bytes));
    (dict, encoded)

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
            if j >= Bitv.length code then true
            else if i >= Bitv.length in_buffer then false
            else
                if Bitv.unsafe_get in_buffer i = Bitv.unsafe_get code j then
                    starts_with code (i + 1)
                else
                    false
        in
        (* find char that matches the code *)
        let rec find_char dict start =
            match dict with
            | (char, code) :: _ when starts_with code start -> char, Bitv.length code
            | _ :: tl -> find_char tl start
            | _ -> raise (Failure "unreachable: there is always a match")
        in

        if start < len then
            let char, len = find_char dict start in 
            Buffer.add_char out_buffer char;
            code_to_string (start + len)
    in

    time "decode.code_to_string" code_to_string 0;
    Buffer.to_bytes out_buffer

