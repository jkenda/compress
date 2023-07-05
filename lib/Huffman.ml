open Tools

type 'a node =
    | Leaf of int * 'a
    | Node of int * 'a node * 'a node

let freq = function
    | Leaf (fr, _)
    | Node (fr, _, _) -> fr

let bitv_of_int num len =
    Bitv.init len (fun i -> (num lsr (len - 1 - i)) land 1 = 1)

let bitv_of_string str =
    Bitv.init (String.length str) (fun i -> str.[i] = '1')

let string_of_bitv bitv =
    String.init (Bitv.length bitv) (fun i -> if Bitv.get bitv i then '1' else '0')

let bitv_to_int bitv =
    let len = Bitv.length bitv in
    let rec aux acc = function
        | i when i >= len -> acc
        | i -> aux (acc lor ((if Bitv.get bitv i then 1 else 0) lsl i)) (i + 1)
    in
    aux 0 0 

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
        let rec aux len code = function
            | Leaf (_, ch) -> [ch, bitv_of_int code len]
            | Node (_, lc, rc) -> aux (len + 1) (2 * code + 0) lc @ aux (len + 1) (2 * code + 1) rc
        in
        aux 0 0 nodes
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
        let array = Array.make 256 0 in
        let _ = Bytes.iter (fun c -> array.(Char.code c) <- array.(Char.code c) + 1) string in
        let rec aux acc = function
            | -1 -> acc
            | n -> aux (if array.(n) > 0 then (Char.chr n, array.(n)) :: acc else acc) (n - 1)
        in
        aux [] 255
    in
    (* put most common characters first *)
    let sort =
        List.sort
        (fun (_, c1) (_, c2) -> Bitv.length c1 - Bitv.length c2)
    in
    (* buffer for building strings *)
    let out_buffer = Bitv.create (Bytes.length message * 8) false in
    let i = ref 0 in
    (* encode mesage with huffman *)
    let to_code huffman =
        let add_code char =
            let rec aux = function
                | (ch, code) :: _ when ch = char ->
                        let len = Bitv.length code in
                        Bitv.blit code 0 out_buffer !i len;
                        i := !i + len
                | _ :: tl -> aux tl
                | [] -> raise (Failure "code for char always exists")
            in
            aux huffman
        in
        Bytes.iter add_code message;
        (Bitv.sub out_buffer 0 !i |> Bitv.to_bytes), !i
    in

    let huffman = message
                |> time "encode.freqs" freqs
                |> time "encode.huffman" huffman
                |> time "encode.sort" sort
    in
    (huffman, time "encode.to_code" to_code huffman)

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
                if Bitv.get in_buffer i = Bitv.get code j then
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

    code_to_string 0;
    Buffer.to_bytes out_buffer

