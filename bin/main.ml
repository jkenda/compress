open Huffman

let bytes_for_bit a = (a + 7) / 8

(* read file with filename and return string *)
let read_whole_file filename =
    (* open_in_bin works correctly on Unix and Windows *)
    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s |> String.to_bytes

(* write whole string to the file with filename *)
let write_whole_file filename bytes =
    let ch = open_out_bin filename in
    output_bytes ch bytes;
    close_out ch

let code_to_bytes code =
    let nbytes = bytes_for_bit (Bitv.length code) in
    let bytes = Bytes.make nbytes (Char.chr 0) in
    let set_bit i bit =
        let bit = if bit then 1 lsl (i mod 8) else 0 in
        let byte_i = nbytes - 1 - (i / 8) in
        let byte = Bytes.get bytes byte_i in
        let byte = Char.chr ((Char.code byte) lor bit) in
        Bytes.set bytes byte_i byte
    in
    Bitv.iteri set_bit code;
    bytes

(*
   encoding: [
       <huffman dict>
       <NULL char>
       <encoded text>
   ]

   <huffman dict>: [
       (char, n_bits, bits)
   ] where char = 8b, n_bits = 8b, bits = 32b
 *)

let bytes_to_huffman_encoded bytes =
    let dict, (bytes, len) = encode bytes in
    let buffer = Buffer.create len in
    (* add huffman dict *)
    List.iter (fun (ch, code) -> 
        Buffer.add_char buffer ch;
        Buffer.add_uint8 buffer (Bitv.length code);
        Buffer.add_bytes buffer (code_to_bytes code)) dict;
    (* add a NULL char as a separator *)
    Buffer.add_char buffer '\x00';
    (* add length in bits *)
    Buffer.add_int32_be buffer (Int32.of_int len);
    (* add encoded text *)
    Buffer.add_bytes buffer bytes;
    Buffer.to_bytes buffer

let huffman_encoded_to_bytes bytes =
    (* get int from a sequence of bytes *)
    let get_int i len =
        let rec aux acc = function
            | n when n = len -> i + len, acc
            | n -> aux (acc lsl 8 lor Char.code (Bytes.get bytes (i + n))) (n + 1)
        in aux 0 0
    in
    (* build dict from sequence of bytes until '\x00' is hit *)
    let rec build_dict dict i =
        match Bytes.get bytes i with
        | '\x00' -> i + 1, dict
        | char ->
                let len = Bytes.get bytes (i + 1) |> Char.code in
                let i, code = get_int (i + 2) (bytes_for_bit len) in
                let _ = Format.printf "char = '%s', len = %d, num = %d\n" (Char.escaped char) len code in
                let code = int_to_bitv code len in
                build_dict ((char, code) :: dict) i
    in

    let i, dict = build_dict [] 0 in
    let i, len = get_int i (32 / 8) in
    let dict = List.rev dict in
    let _ = Format.printf "ločilo: '%s', length: %d\n" ((Bytes.get bytes (i - 1)) |> Char.escaped) len in

    let bytes = Bytes.sub bytes i (Bytes.length bytes - i) in
    decode (dict, (bytes, len))

type mode =
    | Encode
    | Decode

let () =
    if Array.length Sys.argv < 4 then
        raise (Failure "Usage: ./main <encode|decode> <infile> <outfile>")
    else
        let mode =
            match Sys.argv.(1) with
            | "encode" -> Encode
            | "decode" -> Decode
            | _ -> raise (Failure "Usage: ./main <encode|decode> <infile> <outfile>")
        in

        let infile = Sys.argv.(2) in
        let outfile = Sys.argv.(3) in
        let input = read_whole_file infile in

        let output =
            match mode with
            | Encode -> bytes_to_huffman_encoded input
            | Decode -> huffman_encoded_to_bytes input
        in
        
        output
            |> write_whole_file outfile

