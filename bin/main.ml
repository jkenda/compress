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

(*
   encoding: [
       <huffman dict>
       <NULL char>
       <encoded text>
   ]

   <huffman dict>: [
       (char, n_bits, bits)
   ] where char = 1B, n_bits = 1B, bits = 1-32B
 *)

let bytes_to_huffman_encoded bytes =
    (* convert huffman code bitv to bytes *)
    let code_to_bytes code =
        let nbytes = bytes_for_bit (Bitv.length code) in
        let bytes = Bitv.to_bytes code in
        Bytes.sub bytes (Bytes.length bytes - nbytes) nbytes
    in

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
    let bytes_to_code i len =
        let int_size = bytes_for_bit Sys.int_size in
        let buffer = Buffer.create (int_size + bytes_for_bit len) in
        if int_size == 8 then
            Buffer.add_int64_ne buffer (Int64.of_int len)
        else (
            Buffer.add_int32_ne buffer (Int32.of_int len)
        );
        Buffer.add_bytes buffer (Bytes.sub bytes i len); 
        i + bytes_for_bit len, Buffer.to_bytes buffer |> Bitv.of_bytes
    in
    (* build dict from sequence of bytes until '\x00' is hit *)
    let rec build_dict dict i =
        match Bytes.get bytes i with
        | '\x00' -> i + 1, dict
        | char ->
                let len = Bytes.get_uint8 bytes (i + 1) in
                let i, code = bytes_to_code (i + 2) len in
                build_dict ((char, code) :: dict) i
    in

    let i, dict = build_dict [] 0 in
    let i, len = i + 4, Bytes.get_int32_be bytes i |> Int32.to_int in

    let dict = List.rev dict in
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

