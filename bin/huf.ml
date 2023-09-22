open Huffman
open Tools

let bytes_for_bit a = (a + 7) / 8

(* read file with filename and return string *)
let read_whole_file filename =
    (* open_in_bin works correctly on Unix and Windows *)
    let ch = open_in_bin filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    String.to_bytes s

(* write whole string to the file with filename *)
let write_whole_file filename bytes =
    let ch = open_out_bin filename in
    output_bytes ch bytes;
    close_out ch

(*
   encoding: (
       <huffman dict>
       <encoded text>
   )

   <huffman dict>: (
       <num>
       [(char, n_bits, bits); num]
   ) where char = 1B, n_bits = 1B, bits = 1-32B
 *)

let bytes_to_huffman_encoded bytes =
    (* convert huffman code bitv to bytes *)
    let code_to_bytes code =
        let nbytes = bytes_for_bit (Bitv.length code) in
        let bytes = Bitv.to_bytes code in
        Bytes.sub bytes (Bytes.length bytes - nbytes) nbytes
    in
    (* calculate frequencies of characters *)
    let freqs = freqs bytes in

    let dict, (bytes, len) = compress Bytes.length Bytes.iter 1 freqs bytes in
    let buffer = Buffer.create (len * 2) in

    (* add size of dict *)
    Buffer.add_uint8 buffer (List.length dict - 1);
    (* add huffman dict *)
    List.iter (fun (ch, code) -> 
        Buffer.add_char buffer ch;
        Buffer.add_uint8 buffer (Bitv.length code);
        Buffer.add_bytes buffer (code_to_bytes code)) dict;
    (* add length in bits *)
    Buffer.add_int32_be buffer (Int32.of_int len);
    (* add encoded text *)
    Buffer.add_bytes buffer bytes;
    Buffer.to_bytes buffer

let huffman_encoded_to_bytes bytes =
    (* get size of dict *)
    let dict_size = Bytes.get_uint8 bytes 0 + 1 in
    (* get code from sequence of bytes *)
    let bytes_to_code i len =
        let int_size = bytes_for_bit Sys.int_size in
        let buffer = Buffer.create (int_size + bytes_for_bit len) in
        (match int_size with
         | 8 -> Buffer.add_int64_ne buffer (Int64.of_int len)
         | 4 -> Buffer.add_int32_ne buffer (Int32.of_int len)
         | _ -> assert false);
        Buffer.add_subbytes buffer bytes i len; 
        i + bytes_for_bit len, Buffer.to_bytes buffer |> Bitv.of_bytes
    in
    (* build dict from sequence of bytes *)
    let build_dict i =
        let rec aux acc n i =
            if n = dict_size then i, acc
            else
                let char = Bytes.get bytes i in
                let len = Bytes.get_uint8 bytes (i + 1) in
                let i, code = bytes_to_code (i + 2) len in
                aux ((char, code) :: acc) (n + 1) i
        in
        let i, dict = aux [] 0 i in
        i, List.rev dict
    in

    let i, dict = time "decode.build_dict" build_dict 1 in
    let i, len = i + 4, Bytes.get_int32_be bytes i |> Int32.to_int in
    let bytes = Bytes.sub bytes i (Bytes.length bytes - i) in
    decompress (dict, (bytes, len))

type mode =
    | Encode
    | Decode

let () =
    let open Tools in
    if Array.length Sys.argv < 4 then
        raise (Failure "Usage: ./main <encode|decode> <infile> <outfile>")
    else
        let mode =
            match Sys.argv.(1) with
            | "compress" -> Encode
            | "decompress" -> Decode
            | _ -> raise (Failure "Usage: ./main <compress|decompress> <infile> <outfile>")
        in

        let infile = Sys.argv.(2) in
        let outfile = Sys.argv.(3) in
        let input = read_whole_file infile in

        let output =
            match mode with
            | Encode -> time "encode" bytes_to_huffman_encoded input
            | Decode -> time "decode" huffman_encoded_to_bytes input
        in
        
        output
            |> write_whole_file outfile;

        Format.printf "input size: %d B, output size: %d B, ratio: %f\n"
            (Bytes.length input)
            (Bytes.length output)
            (Float.of_int (Bytes.length input) /. Float.of_int (Bytes.length output));

