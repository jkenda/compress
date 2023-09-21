open Lempel_ziv

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


let bytes_to_lz78_encoded bytes =
    let (compressed, dict) =
        bytes
        |> Bytes.to_string
        |> compress
    in
    let size = Hashtbl.length dict in
    let ent_size, f =
        if size <= 1 lsl 8 then 1, Buffer.add_uint8
        else if size <= 1 lsl 16 then 2, Buffer.add_uint16_be
        else if size <= 1 lsl 31 then 4, fun buf x -> x |> Int32.of_int |> Buffer.add_int32_be buf
        else 8, fun buf x -> x |> Int64.of_int |> Buffer.add_int64_be buf
    in
    let buffer = Buffer.create (size * ent_size) in

    (* add size of entry *)
    Buffer.add_uint8 buffer ent_size;
    (* add sequence *)
    List.iter (f buffer) compressed;
    (* output bytes *)
    Buffer.to_bytes buffer

let lz78_encoded_to_bytes bytes =
    (* get size of entry *)
    let ent_size = Bytes.get_uint8 bytes 0 in
    let f =
        match ent_size with
        | 1 -> Bytes.get_uint8
        | 2 -> Bytes.get_uint16_be
        | 4 -> fun buf x -> Bytes.get_int32_be buf x |> Int32.to_int
        | 8 -> fun buf x -> Bytes.get_int64_be buf x |> Int64.to_int
        | _ -> raise (Failure "bad format")
    in
    (* build dict from sequence of bytes *)
    let rec build_list i acc =
        if i >= Bytes.length bytes then List.rev acc
        else
            build_list (i + ent_size)
            @@ f bytes i :: acc
    in

    build_list 1 []
    |> decompress 
    |> Bytes.of_string


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
            | Encode -> time "lz78" bytes_to_lz78_encoded input
            | Decode -> time "decode" lz78_encoded_to_bytes input
        in
        
        output
            |> write_whole_file outfile;

        Format.printf "input size: %d B, output size: %d B, ratio: %f\n"
            (Bytes.length input)
            (Bytes.length output)
            (Float.of_int (Bytes.length input) /. Float.of_int (Bytes.length output));

