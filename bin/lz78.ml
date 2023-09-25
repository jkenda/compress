open Lempel_ziv
open Tools

let (<<) a b = a lsl b
let (>>) a b = a lsr b
let ($)  a b = a lor b
let (&)  a b = a land b

type nbytes =
    | N1
    | N2
    | N4
    | N8

(* the highest number that can be encoded with N bytes *)
let code_limit = function
    | N1 ->  8 - 1
    | N2 -> 16 - 2
    | N4 -> 32 - 3
    | N8 -> 64 - 3

(* encode code length with first few bits *)
let len_bits = function
    | N1 -> 0b0   << (16 - 1)
    | N2 -> 0b10  << (16 - 2)
    | N4 -> 0b110 << (16 - 3)
    | N8 -> 0b111 << (16 - 3)

(* remove length encoding bits to get sequence code *)
let mask = function
    | N1 -> lnot (0b1 << (16 - 1))
    | N2 -> lnot (0b11 << (16 - 2))
    | N4
    | N8 -> lnot (0b111 << (16 - 3))

let compress input =
    (* compress the string *)
    let compressed, _ =
        input
        |> Bytes.to_string
        |> compress
    in
    (* create buffer *)
    let buffer = Buffer.create @@ Bytes.length input in

    (* add next code to buffer *)
    let add_to_buffer code =
        (* decide how many bytes should represent the code *)
        let nbytes, code =
            if code < (1 << code_limit N1) then
                N1, code
            else
                let code = code - code_limit N1 in
                if code < (1 << code_limit N2) then
                    N2, code
                else
                    let code = code - code_limit N2 in
                    if code < (1 << code_limit N4) then
                        N4, code
                    else
                        N8, code
        in
        (* add length info to code, choose add' function based on nbytes *)
        let open Buffer in
        match nbytes with
        | N1 -> add_uint8 buffer (code $ len_bits N1)
        | N2 -> add_uint16_be buffer (code $ len_bits N2)
        | N4 ->
                add_uint16_be buffer (code >> 16 $ len_bits N4);
                add_uint16_be buffer (code >>  0);
        | N8 ->
                add_uint16_be buffer (code >> 48 $ len_bits N8);
                add_uint16_be buffer (code >> 32);
                add_uint16_be buffer (code >> 16);
                add_uint16_be buffer (code >>  0);
    in

    (* add sequence *)
    List.iter add_to_buffer compressed;
    (* output bytes *)
    Buffer.to_bytes buffer

let decompress bytes =
    (* build dict from sequence of bytes *)
    let rec build_list i acc =
        if i >= Bytes.length bytes then List.rev acc
        else
            let nbytes, code =
                let byte = Bytes.get_uint8 bytes i in
                let nbytes =
                    if byte >> (8 - 1) = 0b0 then N1
                    else if byte >> (8 - 2) = 0b10 then N2
                    else if byte >> (8 - 3) = 0b110 then N4
                    else if byte >> (8 - 3) = 0b111 then N8
                    else raise (Failure "unknown length code")
                in
                let code =
                    let get = Bytes.get_uint16_be bytes in
                    match nbytes with
                    | N1 -> byte
                    | N2 -> get i & mask N2
                    | N4 ->
                            (get (i + 0) & mask N4) << 16 $
                            (get (i + 2))
                    | N8 ->
                            (get (i + 0) & mask N8) << 48 $
                            (get (i + 2) << 32) $
                            (get (i + 4) << 16) $
                            (get (i + 6))
                in
                match nbytes with
                | N1 -> 1, code
                | N2 -> 2, code + code_limit N1
                | N4 -> 4, code + code_limit N1 + code_limit N2
                | N8 -> 8, code + code_limit N1 + code_limit N2 + code_limit N4
            in
            build_list (i + nbytes)
            @@ code :: acc
    in

    build_list 0 []
    |> decompress 
    |> Bytes.of_string


type mode =
    | Compress
    | Decompress

let () =
    let usage_err =
        Failure (Format.sprintf "Usage: %s <compress|decompress> <infile> <outfile>" Sys.argv.(0))
    in
    if Array.length Sys.argv < 4 then
        raise usage_err
    else
        let mode =
            match Sys.argv.(1) with
            | "compress" -> Compress
            | "decompress" -> Decompress
            | _ -> raise usage_err
        in

        let infile = Sys.argv.(2) in
        let outfile = Sys.argv.(3) in
        let input = read_whole_file infile in

        let output =
            match mode with
            | Compress -> time "lz78" compress input
            | Decompress -> time "decode" decompress input
        in
        
        output
            |> write_whole_file outfile;

        Format.printf "input size: %d B, output size: %d B, ratio: %f\n"
            (Bytes.length input)
            (Bytes.length output)
            (Float.of_int (Bytes.length input) /. Float.of_int (Bytes.length output));

