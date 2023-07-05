# huffman.ml

This is a encoding-decoding program for losslessly compressing text files.

## Usage example:

This is an example of encoding and decoding an HTML file.

    huffman〉dune build
    huffman〉dune install
    huffman〉huffman encode jpbb.html jpbb.html.huf
    0.623 ms <- encode.freqs
    0.080 ms <- encode.huffman
    0.016 ms <- encode.sort
    48.696 ms <- encode.to_code
    50.089 ms <- encode
    huffman〉huffman decode jpbb.html.huf jpbb.html.huf.decoded
    0.004 ms <- decode.build_dict
    246.918 ms <- decode.decode
    247.266 ms <- decode
    huffman〉ls jpbb*
    ╭───┬───────────────────────┬──────┬──────────┬────────────╮
    │ # │         name          │ type │   size   │  modified  │
    ├───┼───────────────────────┼──────┼──────────┼────────────┤
    │ 0 │ jpbb.html             │ file │ 310.6 KB │ 5 days ago │
    │ 1 │ jpbb.html.huf         │ file │ 213.5 KB │ now        │
    │ 2 │ jpbb.html.huf.decoded │ file │ 310.6 KB │ now        │
    ╰───┴───────────────────────┴──────┴──────────┴────────────╯
    huffman〉diff -s jpbb.html jpbb.html.huf.decoded
    Files jpbb.html and jpbb.html.huf.decoded are identical

