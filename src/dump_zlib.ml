open Inflate

let compression_level = function
  | Fastest -> "Fastest"
  | Fast -> "Fast"
  | Default -> "Default"
  | Maximum -> "Maximum"

let final = function
  | Last -> " (final)"
  | Continues -> ""

let encoding = function
  | FixedHuffman _ -> "fixed Huffman"
  | DynamicHuffman -> "dynamic Huffman"
  | Uncompressed _ -> "uncompressed"

let pp_payload = function
  | Literal x -> Printf.printf "Literal '%c'\n" x;
  | Repeat (len, dist) -> Printf.printf "Repeat <length %d, distance %d>\n"
     len dist

let pp_segments = function
  (continues, blocks) ->
    Printf.printf "New block%s in %s encoding:\n" (final continues)
      (encoding blocks);
    match blocks with
    | FixedHuffman payload -> List.iter pp_payload payload
    | DynamicHuffman -> Printf.printf "Dynamic Huffman TODO\n"
    | Uncompressed bytes -> Printf.printf "Uncompressed %d bytes\n" @@
      String.length bytes

let () =
  let file_name = Array.get Sys.argv 1 in
  let chan = open_in_bin file_name in
  let n = in_channel_length chan in
  let s = Bytes.create n in
  really_input chan s 0 n;
  close_in chan;
  let header, segments, adler32 = Inflate.parse_zlib s in
  Printf.printf "Compression level: %s\n" @@ compression_level
    header.compression_level;
  Printf.printf "Window size: %d\n" header.window_size;
  Printf.printf "Dict: %B\n" header.fdict;
  List.iter pp_segments segments;
  Printf.printf "ADLER32: %lu\n" adler32;
