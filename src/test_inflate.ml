open OUnit2
open Inflate

let zlib_compressed = "x\x9cs\xcaI\xccPH\xc2$\x14\x01o\x19\x08u"
let plaintext = "Blah blah blah blah blah!"

let read_zlib_header expected function_got _ =
  let header, _, _ = parse_zlib zlib_compressed in
  let assert_header = assert_equal in
  assert_header expected (function_got header)

let inflate_blah _ =
  let printer = Some (fun x -> x) in
  assert_equal ?printer plaintext (inflate zlib_compressed)

let suite = "Inflate" >::: [
  "inflate_blah" >:: inflate_blah;
  "read_zlib_header_cm" >:: read_zlib_header Deflate (fun h -> h.compression_method);
  "read_zlib_header_cinfo" >:: read_zlib_header 32768 (fun h -> h.window_size);
  "read_zlib_header_flevel" >:: read_zlib_header Default (fun h -> h.compression_level);
  "read_zlib_header_fcheck" >:: read_zlib_header 28 (fun h -> h.fcheck);
  "read_zlib_header_checksum" >:: read_zlib_header 30876 (fun h ->
          h.checksum)
]

let _ = run_test_tt_main suite
