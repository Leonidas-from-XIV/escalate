open OUnit
open Inflate

let zlib_compressed = "x\x9cs\xcaI\xccPH\xc2$\x14\x01o\x19\x08u"
let plaintext = "Blah blah blah blah blah!"

let read_zlib_header expected function_got () =
  let printer = Some string_of_int in
  let header = parse_zlib_header zlib_compressed in
  let assert_header = assert_equal ?printer:printer in
  assert_header expected (function_got header)

let inflate_blah () =
  let printer = Some (fun x -> x) in
  assert_equal ?printer:printer plaintext (inflate zlib_compressed)

let suite = "Inflate" >::: [
  "inflate_blah" >:: inflate_blah;
  "read_zlib_header_cm" >:: read_zlib_header 8 (fun h -> h.cm);
  "read_zlib_header_cinfo" >:: read_zlib_header 7 (fun h -> h.cinfo);
  "read_zlib_header_flevel" >:: read_zlib_header 2 (fun h -> h.flevel);
  "read_zlib_header_fcheck" >:: read_zlib_header 28 (fun h -> h.fcheck);
  "read_zlib_header_checksum" >:: read_zlib_header 30876 (fun h ->
          h.checksum)
]

let _ = run_test_tt_main suite
