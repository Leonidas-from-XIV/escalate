open OUnit
open Inflate

let zlib_compressed = "x\x9cs\xcaI\xccPH\xc2$\x14\x01o\x19\x08u"
let plaintext = "Blah blah blah blah blah!"

let read_zlib_header () =
        let printer = Some string_of_int in
        let header = parse_zlib_header zlib_compressed in
        let assert_header = assert_equal ?printer:printer in
        assert_header 8 header.cm;
        assert_header 7 header.cinfo;
        assert_header 2 header.flevel;
        assert_header 28 header.fcheck;
        assert_header 30876 header.checksum

let inflate_blah () =
        let printer = Some (fun x -> x) in
        assert_equal ?printer:printer plaintext (inflate zlib_compressed)

let suite = "Inflate" >::: [
        "inflate_blah" >:: inflate_blah;
        "read_zlib_header" >:: read_zlib_header
]

let _ = run_test_tt_main suite
