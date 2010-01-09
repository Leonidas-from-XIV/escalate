open OUnit
open Inflate

let zlib_compressed = "x\x9cs\xcaI\xccPH\xc2$\x14\x01o\x19\x08u"
let plaintext = "Blah blah blah blah blah!"

let read_cm_info () =
        let cm = parse_header zlib_compressed in
        assert_equal cm 6

let inflate_blah () =
        assert_equal (inflate zlib_compressed) plaintext

let suite = "Inflate" >::: [
        "inflate_blah" >:: inflate_blah
]

let _ = run_test_tt_main suite
