open OUnit
open Inflate

let zlib_compressed = "x\x9cs\xcaI\xccPH\xc2$\x14\x01o\x19\x08u"
let plaintext = "Blah blah blah blah blah!"

let read_cm_info () =
        let printer = Some string_of_int in
        let header = parse_zlib_header zlib_compressed in
        assert_equal ?printer:printer 8 header.cm

let inflate_blah () =
        let printer = Some (fun x -> x) in
        assert_equal ?printer:printer plaintext (inflate zlib_compressed)

let suite = "Inflate" >::: [
        "inflate_blah" >:: inflate_blah;
        "read_cm_info" >:: read_cm_info
]

let _ = run_test_tt_main suite
