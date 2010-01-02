open OUnit
open Inflate

let inflate_blah () =
        assert_equal (inflate "x\x9cs\xcaI\xccPH\xc2$\x14\x01o\x19\x08u")
        "Blah blah blah blah blah!"

let suite = "Inflate" >::: [
        "inflate_blah" >:: inflate_blah
]

let _ = run_test_tt_main suite
