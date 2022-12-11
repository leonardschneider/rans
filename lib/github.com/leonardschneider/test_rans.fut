import "rans"

-- Test byte symbol encoding/decoding
-- ==
-- entry: test_C_D_invariant
-- random input { [1000]u8 } output { true }

entry test_C_D_invariant [n](s: [n]u8): bool =
  let test = \s -> (
    let s0 = i64.u8 s
    let msg = [s]
    let freqs = mksym8 msg
    let r0 = encinit 10 freqs
    let renc = C r0 s0
    let (_, sdec) = D renc
    in s0 == sdec
  )
  in any test s

-- Test single byte stream encoding/decoding
-- ==
-- entry: test_encode8_decode8_invariant
-- random input { [1000][1]u8 } output { true }
-- random input { [1000][2]u8 } output { true }
-- random input { [1000][3]u8 } output { true }
-- random input { [1000][4]u8 } output { true }
-- random input { [1000][5]u8 } output { true }
-- random input { [1000][6]u8 } output { true }
-- random input { [1000][7]u8 } output { true }
-- random input { [1000][8]u8 } output { true }
-- random input { [1000][9]u8 } output { true }
-- random input { [1000][10]u8 } output { true }
-- random input { [1000][100]u8 } output { true }

entry test_encode8_decode8_invariant [n][m](msgs: [n][m]u8): bool =
  let test = \msg -> (
    let freqs = mksym8 msg
    let r0 = encinit 10 freqs
    let sz = size8 r0 msg
    let (renc, enc) = encode8 sz r0 msg
    let (_, dec) = decode8 m renc enc
    in map2 (==) dec msg |> reduce (&&) true
  )
  in any test msgs
