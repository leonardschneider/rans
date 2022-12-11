-- encoder state type
type State [n] = {
  sb: u32,
  freq: [n]u32,
  cdf: [n]u32,
  x: u64
}

module type SymbolEncoder = {
  --type State [n]
  val init   [n]: (sb: u32) -> (freq: [n]u32) -> State[n]
  val renorm [n]: (r: State[n]) -> (s: i64) -> bool
  val write  [n]: (r: State[n]) -> (State[n], u32)
  val C      [n]: (r: State[n]) -> (s: i64) -> State[n] 
}

module type SymbolDecoder = {
  --type State[n]
  val init   [n]: (x: u64) -> (sb: u32) -> (freq: *[n]u32) -> State[n]
  val renorm [n]: (r: State[n]) -> bool
  val read   [n]: (r: State[n]) -> (g: u32) -> State[n]
  val D      [n]: (r: State[n]) -> (State[n], i64)
}

module type Encoder = {
  --type State[n]
  val size   [n][m]: (r: State[n]) -> (msg: [m]i64) -> i64
  val encode [n][m]: (k: i64) -> (r: State[n]) -> (msg: [m]i64) -> (State[n], [k]u32)
}

module type Decoder = {
  --type State[n]
  val decode [n][m]: (k: i64) -> (r: State[n]) -> (enc: [m]u32) -> (State[n], [k]i64)
}

let L: u64 = (1 << 31)

-- utility function
let shift1 't [n] (ne: t)(as: [n]t): [n]t =
  map (\i -> if i == 0 then ne else as[i-1]) (iota n)

def cdf [n](freq: [n]u32): [n]u32 =
  freq
    |> scan (+) 0
    |> shift1 0

entry freqrenorm [n](sb: u32)(freq: [n]u32): [n]u32 =
  -- renormalize freqs so that their sum is equal to 1 << sb
  let sum = reduce (+) 0 freq
  in
    if sum == 0 then
      freq
    else
      -- remove n and add it at the end to avoid zeros
      -- and their dreadful divisions
      let goal = (1 << sb) - (u32.i64 n)
      let i = 0
      let rem = 0
      let freq = copy freq
      let (freq, _, _) =
        loop (freq, i, rem) while i < n do
          let f = (freq[i] * goal + rem)
          in (freq with [i] = f // sum, i+1, f % sum)
      let freq = map (+1) freq
      in freq

def init [n](x: u64)(sb: u32)(freq: [n]u32): State [n] =
  -- sum of freqs must be 1 << sb
  -- renormalize accordingly
  let nfreq = freqrenorm sb freq
  in {sb=sb, freq=nfreq, cdf=(cdf nfreq), x=x}

module SymbolEncoder: SymbolEncoder = {
  --type State [n] = State [n]

  def init = init L

  def renorm [n](state: State[n])(s: i64): bool =
    -- check whether the internal state x needs to renomalized
    -- that is, if x, once encoded is such as L <= x < b*L
    -- in this implementation b=32
    let x = state.x
    let sb = u64.u32 state.sb
    let freq = u64.u32 state.freq[s]
    let x_max = ((L >> sb) << 32) * freq
    in x >= x_max

  def write [n](state: State[n]): (State[n], u32) =
    let x = state.x
    in (state with x = x >> 32, u32.u64 x)

  def C [n](state: State[n])(s: i64): State[n] =
    -- encode a symbol and update state
    -- init
    let x = state.x
    let sb = u64.u32 state.sb
    let cdf = u64.u32 state.cdf[s]
    let freq = u64.u32 state.freq[s]
    -- state update
    let x = ((x / freq) << sb) + (x % freq) + cdf
    in state with x = x

}

module SymbolDecoder: SymbolDecoder = {
  --type State [n] = State [n]

  def init = init

  def D [n](state: State[n]): (State[n], i64) =
    -- decode a symbol
    let x = state.x
    let sb = u64.u32 state.sb
    -- optimization for: y = x % (1 << sb)
    let mask = (1 << sb) - 1
    let y = u32.u64 (x & mask)
    let i =
      state.cdf
        |> map (<=y)
        |> map i64.bool
        |> reduce (+) 0
    -- s is such that cdf[s] <= y < cdf[s+1]
    let s = i - 1
    -- update decoder state
    let freq = u64.u32 state.freq[s]
    let cdf = u64.u32 state.cdf[s]
    let x = freq * (x >> sb) + (x & mask) - cdf
    in
      (state with x = x, s)

  def renorm [n](state: State[n]): bool =
    state.x < L

  def read [n](state: State[n])(enc0: u32): State[n] = 
    let x = state.x
    let x = (x << 32) | (u64.u32 enc0)
    in state with x = x

}

module MkEncoder(C: SymbolEncoder): Encoder = {
  --type State [n] = C.State [n]

  def size [n][m](r: State [n])(msg: [m]i64): i64 =
    -- calculate encoded message size
    let j = 0
    let (_, j) =
      loop (r, j) for i in 0..<m do
        let s = msg[m-1-i]
        let (r, j) =
          if (C.renorm r s) then
            let (r, _) = C.write r
            in (r, j + 1)
          else
            (r, j)
        let r = C.C r s
        in (r, j)
    in j

  def encode [n][m](k: i64)(r: State [n])(msg: [m]i64): (State[n], [k]u32) =
    -- encode get value
    let j = 0
    let enc = replicate k 0
    let (r, _, enc) =
      loop (r, j, enc) for i in 0..<m do
        -- encode in reverse order
        let s = msg[m-1-i]
        let (r, j, enc) =
          if (C.renorm r s) then
            let (r, e) = C.write r
            in (r, j + 1, enc with [k-1-j] = e)
          else
            (r, j, enc)
        let r = C.C r s
        in (r, j, enc)
    in (r, enc)

}

module MkDecoder(D: SymbolDecoder): Decoder = {
  --type State [n] = D.State[n]

  def decode [n][m](k: i64)(r: State[n])(enc: [m]u32): (State[n], [k]i64) =
    let msg = replicate k 0
    let i = 0 -- input index
    let (r, _, msg) =
      loop (r, i, msg) for j in 0..<k do
        let (r, s) = D.D r
        let (r, i) =
          if D.renorm r then
            let r = D.read r enc[i]
            in (r, i + 1)
          else
            (r, i)
        in
          (r, i, msg with [j] = s)
    in (r, msg)

}

-- entry points

-- Symbol encoder
entry encinit [n](sb: u32)(freq: [n]u32): State[n] = SymbolEncoder.init sb freq
entry encrenorm [n](r: State[n])(s: i64): bool = SymbolEncoder.renorm r s
entry encwrite [n](r: State[n]): (State[n], u32) = SymbolEncoder.write r
entry C [n](r: State[n])(s: i64): State[n] = SymbolEncoder.C r s

-- Symbol decoder
entry decinit [n](x: u64)(sb: u32)(freq: *[n]u32): State[n] = SymbolDecoder.init x sb freq
entry decrenorm [n](r: State[n]): bool = SymbolDecoder.renorm r
entry decread [n](r: State[n])(g: u32): State[n] = SymbolDecoder.read r g
entry D [n](r: State[n]): (State[n], i64) = SymbolDecoder.D r


-- High level byte-level encoder and decoders

entry mksym8 [n](msg: [n]u8): [256]u32 =
  -- calculate byte symbol freqs from message
  reduce_by_index (replicate 256 0u32) (+) 0 (map i64.u8 msg) (replicate n 1u32)

-- Encoder
module Encoder = MkEncoder SymbolEncoder
entry size8 [n][m](r: State[n])(msg: [m]u8): i64 = Encoder.size r (map i64.u8 msg)
entry encode8 [n][m](k: i64)(r: State[n])(msg: [m]u8): (State[n], [k]u32) = Encoder.encode k r (map i64.u8 msg)

-- Decoder
module Decoder = MkDecoder SymbolDecoder
entry decode8 [n][m](k: i64)(r: State[n])(enc: [m]u32): (State[n], [k]u8) =
  let (r, msg) = Decoder.decode k r enc
  let msg = map u8.i64 msg
  in (r, msg)

