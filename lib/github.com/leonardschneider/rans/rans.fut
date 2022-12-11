
module type State = {
  type State [n]
  val init [n]: (sb: u32) -> (freq: [n]u32) -> State [n]
  val sb   [n]: (r: State [n]) -> u32
  val freq [n]: (r: State [n]) -> i64 -> u32
  val cdf  [n]: (r: State [n]) -> i64 -> u32
  val x    [n]: (r: State [n]) -> u64
  val setx [n]: (r: State [n]) -> u64 -> State [n]
  val next [n]: (r: State [n]) -> (s: i64) -> State [n]
  val prev [n]: (r: State [n]) -> (s: i64) -> State [n] 
}

module type SymbolEncoder = {
  type State [n]
  val renorm [n]: (r: State [n]) -> (s: i64) -> bool
  val write  [n]: (r: State [n]) -> (State [n], u32)
  val C      [n]: (r: State [n]) -> (s: i64) -> State [n] 
}

module type SymbolDecoder = {
  type State[n]
  val renorm [n]: (r: State [n]) -> bool
  val read   [n]: (r: State [n]) -> (g: u32) -> State [n]
  val D      [n]: (r: State [n]) -> (State [n], i64)
}

module type Codec = {
  type State [n]
  val size   [n][m]: (r: State [n]) -> (msg: [m]i64) -> i64
  val encode [n][m]: (k: i64) -> (r: State [n]) -> (msg: [m]i64) -> (State [n], [k]u32)
  val decode [n][m]: (k: i64) -> (r: State [n]) -> (enc: [m]u32) -> (State [n], [k]i64)
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

module MkSymbolEncoder(S: State): SymbolEncoder with State [n] = S.State [n] = {
  type State [n] = S.State [n]

  def renorm [n](r: State [n])(s: i64): bool =
    -- check whether the internal state x needs to renomalized
    -- that is, if x, once encoded is such as L <= x < b*L
    -- in this implementation b=32
    let x = S.x r
    let sb = u64.u32 (S.sb r)
    let freq = u64.u32 (S.freq r s)
    let x_max = ((L >> sb) << 32) * freq
    in x >= x_max

  def write [n](r: State [n]): (State [n], u32) =
    let x = S.x r
    in (S.setx r (x >> 32), u32.u64 x)

  def C [n](r: State [n])(s: i64): State [n] =
    -- encode a symbol and update state
    -- init
    let x = S.x r
    let sb = u64.u32 (S.sb r)
    let cdf = u64.u32 (S.cdf r s)
    let freq = u64.u32 (S.freq r s)
    -- state update
    let x = ((x / freq) << sb) + (x % freq) + cdf
    in S.setx r x

}

module MkSymbolDecoder(S: State): SymbolDecoder with State [n] = S.State[n] = {
  type State [n] = S.State [n]

  def init = init

  def D 't [n](r: State [n]): (State [n], i64) =
    -- decode a symbol
    let x = S.x r
    let sb = u64.u32 (S.sb r)
    -- optimization for: y = x % (1 << sb)
    let mask = (1 << sb) - 1
    let y = u32.u64 (x & mask)
    let i =
      iota n
        |> map (S.cdf r)
        |> map (<=y)
        |> map i64.bool
        |> reduce (+) 0
    -- s is such that cdf[s] <= y < cdf[s+1]
    let s = i - 1
    -- update decoder state
    let freq = u64.u32 (S.freq r s)
    let cdf = u64.u32 (S.cdf r s)
    let x = freq * (x >> sb) + (x & mask) - cdf
    in
      (S.setx r x, s)

  def renorm [n](r: State [n]): bool =
    (S.x r) < L

  def read [n](r: State [n])(enc0: u32): State [n] = 
    let x = S.x r
    let x = (x << 32) | (u64.u32 enc0)
    in S.setx r x

}

module State: State = {
  type State [n] = {
    x: u64,
    sb: u32,
    freq: [n]u32,
    cdf: [n]u32
  }

  def init [n](sb: u32)(freq: [n]u32): State [n] =
    -- sum of freqs must be 1 << sb
    -- renormalize accordingly
    let nfreq = freqrenorm sb freq
    in {sb=sb, freq=nfreq, cdf=(cdf nfreq), x = L}

  def x    [n](r: State [n]): u64 = r.x
  def sb   [n](r: State [n]): u32 = r.sb
  def freq [n](r: State [n])(i: i64): u32 = r.freq[i]
  def cdf  [n](r: State [n])(i: i64): u32 = r.cdf[i]
  def setx [n](r: State [n])(x: u64): State [n] = r with x = x

  def next = \r _ -> r
  def prev = \r _ -> r
}

module MkCodec
  (S: State)
  (C: SymbolEncoder with State [n] = S.State [n])
  (D: SymbolDecoder with State [n] = S.State [n]): Codec with State [n] = S.State [n] = {
  type State [n] = S.State [n]

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

  def encode [n][m](k: i64)(r: State [n])(msg: [m]i64): (State [n], [k]u32) =
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

  def decode [n][m](k: i64)(r: State [n])(enc: [m]u32): (State [n], [k]i64) =
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

module SymbolEncoder = MkSymbolEncoder State

-- Symbol encoder
entry encinit [n](sb: u32)(freq: [n]u32): State.State [n] = State.init sb freq
entry encrenorm [n](r: State.State [n])(s: i64): bool = SymbolEncoder.renorm r s
entry encwrite [n](r: State.State [n]): (State.State [n], u32) = SymbolEncoder.write r
entry C [n](r: State.State [n])(s: i64): State.State [n] = SymbolEncoder.C r s

module SymbolDecoder = MkSymbolDecoder State

-- Symbol decoder
entry decrenorm [n](r: State.State [n]): bool = SymbolDecoder.renorm r
entry decread [n](r: State.State [n])(g: u32): State.State [n] = SymbolDecoder.read r g
entry D [n](r: State.State [n]): (State.State [n], i64) = SymbolDecoder.D r


-- High level byte-level encoder and decoders

entry mksym8 [n](msg: [n]u8): [256]u32 =
  -- calculate byte symbol freqs from message
  reduce_by_index (replicate 256 0u32) (+) 0 (map i64.u8 msg) (replicate n 1u32)

-- Base Encoder/Decoder
module BaseCodec = MkCodec State SymbolEncoder SymbolDecoder
entry size8 [n][m](r: State.State [n])(msg: [m]u8): i64 =
  BaseCodec.size r (map i64.u8 msg)
entry encode8 [n][m](k: i64)(r: State.State [n])(msg: [m]u8): (State.State [n], [k]u32) =
  BaseCodec.encode k r (map i64.u8 msg)
entry decode8 [n][m](k: i64)(r: State.State [n])(enc: [m]u32): (State.State [n], [k]u8) =
  let (r, msg) = BaseCodec.decode k r enc
  let msg = map u8.i64 msg
  in (r, msg)
