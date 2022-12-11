# A Futhark Implementation of rANS [![CI](https://github.com/leonardschneider/rans/workflows/CI/badge.svg)](https://github.com/leonardschneider/rans/actions)

A simple implementation of rANS (range [Asymmetric Numeral System](https://en.wikipedia.org/wiki/Asymmetric_numeral_systems)) in Futhark.

## Installation

```
$ futhark pkg add github.com/leonardschneider/rans
$ futhark pkg sync
```

## Usage

```
> import "lib/github.com/leonardschneider/rans"
> let msg = "Hello world! It is a beautiful day."
> let r = encinit 10 (mksym8 msg)
> let sz = size8 r msg
> sz
4
> let enc = encode8 sz r msg
> let dec = decode8 (len msg) enc.0 enc.1
> (dec.1 :> [35]u8) == msg
true
```
