{-# LANGUAGE LambdaCase #-}

data Bit = Up | Down deriving (Show)

bits = [Up, Down]

bitpairs = [(a, b) | a <- bits, b <- bits]

ops :: [(Bit, Bit) -> Bit]
ops =
  [ \case
      (Up, Up) -> a
      (Up, Down) -> b
      (Down, Up) -> c
      (Down, Down) -> d
    | a <- bits,
      b <- bits,
      c <- bits,
      d <- bits
  ]

outcomes = ops <*> bitpairs

main = print outcomes