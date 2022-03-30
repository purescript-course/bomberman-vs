module Monads where

import Prelude

data Možná a
  = Věc a
  | Nic

data Možnosti a b
  = MožnostA a
  | MožnostB b

data Hromada a
  = PrázdnáHromada
  | PlnáHromada { jedenPrvek :: a, zbytek :: Hromada a }

-- Implementujte Functor pro všechny tři typy výše

instance functorMožná :: Functor Možná where 
  map f možnáA = 
    case možnáA of 
      Nic -> Nic
      Věc a -> Věc (f a)

instance functorMožnosti :: Functor (Možnosti a) where
  map f možnostiA = 
    case možnostiA of 
      MožnostA a -> MožnostA a
      MožnostB b -> MožnostB (f b)

instance functorHromada :: Functor Hromada where 
  map f hromadaA = 
    case hromadaA of 
      PrázdnáHromada -> PrázdnáHromada 
      PlnáHromada {jedenPrvek: first, zbytek: rest} -> PlnáHromada {jedenPrvek: (f first), zbytek: (map f rest)}