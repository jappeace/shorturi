{-# LANGUAGE StrictData #-}

-- | since uri and shortened had their own modules,
--   this needed some place as well
module Sanitization
  ( Sanitization(..)
  )
where

data Sanitization = Incoming
                  | Checked
