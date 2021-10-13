{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}


-- | hides Uri constructor so that validation always happens
module Model
   where

import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Sanitization
import           Shortened
import           Uri

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Mapping
    shortened (Shortened 'Checked)
    original (Uri 'Checked)

    UniqueShortened shortened -- shortened needs to be unique so that we don't accidently create two the same
    UniqueOriginal original -- the original optinally is uqniue, to not polute the shortened namespace.

    deriving Show
|]
