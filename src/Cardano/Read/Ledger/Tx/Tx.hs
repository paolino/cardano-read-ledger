{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2022 IOHK, 2024 Cardano Foundation
-- License: Apache-2.0
--
-- The 'Tx' type represents transactions as they are read from the mainnet ledger.
-- It is compatible with the era-specific index types from @cardano-ledger@.
module Cardano.Read.Ledger.Tx.Tx
    ( Tx (..)
    , TxT
    ) where

import Prelude

import Cardano.Chain.UTxO qualified as Byron
import Cardano.Ledger.Core qualified as Core
import Cardano.Read.Ledger.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Mary
    , Shelley
    )

-- | Closed type family returning the ledger 'Tx' type for each known @era@.
type family TxT era where
    TxT Byron = Byron.ATxAux ()
    TxT Shelley = Core.Tx Shelley
    TxT Allegra = Core.Tx Allegra
    TxT Mary = Core.Tx Mary
    TxT Alonzo = Core.Tx Alonzo
    TxT Babbage = Core.Tx Babbage
    TxT Conway = Core.Tx Conway

-- | A tx in any era
newtype Tx era = Tx {unTx :: TxT era}

deriving instance Show (TxT era) => Show (Tx era)
deriving instance Eq (TxT era) => Eq (Tx era)
deriving instance Ord (TxT era) => Ord (Tx era)
