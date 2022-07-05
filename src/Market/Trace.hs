{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Market.Trace
    ( test
    ) where


import Plutus.Trace.Emulator as Emulator
    ( activateContractWallet, waitNSlots, runEmulatorTraceIO', callEndpoint, EmulatorConfig(..) )
import           Control.Monad    (void)
import           PlutusTx.Prelude as Plutus ( ($), (<>), Either(..) )
import           Ledger.Value     as Value (singleton)
import qualified Data.Map         as Map
import qualified Ledger.Ada       as Ada

import           Prelude      (IO)
import           Data.Default (def)

import Utility         (wallet, mp)
import Market.Offchain (endpoints)
import Market.Types    


royalty1 :: RoyAddr
royalty1 = RoyAddr
    {
        nRoyAddr  = "09aaedfc2c267948a623a4dddd093327c235c3fa88a47f14d41a7347" 
        ,nRoyPrct = 2    
    }


royalty2 :: RoyAddr
royalty2 = RoyAddr
    {
        nRoyAddr  = "d9c4c0fe99024fe7b811e68e9f361d8228dce57305b2d71170eba4ca" 
        ,nRoyPrct = 3
    }


nftEx1 :: StartParams
nftEx1 = StartParams
    { sPrice = 100
    , sTn    = "market1"
    , sCs    = "66"
    , sAd  = [royalty1,royalty2]
    } -- This is an example token, 
      -- As these are the parameters of the validator, this info should be provided by the user of the contract

nftEx2 :: StartParams
nftEx2 = StartParams
    { sPrice = 100
    , sTn    = "market2"
    , sCs    = "66"
    , sAd  = [royalty1]
    }

nftEx1' :: BuyParams
nftEx1' = BuyParams
    { bTn = sTn nftEx1
    , bCs = sCs nftEx1
    }

nftEx2' :: BuyParams
nftEx2' = BuyParams
    { bTn = sTn nftEx2
    , bCs = sCs nftEx2
    }


test :: IO ()
test = do
    let dist = Map.fromList [ (wallet 1, Ada.lovelaceValueOf 10_000_000)
                            , (wallet 2, Ada.lovelaceValueOf 10_000_000)
                            , (wallet 3, Ada.lovelaceValueOf 10_000_000
                                      <> Value.singleton (sCs nftEx2) (sTn nftEx2) 1
                                      <> Value.singleton (sCs nftEx1) (sTn nftEx1) 1)
                            , (wallet 4, Ada.lovelaceValueOf 10_000_000)
                            , (wallet 5, Ada.lovelaceValueOf 10_000_000)
                            , (wallet 6, Ada.lovelaceValueOf 10_000_000)
                            ]
        emCfg = EmulatorConfig (Left dist) def def
    runEmulatorTraceIO' def emCfg $ do
        h1 <- activateContractWallet (wallet 1) endpoints
        h2 <- activateContractWallet (wallet 2) endpoints
        h3 <- activateContractWallet (wallet 3) endpoints
        h4 <- activateContractWallet (wallet 4) endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"sendToken" h1 0
        void $ Emulator.waitNSlots 1
        callEndpoint @"start" h3 nftEx2
        void $ Emulator.waitNSlots 1
        callEndpoint @"start" h3 nftEx1
        void $ Emulator.waitNSlots 1
        callEndpoint @"buy'" h4 (nftEx1', nftEx2')
        void $ Emulator.waitNSlots 1

    