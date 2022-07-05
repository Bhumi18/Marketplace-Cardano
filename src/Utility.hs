{-# LANGUAGE OverloadedStrings #-}

module Utility
    ( walletPubKeyHash
    , wallet
    , companyPkh
    , companyPkhReal
    , mp
    , mpReal
    , mpMainnet ) where

import           Plutus.V1.Ledger.Crypto (PubKeyHash)
import           Wallet.Emulator.Wallet (Wallet, knownWallet, walletPubKeyHash)

import           Prelude hiding ((.))

import Market.Types  (MarketParams(..))

wallet :: Integer -> Wallet
wallet = knownWallet

companyPkh :: PubKeyHash
companyPkh = walletPubKeyHash $ wallet 1


mp :: MarketParams
mp = MarketParams companyPkh



companyPkhReal :: PubKeyHash
companyPkhReal = "06815038a71dd54d8a7db7eccf405aa25fff04590f198134bc70d8a8"


mpReal :: MarketParams
mpReal = MarketParams companyPkhReal




companyPkhMainnet :: PubKeyHash
companyPkhMainnet = "06815038a71dd54d8a7db7eccf405aa25fff04590f198134bc70d8a8"


mpMainnet :: MarketParams
mpMainnet = MarketParams companyPkhMainnet
