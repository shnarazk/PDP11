{-# LANGUAGE OverloadedStrings, TypeOperators, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Monad (mzero)
import Data.Proxy
import GHC.TypeLits

import Network.Discord
import Control.Monad.IO.Class

import qualified Data.Text as T
import Control.Monad
import Data.List (intercalate)
import PDP11 hiding (version)
import Assembler hiding (version)
import Simulator hiding (version)
import qualified PDP11 as Spec (version)
import qualified Assembler as Asm (version)
import qualified Simulator as Sim (version)
import DiscordSecret (token)

instance DiscordAuth IO where
  auth    = return $ Bot token
  version = return $ intercalate ", " [ "0.3.0"
                                      , "spec: " ++ Spec.version
                                      , "assembler: " ++ Asm.version
                                      , "simulator: " ++ Sim.version
                                      ]
  runIO   = id

data MnemonicHandler

instance EventMap MnemonicHandler (DiscordApp IO) where
  type Domain   MnemonicHandler = Message
  type Codomain MnemonicHandler = ()

  mapEvent p (m@Message{ messageContent = c
                       , messageChannel = ch
                       , messageAuthor = User{userIsBot = bot, userId = uid}}
             )
    | bot = return ()
    | "```PDP" `T.isPrefixOf` c = do
        ver <- version
        let code = takeWhile ("```" /=) . tail . lines . T.unpack $ c
            rmes = case runPDP11 (unlines code) of
                     Just output -> "<@" ++ show uid ++ ">"
                                    ++ ", I did. -- "
                                    ++ ver ++ "\n```"
                                    ++ output
                                    ++ "```"
                     Nothing     -> "<@" ++ show uid ++ ">, your code is wrong."
        let form :: String -> String
            form l = l' ++ replicate (24 - length l') ' '
              where trim = dropWhile (`elem` (" \t" :: String))
                    l' = reverse . trim . reverse . trim $ l
            printer l (i, b) = form (if i == 0 then l else "") ++ show b ++ "\n"
            toBit l = case assemble (l ++ "\n") of
              Right [as] -> concatMap (printer l) (zip [0 ..] (toBitBlocks as))
              Left mes -> show mes
            bits = "\n```\n" ++ concatMap toBit code ++ "```\n"
        void $ doFetch $ CreateMessage ch (T.pack (rmes ++ bits)) Nothing
    | "!help" `T.isPrefixOf` c = do
        v <- ("version: " ++) <$> version
        void $ doFetch $ CreateMessage ch (T.pack (v ++ "\n" ++ helpFormat ++ helpAddrMode)) Nothing
    | otherwise = return ()

type PDP11App = (MessageCreateEvent :<>: MessageUpdateEvent) :> MnemonicHandler

instance EventHandler PDP11App IO

main :: IO ()
main = runBot (Proxy :: Proxy (IO PDP11App))

helpFormat = "\n\
\Format\n\n\
\ - start with three backquotes followed by 'PDP'.\n\
\ - end with three backquotes.\n\
\ - 3つのバッククォート文字、それから'PDP'ではじまるよ\n\
\ - 3つのバッククォート文字でおわるよ\n"

helpAddrMode = "\n\
\```\n\
\Syntax  Addressing Mode               Action\n\
\Rn      Register                      Data = Rn\n\
\(Rn)+   Autoincrement                 Data = (Rn)\n\
\                                      Rn++\n\
\-(Rn)   Autodecrement                 Rn–\n\
\                                      Data = (Rn)\n\
\X(Rn)   Index                         Offset address X = (PC)\n\
\                                      PC += 2\n\
\                                      Base address = Rn\n\
\                                      Data = (Rn + X)\n\
\@Rn     Register Deferred             Data = (Rn)\n\
\@(Rn)+  Autoincrement Deferred        Data =((Rn))\n\
\                                      Rn++\n\
\@-(Rn)  Autodecrement Deferred        Rn–\n\
\                                      Data =((Rn))\n\
\@X(Rn)  Index Deferred                Offset address X = (PC)\n\
\                                      PC += 2\n\
\                                      Base address = Rn\n\
\                                      Data = ((Rn + X))\n\
\#n      Immediate                     Data = (PC) = n\n\
\@#A     Immediate Deferred (Absolute) Data = ((PC)) = (A)\n\
\```\n\
\See: https://programmer209.wordpress.com/2011/08/03/the-pdp-11-assembly-language/ \n"
