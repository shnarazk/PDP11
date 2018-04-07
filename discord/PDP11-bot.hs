{-# LANGUAGE LambdaCase, MultiWayIf, OverloadedStrings, RecordWildCards #-}
import Control.Concurrent
import qualified Control.Exception as E
import qualified Data.Text as T
import Pipes
import System.Process
import Network.Discord
import PDP11
import Assembler
import Simulator
import DiscordSecret (channel, uid, token)

version :: String
version = "0.1.0"

send :: T.Text -> Effect DiscordM ()
send mes = fetch' (CreateMessage channel mes Nothing)

reply :: Message -> T.Text -> Effect DiscordM ()
reply Message{messageChannel=chan} cont = fetch' $ CreateMessage chan cont Nothing

main :: IO ()
main = do
  tic <- newEmptyMVar
  tid <- forkFinally mainLoop (\_ -> putMVar tic ())
  void $ takeMVar tic
  threadDelay $ 1000*1000*100
  main
--  
--  E.handle (\case
--               E.UserInterrupt -> putStrLn "User interrupt recieved."
--               e -> print e >> main) $ do
--    mainLoop

mainLoop :: IO ()
mainLoop = runBot (Bot token) $ do
  with ReadyEvent $ \(Init v u _ _ _) -> do
    liftIO . putStrLn $ "Connected to gateway v" ++ show v ++ " as " ++ show u
    send . T.pack $ "Hello, World! I'm back (version " ++ version ++ ")."
    return ()

  with MessageCreateEvent $ \msg@Message{..} -> do
    when (uid /= userId messageAuthor) $ do -- prevent infinite self loop
      if | "```PDP" `T.isPrefixOf` messageContent -> do
             -- liftIO $ print messageAuthor
             let code = unlines . takeWhile ("```" /=) . tail . lines $ (T.unpack messageContent)
             case runPDP11 code of
               Just output -> reply msg . T.pack $ userName messageAuthor ++ ", I did.\n```" ++ output ++ "```"
               Nothing     -> reply msg . T.pack $ userName messageAuthor ++ ", your code is wrong."
             return ()
         | "/help" `T.isPrefixOf` messageContent -> do
             reply msg . T.pack $ helpFormat ++ helpAddrMode
         | otherwise -> return ()

helpFormat = "\n\
\Format\n\n\
\ - start with three backquotes followed by 'PDP'.\n\
\ - end with three backquotes.\n"

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
