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
    send "Hello, World! I'm back."
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
             reply msg $ T.pack "Usage\n - start with three backquotes followed by 'PDP'.\n - end with three backquotes.\n"
         | otherwise -> return ()

runPDP11 :: String -> Maybe String
runPDP11 str = run <$> readASM str
  where run program = unlines $ concatMap (\(n, m) -> [n, show m]) $ zip instrs states
          where states = runSimulator' program
                instrs = "#0 Initial state" : zipWith3 combine [1.. ] mnems program
                combine n a b = "#" ++ show n ++ " " ++ a ++ "\t; " ++ show b
        mnems = lines str
