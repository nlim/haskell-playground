import Control.Monad.Cont
-- Returns a string depending on the length of the name parameter.
-- If the provided string is empty, returns an error.
-- Otherwise, returns a welcome message.
whatsYourName :: String -> String
whatsYourName name =
  (`runCont` id) $ do                      -- 1
    response <- callCC $ \exit -> do       -- 2
               validateName name exit               -- 3
               return $ "Welcome, " ++ name ++ "!"  -- 4
    return response

whatsYourNameCont :: String -> Cont r String
whatsYourNameCont name = do response <- callCC $ \exit -> do validateName name exit
                                                             return $ "Welcome, " ++ name ++ "!"
                            return response
quux :: Cont r Int
quux = callCC $ \k -> do let n = 5
                         when True $ k n
                         return 25

quuxE = runCont id quux

validateName :: Monad m => [a] -> ([Char] -> m ()) -> m ()
validateName name exit = do
  when (null name) (exit "You forgot to tell me your name!")
