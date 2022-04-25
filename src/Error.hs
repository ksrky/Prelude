module Error where

newtype Error = Error {message :: String}

instance Show Error where
    show (Error msg) = msg

returnErr :: String -> Either Error a
returnErr s = Left $ Error s