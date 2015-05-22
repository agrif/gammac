{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text
import Text.PrettyPrint.ANSI.Leijen
import Language.Gamma

main :: IO ()
main = do
  putStrLn "hi"
  src <- getContents
  run src

run :: String -> IO ()
run src = do
  case parseOnly (many pDecl <* endOfInput) (pack src) of
    (Left err) -> print err
    (Right stmts) -> mapM_ doStmt stmts
  
    where doStmt stmt = do
            putStrLn "Statement:"
            print (pretty stmt)
            
            putStrLn ("AST: " ++ show stmt)
            case (runTyp builtinBindings (typecheck stmt)) of
              Left tyErr -> putStrLn ("Type Error: " ++ show tyErr)
              Right ty -> putStr "Type: " >> print (pretty ty) >> print ty
