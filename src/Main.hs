{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Applicative
import Control.Lens
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Text.Trifecta
import Language.Gamma
import Language.Gamma.Parser.Commented

main :: IO ()
main = do
  putStrLn "hi"
  src <- getContents
  run src

run :: String -> IO ()
run src = do
  case parseString (runCommented $ some pDecl <* eof) mempty src of
    (Failure err) -> print err
    (Success stmts) -> mapM_ doStmt stmts
  
    where doStmt stmt = do
            putStrLn "Statement:"
            print (pretty stmt)
            
            putStrLn ("AST: " ++ show stmt)
            case (runTyp builtinBindings (inferTypes stmt)) of
              Left tyErr -> putStrLn ("Type Error: " ++ show tyErr)
              Right ty -> putStr "Type: " >> print (pretty (ty ^. annotation._2)) >> print ty
