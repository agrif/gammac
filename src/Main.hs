{-# LANGUAGE FlexibleContexts #-}

module Main where

import Control.Applicative
import Control.Lens
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))
import Text.Trifecta
import Language.C.Parser
import Language.C.Data.Position
import Language.C.Data.Node
import qualified Language.C.Pretty as CPretty
import Language.Gamma
import Language.Gamma.Parser.Commented

cAST s = (() <$) <$> parseC s nopos

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
              Right typed -> do
                  putStr "Type: "
                  print (pretty (typed ^. annotation._2))
                  print typed
                  
                  case (runGen (codegen typed)) of
                    Left genErr -> putStrLn ("Gen Error: " ++ show genErr)
                    Right (r, _) -> putStrLn "Gen:" >> print (CPretty.pretty (undefNode <$ r))
