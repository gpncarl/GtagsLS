{-# LANGUAGE OverloadedStrings #-}

module GtagsLS
    ( handlers
    ) where

import Language.LSP.Server
import Language.LSP.Types
import Control.Monad.IO.Class
import qualified Data.Text as T
import Text.Regex.PCRE
import Text.Regex.PCRE.String
import System.Process
import System.IO
import Control.Exception (evaluate)
import Control.DeepSeq (force)

handlers :: Handlers (LspM ())
handlers = mconcat
  [ requestHandler STextDocumentDefinition $ \req responder -> do
      let RequestMessage _ _ _ (DefinitionParams doc pos _ _) = req
      word <- liftIO $ wordAtDocPosition doc pos
      res <- liftIO $ globalDR word ""
      responder . Right . InR . InL $ res 
  , requestHandler STextDocumentReferences $ \req responder -> do
      let RequestMessage _ _ _ (ReferenceParams doc pos _ _ _) = req
      word <- liftIO $ wordAtDocPosition doc pos
      res <- liftIO $ globalDR word "r"
      responder $ Right res
  , requestHandler STextDocumentDocumentSymbol $ \req responder -> do
      let RequestMessage _ _ _ (DocumentSymbolParams _ _ doc) = req
          TextDocumentIdentifier uri = doc
          Just path = uriToFilePath uri
          results = do
            (_, Just out, _, _) <- createProcess (proc "global" ["--verbose", "--result=cscope", "-af", path])
                { cwd = Just ".", std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
            content <- hGetContents out
            content' <- evaluate $ force content
            return $ lines content'
      results' <- liftIO results
      let symbols = do
            line <- results'
            let [_, name, l] = take 3 $ words line
                l' = read l - 1
                pos = Position l' 0
                r = Range pos pos
                loc = Location uri r
            return $ SymbolInformation (T.pack name) SkNull Nothing Nothing loc Nothing
      responder $ Right $ InR $ List symbols
  , requestHandler SWorkspaceSymbol $ \req responder -> do
      let RequestMessage _ _ _ (WorkspaceSymbolParams _ _ query) = req
          results = do
            (_, Just out, _, _) <- createProcess (proc "global" ["--verbose", "--result=cscope", "-a", ".*" ++ T.unpack query ++ ".*"])
                { cwd = Just ".", std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
            content <- hGetContents out
            content' <- evaluate $ force content
            return $ lines content'
      results' <- liftIO results
      let symbols = do
            line <- results'
            let [path, name, l] = take 3 $ words line
                l' = read l - 1
                pos = Position l' 0
                r = Range pos pos
                uri = filePathToUri path
                loc = Location uri r
            return $ SymbolInformation (T.pack name) SkNull Nothing Nothing loc Nothing
      responder $ Right $ List symbols
  ]

globalDR:: String -> String -> IO (List Location)
globalDR word arg = do 
      (_, Just out, _, _) <- createProcess (proc "global" ["--verbose", "--result=cscope", "-a" ++ arg, word])
        { cwd = Just ".", std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
      content <- hGetContents out
      results <- evaluate $ force content
      let 
          locs:: [Location]
          locs = do
            line <- lines results
            let ([path, _, l] , contents) = splitAt 3 $ words line
                line_number = read l - 1
                content = unwords contents
                doc' = filePathToUri path
                pos' = Position line_number 0
            return (Location doc' $ Range pos' pos')
      return $ List locs

wordAtDocPosition:: TextDocumentIdentifier -> Position -> IO String
wordAtDocPosition (TextDocumentIdentifier uri) (Position l c) = do
     let Just path = uriToFilePath uri
     content <- lines <$> readFile path
     let line = content !! fromIntegral l
         c' = fromIntegral c
         (a, b) = splitAt c' line
         re_end_word = "^[A-Za-z_0-9]*" :: String
         re_start_word = "[A-Za-z_0-9]*$" :: String
         start_word = a =~ re_start_word :: String
         end_word = b =~ re_end_word :: String
         word = start_word ++ end_word
     case line !! c' of
          ' ' -> return ""
          _ -> return word

