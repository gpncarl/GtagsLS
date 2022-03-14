{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module GtagsLS
    ( handlers
    ) where

import Language.LSP.Server
import Language.LSP.Types
import Language.LSP.Types.Capabilities
import Control.Monad.IO.Class
import qualified Data.Text as T
import Text.Regex.PCRE
import Text.Regex.PCRE.String
import System.Process
import System.IO
import Control.Exception (evaluate)
import Control.DeepSeq (force)

handlers :: Handlers (LspM ())
-- makeExtendingDatatype "InitializeParams" [''WorkDoneProgressParams]
--   [ ("_processId",             [t| Maybe Int32|])
--   , ("_clientInfo",            [t| Maybe ClientInfo |])
--   , ("_rootPath",              [t| Maybe Text |])
--   , ("_rootUri",               [t| Maybe Uri  |])
--   , ("_initializationOptions", [t| Maybe Value |])
--   , ("_capabilities",          [t| ClientCapabilities |])
--   , ("_trace",                 [t| Maybe Trace |])
--   , ("_workspaceFolders",      [t| Maybe (List WorkspaceFolder) |])
--   ]
handlers = mconcat
  [ requestHandler SInitialize $ \req responder -> do
      env <- getLspEnv 
      let RequestMessage _ _ _ (InitializeParams _ _ _ _ rootUri _ _ _ _) = req
          rootPath = rootUri >>= uriToFilePath 
      liftIO $ runLspT (env { resRootPath = rootPath }) (return ())
      responder $ Right $ InitializeResult
        { _capabilities = ServerCapabilities
            { -- | Defines how text documents are synced. Is either a detailed structure
              -- defining each notification or for backwards compatibility the
              -- 'TextDocumentSyncKind' number.
              -- If omitted it defaults to 'TdSyncNone'.
              _textDocumentSync                 = Nothing
              -- | The server provides hover support.
            , _hoverProvider                    = Nothing
              -- | The server provides completion support.
            , _completionProvider               = Nothing
              -- | The server provides signature help support.
            , _signatureHelpProvider            = Nothing
              -- | The server provides go to declaration support.
              --
              -- Since LSP 3.14.0
            , _declarationProvider              = Nothing
              -- | The server provides goto definition support.
            , _definitionProvider               = Just $ InL True
              -- | The server provides Goto Type Definition support. Since LSP 3.6
              --
              -- @since 0.7.0.0
            , _typeDefinitionProvider           = Nothing
              -- | The server provides Goto Implementation support. Since LSP 3.6
              --
              -- @since 0.7.0.0
            , _implementationProvider           = Nothing
              -- | The server provides find references support.
            , _referencesProvider               = Just $ InL True
              -- | The server provides document highlight support.
            , _documentHighlightProvider        = Nothing
              -- | The server provides document symbol support.
            , _documentSymbolProvider           = Just $ InL True
              -- | The server provides code actions.
            , _codeActionProvider               = Nothing
              -- | The server provides code lens.
            , _codeLensProvider                 = Nothing
              -- | The server provides document link support.
            , _documentLinkProvider             = Nothing
              -- | The server provides color provider support. Since LSP 3.6
              --
              -- @since 0.7.0.0
            , _colorProvider                    = Nothing
              -- | The server provides document formatting.
            , _documentFormattingProvider       = Nothing
              -- | The server provides document range formatting.
            , _documentRangeFormattingProvider  = Nothing
              -- | The server provides document formatting on typing.
            , _documentOnTypeFormattingProvider = Nothing
              -- | The server provides rename support.
            , _renameProvider                   = Nothing
              -- | The server provides folding provider support. Since LSP 3.10
              --
              -- @since 0.7.0.0
            , _foldingRangeProvider             = Nothing
              -- | The server provides execute command support.
            , _executeCommandProvider           = Nothing
              -- | The server provides selection range support. Since LSP 3.15
            , _selectionRangeProvider           = Nothing
              -- | The server provides call hierarchy support.
            -- , _callHierarchyProvider            = Nothing
            --   -- | The server provides semantic tokens support.
            --   --
            --   -- @since 3.16.0
            -- , _semanticTokensProvider           = Nothing
              -- | The server provides workspace symbol support.
            , _workspaceSymbolProvider          = Just True
              -- | Workspace specific server capabilities
            , _workspace                        = Nothing
              -- | Experimental server capabilities.
            , _experimental                     = Nothing
            }
            , _serverInfo = Nothing
        }
  , notificationHandler STextDocumentDidSave $ \notify -> do
      let NotificationMessage _ _ (DidSaveTextDocumentParams doc _) = notify
          TextDocumentIdentifier uri = doc
          Just path = uriToFilePath uri
      env <- getLspEnv 
      liftIO $ createProcess (proc "global" ["--verbose", "--single-update", path])
                { cwd = resRootPath env, std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
      return ()
  , requestHandler STextDocumentDefinition $ \req responder -> do
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

