module Main where

import Language.LSP.Server
import Language.LSP.Types
import Control.Monad.IO.Class
import GtagsLS

main :: IO Int
main = runServer $ ServerDefinition
  { onConfigurationChange = const $ const $ Right ()
  , defaultConfig = ()
  , doInitialize = \env _req -> pure $ Right env
  , staticHandlers = handlers
  , interpretHandler = \env -> Iso (runLspT env) liftIO
  , options = defaultOptions
  }
