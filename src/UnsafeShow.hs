module UnsafeShow where

import Outputable
import System.IO.Unsafe

class IOShow a where
  ioshow :: a -> IO String

instance Show a => IOShow a where
  ioshow = return . show

instance IOShow a => Show a where
  show = unsafePerformIO . ioshow

instance Outputable a => IOShow a where
  ioshow x = inSession $ do df <- getSessionDynFlags
                            return (showSDoc df (ppr x))

instance Show ParsedSource where
  show x = undefined -- ppr x

instance Show TypecheckedSource where
  show x = undefined
