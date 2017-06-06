module Config where
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT)


newtype App a = App {runApp :: ReaderT}
