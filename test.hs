import           Data.Char          (isSpace)
import           Data.List          (dropWhileEnd)
import           Data.Text          (strip)
import           System.Environment (getArgs)

data Snippet = Snippet
  { name      :: String
  , trigger   :: String
  , condition :: String
  , body      :: String
  , priority  :: Maybe Int
  }

