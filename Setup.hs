import Control.Arrow
import Control.Monad (filterM, forM)
import Data.Char
import Data.List
import Data.Ord
import Distribution.Simple
import Distribution.Simple.Utils
import Distribution.Verbosity
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , getDirectoryContents
  )
import System.FilePath hiding (combine)

-- A pre-build hook to automatically generate Solutions.all, an array
-- containing all solutions in `src/Solutions`.
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pkg lbi hooks flags -> do
     writeAllSolutions
     buildHook simpleUserHooks pkg lbi hooks flags
  }

toModuleName x = intercalate "." . ("Solutions":) . splitDirectories . dropExtension $ x

writeAllSolutions :: IO ()
writeAllSolutions = do
  let baseDir = "src/Solutions"

  files <- getFilesRecursive baseDir

  let header = unlines
                 [ "module Solutions where"
                 , "import Control.Lens (view)"
                 , "import Dovin.Types (stepNumber)"
                 ]

  modules <- forM files $ \filename -> do
    contents <- readFile $ baseDir </> filename

    let v2 = not $ "Dovin.V1" `isInfixOf` contents

    return (toModuleName filename, v2)

  let imports = unlines . map (("import qualified " <>) . fst) $ modules


  let array = "all = [\n  " <> (intercalate ",\n  " . map formatSolution $ modules) <> "\n  ]"

  let contents = unlines [header, imports, array]
  rewriteFileEx normal "src/Solutions.hs" contents

  where
    formatSolution (m, v2) = "("
      <> show (drop (length "Solutions.") m) <> ", "
      <> m <> ".solution, "
      <> (if v2
           then m <> ".formatter"
           else m <> ".formatter . view stepNumber")
      <> ")"

-- All these functions copy+pasted from hspec-discover
getFilesRecursive :: FilePath -> IO [FilePath]
getFilesRecursive baseDir = sortNaturally <$> go []

  where
    go :: FilePath -> IO [FilePath]
    go dir = do
      c <- map (dir </>) . filter (`notElem` [".", ".."]) <$> getDirectoryContents (baseDir </> dir)
      dirs <- filterM (doesDirectoryExist . (baseDir </>)) c >>= mapM go
      files <- filterM (doesFileExist . (baseDir </>)) c
      return (files ++ concat dirs)

sortNaturally :: [String] -> [String]
sortNaturally = sortBy (comparing naturalSortKey)

data NaturalSortKey = NaturalSortKey [Chunk]
  deriving (Eq, Ord)

data Chunk = Numeric Integer Int | Textual [(Char, Char)]
  deriving (Eq, Ord)

naturalSortKey :: String -> NaturalSortKey
naturalSortKey = NaturalSortKey . chunks
  where
    chunks [] = []
    chunks s@(c:_)
      | isDigit c = Numeric (read num) (length num) : chunks afterNum
      | otherwise = Textual (map (toLower &&& id) str) : chunks afterStr
      where
        (num, afterNum) = span  isDigit s
        (str, afterStr) = break isDigit s
