module Servant.Matlab.Internal
  ( MatlabGenerator
  , CommonGeneratorOptions(..)
  , defCommonGeneratorOptions
  , devCommonGeneratorOptions
  , AjaxReq
  , matlabSegments
  , segmentToStr
  , segmentTypeToStr
  , matlabParams
  , matlabGParams
  , matlabMParams
  , paramToStr
  , toValidFunctionName
  , toMatlabHeader
  -- re-exports
  , (:<|>)(..)
  , (:>)
  , defReq
  , reqHeaders
  , HasForeign(..)
  , HeaderArg(..)
  , concatCase
  , snakeCase
  , camelCase
  , ReqBody
  , JSON
  , FormUrlEncoded
  , Post
  , Get
  , Raw
  , Header
  ) where

import           Control.Lens                  ((^.))
import qualified Data.CharSet as Set
import qualified Data.CharSet.Unicode.Category as Set
import           Data.List
import           Data.Monoid
import qualified Data.Text as T
import           Servant.Foreign

type AjaxReq = Req

-- A 'MatlabGenerator' just takes the data found in the API type
-- for each endpoint and generates Javascript code in a String. Several
-- generators are available in this package.
type MatlabGenerator = [Req] -> String

-- | This structure is used by specific implementations to let you
-- customize the output
data CommonGeneratorOptions = CommonGeneratorOptions
  {
    functionNameBuilder :: FunctionName -> String
    -- ^ function generating function names
  , requestBody :: String
    -- ^ name used when a user want to send the request body (to let you redefine it)
  , moduleName :: String
    -- ^ namespace on which we define the foreign function (empty mean local var)
  , urlPrefix :: String
    -- ^ a prefix we should add to the Url in the codegen
  }

-- | Default options.
--
-- @
-- > defCommonGeneratorOptions = CommonGeneratorOptions
-- >   { functionNameBuilder = camelCase
-- >   , requestBody = "body"
-- >   , moduleName = ""
-- >   , urlPrefix = ""
-- >   }
-- @
defCommonGeneratorOptions :: CommonGeneratorOptions
defCommonGeneratorOptions = CommonGeneratorOptions
  {
    functionNameBuilder = camelCase
  , requestBody = "body"
  , moduleName = ""
  , urlPrefix = "http://cbaas.org/"
  }

devCommonGeneratorOptions :: CommonGeneratorOptions
devCommonGeneratorOptions =
  defCommonGeneratorOptions {urlPrefix = "http://localhost:8000/"}

-- | Attempts to reduce the function name provided to that allowed by @'Foreign'@.
--
-- https://mathiasbynens.be/notes/javascript-identifiers
-- Couldn't work out how to handle zero-width characters.
--
-- @TODO: specify better default function name, or throw error?
toValidFunctionName :: String -> String
toValidFunctionName (x:xs) = [setFirstChar x] <> filter remainder xs
  where
    setFirstChar c = if firstChar c then c else '_'
    firstChar c = prefixOK c || any (Set.member c) firstLetterOK
    remainder c = prefixOK c || any (Set.member c) remainderOK
    -- Valid prefixes
    prefixOK c = c `elem` ['$','_']
    -- Unicode character sets
    firstLetterOK = [ Set.lowercaseLetter
                    , Set.uppercaseLetter
                    , Set.titlecaseLetter
                    , Set.modifierLetter
                    , Set.otherLetter
                    , Set.letterNumber ]
    remainderOK   = firstLetterOK
               <> [ Set.nonSpacingMark
                  , Set.spacingCombiningMark
                  , Set.decimalNumber
                  , Set.connectorPunctuation ]
toValidFunctionName [] = "_"

toMatlabHeader :: HeaderArg -> String
toMatlabHeader (HeaderArg n)          = toValidFunctionName ("header" <> n)
toMatlabHeader (ReplaceHeaderArg n p)
  | pn `isPrefixOf` p = pv <> " + \"" <> rp <> "\""
  | pn `isSuffixOf` p = "\"" <> rp <> "\" + " <> pv
  | pn `isInfixOf` p  = "\"" <> (replace pn ("\" + " <> pv <> " + \"") p)
                             <> "\""
  | otherwise         = p
  where
    pv = toValidFunctionName ("header" <> n)
    pn = "{" <> n <> "}"
    rp = replace pn "" p
    -- Use replace method from Data.Text
    replace old new = T.unpack
                    . T.replace (T.pack old) (T.pack new)
                    . T.pack

matlabSegments :: [Segment] -> String
matlabSegments []  = ""
matlabSegments [x] = "/" ++ segmentToStr x False
matlabSegments (x:xs) = "/" ++ segmentToStr x True ++ matlabSegments xs

segmentToStr :: Segment -> Bool -> String
segmentToStr (Segment st ms) notTheEnd =
  segmentTypeToStr st ++ matlabMParams ms ++ if notTheEnd then "" else "'"

segmentTypeToStr :: SegmentType -> String
segmentTypeToStr (Static s) = s
segmentTypeToStr (Cap s)    = "' + encodeURIComponent(" ++ s ++ ") + '"

matlabGParams :: String -> [QueryArg] -> String
matlabGParams _ []     = ""
matlabGParams _ [x]    = paramToStr x False
matlabGParams s (x:xs) = paramToStr x True ++ s ++ matlabGParams s xs

matlabParams :: [QueryArg] -> String
matlabParams = matlabGParams "&"

matlabMParams :: [MatrixArg] -> String
matlabMParams [] = ""
matlabMParams xs = ";" ++ matlabGParams ";" xs

paramToStr :: QueryArg -> Bool -> String
paramToStr qarg notTheEnd =
  case qarg ^. argType of
    Normal -> name
           ++ "=' + encodeURIComponent("
           ++ name
           ++ if notTheEnd then ") + '" else ")"
    Flag   -> name ++ "="
    List   -> name
           ++ "[]=' + encodeURIComponent("
           ++ name
           ++ if notTheEnd then ") + '" else ")"
  where name = qarg ^. argName
