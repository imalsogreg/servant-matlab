{-# LANGUAGE OverloadedStrings #-}

module Servant.Matlab.Internal
  ( -- MatlabGenerator
  -- , CommonGeneratorOptions(..)
  -- , defCommonGeneratorOptions
  -- , AjaxReq
  -- , matlabSegments
  -- , segmentToStr
  -- , segmentTypeToStr
  -- , matlabParams
  -- , matlabGParams
  -- , paramToStr
  -- , toValidFunctionName
  -- , toMatlabHeader
  -- -- re-exports
  -- , (:<|>)(..)
  -- , (:>)
  -- , defReq
  -- , reqHeaders
  -- , HasForeign(..)
  -- , HeaderArg(..)
  -- , concatCase
  -- , snakeCase
  -- , camelCase
  -- , ReqBody
  -- , JSON
  -- , FormUrlEncoded
  -- , Post
  -- , Get
  -- , Raw
  -- , Header
  ) where

-- import           Control.Lens                  ((^.))
-- import qualified Data.CharSet as Set
-- import qualified Data.CharSet.Unicode.Category as Set
-- import           Data.List
-- import           Data.Monoid
-- import           Data.Text (Text)
-- import qualified Data.Text as T
-- import           Servant.Foreign

-- type AjaxReq = Req

-- -- A 'MatlabGenerator' just takes the data found in the API type
-- -- for each ndpoint and generates a filename and Matlab code in a String.
-- -- Several generators are available in this package.
-- type MatlabGenerator = [Req] -> [(String, String)]

-- -- | This structure is used by specific implementations to let you
-- -- customize the output
-- data CommonGeneratorOptions = CommonGeneratorOptions
--   {
--     functionNameBuilder :: FunctionName -> Text
--     -- ^ function generating function names
--   , requestBody :: Text
--     -- ^ name used when a user want to send the request body (to let you redefine it)
--   , moduleName :: Text
--     -- ^ namespace on which we define the foreign function (empty mean local var)
--   , urlPrefix :: Text
--     -- ^ a prefix we should add to the Url in the codegen
--   }

-- -- | Default options.
-- --
-- -- @
-- -- > defCommonGeneratorOptions = CommonGeneratorOptions
-- -- >   { functionNameBuilder = camelCase
-- -- >   , requestBody = "body"
-- -- >   , moduleName = ""
-- -- >   , urlPrefix = ""
-- -- >   }
-- -- @
-- defCommonGeneratorOptions :: CommonGeneratorOptions
-- defCommonGeneratorOptions = CommonGeneratorOptions
--   {
--     functionNameBuilder = camelCase
--   , requestBody = "body"
--   , moduleName  = ""
--   , urlPrefix   = ""
--   }

-- -- | Attempts to reduce the function name provided to that allowed by @'Foreign'@.
-- --
-- -- https://mathiasbynens.be/notes/javascript-identifiers
-- -- Couldn't work out how to handle zero-width characters.
-- --
-- -- @TODO: specify better default function name, or throw error?
-- toValidFunctionName :: Text -> Text
-- toValidFunctionName t = case T.uncons t of
--   Just (x,xs) -> setFirstChar x `T.cons` T.filter remainder xs
--   Nothing     -> "_"
--   where
--     setFirstChar c = if firstChar c then c else '_'
--     firstChar c = prefixOK c || any (Set.member c) firstLetterOK
--     remainder c = prefixOK c || any (Set.member c) remainderOK
--     -- Valid prefixes
--     prefixOK c = c `elem` ['$','_']
--     -- Unicode character sets
--     firstLetterOK = [ Set.lowercaseLetter
--                     , Set.uppercaseLetter
--                     , Set.titlecaseLetter
--                     , Set.modifierLetter
--                     , Set.otherLetter
--                     , Set.letterNumber ]
--     remainderOK   = firstLetterOK
--                <> [ Set.nonSpacingMark
--                   , Set.spacingCombiningMark
--                   , Set.decimalNumber
--                   , Set.connectorPunctuation ]

-- toMatlabHeader :: HeaderArg -> Text
-- toMatlabHeader (HeaderArg (n,_))   = toValidFunctionName ("header" <> n)
-- toMatlabHeader (ReplaceHeaderArg (n,_) p)
--   | pn `T.isPrefixOf` p = pv <> " + \"" <> rp <> "\""
--   | pn `T.isSuffixOf` p = "\"" <> rp <> "\" + " <> pv
--   | pn `T.isInfixOf` p  = "\"" <> (T.replace pn ("\" + " <> pv <> " + \"") p)
--                              <> "\""
--   | otherwise         = p
--   where
--     pv = toValidFunctionName ("header" <> n)
--     pn = "{" <> n <> "}"
--     rp = T.replace pn "" p
--     -- Use replace method from Data.Text
--     -- replace old new = T.unpack
--     --                 . T.replace (T.pack old) (T.pack new)
--     --                 . T.pack

-- matlabSegments :: [Segment] -> Text
-- matlabSegments []  = ""
-- matlabSegments [x] = "/" <> segmentToStr x False
-- matlabSegments (x:xs) = "/" <> segmentToStr x True <> matlabSegments xs

-- segmentToStr :: Segment -> Bool -> Text
-- segmentToStr (Segment st) notTheEnd =
--   segmentTypeToStr st <> if notTheEnd then "" else "'"

-- segmentTypeToStr :: SegmentType -> Text
-- segmentTypeToStr (Static s) = s
-- segmentTypeToStr (Cap s)    = "' + encodeURIComponent(" <> fst s <> ") + '"

-- matlabGParams :: Text -> [QueryArg] -> Text
-- matlabGParams _ []     = ""
-- matlabGParams _ [x]    = paramToStr x False
-- matlabGParams s (x:xs) = paramToStr x True <> s <> matlabGParams s xs

-- matlabParams :: [QueryArg] -> Text
-- matlabParams = matlabGParams "&"

-- paramToStr :: QueryArg -> Bool -> Text
-- paramToStr qarg notTheEnd =
--   case qarg ^. argType of
--     Normal -> name
--            <> "=' + encodeURIComponent("
--            <> name
--            <> if notTheEnd then ") + '" else ")"
--     Flag   -> name <> "="
--     List   -> name
--            <> "[]=' + encodeURIComponent("
--            <> name
--            <> if notTheEnd then ") + '" else ")"
--   where (name,_)  = qarg ^. argName
