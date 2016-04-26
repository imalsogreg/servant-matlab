{-# LANGUAGE OverloadedStrings #-}

module Servant.Matlab.Functions where

-- import           Control.Arrow (first)
-- import           Control.Lens
-- import           Data.Bool (bool)
-- import           Data.List
-- import           Data.Maybe
-- import           Data.Monoid
-- import           Data.Text (Text)
-- import qualified Data.Text as T
-- import           Servant.API (Get(..),Post(..),Put(..),Delete(..))
-- import           Servant.Foreign
-- import           Servant.Matlab.Internal
-- import           System.FilePath ((</>))

-- -- | Generate matlab functions to make REST requests
-- --   to your API, using webread and webwrite. Uses 'defCommonGeneratorOptions'
-- --   for the 'CommonGeneratorOptions'.
-- matlabFunctions :: MatlabGenerator
-- matlabFunctions = undefined -- map generateMatlabFunctions

-- -- | Generate matlab functions to make REST requests
-- --   to your API, using webread and webwrite. Lets you specify your
-- --   own options.
-- matlabFunctionsWith :: CommonGeneratorOptions -> MatlabGenerator
-- matlabFunctionsWith opts = undefined
--   -- map (generateMatlabPairWith opts)

-- -- | matlab codegen using webread/webwrite using default generation options
-- generateMatlabFunctions :: AjaxReq -> (Text, Text)
-- generateMatlabFunctions = generateMatlabPairWith defCommonGeneratorOptions

-- generateMatlabPairWith :: CommonGeneratorOptions -> AjaxReq -> (Text,Text)
-- generateMatlabPairWith opts req = (n,f)
--   where n = generateMatlabFileNameWith opts req
--         f = generateMatlabFunctionWith opts req

-- generateMatlabFileNameWith :: CommonGeneratorOptions -> AjaxReq -> Text
-- generateMatlabFileNameWith opts req = functionName opts req <> ".m"

-- -- | matlab codegen using webread/webwrite
-- generateMatlabFunctionWith :: CommonGeneratorOptions -> AjaxReq -> Text
-- generateMatlabFunctionWith opts req
--   -- In GET and POST requests, use matlab's webread/webwrite
--   | req ^. reqMethod  == "GET" =
--     "function r = " <> fname <> "(" <> argsStr <> ")\n"
--     <> "  opts = weboptions();"
--     <> T.unlines (map weboptionHeader (req ^. reqHeaders ))
--     <> "\n"
--     <> "  r = webread(" <> url <> ", opts);\n"
--     <> "end\n\n"

--   | req ^. reqMethod == "POST" =
--        "function r = " <> fname <> "(" <> argsStr <> ")\n"
--     <> "  opts = weboptions();"
--     <> T.unlines (map weboptionHeader (req ^. reqHeaders))
--     <> "\n"
--     <> "  r = webwrite(" <> url <> dataBody <> ", opts);\nend\n\n"

--   | req ^. reqMethod == "PUT" =
--     "function r = " <> fname <> "(" <> argsStr <> ")\n"

--      <> "  u = java.net.URL(" <> url <> ");\n"
--      <> "  conn = u.openConnection();\n"
--      <>    T.unlines (map headerStr (req ^. reqHeaders))

--            -- This sends the PUT request's body
--      <> undefined -- <>    bool "" buildAndSendStream (req ^. reqBody)

--            -- This gives a varabiable named `str` with the
--            -- response body
--      <>    buildAndEatStream
--      <> "  r = parse_json(str)\n"
--      <> "end\n\n"

--   | req ^. reqMethod == "DELETE" =
--     "function r = " <> fname <> "(" <> argsStr <> ")\n"

--      <> "  u = java.net.URL(" <> url <> ");\n"
--      <> "  conn = u.openConnection();\n"
--      -- <>    T.unlines (map headerStr (req ^. reqHeaders))
--      <>    buildAndEatStream -- This gives a varabiable named `str` with the
--                              -- response body
--      <> "  r = parse_json(str)\n"
--      <> "end\n\n"
--   | otherwise = "% stub for function" <> fname
--                 <> undefined -- <> " with unrecognized request method " <> (req ^. reqMethod)
--                 <> "\n"

--   where argsStr = undefined
--   -- where argsStr = T.intercalate ", " args
--   --       args = captures
--   --           <> map (view argName) queryparams
--   --           <> body
--   --           -- <> map (toValidFunctionName . (<>) "header" . "") hs -- TODO Use header val

--         captures = map captureArg
--                  . filter isCapture
--                  $ req ^. reqUrl.path

--         hs = req ^. reqHeaders

--         queryparams = req ^.. reqUrl.queryStr.traverse

--         body = fromMaybe "" $ req ^. reqBody

--         dataBody =
--           if isJust $ req ^. reqBody
--             then ", body"
--             else ""

--         headerStr h = ""
--         -- headerStr h = "  opts('" ++ headerArgName h ++ "\', "
--         --            <> " '" ++ toMatlabHeader h ++ "');"

--         fname = functionName opts req

--         url = if url' == "'" then "'/'" else url'
--         url' = "'"
--            <> urlPrefix opts
--            <> urlArgs
--            <> queryArgs

--         urlArgs = matlabSegments
--                 $ req ^.. reqUrl.path.traverse

--         queryArgs = if null queryparams
--                       then ""
--                       else " + '?" <> matlabParams queryparams

--         weboptionHeader h = "weboptions()"
--         -- weboptionHeader h = "  weboptions('"
--         --                     <> headerArgName h <> ", "
--         --                     <> toMatlabHeader h  <> "');\n"


-- functionName :: CommonGeneratorOptions -> AjaxReq -> Text
-- functionName opts req = namespace <> functionNameBuilder opts (req ^. funcName)
--   where namespace = bool (moduleName opts <> ".") "" (T.null $ moduleName opts)

-- buildAndSendStream :: Text
-- buildAndSendStream = ("\n" <>) . T.unlines . map ("  " <>) $
--   ["bodyStr = urlencode(encode_json(body));"
--   , "if ~(isempty (bodyStr))"
--   , "  conn.setDoOutput(true);"
--   , "  conn.addRequestProperty('Content-Type',"
--     <>  "'application/json; charset=utf-8');"
--   , "  conn.getOutputStream.write(bodyStr);"
--   , "end"
--   ]


-- buildAndEatStream :: Text
-- buildAndEatStream = ("\n" <>) . T.unlines . map ("  " <>) $
--   ["in = java.io.BufferedReader ..."
--   ,"     (java.io.InputStreamReader (conn.getInputStream ()));"
--   ,"doneReading = false;"
--   ,"while (~doneReading)"
--   ,"  newStr = in.readLine();"
--   ,"  str    = [str, newStr]"
--   ,"  if (isempty(str))"
--   ,"    doneReading = true;"
--   ,"  end"
--   ,"end\n"
--   ]
