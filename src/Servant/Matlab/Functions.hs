module Servant.Matlab.Functions where

import           Control.Lens
import           Data.Bool (bool)
import           Data.List
import           Data.Monoid
import           Servant.API (Get(..),Post(..),Put(..),Delete(..))
import           Servant.Foreign
import           Servant.Matlab.Internal

-- | Generate matlab functions to make REST requests
--   to your API, using webread and webwrite. Uses 'defCommonGeneratorOptions'
--   for the 'CommonGeneratorOptions'.
matlabFunctions :: MatlabGenerator
matlabFunctions = concatMap generateMatlabFunctions

-- | Generate matlab functions to make REST requests
--   to your API, using webread and webwrite. Lets you specify your
--   own options.
matlabFunctionsWith :: CommonGeneratorOptions -> MatlabGenerator
matlabFunctionsWith opts =
  concatMap $ generateMatlabFunctionWith opts

-- | matlab codegen using webread/webwrite using default generation options
generateMatlabFunctions :: AjaxReq -> String
generateMatlabFunctions = generateMatlabFunctionWith defCommonGeneratorOptions

-- | matlab codegen using webread/webwrite
generateMatlabFunctionWith :: CommonGeneratorOptions -> AjaxReq -> String
generateMatlabFunctionWith opts req
  -- In GET and POST requests, use matlab's webread/webwrite
  | req ^. reqMethod  == "GET" =
    "function r = " <> fname <> "(" <> argsStr <> ")\n"
    <> "  opts = weboptions();"
    <> unlines (map weboptionHeader (req ^. reqHeaders ))
    <> "\n"
    <> "  r = webread(" <> url <> ", opts);\n"
    <> "end\n\n"

  | req ^. reqMethod == "POST" =
       "function r = " <> fname <> "(" <> argsStr <> ")\n"
    <> "  opts = weboptions();"
    <> unlines (map weboptionHeader (req ^. reqHeaders))
    <> "\n"
    <> "  r = webwrite(" <> url <> dataBody <> ", opts);\nend\n\n"

  | req ^. reqMethod == "PUT" =
    "function r = " <> fname <> "(" <> argsStr <> ")\n"

     <> "  u = java.net.URL(" <> url <> ");\n"
     <> "  conn = u.openConnection();\n"
     <>    unlines (map headerStr (req ^. reqHeaders))

           -- This sends the PUT request's body
     <>    bool "" buildAndSendStream (req ^. reqBody)

           -- This gives a varabiable named `str` with the
           -- response body
     <>    buildAndEatStream
     <> "  r = parse_json(str)\n"
     <> "end\n\n"

  | req ^. reqMethod == "DELETE" =
    "function r = " <> fname <> "(" <> argsStr <> ")\n"

     <> "  u = java.net.URL(" <> url <> ");\n"
     <> "  conn = u.openConnection();\n"
     <>    unlines (map headerStr (req ^. reqHeaders))
     <>    buildAndEatStream -- This gives a varabiable named `str` with the
                             -- response body
     <> "  r = parse_json(str)\n"
     <> "end\n\n"
  | otherwise = "% stub for function" <> fname
                <> " with unrecognized request method " <> (req ^. reqMethod)
                <> "\n"

  where argsStr = intercalate ", " args
        args = captures
            ++ map (view argName) queryparams
            ++ body
            ++ map (toValidFunctionName . (<>) "header" . headerArgName) hs

        captures = map captureArg
                 . filter isCapture
                 $ req ^. reqUrl.path

        hs = req ^. reqHeaders

        queryparams = req ^.. reqUrl.queryStr.traverse

        body = [requestBody opts | req ^. reqBody]

        dataBody =
          if req ^. reqBody
            then ", body"
            else ""

        headerStr h = "  opts('" ++ headerArgName h ++ "\', "
                   <> " '" ++ toMatlabHeader h ++ "');"

        namespace = if null (moduleName opts)
                       then ""
                       else moduleName opts <> "."

        fname = namespace <> functionNameBuilder opts (req ^. funcName)

        url = if url' == "'" then "'/'" else url'
        url' = "'"
           ++ urlPrefix opts
           ++ urlArgs
           ++ queryArgs

        urlArgs = matlabSegments
                $ req ^.. reqUrl.path.traverse

        queryArgs = if null queryparams
                      then ""
                      else " + '?" ++ matlabParams queryparams

        weboptionHeader h = "  weboptions('"
                            <> headerArgName h <> ", "
                            <> toMatlabHeader h  <> "');\n"


buildAndSendStream :: String
buildAndSendStream = ("\n" <>) . unlines . map ("  " <>) $
  ["bodyStr = urlencode(encode_json(body));"
  , "if ~(isempty (bodyStr))"
  , "  conn.setDoOutput(true);"
  , "  conn.addRequestProperty('Content-Type',"
    <>  "'application/json; charset=utf-8');"
  , "  conn.getOutputStream.write(bodyStr);"
  , "end"
  ]


buildAndEatStream :: String
buildAndEatStream = ("\n" <>) . unlines . map ("  " <>) $
  ["in = java.io.BufferedReader ..."
  ,"     (java.io.InputStreamReader (conn.getInputStream ()));"
  ,"doneReading = false;"
  ,"while (~doneReading)"
  ,"  newStr = in.readLine();"
  ,"  str    = [str, newStr]"
  ,"  if (isempty(str))"
  ,"    doneReading = true;"
  ,"  end"
  ,"end\n"
  ]
