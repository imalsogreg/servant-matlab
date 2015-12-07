module Servant.Matlab.Functions where

import           Control.Lens
import           Data.Bool (bool)
import           Data.List
import           Data.Monoid
import           Servant.API (Get(..),Post(..),Put(..),Delete(..))
import           Servant.Foreign
import           Servant.Matlab.Internal
import           System.FilePath ((</>))

-- | Generate matlab functions to make REST requests
--   to your API, using webread and webwrite. Uses 'defCommonGeneratorOptions'
--   for the 'CommonGeneratorOptions'.
matlabFunctions :: MatlabGenerator
matlabFunctions = map generateMatlabFunctions

-- | Generate matlab functions to make REST requests
--   to your API, using webread and webwrite. Lets you specify your
--   own options.
matlabFunctionsWith :: CommonGeneratorOptions -> MatlabGenerator
matlabFunctionsWith opts =
  map (generateMatlabPairWith opts)

-- | matlab codegen using webread/webwrite using default generation options
generateMatlabFunctions :: AjaxReq -> (String, String)
generateMatlabFunctions = generateMatlabPairWith defCommonGeneratorOptions

generateMatlabPairWith :: CommonGeneratorOptions -> AjaxReq -> (String,String)
generateMatlabPairWith opts req = (n,f)
  where n = generateMatlabFileNameWith opts req
        f = generateMatlabFunctionWith opts req

generateMatlabFileNameWith :: CommonGeneratorOptions -> AjaxReq -> String
generateMatlabFileNameWith opts req = functionName opts req <> ".m"

-- | matlab codegen using webread/webwrite
generateMatlabFunctionWith :: CommonGeneratorOptions -> AjaxReq -> String
generateMatlabFunctionWith opts req =
    "function r = " <> fname <> "(" <> argsStr <> ")\n"

     <> "  u = java.net.URL([url, " <> url <> "]);\n"
     <> "  conn = u.openConnection();\n"
     <>    unlines (map headerStr (req ^. reqHeaders))
     <>    "  conn.setRequestProperty('Authorization', key)"

           -- This sends the request's body
     <>    bool "" buildAndSendStream (req ^. reqBody)

           -- This gives a varabiable named `str` with the
           -- response body
     <>    buildAndEatStream
     <> "  r = parse_json(str)\n"
     <> "end\n\n"

  where argsStr = intercalate ", " ("url" : "key" : args)
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

        fname = functionName opts req

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


functionName :: CommonGeneratorOptions -> AjaxReq -> String
functionName opts req = namespace <> functionNameBuilder opts (req ^. funcName)
  where namespace = bool (moduleName opts <> ".") "" (null $ moduleName opts)

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
  ,"str = [];"
  ,"doneReading = false;"
  ,"while (~doneReading)"
  ,"  newStr = in.readLine();"
  ,"  str    = [str, newStr];"
  ,"  if (isempty(newStr))"
  ,"    doneReading = true;"
  ,"  end"
  ,"end\n"
  ]
