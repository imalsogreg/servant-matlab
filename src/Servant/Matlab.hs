{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Servant.Matlab
-- License     :  BSD3
-- Maintainer  :  Greg Hale <imalsogreg@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Generating Matlab code to query your APIs
--
-- Using this package is very simple. Say you have this API type around:
--
-- > type API = "users" :> Get '[JSON] [Users]
-- >       :<|> "messages" :> Get '[JSON] [Message]
--
-- All you need to do to generate the Javascript code is to write a 'Proxy'
-- for this API type:
--
-- > api :: Proxy API
-- > api = Proxy
--
--
-- @
-- matlab :: String
-- matlab = 'matlabForAPI' api defaultOptions
-- @
--
-- That's it! If you want to write that code to a file:
--
-- @
-- writeMatlabCode :: IO ()
-- writeMatlabCode = 'writeMatlabForAPI' api defaultOptions "./my_api.m"
-- @
--

-- -- TODO change this section
-- If you want to customize the rendering options, take a look
-- at 'CommonGeneratorOptions' which are generic options common to all the
-- generators. the /xxxWith/ variants all take 'CommonGeneratorOptions' whereas
-- the /xxx/ versions use 'defCommonGeneratorOptions'. Once you have some custom
--
-- > myOptions :: 'CommonGeneratorOptions'
--
-- All you need to do to use it is to use 'vanillaJSWith' and pass it @myOptions@.
--
-- @
-- jsCodeWithMyOptions :: String
-- jsCodeWithMyOptions = 'jsForAPI' api ('vanillaJSWith' myOptions)
-- @
--
-- Follow the same pattern for any other generator.
--
-- /Note/: The Angular generators take an additional type of options,
-- namely 'AngularOptions', to let you tweak aspects of the code generation
-- that are specific to /Angular.js/.
module Servant.Matlab
  ( -- * Generating javascript code from an API type
    matlabForAPI
  , writeMatlabForAPI
  , MatlabGenerator

  , -- * Options common to all generators
    CommonGeneratorOptions
  , defCommonGeneratorOptions

  , -- * Function renamers
    concatCase
  , snakeCase
  , camelCase

  , -- * Misc.
    listFromAPI
  , matlab
  , GenerateList(..)
  ) where

import           Data.Bool               (bool)
import           Data.Proxy
import           Servant.Foreign
import           Servant.Matlab.Internal
import           System.Directory        (createDirectory)
import           System.FilePath         ((</>))

-- | Generate the data necessary to generate javascript code
--   for all the endpoints of an API, as ':<|>'-separated values
--   of type 'AjaxReq'.
matlab :: HasForeign layout => Proxy layout -> Foreign layout
matlab p = foreignFor p defReq

-- | Directly generate all the javascript functions for your API
--   from a 'Proxy' for your API type. You can then write it to
--   a file or integrate it in a page, for example.
matlabForAPI :: (HasForeign api, GenerateList (Foreign api))
             => Proxy api
                -- ^ proxy for your API type
             -> MatlabGenerator
                -- ^ matlab code generator to use
             -> [(String, String)]
                -- ^ a string that you can embed in your pages or write to a file
matlabForAPI p gen = gen (listFromAPI p)

-- | Directly generate all the javascript functions for your API
--   from a 'Proxy' for your API type using the given generator
--   and write the resulting code to a file at the given path.
writeMatlabForAPI :: (HasForeign api, GenerateList (Foreign api))
                  => Proxy api
                        -- ^ proxy for your API type
                     -> MatlabGenerator
                        -- ^ matlab code generator to use
                     -> FilePath
                        -- ^ path to the file you want to write the resulting matlab code into
              -> IO ()
writeMatlabForAPI p gen fp = do
  mapM_ (\(f,c) -> writeFile (fp </> f) c) (matlabForAPI p gen)

-- | Utility class used by 'matlabForAPI' which computes
--   the data needed to generate a function for each endpoint
--   and hands it all back in a list.
class GenerateList reqs where
  generateList :: reqs -> [AjaxReq]

instance GenerateList AjaxReq where
  generateList r = [r]

instance (GenerateList start, GenerateList rest) => GenerateList (start :<|> rest) where
  generateList (start :<|> rest) = (generateList start) ++ (generateList rest)

-- | Generate the necessary data for JS codegen as a list, each 'AjaxReq'
--   describing one endpoint from your API type.
listFromAPI :: (HasForeign api, GenerateList (Foreign api)) => Proxy api -> [AjaxReq]
listFromAPI p = generateList (matlab p)
