{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

-- import           Cookbook.DbPool
-- import           ApiServer      hiding (startApp)
import           Cookbook.Types
import qualified Data.Text      as Text (pack)
import           Lib
import           Servant.Elm    (DefineElm (DefineElm), ElmOptions (urlPrefix),
                                 Proxy (Proxy), UrlPrefix (Static),
                                 defElmImports, defElmOptions,
                                 generateElmModuleWith)

--- spec :: Spec
--- spec = Spec ["Generated", "CoreApi"]
---            (defElmImports : generateElmForAPI (Proxy :: Proxy AppAPI))

--myElmOpts :: ElmOptions
--myElmOpts = defElmOptions { urlPrefix = Static $ "http://localhost" <> ":" <> (Text.pack . show) port }
--
--genElmClient :: IO ()
--genElmClient = generateElmModuleWith
--                myElmOpts
--                [ "Generated"
--                , "CoreApi"
--                ]
--                defElmImports "elm" [ DefineElm (Proxy :: Proxy User)
--                                    , DefineElm (Proxy :: Proxy Product)
--                                    ]
--                (Proxy :: Proxy TestAPI)
--
main :: IO ()
main = startApp -- genElmClient  --- specsToDir [spec] "elmCode"
