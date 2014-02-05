{-# LANGUAGE CPP #-}
module Network.Gitit.ContentTransformer where
import Network.Gitit.Types
import Text.StringTemplate (StringTemplate)

applyHSTMPPlugins :: StringTemplate String -> GititServerPart (StringTemplate String)
