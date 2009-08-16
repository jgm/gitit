module TexMath (plugin) where

-- This plugin converts math to MathML. The script
-- MathMLinHTMLforFirefoxAndIE.js (available with the texmath source)
-- must be linked into every page for this to work (modify your
-- templates to include it).

import Network.Gitit.Interface
import Text.XML.Light
import Text.TeXMath
import Data.Char (toUpper)

plugin :: Plugin
plugin = mkPageTransform texMathTransform

texMathTransform :: Inline -> Inline
texMathTransform (Math t x) =
  case texMathToMathML t' x of
       Left e  -> Math t x 
       Right v -> HtmlInline $ ppElement v
    where t' = if t == DisplayMath then DisplayBlock else DisplayInline
texMathTransform x = x
