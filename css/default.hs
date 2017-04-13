{-# LANGUAGE OverloadedStrings #-}

import Clay
import Clay.Border
import Clay.Text
import Prelude hiding (div, span)

main :: IO ()
main = putCss defaultStylesheet 

blueHighlight = "#1081a8"

defaultStylesheet :: Css
defaultStylesheet = do
  body ? do
    color black
    fontSize (px 16)
    width (pct 80)
    maxWidth (px 1024)
    sym2 margin (px 0) auto

  div # "#header" ? do
    borderBottom solid (px 2) blueHighlight
    marginBottom (px 30)
    sym2 padding (px 12) (px 0)

    "#navigation" ? do
      textAlign (alignSide sideRight)

      a ? do
        color black
        fontSize (px 18)
        fontWeight bold
        marginLeft (px 12)
        textDecoration none
        textTransform uppercase

  div # "#logo" |> a ? do
    color black
    fontSize (px 18)
    fontWeight bold
    textDecoration none
    img ? do
      width (pct 100)

  div # "#footer" ? do
    borderTop solid (px 2) blueHighlight
    color "#555"
    fontSize (px 12)
    marginTop (px 30)
    sym2 padding (px 12) (px 0)
    textAlign (alignSide sideRight)

  h1 ? do
    fontSize (px 24)

  h2 ? do
    fontSize (px 20)

  div # ".info" ? do
    color "#555"
    fontSize (px 14)
    fontStyle italic

