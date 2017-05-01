{-# LANGUAGE OverloadedStrings #-}

import Clay
import Clay.Border
import Clay.Text
import Clay.Display
import Prelude hiding (div, span)
import qualified Clay.Media as Mq

main :: IO ()
main = putCss defaultStylesheet 

blueHighlight = "#1081a8"

orangy = rgb 255 80 10


containerWidth = 1024

-- wide   = query Mq.all [Mq.minWidth 414]
-- narrow = query Mq.all [Mq.maxWidth 414]

defaultStylesheet :: Css
defaultStylesheet = do
  body ? do
    color black
    fontSize (px 16)

  p ? do
    lineHeight (Clay.rem 1.3)

  ".inner" ? do
    width (pct 80)
    maxWidth (px 1024)
    sym2 margin (px 0) auto
    
  header ? do
    borderBottom solid (px 2) blueHighlight
    marginBottom (px 30)
    sym2 padding (px 12) (px 0)


    ".navigation" ? do
      textAlign (alignSide sideRight)

      a ? do
        color black
        fontSize (px 18)
        fontWeight bold
        marginLeft (px 12)
        textDecoration none
        textTransform uppercase

  ".logo" |> a ? do
    color black
    fontSize (px 18)
    fontWeight bold
    textDecoration none
    img ? do
      width (pct 100)

  ".social-media-logo" ? do
    width (px 32)

  ".content" ? do
    ".about-person" ? do
      border solid (px 2) blueHighlight
      marginBottom (px 20)

      ".header" ? do
        sym padding (px 10)
        background blueHighlight
        color white
        marginBottom (px 5)
        fontSize (Clay.rem 1.3)
        fontFamily ["Anonymous Pro"] [monospace]

        ".social-icons" ? do
          float floatRight

          ".fa" ? do
            color white

      ".bio" ? do
        sym padding (px 5)
        display flex
        -- flexWrap (FlexWrap wrap)
        justifyContent spaceBetween

        ".text" ? do
          width (pct 75)

        p ? do
          sym margin (px 0)
          lineHeight (Clay.rem 1.5)
          marginBottom (px 10)

        ".img_container" ? do
          width (pct 30)
          textAlign center
          minWidth (px 200)
          marginRight (px 10)

          "img" ? do
            width (px 200)
            sym borderRadius (pct 100)
            Clay.filter $ grayscale (pct 100)
            -- sym padding (px 10)

    ".podcasts" ? do
      listStyleType none
      sym padding (px 0)

      ".podcast" ? do
        border solid (px 2) blueHighlight
        marginBottom (px 20)

        header ? do
          sym padding (px 10)
          background blueHighlight
          color white
          marginBottom (px 5)
          fontSize (Clay.rem 1.3)
          fontFamily ["Anonymous Pro"] [monospace]

          a ? do
            color white

          ".date" ? do
            float floatRight

        article ? do
          sym padding (px 10)
          display flex
          flexFlow row (FlexWrap "wrap")
          justifyContent spaceAround

          ".summary" ? do
            flexGrow 1
            width (pct 50)
            fontSize (Clay.rem 1.1)

            ".read-post" ? do
              marginTop (px 20)
              a ? do
                sym padding (px 10)
                background orangy
                color white
                textDecoration none
                sym borderRadius (px 3)

          iframe ? do
            flexGrow 1
            height auto
            width (pct 50)


  footer ? do
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

