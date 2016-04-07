-- Generates the .svg for the nixie tube's PCB footprint

{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m ,z)
import Text.Blaze.Svg (Svg, translate)
import qualified Text.Blaze.Svg11 as S
import Text.Blaze.Svg11 (toValue)
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Internal (attribute)
import Text.Blaze.Svg.Renderer.String (renderSvg)
import Data.String (fromString)
import Data.Complex (Complex((:+)), mkPolar)
import Control.Monad (forM_)
import Data.Text (Text, singleton)

main :: IO ()
main = do
  let a = renderSvg svgDoc
  putStrLn a

svgDoc :: S.Svg
svgDoc = S.docTypeSvg ! A.version "1.2"
       ! A.width "20mm" ! A.height "20mm"
       ! A.viewbox "0 0 100 100" $ do
    S.g $ S.g ! A.id_ "copper1" $ S.g ! A.id_ "copper0" $ nixiePads 10 10
    S.g $ S.g ! A.id_ "silkscreen" $ nixieSilk 10 10

nixiePads :: X -> Y -> S.Svg
nixiePads xo yo = forM_ [0..12] $ \i -> do
    let connectorName = fromString $ "connector" ++ show i ++ "pin"
    let (x :+ y) = mkPolar 5.5 ((2 * pi * fromIntegral i / 13) - pi / 2)
    pad (xo + X x) (yo + Y y) 0.5 0.9 ! A.id_ connectorName

nixieSilk :: X -> Y -> S.Svg
nixieSilk xo yo = do
    S.circle ! A.strokeWidth "0.5mm" ! A.stroke "rgb(0,0,0)" ! A.fill "none" ! A.r "9.25mm" ! A.cx (toValue xo) ! A.cy (toValue yo)
    text "▿" ! A.x (toValue xo) ! A.y (toValue (yo + 1))
    forM_ [0..12] $ \i -> do
        let label = singleton ("+.1234567890." !! i)
        let (dx :+ dy) = mkPolar 7.5 ((2 * pi * fromIntegral i / 13) - pi / 2)
        let x = xo + X dx
        let y = yo + Y dy + 0.6
        text label ! A.x (toValue x) ! A.y (toValue y)

text :: Text -> Svg
text t = S.text_ (S.text t) ! A.textAnchor "middle" ! A.fontSize "8"

newtype X = X Double deriving (Num, Fractional)
instance S.ToValue X where
    toValue (X x) = fromString (show x ++ "mm")
newtype Y = Y Double deriving (Num, Fractional)
instance S.ToValue Y where
    toValue (Y y) = fromString (show y ++ "mm")
newtype Radius = Radius Double deriving (Num, Fractional)
instance S.ToValue Radius where
    toValue (Radius r) = fromString (show r ++ "mm")

pad :: X -> Y -> Radius -> Radius -> Svg
pad x y inner outer
    = S.circle
    ! A.stroke "rgb(255, 191, 0)"
    ! A.strokeWidth (S.toValue width)
    ! A.r (S.toValue (inner + width / 2))
    ! A.cx (S.toValue x)
    ! A.cy (S.toValue y)
    ! A.fill "none"
    where
    width = outer - inner
