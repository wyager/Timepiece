-- Generates the .svg for the nixie tube's riser's PCB footprint

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
       ! A.width "34mm" ! A.height "26mm"
       ! A.viewbox "0 0 175 130" $ do
    S.g $ S.g ! A.id_ "copper1" $ S.g ! A.id_ "copper0" $ riserPads 1.5 13
    S.g $ S.g ! A.id_ "silkscreen" $ riserSilk 1.5 13

riserPads :: X -> Y -> S.Svg
riserPads xo yo = forM_ [0..12] $ \h -> forM_ [-1,1] $ \v -> do
    let connectorName = fromString $ "connector" ++ show (h + v*12) ++ "pin"
    let x = X (2.54 * h) + xo
    let y = Y (11 * v) + yo
    pad x y 0.5 1.0 ! A.id_ connectorName

riserSilk :: X -> Y -> S.Svg
riserSilk xo yo = forM_ [0..12] $ \h -> forM_ [-1,1] $ \v -> do
        let labels = case v of
                -1 -> "0.+.1290.+.12" -- Top
                1 -> "9876543876543" -- Bottom
        let label = singleton (labels !! h)
        let x = X (2.54 * fromIntegral h) + xo
        let y = Y (9 * fromIntegral v) + yo + 0.8
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
