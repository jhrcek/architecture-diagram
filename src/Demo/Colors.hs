module Demo.Colors
    ( demoGraph
    ) where

import           Control.Monad                       (zipWithM_)
import           Data.GraphViz.Attributes            (Shape (BoxShape),
                                                      fillColor, filled, shape,
                                                      style, toLabel)
import           Data.GraphViz.Attributes.Colors.SVG (SVGColor)
import           Data.GraphViz.Types.Generalised     (DotGraph)
import           Data.GraphViz.Types.Monadic         (Dot, graph', node)

demoGraph :: DotGraph Int
demoGraph = graph' $ zipWithM_ coloredNode [1 ..] allColors
  where
    coloredNode :: Int -> SVGColor -> Dot Int
    coloredNode nodeId c = node nodeId [style filled, shape BoxShape, fillColor c, toLabel $ show c]
    allColors :: [SVGColor]
    allColors = [minBound .. maxBound]
