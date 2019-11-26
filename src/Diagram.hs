{-# LANGUAGE OverloadedStrings #-}

module Diagram
    ( generateAll
    ) where

import           Data.Foldable                       (traverse_)
import           Data.GraphViz.Attributes            (RankType (SameRank),
                                                      Shape (BoxShape, Ellipse),
                                                      fillColor, filled,
                                                      fontColor, rank, shape,
                                                      style, toLabel)
import           Data.GraphViz.Attributes.Colors     (Color (SVGColor))
import           Data.GraphViz.Attributes.Colors.SVG (SVGColor)
import qualified Data.GraphViz.Attributes.Colors.SVG as SVG
import qualified Data.GraphViz.Attributes.Complete   as C
import           Data.GraphViz.Attributes.HTML       (Align (HCenter), Attribute (Align, BGColor, Border, CellBorder, CellSpacing, Color),
                                                      Cell (LabelCell),
                                                      Format (Bold),
                                                      Label (Table, Text),
                                                      Row (Cells),
                                                      Table (HTable),
                                                      TextItem (Font, Format, Str))
import qualified Data.GraphViz.Attributes.HTML       as HTML
import           Data.GraphViz.Commands              (GraphvizCommand (Osage), GraphvizOutput (DotOutput, Png, Svg),
                                                      quitWithoutGraphviz,
                                                      runGraphviz,
                                                      runGraphvizCommand)
import           Data.GraphViz.Types.Generalised     (DotGraph)
import           Data.GraphViz.Types.Monadic         (Dot, GraphID (Num),
                                                      digraph', graphAttrs,
                                                      node, node', subgraph,
                                                      (-->))
import           Data.Text.Lazy
import qualified Demo.Colors

generateAll :: IO ()
generateAll = do
    quitWithoutGraphviz
        "There is no 'dot' binary in you PATH. Please install Graphviz (https://www.graphviz.org/)"
    _ <- runGraphviz demoGraph Svg "demo.svg"
    _ <- runGraphviz demoGraph Png "demo.png"
    _ <- runGraphviz demoGraph DotOutput "demo.dot"
    _ <- runGraphvizCommand Osage Demo.Colors.demoGraph Svg "svg_colors.svg"
    pure ()

demoGraph :: DotGraph Int
demoGraph =
    digraph' $ do
        legend
        system 0 "System1" ["<<myteam>>", "Fancy app"]
        system 1 "Example System" ["<<myteam>>", "A piece of spectacular engineering"]
        systemExt 2 "External System" ["<<qualtrics>>"]
        systemGwiExt 3 "GWI System" ["<<otherteam>>", "breathtaking"]
        databaseGwi 4 "GWI DB" ["<<otherteam>>", "this has data about some stuff"]
        database 5 "Ones and zeros" ["<<myteam>>", "Occasional two"]
        userExt 6 "External User" ["I'm not from GWI"]
        userGwi 7 "GWI User" ["GWI employee"]
        0 --> 1
        3 --> 1
        1 --> 2
        1 --> 4
        1 --> 5
        6 --> 0
        7 --> 1
        sameRank 8 [1, 2, 3]

sameRank :: Int -> [Int] -> Dot Int
sameRank subgraphId nodeIds =
    subgraph (Num $ C.Int subgraphId) $ do
        graphAttrs [rank SameRank]
        traverse_ node' nodeIds

nodeWithShapeAndColor :: C.Attribute -> SVGColor -> Int -> Text -> [Text] -> Dot Int
nodeWithShapeAndColor shapeAttr bgColor nodeId title otherTexts =
    node
        nodeId
        [ style filled
        , shapeAttr
        , fillColor bgColor
        , fontColor SVG.White
        , toLabel $
          Table $
          HTable
              Nothing
              -- TODO generating via -Tsvg doesn't keep the text correctly centered.
              -- https://gitlab.com/graphviz/graphviz/issues/1426 recommends using
              -- -Tsvg:cairo, but that's not supported OOTB by graphviz library
              [Border 0, CellBorder 0, CellSpacing 0, Align HCenter]
              (Cells [LabelCell [] (Text [Format Bold [Str title]])] : fmap tableRow otherTexts)
        ]

tableRow :: Text -> Row
tableRow label = Cells [LabelCell [] (Text [HTML.Str label])]

-- SYSTEM = Colored rectangles
system :: Int -> Text -> [Text] -> Dot Int
system = nodeWithShapeAndColor (shape BoxShape) SVG.DodgerBlue

systemExt :: Int -> Text -> [Text] -> Dot Int
systemExt = nodeWithShapeAndColor (shape BoxShape) SVG.DarkGray

systemGwiExt :: Int -> Text -> [Text] -> Dot Int
systemGwiExt = nodeWithShapeAndColor (shape BoxShape) SVG.DeepPink

-- DATABASES = Colored cylinders
database :: Int -> Text -> [Text] -> Dot Int
database = nodeWithShapeAndColor cylinderShape SVG.DodgerBlue

databaseGwi :: Int -> Text -> [Text] -> Dot Int
databaseGwi = nodeWithShapeAndColor cylinderShape SVG.DeepPink

-- USERS = Colored Elipses
userGwi :: Int -> Text -> [Text] -> Dot Int
userGwi = nodeWithShapeAndColor (shape Ellipse) SVG.RoyalBlue

userExt :: Int -> Text -> [Text] -> Dot Int
userExt = nodeWithShapeAndColor (shape Ellipse) SVG.DimGray

-- WTF - Shape doesn't have Cylinder constructor
cylinderShape :: C.Attribute
cylinderShape = C.customAttribute "shape" "cylinder"

legend :: Dot Int
legend =
    colorLegend
        [ (SVG.DimGray, "External User")
        , (SVG.RoyalBlue, "GWI User")
        , (SVG.DeepPink, "GWI Database/System")
        , (SVG.DodgerBlue, "Team's DB/System")
        , (SVG.DarkGray, "External System")
        ]

colorLegend :: [(SVGColor, Text)] -> Dot Int
colorLegend legend =
    node
        1000
        [ shape BoxShape
        --, fontColor SVG.White
        , toLabel $
          Table $
          HTable
              Nothing
              [Border 0, CellBorder 0, CellSpacing 0, Align HCenter]
              (Cells [LabelCell [] (Text [Format Bold [Str "Legend"]])] : fmap legendRow legend)
        ]
  where
    legendRow :: (SVGColor, Text) -> Row
    legendRow (c, t) =
        Cells
            [ LabelCell
                  [BGColor (SVGColor c)]
                  (Text [Font [Color (SVGColor SVG.White)] [HTML.Str t]])
            ]
