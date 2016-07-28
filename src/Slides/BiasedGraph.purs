module Slides.BiasedGraph (slides) where

import Prelude hiding (div)
import Slides.Util
import Data.Graph as Graph
import Data.Graph.Render.SVG as Graph
import Pokeball (pokeball)

import Debug.Trace as Debug

slides :: forall b p i. Array (TTween b (HTML p i))
slides =
  [ frameMatrix
  , matrixTheorem
  , matroidTheorem
  , binaryFrame
  , bicircular
  --, cycleSpectrum
  , signedGraphs
  , biasedGraphs
  , biasedGraphTheorem
  ]

frameMatrix :: forall b p i. TTween b (HTML p i)
frameMatrix = do
  -- i1 <- newStep
  -- i2 <- newStep
  i3 <- newStep
  slide
    [ wideOverlayParent
      [ fadeOut' i3 $ p []
        [ text "A "
        , emph "frame matrix"
        , text " is a matrix in which each column has at most two non-zero entries."
        ]
      , fadeIn' i3 $ overlay
        [ p []
          [ text "A "
          , emph "frame matroid "
          , text "is a matroid that has a frame matrix representation."
          ]
        ]
      ]
    , overlayParent
      [ p_ $ unlines
        [ "\\[\\begin{pmatrix}"
        , " -1 &  2 &    &  1 &    &    &    &    \\\\"
        , "    &    & -2 &    & -2 &    &  1 &    \\\\"
        , "    &    &    &    &    &  2 &    &  1 \\\\"
        , "    &  1 &  1 &    &    &    &    &    \\\\"
        , "    &    &    &  1 &  2 &  2 &    &    "
        , "\\end{pmatrix}\\]"
        ]
      -- , fadeInOut' i1 i2 $ svgOverlay
      --   [ rect [x 0.05, y 0.1, width 0.1, height 0.8, class_ "highlight-red", fillOpacity 0.5] [] ]
      ]
    ]

matrixTheorem :: forall b p i. TTween b (HTML p i)
matrixTheorem = do
  slide
    [ theorem
      [ text "For each field $\\mathbb{F}$, there exists a polynomial-time algorithm to decide whether a matrix over $\\mathbb{F}$ is row-equivalent to a frame-matrix." ]
    ]

matroidTheorem :: forall b p i. TTween b (HTML p i)
matroidTheorem = do
  slide
    [ theorem
      [ text "For each field $\\mathbb{F}$, there exists a polynomial-time algorithm to decide whether an $\\mathbb{F}$-represented matroid is a frame matroid." ]
    ]

binaryFrame :: forall b p i. TTween b (HTML p i)
binaryFrame = do
  i0 <- newStep
  i1 <- newStep
  i2 <- newStep
  slide
    [ slideTitle "Binary frame matroids (graphic matroids)"
    , overlayParent
      [ fadeInOut' i0 i2 $ p_ $ unlines
        [ "\\[\\begin{matrix}"
        , " & E \\\\"
        , " V & \\begin{pmatrix}"
        , "  1 &  1 &    &  1 &    &    &    &    \\\\"
        , "  1 &    &  1 &    &  1 &    &  1 &    \\\\"
        , "    &    &    &    &    &  1 &    &  1 \\\\"
        , "    &  1 &  1 &    &    &    &    &    \\\\"
        , "    &    &    &  1 &  1 &  1 &    &  1 "
        , "\\end{pmatrix}\\end{matrix}\\]"
        ]
      , fadeInOut' i1 i2 $ svgOverlay
        [ rect [x 0.18, y 0.23, width 0.76, height 0.12, class_ "highlight-red", fillOpacity 0.5] [] ]
      , fadeIn' i2 $ overlay [ text $ unlines
          [ "\\[\\begin{matrix}"
          , " & E \\\\\\\\"
          , " V & \\begin{pmatrix}"
          , "  1 &    &  1 &    &  1 &    &  1 &    \\\\"
          , "    &    &    &    &    &  1 &    &  1 \\\\"
          , "    &  1 &  1 &    &    &    &    &    \\\\"
          , "    &    &    &  1 &  1 &  1 &    &  1 "
          , "\\end{pmatrix}\\end{matrix}\\]"
          ]
        ]
      ]
    ]

bicircular :: forall b p i. TTween b (HTML p i)
bicircular = do
  i1 <- newStep
  i2 <- newStep
  i3 <- newStep
  i4 <- newStep
  let size = 50.0
      padding =
        { left: 2.0
        , right: 2.0
        , top: 2.0
        , bottom: 8.0
        }
      boundingBox =
        { left: (-size/2.0 + padding.left)
        , right: size/2.0 - padding.right
        , top: (-size/2.0 + padding.top)
        , bottom: size/2.0 - padding.bottom
        }
      viewBox' = viewBox (-size/2.0) (-size/2.0) size size
      g0 = Graph.mkRGraph
        { vertices :
          [ { id: "1", x: 0.33, y: 0.77 }
          , { id: "2", x: 0.72, y: 0.76 }
          , { id: "3", x: 0.87, y: 0.40 }
          , { id: "4", x: 0.62, y: 0.51 }
          , { id: "5", x: 0.35, y: 0.40 }
          , { id: "6", x: 0.23, y: 0.14 }
          , { id: "7", x: 0.09, y: 0.51 }
          ]
        , edges :
          [ { id: "a", source: "1", target: "2" }
          , { id: "b", source: "2", target: "3" }
          , { id: "c", source: "3", target: "4" }
          , { id: "d", source: "4", target: "5" }
          , { id: "e", source: "5", target: "6" }
          , { id: "f", source: "6", target: "7" }
          , { id: "g", source: "7", target: "1" }
          , { id: "h", source: "2", target: "4" }
          , { id: "i", source: "5", target: "7" }
          , { id: "j", source: "1", target: "1" }
          ]
        }
        # Graph.autoTransform boundingBox
        # Graph.circularLoop "j" {radius: 4.0, angle: 2.0}
      g1 = Graph.lightenOtherEdges ["a","b","c","h","e","f","i"] g0
      g2 = Graph.lightenOtherEdges ["d","b","c","h","e","f","i"] g0
      g3 = Graph.lightenOtherEdges ["j"] g0
  slide
    [ slideTitle "Bicircular matroids"
    , p_ "$X\\subseteq E$ independent $\\Leftrightarrow$ each component of $G[X]$ has at most one cycle"
    , wideOverlayParent
      [ fadeOut' i4 $ svg [width (size # rem), height (size # rem), viewBox'] $
        -- [ pure $ pure $ Graph.render $ g0
        -- ]
        [ fadeOut' i1 $ pure $ pure $ Graph.render $ g0
        , fadeInOut' i1 i2 $ pure $ pure $ Graph.render $ g1
        , fadeInOut' i2 i3 $ pure $ pure $ Graph.render $ g2
        , fadeInOut' i3 i4 $ pure $ pure $ Graph.render $ g3
        ]
      , fadeInOut' i3 i4 $ fadeInOut' i1 i2 $ overlay
        [ p [position absolute, right (20.0 # rem)]
          [ redText "independent" ]
        ]
      , fadeInOut' i2 i3 $ overlay
        [ p [position absolute, right (20.0 # rem)]
          [ blueText "dependent" ]
        ]
      , fadeIn' i4 $ overlay [ text $ unlines
          [ "\\[\\begin{pmatrix}"
          --   a    b    c    d    e    f    g    h
          , "  1 & 1         &  1        &           &           &           &           \\\\" -- 1
          , "    & \\alpha_1 &           & 1         & 1         &           &           \\\\" -- 2
          , "    &           &           & \\alpha_3 &           & 1         & 1         \\\\" -- 3
          , "    &           &           &           & \\alpha_4 & \\alpha_5 &           \\\\" -- 4
          , "    &           & \\alpha_2 &           &           &           & \\alpha_6 " -- 5
          , "\\end{pmatrix}\\]"
          ]
        ]
      ]
    ]

cycleSpectrum :: forall b p i. TTween b (HTML p i)
cycleSpectrum = do
  i1 <- newStep
  slide
    [ p []
      [ ul []
        [ li_ "Graphic: each cycle is dependent"
        , fadeIn' i1 $ li_ "Other frame matroids"
        , li_ "Bicircular: no cycle is dependent"
        ]
      ]
    ]

signedGraphs :: forall b p i. TTween b (HTML p i)
signedGraphs = do
  i1 <- newStep
  i2 <- newStep
  i3 <- newStep
  i4 <- newStep
  i5 <- newStep
  let j1 = i5
  j2 <- newStep
  j3 <- newStep
  j4 <- newStep
  j5 <- newStep
  j6 <- newStep
  j7 <- newStep
  let k1 = j7
  k2 <- newStep
  k3 <- newStep
  k4 <- newStep
  let size = 50.0
      padding =
        { left: 2.0
        , right: 2.0
        , top: 2.0
        , bottom: 8.0
        }
      boundingBox =
        { left: (-size/2.0 + padding.left)
        , right: size/2.0 - padding.right
        , top: (-size/2.0 + padding.top)
        , bottom: size/2.0 - padding.bottom
        }
      viewBox' = viewBox (-size/2.0) (-size/2.0) size size
      oddEdges = ["c", "d", "f", "j"]
      g0 = Graph.mkRGraph
        { vertices :
          [ { id: "1", x: 0.33, y: 0.77 }
          , { id: "2", x: 0.72, y: 0.76 }
          , { id: "3", x: 0.87, y: 0.40 }
          , { id: "4", x: 0.62, y: 0.51 }
          , { id: "5", x: 0.35, y: 0.40 }
          , { id: "6", x: 0.23, y: 0.14 }
          , { id: "7", x: 0.09, y: 0.51 }
          ]
        , edges :
          [ { id: "a", source: "1", target: "2" }
          , { id: "b", source: "2", target: "3" }
          , { id: "c", source: "3", target: "4" }
          , { id: "d", source: "4", target: "5" }
          , { id: "e", source: "5", target: "6" }
          , { id: "f", source: "6", target: "7" }
          , { id: "g", source: "7", target: "1" }
          , { id: "h", source: "2", target: "4" }
          , { id: "i", source: "5", target: "7" }
          , { id: "j", source: "1", target: "1" }
          , { id: "k", source: "2", target: "2" }
          ]
        }
        # Graph.autoTransform boundingBox
        # Graph.circularLoop "j" {radius: 4.0, angle: 2.0}
        # Graph.circularLoop "k" {radius: 4.0, angle: 1.0}
        # Graph.highlightEdges "red" oddEdges
      cycle1 = ["a", "b", "c", "d", "i", "g"]-- ["e", "f", "h", "j", "k"] -- V -
      cycle2 = ["a", "b", "c", "d", "e", "f", "g"] -- ["h", "i", "j", "k"] -- V -
      cycle3 = ["j"] -- ["b", "c", "d", "e", "f", "g", "h", "i", "j"] -- V - ["a"]
      cycle4 = ["k"]
      indep1 = ["a","b","c","h","e","f","i"]
      dep1 = ["d","b","c","h","e","f","i"]
      g1 = Graph.lightenOtherEdges cycle1 g0
      g2 = Graph.lightenOtherEdges cycle2 g0
      g3 = Graph.lightenOtherEdges cycle3 g0
      g4 = Graph.lightenOtherEdges cycle4 g0
      h1 = Graph.lightenOtherEdges indep1 g0
      h2 = Graph.lightenOtherEdges dep1 g0
  slide
    [ slideTitle'
      [ text "Signed graph"
      , fadeIn' j1 $ span [] [text "ic matroid"]
      ]
    , wideOverlayParent
      [ fadeOut' i1 $ overlay
        [ p []
          [ text "A graph together with a set of "
          , redText "odd"
          , text " edges"
          ]
        ]
      , fadeInOut' i1 j1 $ overlay
        [ p []
          [ text "A cycle in is "
          , emph "balanced"
          , text " if it contains an even number of odd edges"
          ]
        ]
      , fadeIn' j1 $ p []
        [ text "$X\\subseteq E$ independent $\\Leftrightarrow$ each component of $G[X]$ has no balanced cycle, and at most one unbalanced cycle" ]
      ]
    , wideOverlayParent
      [ svg [width (size # rem), height (size # rem), viewBox'] $
        -- [ pure $ pure $ Graph.render $ g0
        -- ]
        [ fadeInOut' j1 j2 $ fadeOut' i1 $ pure $ pure $ Graph.render $ g0
        , fadeInOut' i1 i2 $ pure $ pure $ Graph.render $ g1
        , fadeInOut' i2 i3 $ pure $ pure $ Graph.render $ g2
        , fadeInOut' i3 i4 $ pure $ pure $ Graph.render $ g3
        , fadeInOut' i4 i5 $ pure $ pure $ Graph.render $ g4
        , fadeInOut' j2 j3 $ pure $ pure $ Graph.render $ h1
        , fadeInOut' j3 j4 $ pure $ pure $ Graph.render $ h2
        , fadeInOut' j4 j5 $ pure $ pure $ Graph.render $ g1
        , fadeInOut' j5 j6 $ pure $ pure $ Graph.render $ g3
        , fadeInOut' j6 j7 $ pure $ pure $ Graph.render $ g4
        ]
      , fadeInOut' i4 i5 $ fadeInOut' i1 i2 $ overlay
        [ p [position absolute, right (30.0 # rem)]
          [ emph "balanced" ]
        ]
      , fadeInOut' i2 i4 $ overlay
        [ p [position absolute, right (30.0 # rem)]
          [ redText "unbalanced" ]
        ]
      , fadeInOut' j5 j6 $ fadeInOut' j2 j3 $ overlay
        [ p [position absolute, right (20.0 # rem)]
          [ redText "independent" ]
        ]
      , fadeInOut' j3 j5 $ overlay
        [ p [position absolute, right (20.0 # rem)]
          [ blueText "dependent" ]
        ]
      , fadeInOut' j6 j7 $ overlay
        [ p [position absolute, right (40.0 # rem)]
          [ blueText "loop" ]
        ]
      , fadeIn' k1 $ overlay
        [ overlayParent
          [ p_ $ unlines
            [ "\\[\\begin{pmatrix}"
            --   a    b    c    d    e    f    g    h
            , "  1 & 1 &  1 &    &    &    &  \\\\" -- 1
            , "    & 1 &    &  1 &  1 &    &  \\\\" -- 2
            , "    &   &    & -1 &    &  1 & 1\\\\" -- 3
            , "    &   &    &    & -1 &  1 &  \\\\" -- 4
            , "    &   & -1 &    &    &    & 1 " -- 5
            , "\\end{pmatrix}\\]"
            ]
          , fadeInOut' k2 k3 $ svgOverlay
            [ rect [x 0.17, y 0.11, width 0.045, height 0.78, class_ "highlight-red", fillOpacity 0.5] [] ]
          , fadeInOut' k3 k4 $ svgOverlay
            [ rect [x 0.06, y 0.11, width 0.045, height 0.78, class_ "highlight-red", fillOpacity 0.5] [] ]
          ]
        ]
      ]
    ]

biasedGraphs :: forall b p i. TTween b (HTML p i)
biasedGraphs = do
  poke <- pokeball
  i1 <- newStep
  i2 <- newStep
  let j1 = i2
  slide
    [ wideOverlayParent
      [ fadeOut' i1 $ slideTitle "Biased graph"
      , fadeIn' i1 $ overlay [ slideTitle "Biased matroid" ]
      ]
    , wideOverlayParent
      [ fadeOut' i1 $ div []
        [ p []
          [ text "A graph $G$ together with a family $\\Psi$ of "
          , redText "unbalanced"
          , text " cycles satisfying the "
          , emph "theta-property"
          ]
          , pure poke
        ]
      , fadeInOut' i1 j1 $ overlay
        [ p []
          [ text "The "
          , emph "biased matroid"
          , text " $M(G,\\Psi)$ is the matroid with "
          , ul []
            [ li_ "$E(M(G,\\Psi))=E(G)$ and "
            , li_ "$X\\subseteq E$ independent $\\Leftrightarrow$ each component of $G[X]$ has no balanced cycle, and at most one unbalanced cycle"
            ]
          ]
        ]
      , fadeIn' j1 $ overlay
        [ lemma
          [ text "For each graph $G$ and each matrix $A$ with columns $E(G)$, the following are equivalent:"
          , ul []
            [ li [] [text "$M(A)=M(G,\\Psi)$, for some $\\Psi$,"]
            , li [] [ text "there exists a $(V\\times E)$-matrix $B$ "
                    -- , emphOnOff i2 i3 $ "supported by $G$"
                    , text "supported by $G$"
                    , text " which is row-equivalent to $A$."
                    ]
            ]
          ]
        ]
      ]
    ]

biasedGraphTheorem :: forall b p i. TTween b (HTML p i)
biasedGraphTheorem = do
  slide
    [ theorem
      [ text "For each field $\\mathbb{F}$, there exists a polynomial-time algorithm to decide whether an $\\mathbb{F}$-represented matroid is a biased matroid." ]
    ]
