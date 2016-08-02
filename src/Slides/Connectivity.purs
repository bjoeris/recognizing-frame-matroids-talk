module Slides.Connectivity where

import Prelude hiding (div, bottom)
import Slides.Util
import Data.Graph as Graph
import Data.Graph.Render.SVG as Graph
import Halogen.HTML.Styles.Tweened as Styles
import Slides.Images as Img

slides :: forall b p i. Array (TTween b (HTML p i))
slides =
  [ treeDecomp
  , algoOverview
  , decomp2
  , weak4conn
  , weak4connThm
  , biasedGraphParts
  , biased4conn
  , biased4conn'
  , partsReview
  , compat
  , compatThm
  , algorithm
  ]

treeDecomp :: forall b p i. TTween b (HTML p i)
treeDecomp = do
  i1 <- newStep
  i2 <- newStep
  slide
    [ wideOverlayParent
      [ fadeOut' i1 $ p []
        [ text "A "
        , emph "tree-decomposition"
        , text " of a matroid $M$ consists of a tree $T$ and, for each node $t\\in V(T)$, a "
        , emph "bag"
        , text " $E_t$ such that $(E_t)_{t\\in V(T)}$ is a partition of $E(M)$."
        ]
      , fadeInOut' i1 i2 $ overlay
        [ p []
          [ text "Each edge $e\\in E(T)$ induces a separation." ]
        ]
      , fadeIn' i2 $ overlay
        [ p []
          [ text "Each node $t\\in V(T)$ also has an associated matroid $M_t$, called the "
          , emph "blob"
          , text " of $M$ at $t$, obtained from $M$ by contracting onto $M[E_t]$."
          ]
        ]
      ]
    , overlayParent
      [ fadeOut' i1 $ img
        [ src Img.treeDecompTree, Styles.height (50.0 # rem) ]
      , fadeInOut' i1 i2 $ overlay
        [ img [src Img.treeDecompFlats, Styles.height (50.0 # rem) ]
        ]
      , fadeIn' i2 $ overlay
        [ img [src Img.treeDecompBlob, Styles.height (50.0 # rem) ]
        ]
      ]
    ]

algoOverview :: forall b p i. TTween b (HTML p i)
algoOverview = do
  i1 <- newStep
  i2 <- newStep
  i3 <- newStep
  i4 <- newStep
  i5 <- newStep
  i6 <- newStep
  slide
    [ p []
      [ slideTitle "Algorithm Overview"
      , wideOverlayParent
        [ fadeInOut' i1 i2 $ overlay
          [ ul []
            [ li_ "Construct a special tree-decomposition." ]
          ]
        , fadeInOut' i2 i4 $ overlay
          [ ul []
            [ li_ "Find a set of candidate biased graphs representing each part." ]
          ]
        , fadeIn' i4 $ ul []
          [ li_ "Combine compatible candidates into a biased graph for the entire matroid." ]
        ]
      ]
    , overlayParent
      [ fadeInOut' i1 i2 $ img
        [ src Img.treeDecompFlats, Styles.height (50.0 # rem) ]
      , fadeInOut' i2 i3 $ overlay
        [ img
          [ src Img.treeDecompGraphs, Styles.height (50.0 # rem) ]
        ]
      , fadeInOut' i3 i4 $ overlay
        [ img
          [ src Img.treeDecompGraphs2, Styles.height (50.0 # rem) ]
        ]
      , fadeInOut' i4 i5 $ overlay
        [ img
          [ src Img.treeDecompGraphsMerge, Styles.height (50.0 # rem) ]
        ]
      , fadeInOut' i5 i6 $ overlay
        [ img
          [ src Img.treeDecompGraphsMerge2, Styles.height (50.0 # rem) ]
        ]
      ]
    ]

decomp2 :: forall b p i. TTween b (HTML p i)
decomp2 = do
  slide
    [ div [marginTop (-10.0 # rem)]
      [ namedTheorem "Cunningham, Edmonds"
        [ text "Each matroid has a tree-decomposition along 2-separations into blobs that are either:"
        , ul []
          [ li_ "$U_{1,n}$,"
          , li_ "$U_{n-1,n}$, or"
          , li_ "3-connected"
          ]
        ]
      ]
    , img
      [ src Img.tree2Decomp
      , Styles.height (50.0 # rem)
      , marginTop (-35.0 # rem)
      , marginLeft (15.0 # rem)
      ]
    ]

weak4conn :: forall b p i. TTween b (HTML p i)
weak4conn = do
  i1 <- newStep
  i2 <- newStep
  i3 <- newStep
  i4 <- newStep
  slide
    [ wideOverlayParent
      [ fadeOut' i4 $ p []
        [ text "A "
        , emph "flower of size $n$"
        , text " in a matroid $M$ is a partition $(A_1,\\ldots,A_n)$ of $E(M)$ such that, for each "
        , text "$1\\leq i\\leq j \\leq n$, $\\lambda_M(A_i\\cup\\cdots\\cup A_j)<2$."
        ]
      , fadeIn' i4 $ overlay
        [ p []
          [ text "A matroid $M$ is "
          , emph "weakly 4-connected"
          , text " if it is 3-connected and has no 3-flower of size 10."
          ]
        ]
      ]
    , overlayParent
      [ fadeIn' i4 $ fadeOut' i1 $ img
        [ src Img.flower
        , Styles.height (50.0 # rem)
        ]
      , fadeInOut' i1 i2 $ overlay
        [img
          [ src Img.flower1
          , Styles.height (50.0 # rem)
          ]
        ]
      , fadeInOut' i2 i3 $ overlay
        [img
          [ src Img.flower2
          , Styles.height (50.0 # rem)
          ]
        ]
      , fadeInOut' i3 i4 $ overlay
        [ img
          [ src Img.flower3
          , Styles.height (50.0 # rem)
          ]
        ]
      ]
    ]

weak4connThm :: forall b p i. TTween b (HTML p i)
weak4connThm = do
  slide
    [ lemma
      [ text "Each matroid has a tree-decomposition along 3-separations into blobs that are either"
      , ul []
        [ li_ "$U_{1,n}$"
        , li_ "$U_{n-1,n}$"
        , li_ "a flower, or"
        , li_ "weakly 4-connected"
        ]
      ]
    , img
      [ src Img.treeDecompFlats
      , Styles.height (50.0 # rem)
      , marginTop (-40.0 # rem)
      , marginLeft (35.0 # rem)
      , marginRight (-10.0 # rem)
      ]
    ]

biasedGraphParts :: forall b p i. TTween b (HTML p i)
biasedGraphParts = do
  i1 <- newStep
  i2 <- newStep
  i3 <- newStep
  i4 <- newStep
  i5 <- newStep
  i6 <- newStep
  slide
    [ wideOverlayParent
      [ fadeOut' i2 $ p []
        [ text "Biased graphs for $U_{1,n}$:" ]
      , fadeInOut' i2 i5 $ overlay
        [ p []
          [ text "Biased graphs for $U_{n-1,n}$:" ]
        ]
      , fadeIn' i5 $ overlay
        [ p []
          [ text "Biased graphs for flowers:" ]
        ]
      ]
    , overlayParent
      [ fadeOut' i1 $ img
        [ src Img.bond1
        , Styles.height (50.0 # rem)
        ]
      , fadeInOut' i1 i2 $ overlay
        [ img
          [ src Img.bond2
          , Styles.height (50.0 # rem)
          ]
        ]
      , fadeInOut' i2 i3 $ overlay
        [ img
          [ src Img.cycle1
          , Styles.height (50.0 # rem)
          ]
        ]
      , fadeInOut' i3 i4 $ overlay
        [ img
          [ src Img.cycle2
          , Styles.height (50.0 # rem)
          ]
        ]
      , fadeInOut' i4 i5 $ overlay
        [ img
          [ src Img.cycle3
          , Styles.height (50.0 # rem)
          ]
        ]
      , fadeInOut' i5 i6 $ overlay
        [ img
          [ src Img.wheel1
          , Styles.height (50.0 # rem)
          ]
        ]
      , fadeIn' i6 $ overlay
        [ img
          [ src Img.wheel2
          , Styles.height (50.0 # rem)
          ]
        ]
      ]
    ]

biased4conn :: forall b p i. TTween b (HTML p i)
biased4conn = do
  slide
    [ theorem
      -- [ text "If $M$ is a weakly 4-connected matroid and $|E(M)|\\geq 20$, then $M\\setminus e$ is weakly 4‑connected, for some $e\\in E(M)$." ]
      [ text "There exists a polynomial $p$ such that each weakly 4-connected matroid with $n$ elements has at most $p(n)$ biased graph representations (up to vertex relabeling)." ]
    ]

biased4conn' :: forall b p i. TTween b (HTML p i)
biased4conn' = do
  slide
    [ theorem
      -- [ text "If $M$ is a weakly 4-connected matroid and $|E(M)|\\geq 20$, then $M\\setminus e$ is weakly 4‑connected, for some $e\\in E(M)$." ]
      [ text "There exists a polynomial-time algorithm to enumerate all biased-graphs of a weakly 4-connected matroid (up to vertex relabeling)." ]
    ]

partsReview :: forall b p i. TTween b (HTML p i)
partsReview = do
  slide
    [ p []
      [ text "Biased graphs at each blob:"
      , ul []
        [ li_ "$U_{1,n}$: 2 biased graphs"
        , li_ "$U_{n-1,n}$: well understood structure"
        , li_ "flower: well understood structure"
        , li_ "weakly 4-connected: polynomial number"
        ]
      ]
    ]

compat :: forall b p i. TTween b (HTML p i)
compat = do
  i1 <- newStep
  i2 <- newStep
  i3 <- newStep
  i4 <- newStep
  i5 <- newStep
  i6 <- newStep
  i7 <- newStep
  i8 <- newStep
  i9 <- newStep
  slide
    [ wideOverlayParent
      [ fadeOut' i4 $ overlay [
          p []
          [ text "A separation $(A,B)$ in a biased graph $(G,\\Psi)$ is "
          , emph "unbalanced"
          , text " if"
          , ul []
            [ li_ "both $(G,\\Psi)[A]$ and $(G,\\Psi)[B]$ are unbalanced, and"
            , li_ "$|V_G(A)\\cap V_G(B)|=\\lambda(A)$."
            ]
          ]
        ]
      , fadeInOut' i4 i8 $ p []
        [ text "A separation $(A,B)$ in a biased graph $(G,\\Psi)$ is "
        , emph "semibalanced"
        , text " if"
        , ul []
          [ li_ "exactly one of $(G,\\Psi)[A]$ or $(G,\\Psi)[B]$ is unbalanced, and"
          , li_ "$|V_G(A)\\cap V_G(B)|=\\lambda(A) + 1$."
          ]
        ]
      , fadeIn' i8 $ overlay
        [ p []
          [ text "A separation that is neither unbalanced nor semibalanced is "
          , emph "degenerate"
          ]
        ]
      ]
    , div [marginTop (-10.0 # rem)]
      [ overlayParent
        [ fadeOut' i1 $ img
          [ src Img.unbalanced2Sep
          , Styles.height (50.0 # rem)
          ]
        , fadeInOut' i1 i2 $ overlay
          [ img
            [ src Img.unbalanced2SepApart
            , Styles.height (50.0 # rem)
            ]
          ]
        , fadeInOut' i2 i3 $ overlay
          [ img
            [ src Img.unbalanced3Sep
            , Styles.height (50.0 # rem)
            ]
          ]
        , fadeInOut' i3 i4 $ overlay
          [ img
            [ src Img.unbalanced3SepApart
            , Styles.height (50.0 # rem)
            ]
          ]
        , fadeInOut' i4 i5 $ overlay
          [ img
            [ src Img.semibalanced2Sep
            , Styles.height (50.0 # rem)
            ]
          ]
        , fadeInOut' i5 i6 $ overlay
          [ img
            [ src Img.semibalanced2SepApart
            , Styles.height (50.0 # rem)
            ]
          ]
        , fadeInOut' i6 i7 $ overlay
          [ img
            [ src Img.semibalanced3Sep
            , Styles.height (50.0 # rem)
            ]
          ]
        , fadeInOut' i7 i8 $ overlay
          [ img
            [ src Img.semibalanced3SepApart
            , Styles.height (50.0 # rem)
            ]
          ]
        , fadeInOut' i8 i9 $ overlay
          [ img
            [ src Img.degenerate2Sep
            , Styles.height (50.0 # rem)
            ]
          ]
        , fadeIn' i9 $ overlay
          [ img
            [ src Img.degenerate3Sep
            , Styles.height (50.0 # rem)
            ]
          ]
        --, fadeIn' i5 $ overlay [ text "TODO: picture of complicated degenerate 3-separation" ]
        ]
      ]
    ]


compatThm :: forall b p i. TTween b (HTML p i)
compatThm = do
  slide
    [ theorem
      [ text "There exists a constant $c$ such that, for each representable matroid $M(A)$ has a tree-decomposition along 3-separations such that"
      , ul []
        [ li_ "each blob is $U_{1,n}$, $U_{n-1,n}$, a flower, or weakly 4-connected, and"
        , li_ "for each biased-graph representing $M(A)$, at most $c$ edges in $T$ induce degenerate separations."
        ]
      ]
    ]

algorithm :: forall b p i. TTween b (HTML p i)
algorithm = do
  i1 <- newStep
  i2 <- newStep
  i3 <- newStep
  i4 <- newStep
  i5 <- newStep
  let h = 60.0 # rem
  slide
    [ slideTitle "Algorithm"
    , wideOverlayParent
      [ fadeOut' i1 $ p []
        [ text "Guess degenerate separations" ]
      , fadeInOut' i1 i2 $ overlay
        [ p [] [ text "Contract degenerate tree-edges" ] ]
      ]
    , overlayParent
      [ fadeOut' i1 $ img
        [ src Img.algorithm1
        , Styles.height h
        ]
      , fadeInOut' i1 i2 $ overlay
        [ img
          [ src Img.algorithm2
          , Styles.height h
          ]
        ]
      , fadeInOut' i2 i3 $ overlay
        [ img
          [ src Img.algorithm3
          , Styles.height h
          ]
        ]
      , fadeInOut' i3 i4 $ overlay
        [ img
          [ src Img.algorithm4
          , Styles.height h
          ]
        ]
      , fadeInOut' i4 i5 $ overlay
        [ img
          [ src Img.algorithm5
          , Styles.height h
          ]
        ]
      , fadeIn' i5 $ overlay
        [ img
          [ src Img.algorithm6
          , Styles.height h
          ]
        ]
        -- try all possible sets of possible degenerate edges (polynamially many)
        -- for each guess, merge the incident bags in the tree decomposition
        -- this can't give the bags too many more representations
        -- [ text "TODO: picture of graph tree-decomp with a path of 2 degenerate separation. Merge it into 1 node" ]
      -- , fadeInOut' i1 i2 $ overlay
      --   -- root the tree arbitrarily
      --   [ text "TODO: root the tree" ]
      -- , fadeInOut' i2 i3 $ overlay
      --   -- we don't actually know the rep yet
      --   [ text "TODO: switch to matroid tree decomp" ]
      -- , fadeInOut' i3 i4 $ overlay
      --   -- recursively, find all the ways that a rep of a subtree can attach to its parent
      --   [ text "TODO: highlight a subtree" ]
      -- , fadeInOut' i4 i5 $ overlay
      --   [ text "TODO: replace subtree by list of triangle and unbalanced parallel pair with loop" ]
      -- , fadeInOut' i5 i6 $ overlay
      --   [ text "TODO: replace sibling subtrees with similar lists" ]
      -- , fadeInOut' i6 i7 $ overlay
      --   -- the parent can then find which of those child attachments are compatible, to generate the list of possible attachments to the grandparent
      --   [ text "TODO: replace parent with similar list" ]
      ]
    ]
