module TreeDecomp where

-- import Data.Graph.Force.ManyBody
-- import Data.Graph.Force.Link
-- import Data.Graph.Force
-- import Data.Graph.Render.SVG
-- import Data.Graph
-- import Data.StrMap (StrMap)
-- import Data.StrMap as StrMap
-- import Math (sqrt)

-- type GV = RV (PV (V v))
-- type GE = RE (E e)

-- type TV gv ge tv = RV ( PV (V ( bag :: Graph (GV gv) (GE ge) | tv) ) )
-- type TE e = RE ( E ( connVerts :: Array Vertex | e ) )

-- data Tree' a
--   = Node a (Array (Tree' a))

-- type Tree = Tree' {id :: String, bag :: Graph (GV gv) (GE ge), connVerts :: Array Vertex}

-- node :: String -> Array Vertex -> Graph (GV gv) (GE ge) -> Array Tree -> Tree
-- node id connVerts bag children = Node {id,bag,connVerts} children

-- mkTreeDecomp :: Tree -> Graph (TV gv ge ()) (TE ())
-- mkTreeDecomp (Node {id,bag,connVerts} children) = 

-- treeDecompForces :: forall tv te p i. Graph (TV tv) (TE te) ->
--   { treeForce :: Force (TV tv)
--   , bagForces :: StrMap (Force GV gv)
--   }
-- treeDecompForces tree = {treeForce, bagForces}
--   where
--   treeForce = 
--     manyBody manyBodyOptions 
--     <> link linkOptions (edgeArray tree)
--     <> dampVelocity 0.9
--     <> moveNodes
--   bagForces = vertexArray tree <#> bagForce # StrMap.fromFoldable
--   bagForce node = 
--     manyBody manyBodyOptions
--     <> link linkOptions (edgeArray node.bag)
--     <> pull 30.0 node.x node.y
--     <> lockConnVerts node
--     <> dampVelocity
--     <> moveNodes
--   connVertLocs node = 
--     StrMap.fromFoldable $ Array.concat $ incidentEdges node tree <#> \e ->
--       let sx = e.source.x
--           sy = e.source.y
--           tx = e.target.x
--           ty = e.target.y
--           l = sqrt (sx * sx + sy * sy)
--           w = l / 3.0
--           n = Array.length e.connVerts
--           connVertLoc i v = 
--             Tuple v.id
--               { x : (sx + tx) / 2.0 - (ty - sy) * w * (toNumber i - toNumber n / 2.0)
--               , y : (sy + sy) / 2.0 + (tx - sx) * w * (toNumber i - toNumber n / 2.0)
--               }
--       in Array.zipWith connVertLoc (Array.range 0 (n-1)) e.connVerts
--   lockConnVerts node = fixPositions $ connVertLocs node