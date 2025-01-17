module UdGraphic (
    Comanda(..),
    Distancia,
    Angle,
    execute,
    display
    )
    where

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT hiding (Angle)
import Data.IORef
import Data.List
import Control.Monad( liftM, liftM2, liftM3 )
import System.Random
import Test.QuickCheck

infixr 5 :#:

-- Punts

data Pnt = Pnt Float Float
  deriving (Eq,Ord,Show)

instance Num Pnt where
  Pnt x y + Pnt x' y'  =  Pnt (x+x') (y+y')
  Pnt x y - Pnt x' y'  =  Pnt (x-x') (y-y')
  Pnt x y * Pnt x' y'  =  Pnt (x*x') (y*y')
  fromInteger          =  scalar . fromInteger
  abs (Pnt x y)        =  Pnt (abs x) (abs y)
  signum (Pnt x y)     =  Pnt (signum x) (signum y)

instance Fractional Pnt where
  Pnt x y / Pnt x' y'  =  Pnt (x/x') (y/y')
  fromRational         =  scalar . fromRational

scalar :: Float -> Pnt
scalar x  =  Pnt x x

scalarMin :: Pnt -> Pnt
scalarMin (Pnt x y)  =  scalar (x `min` y)

scalarMax :: Pnt -> Pnt
scalarMax (Pnt x y)  =  scalar (x `max` y)

dimensions :: Pnt -> (Int,Int)
dimensions (Pnt x y)  =  (ceiling x, ceiling y)

lub :: Pnt -> Pnt -> Pnt
Pnt x y `lub` Pnt x' y'  =  Pnt (x `max` x') (y `max` y')

glb :: Pnt -> Pnt -> Pnt
Pnt x y `glb` Pnt x' y'  =  Pnt (x `min` x') (y `min` y')

pointToSize :: Pnt -> Size
pointToSize (Pnt x y) = Size (ceiling x) (ceiling y)

sizeToPoint :: Size -> Pnt
sizeToPoint (Size x y) = Pnt (fromIntegral x) (fromIntegral y)

-- Colors

data Llapis = Color' GL.GLfloat GL.GLfloat GL.GLfloat
            | Transparent
            | Inkless
            deriving (Eq, Ord, Show)

pencilToRGB :: Llapis -> GL.Color3 GL.GLfloat
pencilToRGB (Color' r g b)  =  GL.Color3 r g b
pencilToRGB Transparent  =  error "pencilToRGB: transparent"

blanc, negre, vermell, verd, blau :: Llapis
blanc   = Color' 1.0 1.0 1.0
negre   = Color' 0.0 0.0 0.0
vermell = Color' 1.0 0.0 0.0
verd    = Color' 0.0 1.0 0.0
blau    = Color' 0.0 0.0 1.0

-- Lines

data Ln = Ln Llapis Pnt Pnt
  deriving (Eq,Ord,Show)

-- Estat actual

data Act = Act Pnt Angle
  deriving (Eq)

-- Window parameters

theCanvas :: Pnt
theCanvas  =  Pnt 800 800

theBGcolor :: GL.Color3 GL.GLfloat
theBGcolor = pencilToRGB blanc



-- Main drawing and window functions

display :: Comanda -> IO ()
display c = do
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize  $= pointToSize theCanvas
  getArgsAndInitialize
  w <- createWindow "pencilcil Graphics"
  displayCallback $= draw c
  reshapeCallback $= Just (\x -> (viewport $= (Position 0 0, x)))
  --actionOnWindowClose $= ContinueExectuion
  draw c
  mainLoop

draw :: Comanda -> IO ()
draw c = do clear [ColorBuffer]
            loadIdentity
            background
            toGraphic $ rescale $ execute c
            swapBuffers

toGraphic :: [Ln] -> IO ()
toGraphic lines  = sequence_ (map f lines)
  where
  f (Ln pencil startP endP)  =
    GL.color (pencilToRGB pencil) >>
    GL.renderPrimitive GL.LineStrip (toVertex startP >> toVertex endP)

background :: IO ()
background = do GL.color theBGcolor
                GL.renderPrimitive GL.Polygon $ mapM_ GL.vertex
                      [GL.Vertex3 (-1) (-1) 0,
                       GL.Vertex3   1  (-1) 0,
                       GL.Vertex3   1    1  0,
                       GL.Vertex3 (-1)   1 (0::GL.GLfloat) ]


toVertex (Pnt x y)  =  GL.vertex $ GL.Vertex3
 (realToFrac x) (realToFrac y) (0::GL.GLfloat)



-- Definició de les comandes per moure el llapis

type Angle     = Float
type Distancia = Float
data Comanda   = Avança Distancia
               | Gira Angle
               | Comanda :#: Comanda
               | Para
               | CanviaColor Llapis
               | Branca Comanda
               deriving (Show, Eq)

data Branca = Comanda



-- Problema 8
-- Pas de comandes a lines a pintar per GL graphics

separa :: Comanda -> [Comanda]
separa (x :#: xs) = (separa x) ++ (separa xs) -- l'unic element que hi passem es la llista de comandes
separa (Para) = [] -- en cas que sigui un Para, no fem res
separa (comanda) = [comanda] -- altrament retornem la mateixa comanda

execute :: Comanda -> [Ln]
execute comanda = execute' (Act (Pnt 0 0) 0) comandesNoves
  where comandesNoves = separa comanda

execute' :: Act -> [Comanda] -> [Ln]

execute' (Act puntIni angleIni) ((CanviaColor l) : resta) = Ln l puntIni puntIni : (execute' (Act puntIni angleIni) resta)

execute' (Act puntIni angleIni) ((Branca c):resta) = ((Ln negre puntIni puntIni) : (execute' (Act puntFinal angleFinal) comandesBranca)) ++ (execute' (Act puntIni angleIni) resta)
  where 
    comandesBranca = separa c
    (Act puntFinal angleFinal) = fesMoviment (Act puntIni angleIni) (c)

-- Cas: avança, mostrem el progres
execute' (Act puntIni angleIni) ((Avança a): resta) = Ln negre puntIni puntFinal : execute' (Act puntFinal angleFinal) resta
 where (Act puntFinal angleFinal) = fesMoviment (Act puntIni angleIni) (Avança a)
  
-- Cas: no avança, fem progres i no mostrem
execute' (Act puntIni angleIni) (c: restacom) = execute' (Act puntFinal angleFinal) restacom
 where (Act puntFinal angleFinal) = fesMoviment (Act puntIni angleIni) c
 


-- Cas base
execute' (Act _ _) [] = []

-- FUNCIO fesMoviment
-- Donat un punt, un angle i una comanda, retorna nou punt i angle un cop aplicada la comanda
fesMoviment :: Act -> Comanda -> Act
fesMoviment (Act (Pnt x y) angle) (Gira angleNou) = Act (Pnt x y) (angle+angleNou)
fesMoviment (Act (Pnt x y) angle) (Avança dist) = 
  Act (Pnt (x + dist*(cos (angle* (-pi/180)))) ((y + dist*(sin (angle* (-pi/180)))))) angle
fesMoviment act para = act

-- Rescales all points in a list of lines
--  from an arbitrary scale
--  to (-1.-1) - (1.1)

rescale :: [Ln] -> [Ln]
rescale lines | points == [] = []
              | otherwise    = map f lines
  where
  f (Ln pencil p q)  =  Ln pencil (g p) (g q)
  g p             =  swap ((p - p0) / s)
  points          =  [ r | Ln pencil p q <- lines, r <- [p, q] ]
  hi              =  foldr1 lub points
  lo              =  foldr1 glb points
  s               =  scalarMax (hi - lo) * scalar (0.55)
  p0              =  (hi + lo) * scalar (0.5)
  swap (Pnt x y)  =  Pnt y x


-- Generators for QuickCheck

instance Arbitrary Llapis where
    arbitrary  =  sized pencil
        where
          pencil n  =  elements [negre,vermell,verd,blau,blanc,Transparent]


instance Arbitrary Comanda where
    arbitrary  =  sized cmd
        where
          cmd n  |  n <= 0     =  oneof [liftM (Avança . abs) arbitrary,
                                         liftM Gira arbitrary ]
                 |  otherwise  =  liftM2 (:#:) (cmd (n `div` 2)) (cmd (n `div`2))
