module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type Elapsed = Float

fps :: Int
fps = 60

-- Max instantaneous BPM points remembered.
historySize :: Int
historySize = 16

-- Graphical BPM range.
bpmRange :: (Float, Float)
bpmRange = (20, 140)

-- Total number of ticks on y-axis.
ticks :: Int
ticks = 120
majorTicks :: Int
majorTicks = 20  -- Every 20 ticks get a text label

-- Scene contains time elapsed since start, the size and da beats.
data Scene = Scene Elapsed (Int, Int) Beats
-- Beats contains the beat count, a history of instantaneous BPMs,
-- the old and new BPM, and the time of the last and current beat.
data Beats = Beats Int [Float] Float Float Elapsed Elapsed

-- Shift and scale a bounded value to a different set of bounds.
align :: Fractional a => a -> a -> a -> a -> a -> a
align x xmin xmax ymin ymax = y
  where y = (x - xmin) / (xmax - xmin) * (ymax - ymin) + ymin

-- Restrict a value between two bounds
clamp :: Fractional a => Ord a => a -> a -> a -> a
clamp x xmin xmax
  | x > xmax = xmax
  | x < xmin = xmin
  | otherwise = x

initScene :: Scene
initScene = Scene 0 (1028, 720) (Beats 0 (take historySize $ repeat 60) 60 60 0 0)

plum :: Color
plum = makeColor 0.67 0.194 0.255 1
darker :: Color
darker = makeColor 0.175 0.0212 0.0212 1

main :: IO ()
main = play window darker fps initScene draw handleInput updateState
  where window = InWindow "Tap a Beat" (1028, 720) (20, 20)

draw :: Scene -> Picture
draw (Scene sinceStart (w, h) (Beats _ hist oldBpm bpm lastBeat beat)) =
  Pictures [ drawBeatCircle sinceBeat beatDuration
           , Color white $ drawGraph w h hist sinceBeat oldBpm bpm
           , Color white $ drawBPM bpm
           ]
  where sinceBeat = sinceStart - beat
        beatDelta = beat - lastBeat
        beatDuration = 1 / bpm

updateBeat :: Int -> [Float] -> Float -> Elapsed -> Elapsed -> Beats
updateBeat beats history oldBpm lastBeat beatTime
  = Beats beats newHistory oldBpm bpm lastBeat beatTime
  where bpm = fromIntegral (fps * beats) / beatTime
        instantaneousBPM = (fromIntegral fps) / (beatTime - lastBeat)
        newHistory = take historySize (instantaneousBPM:history)

handleBeat :: Scene -> Scene
handleBeat (Scene t size (Beats n h _ o _ b))
  = Scene t size $ updateBeat (succ n) h o b t

handleInput :: Event -> Scene -> Scene
handleInput (EventResize (w, h)) (Scene t _ b) = Scene t (w, h) b
-- FIXME: Find a better way to quit.
handleInput (EventKey (SpecialKey KeyEsc) _ _ _) _ = error "exit"
handleInput (EventKey (Char 'q') Down _ _) _       = error "exit"
handleInput (EventKey (Char 'r') Down _ _) (Scene _ size _) = initScene
handleInput (EventKey (Char  _ )      Down _ _) s = handleBeat s
handleInput (EventKey (MouseButton _) Down _ _) s = handleBeat s
handleInput (EventKey (SpecialKey _)  Down _ _) s = handleBeat s

handleInput _ scene = scene

updateState :: Elapsed -> Scene -> Scene
updateState elapsed (Scene time size beat) = Scene (time + elapsed) size beat

-- Take time when beat was hit, and current time.
-- Produces a circle shrinking in size since time beat.
drawBeatCircle :: Elapsed -> Elapsed -> Picture
drawBeatCircle sinceBeat beatDuration
  = Color (withAlpha ((1 - (clamp sinceBeat 0 1)) / 4 + 0.2) plum)
  $ circleSolid (500 + 60 * (beatCurve beatDuration sinceBeat))

drawBPM :: Float -> Picture
drawBPM bpm = translate (-70) (-50) $ (text . show . round) bpm

drawAxisTick :: Int -> (Float, Float) -> Picture
drawAxisTick tick (w, h)
  -- major tick
  | tick `mod` majorTicks == 0 = Pictures
    [ Line [(-w / 2, y), (-w / 2 + 15, y)]
    , Translate (-w / 2 + 20) (y - 10) $ Scale 0.3 0.3 (text . show $ label)
    ]
  -- minor tick
  | otherwise = Line [(-w / 2, y), (-w / 2 + 5, y)]
  where tickf  = fromIntegral tick  :: Float
        ticksf = fromIntegral ticks :: Float
        y = align tickf 0 ticksf (-h / 2) (h / 2)
        label = round $ align tickf 0 ticksf (fst bpmRange) (snd bpmRange)


drawAxisTicks :: Float -> Float -> Picture
drawAxisTicks w h = Pictures [ drawAxisTick tick (w, h) | tick <- [0..ticks] ]

drawHistory :: Float -> Float -> [Float] -> Picture
drawHistory w h history
  = Pictures [ drawBpmPoint n bpm | (n, bpm) <- zip [0..] history ]
  where drawBpmPoint :: Int -> Float -> Picture
        drawBpmPoint n bpm = Translate x y $ circle r
          where r = 5  -- instantaneous BPM data-point circle radius.
                y = align bpm (fst bpmRange) (snd bpmRange) (-h / 2) (h / 2)
                x = align (fromIntegral n) 0 (fromIntegral . length $ history)
                          (-w / 2 + 80) (w / 2)

drawGraph :: Int -> Int -> [Float] -> Elapsed -> Float -> Float -> Picture
drawGraph width height history sinceBeat oldBpm bpm
  = Pictures [ Color (withAlpha 0.5 white) $ Line [(-w / 2, ybpm), (w / 2, ybpm)]
             , drawAxisTicks w h
             , drawHistory w h history
             , if null history
                then blank
                else Translate (w/2 - 60) (-h/2) $ Scale 0.2 0.2
                     (Text . show . round $ history!!0)
             ]
  where (w, h) = (fromIntegral width, fromIntegral height - 60)
        lerpBpm = oldBpm + (bpm - oldBpm) * (clamp (5 * sinceBeat) 0 1)
        ybpm = align lerpBpm (fst bpmRange) (snd bpmRange) (-h / 2) (h / 2)


-- Sharply rising curve grows circle once beat was hit, slowly drops off.
beatCurve :: Float -> Float -> Float
beatCurve beatDuration x
  | x <= 0 = 0
  | x <= b / (2*r) = 2*r/b * x
  | x <= b = 1 / (1 - 1 / (2*r)) * (1 - x / b)
  | otherwise = 0
  where b = 30 * beatDuration - 0.1 -- b is duration of animation
        r = 15                      -- r is how sharply the circle radius rises.
--beatCurve x = exp $ -(1 / (0.5*x + 0.2) - 2) ** 2
