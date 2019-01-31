-- Working toward a simple Asteroids game.


import Fran
import qualified StaticTypes as S
import Win32Key  -- for vkeys

main = displayU flyShip2

-- Try out flying the ship

-- flyShip1, flyShip2, flyShip3, flyShipBombs4, flyShip5 :: User -> ImageB

fireKey = vK_SPACE
restartKey = vK_ESCAPE

restart, fire :: User -> Event User

restart = keyUser restartKey
fire    = keyUser fireKey

keyUser key = nextUser_ (keyPress key)

rotateControl u = 5 * keysSign vK_LEFT vK_RIGHT u
thrustControl u = 3 * thrustState u
thrustState   u = keysSign vK_UP vK_DOWN u

-- Hit escape to start over with these flying demos.

-- With this guy, the up/down keys adjust *speed*, not velocity.

flyShip1 u = ship pos vel angle u `untilB` restart u ==> flyShip1
 where
   pos   = integral vel u
   vel   = vector2Polar speed angle
   angle = integral angularVel u
   speed = integral forwardAccel u
   angularVel   = rotateControl u
   forwardAccel = thrustControl u

-- The one you'd expect.

flyShip2 u = ship pos vel angle u `untilB` restart u ==> flyShip2
 where
   pos   = integral vel u
   vel   = integral acc u
   angle = integral angularVel u
   acc	 = vector2Polar accelMag angle
   angularVel = rotateControl u
   accelMag   = thrustControl u


-- Misc experimental

-- Add some sound.  First just buzz upon each space press

spaceBuzzer u = loop u `untilB` restart u ==> spaceBuzzer
 where
  loop u = silence `untilB` fire u ==> \ u ->
           shotSound u `mix` loop u

engineSound = importWave "..\\..\\Media\\sengine.wav"

-- This is supposed to be a short sound.  However, the delay between the
-- start event time and its response is so long that most of the sound
-- gets chopped off.
shotSound u = engineSound `untilB` userTimeIs 0.5 u -=> silence

shoot :: Vector2B -> Vector2B -> RealB -> User -> ImageB
shoot pos vel angle u = accumB over emptyImage missileE
 where
   missileE =
     keyPress fireKey u `snapshot_` tripleB pos vel angle
                        `afterE`    u
                        ==>         shootMissile
   shootMissile ((p,v,a),u) =
     move (constantB p + atRate (constantB v') u) (missile dur u)
      `untilB` userTimeIs dur u -=> emptyImage
    where
      v' = v + S.vector2Polar boost a
      boost = 1  -- boost beyond ship velocity
      dur   = 2  -- missile's duration

missile :: DTime -> User -> ImageB
missile dur u = soundImage (volume vol missileSound) `over` im
 where
  im  = turn (2 * userTime u) missileShape
  vol = 1 - userTime u / constantB dur

missileShape = withColor yellow $
               stretch 0.1      $
               star 3 7

missileSound = pitch 100 $ importWave "..\\..\\Media\\bounce.wav"

thrustSound u = volume vol engineSound
 where
   vol = abs (thrustState u)

buzzyShip u = flyShip1 u `over` soundImage (spaceBuzzer u)

ship pos vel angle u = move pos (
                         flipImage shipBook (shipPage angle) `over`
                         soundImage (thrustSound u)          `over`
                         shoot pos vel angle u )
 where
   -- The pages start out pointing up, i.e., pi/2, and rotate clockwise
   -- (negative), so we have to adjust
   shipPage angle = shipPagesPerRadian * (halfPi - angle)
  
shipPagesPerRadian :: Floating a => a
shipPagesPerRadian = fromInt (flipBookPages shipBook) / twicePi

halfPi, twicePi :: Floating a => a

halfPi = pi / 2
twicePi = 2 * pi

-- 0, 1, -1, based on key presses
keysSign :: VKey -> VKey -> User -> RealB
keysSign posKey negKey u =
 condB (keyState posKey u) ( 1) $
 condB (keyState negKey u) (-1) 0

keyState :: VKey -> User -> BoolB
keyState key u = stepper False (keyPress   key u -=> True
                            .|. keyRelease key u -=> False )


-- All of the flipbooks.  (See "gameBooks" below.)

[ donutBook, pyramidBook, sphereBook, cubeBook, shipBook, shipShieldBook ] =
 gameBooks donutsSurface
	   [(5,6), (10,4), (20,2), (20,2), (10,4), (10,4)]
	   0

[ spikeyBook ] = gameBooks spikeysSurface [ (6,10) ] 0

[ spikeyBookBig ] = gameBooks spikeysSurfaceBig [ (6,10) ] 0

-- Helpers for constructing gamebooks.

-- Too bad we have to hardwire the path.  There should be a searching
-- mechanism.

donutsSurface = bitmapDDSurface "..\\..\\Media\\donuts.bmp"

spikeysSurface = bitmapDDSurface "..\\..\\Media\\spikeys60 small.bmp"
spikeysSurfaceBig = bitmapDDSurface "..\\..\\Media\\spikeys60 big.bmp"

gameBooks :: HDDSurface -> [(Int, Int)] -> Int -> [HFlipBook]

gameBooks surf = loop
 where
  (surfWidth, _) = ddSurfaceSize surf
  loop [] _  = []

  loop (descr : descrs') top = book : loop descrs' top'
    where
      (book, top') = oneBook descr top

      oneBook :: (Int, Int) -> Int -> (HFlipBook, Int)

      oneBook (columns, rows) top =
       --trace ("flipBook <surf> " ++ show (size, top, columns, rows) ++ "\n") $
       ( flipBook surf size size 0 top columns rows,
	 bottom )
       where
	 size = surfWidth `div` columns
	 bottom = top + size * rows

-- I don't quite understand the bullets.



-- Test out flipbook display

tryBook :: HFlipBook -> User -> ImageB

tryBook book u = flipImage book (50 * (time - constantB t0))
 where t0 = userStartTime u


fly0 :: HFlipBook -> User -> ImageB

fly0 book u = move (mouseMotion u) (tryBook book u)

