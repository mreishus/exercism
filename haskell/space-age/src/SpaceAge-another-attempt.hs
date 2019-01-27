{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

newtype EarthYears = EarthYears Float
data Seconds = Seconds Float

ageOn :: Planet -> Seconds -> Seconds
ageOn planet seconds = Seconds $ seconds_f / period_f
  where (Seconds seconds_f) = seconds
        (Seconds period_f) = orbitalPeriod planet

orbitalPeriod :: Planet -> Seconds
orbitalPeriod Earth = toSeconds $ EarthYears 1
orbitalPeriod Mercury = toSeconds $ EarthYears 0.2408467
orbitalPeriod Venus = toSeconds $ EarthYears 0.61519726
orbitalPeriod Mars = toSeconds $ EarthYears 1.8808158
orbitalPeriod Jupiter = toSeconds $ EarthYears 11.862615
orbitalPeriod Saturn = toSeconds $ EarthYears 29.447498
orbitalPeriod Uranus = toSeconds $ EarthYears 84.016846
orbitalPeriod Neptune = toSeconds $ EarthYears 164.79132

toSeconds :: EarthYears -> Seconds
toSeconds ey = Seconds $ 31557600 * x
  where (EarthYears x) = ey

{-|
  My notes:

  1. I am trying out using newtype to specify what units I am working with
  instead of useless nameless Int, Float, etc, but running into some problems.

  I seem to have done ok with 'EarthYears'.

  I tried adding adding a `Seconds` type with these steps.
     - Add `newtype Seconds = Seconds Float`
     - Converting most `Floats` in type signatures to `Seconds`
     - Changing `toSeconds` to the following:
       -- toSeconds :: EarthYears -> Seconds
       -- toSeconds (EarthYears x) = Seconds $ 31557600 * x
     - However, I couldn't get the seconds to divide in `ageOn` - no implementation of Fractional.
     
  Maybe I need to use pattern matching to pull out the floats from the Seconds?  Not sure if I was
  going down the right road.

  2. The repetition of "toSeconds $ EarthYears" seems ugly.
  I could make a function like so:

  -- x :: Float -> Float
  -- x = toSeconds . EarthYears

  And set up the definitions like:

  orbitalPeriod Earth = x 1
  orbitalPeriod Mercury = x 0.2408467

  perhaps a better name is in order, and it gets rid of the reptition, however
  it seems to defeat my type safety..

  Not sure where to go w/ this.
-}
