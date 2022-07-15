:set -XOverloadedStrings
:set prompt ""

import Data.Maybe (isJust, fromJust)
import Data.List (elemIndex)
import qualified Sound.Tidal.Tempo as T
import Sound.Tidal.Context
import System.IO (hSetEncoding, stdout, utf8)
hSetEncoding stdout utf8

-- total latency = oLatency + cFrameTimespan
tidal <- startTidal (superdirtTarget {oLatency = 0.1, oAddress = "127.0.0.1", oPort = 57120}) (defaultConfig {cFrameTimespan = 1/20,cCtrlAddr = "0.0.0.0", cCtrlPort = 6060})

:{
let p = streamReplace tidal
    hush = streamHush tidal
    list = streamList tidal
    mute = streamMute tidal
    unmute = streamUnmute tidal
    solo = streamSolo tidal
    unsolo = streamUnsolo tidal
    once = streamOnce tidal
    first = streamFirst tidal
    asap = once
    nudgeAll = streamNudgeAll tidal
    all = streamAll tidal
    resetCycles = streamResetCycles tidal
    setcps = asap . cps
    xfade i = transition tidal True (Sound.Tidal.Transition.xfadeIn 4) i
    xfadeIn i t = transition tidal True (Sound.Tidal.Transition.xfadeIn t) i
    histpan i t = transition tidal True (Sound.Tidal.Transition.histpan t) i
    wait i t = transition tidal True (Sound.Tidal.Transition.wait t) i
    waitT i f t = transition tidal True (Sound.Tidal.Transition.waitT f t) i
    jump i = transition tidal True (Sound.Tidal.Transition.jump) i
    jumpIn i t = transition tidal True (Sound.Tidal.Transition.jumpIn t) i
    jumpIn' i t = transition tidal True (Sound.Tidal.Transition.jumpIn' t) i
    jumpMod i t = transition tidal True (Sound.Tidal.Transition.jumpMod t) i
    mortal i lifespan release = transition tidal True (Sound.Tidal.Transition.mortal lifespan release) i
    interpolate i = transition tidal True (Sound.Tidal.Transition.interpolate) i
    interpolateIn i t = transition tidal True (Sound.Tidal.Transition.interpolateIn t) i
    clutch i = transition tidal True (Sound.Tidal.Transition.clutch) i
    clutchIn i t = transition tidal True (Sound.Tidal.Transition.clutchIn t) i
    anticipate i = transition tidal True (Sound.Tidal.Transition.anticipate) i
    anticipateIn i t = transition tidal True (Sound.Tidal.Transition.anticipateIn t) i
    forId i t = transition tidal False (Sound.Tidal.Transition.mortalOverlay t) i
    d1 = p 1 . (|< orbit 0)
    d2 = p 2 . (|< orbit 1)
    d3 = p 3 . (|< orbit 2)
    d4 = p 4 . (|< orbit 3)
    d5 = p 5 . (|< orbit 4)
    d6 = p 6 . (|< orbit 5)
    d7 = p 7 . (|< orbit 6)
    d8 = p 8 . (|< orbit 7)
    d9 = p 9 . (|< orbit 8)
    d10 = p 10 . (|< orbit 9)
    d11 = p 11 . (|< orbit 10)
    d12 = p 12 . (|< orbit 11)
    d13 = p 13
    d14 = p 14
    d15 = p 15
    d16 = p 16
:}

:{
let getState = streamGet tidal
    setI = streamSetI tidal
    setF = streamSetF tidal
    setS = streamSetS tidal
    setR = streamSetR tidal
    setB = streamSetB tidal
:}

:{
capply
  :: (Ord a1, Num a1) =>
     Pattern a1
     -> (Pattern a2 -> Pattern a2) -> Pattern a2 -> Pattern a2
capply condpat effectpat = every
    (fmap (\x -> if x > 0 then 1 else 0)
    (segment 1 condpat)) (effectpat)
:}

:{
capply'
  :: (a1 -> Int)
     -> Pattern a1
     -> (Pattern a2 -> Pattern a2)
     -> Pattern a2
     -> Pattern a2
capply' func condpat effectpat = every
    (fmap func (segment 1 condpat)) (effectpat)
:}

:{
pingpong = pF "pingpong"
pingpongt = pF "pingpongt"
pingpongfb = pF "pingpongfb"
:}

:{
theta = pF "theta"
phi = pF "phi"
:}

:{
real = pF "real"
imag = pF "imag"
realHpf = pI "realHpf"
:}

:{
vibratoFreq = pF "vibratoFreq"
vibratoDepth = pF "vibratoDepth"
:}

:{
scReverb = pF "scReverb"
ice = pF "ice"
freeverb = pF "freeverb"
damp = pF "damp"
:}

:{
mix
  :: (Pattern ValueMap -> Pattern ValueMap)
     -> Pattern ValueMap -> Pattern ValueMap
mix f p = stack [p, f $ p]
:}

:{
ifb :: (Bool) -> (Pattern a -> Pattern a) ->  Pattern a -> Pattern a
ifb test f p = splitQueries $ p {query = apply}
  where apply st | test = query (f p) st
                 | otherwise = query p st
:}

:{
let resetCyclesTo n = T.changeTempo (sTempoMV tidal) (\t tempo -> tempo {T.atTime = t, T.atCycle = n})
:}

:{
majorC :: Num a => [a]
majorC = [0,2,4,5,7,9,11]

minorC :: Num a => [a]
minorC = [0,2,3,5,7,8,10]

scaleKey :: (Eq a, Data.String.IsString a, Num b) => a -> [b] -> [b]
scaleKey key scaleC
  | elem key ["c"] = scaleC
  | elem key ["cs","df"] = map (+1) scaleC
  | elem key ["d"] = map (+2) scaleC
  | elem key ["ds", "ef"] = map (+3) scaleC
  | elem key ["e"] = map (+4) scaleC
  | elem key ["f"] = map (+5) scaleC
  | elem key ["fs", "gf"] = map (+6) scaleC
  | elem key ["g"] = map (+7) scaleC
  | elem key ["gs", "af"] = map (+8) scaleC
  | elem key ["gs", "af"] = map (+9) scaleC
  | elem key ["a"] = map (+10) scaleC
  | elem key ["as", "bf"] = map (+11) scaleC
  | elem key ["b"] = map (+12) scaleC
  | otherwise = error "invalid key name!"

degreesUp :: (Num a, Eq a1, Data.String.IsString a1) => a1 -> Int -> Int -> a
degreesUp key degree n =
  noteInScale (majorScale) (degree+elemScaleIndex n)
  where majorScale = scaleKey key majorC
        octave s x = x `div` length s
        noteInScale s x = (s !! (mod x (length s))) + fromIntegral (12 * octave s x)
        elemScaleIndex n
          | isJust(elemIndex (mod n 12) majorScale) = fromJust(elemIndex (mod n 12) majorScale) + (length majorScale)*(n `div` 12)
          | otherwise = elemScaleIndex (n+1)

degreesUp' :: (Num a, Eq a1, Data.String.IsString a1) => a1 -> Int -> Int -> a
degreesUp' key degree n
  | degree==0  = degreesUp key 0 n
  | degree>0   = degreesUp key (degree-1) n
  | degree<0   = degreesUp key (degree+1) n
:}

:set prompt "tidal> "
:set prompt-cont ""
