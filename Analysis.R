##########################
# Analysis: Started 3/29 #
##########################

## Load required packages

library(tuneR)


## Lets start with a song that I have, Mercy by Dave Matthews Band. 

mercy = readMP3("mercy.mp3")

# noSilence() cuts out silence at beginning and end of song. USEFULL!
mercy.nosil = noSilence(mercy)  

# audspec() don't know what this does.
pspectrum <- powspec(mercy@left,mercy@samp.rate)
aspectrum <- audspec(pspectrum, mercy@samp.rate)

# channel() we can pick from which = c("right","left","both","mirror"). USEFULL!
mercy.left = channel(mercy, which = "left") 

# deltas() don't know what this does.
m = melfcc(mercy.left, frames_in_rows=FALSE)
d = deltas(m)

# equalWave() This function doesn't exist. 
wal = readMP3("wal.mp3")
wal.nosil = noSilence(wal)

equalWave(mercy.nosil, wal.nosil)

# extractWave() cuts out a piece of the song. USEFULL!
mercy.extract = extractWave(mercy.nosil, from = 15, to = 30, xunit = "time")

# FF()






################################
## Song to Data Frame function

songtoDF = function(song){
  x = noSilence(song)
  left = x@left
  right = x@right
  d.left = data.frame(mean.l = mean(left), sd.l = sd(left), median.l = median(left), min.l = min(left), 
                 max.l = max(left), range.l = max(left) - min(left))
  d.right = data.frame(mean.r = mean(right), sd.r = sd(right), median.r = median(right), min.r = min(right), 
                 max.r = max(right), range.r = max(right) - min(right))
  d = data.frame(d.left, d.right)
  return(d)
}

data = songtoDF(mercy)


######################################################################
## Lets load the songs that were picked and us the function on those

Aquarius = readWave("aquarius.wav")
BigGirls = readWave("biggirls.wav")
BoomBoom = readWave("boom.wav")
DontForget = readWave("dontforget.wav")
DropIt = readWave("dropit.wav")
Fame = readWave("fame.wav")
HalfBreed = readWave("halfbreed.wav")
HeyJude = readWave("jude.wav")
IllBeThere = readWave("bethere.wav")
Drag = readWave("drag.wav")
LoveChild = readWave("lovechild.wav")
Jagger = readWave("jagger.wav")
NoOne = readWave("noone.wav")
ReachOut = readWave("reachout.wav")
SaySomething = readWave("saysomething.wav")
Georgia = readWave("georgia.wav")
Umbrella = readWave("umbrella.wav")
WeBelong = readWave("together.wav")
Stronger = readWave("stronger.wav")

## Now apply the function

Aquarius.dat = songtoDF(Aquarius)
BigGirls.dat = readWave("biggirls.wav")
BoomBoom.dat = readWave("boom.wav")
DontForget.dat = readWave("dontforget.wav")
DropIt.dat = readWave("dropit.wav")
Fame.dat = readWave("fame.wav")
HalfBreed.dat = readWave("halfbreed.wav")
HeyJude.dat = readWave("jude.wav")
IllBeThere.dat = readWave("bethere.wav")
Drag.dat = readWave("drag.wav")
LoveChild.dat = readWave("lovechild.wav")
Jagger.dat = readWave("jagger.wav")
NoOne.dat = readWave("noone.wav")
ReachOut.dat = readWave("reachout.wav")
SaySomething.dat = readWave("saysomething.wav")
Georgia.dat = readWave("georgia.wav")
Umbrella.dat = readWave("umbrella.wav")
WeBelong.dat = readWave("together.wav")
Stronger.dat = readWave("stronger.wav")

