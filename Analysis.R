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

# Periodogram

stuff = periodogram(mono(Aquarius, "left"), width = 1024)
hello = periodogram(mono(SaySomething, "left"), width = 1024)

max(hello@spec[[3]])



################################
## Song to Data Frame function

songtoDF = function(song){
  x = noSilence(song)
  left = x@left
  right = x@right
  d.left = data.frame(mean.l = mean(left), sd.l = sd(left), median.l = median(left), min.l = min(left), 
                 max.l = max(left), range.l = max(left) - min(left))
  if(mean(right) == "NaN"){
    d.right = data.frame(mean.r = mean(left), sd.r = sd(left), median.r = median(left), min.r = min(left),
                         max.r = max(left), range.r = max(left) - min(left))
  } else{
    d.right = data.frame(mean.r = mean(right), sd.r = sd(right), median.r = median(right), min.r = min(right), 
                 max.r = max(right), range.r = max(right) - min(right))
  }
  d = data.frame(d.left, d.right)
  return(d)
}



######################################################################
## Lets load the songs that were picked and us the function on those

Aquarius = readWave("aquarius.wav")
BigGirls = readWave("biggirls.wav")
Boom = readWave("boom.wav")
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
WorkItOut = readWave("workitout.wav")


## Now apply the function

songs = c(HalfBreed, Georgia, Aquarius, HeyJude, LoveChild, Drag, IllBeThere, WorkItOut,
          Fame, ReachOut, Stronger, Jagger, NoOne, Umbrella, BigGirls, DontForget, Boom,
          DropIt, SaySomething, WeBelong)

names = c("HalfBreed", "Georgia", "Aquarius", "HeyJude", "LoveChild", "Drag", "IllBeThere", 
          "WorkItOut", "Fame", "ReachOut", "Stronger", "Jagger", "NoOne", "Umbrella", 
          "BigGirls", "DontForget", "Boom", "DropIt", "SaySomething", "WeBelong")

songstoDF = function(songs, names){
  data = NULL
  for(i in 1:length(songs)){
    d = songtoDF(songs[[i]])
    if(i == 1){data = d} else {data = rbind(data, d)}
  }
  end = cbind(names,data)
  return(end)
}

info = songstoDF(songs, names)

data = cbind(songdata[,-1], info[,-1])
decade = c(rep("old",10), rep("new",10))
data = cbind(data, decade)
data = data[,-1]

write.csv(data, file = "data.csv")

data = read.csv("data.csv")[,-1]


library(ggplot2)

qplot(x = sd.r, y = mean.r, color = decade, data = data) + geom_smooth(aes(group = decade), se = F)

qplot(decade, mean.r, group = decade, geom = "boxplot", data = data)

qplot(max.r, min.r, color = decade, data = data) + geom_smooth(aes(group = decade))


