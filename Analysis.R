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
  d.left = data.frame(mean = mean(left), sd = sd(left), median = median(left), min = min(left), 
                 max = max(left), range = min(left) - max(left))
  d.right = data.frame(mean = mean(right), sd = sd(right), median = median(right), min = min(right), 
                 max = max(right), range = min(right) - max(right))
  d = list(left = d.left, right = d.right)
  return(d)
}

data = songtoDF(mercy)






