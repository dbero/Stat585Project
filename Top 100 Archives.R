##############################
# Billboard Top 100 Archives #
##############################

## Load necessary packages
library(XML)


## Lets get a list of the top songs over the last 60+ years.

getyear = function(year) {
  url = sprintf("http://www.billboard.com/archive/charts/%d/hot-100", year)
  table = readHTMLTable(url)$`NULL`
  
  # rename the columns
  names(table)[1] = "Week"
  names(table)[2] = "Song"
  names(table)[3] = "Artist"
  
  ## This function makes sure there is a song and artist for each week. Repeats were originally
  ## left as <NA>
  for(i in 1:nrow(table)){
    if(is.na(table[i,]$Song)){
      table[i,]$Song = table[i-1,]$Song
    } else{
      table[i,]$Song = table[i,]$Song
    }
    if(is.na(table[i,]$Artist)){
      table[i,]$Artist = table[i-1,]$Artist
    } else{
      table[i,]$Artist = table[i,]$Artist
    }
  }
  return(table)
}

## We can get all of the years using this command. Note: the for loop is actually faster than the ldply() statement 
## by 20.904 seconds. 

data = NULL
years = c(1958:2014)

# Use this command. 156.524 seconds (2:37)
for(i in 1:length(years)){
  y = getyear(years[i])
  y$Year = years[i]
  if(i == 1){
      data = y
    } else{
        data = rbind(data,y)
      }
}

# Don't use this one. 177.428 seconds (2:57)
data = ldply(as.list(years), function(x) {
  y = getyear(x)
  y$Year <- x
  y
})


###############################################################################
## Lets get a random sample of 10 songs from 1965-1975 and 10 songs from 
## 2004-2014. Note that these are all of the number one songs on the Billboard
## top 100. Some of them appear as number one for consecutive weeks. This
## is okay, we want more popular songs to have a higher prob of getting 
## selected. After all, this project is about checking if "hit" songs from
## today are more homogenous than hit songs from the 60's-70's. Another 
## note, we could break the songs into decades to do a more in-depth analysis
## over time. This could be interesting!


data.6070s = data[which(data$Year %in% 1965:1975),]
set.seed(123456)
oldies = data.6070s[sample(1:nrow(data.6070s), 10, replace=F),]

data.0010s = data[which(data$Year %in% 2004:2014),]
set.seed(123456)
newsongs = data.0010s[sample(1:nrow(data.0010s), 10, replace=F),]


## Lets save those data frames so we don't have to do this again
write.csv(oldies,file = "oldies.csv")
write.csv(newsongs,file = "newsongs.csv")

oldies = read.csv("oldies.csv")
newsongs = read.csv("newsongs.csv")
