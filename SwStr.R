library(dplyr)
library(tidyverse)
library(Lahman)

#### 2016 retrosheet
fields <- read_csv("https://raw.githubusercontent.com/beanumber/baseball_R/master/data/fields.csv")  
data2016 <- read_csv("https://raw.githubusercontent.com/beanumber/baseball_R/master/data/all2016.csv",  
                     col_names = pull(fields, Header),  
                     na = character()) 

## Loop over every pitch seen by Cubs hitters
swStrikes_Calc = function(pitches) {
  total_pitches = 0
  swinging_strikes = 0
  for (i in pitches) {
    for (j in strsplit(i, "")[[1]]) { 
      total_pitches = total_pitches + 1
      if(j == "S") { swinging_strikes = swinging_strikes + 1 }
    }
  }
  hitting_swStr = round((swinging_strikes / total_pitches) * 100, 2)
  hitting_swStr
}


## Swinging-Strike Percentage by inning
swStr_Inning = function(pitches) {
  for (i in pitches) {
    pitches = 0
    sw_strikes = 0
    for(j in i) {
      for(k in strsplit(j, "")[[1]]) {
        pitches = pitches + 1
        if(k == "S") { sw_strikes = sw_strikes + 1 }
      }
    }
    print(round((sw_strikes / pitches) * 100, 1))
  }
}


### Batter hitting data
#unique(data2016$BAT_ID)
hitting <- data2016 %>%  
  mutate(pseq = gsub("[.>123N+*MNUV]","", PITCH_SEQ_TX)) 

hitter_data = split(hitting$pseq, f = hitting$BAT_ID)


regulars = hitter_data[lapply(hitter_data, length) > 400]

# Swinging Strike % for Jose Abreu 2016



## Cubs hitting data for swinging strike purposes
data2016 %>%
  filter((substr(GAME_ID, 0, 3) == "CHN" & BAT_HOME_ID == 1) | 
           (AWAY_TEAM_ID == "CHN" & BAT_HOME_ID == 0)) -> cubs

cubs <- cubs %>%  
  mutate(pseq = gsub("[.>123N+*MNUV]","", PITCH_SEQ_TX)) 

pitch_seq = cubs$pseq
pitch_seq

# CHC 2016 Swinging Strike Percentage
swStrikes_Calc(pitch_seq)


# Swinging Strike Percentage by Inning
cubs_innings <- split(cubs$pseq, f = cubs$INN_CT)
swStr_Inning(cubs_innings)



######## What about for a team's pitchers?
cubs_pitching <- data2016 %>%
  filter((substr(GAME_ID, 0, 3) == "CHN" & BAT_HOME_ID == 0) | (AWAY_TEAM_ID == "CHN" & BAT_HOME_ID == 1))
  
cubs_pitching <- cubs_pitching %>%  
  mutate(pseq = gsub("[.>123N+*MNUV]","", PITCH_SEQ_TX)) 

pitchers_seq = cubs_pitching$pseq

# Swinging Strikes Percentage for Cubs Pitchers
swStrikes_Calc(pitchers_seq)


# Swinging Strike Percentage for Cubs Pitchers, By Inning
cubs_pitching_innings = split(cubs_pitching$pseq, f = cubs_pitching$INN_CT)

swStr_Inning(cubs_pitching_innings)





