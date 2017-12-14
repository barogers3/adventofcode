#day 13
# http://adventofcode.com/2017/day/13
# You need to cross a vast firewall. The firewall consists of several layers, each with a security scanner that moves back and forth across the layer. To succeed, you must not be detected by a scanner.
# 
# By studying the firewall briefly, you are able to record (in your puzzle input) the depth of each layer and the range of the scanning area for the scanner within it, written as depth: range. Each layer has a thickness of exactly 1. A layer at depth 0 begins immediately inside the firewall; a layer at depth 1 would start immediately after that.
#Your plan is to hitch a ride on a packet about to move through the firewall. The packet will travel along the top of each layer, and it moves at one layer per picosecond. Each picosecond, the packet moves one layer forward (its first move takes it into layer 0), and then the scanners move one step. If there is a scanner at the top of the layer as your packet enters it, you are caught. (If a scanner moves into the top of its layer while you are there, you are not caught: it doesn't have time to notice you before you leave.) If you were to do this in the configuration above, marking your current position with parentheses, your passage through the firewall would look like this:
# In this situation, you are caught in layers 0 and 6, because your packet entered the layer when its scanner was at the top when you entered it. You are not caught in layer 1, since the scanner moved into the top of the layer once you were already there.
# 
# The severity of getting caught on a layer is equal to its depth multiplied by its range. (Ignore layers in which you do not get caught.) The severity of the whole trip is the sum of these values. In the example above, the trip severity is 0*3 + 6*4 = 24.
# 
# Given the details of the firewall you've recorded, if you leave immediately, what is the severity of your whole trip?
# 


library(dplyr)

rawinput <- tribble(~V1,
    "0: 5",
    "1: 2",
    "2: 3",
    "4: 4",
    "6: 6",
    "8: 4",
   "10: 6",
  "12: 10",
   "14: 6",
   "16: 8",
   "18: 6",
   "20: 9",
   "22: 8",
   "24: 8",
   "26: 8",
  "28: 12",
  "30: 12",
   "32: 8",
   "34: 8",
  "36: 12",
  "38: 14",
  "40: 12",
  "42: 10",
  "44: 14",
  "46: 12",
  "48: 12",
  "50: 24",
  "52: 14",
  "54: 12",
  "56: 12",
  "58: 14",
  "60: 12",
  "62: 14",
  "64: 12",
  "66: 14",
  "68: 14",
  "72: 14",
  "74: 14",
  "80: 14",
  "82: 14",
  "86: 14",
  "90: 18",
  "92: 17"
  )



testinput <- tribble(~V1,
               "0: 3",
               "1: 2",
               "4: 4",
               "6: 4"
               )

library(stringr)

input <- as.data.frame(str_split_fixed(rawinput$V1,":",2))
input <- input %>% 
  mutate(depth = as.numeric(as.character(V1)),
         range = as.numeric(as.character(V2))) %>%
  select(depth, range)

#add in empty moves
all_space <- data.frame(depth = seq(1:max(input$depth)))
input_all <- input %>%
  full_join(all_space, by = 'depth') %>%
  mutate(range = ifelse(is.na(range),0,range)) %>%
  arrange(depth)

caught <- data.frame()

for (i in 0:(nrow(input_all)-1)) { #
 if (i==0) {
   moving_df <- input_all %>%
     mutate(sensor = ifelse(range == 0,0,1),
            direction = 1)
 }
  
  #see if caught
  caught_new <- moving_df %>%
    filter(depth == i) %>%
    mutate(caught = ifelse(sensor == 1,1,0)) #sensor at top so caught
  
  caught <- rbind(caught,
                  caught_new)
  
  #move the sensors
  moving_df <- moving_df %>%
    mutate(direction = ifelse(sensor == range,-1,
                              ifelse(sensor == 1,1,direction)),
      sensor = ifelse(range == 0,0,
                      sensor + direction))
   
}
i=i+1

severity <- caught %>%
  filter(caught == 1) %>%
  mutate(severity = depth * range)

sum(severity$severity)

#788

#part 2
# Now, you need to pass through the firewall without being caught - easier said than done.
# 
# You can't control the speed of the packet, but you can delay it any number of picoseconds. For each picosecond you delay the packet before beginning your trip, all security scanners move one step. You're not in the firewall during this time; you don't enter layer 0 until you stop delaying the packet.
# 
# In the example above, if you delay 10 picoseconds (picoseconds 0 - 9), you won't get caught:
caught <- data.frame()

startpos <- function(df,delay){
    if (delay == 1) {
      moving_df2 <- df
    }
    #move the sensors
    moving_df2 <- moving_df2 %>%
      mutate(direction = ifelse(sensor == range,-1,
                                ifelse(sensor == 1,1,direction)),
             sensor = ifelse(range == 0,0,
                             sensor + direction))
    return(moving_df2)
}


firewall <- function(delay) {
  caught <- data.frame()
  moving_df <- input_all %>%
    mutate(sensor = ifelse(range == 0,0,1),
           direction = 1)
  print(delay)
  moving_df <- map2_df(moving_df,seq(1,10),startpos)
  
  for (i in 0:(nrow(input_all)-1)) { #

    #see if caught
    caught_new <- moving_df %>%
      filter(depth == i) %>%
      mutate(caught = ifelse(sensor == 1,1,0)) #sensor at top so caught
    
    caught <- rbind(caught,
                    caught_new)
    
    #move the sensors
    moving_df <- moving_df %>%
      mutate(direction = ifelse(sensor == range,-1,
                                ifelse(sensor == 1,1,direction)),
             sensor = ifelse(range == 0,0,
                             sensor + direction))
    
    if (nrow(filter(caught,caught == 1)) >0) {
      print("got caught!")
      break
    }
  }
  if (i==92) {
    print("winner!")
    print(delay)
    i="winner" #breaks 
  }
  return(i)
}


library(purrr)

test <- map_dbl(seq(1294,10000),firewall)



