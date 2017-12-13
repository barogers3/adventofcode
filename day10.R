#day 10
# 
# Suppose we instead only had a circular list containing five elements, 0, 1, 2, 3, 4, and were given input lengths of 3, 4, 1, 5.
# 
# The list begins as [0] 1 2 3 4 (where square brackets indicate the current position).
# The first length, 3, selects ([0] 1 2) 3 4 (where parentheses indicate the sublist to be reversed).
# After reversing that section (0 1 2 into 2 1 0), we get ([2] 1 0) 3 4.
# Then, the current position moves forward by the length, 3, plus the skip size, 0: 2 1 0 [3] 4. Finally, the skip size increases to 1.
# The second length, 4, selects a section which wraps: 2 1) 0 ([3] 4.
#The sublist 3 4 2 1 is reversed to form 1 2 4 3: 4 3) 0 ([1] 2.
#The current position moves forward by the length plus the skip size, a total of 5, causing it not to move because it wraps around: 4 3 0 [1] 2. The skip size increases to 2.
#The third length, 1, selects a sublist of a single element, and so reversing it has no effect.
#The current position moves forward by the length (1) plus the skip size (2): 4 [3] 0 1 2. The skip size increases to 3.
#The fourth length, 5, selects every element starting with the second: 4) ([3] 0 1 2. Reversing this sublist (3 0 1 2 4 into 4 2 1 0 3) produces: 3) ([4] 2 1 0.
#Finally, the current position moves forward by 8: 3 4 2 1 [0]. The skip size increases to 4.
#In this example, the first two numbers in the list end up being 3 and 4; to check the process, you can multiply them together to produce 12.
#
#However, you should instead use the standard list size of 256 (with values 0 to 255) and the sequence of lengths in your puzzle input. Once this process is complete, what is the result of multiplying the first two numbers in the list?


startlist <- seq(0,255)
cur_pos <- 1
skip_size <- 0
input <- c(94,84,0,79,2,27,81,1,123,93,218,23,103,255,254,243)
#input <- c(3,4,1,5)

for (i in 1:length(input)){
  if (i==1) {
    curlist <- startlist
  }
  
  len <- input[i]
  
  #select sublist
  if (len == 0) {
    #skip
  } else {
    sublist <- curlist[cur_pos:(cur_pos +len - 1)]
    if (length(sublist) != len) {
      print("error - abort")
      break
    }
    #logic for wrapping
      if (sum(is.na(sublist)) > 0) {
        na_count <- sum(is.na(sublist))
        na_sublist <- curlist[1:na_count] #start at beginning
        sublist[(length(sublist) - na_count +1):length(sublist)] <- na_sublist
      }
    
    #reverse sublist
    rev_sublist <- rev(sublist)
    
    #replace elements with new list
    #logic for wrapping
    if ((cur_pos + len - 1) > length(curlist)) {
      #print("test")
      curlist[cur_pos:length(curlist)] <- rev_sublist[1:length(curlist[cur_pos:length(curlist)])]
      curlist[1:(length(rev_sublist) - length(curlist[cur_pos:length(curlist)]))] <- rev_sublist[(length(curlist[cur_pos:length(curlist)])+1):length(rev_sublist)]
    } else {
      curlist[cur_pos:(cur_pos + len - 1)] <- rev_sublist
    }
  }
  #current position moves forward length plus skip size
  #logic for wrapping
  if ((cur_pos + len + skip_size) > length(curlist)) {
    if ((len + skip_size) > length(curlist)) {
      moveforward <- len + skip_size - length(curlist)
      spaceuntilend <- length(curlist) - cur_pos
      if ((cur_pos + moveforward) <= length(curlist)) {
        cur_pos <- cur_pos + moveforward
      } else {
        cur_pos <- moveforward - spaceuntilend
      }
    } else {
      moveforward <- len + skip_size
      spaceuntilend <- length(curlist) - cur_pos
      cur_pos <- len + skip_size - spaceuntilend
    }
  } else {
    cur_pos <- cur_pos + len + skip_size
    }
  
  skip_size <- skip_size + 1
  if (cur_pos > length(startlist)){
    print(i)
  }
}
i=i+1
curlist[1] * curlist[2]

#23715

# --- Part Two ---
#   The logic you've constructed forms a single round of the Knot Hash algorithm; running the full thing requires many of these rounds. Some input and output processing is also required.
# 
# First, from now on, your input should be taken not as a list of numbers, but as a string of bytes instead. Unless otherwise specified, convert characters to bytes using their ASCII codes. This will allow you to handle arbitrary ASCII strings, and it also ensures that your input lengths are never larger than 255. For example, if you are given 1,2,3, you should convert it to the ASCII codes for each character: 49,44,50,44,51.


acii_input <- as.numeric(charToRaw("94,84,0,79,2,27,81,1,123,93,218,23,103,255,254,243"))




