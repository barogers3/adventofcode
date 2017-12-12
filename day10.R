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


startlist <- seq(0,4)
cur_pos <- 1
skip_size <- 0
#input <- c(94,84,0,79,2,27,81,1,123,93,218,23,103,255,254,243)
input <- c(3,4,1,5)

for (i in 1:length(input)){
  if (i==1) {
    curlist <- startlist
  }
  
  len <- input[i]
  
  #select sublist
  sublist <- curlist[cur_pos:(cur_pos +len - 1)]
  #logic for wrapping
    if (sum(is.na(sublist)) > 0) {
      na_count <- sum(is.na(sublist))
      na_sublist <- curlist[1:na_count]
      sublist[(length(sublist) - na_count +1):length(sublist)] <- na_sublist
    }
  
  #reverse sublist
  rev_sublist <- rev(sublist)
  
  #replace elements with new list
  curlist[cur_pos:(cur_pos + len - 1)] <- rev_sublist
  
  #current position moves forward length plus skip siz
  cur_pos <- cur_pos + len + skip_size
  
  skip_size <- skip_size + 1
}







