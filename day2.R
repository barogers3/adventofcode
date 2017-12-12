#day 2
#http://adventofcode.com/2017/day/2
#The spreadsheet consists of rows of apparently-random numbers. To make sure the recovery process is on the right track, they need you to calculate the spreadsheet's checksum. For each row, determine the difference between the largest value and the smallest value; the checksum is the sum of all of these differences.
#For example, given the following spreadsheet:
#  5 1 9 5
#7 5 3
#2 4 6 8
#The first row's largest and smallest values are 9 and 1, and their difference is 8.
#The second row's largest and smallest values are 7 and 3, and their difference is 4.
#The third row's difference is 6.
#In this example, the spreadsheet's checksum would be 8 + 4 + 6 = 18.

#What is the checksum for the spreadsheet in your puzzle input?

library(dplyr)

#used copy paste addin to paste as tribble
codeinput <- tribble(~V1,~V2,~V3,~V4,~V5,~V6,~V7,~V8,~V9,~V10,~V11,~V12,~V13,~V14,~V15,~V16,
   "121",   "59",  "141",   "21",  "120",   "67",   "58",   "49",   "22",   "46",   "56",  "112",   "53",  "111",  "104",  "130",
  "1926", "1910",  "760", "2055",   "28", "2242",  "146", "1485",  "163",  "976", "1842", "1982",  "137", "1387",  "162",  "789",
  "4088",  "258", "2060", "1014", "4420",  "177", "4159",  "194", "2794", "4673", "4092",  "681",  "174", "2924",  "170", "3548",
   "191",  "407",  "253",  "192",  "207",  "425",  "580",  "231",  "197",  "382",  "404",  "472",  "164",  "571",  "500",  "216",
  "4700", "1161",  "168", "5398", "5227", "5119",  "252", "2552", "4887", "5060", "1152", "3297",  "847", "4525",  "220",  "262",
  "2417",  "992", "1445",  "184",  "554", "2940",  "209", "2574", "2262", "1911", "2923",  "204", "2273", "2760",  "506",  "157",
   "644",  "155",  "638",   "78",  "385",  "408",  "152",  "360",  "588",  "618",  "313",  "126",  "172",  "220",  "217",  "161",
   "227", "1047",  "117",  "500", "1445",  "222",   "29",  "913",  "190",  "791",  "230", "1281", "1385",  "226",  "856", "1380",
   "436",   "46",  "141",  "545",  "122",   "86",  "283",  "124",  "249",  "511",  "347",  "502",  "168",  "468",  "117",   "94",
  "2949", "3286", "2492", "2145", "1615",  "159",  "663", "1158",  "154",  "939",  "166", "2867",  "141",  "324", "2862",  "641",
  "1394",  "151",   "90",  "548",  "767", "1572",  "150",  "913",  "141", "1646",  "154", "1351", "1506", "1510",  "707",  "400",
   "646",  "178", "1228", "1229",  "270",  "167",  "161", "1134",  "193", "1312", "1428",  "131", "1457",  "719", "1288",  "989",
  "1108", "1042",   "93",  "140",  "822",  "124", "1037", "1075",  "125",  "941", "1125",  "298",  "136",   "94",  "135",  "711",
   "112", "2429", "1987", "2129", "2557", "1827",  "477",  "100",   "78",  "634",  "352", "1637",  "588",   "77", "1624", "2500",
   "514",  "218",  "209",  "185",  "197",  "137",  "393",  "555",  "588",  "569",  "710",  "537",   "48",  "309",  "519",  "138",
  "1567", "3246", "4194",  "151", "3112",  "903", "1575",  "134",  "150", "4184", "3718", "4077",  "180", "4307", "4097", "1705"
  )

codeoutput <- codeinput %>%
  mutate_all(as.numeric) %>%
  rowwise() %>%
  mutate(largest = max(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16),
         smallest = min(V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,V11,V12,V13,V14,V15,V16),
         difference = largest - smallest)

sum(codeoutput$difference)


#part 2
#It sounds like the goal is to find the only two numbers in each row where one evenly divides the other - that is, where the result of the division operation is a whole number. They would like you to find those numbers on each line, divide them, and add up each line's result.
#For example, given the following spreadsheet:

#5 9 2 8
#9 4 7 3
#3 8 6 5
#In the first row, the only two numbers that evenly divide are 8 and 2; the result of this division is 4.
#In the second row, the two numbers are 9 and 3; the result is 3.
#In the third row, the result is 2.
#In this example, the sum of the results would be 4 + 3 + 2 = 9.
codeoutput2 <- codeinput %>%
  mutate_all(as.numeric)

row1 <- codeoutput2[1,]

library(purrr)


#fulldata
builddf <- data.frame()
fulldata <- function(inputval) {
  for (i in 1:length(inputval)) {
    rowinput <- inputval[i,]
    test <-map_df(map_df(map_df(t(rowinput),
                                function(x) rowinput %% x),
                         function(y) y==0),
                  function(z) rowinput * z)
  

    #get the row with 2 vectors
    test2<-map_df(map_df(map_df(t(rowinput),
                                function(x) rowinput %% x),
                         function(y) y==0),
                  function(z) sum(z) == 2) %>% match(TRUE)
    
    test3 <-as.data.frame(t(test * test2)) 
    
    test4<-test3[!is.na(test3)]
    test5<-test4[!(test4==0)]
    outp<-max(test5)/min(test5) 
    
    builddf<-rbind(builddf,
                   outp)
  }
return(builddf)
}

sum(fulldata(codeoutput2))

map_df(codeoutput2,fulldata)

