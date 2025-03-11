#March 11 R User Group Meeting Presentation: Regular Expressions and DEEP Spill Incident Dataset

#Matt Hono, OPM DAPA


# Import library
library(tidyverse)


################################-
# Working with text ############
################################-

#Can save text as a object in R
text1 <- "Address: 450 Capitol Ave, Hartford, CT 06106"
text1

#Paste to concatenate text

paste("text1", "text2", "and", "text3")


####################################
# Detecting patterns ###############
####################################

#Detecting letters (R is case sensitive)
text1 <- "Address: 450 Capitol Ave, Hartford, CT 06106"

str_detect(string = text1, pattern = "Hartford")
str_detect(text1, "Hartford")
str_detect(text1, "hartford")
str_detect(text1, "Capitol")
str_detect(text1, "art")


#Detecting numbers 

str_detect(text1, "450")
str_detect(text1, "00")


#Detecting symbols or special characters

str_detect(text1, ":")
str_detect(text1, " ") #looks for a blank space 
str_detect(text1, ".") #looks for any character
str_detect(text1, "\\.") #looks for a literal period


#Detecting text using quantifiers: 

#? zero or one
#* zero or more
#+ one or more 
#{n} exactly n times
#{n,} n or more
#{n,m} between n and m

text2 <- "33333 ooo aaa kkkk bbbbb "

str_detect(text2, "33") #there are two sequential 3s in the string so this is TRUE
str_detect(text2, "3+") #there is more than one 3 so TRUE
str_detect(text2, "3{6}") #there is only 5 so FALSE
str_detect(text2, "o{2,4}")

#Detecting text using character classes via squared brackets []

text2 <- "33333 ooo aaa kkkk bbbbb "
str_detect(text2, "[0-9]") #looks for any number between 0 an 9
str_detect(text2, "[a-z]") #looks for any letter a and z (lower case)
str_detect(text2, "[A-Z]") #looks for any letter A and Z (upper case)
str_detect(text2, "[:digit:]") #looks for any digits: 0 to 9
str_detect(text2, "[:alpha:]") #looks for any letter: a to z and A to Z
str_detect(text2, "[:punct:]") #looks for any punctuation(: . , ;)
str_detect(text2, "[:alnum:]") #looks for any digits or alphabetic characters

#Detecting text using alternation | "or"

str_detect(text2, "w|o") #detects w or o
str_detect(text2, "oooo|aaaa") #detects oooo or aaaa


#Detecting text using anchors $ (dollar sign) ^ (caret)
#a$ end of string
#^a start of string

text3 <- "aa bb cc"
str_detect(text3, "a") #looks for an a
str_detect(text3, "a$") #looks for an a at the end of the text
str_detect(text3, "^Hartford") #looks for an a at the start of the text

####################################
# Counting matches #################
####################################

#Counting specific characters

str_count(string = "aaaa aaaaaa", pattern = "a") #there are ten instances of "a"

str_count("aaa,333,aaa", "aa") #only two instances of double a ("aa")


####################################
# Replacing values #################
####################################

#Replace the first pattern detected

str_replace(string = "This is a text", pattern = "text", replacement = "number")
str_replace(string = "5555", pattern = "5", replacement = "2") #replace first 5
str_replace(string = "5555", pattern = "5{2}", replacement = "2") #replace first 2 5s
str_replace(string = "5555 7777", pattern = "5+", replacement = "2") #replace all 5s
str_replace(string = "5555", pattern = "5$", replacement = "2") # replace the last 5
str_replace(string = "5555", pattern = "^5", replacement = "2") # replace the first 5


#Replace all patterns detected

str_replace_all(string = "5 one 55 two 555", pattern = "5", replacement = "2")

str_replace_all(string = "293-29-83, 22/33", pattern = "[[:punct:]]", replacement = " ")


#Changing upper-lower cases

str_to_lower("AAaaAA")
str_to_upper("AAaaAA")
str_to_title("aaaa bbbb ccc")
str_to_sentence("aaaa bbbb ccc")


####################################
# Removing values/characters #######
####################################

#Remove characters 

str_remove(string = "1 xxx 2", pattern = "x") #remove first match
str_remove_all(string = "1 xxx 2", pattern = "x") #remove all matches


#Remove blank spaces at the right, left, or both sides 

str_trim(string = "  aaa  ", side = "left")
str_trim(string = "  aaa  ", side = "right")
str_trim(string = "  aaa  ", side = "both")


####################################
# Extracting values ################
####################################

#Extracts the first complete match from a string

str_extract(string = "450 Capitol Ave, Hartford, CT 06106", pattern = "Hartford")
str_extract(string = "450 Capitol Ave, Hartford, CT 06106", pattern = "New Haven")

#Extracts the all matches

str_extract_all(string = "450 Capitol Ave, Hartford, CT 06106", "[0-9]")

################################################################################################
# Application Example: DEEP Dataset of Spill Incidents from January 1, 1996 to June 30, 2022####
################################################################################################

#Load in data
dfspills<-read.csv("https://data.ct.gov/resource/wr2a-rnsg.csv?$limit=100000000")

#Look at what towns have the most spills
table<-data.frame(table(dfspills$townrelease))

#Create indicator variable of if the spill occurred in an urban area or not using regular expressions
dfspills_2 <- dfspills %>%
  mutate(urban_area = ifelse(str_detect(str_to_upper(townrelease), 
                                        pattern = "BRIDGEPORT|NEW HAVEN|^HARTFORD|WATERBURY|STAMFORD"),1,0)) %>%
  select(year, townrelease, urban_area)

#Create a frequency table, including percents
table_urbanarea <- data.frame(table(dfspills_2$urban_area)) #use table function to create a frequency table
table_urbanarea$percent<- (table_urbanarea$Freq/ sum(table_urbanarea$Freq))*100

#Conclusion: 12 percent of the spills have occurred in these 5 urban towns

#Look at the most common causes
df<-data.frame(table(dfspills$causeinfo))

#Count how many spills were caused by MV Accidents

sum(str_count(str_to_upper(dfspills$causeinfo),"MV ACCIDENT"), na.rm = TRUE)

#Count how many spills involved oil

sum(str_count(str_to_upper(dfspills$releasesubstance), pattern ="OIL"), na.rm = TRUE)



