##Jennifer Otiono



## IMPORT DATASETS
library(readr)
library(ggplot2)

charts <- read_csv("charts.csv", col_types = cols(date = col_date(format = "%Y-%m-%d"), 
                  rank = col_integer(), `last-week` = col_integer(), 
                  `peak-rank` = col_integer(), `weeks-on-board` = col_integer()))
View(charts)

library(readr)
GenderDataset <- read_csv("data/Gender_Dataset_2.3.csv", 
                          col_types = cols(GENDER = col_character()))
View(GenderDataset)

chart_durations <- read_csv("data/2024-05-10-chart_durations.csv",
col_types = cols(num_songs = col_integer(),
num_weeks_on_charts = col_integer(),
earliest_date = col_date(format = "%Y-%m-%d")))



###handle the multiple artist collaborations
# note: library(stringr) ## contains functions for manipulating text data

# ### handle genders
library(dplyr) ## mutate helps you construct new variables

GenderDataset = mutate(GenderDataset,gender2 = case_when(PRONOUN=="he/him"~"male", PRONOUN =="she/her"~"female",
                                        PRONOUN =="they/them"~"nonbinary"))

View(GenderDataset)



##MERGE DATATSETS

chart_artists = left_join(
  chart_durations,
  GenderDataset,
  by = c("artist" = "NAME")
)
#note: there were implication for using left-join on the count of artists.
#chart_durations contains artists that were on the billboard but GenderDataset contains artists that exceeds that.
# I noted that total in one dataset was 6659 however the total count from merged dataset
# exceeded that which made me wonder: where are these extra values or artists coming from?
# I found one evidence that a single artist can show up more than once in GenderDataset
# with left join, when it finds a match between chart-duration and GenderDataset it keeps those multiple instances from GenderDataset into the merged dataset.
#for example, "LadyA, Latto, Drake show up twice in chart-artists and Lisa shows up three times"

View(chart_artists)

count(chart_durations, artist)
#count: 6,669

View (count(chart_durations, artist))
# i do this to check if each artist is distinct. the number of rows is the number of distinct artist.

count(GenderDataset,NAME)
View(count(GenderDataset,NAME)) 

count(chart_artists, artist)
#the chart_artist is limited by the limiting factor (gender). count:6,669


View(count(chart_artists, artist))
count(chart_durations)
#found an artist "12 Gauge" appears twice in the GenderDataset. so does: $peedyyy, $way, 10 The Chemist,  $wish (appears 3 times) - "Drake aappears twice here too"




#JNM additions

## binary value for whether the row was merged or not
chart_artists$merged <- is.na(chart_artists$CHARTMETRIC_ID)!=TRUE


## count the number of merged artists
summary(chart_artists$merged)

## create a dataframe that has only merged artists 
## (note: there are enough non merged artists that this could affect the results)
chart_artists_merged <- subset(chart_artists, merged==TRUE)



ggplot(chart_artists_merged, aes(log(num_weeks_on_charts), gender2)) +
  geom_boxplot()

ggplot(chart_artists_merged, aes(log(num_weeks_on_charts), gender2)) +
  geom_violin()


head(chart_artists_merged)
nrow(chart_artists_merged)


#summary statistics
summary(chart_artists)



# group by gender and average number of weeks
chart_artists%>%group_by(gender2)%>%summarise(avg_weeks = mean(num_weeks_on_charts))

genderbyavgweeks = chart_artists%>%group_by(gender2)%>%summarise(avg_weeks = mean(num_weeks_on_charts)) # to make it its own dataset


# %>% this syntax is called 'pipes' and it just means whatever is one the left will become the first argument
# of what is one the right.

#mutate() does some calculation for every row without collapsing it - do if you care about the
#individual observations in the dataset

#summarise() does calculation but collapses the dataset


#graphing/plotting
#boxplot(chart_artists$num_songs)



#t-test to determine if female vs male are statistically different from eachother.
# i'm going to do a t-test first because the questions explicitly states women vs men.
# because this questionasks for whether two differ from eachother vs a known value,
#this requires a two sample t-test

genderbyavgweeks =  chart_artists_merged%>% filter(gender2 == 'female' | gender2 == 'male') # to exclude the nonbinary category


t.test(log(num_weeks_on_charts)~ gender2, data =genderbyavgweeks)

# i was using avg_weeks instead of num_weeks_on....t.test wants to look at everything
#note: this data is skewed so we do log of on num-weeks for which the t.test will do better
#the p is 0.608 which greater than 0.05 meaning we fail to reject that there is a significant difference in avg weeks on billboard by gender


boxplot(log(num_weeks_on_charts)~ gender2, data =genderbyavgweeks)



#ANOVA

for_anova = lm(log(num_weeks_on_charts)~ gender2, data =chart_artists_merged)
#note:intercept 3.03 is the female so -0.025 gender2male is the difference between the intercept (female) and the male gender
#note: lm: sets the data up

anova(for_anova)

#note: if F > 1 that would suggest difference; if F< 1 nothing exciting hehe

#break it down more ? emmeans
#install.packages("emmeans",type = "binary")
library(emmeans)

#emmeans(for_anova, ~gender2)
emmeans(for_anova, pairwise~gender2) #gives extra set of t tests comparing each pair of means

#tukey method - across this set of 3, we might find something to be significant. keep it on. to control for multiple testing.




#-old concerns 
#note: the NA in dataset might be causing issues.
#note: help!! how to make this process neater? and is this even correct? (i cant tell because i dont 
#see labeling on graphs when i checked so i think i did it incorrectly. :/)
#anova to determine if there is difference between male, female, and nonbinary (three or more groups)
