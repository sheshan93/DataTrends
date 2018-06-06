library(tidyverse)
library(purrr)
library(magrittr)
library(ggplot2)

##IMPORTING ORIGINAL DATASET AND CREATING SUBSET

original <- read_csv('cms-procedures.csv')
names(original)

df6<-original %>% filter(Place.of.Service=='F') %>% select(HCPCS.Code,year,cnt=Number.of.Unique.Beneficiary.Provider.Interactions) %>% 
  spread(year,cnt)

#calculating the number of unique interactions for year 2015 and divided by year 2012.
df6 %<>% mutate(jump=`2015`/`2012`)

#calculating the percent increase for each interaction (where % of change= amount of change/original amount * 100)
df6 %<>% mutate(increase=(((`2015`-`2012`)/`2012`)*100)) 

#creation of lookup file
lkup<-original %>% select(HCPCS.Code,HCPCS.Description) %>% distinct()
lkup %>% count(HCPCS.Code) %>% count(n)

#joining the spread of years to the lookup file to add a name to the codes
df6 %<>% left_join(lkup)


#creation of df where % increase is greater than 100 
df7 = df6 %>% filter(`increase` > 100)

#gather columns so that we can create graphs over the years
df8 <- gather(df7, "year", "count", 2:5) 

#create csv of df8
write.csv(df8, "rwd-procs-subset.csv")




