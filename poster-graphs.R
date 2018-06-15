library(plyr)
library(tidyverse)
library(purrr)
library(magrittr)
library(ggplot2)

original <- read_csv('cms-procedures.csv')
names(original)



#remove unnecessary columns

df <- select(original, HCPCS.Code, HCPCS.Description, 
             HCPCS.Drug.Indicator, Place.of.Service, Number.of.Services, Number.of.Unique.Beneficiary.Provider.Interactions, 
             Number.of.Distinct.Medicare.Beneficiary.Per.Day.Services, year)
names(df)

b<-df
#b will be RWD


#extend B with codes that are also in A-literature

map<-read_csv('../mesh-cpt-map.csv')
names(map)
names(b)

b2<-b%>% left_join(map,by=c('HCPCS.Code'='CODE.y'))
names(b2)

#now b2 has some mesh in column SDUI.X   
b2



#skip to end




#get lkup table for HCPSC code from B data
names(b)
lkup_hcpcs <- b %>% select(1,2) %>% distinct()
#these two are not there
lkup_hcpcs %>% filter(HCPCS.Code=='97811')
lkup_hcpcs %>% filter(HCPCS.Code=='97810')

#this one is there
lkup_hcpcs %>% filter(HCPCS.Code=='82805')



fname='c:/temp/concept.rds'
concept<-read_rds(fname)
lkup_hcpcs<-concept %>% filter(vocabulary_id=='CPT4' | vocabulary_id == 'HCPCS')

concept %>% count(vocabulary_id) %>% View()
lkup_hcpcs %>% count(vocabulary_id) %>% View()
lkup_hcpcs %>% filter(concept_code=='97811')
lkup_hcpcs %>% write_csv('lkup_hcpcs.csv')
#do not upload this file to inet, due to AMA copyright







#literature
a<-read_csv('A-literature.csv')

names(a2)
a2<-a %>% rename(code=descriptorname_ui,source=value) %>% select(-ui)

names(b3)
b3<- b2 %>% rename(code=SDUI.x,count=Number.of.Unique.Beneficiary.Provider.Interactions,source=Place.of.Service) %>% 
  select(code,count,year,name=HCPCS.Description,name2=STR.x,source) %>% distinct()





#tidy data
# mesh_id,year,lit,number
# 
# D1651,2012,lit,7
# d1651,2013,lit,20
# D1651,2015,world,4000


#DONE ON JUNE 8th
#clean b dataset



ab<-a2 %>% bind_rows(b3)

#ab %>% count(code,HCPCS.Code) %>% View()

#find items in both a and b
#all A itmes are
bitems<-b3 %>% select(code) %>% distinct()
aitems<-a2 %>% select(code) %>% distinct()

#get items in BOTH A and B
abitems<-bitems %>% inner_join(aitems)

#restrict ab to only items in both
ab2<-ab %>% inner_join(abitems)


#ab2 is super nice object
ab2 %>% write_csv('ab-proc-combined.csv')



#plot

ab2 %>% count(code)
#172 items to plot

#pericardio subset

peri <- a2 %>% filter(code=='D020519')


#stacked graph

g<- ggplot(peri, aes(x=year, y=count, fill=source)) + geom_bar(stat="identity") +
  labs(title="Pericardiocentesis", x="Year", y="Number of articles", caption="Source: PubMed") +
  scale_fill_discrete(name= "MeSH Qualifier") +
  theme(legend.position = "left") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g


#seperated line graphs lines

peri2 <- ab2 %>% filter(code=='D020519')

peri2$source<-mapvalues(peri2$source, from = c("F","O"), to = c("inpatient procedure", "outpatient procedure"))

peri2 <- peri2[-c(127, 128, 129, 130, 133, 134, 137, 138, 139, 140, 143, 144, 145, 146),]

l<-ggplot(peri2, aes(x=year, y=count, color=source)) + geom_point() + geom_line() + facet_wrap(~source,scales = 'free_y') +
  labs(title="Pericardiocentesis", x="Year", y="Count", caption="Source: PubMed, data.cms.gov") +
  scale_fill_discrete(name= "Source") +
  theme(legend.position = "left") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(strip.text=element_text(size=6))

l
