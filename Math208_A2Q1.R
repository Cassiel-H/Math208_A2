
#Math208 A2Q1
#Jiachen Huo



library(fivethirtyeight) 
library(tidyverse)
library(ggplot2)
library(ggmosaic)
library(knitr)

data(biopics)

## @knitr 1a)
biopics_tbl<-as_tibble(biopics)
#summary table
per_year<-biopics_tbl %>% group_by(year_release) %>% summarise(count=n())
#time interval
early<-per_year %>% summarise(min(year_release))
late<-per_year %>% summarise(max(year_release))
#plot
ggplot(per_year, aes(x=year_release,y=count))+
   geom_line()+
  labs(x="Year",y="count",title="Number of release by year")+
  scale_x_continuous(limits = c(as.numeric(early),as.numeric(late)),
                     breaks = seq(as.numeric(early),as.numeric(late),10))


## @knitr 1b)
ggplot(biopics_tbl, aes(x=year_release, fill=subject_sex ))+
  geom_bar()+scale_fill_viridis_d()+
  labs(x="Year",y="Number of person(s)",title="Sexual distribution by year")

## @knitr 1cTry
select(biopics_tbl, subject_race)

cutoff<-biopics_tbl%>%
  mutate(race=fct_lump(subject_race,1, other_level = "non-white")) 

cutoff

#standard all race 
ggplot(biopics_tbl, aes(x=year_release, fill=subject_race ))+
  geom_bar()+scale_fill_viridis_d()+
  labs(x="Year",y="Number of person(s)",title="Sexual distribution by year")

## @knitr 1c)
ggplot(biopics_tbl, aes(x=year_release, fill=fct_lump(subject_race,1, other_level = "non-white") ))+
  geom_bar()+
  labs(x="Year",y="Number of person(s)",title="Race distribution by year")+
  scale_fill_discrete(name = "race", labels = c("white", "non-white", "unknown"))

## @knitr 1d)
sex_race<- biopics_tbl%>%
  replace_na(list(subject_race="unknown"))%>% 
  mutate (subject_race=factor(ifelse(
   subject_race=="White"| subject_race=="unknown", subject_race, "non-white"))) 

ggplot(sex_race)+
  geom_mosaic(aes(x=product(subject_race, subject_sex),
                  fill=subject_sex))+
  labs(x="Sex of subject", y="Race of subject", title="Sex-Race distribution",fill="subject sex")
  


## @knitr 1e)
temp<-sex_race %>% group_by(year_release,subject_race,subject_sex)%>%
  select(year_release,subject_sex,subject_race)%>%
  summarise(count=n())%>%ungroup()%>%
  group_by(year_release)%>%
  mutate(prop=paste0(round(100*(count/sum(count)), 2), "%"))%>% 
  unite(race_sex, c(subject_race, subject_sex), remove=FALSE)%>%
  select(-c(subject_sex,subject_race))

kable(temp[1:20,])


## @knitr 1f)
temp<- sex_race  %>% 
  mutate(subject_sex=factor(if_else(sex_race$subject_sex=="Male",0,1)))%>% 
  group_by(year_release,subject_race,subject_sex)%>%
  select(year_release,subject_sex,subject_race)%>%
  summarise(count=n())%>% ungroup()%>%
  group_by(year_release)%>%
  mutate(prop=count/sum(count))

#count plot
ggplot(temp, aes(x=year_release,y=count, 
                 group=interaction(subject_sex,subject_race),
                 col=interaction(subject_sex,subject_race)))+
  geom_smooth(se=FALSE)+
  labs(x="Year",y="count",col=interaction("subject groups"),
       title="subject distribution among groups by count")

#proportion plot
ggplot(temp, aes(x=year_release,y=prop, 
                 group=interaction(subject_sex,subject_race),
                 col=interaction(subject_sex,subject_race)))+
  geom_smooth(se=FALSE)+
  labs(x="Year",y="count",col=interaction("subject groups"),
       title="subject distribution among groups by proportion")

