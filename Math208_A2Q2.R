
#Math208 A2Q2
#Jiachen Huo

library(heplots)
library(tidyverse)
library(ggplot2)
data(Diabetes)

?Diabetes

## @knitr 2a)
diabetes_tbl<-as_tibble(Diabetes)

summary_tbl<- diabetes_tbl %>% group_by(group)%>%
  select(group,relwt,glufast,glutest,instest,sspg)%>%
  summarise_all(list(Avg=mean,Med=median))%>%
  pivot_longer(cols=contains("_"),names_to="Measure")%>%
  pivot_wider(id_cols = Measure,names_from=group)

summary_tbl

## @knitr 2b)
ggplot(diabetes_tbl, aes(x=instest,y=glutest,col=group))+
  facet_wrap(~group)+
  geom_point()+geom_smooth(method="lm",col="black")+
  labs(x="insulin test",y="glucose test",title ="Glutest V.S. Instest")+
  theme(legend.position = "none")

ggplot(diabetes_tbl, aes(x=instest,y=sspg,col=group))+
  facet_wrap(~group)+
  geom_point()+geom_smooth(method="lm",col="black")+
  labs(x="insulin test",y="sspg test",title ="sspg V.S. Instest")+
  theme(legend.position = "none")

ggplot(diabetes_tbl, aes(x=sspg,y=glutest,col=group))+
  facet_wrap(~group)+
  geom_point()+geom_smooth(method="lm",col="black")+
  labs(x="sspg test",y="glucose test",title ="Glutest V.S. sspg")+
  theme(legend.position = "none")

## @knitr 2c)
ggplot(diabetes_tbl,aes(x=sspg,y=instest,group=group,fill=group))+
  geom_bin2d()+facet_wrap(~group)+
  labs(title="2D histogram of Instest V.S. sspg")

ggplot(diabetes_tbl,aes(x=sspg,y=instest))+
  stat_density_2d(col="red",bins=30)+facet_wrap(~group)+
  xlim(c(-50,600))+ylim(c(-50,800))+
  labs(title="2D contour plot of Instest V.S. sspg")


