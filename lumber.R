library(tidyverse)
library(cowplot)
library(lubridate)
library(cansim)


#### Download lumber price index from Stats Can
ippi_lumber<-get_cansim("18-10-0265-01") %>%
  normalize_cansim_values %>%
  rename(category="North American Product Classification System (NAPCS)") %>%
  filter(category %in% c("Total, Industrial product price index (IPPI)","Lumber and other wood products [P41]")) %>%
  mutate(date=paste(REF_DATE,"01",sep="-")) %>%
  select(date,category,VALUE) %>%
  mutate(date=ymd(date),
         category=ifelse(category=="Total, Industrial product price index (IPPI)","Industrial\nProduct\nPrice Index",category),
         category=ifelse(category=="Lumber and other wood products [P41]","Lumber",category)) %>%
  filter(date>"2001-02-01")


##convert base year to 2001 March
ippi_base<-ippi_lumber %>%
  filter(date=="2001-03-01") %>%
  rename(base=VALUE)%>%
  select(category,base)

ippi_lumber<-ippi_lumber %>%
  left_join(ippi_base) %>%
  mutate(VALUE=100*VALUE/base) %>%
  select(-base)

price_plot<-ippi_lumber %>%
  ggplot(aes(date,VALUE)) +
  geom_line(aes(colour=category),size=1.3)+
  scale_x_date(expand=expansion())+
  theme_minimal_hgrid(14)+ 
  labs(title="Lumber Prices: 2001-2021",
       caption="Data: Statistics Canada Table 18-10-0265-01",
       x=NULL,
       y="Index (March 2001=100)")+
  theme(legend.position="none",
        plot.title = element_text(size=28))

price_labels<-ippi_lumber %>%
  filter(date==max(date)) %>%
  mutate(y=VALUE) %>%
  select(category,y)

price_label_axis<-axis_canvas(price_plot,axis="y")+
  geom_text(
    data=price_labels,
    aes(y=y, label=category, color=category),
    x=0.05,
    size=4.5,
    hjust=0
  )

p_price_labels<-insert_yaxis_grob(price_plot, price_label_axis)

ggdraw(p_price_labels)


###### GEt BC Lumber production data

#old series
old_lumber<-get_cansim("16-10-0045-01") %>%
  normalize_cansim_values %>%
  rename(old_series=VALUE,
         category="Standard Classification of Goods (SCG)") %>%
  filter(GEO=="British Columbia") %>%
  filter(category=="Total softwood, production [4407.10]") %>%
  mutate(date=paste(REF_DATE,"01",sep="-")) %>%
  select(date,old_series) %>%
  mutate(date=ymd(date))



new_lumber<-get_cansim("16-10-0017-01") %>%
  normalize_cansim_values %>%
  rename(new_series=VALUE,
         category="North American Product Classification System (NAPCS)") %>%
  filter(GEO=="British Columbia") %>%
  filter(category=="Total softwood, production [24112]") %>%
  mutate(date=paste(REF_DATE,"01",sep="-")) %>%
  select(date,new_series) %>%
  mutate(date=ymd(date))


lumber<- full_join(old_lumber,new_lumber) %>%
  pivot_longer(old_series:new_series, values_to="Softwood lumber production",names_to="Series",values_drop_na=TRUE)


lumber %>%
  mutate(production=`Softwood lumber production`/1000) %>%
  ggplot(aes(date,production))+
  geom_point(aes(color=Series),size=2)+
  geom_smooth(span=0.2,se=FALSE)+
  theme_minimal_hgrid(14)+ 
  labs(title="British Columbia softwood lumber production: Jan 2003 - Feb 2021",
       caption="Data: Statistics Canada Tables 16-10-0045-01 & 16-10-0017-01",
       x=NULL,
       y="thousands of cubic metres")+
  theme(legend.position=c(0.7, 0.9),
        legend.title=element_blank(),
        plot.title = element_text(size=24))


