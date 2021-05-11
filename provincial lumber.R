###### GEt Provincial Lumber production data

#old series
old_lumber<-get_cansim("16-10-0045-01") %>%
  normalize_cansim_values %>%
  rename(old_series=VALUE,
         category="Standard Classification of Goods (SCG)") %>%
  filter(GEO!="Canada") %>%
  filter(category=="Total softwood, production [4407.10]") %>%
  mutate(date=paste(REF_DATE,"01",sep="-")) %>%
  select(date,GEO,old_series) %>%
  mutate(date=ymd(date))



new_lumber<-get_cansim("16-10-0017-01") %>%
  normalize_cansim_values %>%
  rename(new_series=VALUE,
         category="North American Product Classification System (NAPCS)") %>%
  filter(GEO!="Canada") %>%
  filter(category=="Total softwood, production [24112]") %>%
  mutate(date=paste(REF_DATE,"01",sep="-")) %>%
  select(date,GEO,new_series) %>%
  mutate(date=ymd(date))


provs<-c("Alberta","British Columbia","Quebec", "Ontario","Nova Scotia","New Brunswick")

lumber<- full_join(old_lumber,new_lumber) %>%
  pivot_longer(old_series:new_series, values_to="Softwood lumber production",names_to="Series",values_drop_na=TRUE)


lumber %>%
  mutate(production=`Softwood lumber production`/1000) %>%
  filter(GEO %in% provs) %>%
  ggplot(aes(date,production))+
  geom_point(aes(color=Series),size=2)+
  geom_smooth(span=0.2,se=FALSE)+
  facet_wrap(~GEO,scales="free_y")+
  theme_minimal_hgrid(14)+ 
  labs(title="Softwood lumber production: Jan 2003 - Feb 2021",
       caption="Data: Statistics Canada Tables 16-10-0045-01 & 16-10-0017-01\nNote: Ontario & New Brunswick only have data to Jan 2021",
       x=NULL,
       y="thousands of cubic metres")+
  theme(legend.position="bottom",
        legend.title=element_blank(),
        plot.title = element_text(size=24))

lumber %>% filter(date==max(date)) %>% view()
