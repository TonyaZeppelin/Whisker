rm(list=ls())
# setwd("/Users/brian.brost/Documents/sandbox/zeppelin/vibrissae")

# source("data.setup.R")
library(ggplot2)
library(tidyverse)
library(lubridate)
library(here)


# Isotope data
dat_proc <- read.csv(here('Data', 'derived', "dat_Brost.csv"))

dat <- dat_proc %>% filter(length_class=="long") %>% arrange(id,segment_percentile)
# head(dat)

# Locations where first derivative is zero - output from long_analysis.R
zeros <- read.csv("results/id_zeros_Brost.csv")
# head(zeros)

# Harvest dates
harvest <- read.csv("data/raw/all.csv")
# head(harvest)

harvest <- harvest %>% rename("collection_date"=collection.date) %>% select(id,collection_date) %>%
  distinct() %>% mutate(collection_date=as.Date(collection_date,format="%m/%d/%Y"))

###
### Estimate birth dates from pup vibrissae
###

# Figure - pup
dat %>% filter(age_class=="pup") %>%
  ggplot(aes(x=segment_percentile,y=d15N)) + 
  facet_wrap(~id) +
  geom_line() + 
  geom_point() +
  geom_vline(data=zeros %>% filter(isotope=="d15N",age_n==0) %>% group_by(id) %>% filter(fit==min(fit)),
             aes(xintercept=segment_percentile),col=2)
# ggsave(file="pup_N.pdf")

# Extract relevant information from data
dat_sum <- dat %>% filter(age_class=="pup",id!=13) %>% 
	group_by(id) %>% 
	summarize(max_percentile=max(segment_percentile),
			  whisker_length=max(segment_end))

# Add birth dip
dat_sum <- dat_sum %>% 
	left_join(
		zeros %>% filter(isotope=="d15N",age_n==0) %>% select(id,segment_percentile,fit) %>%
			group_by(id) %>% filter(fit==min(fit)),  # minimum fitted value is birth dip
		by="id")

# Add harvest date	
dat_sum <- dat_sum %>% left_join(harvest,by="id")

# Tidy data
dat_sum <- dat_sum %>% mutate(id=as.numeric(id)) %>% arrange(id) %>%
	rename(birth_percentile=segment_percentile)

# # Birth date assuming 90 days in utero growth
# diu <- 90  # assumed number of days whisker growth in utero 
# birth_90 <- dat_sum %>% 
# 	mutate(birth_date=collection_date-(1-birth_percentile)*(diu/birth_percentile)) %>%
# 	# mutate(birth_date=collection_date-(max_percentile-birth_percentile)*(diu/birth_percentile)) %>%
# 	mutate(age=collection_date-birth_date,
# 		   in_utero_growth_rate=birth_percentile*whisker_length/diu,
# 		   post_utero_growth_rate=(1-birth_percentile)*whisker_length/diu) %>%
# 	select(id,birth_percentile,collection_date,age,whisker_length,birth_date,
# 		   in_utero_growth_rate,post_utero_growth_rate) %>%
# 	mutate(across(where(is.double),\(x) round(x,2))) %>%
# 	mutate(age=round(age))	   
# 
# # Birth date assuming 120 days in utero growth
# diu <- 120  # assumed number of days whisker growth in utero 
# birth_120 <- dat_sum %>% 
# 	mutate(birth_date=collection_date-(1-birth_percentile)*(diu/birth_percentile)) %>%
# 	mutate(age=collection_date-birth_date,
# 		   in_utero_growth_rate=birth_percentile*whisker_length/diu,
# 		   post_utero_growth_rate=(1-birth_percentile)*whisker_length/diu) %>%
# 	select(id,birth_percentile,collection_date,age,whisker_length,birth_date,
# 		   in_utero_growth_rate,post_utero_growth_rate) %>%
# 	mutate(across(where(is.double),\(x) round(x,2))) %>%
# 	mutate(age=round(age))	 

#ajw addition June 2025
#assume mean birth date of July 9th(?)
diu <- 120  # assumed number of days whisker growth in utero 
birth_mean_120p <- dat_sum %>% 
  # mutate(birth_date=collection_date-(1-birth_percentile)*(diu/birth_percentile)) %>%
  mutate(birth_date = as.Date("2015-07-09")) %>%
  mutate(age=collection_date-birth_date,
         in_utero_growth_rate=birth_percentile*whisker_length/diu,
         # post_utero_growth_rate=(1-birth_percentile)*whisker_length/diu) 
        #why dividing by diu for post_utero?
        post_utero_growth_rate=(1-birth_percentile)*whisker_length/(as.numeric(age))) %>%
  
  select(id,birth_percentile,collection_date,age,whisker_length,birth_date,
         in_utero_growth_rate,post_utero_growth_rate) %>%
  mutate(across(where(is.double),\(x) round(x,2))) %>%
  mutate(age=round(age))

diu <- 90  # assumed number of days whisker growth in utero 
birth_mean_90p <- dat_sum %>% 
  # mutate(birth_date=collection_date-(1-birth_percentile)*(diu/birth_percentile)) %>%
  mutate(birth_date = as.Date("2015-07-09")) %>%
  mutate(age=collection_date-birth_date,
         in_utero_growth_rate=birth_percentile*whisker_length/diu,
         # post_utero_growth_rate=(1-birth_percentile)*whisker_length/diu
         #why dividing by diu for post_utero?
         post_utero_growth_rate=(1-birth_percentile)*whisker_length/(as.numeric(age))) %>% #birth to collection

  select(id,birth_percentile,collection_date,age,whisker_length,birth_date,
         in_utero_growth_rate,post_utero_growth_rate) %>%
  mutate(across(where(is.double),\(x) round(x,2))) %>%
  mutate(age=round(age))

# Write output
# write.csv(birth_90,"results/birth_dates_90.csv",row.names=FALSE)
# write.csv(birth_120,"results/birth_dates_120.csv",row.names=FALSE)

write.csv(birth_mean_90p,"results/birth_dates_mean_90p.csv",row.names=FALSE)
write.csv(birth_mean_120p,"results/birth_dates_mean_120p.csv",row.names=FALSE)

######age-2
ggplot(dat %>% filter(id %in% c('T17', 'T3', 'T5')),
                      aes(x=segment_percentile,y=d15N)) + 
  facet_wrap(~id) +
  geom_line() + 
  geom_point() +
  geom_vline(data=zeros %>% filter(isotope=="d15N",age_n==2 & segment_percentile < 0.3) %>% group_by(id) %>% filter(fit==min(fit)),
             aes(xintercept=segment_percentile),col=2)

# ggplot(dat %>% filter(id %in% c('T17', 'T3', 'T5')),
#        aes(x=segment_percentile,y=d13C)) + 
#   facet_wrap(~id) +
#   geom_line() + 
#   geom_point() +
#   geom_vline(data=zeros %>% filter(isotope=="d13C",age_n==2 & segment_percentile < 0.3) %>% group_by(id) %>% filter(fit==min(fit)),
#              aes(xintercept=segment_percentile),col=2)

dat_sum2 <- dat %>% filter(age_n=="2",id!=13) %>% 
  # filter(segment_percentile < 0.3) %>%
  group_by(id) %>% 
  summarize(max_percentile=max(segment_percentile),
            whisker_length=max(segment_end))

# Add birth dip
dat_sum2 <- dat_sum2 %>% 
  left_join(
    zeros %>% filter(isotope=="d15N",age_n==2) %>% select(id,segment_percentile,fit) %>%
      filter(segment_percentile < 0.3) %>%
      group_by(id) %>% filter(fit==min(fit)),  # minimum fitted value is birth dip
    by="id")

dat_wean <- dat_sum2 %>% 
  left_join(
    zeros %>% filter(isotope=="d15N",age_n==2) %>% select(id,segment_percentile,fit) %>%
      filter(segment_percentile < 0.52 & segment_percentile > 0.22) %>%
      group_by(id) %>% filter(fit==max(fit)),  # minimum fitted value is birth dip
    by="id")

# Add harvest date	
dat_sum2 <- dat_sum2 %>% left_join(harvest,by="id") %>% left_join(dat_wean, by = c('id', 'whisker_length'))

# Tidy data
dat_sum2 <- dat_sum2 %>% #mutate(id=as.numeric(id)) %>% arrange(id) %>%
  rename(birth_percentile=segment_percentile) #%>%
  # mutate(id=as.numeric(id))

#ajw addition June, November 2025
#assume mean birth date of July 9th
diu <- 120  # assumed number of days whisker growth in utero 
birth_mean_120 <- dat_sum2 %>% 
  # mutate(birth_date=collection_date-(1-birth_percentile)*(diu/birth_percentile)) %>%
  mutate(birth_date = as.Date("2012-07-09")) %>%
  #growth rates
  mutate(age=collection_date-birth_date,
         in_utero_growth_rate=birth_percentile*whisker_length/diu,
         # post_utero_growth_rate=(1-birth_percentile)*whisker_length/diu, #birth to collection
         #why dividing by diu for post_utero?
         post_utero_growth_rate=(1-birth_percentile)*whisker_length/(as.numeric(age))) %>% #birth to collection

  #growth rate from birth to wean?
  
  select(id,birth_percentile,collection_date,age,whisker_length,birth_date,
         in_utero_growth_rate,post_utero_growth_rate) %>%
  mutate(across(where(is.double),\(x) round(x,2))) %>%
  mutate(age=round(age))

diu <- 90  # assumed number of days whisker growth in utero 
birth_mean_90 <- dat_sum2 %>% 
  # mutate(birth_date=collection_date-(1-birth_percentile)*(diu/birth_percentile)) %>%
  mutate(birth_date = as.Date("2012-07-09")) %>%
  mutate(age=collection_date-birth_date,
         in_utero_growth_rate=birth_percentile*whisker_length/diu,
         post_utero_growth_rate=(1-birth_percentile)*whisker_length/(as.numeric(age))) %>%
  select(id,birth_percentile,collection_date,age,whisker_length,birth_date,
         in_utero_growth_rate,post_utero_growth_rate) %>%
  mutate(across(where(is.double),\(x) round(x,2))) %>%
  mutate(age=round(age))

write.csv(birth_mean_90,"results/birth_dates_mean_90j.csv",row.names=FALSE)
write.csv(birth_mean_120,"results/birth_dates_mean_120j.csv",row.names=FALSE)

# Old code...
# birth <- zeros %>% filter(isotope=="d15N",age_n==0,id!=13) %>% 
  # group_by(id) %>% filter(fit==min(fit)) %>%  # minimum fitted value is the birth dip
  # left_join(dat %>% group_by(id) %>% summarize(max_percentile=max(segment_percentile))) %>%  # add max segment percentile
  # left_join(harvest,by="id") %>%  # add collection dates
  # mutate(birth_date=collection_date-(max_percentile-segment_percentile)*(diu/segment_percentile))  
        
# birth %>%
  # ggplot(aes(x=birth_date)) +
  # geom_histogram(binwidth=7,fill="gray55") +
  # labs(x="Birth date", y="Number of individuals") + 
  # theme_bw() +
  # # theme(axis.text.x=element_text(angle=60, hjust=1)) +  # rotate labels
  # scale_x_date(date_breaks="weeks",date_labels="%m/%d")
# # ggsave(file="results/birth_dates.pdf",width=8,height=6)

# mean(birth$birth_date)
# sd(birth$birth_date)

# write.csv(birth %>% rename(birth_percentile=segment_percentile) %>% 
          # select(id,age_n,collection_date,isotope,fit,birth_percentile,max_percentile,birth_date),
          # "results/birth_dates.csv",row.names=FALSE)


###
### Estimate weaning date from 2-year-old vibrissae
###

# Figure
dat %>% filter(age==2) %>%
  ggplot(aes(x=segment_percentile,y=d15N)) + 
  facet_wrap(~id) +
  geom_line() + 
  geom_point() +
  geom_vline(data=zeros %>% filter(isotope=="d15N",age_n==2) %>% group_by(id) %>% filter(fit==max(fit)),
             aes(xintercept=segment_percentile),col=2)
# ggsave(file="age2_N.pdf")

wean <- zeros %>% filter(isotope=="d15N",age_n==2,id!="T17") %>% 
  group_by(id) %>% filter(fit==max(fit)) %>%  # maximum fitted value is the wean peak
  left_join(dat %>% group_by(id) %>% summarize(max_percentile=max(segment_percentile))) %>%  # add max segment percentile
  left_join(harvest,by="id") # add collection dates
  
# Add birth percentiles
zeros %>% filter(id%in%c("T3","T5"),isotope=="d15N") %>%
  ggplot(aes(x=segment_percentile,y=fit,color=id)) +
  geom_line() +
  geom_point()

wean$birth_percentile <- c(
  zeros %>% filter(id=="T5",isotope=="d15N") %>% slice(2) %>% pull(segment_percentile),
  zeros %>% filter(id=="T3",isotope=="d15N") %>% slice(1) %>% pull(segment_percentile)
)

dat %>% filter(age==2,id!="T17") %>%
  ggplot(aes(x=segment_percentile,y=d15N)) + 
  facet_wrap(~id) +
  geom_line() + 
  geom_point() +
  geom_vline(data=wean %>% select(id,segment_percentile),aes(xintercept=segment_percentile),col=2) +
  geom_vline(data=wean %>% select(id,birth_percentile),aes(xintercept=birth_percentile),col=4)
# ggsave(file="results/2yo.pdf",width=8,height=6)

wean <- wean %>% mutate(wean_date=collection_date-(collection_date-as.Date("2012-07-09"))*
                          ((max_percentile-segment_percentile)/(max_percentile-birth_percentile)))

# Devin's calculation in birth_wean.R
wean %>% mutate(wean_date=collection_date-(collection_date-as.Date("2012-07-09"))*
                          (max_percentile-birth_percentile))

write.csv(wean %>% rename(wean_percentile=segment_percentile) %>% 
            select(id,age_n,collection_date,isotope,fit,birth_percentile,wean_percentile,max_percentile,wean_date),
          "results/wean_dates.csv",row.names=FALSE)


###
### Devin's code
###

# source("data.setup.R")
# library(ggplot2)
# library(tidyverse)
# library(lubridate)
# 
# all3 <- all3 %>% arrange(id, midpoint.pctle)
# 
# 
# ggplot(data=all3 %>% arrange(id, midpoint.pctle) %>% filter(class=="pup")) + 
#   geom_path(aes(x=midpoint.pctle, y=d15N)) + 
#   geom_point(aes(x=midpoint.pctle, y=d15N)) + facet_wrap(~id)
# 
# # ggsave(file="pup_N.pdf")
# 
# 
# ggplot(data=all3 %>% arrange(id, midpoint.pctle) %>% filter(age==2)) + 
#   geom_path(aes(x=midpoint.pctle, y=d15N)) + 
#   geom_point(aes(x=midpoint.pctle, y=d15N)) + facet_wrap(~id)
# 
# # ggsave(file="age2_N.pdf")
# 
# 
# 
# #### Birth
# 
# diu <- 90 # whisker growth days in utero 
# pctle.fac <- 1 # multiplier for the midpoint.pctle value at the dip
# 
# 
# birth <- all3 %>% filter(class=="pup", !id==13) %>% group_by(id) %>% 
#   mutate(
#     minN = min(d15N, na.rm=T),
#     bd = d15N==minN
#   ) %>% summarize(
#     birth.pctle = max(midpoint.pctle*bd, na.rm=T),
#     max.pctle = max(midpoint.pctle),
#     collection.date = as.Date(first(collection.date))
#   ) %>% 
#   mutate(
#     birth.date = collection.date - (max.pctle-birth.pctle*pctle.fac)*diu/(birth.pctle*pctle.fac)
#   )
# 
# mean(birth$birth.date)
# sd(birth$birth.date)
# 
# 
# wean <- all3 %>% filter(age==2, !id=="T17") %>% group_by(id) %>% 
#   mutate(
#     maxN = max(d15N, na.rm=T),
#     wd = d15N==maxN
#   ) %>% summarize(
#     wean.pctle = max(midpoint.pctle*wd, na.rm=T),
#     max.pctle = max(midpoint.pctle, na.rm=T),
#     collection.date = as.Date(first(collection.date))
#   ) 
# 
# wean$birth.pctle = c(
#   all3 %>% filter(id=="T3") %>% slice(2) %>% pull(midpoint.pctle),
#   all3 %>% filter(id=="T5") %>% slice(4) %>% pull(midpoint.pctle)
# )
# wean <- mutate(wean,
#                birth.date = collection.date - (collection.date-as.Date("2012-07-09"))*(max.pctle-birth.pctle)
# )
# 
# 
# 
