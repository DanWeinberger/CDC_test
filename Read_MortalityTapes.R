#library(feather)
library(data.table)
library(readr)
library(pbapply)
library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)

#the geographic resolution missing from the public data


#https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm
# file.names1<- list('VS14MORT.DUSMCPUB', 'VS15MORT.DUSMCPUB','VS16MORT.DUSMCPUB','VS17MORT.DUSMCPUB',
#              'Mort2018US.PubUse.txt','VS19MORT.DUSMCPUB_r20210304')
# 
# all.ds <- lapply(file.names1, function(x){
#   d1 <- read_fwf(file=paste0("./CDC_tapes/" ,x),
#                  fwf_positions(start=c(20,21,65,69,102,445,70,71, 79,484,146,167,174,181,188,195,202,209,216,223,230,237,244,251,258,265,272,279,286,293,300),
#                                end=c(20,34,66,69,105,446,  70,73, 80,486,149, 171,178,185,192,199,206,213,220,227,234,241,248,255,262,269,276,283,290,297,304),
#                                col_names = c('res_status','state','month','sex','year','race','age_detail_class','age_detail_number','agec','hispanic', paste0('icd', 1:21 ) )),
#                   guess_max=10000)
#   return(d1)
# })
# 
# df1 <- bind_rows(all.ds)
# saveRDS(df1, './CDC_tapes/compiled_data.rds')

df1 <- readRDS('./CDC_tapes/confidential/compiled_data_county.rds')

df1$hisp_recode <- 999
df1$hisp_recode[df1$hispanic<=199 & df1$hispanic>=100] <- 0
df1$hisp_recode[df1$hispanic >=200 & df1$hispanic <= 299] <- 1
#table(df1$hisp_recode)

# df1$race_ethnicity <- 999
# df1$race_ethnicity[df1$race %in% c('01') & df1$hisp_recode != 1] <- 1 #white, non-Hospanic
# df1$race_ethnicity[df1$race %in% c('02') & df1$hisp_recode != 1]  <- 2 #black, non-Hispanic
# df1$race_ethnicity[ df1$hisp_recode == 1]  <- 3 #Hispanic
# df1$race_ethnicity[ df1$race %in% c('04','05','18','28','48' , '06','07','38','58','68','78') & df1$hisp_recode != 1]  <- 4 #Asian
# df1$race_ethnicity[ df1$race %in% c('03') & df1$hisp_recode != 1]  <- 5 #American Indian
# #table(df1$race_ethnicity)

df1$race_recode <- 999
df1$race_recode[df1$race %in% c('01') ] <- 1 #white, non-Hospanic
df1$race_recode[df1$race %in% c('02') ]  <- 2 #black, non-Hispanic
df1$race_recode[ df1$race %in% c('03') ]  <- 3 #American Indian
df1$race_recode[ df1$race %in% c('04','05','18','28','48' ,'68','78')]  <- 4 #Asian
df1$race_recode[ df1$race %in% c( '06','07','38','58')]  <- 5 #Hawaain/Pac Is


df1$qtr <- NA
df1$qtr[df1$month %in% c('01','02','03')] <- 1
df1$qtr[df1$month %in% c('04','05','06')] <- 2
df1$qtr[df1$month %in% c('07','08','09')] <- 3
df1$qtr[df1$month %in% c('10','11','12')] <- 4


#table(df1$race_ethnicity)
  
#Create aggregate time series for analysis
agg1 <- df1 %>%
  bind_rows() %>% 
  group_by(year, qtr, sex, agec,race_recode) %>%
  summarize(N_deaths = n())  %>%
  ungroup  %>%
  complete(year,qtr, sex, agec,race_recode, fill=list(N_deaths=0)) #fills 0s

saveRDS(agg1,'./Data/Confidential/compiled_sex_age_race_qtr.rds')


## Cause specific deaths
df1$one <-1

rsv.codes <- c('B974','J121', "J210", 'J205') #Define codes for RSV

pneumococcal.codes <- c('A403','J13','B953','G001')

ld.codes <- c('A481')

icd.cols <- grep('icd',names(df1)) #Define columns with multiple cause of death stats

df.rsv <- pbapply(df1[,icd.cols],2, function(x) x %in% rsv.codes )

df.pneumo <- pbapply(df1[,icd.cols],2, function(x) x %in% pneumococcal.codes )

df.pneumo.sepsis <- pbapply(df1[,icd.cols],2, function(x) x %in% 'A403' )
df.pneumo.pneum <- pbapply(df1[,icd.cols],2, function(x) x %in% 'J13' )


df.leg <- pbapply(df1[,icd.cols],2, function(x) x %in% ld.codes )

  
df1$rsv <- rowSums(df.rsv) #how many RSV codes re there per row?
df1$rsv <- 1*(df1$rsv>0) #convert to binary

df1$pneumo <- rowSums(df.pneumo) #how many RSV codes re there per row?
df1$pneumo <- 1*(df1$pneumo>0) #convert to binary

df1$pneum.sepsis <- rowSums(df.pneumo.sepsis)
df1$pneum.sepsis <- 1*(df1$pneum.sepsis>0)

df1$pneum.pneu <- rowSums(df.pneumo.pneum)
df1$pneum.pneu <- 1*(df1$pneum.pneu>0)

df1$ld <- rowSums(df.leg) #how many RSV codes re there per row?
df1$ld <- 1*(df1$ld>0) #convert to binary


df1$infant <- 0
df1$infant[df1$age_detail_class==1 & df1$age_detail_class==1] <-1
df1$infant[df1$age_detail_class %in% c(4,5,6) ] <-1

df1$agey <- as.numeric(df1$age_detail_number)
#df1$date <- as.Date(paste(df1$year, df1$month, '01', sep='-'))



#looks at seasonality by cause
agg1.season <- df1 %>%
  group_by(year,month, agec) %>%
  summarize(N_deaths = n(), pneumo=sum(pneumo),pneum.pneu=sum(pneum.pneu),pneum.sepsis=sum(pneum.sepsis),  rsv=sum(rsv), ld=sum(ld)) %>%
  ungroup()
  
agg1.season$month <- as.numeric(agg1.season$month)



p1 <- ggplot(agg1.season, aes(x=month, y=pneumo, group=year, col=year)) +
  geom_line() +
  ylab("Number of pneumococcal deaths") +
  xlab("Date") +
  theme_classic() +
  theme(panel.spacing= unit(2,'lines') , axis.text.x=element_text(angle=90)) +
  geom_hline(yintercept=0, col='gray', lty=2) +
  facet_wrap(~ agec , scales='free') 
p1

p2 <- ggplot(agg1.season, aes(x=month, y=rsv, group=year, col=year)) +
  geom_line() +
  ylab("Number of RSV deaths") +
  xlab("Date") +
  theme_classic() +
  theme(panel.spacing= unit(2,'lines') , axis.text.x=element_text(angle=90)) +
  geom_hline(yintercept=0, col='gray', lty=2) +
  facet_wrap(~ agec , scales='free') 
p2


p3 <- ggplot(agg1.season, aes(x=month, y=ld, group=year, col=year)) +
  geom_line() +
  ylab("Number of LD deaths") +
  xlab("Date") +
  theme_classic() +
  theme(panel.spacing= unit(2,'lines') , axis.text.x=element_text(angle=90)) +
  geom_hline(yintercept=0, col='gray', lty=2) +
  facet_wrap(~ agec , scales='free') 
p3



agg1.season.allyr <- df1 %>%
  group_by(agec,month) %>%
  summarize(N_deaths = n(), pneumo=sum(pneumo), rsv=sum(rsv), ld=sum(ld)) %>%
  ungroup()

agg1.season.allyr$month <- as.numeric(agg1.season.allyr$month)

p4 <- ggplot(agg1.season.allyr, aes(x=month, y=ld, group=agec)) +
  geom_line() +
  ylab("Number of LD deaths") +
  xlab("Date") +
  theme_classic() +
  geom_hline(yintercept=0, col='gray', lty=2) +
  theme(panel.spacing= unit(2,'lines') , axis.text.x=element_text(angle=90)) +
  facet_wrap(~ agec , scales='free') 
p4

#Aggregate databy year, month, sex, age
agg1 <- all.ds %>%
  bind_rows() %>% 
  group_by(month, year, sex, agec) %>%
  summarize(N_deaths = n())

agg1$date <-    as.Date(paste(agg1$year, agg1$month,'01',sep='-'))

agg1 <- agg1[order(agg1$agec, agg1$sex, agg1$date),] 



ave.age.ldf <-  df1[df1$ld==1,] %>%
  group_by(month) %>%
  summarize(ave.age  = mean(agey) )

## Broader age groups

df1$agec2 <- NA
df1$agec2[df1$agey<5 & df1$agey >=0] <- 1
df1$agec2[df1$agey<18 & df1$agey >=5] <- 2
df1$agec2[df1$agey<40 & df1$agey >=18] <- 3
df1$agec2[df1$agey<64 & df1$agey >=40] <- 4
df1$agec2[df1$agey<80 & df1$agey >=65] <- 5
df1$agec2[df1$agey<120 & df1$agey >=80] <- 6


#looks at seasonality by cause
agg1.season <- df1 %>%
  group_by(year,month, agec2) %>%
  summarize(N_deaths = n(), pneumo=sum(pneumo),pneum.pneu=sum(pneum.pneu),pneum.sepsis=sum(pneum.sepsis),  rsv=sum(rsv), ld=sum(ld)) %>%
  ungroup()

agg1.season$month <- as.numeric(agg1.season$month)

agg1.season <- agg1.season[agg1.season$agec2 %in% c(1,2,3,4,5,6),]

agg1.season$year <- as.factor(agg1.season$year)

cols.plot <- c(rep('gray',6), 'red')

p1 <- ggplot(agg1.season, aes(x=month, y=pneumo, group=year , color=year)) +
  geom_line() +
  scale_color_manual(values=cols.plot) + 
  ylab("Number of pneumococcal deaths") +
  xlab("Date") +
  theme_classic() +
  theme(panel.spacing= unit(2,'lines') , axis.text.x=element_text(angle=90)) +
  #geom_hline(yintercept=0, col='gray', lty=2) +
  facet_wrap(~ agec2 , scales='free') 
p1

p1b <- ggplot(agg1.season, aes(x=month, y=pneum.pneu, group=year , color=year)) +
  geom_line() +
  scale_color_manual(values=cols.plot) + 
  ylab("Number of pneumococcal pneumonia deaths") +
  xlab("Date") +
  theme_classic() +
  theme(panel.spacing= unit(2,'lines') , axis.text.x=element_text(angle=90)) +
  #geom_hline(yintercept=0, col='gray', lty=2) +
  facet_wrap(~ agec2 , scales='free') 
p1b

p1c <- ggplot(agg1.season, aes(x=month, y=pneum.sepsis, group=year , color=year)) +
  geom_line() +
  scale_color_manual(values=cols.plot) + 
  ylab("Number of pneumococcal sepsis deaths") +
  xlab("Date") +
  theme_classic() +
  theme(panel.spacing= unit(2,'lines') , axis.text.x=element_text(angle=90)) +
  #geom_hline(yintercept=0, col='gray', lty=2) +
  facet_wrap(~ agec2 , scales='free') 
p1c

p2 <- ggplot(agg1.season, aes(x=month, y=rsv, group=year, col=year)) +
  geom_line() +
  scale_color_manual(values=cols.plot) + 
  
  ylab("Number of RSV deaths") +
  xlab("Date") +
  theme_classic() +
  theme(panel.spacing= unit(2,'lines') , axis.text.x=element_text(angle=90)) +
  #geom_hline(yintercept=0, col='gray', lty=2) +
  facet_wrap(~ agec2 , scales='free') 
p2


#Age dist of RSV in first year of life
rsv.age.dist <- df1[df1$rsv==1,]
hist(rsv.age.dist$agey)
table(rsv.age.dist$age_detail_class)

rsv.age.dist$detail.age <- NA
rsv.age.dist$detail.age[rsv.age.dist$age_detail_class==1] <- as.numeric(rsv.age.dist$age_detail_number[rsv.age.dist$age_detail_class==1])
rsv.age.dist$detail.age[rsv.age.dist$age_detail_class==2] <- as.numeric(rsv.age.dist$age_detail_number[rsv.age.dist$age_detail_class==2])/12
rsv.age.dist$detail.age[rsv.age.dist$age_detail_class==4] <- as.numeric(rsv.age.dist$age_detail_number[rsv.age.dist$age_detail_class==4])/365
hist(rsv.age.dist$detail.age)
hist(rsv.age.dist$detail.age[rsv.age.dist$detail.age<12])

# Agec
# 01 ... Under 1 year (includes not stated infant ages)
# 02 ... 1 - 4 years
# 03 ... 5 - 14 years
# 04 ... 15 - 24 years
# 05 ... 25 - 34 years
# 06 ... 35 - 44 years
# 07 ... 45 - 54 years
# 08 ... 55 - 64 years
# 09 ... 65 - 74 years
# 10 ... 75 - 84 years
# 11 ... 85 years and over
# 12 ... Age not stated


#Race
# 01 ... White
# 02 ... Black
# 03 ... American Indian (includes Aleuts and Eskimos)
# 04 ... Chinese
# 05 ... Japanese
# 06 ... Hawaiian (includes Part-Hawaiian)
# 07 ... Filipino
# 18 ... Asian Indian
# 28 ... Korean
# 38 ... Samoan
# 48 ... Vietnamese
# 58 ... Guamanian
# 68 ... Other Asian or Pacific Islander in areas reporting codes 18-58
# 78 ... Combined other Asian or Pacific Islander, includes codes 18-68
# for areas that do not report them separately


#Hispanic:
# 484-486 3 Hispanic Origin
# 100-199 ... Non – Hispanic
# 200-209 … Spaniard
# 210-219 ... Mexican
# 260-269 ... Puerto Rican
# 270-274 ... Cuban
# 275-279 … Dominican
# 220 … Central and South American
# 221-230 ... Central American
# 231-249 … South American
# 250-259 … Latin American
# 280-299 ... Other Hispanic
# 996-999 ... Unknown