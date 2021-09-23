#library(feather)
library(data.table)
library(readr)
library(dplyr)
library(reshape2)
library(tidyr)

#the geographic resolution missing from the public data

#https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm
file.names1<- list('VS14MORT.DUSMCPUB', 'VS15MORT.DUSMCPUB','VS16MORT.DUSMCPUB',
             'Mort2018US.PubUse.txt','VS19MORT.DUSMCPUB_r20210304')

all.ds <- lapply(file.names1, function(x){
  d1 <- read_fwf(file=paste0("./CDC_tapes/" ,x),
                 fwf_positions(start=c(20,21,65,69,102,445,77,484,146),
                               end=c(20,34,66,69,105,446,78,486,149),
                               col_names = c('res_status','state','month','sex','year','race','agec','hispanic','icd1')),
                  guess_max=10000)
  return(d1)
})

df1 <- bind_rows(all.ds)
saveRDS(df1, './CDC_tapes/compiled_data.rds')
df1$one <-1

#Tidyverse version
agg1 <- all.ds %>%
  bind_rows() %>% 
  mutate(one=1) %>%
  reshape2::dcast( month+year+sex+agec ~ ., fun.aggregate = length, value.var='one')  %>%
  mutate(date= as.Date(paste(year, month,'01',sep='-') ))
       
agg1 <- agg1[order(agg1$agec, agg1$sex, agg1$date),] 


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