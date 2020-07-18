
library(tidyverse)
library(bibliometrix)
library(countrycode)

######################################################
# PROCESS THE FILES WITH PACKAGE `bibliometrix`, 
# THEN USE BOTH METHODS TO EXTRACT '1ST AUTHOR COUNTRY'  
######################################################

# # Load WOS records  


articles_wos <- c('./data_raw/savedrecs1.txt',
                  './data_raw/savedrecs2.txt',
                  './data_raw/savedrecs3.txt',
                  './data_raw/savedrecs4.txt',
                  './data_raw/savedrecs5.txt',
                  './data_raw/savedrecs6.txt',
                  './data_raw/savedrecs7.txt',
                  './data_raw/savedrecs8.txt',
                  './data_raw/savedrecs9.txt',
                  './data_raw/savedrecs10.txt',
                  './data_raw/savedrecs11.txt',
                  './data_raw/savedrecs12.txt',
                  './data_raw/savedrecs13.txt',
                  './data_raw/savedrecs14.txt')

################################################################
# convert2df
################################################################
articles_wos_df <- convert2df(articles_wos, dbsource = "wos", format = "plaintext")


write_csv(articles_wos_df,"./output/articles_wos_bibliometrix.csv")

################################################################
# FIRST AUTHOR COUNTRY: METHOD 1
# metaTagExtraction
# p. 39: https://cran.r-project.org/web/packages/bibliometrix/bibliometrix.pdf
# Section 4.3: Aria, M. & Cuccurullo, C. (2017). bibliometrix: An R-tool 
# # for comprehensive science mapping analysis, Journal of Informetrics, 
# 11(4), pp 959-975, Elsevier, DOI: 10.1016/j.joi.2017.08.007
################################################################
# AU1_CO Corresponding Author’s Country (disambiguated)
country_method_1 <- metaTagExtraction(articles_wos_df, Field = "AU1_CO", sep = ";")
# add package column
country_method_1$package<-"bibliometrix"
country_method_1$AU1_CO
country_method_1$AuthorNum<-1

country_method_1<-as_tibble(country_method_1)
country_method_1<-country_method_1 %>% select(package,
                                            DI,TI,
                                            PY,SO,AU,
                                            AU_UN,AU1_CO)


country_method_1<-country_method_1 %>% distinct(TI,AU1_CO, .keep_all = TRUE)
write_csv(country_method_1,"./output/biblio_country_method_1.csv")

################################################################
# FIRST AUTHOR COUNTRY: METHOD 2
# biblioAnalysis
# Section 4.2 in Aria, M. & Cuccurullo, C. (2017). bibliometrix: An R-tool 
# # for comprehensive science mapping analysis, Journal of Informetrics, 
# 11(4), pp 959-975, Elsevier, DOI: 10.1016/j.joi.2017.08.007
################################################################
country_method_2<-biblioAnalysis(articles_wos_df, sep=";")
country_method_2<-as_tibble(country_method_2$CO) %>% rename("CO"="value")


################################################################
# bind the results from method 1 and method 2
WOS_bibliometrix<-bind_cols(country_method_1,country_method_2)
#to make it easier to compare titles, make them lower case
WOS_bibliometrix$TI<-tolower(WOS_bibliometrix$TI)

################################################################
# add 3 digit ISO codes for each country using package `countrycode`

# FIRST ADD FOR METHOD 1
WOS_bibliometrix$CO<-gsub("england","UK",WOS_bibliometrix$CO)
WOS_bibliometrix$CO<-gsub("north ireland","UK",WOS_bibliometrix$CO)
WOS_bibliometrix$CO<-gsub("papua n guinea","PNG",WOS_bibliometrix$CO)
WOS_bibliometrix$CO<-gsub("scotland","UK",WOS_bibliometrix$CO)
WOS_bibliometrix$CO<-gsub("wales","UK",WOS_bibliometrix$CO)
# add the 3-digit country code and convert to factor
WOS_bibliometrix$code_CO<-
  countrycode(WOS_bibliometrix$CO,"country.name", "iso3c", warn = TRUE)

# NOW ADD FOR METHOD 2
WOS_bibliometrix$AU1_CO<-gsub("england","UK",WOS_bibliometrix$AU1_CO)
WOS_bibliometrix$AU1_CO<-gsub("north ireland","UK",WOS_bibliometrix$AU1_CO)
WOS_bibliometrix$AU1_CO<-gsub("papua n guinea","PNG",WOS_bibliometrix$AU1_CO)
WOS_bibliometrix$AU1_CO<-gsub("scotland","UK",WOS_bibliometrix$AU1_CO)
WOS_bibliometrix$AU1_CO<-gsub("wales","UK",WOS_bibliometrix$AU1_CO)
# add the 3-digit country code and convert to factor
WOS_bibliometrix$code_AU1_CO<-
  countrycode(WOS_bibliometrix$AU1_CO,"country.name", "iso3c", warn = TRUE)

head(WOS_bibliometrix,10)

# clean up the environment
rm(country_method_2,
   country_method_1,
   articles_wos_df)





#############################################################################
# PROCESS THE FILES WITH PACKAGE `REFSPLITR`, 
# more on package and how to use here: https://github.com/ropensci/refsplitr
# (refsplitr extracts author country from address)
#############################################################################
# to install refsplitr: 
# library(devtools)
# devtools::install_github("ropensci/refsplitr")

# there is some cross-package incomaptibility, probably because of 
# something related to dependencies. refsplitr processing set up as a function
source("./WOS_refsplitr.R") 
# This will take the same WOS files processed with bibliometrix and 
# extract country affiliation for each author with `refsplitr`
# It can take ~5-10 min depnding on processor power 
WOS_refsplitr<-WOS_refsplitr()
WOS_refsplitr<-tibble(WOS_refsplitr)
# the refsplitr df to be compared with bibliometrix output 
head(WOS_refsplitr, 10)
######################################################

################################################################
# BIND THE DATASETS from BIBLIOMETRIX AND REFSPLITR
# select just the first authors in the refsplitr output
WOS_refsplitr_1<-WOS_refsplitr %>% filter(AuthorNum==1)
# the way the TI are saved is different, so edit the ones
# in refsplitr to make them consistent
WOS_refsplitr_1$TI<-gsub("\n"," ",WOS_refsplitr_1$TI)

nrow(WOS_bibliometrix)
nrow(WOS_refsplitr_1)

first_countries<-full_join(WOS_bibliometrix,WOS_refsplitr_1,by="DI","TI")

summary(first_countries$PY.x==first_countries$PY.y)
summary(first_countries$SO.x==first_countries$SO.y) 
summary(first_countries$TI.x==first_countries$TI.y) 

first_countries$TI_check<-(first_countries$TI.x==first_countries$TI.y) 
first_countries$PY_check<-(first_countries$PY.x==first_countries$PY.y) 
first_countries<-first_countries %>% select(refID,DI,
                                            TI.x,TI.y,TI_check,
                                            PY.x,PY.y,PY_check,
                                            SO=SO.x,
                                            AU,AU_UN,
                                            country_refsplitr,AU1_CO,CO, 
                                            code_refsplitr,code_AU1_CO,code_CO)


# these articles are not processed by bibliometrix so lets eliminate from df
first_countries<-first_countries %>% filter(!refID%in% c(212,3300,5453,5784,6536,952,1995,5528,1549))

# WOS has some problems with DOIs - there are cases where 2 articles have
# the same DOI. For simplicity, lets eliminate these from df
first_countries<-first_countries %>% filter(TI_check==TRUE)
# cleanup the environment

#####################################################################
# COMPARISON 1: FIRST AUTHORS REFSPLITR VS. BIBLIOMETRIX
#####################################################################
first_countries$country_refsplitr<-as.character(first_countries$country_refsplitr)
first_countries$country_v_AU1_CO<-first_countries$code_refsplitr==first_countries$code_AU1_CO
first_countries$country_v_CO<-first_countries$code_refsplitr==first_countries$code_CO
first_countries$AU1_CO_v_CO<-first_countries$code_AU1_CO==first_countries$code_CO
write_csv(first_countries,"./output/first_countries.csv")

# summary(first_countries$country_v_AU1_CO)
# summary(first_countries$country_v_CO)
# summary(first_countries$AU1_CO_v_CO)
# table of % matches between refsplitr and AU1_CO
first_countries_summary_country_v_AU1_CO<-as.data.frame(table(first_countries$country_v_AU1_CO))
first_countries_summary_country_v_AU1_CO$perc.false<-
   first_countries_summary_country_v_AU1_CO$Freq/
   sum(first_countries_summary_country_v_AU1_CO$Freq)*100
first_countries_summary_country_v_AU1_CO

# DOIs of papers where country not matching, with country names/codes from 
# each package, and also saved as a csv
papers2check<-first_countries %>% 
  filter(country_v_AU1_CO==FALSE) %>% 
  select(DI,country_refsplitr, AU1_CO,code_refsplitr,code_AU1_CO)

write_csv(papers2check,"./output/papers2check.csv")
#    
# rm(WOS_bibliometrix,
#    WOS_refsplitr)
