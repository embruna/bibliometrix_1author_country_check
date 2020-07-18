WOS_refsplitr<-function() {
  library(refsplitr)
# load the reference records
WOS_refs<-references_read(data = './data_raw', dir = TRUE, include_all=FALSE)
# You can save a csv of the output
write_csv(WOS_refs,"./output/WOS_refs.csv")
# WOS_refs<-read_csv("./output/WOS_refs.csv")
# Now extract the authors from each record and disambiguate author names
# NOTE: when you use this will throw a warning because there are "group authors"
# because it is in a function, I have commented it out and just made the changes
# WOS_authors<-authors_clean(WOS_refs)

# to correct these group authors do the following
WOS_refs$AU <- with( WOS_refs, ifelse( is.na(AU), CA, AU))
WOS_refs$AF <- with( WOS_refs, ifelse( is.na(AF), CA, AF))
# should no be able to process authors without problems
WOS_authors<-authors_clean(WOS_refs)
# Save the preliminary disambiguation as a csv file for manual review
# write_csv(WOS_authors$prelim,"./output/WOS_authors_prelim.csv")
# save the names suggested for review as a csv file
# write_csv(WOS_authors$review,"./output/WOS_authors_review.csv")
######################
# Accept the disambiguation (or load/merge your corrections, see documentation)
WOS_refined <- authors_refine(WOS_authors$review,WOS_authors$prelim)
######################
# Prepare the file for comparison with output from bibliometrix
# only keep necessary columns
WOS_refined_slim <- WOS_refined %>% select(author_name,author_order,
                                           country,RP_address,refID,PY)

# The refined file doesn't have the DOIs, titles, etc., so need to extract
# this from the original reference file and merge by refID number 
WOS_slim <- WOS_refs %>% select(DI,TI,PY,SO,refID)
# WOS_refined_slim<-as.data.frame(WOS_refined_slim)
# WOS_slim<-as.data.frame(WOS_slim)
# merge the two into a single df
WOS_refsplitr<-left_join(WOS_refined_slim,WOS_slim,by=c("refID","PY")) %>%
  arrange(refID,author_order)
# add the name of the database from which these data were extracted
WOS_refsplitr$package<-"refsplitr"

#add 3 digit ISO codes for each country using package `countrycode`
# this makes comparison easier becuase it accounts for different ways 
# scopus and wos report names of countries
# first need to change a few of them in the original data so countrycode
# can properly process them
WOS_refsplitr$country<-gsub("england","UK",WOS_refsplitr$country)
WOS_refsplitr$country<-gsub("north ireland","UK",WOS_refsplitr$country)
WOS_refsplitr$country<-gsub("papua n guinea","PNG",WOS_refsplitr$country)
WOS_refsplitr$country<-gsub("scotland","UK",WOS_refsplitr$country)
WOS_refsplitr$country<-gsub("wales","UK",WOS_refsplitr$country)
# add the 3-digit country code and convert to factor
WOS_refsplitr$code_refsplitr<-
  countrycode(WOS_refsplitr$country,"country.name", "iso3c", warn = TRUE)

# rename and reorder columns
WOS_refsplitr<-WOS_refsplitr %>% select(refID, 
                                        package, 
                                        DI, 
                                        SO, 
                                        PY, 
                                        TI,
                                        AuthorNum = author_order, 
                                        country_refsplitr = country,
                                        code_refsplitr)

# set column data types
WOS_refsplitr$refID<-as.factor(WOS_refsplitr$refID)
WOS_refsplitr$package<-as.factor(WOS_refsplitr$package)
WOS_refsplitr$DI<-as.factor(WOS_refsplitr$DI)
WOS_refsplitr$SO<-as.factor(WOS_refsplitr$SO)
WOS_refsplitr$PY<-as.factor(WOS_refsplitr$PY)
WOS_refsplitr$TI<-as.character(WOS_refsplitr$TI)
WOS_refsplitr$TI<-tolower(WOS_refsplitr$TI)
WOS_refsplitr$country_refsplitr<-as.factor(WOS_refsplitr$country_refsplitr)
WOS_refsplitr$code_refsplitr<-as.factor(WOS_refsplitr$code_refsplitr)
return(WOS_refsplitr)

}