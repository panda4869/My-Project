install.packages("XBRL")

install.packages("Rcpp")
## Setting stringsAsFactors = FALSE is highly recommended
## to avoid data frames to create factors from character vectors.
options(stringsAsFactors = FALSE)
## Load the library
library(XBRL)
library(Rcpp)
## XBRL instance file to be analyzed, accessed
## directly from SEC website:
inst <- "http://www.sec.gov/Archives/edgar/data/40554/000004054515000032/gecc-20141231.xml"
xbrl <- XBRL()
xbrl$setCacheDir("XBRLcache")
xbrl$openInstance(inst)
## Perform a discovery of the taxonomy:
xbrl$processSchema(xbrl$getSchemaName())
## Process instance file:
xbrl$processContexts()
xbrl$processUnits()
xbrl$processFacts()
xbrl$processFootnotes()
xbrl$closeInstance()
xbrl.vars <- xbrl$getResults()
summary(xbrl.vars )
str(xbrl.vars)
View(xbrl.vars)

xbrl.vars$element
View(xbrl.vars$context)
View(xbrl.vars$element)
##
install.packages("knitr")
library(knitr)


kable(xbrl.vars$element, format = "markdown")

install.packages("dplyr")
library(dplyr)


#how many kinds #
xbrl.vars$role %>%
  group_by(type) %>%
  summarize(count=n()) 
#Disclosure#
xbrl.disclosure<-xbrl.vars$role %>%
  filter(type == "Disclosure") %>%
  select(roleId, definition) 
xbrl.disclosure
agrep("risk",xbrl.disclosure)
#Document#
xbrl.disclosure<-xbrl.vars$role %>%
  filter(type == "Document") %>%
  select(roleId, definition) 
xbrl.disclosure
agrep("risk",xbrl.disclosure)
#Statement#
xbrl.disclosure<-xbrl.vars$role %>%
  filter(type == "Statement") %>%
  select(roleId, definition) 
xbrl.disclosure
##
install.packages("XML")
library(XML)


