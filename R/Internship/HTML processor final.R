#install packages #
install.packages("RCurl")
install.packages("XML")
library(XML)
library(bitops)
library(RCurl)


# download html
theurl <- "http://www.sec.gov/Archives/edgar/data/37996/000003799615000013/f1231201410-k.htm"
# parse html
doc = htmlParse(getURL(theurl), asText=TRUE)
plain.text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::table)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
text<-vector()
if(length(which(plain.text=="\n"))!=0){text<-plain.text[-which(plain.text=="\n")]}else{text<-plain.text}
if(length(which(text=="\n\n"))!=0){text<-text[-which(text=="\n\n")]}else{text<-text}
if(length(agrep("FORM 10-K",text))!=0){text<-text[-agrep("FORM 10-K",text)]}else{text<-text}
if(length(agrep("Table of Contents",text,ignore.case=TRUE))!=0){text<-text[-agrep("Table of Contents",text,ignore.case=TRUE)]}else{text<-text}

# show the section we want
if(length(agrep("ITEM 1A. RISK FACTORS",text,ignore.case=TRUE))!=0)
{
  
  cat(paste(text[agrep("ITEM 1A. RISK FACTORS",text,ignore.case=TRUE):
                   min(agrep("Item 3.Legal Proceedings",text,ignore.case=TRUE),
                       agrep("ITEM 1B.Unresolved Staff Comments",text,ignore.case=TRUE),
                       agrep("ITEM 2.PROPERTIES",text,ignore.case=TRUE),
                       agrep("ITEM 3. LEGAL PROCEEDINGS",text,ignore.case=TRUE))-1], collapse = "\n"))
  
  t<-paste(text[agrep("ITEM 1A. RISK FACTORS",text,ignore.case=TRUE):
                  min(agrep("Item 3",text,ignore.case=TRUE),
                      agrep("ITEM 1B.Unresolved Staff Comments",text,ignore.case=TRUE),
                      agrep("ITEM 2.PROPERTIES",text,ignore.case=TRUE),
                      agrep("ITEM 3. LEGAL PROCEEDINGS",text,ignore.case=TRUE))-1], collapse = "\n")
  
  
  
}else{
  plain.text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
  text<-vector()
  if(length(which(plain.text=="\n"))!=0){text<-plain.text[-which(plain.text=="\n")]}else{text<-plain.text}
  if(length(which(text=="\n\n"))!=0){text<-text[-which(text=="\n\n")]}else{text<-text}
  if(length(agrep("FORM 10-K",text))!=0){text<-text[-agrep("FORM 10-K",text)]}else{text<-text}
  if(length(agrep("Table of Contents",text,ignore.case=TRUE))!=0){text<-text[-agrep("Table of Contents",text,ignore.case=TRUE)]}else{text<-text}
  
  cat(paste(text[max(agrep("ITEM 1A.",text,ignore.case=TRUE)[agrep("Risk Factors",text[agrep("ITEM 1A.",text,ignore.case=TRUE)+1],ignore.case=TRUE)]):
                   min(max(agrep("Item 3.",text,ignore.case=TRUE)[agrep("Legal Proceedings",text[agrep("Item 3.",text,ignore.case=TRUE)+1],ignore.case=TRUE)]),
                       max(agrep("ITEM 1B.",text,ignore.case=TRUE)[agrep("Unresolved Staff Comments",text[agrep("ITEM 1B.",text,ignore.case=TRUE)+1],ignore.case=TRUE)]),
                       max(agrep("ITEM 2.",text,ignore.case=TRUE)[agrep("PROPERTIES",text[agrep("ITEM 2.",text,ignore.case=TRUE)+1],ignore.case=TRUE)]))-1], collapse = "\n"))
  
  t<-paste(text[max(agrep("ITEM 1A.",text,ignore.case=TRUE)[agrep("Risk Factors",text[agrep("ITEM 1A.",text,ignore.case=TRUE)+1],ignore.case=TRUE)]):
                  min(max(agrep("Item 3.",text,ignore.case=TRUE)[agrep("Legal Proceedings",text[agrep("Item 3.",text,ignore.case=TRUE)+1],ignore.case=TRUE)]),
                      max(agrep("ITEM 1B.",text,ignore.case=TRUE)[agrep("Unresolved Staff Comments",text[agrep("ITEM 1B.",text,ignore.case=TRUE)+1],ignore.case=TRUE)]),
                      max(agrep("ITEM 2.",text,ignore.case=TRUE)[agrep("PROPERTIES",text[agrep("ITEM 2.",text,ignore.case=TRUE)+1],ignore.case=TRUE)]))-1], collapse = "\n")
  #write.table(t,"Apple.txt")
}
