install.packages("RCurl")
install.packages("XML")

library(XML)
library(bitops)
library(RCurl)


theurl <- "http://www.sec.gov/Archives/edgar/data/40554/000004054515000032/gecc10k2014.htm"
webpage <- getURL(theurl)
webpage <- readLines(tc <- textConnection(webpage)); close(tc)

pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
doc<-htmlParse("http://www.sec.gov/Archives/edgar/data/40554/000004054515000032/gecc10k2014.htm")


doc = htmlParse(getURL(theurl), asText=TRUE)
plain.text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::table)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
text<-vector()

text<-plain.text[-which(plain.text=="\n")]
text<-text[-which(text=="\n\n")]
text<-text[-which(text=="  ")]
View(text)



cat(paste(text[grep("ITEM 1A. RISK FACTORS",text):min(grep("Item 3",text),grep("ITEM 1B.Unresolved Staff Comments",text),grep("ITEM 2.PROPERTIES",text),grep("ITEM 3. LEGAL PROCEEDINGS",text))-1], collapse = "\n"))

