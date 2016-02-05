#install packages #
install.packages("RCurl")
install.packages("XML")
library(XML)
library(bitops)
library(RCurl)
#GM#
http://www.sec.gov/Archives/edgar/data/1467858/000146785815000036/gm201410k.htm
#GE#
http://www.sec.gov/Archives/edgar/data/40554/000004054515000032/gecc10k2014.htm
#MS#
http://www.sec.gov/Archives/edgar/data/789019/000119312514289961/d722626d10k.htm
#FACEBOOK#(not good)
http://www.sec.gov/Archives/edgar/data/1326801/000132680115000006/fb-12312014x10k.htm
#NIKE#
http://www.sec.gov/Archives/edgar/data/320187/000032018714000097/nke-5312014x10k.htm
#Wal mart#(not good)
http://www.sec.gov/Archives/edgar/data/104169/000010416915000011/wmtform10-kx13115.htm
#CHEVRON#
http://www.sec.gov/Archives/edgar/data/93410/000009341015000010/cvx-123114x10kdoc.htm
# FORD MOTOR#(not good)
http://www.sec.gov/Archives/edgar/data/37996/000003799615000013/f1231201410-k.htm
#AT&T#
http://www.sec.gov/Archives/edgar/data/732717/000073271715000016/ye14_10k.htm
#Apple#
http://www.sec.gov/Archives/edgar/data/320193/000119312514383437/d783162d10k.htm
# download html
theurl <- "http://www.sec.gov/Archives/edgar/data/1326801/000132680115000006/fb-12312014x10k.htm"
#AA#
http://www.sec.gov/Archives/edgar/data/4515/000119312515061145/d829913d10k.htm
#BERKSHIRE HATHAWAY#


# parse html
doc = htmlParse(getURL(theurl), asText=TRUE)
plain.text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::table)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
# clean the text
text<-vector()
text<-which(length(text[grep("7",text,fixed=TRUE)])==1)
if(length(which(plain.text=="\n"))!=0){text<-plain.text[-which(plain.text=="\n")]}else{text<-plain.text}
if(length(which(text=="\n\n"))!=0){text<-text[-which(text=="\n\n")]}else{text<-text}
if(length(agrep("FORM 10-K",text))!=0){text<-text[-agrep("FORM 10-K",text)]}else{text<-text}
if(length(agrep("Table of Contents",text,ignore.case=TRUE))!=0){text<-text[-agrep("Table of Contents",text,ignore.case=TRUE)]}else{text<-text}


# show the section we want
cat(paste(text[agrep("ITEM 1A. RISK FACTORS",text,ignore.case=TRUE):min(agrep("Item 3.Legal Proceedings",text,ignore.case=TRUE),agrep("ITEM 1B.Unresolved Staff Comments",text,ignore.case=TRUE),agrep("ITEM 2.PROPERTIES",text,ignore.case=TRUE),agrep("ITEM 3. LEGAL PROCEEDINGS",text,ignore.case=TRUE))-1], collapse = "\n"))

t<-paste(text[agrep("ITEM 1A. RISK FACTORS",text,ignore.case=TRUE):min(agrep("Item 3",text,ignore.case=TRUE),agrep("ITEM 1B.Unresolved Staff Comments",text,ignore.case=TRUE),agrep("ITEM 2.PROPERTIES",text,ignore.case=TRUE),agrep("ITEM 3. LEGAL PROCEEDINGS",text,ignore.case=TRUE))-1], collapse = "\n")

write.table(t,"GM risk.txt")