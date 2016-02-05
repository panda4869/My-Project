#difine a function#
RFextraction<-function(address,method.type,file.txt){
  #install packages#
  if(is.element("RCurl",installed.packages()[,1])){library(bitops);library(RCurl)}else{library(bitops);install.packages("RCurl");library(RCurl)}
  if(is.element("XML",installed.packages()[,1])){library(XML)}else{install.packages("XML");library(XML)}
  if(is.element("gtools",installed.packages()[,1])){library(gtools)}else{install.packages("gtools");library(gtools)}
  #define a function to deal with the empty set#
  fixempty<-function(a){
    if(length(a)==0){ return(10000)}else{return(a)}
    

  }
  # download html
  theurl<-address
  # parse html
  doc = htmlParse(getURL(theurl), asText=TRUE)
  #create two separate method#
  #first method where the title "ITEM 1A. RISK FACTORS" is written in one cell#
  if(method.type==1){
    
    plain.text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
    # clean the text
    text<-vector()
    if(length(which(plain.text=="\n"))!=0){text<-plain.text[-which(plain.text=="\n")]}else{text<-plain.text}
    if(length(which(text=="\n\n"))!=0){text<-text[-which(text=="\n\n")]}else{text<-text}
    if(length(which(nchar(text)<=2))!=0){text<-text[-which(nchar(text)<=2)]}
    if(length(agrep("FORM 10-K",text))!=0){text<-text[-agrep("FORM 10-K",text)]}else{text<-text}
    if(length(agrep("Table of Contents",text,ignore.case=TRUE))!=0){text<-text[-agrep("Table of Contents",text,ignore.case=TRUE)]}else{text<-text}
    #text<-text[-(1:ceiling(length(text)*0.009*length(agrep("ITEM 1A. RISK FACTORS",gsub("\\s+", "",text),ignore.case=TRUE))))]
    
    #print the result#
    if(length(agrep("ITEM 1A. RISK FACTORS",gsub("\\s+", "",text),ignore.case=TRUE))!=0){
    cat(paste(text[agrep("ITEM 1A. RISK FACTORS",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("ITEM 1A. RISK FACTORS",gsub("\\s+", "",text),ignore.case=TRUE)])<30)]:
                     min(agrep("Item 3.Legal Proceedings",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("Item 3.Legal Proceedings",gsub("\\s+", "",text),ignore.case=TRUE)])<35)],
                         agrep("ITEM 1B.Unresolved Staff Comments",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("ITEM 1B.Unresolved Staff Comments",gsub("\\s+", "",text),ignore.case=TRUE)])<40)],
                         agrep("ITEM 2.PROPERTIES",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("ITEM 2.PROPERTIES",gsub("\\s+", "",text),ignore.case=TRUE)])<30)],
                         agrep("ITEM 3. LEGAL PROCEEDINGS",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("ITEM 3. LEGAL PROCEEDINGS",gsub("\\s+", "",text),ignore.case=TRUE)])<35)])-1], collapse = "\n"))
    #write a text#
    t<-paste(text[agrep("ITEM 1A. RISK FACTORS",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("ITEM 1A. RISK FACTORS",gsub("\\s+", "",text),ignore.case=TRUE)])<30)]:
                    min(agrep("Item 3.Legal Proceedings",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("Item 3.Legal Proceedings",gsub("\\s+", "",text),ignore.case=TRUE)])<35)],
                        agrep("ITEM 1B.Unresolved Staff Comments",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("ITEM 1B.Unresolved Staff Comments",gsub("\\s+", "",text),ignore.case=TRUE)])<40)],
                        agrep("ITEM 2.PROPERTIES",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("ITEM 2.PROPERTIES",gsub("\\s+", "",text),ignore.case=TRUE)])<30)],
                        agrep("ITEM 3. LEGAL PROCEEDINGS",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("ITEM 3. LEGAL PROCEEDINGS",gsub("\\s+", "",text),ignore.case=TRUE)])<35)])-1], collapse = "\n")
    }
    
    else if(length(agrep("Item1A—RiskFactors",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("Item1A—RiskFactors",gsub("\\s+", "",text),ignore.case=TRUE)])<30)])!=0){
      cat(paste(text[agrep("Item1A—RiskFactors",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("Item1A—RiskFactors",gsub("\\s+", "",text),ignore.case=TRUE)])<30)]:
                       min(agrep("Item3-LegalProceedings",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("Item3-LegalProceedings",gsub("\\s+", "",text),ignore.case=TRUE)])<35)],
                           agrep("ITEM1B-UnresolvedStaff Comments",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("ITEM1B-UnresolvedStaffComments",gsub("\\s+", "",text),ignore.case=TRUE)])<40)],
                           agrep("ITEM2-PROPERTIES",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("ITEM2-PROPERTIES",gsub("\\s+", "",text),ignore.case=TRUE)])<30)],
                           agrep("ITEM3-LEGALPROCEEDINGS",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("ITEM3-LEGALPROCEEDINGS",gsub("\\s+", "",text),ignore.case=TRUE)])<35)])-1], collapse = "\n"))
      #write a text#
      t<-paste(text[agrep("Item1A—RiskFactors",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("Item1A—RiskFactors",gsub("\\s+", "",text),ignore.case=TRUE)])<30)]:
                      min(agrep("Item3-LegalProceedings",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("Item3-LegalProceedings",gsub("\\s+", "",text),ignore.case=TRUE)])<35)],
                          agrep("ITEM1B-UnresolvedStaff Comments",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("ITEM1B-UnresolvedStaffComments",gsub("\\s+", "",text),ignore.case=TRUE)])<40)],
                          agrep("ITEM2-PROPERTIES",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("ITEM2-PROPERTIES",gsub("\\s+", "",text),ignore.case=TRUE)])<30)],
                          agrep("ITEM3-LEGALPROCEEDINGS",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("ITEM3-LEGALPROCEEDINGS",gsub("\\s+", "",text),ignore.case=TRUE)])<35)])-1], collapse = "\n")
      
      
      
      
    }
    else if (length(agrep("Item1ARiskFactors",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("Item1ARiskFactors",gsub("\\s+", "",text),ignore.case=TRUE)])<30)])!=0){
      
      cat(paste(text[agrep("Item1ARiskFactors",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("Item1ARiskFactors",gsub("\\s+", "",text),ignore.case=TRUE)])<30)]:
                       min(agrep("Item3LegalProceedings",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("Item3LegalProceedings",gsub("\\s+", "",text),ignore.case=TRUE)])<35)],
                           agrep("ITEM1BUnresolvedStaff Comments",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("ITEM1BUnresolvedStaffComments",gsub("\\s+", "",text),ignore.case=TRUE)])<40)],
                           agrep("ITEM2PROPERTIES",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("ITEM2PROPERTIES",gsub("\\s+", "",text),ignore.case=TRUE)])<30)],
                           agrep("ITEM3LEGALPROCEEDINGS",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("ITEM3LEGALPROCEEDINGS",gsub("\\s+", "",text),ignore.case=TRUE)])<35)])-1], collapse = "\n"))
      #write a text#
      t<-paste(text[agrep("Item1ARiskFactors",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("Item1ARiskFactors",gsub("\\s+", "",text),ignore.case=TRUE)])<30)]:
                      min(agrep("Item3LegalProceedings",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("Item3LegalProceedings",gsub("\\s+", "",text),ignore.case=TRUE)])<35)],
                          agrep("ITEM1BUnresolvedStaff Comments",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("ITEM1BUnresolvedStaffComments",gsub("\\s+", "",text),ignore.case=TRUE)])<40)],
                          agrep("ITEM2PROPERTIES",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("ITEM2PROPERTIES",gsub("\\s+", "",text),ignore.case=TRUE)])<30)],
                          agrep("ITEM3LEGALPROCEEDINGS",gsub("\\s+", "",text),ignore.case=TRUE)[which(nchar(text[agrep("ITEM3LEGALPROCEEDINGS",gsub("\\s+", "",text),ignore.case=TRUE)])<35)])-1], collapse = "\n")
      
      
      
      
    }
    
    #output the result in txt form#
    write.table(t,file.txt)
    print("A txt file has been created")
    
  }
  #Second method where the title "ITEM 1A." and "RISK FACTORS" are written in different cells#
  if(method.type==2){
    
    plain.text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
    # clean the text
    text<-vector()
    if(length(which(plain.text=="\n"))!=0){text<-plain.text[-which(plain.text=="\n")]}else{text<-plain.text}
    if(length(which(text=="\n\n"))!=0){text<-text[-which(text=="\n\n")]}else{text<-text}
    if(length(which(nchar(text)<=2))!=0){text<-text[-which(nchar(text)<=2)]}
    if(length(agrep("FORM 10-K",text))!=0){text<-text[-agrep("FORM 10-K",text)]}else{text<-text}
    if(length(agrep("Table of Contents",text,ignore.case=TRUE))!=0){text<-text[-agrep("Table of Contents",text,ignore.case=TRUE)]}else{text<-text}
    
    if(length(agrep("ITEM 1A.",gsub("\\s+", "",text),ignore.case=TRUE)[agrep("Risk Factors",gsub("\\s+", "",text[agrep("ITEM 1A.",gsub("\\s+", "",text),ignore.case=TRUE)+1]),ignore.case=TRUE)])!=0){
    cat(paste(text[max(fixempty(agrep("ITEM 1A.",gsub("\\s+", "",text),ignore.case=TRUE)[agrep("Risk Factors",gsub("\\s+", "",text[agrep("ITEM 1A.",gsub("\\s+", "",text),ignore.case=TRUE)+1]),ignore.case=TRUE)])):
                     min(max(fixempty(agrep("Item 3.",gsub("\\s+", "",text),ignore.case=TRUE)[agrep("Legal Proceedings",gsub("\\s+", "",text[agrep("Item 3.",gsub("\\s+", "",text),ignore.case=TRUE)+1]),ignore.case=TRUE)])),
                         max(fixempty(agrep("ITEM 1B.",gsub("\\s+", "",text),ignore.case=TRUE)[agrep("Unresolved Staff Comments",gsub("\\s+", "",text[agrep("ITEM 1B.",gsub("\\s+", "",text),ignore.case=TRUE)+1]),ignore.case=TRUE)])),
                         max(fixempty(agrep("ITEM 2.",gsub("\\s+", "",text),ignore.case=TRUE)[agrep("PROPERTIES",gsub("\\s+", "",text[agrep("ITEM 2.",gsub("\\s+", "",text),ignore.case=TRUE)+1]),ignore.case=TRUE)])))-1], collapse = "\n"))
    
    t<-paste(text[max(fixempty(agrep("ITEM 1A.",gsub("\\s+", "",text),ignore.case=TRUE)[agrep("Risk Factors",gsub("\\s+", "",text[agrep("ITEM 1A.",gsub("\\s+", "",text),ignore.case=TRUE)+1]),ignore.case=TRUE)])):
                    min(max(fixempty(agrep("Item 3.",gsub("\\s+", "",text),ignore.case=TRUE)[agrep("Legal Proceedings",gsub("\\s+", "",text[agrep("Item 3.",gsub("\\s+", "",text),ignore.case=TRUE)+1]),ignore.case=TRUE)])),
                        max(fixempty(agrep("ITEM 1B.",gsub("\\s+", "",text),ignore.case=TRUE)[agrep("Unresolved Staff Comments",gsub("\\s+", "",text[agrep("ITEM 1B.",gsub("\\s+", "",text),ignore.case=TRUE)+1]),ignore.case=TRUE)])),
                        max(fixempty(agrep("ITEM 2.",gsub("\\s+", "",text),ignore.case=TRUE)[agrep("PROPERTIES",gsub("\\s+", "",text[agrep("ITEM 2.",gsub("\\s+", "",text),ignore.case=TRUE)+1]),ignore.case=TRUE)])))-1], collapse = "\n")

    }
    else if(length(agrep("ITEM 1A.",gsub("\\s+", "",text),ignore.case=TRUE)[agrep("Risk Factors",gsub("\\s+", "",text[agrep("ITEM 1A.",gsub("\\s+", "",text),ignore.case=TRUE)+2]),ignore.case=TRUE)])!=0){
    
      cat(paste(text[max(fixempty(agrep("ITEM 1A.",gsub("\\s+", "",text),ignore.case=TRUE)[agrep("Risk Factors",gsub("\\s+", "",text[agrep("ITEM 1A.",gsub("\\s+", "",text),ignore.case=TRUE)+2]),ignore.case=TRUE)])):
                       min(max(fixempty(agrep("Item 3.",gsub("\\s+", "",text),ignore.case=TRUE)[agrep("Legal Proceedings",gsub("\\s+", "",text[agrep("Item 3.",gsub("\\s+", "",text),ignore.case=TRUE)+2]),ignore.case=TRUE)])),
                                    max(fixempty(agrep("ITEM 1B.",gsub("\\s+", "",text),ignore.case=TRUE)[agrep("Unresolved Staff Comments",gsub("\\s+", "",text[agrep("ITEM 1B.",gsub("\\s+", "",text),ignore.case=TRUE)+2]),ignore.case=TRUE)])),
                                             max(fixempty(agrep("ITEM 2.",gsub("\\s+", "",text),ignore.case=TRUE)[agrep("PROPERTIES",gsub("\\s+", "",text[agrep("ITEM 2.",gsub("\\s+", "",text),ignore.case=TRUE)+2]),ignore.case=TRUE)])))-1], collapse = "\n"))
      
      t<-paste(text[max(fixempty(agrep("ITEM 1A.",gsub("\\s+", "",text),ignore.case=TRUE)[agrep("Risk Factors",gsub("\\s+", "",text[agrep("ITEM 1A.",gsub("\\s+", "",text),ignore.case=TRUE)+2]),ignore.case=TRUE)])):
                      min(max(fixempty(agrep("Item 3.",gsub("\\s+", "",text),ignore.case=TRUE)[agrep("Legal Proceedings",gsub("\\s+", "",text[agrep("Item 3.",gsub("\\s+", "",text),ignore.case=TRUE)+2]),ignore.case=TRUE)])),
                          max(fixempty(agrep("ITEM 1B.",gsub("\\s+", "",text),ignore.case=TRUE)[agrep("Unresolved Staff Comments",gsub("\\s+", "",text[agrep("ITEM 1B.",gsub("\\s+", "",text),ignore.case=TRUE)+2]),ignore.case=TRUE)])),
                          max(fixempty(agrep("ITEM 2.",gsub("\\s+", "",text),ignore.case=TRUE)[agrep("PROPERTIES",gsub("\\s+", "",text[agrep("ITEM 2.",gsub("\\s+", "",text),ignore.case=TRUE)+2]),ignore.case=TRUE)])))-1], collapse = "\n")
      
      
    }
      
      else if(length(agrep("ITEM 1A",gsub("\\s+", "",text),ignore.case=TRUE)[agrep("Risk Factors",gsub("\\s+", "",text[agrep("ITEM 1A",gsub("\\s+", "",text),ignore.case=TRUE)+2]),ignore.case=TRUE)])!=0){
      
      cat(paste(text[max(fixempty(agrep("ITEM 1A",gsub("\\s+", "",text),ignore.case=TRUE)[agrep("Risk Factors",gsub("\\s+", "",text[agrep("ITEM 1A",gsub("\\s+", "",text),ignore.case=TRUE)+2]),ignore.case=TRUE)])):
                       min(max(fixempty(agrep("Item 3",gsub("\\s+", "",text),ignore.case=TRUE)[agrep("Legal Proceedings",gsub("\\s+", "",text[agrep("Item 3",gsub("\\s+", "",text),ignore.case=TRUE)+2]),ignore.case=TRUE)])),
                           max(fixempty(agrep("ITEM 1B",gsub("\\s+", "",text),ignore.case=TRUE)[agrep("Unresolved Staff Comments",gsub("\\s+", "",text[agrep("ITEM 1B",gsub("\\s+", "",text),ignore.case=TRUE)+2]),ignore.case=TRUE)])),
                           max(fixempty(agrep("ITEM 2",gsub("\\s+", "",text),ignore.case=TRUE)[agrep("PROPERTIES",gsub("\\s+", "",text[agrep("ITEM 2",gsub("\\s+", "",text),ignore.case=TRUE)+2]),ignore.case=TRUE)])))-1], collapse = "\n"))
      
      t<-paste(text[max(fixempty(agrep("ITEM 1A",gsub("\\s+", "",text),ignore.case=TRUE)[agrep("Risk Factors",gsub("\\s+", "",text[agrep("ITEM 1A",gsub("\\s+", "",text),ignore.case=TRUE)+2]),ignore.case=TRUE)])):
                      min(max(fixempty(agrep("Item 3",gsub("\\s+", "",text),ignore.case=TRUE)[agrep("Legal Proceedings",gsub("\\s+", "",text[agrep("Item 3",gsub("\\s+", "",text),ignore.case=TRUE)+2]),ignore.case=TRUE)])),
                          max(fixempty(agrep("ITEM 1B",gsub("\\s+", "",text),ignore.case=TRUE)[agrep("Unresolved Staff Comments",gsub("\\s+", "",text[agrep("ITEM 1B",gsub("\\s+", "",text),ignore.case=TRUE)+2]),ignore.case=TRUE)])),
                          max(fixempty(agrep("ITEM 2",gsub("\\s+", "",text),ignore.case=TRUE)[agrep("PROPERTIES",gsub("\\s+", "",text[agrep("ITEM 2",gsub("\\s+", "",text),ignore.case=TRUE)+2]),ignore.case=TRUE)])))-1], collapse = "\n")
      
      
      
    }
    #output the result in txt form#
    write.table(t,file.txt)
    print("A txt file has been created")
  } 
  
  
}

#example: GE#
RFextraction("http://www.sec.gov/Archives/edgar/data/50863/000005086315000015/a10kdocument12272014.htm",2,"GE.txt")

