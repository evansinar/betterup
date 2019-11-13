###WRITING CONSOLE TO CLIPBOARD FOR EXCEL AND VICE-VERSA
###REFERENCE https://www.r-bloggers.com/copying-data-from-excel-to-r-and-back/

#To Clipboard

write.excel <- function(EEIbyIndustry,row.names=FALSE,col.names=TRUE,...) {
  write.table(EEIbyIndustry,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(EEIbyIndustry)

#From Clipboard

read.excel <- function(header=TRUE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}

dat=read.excel()