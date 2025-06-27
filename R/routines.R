#' function 
#' @title Splitting names 
#' @details:  The function splits concatenated names that are concatenated via “\r\n”, 
#' e.g.,"Agricultural Sciences\r\nAnimal Breeding and Genetics\r\nOther Life Sciences\r\nTechnical Training\r\nConsulting".  
#' @param labelname character vector containing names which have to splitted into single names
#' @return The function returns a character vector containing all single names.
rename.it<-function(labelname)
{
  name1.new <- unlist(sapply(labelname, function(x) (strsplit(x,"\r\n"))))
  name1.new
}

#' function 
#' @title Create barplot
#' @details The function enables the creation of a barplot, whereby various parameters can be specified in order to obtain a suitable result.
#' @param dat table containing the contingency table 
#' @param xlim.add.width numeric, how much should be added to the xlim. xlim is determined by the maximum value of dat + xlim-add-width.
#' @param text.corr.pos numeric, how much should be added to text() in x-direction. x is determined by the maximum value of dat + text.corr.pos 
#' @param font.cex numeric, font size for the parameter cex in text()
#' @param set.para numeric, specification of the xlab to be used
create.barplot<-function(dat,xlim.add.width,text.corr.pos,font.cex,set.para=0)
{
  colo=c("cornflowerblue","azure2","azure4","cadetblue","cadetblue3","cadetblue4","antiquewhite","antiquewhite3","antiquewhite4")
 
  bp<-graphics::barplot(dat,col=colo[1:length(dat)],width=0.5,xlim=c(0,max(dat)+xlim.add.width),horiz=T,las=1, cex.lab=font.cex ,cex.names=font.cex,cex.axis=font.cex) 
  graphics::text(x=max(dat)+text.corr.pos,y=bp,labels=paste(round((dat/sum(dat))*100),"%"),cex=font.cex-0.1)
  if(set.para==0){
    graphics::title(xlab="Frequency", line=1.9, cex.lab=font.cex,outer=FALSE)
  }else{
    graphics::title(xlab="Average rank",line=1.9,cex.lab=font.cex,outer=FALSE)
  }
}

#' function
#' @title Insert line break in a string
#' @param name character, vector of names (string)
#' @param split.len numeric, provide the number at which character length the string should be split
#' @returns The function returns a character vector.
insert.linebreaks <- function(name, split.len)
{ 
  new.names <-apply(as.data.frame(name), 1, function(x) paste(strwrap(x, split.len), collapse = "\n"))
  new.names
}
