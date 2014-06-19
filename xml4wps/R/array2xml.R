#' This function provides a basic xml encoding used in the EVOp prototype wps services
#' 
#' @author Claudia Vitolo
#' 
#' @description This function is used in wps processes based on R code to encode reults in xml format. It takes 3 inputs: datetime, idata and filePath. It transforms the input array 'idata' into an xml document based on a fixed tree. The datetime input is often the index of a zoo time series object, while the idata input is often the core data of a zoo time series object.
#' 
#' @param datetime this is date&time format (often the index of a zoo time series object)      
#' @param idata input array (often the core data of a zoo time series object)
#' @param filePath file path where file should be saved to 
#' 
#' @return saves xml file in the defined filePath
#' 
#' @export
#' 
#' @references Environmental Virtual Observatory Pilot project: http://www.evo-uk.org/
#' 

array2xml <- function(datetime,idata,filePath) {

   # from the package documentation:
   b = newXMLNode("element")
   saveXML(b)
   f = tempfile()
   saveXML(b, f)
   doc = xmlInternalTreeParse(f)
   saveXML(doc)
   con <- xmlOutputDOM()
   
   j=1
   idata2 <- rep(NA,4*length(idata)-1)
   for (i in 1:length(idata)) {
      idata2[j]<-as.character(datetime[i])    #date&time
      j <- j+1
      idata2[j] <- " "     #space
      j <- j+1
      idata2[j]<-idata[i]  #value
      j <- j+1
      idata2[j]<-","       #comma
      j <- j+1
   }
   
   con$addTag("values", idata2)
   
   saveXML(con$value(), file = filePath)

}
