#' adjust_network
#' 
#' run adjustment of a geodetic free network, using the GNU gama library.
#'
#' @param fixed data.frame; giving the coordinates to the fixed (known) points in the network.
#' @param obs data.frame; giving the measured distance and bearing to the known and unknown points.
#' @param output optional filename for the output from gama-local. Some info from the R-package will
#' be added. No information from gama-local will be removed.
#'
#' @return a data.frame with the coordinates for all the points in the network, including the known points.
#' Optionally a file with the output from the adjustment in gama-local (adjustment errors etc.).
#'
#' @note the input coordinates of the known points should be formated as a three-column data.frame
#' with three columns
#'   
#'  \itemize{
#'  \item \bold{id}  name of the point
#'  \item \bold{x}  x (east) coordinate of the point
#'  \item \bold{y}  y (north) coordinate of the point
#'	}
#'  
#' The input distances and bearings should be formated as a data.frame with four colums:
#' 
#'  \itemize{
#'  \item \bold{from}  name of the point from where the observation is done
#'  \item \bold{to}  name of the observed point 
#'  \item \bold{direction}  bearing to the observed point
#'  \item \bold{distance}  distance to the observed point
#'	}
#' 
#' @references Aleš Čepek, GNU gama project, https://www.gnu.org/software/gama/
#'
#' @author Marius Hauglin 2016-2017 \email{marius.hauglin@@gmail.com}
#' @export



adjust_network<-function(fixed,obs,output=NA){
	
	if (is.na(output)) { out_temp<-TRUE } else {out_temp<-FALSE}
	
	tryCatch({ # in order to properly clean up on errors	
	
	
# generate temp xml file	
xml_file<-.write_xml(knownp=fixed,obs=obs)


# output in another (temp) file
if (is.na(output)) output<-paste0(tempfile(),'.txt')	


# run gama-local
system(paste0('gama-local ',xml_file,' --text ',output))
	

# parse output	
res_cols<-c('id','x','y')
res<-data.frame(matrix(nrow=0,ncol=length(res_cols)))
outp<-scan(output,sep='\n',what='character',quiet = TRUE,strip.white=TRUE)	

for (i in 1:length(outp))if (outp[i] == 'Adjusted coordinates'){i=i+4;break}
	
repeat{
	
	if(outp[i] == 'Adjusted orientation unknowns') break
	
	id<-outp[i]
	i<-i+1
	x<-unlist(strsplit(outp[i],' '))
	x<-x[x != ""][5]
	
	i<-i+1
	y<-unlist(strsplit(outp[i],' '))
	y<-y[y != ""][5]
	
	
	res<-rbind(res,data.frame(id=id,x=x,y=y))
	
	i<-i+1
	
}

res<-rbind(res,fixed)
res$x<-as.numeric(res$x)
res$y<-as.numeric(res$y)


# if permanent outfile, add info from R-package gama
if(!out_temp){
	
	fConn <- file(output, 'r+')
	Lines <- readLines(fConn)
	rgama_pretext<-paste('R-package gama version',packageVersion("gama"),'\n',
			R.Version()$version.string,'\n**************************************\n\n')
	outtext<-c(rgama_pretext, Lines, sep='\n')
	writeLines(outtext,con = fConn)
	
	writeLines('\n\nR gama inputs:\n**************\n',con = fConn)
	writeLines('\nknown points:\n',con = fConn)
	capture.output(known,file=fConn)
	writeLines('\n\nobs points:\n',con = fConn)
	capture.output(obs,file=fConn)
	
	lastline<-paste('\n\nThis file was created by R gama,',date())
	writeLines(lastline,con = fConn)
	
	close (fConn)
	
}



}, finally={
	
	# delete all temporary files
	file.remove(xml_file)
	if(out_temp) file.remove(output)
	
	
	
}) # end tryCatch statement	


	return(res)
	
} # end adjust network



