#' write_xml
#' 
#' internal function to write xml-file in the gama package
#'
#' @param knownp coordinates of known points
#' @param obs distance and bearing from the unknown points to the known points
#'
#' @return filename to the temporary XML-file
#' 
#' @references Aleš Čepek, GNU gama project, https://www.gnu.org/software/gama/
#'
#' @author Marius Hauglin 2016 \email{marius.hauglin@@nmbu.no}



.write_xml<-function(knownp,obs){

unique_from<-unique(obs$from)		

unique_to<-unique(obs$to)
unique_to<-unique_to[!(unique_to %in% knownp$id)] # exlude known points from this list
	

xml<-c('<?xml version="1.0" ?>',
		'<gama-local xmlns="http://www.gnu.org/software/gama/gama-local">',
		'<network axes-xy="en">',
		

		'<description>',
		'XML input stream of points and observation data for the program GNU gama',
		'</description>',

		'<!-- parameters are expressed with empty-element tag -->',
		'<parameters sigma-act = "aposteriori" />',

		'<points-observations>',
		'<!-- fixed point, constrained point -->'
		)

		
# insert fixed (known) points (example under)		
# <point id="1" y="644498.590" x="1054980.484" fix="xy" />
# <point id="2" y="643654.101" x="1054933.801" fix="xy" /> 	
for (i in 1:nrow(knownp)) xml<-c(xml,paste0('<point id="',knownp$id[i],'" y="',knownp$y[i],'" x="',knownp$x[i],'" fix="xy" />'))	
	
	
# insert observed points (example under)
# <point id="403" adj="xy" />
# <point id="407" adj="xy" />
# <point id="409" adj="xy" />
# <point id="411" adj="xy" />
# <point id="413" adj="xy" />
# <point id="416" adj="xy" />
# <point id="418" adj="xy" />
# <point id="420" adj="xy" />
# <point id="422" adj="xy" />
# <point id="424" adj="xy" />	

for (i in unique_to) xml<-c(xml,paste0('<point id="',i,'" adj="xy" />'))	
for (i in unique_from[!(unique_from %in% knownp$id)]) xml<-c(xml,paste0('<point id="',i,'" adj="xy" />'))	




# <obs from="1">
#	<direction  to=  "2" val=  "0.0000" stdev="10.0" />
#	<direction  to="422" val= "28.2057" stdev="10.0" />
#	<direction  to="424" val= "60.4906" stdev="10.0" />
#	<direction  to="403" val="324.3662" stdev="10.0" />
#	<direction  to="407" val="382.8182" stdev="10.0" />
#	<distance   to=  "2" val= "845.777" stdev="5.0"  />
#	<distance   to="422" val= "493.793" stdev="5.0"  />
#	<distance   to="424" val= "288.301" stdev="5.0"  />
#	<distance   to="403" val= "388.536" stdev="5.0"  />
#	<distance   to="407" val= "498.750" stdev="5.0"  />
#	</obs>	

unique_from<-unique(obs$from)		
for (i in 1:length(unique_from)){	

obsn<-obs[obs$from == unique_from[i],]	

xml<-c(xml,paste0('<obs from="',unique_from[i],'">'))

for (j in 1:nrow(obsn)) xml<-c(xml,paste0('<direction   to="',obsn$to[j],'" val= "',obsn$direction[j],'" stdev="10.0"  />'))	
for (j in 1:nrow(obsn)) xml<-c(xml,paste0('<distance   to="',obsn$to[j],'" val= "',obsn$distance[j],'" stdev="5.0"  />'))	

xml<-c(xml,paste0('</obs>'))

}	
	
# close open xml tags
xml<-c(xml,'</points-observations>','</network>','</gama-local>')

	
# create temp file	
fi<-paste0(tempfile(),'.xml')	

# write to xmlfile
ficon<-file(fi)
writeLines(xml, ficon)
close(ficon)

return(fi)	
}
