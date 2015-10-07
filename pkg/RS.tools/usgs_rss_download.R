# order_rss <- http://espa.cr.usgs.gov/status/jgrn@illinois.edu/rss/

download_folder <- "P:\\lidar_unmixing\\raster\\landsat_tm\\raw\\downloaded"

library("XML")

rss_parse <- xmlTreeParse(order_rss)
download_paths <- xpathApply(xmlRoot(rss_parse),"//item")
setwd(download_folder)
sfQuickInit()
foreach(i=1:length(download_paths),.packages=c("XML")) %dopar%
{
	dlURL <- xmlSApply(download_paths[[i]],xmlValue)["link"]
	download.file(dlURL,basename(dlURL),mode="wb")
}