
landsat_filename_to_metadata <- function(landsat_filename)
{
	landsat_metadata <- vector(mode="list")
	
	sensor_raw <- substr(landsat_filename,1,3)
	if(sensor_raw=="LE7") landsat_metadata$sensor = "Landsat 7 Enhanced Thematic Mapper Plus"
	if(sensor_raw=="LT5") landsat_metadata$sensor = "Landsat 5 Thematic Mapper"
	if(sensor_raw=="LT4") landsat_metadata$sensor = "Landsat 4 Thematic Mapper"
	#TODO: Landsat 8
	
	landsat_metadata$path <- substr(landsat_filename,4,6)
	landsat_metadata$row <- substr(landsat_filename,7,9)
	
	landsat_metadata$year <- substr(landsat_filename,10,13)
	landsat_metadata$doy <- substr(landsat_filename,14,16)
	
	landsat_metadata$ground_station <- substr(landsat_filename,17,19)
	landsat_metadata$version <- substr(landsat_filename,20,21)
	
	return(landsat_metadata)
}