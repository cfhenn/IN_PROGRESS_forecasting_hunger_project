#*******************************************************************************
#* This program extracts data for temperature, rainfall, nightlight, and hunger
#* for Malawi. The hunger data comes from the LSMS-ISA survey in Malawi in 2013.
#* All other data comes from .tif files from satellite data.
#* To run the program, is necessary to already have all of the .tif files in 
#* folders along with text files listing all of their file names.
#* The data is combined into one large .csv file disaggregated by LSMS-ISA
#* enumeration area and month.
#*******************************************************************************

rm(list=ls())

library(raster)
library(rgdal)
library(foreign)
library(data.table)
library(dplyr)

#*******************************************************************************
#* Function Description:  Takes global satellite images (as .tif files) taken 
#* monthly and pulls data  from specific  country, and combines data from monthly 
#* images into one wide-format  file
#* 
#* Inputs:  a path to a folder storing the .tif files, the name of a text file 
#* containing the names of all the .tif files being combined, a shapefile of the 
#* region for which data is desired
#* 
#* Outputs:  one data frame with a number of months of data for each geo location
#* 
#* Assumptions:  longitude and latitude are first two columns of the dataframes
#* that  results when the .tif files are read
#* 
#*******************************************************************************
combine_satellite_data_from_monthly_tif_files <- function(path, file_names_doc, shapefile) {
  
  #load names of all tif files from file
  filenames <- read.table(file_names_doc)
  
  #get the data for the first month (turn .tif into raster into dataframe)
  df <- as.data.frame(crop(raster(paste0(path, filenames[1,1], ".tif")), shapefile), xy=TRUE)
  names(df)[1:2] <- c("longitude", "latitude")
  
  #merge in the data for all future months
  for (row in 2:nrow(filenames)) {
    curr <- as.data.frame(crop(raster(paste0(path, filenames[row,1], ".tif")), shapefile), xy=TRUE)
    names(curr)[1:2] <- c("longitude", "latitude")
    df <- merge(df, curr, by = c("longitude", "latitude"))
    
    #display progress
    print(paste("currently loading", filenames[row,1]))
  }
  #return dataframe with data by month (in wide format)
  df
}



#*******************************************************************************
#* Description: Matches geo-locations to the LSMS-ISA  survey enumaration areas 
#* that they would fall  into
#* 
#* Inputs: a data  frame  with latitudes/longitudes and a dataframe with LSMS-
#* ISA enumeration areas(EAs) as well as latitudes and longitudes
#* 
#* Outputs: the lat/lon data frame with a column saying which EA it would be in
#* 
#* Assumptions: columns named latitude, longitude, in both dfs
#* column named "enumeration_area" in district df
#*******************************************************************************
find_districts <- function(geo_df, district_df) {
  for (row in 1:nrow(geo_df)){
    temp <- district_df[(district_df$min_lat <= geo_df$latitude[row]),]
    temp <- temp[(district_df$min_lat <= geo_df$latitude[row]),]
    temp <- temp[(district_df$max_lat >= geo_df$latitude[row]),]
    temp <- temp[(district_df$min_lon <= geo_df$longitude[row]),]
    temp <- temp[(district_df$max_lon >= geo_df$longitude[row]),]
    geo_df$enumeration_area[row] <- temp$enumeration_area[1]
    
    remove(temp)
  }
  geo_df <- geo_df[!(is.na(geo_df$enumeration_area)),]
  geo_df
}



#*******************************************************************************
#* Description:
#* Inputs:
#* Outputs:
#* Assumptions:
#*******************************************************************************
shrink_geo_df <- function(geo_df) {
  geo_df$latitude  <- round(geo_df$latitude,  digits = 2)
  geo_df$longitude <- round(geo_df$longitude, digits = 2)
  geo_df <- aggregate(.~latitude+longitude, geo_df, mean)
}


#*******************************************************************************
#* Description: reads data a .tif files, reduces the precision to 
#* one observation to two-digit lat/lon combo (for speed), finds the ea of each
#* location, and aggregegates by ea
#* 
#* Inputs: needs a file path that leads to a folder with all the needed .tif
#* files, and the path to a text file containing the names of all the .tif files
#* names for each variable read in  from  the .tif file, a dataframe  with the
#* lat/lons of  the extremes of each ea, and  a descriptor of the  data  the  
#* .tif files contain
#* 
#* Outputs: a data frame with the data arranged by month/ea
#* 
#*******************************************************************************
prepare_data_for_merging <- function(path, file_names_doc, shapefile, varnames, district_df, data_description) {
  df <- combine_satellite_data_from_monthly_tif_files(path, file_names_doc, shapefile)
  names(df)[3:ncol(df)] <- varnames
  df <- shrink_geo_df(df)
  df <- find_districts(df, district_df)
  df <- subset(df, select = -c(latitude, longitude))
  df <- aggregate(. ~ enumeration_area, df, mean)
  df <- melt(df, id="enumeration_area")
  names(df)[2:3] <- c("month", data_description)
  df
}


#*******************************************************************************
#* Description: reads in data from a .dta file as a data  frame
#*
#* Inputs: a path to a .dta file, a list of names of vars desired from that file
#* a list of new names for those vars
#* 
#* Outputs: a data frame with the desired data and desired names
#* 
#*******************************************************************************
get_lsms_data_as_df <- function(lsms_file, lsms_varnames, new_varnames) {
  print(lsms_file)
  df <- as.data.frame(read.dta(lsms_file))
  df <- df[lsms_varnames]
  setnames(df, old = lsms_varnames, new = new_varnames)
}

#*******************************************************************************
#* Description: gets the furthest point of each LSMS-ISA EA  in  each of the  
#* cardinal directions
#* 
#* Inputs: a data frame  with latitudes & longitudes & ea numbers
#* 
#* Outputs: a data  frame of each ea with  its four most extreme points
#* 
#*******************************************************************************
get_ea_limits <- function(lsms_df) {
  ea_mincoords <- aggregate(cbind(latitude, longitude) ~ enumeration_area, lsms_df, FUN = min)
  names(ea_mincoords) <- c("enumeration_area", "min_lat", "min_lon")
  ea_maxcoords <- aggregate(cbind(latitude, longitude) ~ enumeration_area, lsms_df, FUN = max)
  names(ea_maxcoords) <- c("enumeration_area", "max_lat", "max_lon")
  ea_limits <- merge(ea_mincoords, ea_maxcoords, by = "enumeration_area")
  remove(ea_mincoords)
  remove(ea_maxcoords)
  ea_limits
}

project_path     <- "//netid.washington.edu/csde/other/Desktop/cfhenn/Desktop/Capstone/data/"
malawi_shp       <- readOGR("//netid.washington.edu/csde/other/desktop/cfhenn/Desktop/Capstone/mwi_adm_nso_20181016_shp/mwi_admbnda_adm0_nso_20181016.shp")
lsms_months      <- c("2012_04", "2012_05", "2012_06", "2012_07", "2012_08", "2012_09", "2012_10", "2012_11", "2012_12", "2013_01", "2013_02", "2013_03", "2013_04", "2013_05", "2013_06", "2013_07", "2013_08", "2013_09", "2013_10")


#*******************************************************************************
#*This section of the program retrieves data from LSMS
#*data on monthly hunger and data  on the geography of enumeration  areas
#*******************************************************************************
lsms_hunger_vars  <- c("y2_hhid", "hh_h05a", "hh_h05b", "hh_h05c", "hh_h05d", "hh_h05e", "hh_h05f", "hh_h05g", "hh_h05h", "hh_h05i", "hh_h05j", "hh_h05k", "hh_h05l", "hh_h05m", "hh_h05n", "hh_h05o", "hh_h05p", "hh_h05q", "hh_h05r", "hh_h05s")
lsms_hunger_names <- c("HHID",  lsms_months)  
lsms_geo_vars     <- c("y2_hhid", "LAT_DD_MOD", "LON_DD_MOD")
lsms_geo_names    <- c("HHID", "latitude",   "longitude")
lsms_admin_vars   <- c("y2_hhid", "ea_id")#district
lsms_admin_names  <- c("HHID", "enumeration_area") #district

lsms_admin_df     <- get_lsms_data_as_df(paste0(project_path, "lsms_isa/HH_MOD_A_FILT_13.dta"), lsms_admin_vars, lsms_admin_names)
lsms_geo_df       <- get_lsms_data_as_df(paste0(project_path, "lsms_isa/HouseholdGeovariables_IHPS_13.dta"), lsms_geo_vars, lsms_geo_names)
lsms_hunger_df    <- get_lsms_data_as_df(paste0(project_path, "lsms_isa/HH_MOD_H_13.dta"), lsms_hunger_vars, lsms_hunger_names)
  
hunger_df         <-  merge(merge(lsms_admin_df, lsms_geo_df, by = "HHID"), lsms_hunger_df, by = "HHID")
ea_limits <- get_ea_limits(hunger_df)

hunger_df <-  subset(hunger_df, select = -c(HHID, latitude, longitude))

#make variables 1/0 rather than x/space
hunger_df[hunger_df == "X"] <- 1
hunger_df[hunger_df == ""]  <- 0

#put hunger data in long format, to merge with satellite data later on
hunger_df <- melt(hunger_df, id="enumeration_area")
names(hunger_df)[2:3] <- c("month", "hunger")
hunger_df$hunger <- as.integer(hunger_df$hunger)

#get hunger prevalence per unique ea/month
hunger_df <- aggregate(hunger ~  enumeration_area+month, hunger_df, mean)



#*******************************************************************************
#*This section of the program retrieves data from various publicly available
#*satellite sources on climate and nightlight
#*******************************************************************************
maxtemp_df <- prepare_data_for_merging(paste0(project_path, "maxtemp/"), paste0(project_path, "maxtemp/","maxtemp_filenames.txt"), malawi_shp, lsms_months, ea_limits, "maxtemp")
mintemp_df <- prepare_data_for_merging(paste0(project_path, "mintemp/"), paste0(project_path, "mintemp/","mintemp_filenames.txt"), malawi_shp, lsms_months, ea_limits, "mintemp")
avgtemp_df <- prepare_data_for_merging(paste0(project_path, "avgtemp/"), paste0(project_path, "avgtemp/","avgtemp_filenames.txt"), malawi_shp, lsms_months, ea_limits, "avgtemp")
precipt_df <- prepare_data_for_merging(paste0(project_path, "precipitation/"), paste0(project_path, "precipitation/","precipitation_filenames.txt"), malawi_shp, lsms_months, ea_limits, "precipitation")
nightlt_df <- prepare_data_for_merging(paste0(project_path, "nightlight/"), paste0(project_path, "nightlight/","nightlight_filenames.txt"), malawi_shp, lsms_months, ea_limits, "nightlight")



#*******************************************************************************
#*This section merges  all data frames together and  saves them  to a .csv  file
#*******************************************************************************
df_merge <- merge(merge(merge(merge(merge(
              hunger_df, maxtemp_df, by = c("enumeration_area", "month")),
                          mintemp_df, by = c("enumeration_area", "month")), 
                            avgtemp_df, by = c("enumeration_area", "month")),
                              precipt_df, by = c("enumeration_area", "month")), 
                                nightlt_df, by = c("enumeration_area", "month"))

write.csv(df_merge, paste0(project_path, "combined_malawi_data/", "malawi_ea_level_data.csv"), row.names = FALSE)
rm(list=ls())