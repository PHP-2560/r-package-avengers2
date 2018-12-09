# Purpose: plot map of contributions at county level

# Initialize libraries 
required_packages = c("dplyr",
                      "tidyr",
                      "rgeos",
                      "maptools",
                      "ggplot2",
                      "ggmap",
                      "rgdal") # list of packages required

# Check if any listed packages are not installed
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
# Install packages_new if it's not empty
if(length(new_packages)) install.packages(new_packages) 
# Load packages
lapply(required_packages, library, character.only = TRUE)

###########################
# Manually pulling in data to test 
# input_state <- "TX"
# input_candidates <- c("O'Rourke, Beto", "Cruz, Ted")
input_state <- "WY"
input_candidates <- c("Barrasso, John", "Trauner, Gary")
df <- read.csv(paste0("../week-09-inclass-avengers/data/", input_state,"_ind_cont.csv")) # use stored data to save some time
###########################

donation_map <- function(df) {

  #read in zip-level shapefile (TO FINISH - AUTOMATICALLY DOWNLOAD MAP VIA CENSUS API TIGRIS)
  zipmap_dir<-"./data/cb_2017_us_zcta510_500k"
  zipusa<-readOGR(dsn = zipmap_dir, layer = "cb_2017_us_zcta510_500k")
  #convert shapefile to dataframe
  zipusa.df<-fortify(zipusa, region = as.character("ZCTA5CE10")) # select as region the geo-level of analysis: zipcode
  #add the shapefile data to the new dataframe
  zipusa@data$ZCTA5CE10<-as.character(zipusa@data$ZCTA5CE10)
  zipusa.df<-left_join(zipusa.df, zipusa@data, by = c('id'='ZCTA5CE10'))
  #remove Alaska, Hawaii and Puerto Rico to have the Contiguous US map
  zipusa.df<- zipusa.df %>%
    filter(long>=-124.7844079 &  long<=-66.9513812 & lat>=24.7433195 & lat<=49.3457868)
  
  # Read in contribution file
  ind.cont<-df
  #extract 5-digits zipcode level (and call it as in the map daatframe)
  ind.cont$GEOID10<- substr(ind.cont$zipcode,1,5)
  #aggregate contributions at the 5-digits zipcode level
  zip.cont<-ind.cont %>%
    group_by(GEOID10,party) %>%
    summarise(tot_contr = sum(amount), avg_contr = mean(amount), sd_contr= sd(amount), max_contr = max(amount)) %>%
    gather(var, val, tot_contr:max_contr) %>%
    unite(temp, party, var) %>%
    spread(temp, val)
  
  #merge zip-level contrib with map
  zipusa.df<- zipusa.df %>%
    left_join(zip.cont)
  
  border<-map_data("usa")
  
  #prepare themes for plot 
  ditch_the_axes <- theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank(), 
    legend.position = c(0.9, 0.15)
  )
  
  #create color scales (blue is already built-in)
  reds<-colorRampPalette(c("lightpink3","darkred"))
  
  ## REPUBLICANS
  # remove outliers (<1% and >99%) for better visualization
  outliers<-as.numeric(quantile(zipusa.df$REP_tot_contr, c(0.01,0.99), na.rm = TRUE))
  zipusa.df$REP_tot_contr[zipusa.df$REP_tot_contr<=outliers[1] | zipusa.df$REP_tot_contr>=outliers[2]]<- NA
  
  # plot
  republican_plot = ggplot(data = zipusa.df) + 
    geom_polygon(aes(x = long , y = lat, group = group, fill = REP_tot_contr), color = NA) +
    scale_fill_gradientn(na.value="snow3", guide = guide_colourbar(ticks = FALSE), colors = reds(10)) +
    coord_fixed(1.3) + 
    ditch_the_axes +
    geom_polygon(data = border, aes(x = long, y = lat, group = group), fill = NA)
  # ggsave(filename = paste0("./output/", state,"_rep_total_map.pdf"))
  # return(republican_plot)
  
  ##DEMOCRATS
  outliers<-as.numeric(quantile(zipusa.df$DEM_tot_contr, c(0.01,0.99), na.rm = TRUE))
  zipusa.df$DEM_tot_contr[zipusa.df$DEM_tot_contr<=outliers[1] | zipusa.df$DEM_tot_contr>=outliers[2]]<- NA
  
  # plot
  democrat_plot = ggplot(data = zipusa.df) + 
    geom_polygon(aes(x = long , y = lat, group = group, fill = DEM_tot_contr), color = NA) +
    scale_fill_gradientn(na.value="snow3", guide = guide_colourbar(ticks = FALSE), colors = blues9) +
    coord_fixed(1.3) + 
    ditch_the_axes +
    geom_polygon(data = border, aes(x = long, y = lat, group = group), fill = NA)
  # ggsave(filename = paste0("./output/", state,"_dem_total_map.pdf"))
  # return(democrat_plot)
  
  maps<-list("Rep" = republican_plot, "Dem" = democrat_plot)
  return(maps)
}

