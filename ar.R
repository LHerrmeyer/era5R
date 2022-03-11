library(pacman)
pacman::p_load(tidyverse, ecmwfr, raster, ncdf4,
               viridis, rnaturalearth, USAboundaries)

load_nc <- function(filename){
  nc_data <- nc_open(filename)
  df <- expand.grid(ncvar_get(nc_data, "longitude"),
                      ncvar_get(nc_data, "latitude"),
                      ncvar_get(nc_data, "time"))
  names(df) <- c("Lon","Lat","Time") 
  df <- df %>% mutate(step = match(Time, uniq_times))
  df <- df %>% mutate(Date = as.POSIXct(Time*3600, origin='1900-01-01 00:00'))
  
  for(name in names(nc_data$var)){
    df[name] = as.vector(ncvar_get(nc_data, name))
  }
  if("p71.162" %in% names(df)){
    df <- df %>% 
      rename(viwve=p71.162, viwvn=p72.162)
  }
  if("viwve" %in% names(df)){
    df <- df %>%
      mutate(IVT = sqrt(viwve^2 + viwvn^2))
  }
  
  return(df)
}
nc_data <- nc_open("download_e5.nc")

east_flux <- ncvar_get(nc_data, "p71.162")
north_flux <- ncvar_get(nc_data, "p72.162")
total_ivt <- sqrt(north_flux^2 + east_flux^2)
grid <- expand.grid(ncvar_get(nc_data, "longitude"),
                    ncvar_get(nc_data, "latitude"),
                    ncvar_get(nc_data, "time"))
ivt_df <- as.data.frame(cbind(grid,
                              as.vector(total_ivt),
                              as.vector(east_flux),
                              as.vector(north_flux)
                              ))
colnames(ivt_df) <- c("Lon","Lat","Time","IVT","East_IVT","North_IVT")
uniq_times <- ivt_df$Time %>% unique()
ivt_df <- ivt_df %>% mutate(step = match(Time, uniq_times))
ivt_df <- ivt_df %>% mutate(Date = as.POSIXct(Time*3600, origin='1900-01-01 00:00'))

vec_scale <- 500

plt <- ggplot(ivt_df %>% filter(step == 14) %>% filter(IVT > 150)) +
  geom_raster(aes(fill = IVT, x=Lon, y=Lat)) +
  geom_sf(data=ne_countries(returnclass="sf"), fill=NA) +
  geom_sf(data=us_states(), fill=NA) +
  coord_sf(xlim=c(-130,-100),ylim=c(15,50)) +
  scale_fill_stepsn(limits=c(0,1600), n.breaks=8, colors=rev(inferno(8))) +
  geom_vector(aes(dx=East_IVT/vec_scale,dy=North_IVT/vec_scale,x=Lon,y=Lat),
              arrow.angle=45, skip=5)