library(pacman)
pacman::p_load(tidyverse, ecmwfr, raster, ncdf4,
               viridis, rnaturalearth, USAboundaries, metR,
               gifski, gganimate)

load_nc <- function(filename){
  nc_data <- nc_open(filename)
  df <- expand.grid(ncvar_get(nc_data, "longitude"),
                      ncvar_get(nc_data, "latitude"),
                      ncvar_get(nc_data, "time"))
  names(df) <- c("Lon","Lat","Time") 
  uniq_times <- df$Time %>% unique()
  df <- df %>% mutate(step = match(Time, uniq_times))
  df <- df %>% mutate(Date = as.POSIXct(Time*3600, tz="GMT",
                                        origin='1900-01-01 00:00'))
  # Compensate for off by one hour time error
  
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
  
  nc_close(nc_data)
  return(df)
}

ivt_df <- load_nc("C:\\Users\\Logan\\Documents\\jul_id.nc")

ivt_df <- ivt_df %>%
  filter(Date < "2021-07-21")

num_steps <- length(ivt_df$step %>% unique())
vec_scale <- 500

plt <- ggplot(ivt_df %>% filter(tcw > 25)) +
  geom_raster(aes(fill = tcw, x=Lon, y=Lat)) +
  geom_sf(data=ne_countries(returnclass="sf"), fill=NA) +
  geom_sf(data=us_states(), fill=NA) +
  coord_sf(xlim=c(-130,-100),ylim=c(15,50)) +
  scale_fill_stepsn(limits=c(25,75), n.breaks=10, colors=rev(inferno(10))) +
  transition_time(Date) +
  labs(title = "Total Column Water (mm) and IVT vectors",
       subtitle = "{frame_time-3599.9} UTC") +
  geom_vector(aes(dx=viwve/vec_scale,dy=viwvn/vec_scale,x=Lon,y=Lat),
              arrow.angle=45, skip=5)

animate(plt, nframe=num_steps, fps=2, renderer=gifski_renderer())
anim_save("tcw_jul_id.gif")

plt2 <- ggplot(ivt_df %>% filter(IVT > 150)) +
  geom_raster(aes(fill = IVT, x=Lon, y=Lat)) +
  geom_sf(data=ne_countries(returnclass="sf"), fill=NA) +
  geom_sf(data=us_states(), fill=NA) +
  coord_sf(xlim=c(-130,-100),ylim=c(15,50)) +
  scale_fill_stepsn(limits=c(100,1200), n.breaks=12, colors=rev(inferno(12))) +
  transition_time(Date) +
  labs(title = "IVT (kg*m^-1*s^-1) (shaded >150) and IVT vectors",
       subtitle = "{frame_time-3599.9} UTC") +
  geom_vector(aes(dx=viwve/vec_scale,dy=viwvn/vec_scale,x=Lon,y=Lat),
              arrow.angle=45, skip=5)
animate(plt2, nframe=num_steps, fps=2, renderer=gifski_renderer())
anim_save("ivt_jul_id.gif")