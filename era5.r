# https://confluence.ecmwf.int/pages/viewpage.action?pageId=216496313

#  '2m_dewpoint_temperature', '2m_temperature', 'convective_available_potential_energy',
# 'convective_inhibition', 'convective_precipitation', 'k_index',
# 'mean_convective_precipitation_rate', 'mean_sea_level_pressure', 'total_column_water_vapour',
# 'total_column_water'
get_era5 <- function(var,
                     year,
                     month,
                     day,
                     time,
                     area=c(90, -180, -90, 180),
                     filename="download_e5.nc",
                     dataset="reanalysis-era5-single-levels"){
  if(!file.exists(filename)){
    request <- list(
      dataset_short_name = dataset,
      product_type   = "reanalysis",
      format = "netcdf",
      variable = var,
      year = year,
      month = month,
      day = day,
      # c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
      time = time,
      # area is specified as N, W, S, E
      area = area,
      target = filename
    )
    
    myfile <- wf_request(user = "125674",
                         request = request,
                         transfer = TRUE,
                         path = ".",
                         verbose = TRUE)
  }
  
  return(myfile)
}

#file <- get_era5(var=c("vertical_integral_of_eastward_water_vapour_flux", "vertical_integral_of_northward_water_vapour_flux"),year="2021",month="07",day=c("17","18","19","20"),time=sprintf("%02d:00",(0:23)),area=c(50, -130, 15, -100))

if(FALSE){
  file <- get_era5(
    var=c("vertical_integral_of_eastward_water_vapour_flux",
          "vertical_integral_of_northward_water_vapour_flux",
          "total_column_water_vapour",
          "k_index",
          "mean_sea_level_pressure",
          "convective_available_potential_energy"),
    year="2021",
    month="07",
    day=c("17","18","19","20"),
    time=sprintf("%02d:00",(0:6)*3),area=c(50, -130, 15, -100), filename="jul_id.nc")
  
}