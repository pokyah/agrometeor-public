#'---
#'author: "Thomas Goossens (CRA-W) - t.goossens@cra.wallonie.be"
#'output: 
#'  html_document:
#'    theme: default
#'    toc: false
#'    toc_depth: 6
#'    toc_float:
#'      collapsed: false
#'      smooth_scroll: true
#'title: "Group of R helper functions commonly used in various R scripts developed in the context of the Agromet Project"
#'date: \ 20-04-2018\
#'---


#+ ---------------------------------
#' ## helper function to add a day or night column on the basis of mtime, sunset and sunrise columns
#' 
#+ is_it_day, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

  h.is_it_day <- function(records.df){
    
    # loading the required package
    library(chron)
  
    # declaration of the function to return a boolean for day state
    returnDayState <- function(mtime, sunrise, sunset){
      if(times(strftime(mtime,"%H:%M:%S")) >= sunrise && times(strftime(mtime, format="%H:%M:%S")) <= sunset){
        day <- TRUE
      }else{
        day <- FALSE
      }
      return(day)
    }
    # add a boolean column for day = TRUE or FALSE
    records.df <- records.df %>%
      rowwise() %>%
      mutate(day=returnDayState(mtime, sunrise, sunset))
    
    # reorder and return the dataframe
    meta <-subset(records.df, select=(1:12))
    sun <-subset(records.df, select=(c(13,14,length(records.df))))
    sensors <- subset(records.df, select=(15:(length(records.df)-1)))
    records.df <- data.frame(bind_cols(meta, sun, sensors))
    return(records.df)
  }
  
  

#+ ---------------------------------
#' ## helper function to check for the presence of NA values.  
#' Returns a dataframe of NA values if present 
#+ check_NA, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

  h.check_NA <- function(records.df){
    numberOfNAObs <- nrow(records.df[rowSums(is.na(records.df)) > 0,])
    logNoNa.df <- records.df[rowSums(is.na(records.df))==0,]
    if((numberOfNAObs > 0) ==TRUE){
      cat("NA values found in dataframe. ")
      logNa.df <- records.df[rowSums(is.na(records.df))>0,]
      return(logNa.df)
    }else{
      cat("No NA values found in dataframe. ")
    }
  }

#+ ---------------------------------
#' ## helper function to define a color palette based on the [ggplot default colors](https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette).  
#+ ggplot_colours, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

  h.ggplot_colours <- function(n = 6, h = c(0, 360) + 15){
    if ((diff(h) %% 360) < 1) h[2] <- h[2] - 360/n
    hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
  }

#+ ---------------------------------
#' ## helper function to get the widest shared date range for 2 stations 
#+ get_date, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

  h.get_date <- function(records.df, s1, s2, type){
    format = "%Y-%m-%d %H:%M:%S"
    tz = "GMT"
    s1_data <- filter(records.df, sid==get_id(s1))
    s2_data <- filter(records.df, sid==get_id(s2))
    if(type == "min"){
      min_s1 <- as.POSIXct(s1_data$from, format=format, tz=tz)
      min_s2 <- as.POSIXct(s2_data$from, format=format, tz=tz)
      minDate <- max(c(min_s1, min_s2))
      return(minDate)
    }
    if(type == "max"){
      maxDate <- as.POSIXct(Sys.Date())
      if(get_id(s1) == "1000" || get_id(s2) == "1000"){
        maxDate = as.Date("2017-11-01")
      }
      return(maxDate)
    }
  }

#+ ---------------------------------
#' ## helper function to get the id of a station 
#+ get_id, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

  h.get_id <- function(station_name_id.chr){
    id <- strsplit(station_name_id.chr, " - ")[[1]][2]
    return(id)
  }

#+ ---------------------------------
#' ## helper function to filter the records based on passed criteria
#+ filter_records, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

  h.filter_records <- function(records.df, sensor.chr, dateRange.chr, filter.chr){
    records.df <- records.df %>% 
      dplyr::filter(
        between(as.Date(mtime), dateRange.chr[1], dateRange.chr[2])
      ) 
    # removing observations with NA values. We need to remove for all stations to keep station by station observations
    station1.df <- records.df %>% dplyr::filter(sid== unique(records.df$sid)[1]) %>% dplyr::filter_(paste("!is.na(",sensor.chr, ")"))
    station2.df <- records.df %>% dplyr::filter(sid== unique(records.df$sid)[2]) %>% dplyr::filter_(paste("!is.na(",sensor.chr, ")"))
    station2.df <- semi_join(station2.df, station1.df, by="mtime")
    records.df <- bind_rows(station1.df, station2.df)
    
    # applying the passed filter
    if(filter.chr == "no_extra_filter"){
      records.df
    }
    if(filter.chr == "day_only"){
      records.df <- h.is_it_day(records.df) %>% dplyr::filter(day==TRUE)
    }
    if(filter.chr == "night_only"){
      records.df <- h.is_it_day(records.df) %>% dplyr::filter(day==FALSE)
    }
    if(filter.chr == "high_rad_low_wind"){
      vvt.q75.df <- records.df %>%
        summarise_(.dots = paste0('quantile(', "vvt",', probs= .75, na.rm = TRUE)'))
      day.records.df <- filter(h.is_it_day(records.df), day==TRUE)
      ens.day.mean.df <- day.records.df %>%
        summarise_(.dots = paste0('mean(', "ens",', na.rm = TRUE)'))
      retained.df <- records.df %>% dplyr::filter(vvt < vvt.q75.df[1,]) %>% dplyr::filter(ens >= ens.day.mean.df[1,])
      records.df <- records.df %>%
        filter(mtime %in% retained.df$mtime)
    }
    #### right part of BA - hypothesis 1
    if(filter.chr == "up10deg"){
      retained.df <- records.df %>% dplyr::filter(tsa > 10)
      records.df <- records.df %>%
        filter(mtime %in% retained.df$mtime)
    }
    if(filter.chr == "low_rad_high_wind"){
      vvt.q75.df <- records.df %>%
        summarise_(.dots = paste0('quantile(', "vvt",', probs= .75, na.rm = TRUE)'))
      retained.df <- records.df %>% dplyr::filter(vvt >= vvt.q75.df[1,]) %>% dplyr::filter(ens == 0)
      records.df <- records.df %>%
        filter(mtime %in% retained.df$mtime)
    }
    if(filter.chr == "low_rad_high_wind_up10"){
      vvt.q75.df <- records.df %>%
        summarise_(.dots = paste0('quantile(', "vvt",', probs= .75, na.rm = TRUE)'))
      retained.df <- records.df %>% dplyr::filter(vvt >= vvt.q75.df[1,]) %>% dplyr::filter(ens == 0) %>% dplyr::filter(tsa > 10)
      records.df <- records.df %>%
        filter(mtime %in% retained.df$mtime)
    }
    #### left part of BA - hypothesis 2
    if(filter.chr == "below10deg"){
      retained.df <- records.df %>% dplyr::filter(tsa <= 10)
      records.df <- records.df %>%
        filter(mtime %in% retained.df$mtime)
    }
    if(filter.chr == "high_rad_high_wind"){
      vvt.q75.df <- records.df %>%
        summarise_(.dots = paste0('quantile(', "vvt",', probs= .75, na.rm = TRUE)'))
      day.records.df <- filter(h.is_it_day(records.df), day==TRUE)
      ens.day.mean.df <- day.records.df %>%
        summarise_(.dots = paste0('mean(', "ens",', na.rm = TRUE)'))
      retained.df <- records.df %>% dplyr::filter(vvt >= vvt.q75.df[1,]) %>% dplyr::filter(ens >= ens.day.mean.df[1,])
      records.df <- records.df %>%
        filter(mtime %in% retained.df$mtime)
    }
    if(filter.chr == "high_rad_high_wind_below10"){
      vvt.q75.df <- records.df %>%
        summarise_(.dots = paste0('quantile(', "vvt",', probs= .75, na.rm = TRUE)'))
      day.records.df <- filter(h.is_it_day(records.df), day==TRUE)
      ens.day.mean.df <- day.records.df %>%
        summarise_(.dots = paste0('mean(', "ens",', na.rm = TRUE)'))
      retained.df <- records.df %>% dplyr::filter(vvt >= vvt.q75.df[1,]) %>% dplyr::filter(ens >= ens.day.mean.df[1,]) %>% dplyr::filter(tsa <= 10)
      records.df <- records.df %>%
        filter(mtime %in% retained.df$mtime)
    }
    #### clearness index
    if(filter.chr == "q70_ci"){
      ci.q70.df <- records.df %>%
        dplyr::filter(ci > 0) %>%
        summarise_(.dots = paste0('quantile(', "ci",', probs= .70, na.rm = TRUE)'))
      retained.df <- records.df %>% dplyr::filter(ci >= ci.q70.df[1,])
      records.df <- records.df %>%
        filter(mtime %in% retained.df$mtime)
    }
    return(records.df)
  }

#+ ---------------------------------
#' ## helper function to transform a records dataframe [from long to wide format](https://www.r-bloggers.com/data-manipulation-with-tidyr/) for the desired sensor
#+ make_wide, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'

  h.make_wide <- function(input_records.df, sensor_name.chr){
    input_records_wide.df <- input_records.df[c("mtime", "sid", sensor_name.chr)] %>% spread_("sid", sensor_name.chr)
  }

#+ ---------------------------------
#' ## helper function to compute the summary statistics for the passed sensor
#+ compute_stats, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'  

  h.compute_stats <- function(input_records.df, sensor_name.chr){
  
    funs <- c("sum", "mean", "min", "max", "sd", "var")
    stats <- paste0("round(", funs, "(", sensor_name.chr,", na.rm=TRUE), 2)")
    names(stats) <- funs
    as.list(stats)
    
    probs <- c("25", "50", "75")
    quants <- paste0("round(quantile(", sensor_name.chr, ",probs=.", probs, ", na.rm=TRUE), 2)")
    names(quants) <- paste0("q", probs)
    as.list(quants)
    
    summarys_stats <- do.call(c, list(stats, quants))
    
    # Compute summary
    summary.df <- input_records.df %>% select_("id", "sid", "poste", sensor_name.chr) %>%
      group_by_("id", "sid", "poste") %>%
      #group_by(.dots=names(input_records.df)[-grep(sensor_name.chr, names(input_records.df))]) %>%
      #group_by_at_(vars(-sensor_name.chr)) %>% 
      summarise_(.dots = summarys_stats) 
    
    # return summary
    return(summary.df)
  }
  
#+ ---------------------------------
#' ## helper function to compute a simple linear model and return its statistics in a dataframe
#+ compute_lm, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'  
  
  compute_lm <- function(records.wide.df, output){
    station1 <- records.wide.df[2]
    station2 <- records.wide.df[3]
    lm.mod <- lm(as_vector(station1)~as_vector(station2))
    lm.mod.sum <- summary(lm.mod)
    lm.mod.sum.df <- broom::tidy(lm.mod.sum)
    lm.mod.df <- glance(lm.mod)
    
    if(output=="lm.sum"){
      return(lm.mod.sum.df)
    }
    if(output=="lm"){
      return(lm.mod.df)
    }
  }

#+ ---------------------------------
#' ## helper function to compute a [Bland-Altman analysis](https://pokyah.github.io/howto/assessing-the-agreement-between-two-quantitative-methods-of-measurements-understanding-the-Bland-Altman-analysis/)
#+ compute_ba, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'  

  h.compute_ba <- function(records.wide.df, output.chr, station1, station2){
    records.wide.df <- records.wide.df %>% mutate(month = as.factor(month(mtime)))
    station1 <- records.wide.df[2] # irm1000
    station2 <- records.wide.df[3] # pameseb61
    
    # compute the stats
    ba_stats.l <- bland.altman.stats(station1[[1]], station2[[1]])
    ba_data.df <- bind_cols(as.data.frame(ba_stats.l$means), as.data.frame(ba_stats.l$diffs))
    ba_data.df <- bind_cols(ba_data.df, records.wide.df["mtime"], records.wide.df["month"] )
    colnames(ba_data.df) <- c("means", "diffs", "mtime", "month")
    
    # build the ba plot
    blandAltman_plot.l <- ggplot(ba_data.df, aes(x=means, y=diffs, color=month)) + 
      geom_point() +
      geom_smooth(method=glm, se=TRUE, color="black", linetype="dashed") +
      geom_hline(yintercept= 0, color = "black", size=0.5) +
      geom_hline(yintercept= ba_stats.l$lines[2], color = "red", size=0.5) +
      geom_hline(yintercept= ba_stats.l$lines[1], color = "blue", size=0.5) +
      geom_hline(yintercept= ba_stats.l$lines[3], color = "blue", size=0.5) +
      geom_hline(yintercept= ba_stats.l$CI.lines[1], linetype="dashed", color = "blue", size=0.5) + 
      geom_hline(yintercept= ba_stats.l$CI.lines[2], linetype="dashed", color = "blue", size=0.5) + 
      geom_hline(yintercept= ba_stats.l$CI.lines[5], linetype="dashed", color = "blue", size=0.5) + 
      geom_hline(yintercept= ba_stats.l$CI.lines[6], linetype="dashed", color = "blue", size=0.5) + 
      geom_hline(yintercept= ba_stats.l$CI.lines[3], linetype="dashed", color = "red", size=0.5) + 
      geom_hline(yintercept= ba_stats.l$CI.lines[4], linetype="dashed", color = "red", size=0.5) +
      scale_color_manual(values= h.ggplot_colours(n=12))
    
    if(output.chr=="plot"){
      return(blandAltman_plot.l)
    }
    if(output.chr=="table"){
      ba_stats.l <- as.data.frame(ba_stats.l[4:length(ba_stats.l)])
      ba_stats.l[-1,c(1,2,3,4,7,8)] <- NA
      return(ba_stats.l)
    }
    if(output.chr=="data"){
      return(ba_data.df)
    }
  }
  
#+ ---------------------------------
#' ## helper function to build commonly used plots
#+ render_plot, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'  

  h.render_plot <- function(records.df, plot.chr, sensor_name.chr){
  
    # draw the frequency plot
    if(plot.chr=="freq"){
      density.plot <- records.df %>%
        ggplot(data=.,
               x = records.df$mtime,
               y = records.df[sensor_name.chr]) +
        aes_string(
          x=sensor_name.chr,
          colour=colnames(records.df)[2]) + 
        geom_density() # +
      # ggtitle(paste0(sensor.chr, "(", duration.chr, ") - ", "density"))
      return(density.plot)
    }
    
    # draw the timeSerie plot
    if(plot.chr=="timeSerie"){
      time.plot <- records.df %>%
        ggplot(data=.,
               x = records.df$mtime,
               y = records.df[sensor_name.chr]) +
        aes_string(
          #x=colnames(records.df)[12],
          x="mtime",
          y=sensor_name.chr,
          colour=colnames(records.df)[2]
          # text = paste(
          #   'station :', records.df[3],
          #   '<br>Date: ', as.Date(records.df$mtime),
          #   '<br>Value: ', records.df[sensor_col.num]
          # )
        ) +
        geom_line() #+
      # ggtitle(paste0(sensor.chr, "(", duration.chr, ") - ", "time"))
      return(time.plot)
    }
    
    # draw the regression plot
    if(plot.chr=="scatter"){
      records.df <- records.df %>% mutate(month = as.factor(month(mtime)))
      # mutate_at(records.df, "mtime", yday)
      # records.df[1] <- apply(records.df[1], 1, yday)
      scatter.plot <- records.df %>%
        ggplot(data=.,
               aes_(x= as.name(names(records.df)[2]),
                   y= as.name(names(records.df)[3]),
                   colour= as.name(names(records.df)[4])
                  )
               ) +
        labs(x = paste(" station ", colnames(records.df[2]), sep= "")) +
        labs(y = paste(" station ", colnames(records.df[3]), sep= "")) + 
        geom_point() +
        geom_smooth(method=lm, color="darkred", fill="blue")
      return(scatter.plot)
    }
  }
  
  #+ ---------------------------------
  #' ## helper function to add a radiation top atmosphere column on the basis of mtime, lon and lat
  #' 
  #+ rad_top_atm, echo=TRUE, warning=FALSE, message=FALSE, error=FALSE, results='asis'
  
  h.rad_top_atm <- function(records.df){
    
    compute_rad_top_atm=function(datetime.dt, lat.num, lon.num){

      RAD2DEG=180./pi
      DEG2RAD=pi/180.
      
      sunrise_sunset=function(lat, lon, DAYCODE){
        lambda=lon*DEG2RAD
        phi=lat*DEG2RAD
        year=floor(as.numeric(DAYCODE)/10000)
        
        #  DAILY SOLAR PARAMETERS
        julian_day = make_julian_day(DAYCODE)
        day_angle = Day_Angle(julian_day)           
        delta = declination_sun(year, julian_day, lambda)              
        time_diff = time_difference_between_LAT_UT(julian_day, lambda)  
        
        #  SUNRISE + SUNSET HOURS (decimal hours in UTC)
        res = sunrise_hour_angle(phi, delta, -1)
        omega_sr=res[1]
        omega_ss=res[2]
        hour_sr=omega_to_LAT(omega_sr)
        hour_ss=omega_to_LAT(omega_ss)
        hour_sr=hour_sr-time_diff
        hour_ss=hour_ss-time_diff  
        return(c(hour_sr, hour_ss))
      }
      
      daylength=function(lat, lon, DAYCODE){
        res=sunrise_sunset(lat, lon, DAYCODE)
        return(res[2]-res[1])
      }
      
      solar_angles=function(lat, lon, DAYCODE, hour){
        year=floor(as.numeric(DAYCODE)/10000)
        lambda=lon*DEG2RAD
        phi=lat*DEG2RAD
        
        #  DAILY SOLAR PARAMETERS
        julian_day = make_julian_day(DAYCODE)
        day_angle = Day_Angle(julian_day)           
        delta = declination_sun(year, julian_day, lambda)              
        time_diff = time_difference_between_LAT_UT(julian_day, lambda)  
        
        #  SOLAR ELEVATION, ZENITHAL ANGLE and AZIMUTHAL ANGLE  (in radian)  
        omega = solar_hour_angle(hour+time_diff)
        res = elevation_zenith_sun(phi, delta, omega)
        gamma=res[1] 
        theta=res[2]
        alpha = azimuth_sun(phi, delta, omega, gamma)
        return(c(gamma, theta, alpha))
      }
      
      E_solar_radiation=function(lat, lon, DAYCODE, hour1, hour2){ #decimal hour in UTC
        lambda=lon*DEG2RAD
        phi=lat*DEG2RAD
        year=floor(as.numeric(DAYCODE)/10000)
        
        #  DAILY SOLAR PARAMETERS
        julian_day = make_julian_day(DAYCODE)
        day_angle = Day_Angle(julian_day)           
        delta = declination_sun(year, julian_day, lambda)              
        time_diff = time_difference_between_LAT_UT(julian_day, lambda)  
        eccentricity=corr_distance(day_angle)
        
        #  TOP-OF-ATMOSPHERE HORIZONTAL SOLAR RADIATION BETWEEN 2 TIMESTAMPS (in Wh/m²)  
        omega1 = solar_hour_angle(hour1+time_diff)
        omega2 = solar_hour_angle(hour2+time_diff)      
        E = G0_general(phi, eccentricity, delta, omega1, omega2)      
        return(E)
      }
      
      make_julian_day=function(DAYCODE){
        #  The procedure "make_julian_day" converts a day given in day, month and year into a julian day.
        #  Outputs :  julian_day : integer day number or julian day (1..366)
        
        tmp = as.Date(DAYCODE, format = "%Y%m%d")
        julian_day=as.numeric(format(tmp, "%j"))
        return(julian_day)
      }
      
      Day_Angle=function(julian_day){
        #  Inputs :   julian_day : integer day number or julian day (1..366) 
        #  Outputs :  day_angle : day angle (in radians) 
        #  The procedure "Day_Angle" expresses the integer day number as an angle (in radians) from 12:00 hours on the day 31st December. A year length of 365.2422 days is used. 
        
        day_angle = julian_day * 2.0 * pi / 365.2422  
        return(day_angle)
      }
      
      declination_sun=function(year_number, julian_day, lambda){
        #    Sources : 
        #    Bourges, B., 1985. Improvement in solar declination computation. Solar 
        #    Energy, 35 (4), 367-369. 
        #    Carvalho, M.J. and Bourges, B., 1986. Program Eufrad 2.0 - User's Guide. 
        #    Project EUFRAT final scientific report, Contract EN3S-0111-F, Solar Energy 
        #    and Development in the European Community, pp. 12.1-12.74.
        #    Duffie, J.A. and Beckman, W.A., 1980. Solar Engineering of Thermal 
        #    Processes. Wiley-Interscience, New York. 
        #    Inputs :
        #    year_number : year number (4 digits)
        #    julian_day  : integer day number or julian day (1..366)
        #    lambda      : longitude (in radians, positive to East) 
        #    Outputs :
        #    delta : solar declination angle at noon (in radians) 
        #    The procedure "declination_sun" computes the solar declination at noon in 
        #    solar time (in radians). A single (average) value per day -at noon- is 
        #    adequate for pratical calculations. The noon declination depends on 
        #    longitude, as noon occurs earlier if longitude is East of Greenwich, and 
        #    later if it is West. The chosen algorithm uses 1957 as base year; it is 
        #    basically a truncated Fourier series with six harmonics.
        
        wt = 0.
        b1 =  0.0064979
        b2 =  0.4059059
        b3 =  0.0020054
        b4 = -0.0029880
        b5 = -0.0132296
        b6 =  0.0063809
        b7 =  0.0003508
        
        #    n0 : spring-equinox time expressed in days from the beginning of the year 
        #     i.e. the time in decimal days elapsing from 00:00 hours Jan 1st to the 
        #     spring equinox at Greenwich in a given year 
        #   t1 : time in days, from the spring equinox 0.5 represents the decimal day number at noon on Jan 1st at Greenwich 
        n0 = 78.8946 + 0.2422*(year_number-1957) - 0.25*(year_number-1957)
        t1 = - 0.5 - lambda / (2 * pi) - n0
        w0 = 2 * pi / 365.2422
        wt = w0 * (julian_day + t1)
        delta = b1 + b2 * sin(wt) + b3 * sin(2 * wt) + b4 * sin(3 * wt) + b5 * cos(wt) + b6 * cos(2 * wt) + b7 * cos(3 * wt)
        return(delta)
      }
      
      geogr_to_geoce=function(phi_g){
        CC=0.99330552        # Correction factor for converting geographic 
        # into geocentric latitude. CC=(Rpole/Requator)**2 
        # Rpole=6356.752, Requator=6378.137 
        if((phi_g >= -(pi/2.0-0.0002)) | (phi_g <= (pi/2.0-0.0002))){
          phi=atan(tan(phi_g)*CC)
        } else {
          phi=phi_g
        }
        return(phi)
      }
      
      sunrise_hour_angle=function(phi_g, delta, gamma_riset){
        #  Source : 
        #  Inputs :
        #    phi_g       : latitude of site (in radians, positive to North)
        #    delta       : solar declination angle (in radians)
        #    gamma_riset : solar elevation near sunrise/sunset:
        #                  - set to  0.0 for astronomical sunrise/sunset
        #          - set to -1.0 for refraction corrected sunrise/sunset. 
        #  Outputs :
        #    omega_sr : sunrise solar hour angle (in radians)
        #    omega_ss : sunset solar hour angle (in radians) 
        #  The procedure "sunrise_hour_angle" supplies the sunrise and sunset hour 
        #    angles (in radians). Due to the dimension of the solar disk and the effect 
        #    of the atmospheric refraction, the edge of the solar disk will just appear 
        #    (disappear) at the horizon at sunrise (at sunset) when the calculated 
        #    astronomical elevation is 50'. 
        
        cos_omegas = 0.
        omegas = 0.
        
        horizon = (-50.0 / 60.0) * DEG2RAD  # horizon, -50' in radians 
        if(gamma_riset >= horizon){
          horizon = gamma_riset
        }
        
        phi=geogr_to_geoce(phi_g)
        max_delta = 23.45 * DEG2RAD
        if ( (abs(phi) < (pi/2.0)) & (abs(delta) <= max_delta)){
          cos_omegas = (sin(horizon) - (sin(phi) * sin(delta))) / (cos(phi) * cos(delta))
        } 
        
        if(abs(cos_omegas) < 1.0){
          omegas = acos(cos_omegas)
        }
        if(cos_omegas >= 1.0){  # the sun is always below the horizon : polar night 
          omegas = 0.0
        }
        if(cos_omegas <= -1.0) {# the sun is always above the horizon : polar day 
          omegas = pi
        }
        
        omega_sr = -omegas
        omega_ss =  omegas
        return(c(omega_sr, omega_ss))
      }
      
      time_difference_between_LAT_UT=function(julian_day, lambda) {
        ier = 1
        a1 = -0.128
        a2 = -0.165
        a3 =  2.80 * DEG2RAD
        a4 = 19.70 * DEG2RAD
        
        day_angle = Day_Angle(julian_day)
        ET = a1 * sin(day_angle - a3) + a2 * sin(2.0 * day_angle + a4) # in decimal hours
        time_diff=ET + (lambda * 12.0 / pi) # in decimal hour
        #LAT = UT + time_diff
        return(time_diff)
      }
      
      omega_to_LAT=function(omega){
        #  Source :
        #  Inputs :
        #    omega : solar hour angle (in radians) 
        #  Outputs :
        #    t : solar time i.e. LAT (0..24 decimal hours) 
        #  The procedure "omega_to_LAT" does the reverse operation of the procedure 
        #    "solar_hour_angle" i.e. computes the solar time (in decimal hours) from the 
        #    solar hour angle (in radians). 
        
        t = 12.0 * (1.0 + omega / pi)
        return(t)
      }
      
      solar_hour_angle=function(t){
        #  Source : 
        #  Inputs :
        #    t : solar time i.e. LAT (0..24 decimal hours) 
        #  Outputs :
        #    omega : solar hour angle (in radians) 
        #  The procedure "solar_hour_angle" supplies the solar hour angle (in radians).
        #    By convention the hour angle is negative before noon and positive after noon
        omega = (t - 12.0) * pi / 12.0
        return(omega)
      }
      
      elevation_zenith_sun=function(phi_g, delta, omega){
        #  Source : 
        #  Inputs :
        #    phi_g : latitude of site (in radians, positive to North)
        #    delta : solar declination angle (in radians)
        #    omega : solar hour angle (in radians) 
        #  Outputs :
        #    gamma : solar altitude angle (in radians)
        #    theta : solar zenithal angle (in radians) 
        #  The procedure "elevation_zenith_sun" computes the solar elevation (or 
        #    altitude) angle and the solar zenithal (or incidence) angle. These two 
        #    angles are complementary. 
        
        phi=geogr_to_geoce(phi_g)
        res = sunrise_hour_angle(phi_g,delta,0.0)
        omega_sr=res[1]
        omega_ss=res[2]
        if((omega < omega_sr) || (omega > omega_ss)){
          gamma = 0.0
        } else {
          gamma = asin( sin(phi) * sin(delta) + cos(phi) * cos(delta) * cos(omega) )
        } 
        if (gamma < 0.0){
          gamma = 0.0
        }
        theta = (pi / 2.0) - gamma
        return(c(gamma, theta))
      }
      
      azimuth_sun=function(phi_g, delta, omega, gamma){
        #  Source : 
        #  Inputs :
        #    phi_g : latitude of site (in radians, positive to North)
        #    delta : solar declination angle (in radians)
        #    omega : solar hour angle (in radians)
        #    gamma : solar altitude angle (in radians) 
        #  Outputs :
        #    alpha : solar azimuthal angle (in radians) 
        #  The procedure "azimuth_sun" computes the solar azimuth angle in the Northern
        #    hemisphere. The azimuth angle has a positive value when the sun is to the 
        #    west of South, i.e. during the afternoon in solar time. For the Southern 
        #    hemisphere, the azimuth angle is measured from North.
        
        phi=geogr_to_geoce(phi_g)
        cos_as = (sin(phi) * sin(gamma) - sin(delta)) / (cos(phi) * cos(gamma))
        if(phi < 0.0) cos_as = -cos_as  #  Southern hemisphere 
        sin_as = cos(delta) * sin(omega) / cos(gamma)
        x = acos(cos_as)
        if(sin_as >= 0.0){
          alpha =  x
        } else {
          alpha = -x
        }
        return(alpha)
      }
      
      corr_distance=function(day_angle){
        #  Source : Gruter (ed.) (1984) 
        #  Inputs :
        #    day_angle : day angle (in radians) 
        #  Outputs :
        #    eccentricity : correction for Earth orbit eccentricity 
        #  The procedure "corr_distance" computes the correction for the variation of 
        #    sun-earth distance from its mean value (also known as eccentricity). It is a
        #    fucntion of time, but a single (average) value per day is enough for 
        #    practical calculations.  
        
        a = 2.80 * DEG2RAD
        eccentricity = 1.0 + 0.03344 * cos(day_angle - a) 
        return(eccentricity)
      }
      
      G0_general=function(phi_g, eccentricity, delta, omega1, omega2){
        #  Source : 
        #  Inputs :
        #    phi_g        : latitude of site (in radians, positive to North)
        #    eccentricity : correction for Earth orbit eccentricity
        #    delta        : solar declination angle (in radians)
        #    omega1       : solar hour angle at beginning of the period (in radians)
        #    omega2       : solar hour angle at end of the period (in radians) 
        #  Outputs :
        #    G0_12 : extraterrestrial solar irradiation (in Wh/m2) 
        #  The procedure "G0_general" delivers the extraterrestrial solar irradiation 
        #    incident on an horizontal surface in the general case (in Wh/m2).
        
        I0=1367.0 # solar constant in W/m2 
        Dl=24.0  #average value for the length of the day in decimal hours 
        
        phi=geogr_to_geoce(phi_g)
        res = sunrise_hour_angle(phi_g,delta,0.0)
        omega_sr=res[1]
        omega_ss=res[2]
        if(omega1 < omega_sr){
          omega1 = omega_sr
        }
        if(omega2 < omega_sr){
          omega2 = omega_sr
        }
        if(omega1 > omega_ss){
          omega1 = omega_ss
        }
        if(omega2 > omega_ss){
          omega2 = omega_ss
        }
        
        if(omega2 <= omega1){
          G0_12 = 0.0
        } else {  
          a = I0 * eccentricity * Dl / (2.0 * pi)
          b1 = sin(phi) * sin(delta) * (omega2 - omega1)
          b2 = cos(phi) * cos(delta) * (sin(omega2) - sin(omega1))
          c = a * (b1 + b2)
          if(c < 0.0){
            G0_12 = 0.0
          } else {
            G0_12 = c
          }
        }
        return(G0_12)
      }
      
      #  DEFINE DAY AND LOCATION 
      DAYCODE = strftime( records.df$mtime[17], format="%Y%m%d") 
        #gsub('-', '', as.character(datetime.dt))
        #strftime(datetime.dt, format="%Y%m%d%h%m%s")
      lat=lat.num
      lon=lon.num
      
      year= floor(as.numeric(DAYCODE)/10000) #lubridate::year(datetime.dt) 
      lambda=lon*DEG2RAD
      phi=lat*DEG2RAD
      
      #  DAILY SOLAR PARAMETERS
      julian_day = make_julian_day(DAYCODE)
      day_angle = Day_Angle(julian_day)           
      delta = declination_sun(year, julian_day, lambda)              
      time_diff = time_difference_between_LAT_UT(julian_day, lambda)  
      
      #  SUNRISE + SUNSET HOURS (decimal hours in UTC)
      res = sunrise_hour_angle(phi, delta, -1)
      omega_sr=res[1]
      omega_ss=res[2]
      hour_sr=omega_to_LAT(omega_sr)
      hour_ss=omega_to_LAT(omega_ss)
      hour_sr=hour_sr-time_diff
      hour_ss=hour_ss-time_diff  
 
      #  SOLAR ELEVATION, ZENITHAL ANGLE and AZIMUTHAL ANGLE  (in radian)  
      hour= as.numeric(strftime(datetime.dt, format="%H")) # 12.5;  #decimal hour in UTC
      omega = solar_hour_angle(hour+time_diff)
      res = elevation_zenith_sun(phi, delta, omega)
      gamma=res[1] 
      theta=res[2]
      alpha = azimuth_sun(phi, delta, omega, gamma)
      
      #  TOP-OF-ATMOSPHERE HORIZONTAL SOLAR RADIATION BETWEEN 2 TIMESTAMPS (in Wh/m²)  
      hour1= as.numeric(strftime(datetime.dt, format="%H")) #12.5  #decimal hour in UTC
      hour2= as.numeric(strftime(datetime.dt, format="%H"))+1 # 14.5  #decimal hour in UTC
      omega1 = solar_hour_angle(hour1+time_diff)
      omega2 = solar_hour_angle(hour2+time_diff)      
      eccentricity=corr_distance(day_angle)
      E = G0_general(phi, eccentricity, delta, omega1, omega2)      
      return(E)
    }
    
    # add a column rad_top_atm
    records.df <- records.df %>%
      rowwise() %>%
      mutate(rad_top_atm=compute_rad_top_atm(datetime.dt=mtime, lat.num=latitude, lon.num=longitude))
    
    # add a column with the clearness index
    records.df <- records.df %>%
      mutate(ci=ens/rad_top_atm) %>%
      mutate_at(.vars = vars(ci), .funs = funs(ifelse(is.na(.), 0, .))) %>% #replacing NA by 0
      mutate_at(.vars = vars(ci), .funs = funs(ifelse(.>1, 1, .))) #replacing values larger than 1 by 1
    
    # return the dataframe containing the rad_top_atm and ci columns 
    return(records.df)
  }

