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
      records.df <- is_it_day(records.df) %>% dplyr::filter(day==TRUE)
    }
    if(filter.chr == "night_only"){
      records.df <- is_it_day(records.df) %>% dplyr::filter(day==FALSE)
    }
    if(filter.chr == "high_rad_low_wind"){
      vvt.mean.df <- records.df %>%
        summarise_(.dots = paste0('mean(', "vvt",', na.rm = TRUE)'))
      day.records.df <- filter(is_it_day(records.df), day==TRUE)
      ens.day.mean.df <- day.records.df %>%
        summarise_(.dots = paste0('mean(', "ens",', na.rm = TRUE)'))
      retained.df <- records.df %>% dplyr::filter(vvt < vvt.mean.df[1,]) %>% dplyr::filter(ens >= ens.day.mean.df[1,])
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
    ba_stats.df <- bland.altman.stats(station2[[1]], station1[[1]])
    ba_data.df <- bind_cols(as.data.frame(ba_stats.df$means), as.data.frame(ba_stats.df$diffs))
    ba_data.df <- bind_cols(ba_data.df, records.wide.df["mtime"], records.wide.df["month"] )
    colnames(ba_data.df) <- c("means", "diffs", "mtime", "month")
    
    # build the ba plot
    blandAltman_plot.l <- ggplot(ba_data.df, aes(x=means, y=diffs, color=month)) + 
      geom_point() +
      geom_smooth(method=glm, se=TRUE, color="black", linetype="dashed") +
      geom_hline(yintercept= 0, color = "black", size=0.5) +
      geom_hline(yintercept= ba_stats.df$lines[2], color = "red", size=0.5) +
      geom_hline(yintercept= ba_stats.df$lines[1], color = "blue", size=0.5) +
      geom_hline(yintercept= ba_stats.df$lines[3], color = "blue", size=0.5) +
      geom_hline(yintercept= ba_stats.df$CI.lines[1], linetype="dashed", color = "blue", size=0.5) + 
      geom_hline(yintercept= ba_stats.df$CI.lines[2], linetype="dashed", color = "blue", size=0.5) + 
      geom_hline(yintercept= ba_stats.df$CI.lines[5], linetype="dashed", color = "blue", size=0.5) + 
      geom_hline(yintercept= ba_stats.df$CI.lines[6], linetype="dashed", color = "blue", size=0.5) + 
      geom_hline(yintercept= ba_stats.df$CI.lines[3], linetype="dashed", color = "red", size=0.5) + 
      geom_hline(yintercept= ba_stats.df$CI.lines[4], linetype="dashed", color = "red", size=0.5) +
      scale_color_manual(values= ggplotColours(n=12))
    
    if(output.chr=="plot"){
      return(blandAltman_plot.l)
    }
    if(output.chr=="table"){
      ba_stats.df <- as.data.frame(ba_stats.df[4:length(ba_stats.df)])
      ba_stats.df[-1,c(1,2,3,4,7,8)] <- NA
      return(ba_stats.df)
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

