sample_parameter_plot <- function(parameter, event_df) {
  df <- event_df %>% filter(Parameter == parameter) %>% 
    mutate(`Station Name` = str_c(`Station Name`, " (", `TMDL ID`, ")"))
  ggplot(df, aes(x = Interval, y = Result, color = `Station Name`)) + 
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = 1:24) +
    ylab(parameter) +
    background_grid(major = "xy", minor = "xy")
}


overall_sample_parameter_plot <- function(parameter, df) {
  df <- df %>% filter(Parameter == parameter) %>% 
    mutate(`Station Name` = if_else(is.na(`TMDL ID`), `Station Name`, str_c(`Station Name`, " (", `TMDL ID`, ")")),
           Date = case_when(Date %in% c(ymd("2018-06-12"), ymd("2018-06-13"), ymd("2018-06-14")) ~Date,
                            Date == "2018-06-26" ~ ymd("2018-06-15"),
                            Date == "2018-06-27" ~ ymd("2018-06-16"),
                            Date == "2018-06-28" ~ ymd("2018-06-17"),
                            Date == "2018-07-10" ~ ymd("2018-06-18"),
                            Date == "2018-07-11" ~ ymd("2018-06-19"),
                            Date == "2018-07-12" ~ ymd("2018-06-20"),
                            Date == "2018-08-07" ~ ymd("2018-06-21"),
                            Date == "2018-08-08" ~ ymd("2018-06-22"),
                            Date == "2018-08-09" ~ ymd("2018-06-23")))
  #return(df)
  if (parameter %ni% c("Chl a (ug/L)", "NH3 (mg/L)", "TSS  (mg/L)")) {
    max_discharger <- max(df %>% filter(Type == "Discharger") %>% pull(Result), na.rm = TRUE)
    max_ambient <- max(df %>% filter(Type == "Ambient") %>% pull(Result), na.rm = TRUE)
    df <- df %>% mutate(   
      Result = if_else(Type == "Discharger", Result * max_ambient / max_discharger, Result)
    )
  }
  day(df$Datetime) <- day(df$Date)
  month(df$Datetime) <- month(df$Date)
  
  plot <- ggplot(df, aes(x = Datetime, y = Result, color = `Station Name`, shape = Type)) +
    geom_point(aes(size = Type)) + 
    ylab(str_c(parameter, " (Ambient)")) + 
    xlab("Date") + 
    background_grid(major = "xy", minor = "xy") +
    scale_x_datetime(limits = c(ymd_hms("2018-06-12 00:00:00"), ymd_hms("2018-06-24 00:00:00")),
                     breaks = c(ymd_hms("2018-06-12 00:00:00"),ymd_hms("2018-06-13 00:00:00"),
                                ymd_hms("2018-06-14 00:00:00"),ymd_hms("2018-06-15 00:00:00"),
                                ymd_hms("2018-06-16 00:00:00"),
                                ymd_hms("2018-06-17 00:00:00"),
                                ymd_hms("2018-06-18 00:00:00"),
                                ymd_hms("2018-06-19 00:00:00"),
                                ymd_hms("2018-06-20 00:00:00"),
                                ymd_hms("2018-06-21 00:00:00"),
                                ymd_hms("2018-06-22 00:00:00"),
                                ymd_hms("2018-06-23 00:00:00")),
                     labels = c("Jun 12", "Jun 13", "Jun 14", "Jun 26", "Jun 27", "Jun 28", "Jul 10", "Jul 11", "Jul 12", "Aug 07", "Aug 08", "Aug 09")) 
  if (parameter %ni% c("Chl a (ug/L)", "NH3 (mg/L)", "TSS  (mg/L)")) {
    plot <- plot + scale_y_continuous(sec.axis = sec_axis(~. * max_discharger / max_ambient  , name = str_c(parameter, ( " Discharger")))) +
      scale_size_manual(values = c("Ambient" = 1, "Discharger" = 4), breaks = c("Ambient", "Discharger"))
  }
  
  plot
  
  
  
}



overall_sample_parameter_plot2 <- function(parameter, df) {
  df <- df %>% filter(Parameter == parameter) %>% 
    mutate(`Station Name` = if_else(is.na(`TMDL ID`), `Station Name`, str_c(`Station Name`, " (", `TMDL ID`, ")")),
           Date = case_when(Date %in% c(ymd("2018-06-12"), ymd("2018-06-13"), ymd("2018-06-14")) ~Date,
                            Date == "2018-06-26" ~ ymd("2018-06-15"),
                            Date == "2018-06-27" ~ ymd("2018-06-16"),
                            Date == "2018-06-28" ~ ymd("2018-06-17"),
                            Date == "2018-07-10" ~ ymd("2018-06-18"),
                            Date == "2018-07-11" ~ ymd("2018-06-19"),
                            Date == "2018-07-12" ~ ymd("2018-06-20"),
                            Date == "2018-08-07" ~ ymd("2018-06-21"),
                            Date == "2018-08-08" ~ ymd("2018-06-22"),
                            Date == "2018-08-09" ~ ymd("2018-06-23")))
  #return(df)
  if (parameter %ni% c("Chl a (ug/L)", "NH3 (mg/L)", "TSS  (mg/L)")) {
    max_discharger <- max(df %>% filter(Type == "Discharger") %>% pull(Result), na.rm = TRUE)
    max_ambient <- max(df %>% filter(Type == "Ambient") %>% pull(Result), na.rm = TRUE)
    df <- df %>% mutate(   
      Result = if_else(Type == "Discharger", Result * max_ambient / max_discharger, Result)
    )
  }
  day(df$Datetime) <- day(df$Date)
  month(df$Datetime) <- month(df$Date)
  
  plot <- ggplot(df, aes(x = Datetime, y = Result, color = `Station Name`, shape = Type)) +
    geom_point(aes(size = Type)) + 
    background_grid(major = "xy", minor = "xy") +
    scale_x_datetime(limits = c(ymd_hms("2018-06-12 00:00:00"), ymd_hms("2018-06-24 00:00:00")),
                     breaks = c(ymd_hms("2018-06-12 00:00:00"),ymd_hms("2018-06-13 00:00:00"),
                                ymd_hms("2018-06-14 00:00:00"),ymd_hms("2018-06-15 00:00:00"),
                                ymd_hms("2018-06-16 00:00:00"),
                                ymd_hms("2018-06-17 00:00:00"),
                                ymd_hms("2018-06-18 00:00:00"),
                                ymd_hms("2018-06-19 00:00:00"),
                                ymd_hms("2018-06-20 00:00:00"),
                                ymd_hms("2018-06-21 00:00:00"),
                                ymd_hms("2018-06-22 00:00:00"),
                                ymd_hms("2018-06-23 00:00:00")),
                     labels = c("Jun 12", "Jun 13", "Jun 14", "Jun 26", "Jun 27", "Jun 28", "Jul 10", "Jul 11", "Jul 12", "Aug 07", "Aug 08", "Aug 09")) 
  if (parameter %ni% c("Chl a (ug/L)", "NH3 (mg/L)", "TSS  (mg/L)")) {
    plot <- plot + scale_y_continuous(sec.axis = sec_axis(~. * max_discharger / max_ambient  , name = str_c(parameter, ( " Discharger")))) +
      scale_size_manual(values = c("Ambient" = 1, "Discharger" = 4), breaks = c("Ambient", "Discharger"))
  }
  
  plot + labs(x = 'Date',
              y = str_c(parameter, " (Ambient)")
  )
              
  
  
  
}