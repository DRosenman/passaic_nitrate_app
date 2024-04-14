plot_tmdl_cross_data <- function(df) {
  if(ncol(df) == 5L) {
    parameter_x <- names(df)[[4]]
    parameter_y <- names(df)[[5]]
    names(df)[[4]] <- 'x'
    names(df)[[5]] <- 'y'
    if(all(c("max", "min") %in% df$Stat)) {
      ggplot(data = df) + 
        geom_point(aes(x, y, color = Stat)) +
        scale_colour_manual(name = '', values = c("max" = "blue", "min" = "red"), labels = c("Max", "Min"))  +
        xlab(parameter_x) +
        ylab(parameter_y) +
        background_grid(major = "xy", minor = "xy")
    } else {
      stat <- df$Stat[[1]]
      label_x <- glue::glue("{parameter_x} ({stat})")
      label_y <- glue::glue("{parameter_y} ({stat})")
      ggplot(data = df) + 
        geom_point(aes(x, y)) +
        xlab(label_x) +
        ylab(label_y) +
        background_grid(major = "xy", minor = "xy")
      
    }
  } else {
    parameter <- names(df)[[4]]
    names(df)[[4]] <- 'x'
    
    if(all(c("max", "min") %in% df$Stat)) {
      ggplot(data = df) + 
        geom_point(aes(x, x, color = Stat)) +
        scale_colour_manual(name = '', values = c("max" = "blue", "min" = "red"), labels = c("Max", "Min"))  +
        xlab(parameter) +
        ylab(parameter) +
        background_grid(major = "xy", minor = "xy")
    } else {
      stat <- df$Stat[[1]]
      label_x <- glue::glue("{parameter} ({stat})")
      ggplot(data = df) + 
        geom_point(aes(x, x)) +
        xlab(label_x) +
        ylab(label_x) +
        background_grid(major = "xy", minor = "xy")
      
    }
    
  }
  
  
  
  
  
}