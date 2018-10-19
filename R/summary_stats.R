library(LDATS)
data(rodents)
rem <- which(colnames(rodents) %in% c("newmoon", "date", "plots", "traps"))
obs <- rodents[,-rem]
dct <- data.frame(newmoon = rodents[,"newmoon"])

head(obs)
timename <- "newmoon"
timevar <- dct[ , timename]

data = obs
time_variable = timevar

ts_summary <- function(data, time_variable = NULL){
  if (is.null(time_variable)){
    time_variable <- 1:nrow(data)
  }
  if(length(time_variable) != nrow(data)){
    stop("time variable and data set not of same length")
  }

  nobs <- nrow(data)
  time_range <- range(time_variable)
  auto_cor <- temp_autocor(data, time_variable)
  
}

# lag at 1, 2
# n steps to get acf < 1/e

auto_cor <- function(data, time_variable, interp_method = "na.interp"){
  interp_data <- interpolate_data(data, time_variable, interp_method)
  acf(interp_data, plot = FALSE)

}

na.interp <- function(x){
  forecast::na.interp(x)
}

interpolate_data <- function(data, time_variable, 
                             method = "na.interp"){

  time_diff <- diff(time_variable)
  times <- min(time_variable):max(time_variable)
  ntimes <- length(times)
  nspp <- ncol(data)
  out <- data.frame(matrix(NA, nrow = ntimes, ncol = nspp))
  colnames(out) <- colnames(data)
  for (i in 1:ntimes){
    time_match <- which(times == time_variable[i])
    out[time_match, ] <- data[i, ]
  }
  for (i in 1:nspp){
    out[ , i] <- do.call(method, list(out[ , i]))
    out[ , i] <- round(out[ , i], 0)
  }
  out
}


# things to return
# number of observations
# time range
# summary of observations: range, mean, median, sd; for species and total

summarize_obervations <- function(data){



}

