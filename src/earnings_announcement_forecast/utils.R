
library(tidyverse)
library(lubridate)
library(rvest)
library(RSelenium)
library(netstat)
library(jsonlite)


event_vol <- function(nonevent_vol, iv, dte){
  value = sqrt(iv ^ 2 * dte - nonevent_vol ^ 2 * (dte - 1))
  return(value)
}

event_move <- function(event_vol){
  value = sqrt(2/(365*pi)) * event_vol
  return(value)
}