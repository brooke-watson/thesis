#' getseason
#'
#' This function encodes a season variable based on dates of data collection in the study.
#' @param d list of dates
#' @return character vector containing 1 of 3 values: 1 (Rainy season), 0 (Dry season), or
#' "Outside study population"
#' @keywords time
#' @export
#' @examples
#' getseason(c("2015-7-01 00:00:10", "2015-12-01 00:00:00", "2012-12-01 01:00:00"))

getseason <- function(d) {
  install.packages("lubridate", repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
  library(lubridate)
  start = ymd_hms("2014-12-01 00:00:00") # start of study collection
  rainstart1 = ymd_hms("2015-05-01 00:00:00") # start of first rainy (summer) season in study
  drystart2 = ymd_hms("2015-12-01 00:00:00") # start of second dry season
  rainstart2 = ymd_hms("2016-05-01 00:00:00") # start of second rainy season
  end = ymd_hms("2016-08-01 00:00:00") # end of study collection

  ifelse (d >= start & d < rainstart1 | d >= drystart2 & d < rainstart2, 0,
          ifelse( d >= rainstart1 & d < drystart2 | d >= rainstart2 & d < end, 1,
          "Outside Study Population"))

  # if it's during the dry season, label "0"
  # during the rainy season, label "1"
  # if it's outside the study collection date range, label "Outside Study Population"

  # original stata source code:
  #------------------------------------------------------------------------------------
  # *generate season variable
  # gen season = doe
  # display td(1feb2015)
  # display td(30apr2015)
  # display td(1may2015)
  # display td(30nov2015)
  # display td(30apr2016)
  # recode season 20120/20208=1 20209/20422=0 20423/20574=1 19360=0
  # tab season
  # label define season 1 "Rainy/Summer: December-April" 0 "Dry/Winter: May-November"
  # label values season season
}
