load_data <- function() {
  north <- read.csv("data/contents",
                    header = T,
                    fill = T,
                    stringsAsFactors = F)
  north$direction <- "north"
  south <- read.csv("data/AllSouthboundRevised.csv",
                    header = T,
                    fill = T,
                    stringsAsFactors = F)
  south$direction <- "south"
  names(south)[1:2] <- names(north)[1:2]
  d <- rbind(north, south)

  d$CRASHTIME <- as.character(d$CRASHTIME)
  library(stringr)
  d$CRASHTIME <- str_pad(d$CRASHTIME, 4, pad = "0")
  d$time <- as.POSIXct(
    format(as.POSIXct(paste(d$CRASHDATE, d$CRASHTIME),
                      format = "%m/%d/%Y %H%M"))
  )

  d <- d[!is.na(d$time), ]
  names(d) <- tolower(names(d))
  d$time <- as.character(d$time)
  d$cntofpers[d$cntofpers == "" | d$cntofpers == " "] <- NA
  d$cntoffatl[d$cntoffatl == "" | d$cntoffatl == " "] <- NA
  d$cntofsvinj[d$cntofsvinj == "" | d$cntofsvinj == " "] <- NA
  d$cntoffatl <- as.numeric(d$cntoffatl)
  d$cntofsvinj <- as.numeric(d$cntofsvinj)
  # BOOL of if there's a svinj or fatal
  d$fatal <- d$cntoffatl > 0
  d$sev_inj <- d$cntofsvinj > 0
  d <- d[!is.na(d$latitude) & !is.na(d$longitude), ]

  return(tbl_df(d))
}

make_json_output <- function(d) {
  toGeoJSON(data = d,
            name = "crashes",
            lat.lon = which(names(d) %in%
                              c("latitude", "longitude")),
            dest = getwd()
  )
}

new_tropic_theme <- function (base_size = 12, base_family = "sans")
{
  (theme_foundation(base_size = base_size, base_family = base_family) +
     theme(line = element_line(), rect = element_rect(fill = ggthemes_data$fivethirtyeight["ltgray"],
                                                      linetype = 0, colour = NA), text = element_text(colour = ggthemes_data$fivethirtyeight["dkgray"]),
           axis.text = element_text(),
           axis.ticks = element_blank(), axis.line = element_blank(),
           legend.background = element_rect(), legend.position = "bottom",
           legend.direction = "horizontal", legend.box = "vertical",
           panel.grid = element_line(colour = NULL), panel.grid.major = element_line(colour = ggthemes_data$fivethirtyeight["medgray"]),
           panel.grid.minor = element_blank(), plot.title = element_text(hjust = 0,
                                                                         size = rel(1.5), face = "bold"), plot.margin = unit(c(1,
                                                                                                                               1, 1, 1), "lines"), strip.background = element_rect()))
}
