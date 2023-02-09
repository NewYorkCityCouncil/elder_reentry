library(tidyverse)
library(lubridate)
library(data.table)

l_files <- list.files("/home/brooke/doc_daily_census", pattern = ".csv", full.names = TRUE)
# l_files <- paste("/home/brooke/doc_daily_census/", l_files, sep = "")
l_files <- sapply(l_files, fread, simplify = FALSE)

data_doc <- rbindlist(l_files, idcol="id")


# clean dates
data_doc[, id := trimws(id)]
data_doc[, id:= gsub("  ", " ", id)]
data_doc[, id := gsub(".csv", "", id, fixed = TRUE)]

data_doc[, month := sapply(strsplit(id, " "), "[", 3)]
data_doc[, day := sapply(strsplit(id, " "), "[", 4)]
data_doc[, year := sapply(strsplit(id, " "), "[", 6)]
data_doc[, m := recode(month, "Jan" = 1, "Feb" = 2, "Mar" = 3, 
                   "Apr" = 4, "May" = 5, "Jun" = 6, "Jul" = 7, 
                   "Aug" = 8, "Sep" = 9, "Oct" = 10, "Nov" = 11,
                   "Dec" = 12)]


data_doc[, date_ := paste(m, day, year, sep = "-")]
data_doc[, date_ := as.Date(date_, format = "%m-%d-%Y")]
data_doc[,id:= NULL]
data_doc[, admitted_dt:= as.Date(admitted_dt)]

setorder(data_doc, date_, inmateid)

# look for people who have more than 1 admitted date, save those ids
subdt <- data_doc[,.(admitted_dt, inmateid)]
subdt <- unique(subdt)

recids <- subdt[, .N, by = "inmateid"][order(N, decreasing=TRUE)][N>1]
recids_doc <- data_doc[inmateid %in% recids$inmateid, .(inmateid, inmate_status_code, top_charge, date_, admitted_dt)]

recids_doc <- unique(recids_doc)




