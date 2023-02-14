library(lubridate)
library(data.table)
library(dplyr)
library(ggplot2)

l_files <- list.files("/home/brooke/doc_daily_census", pattern = ".csv", full.names = TRUE)
# l_files <- paste("/home/brooke/doc_daily_census/", l_files, sep = "")
l_files <- sapply(l_files, fread, simplify = FALSE)

data_doc <- rbindlist(l_files, idcol="id")

# clean dates
data_doc[, id := trimws(id)]
data_doc[, id:= gsub("  ", " ", id)]
data_doc[, id := gsub(".csv", "", id, fixed = TRUE)]

data_doc[, month_ := sapply(strsplit(id, " "), "[", 3)]
data_doc[, day := sapply(strsplit(id, " "), "[", 4)]
data_doc[, year := sapply(strsplit(id, " "), "[", 6)]
data_doc[, m := recode(month_, "Jan" = 1, "Feb" = 2, "Mar" = 3, 
                   "Apr" = 4, "May" = 5, "Jun" = 6, "Jul" = 7, 
                   "Aug" = 8, "Sep" = 9, "Oct" = 10, "Nov" = 11,
                   "Dec" = 12)]

data_doc[, date_ := paste(m, day, year, sep = "-")]
data_doc[, date_ := as.Date(date_, format = "%m-%d-%Y")]
data_doc[,id:= NULL]
data_doc[, admitted_dt:= as.Date(admitted_dt)]
data_doc[, `50 Years and Over` := ifelse(age>=50, TRUE,FALSE), by = "admitted_dt"]

# subset to data we can trust because we dont know about turnover before the chron job, so admits after the date_pulled
date_doc <- data_doc[admitted_dt>=date_, ]
data_doc[, year_adm := year(admitted_dt)]

#### **** TOTAL POPULATION
# let's look at n per census date (total pop at that day) + broken down by age

data_doc[, n_per_day_date := .N, by = "date_"]
data_doc[, n_per_day_age_date := .N, by =c("date_", "50 Years and Over")]

# let's look number admitted 
# n per day admit date - brain fog not sure if this is the right approach, dont think it can be trusted before date
plot_dt_adm <- data_doc[, .(n_per_day_age_adm = .N), by =c("admitted_dt", "50 Years and Over")]

ggplot(plot_dt_adm[!is.na(`50 Years and Over`),],  aes(x=admitted_dt, y = n_per_day_age_adm, group=`50 Years and Over`, color=`50 Years and Over`)) +
theme_bw()  + geom_smooth(se=FALSE) + xlab("Date of Admission") + ylab("Number per Day")

# n per day population by date_ (date of census)
plot_dt_dage <- unique(data_doc[, .(date_, n_per_day_age_date, n_per_day_date,`50 Years and Over`)])

# (plot_dt_dage, "data/n_per_day_age_pop.csv")
ggplot(plot_dt_dage[!is.na( `50 Years and Over`),],  aes(x=date_, y = n_per_day_age_date, group=`50 Years and Over`, color=`50 Years and Over`)) + 
  theme_bw()  + geom_smooth(se=FALSE) + xlab("Date") + ylab("Number in Rikers that Day")

# subset to separate once vs recids 
subdt <- data_doc[,.(admitted_dt, inmateid)]
subdt <- unique(subdt)

once_rikers <- subdt[, .N, by = "inmateid"][order(N, decreasing=TRUE)][N==1]
once_rikers_dt<- data_doc[inmateid %in% once_rikers$inmateid, ]
once_rikers_dt[, approx_release_date:= max(date_), by = "inmateid"]

# length of time for folks who have been in rikers once 
once_rikers_dt_lot <- unique(once_rikers_dt[])

# recids - trickier
recids <- subdt[, .N, by = "inmateid"][order(N, decreasing=TRUE)][N>1]
recids_doc <- data_doc[inmateid %in% recids$inmateid, .(inmateid, inmate_status_code, top_charge, date_, admitted_dt, age)]
recids_doc <- unique(recids_doc)

# people who have been in once
# make sure order is in tact
setorder(recids_doc, "admitted_dt", "inmateid")
recids_doc_sub <- recids_doc[,.(admitted_dt, inmateid)]
recids_doc_sub <- unique(recids_doc_sub)

# count/rank the order of admit by date
recids_doc_sub[, order_adm_times := order(admitted_dt), by = "inmateid"]
recids_2 <- merge(recids_doc, recids_doc_sub, by = c("admitted_dt", "inmateid"))
setorder(recids_doc, "date_", "inmateid")

# split by rank and id and then grab the last record
list_inmate_id <- split(recids_2, by=c("inmateid", "order_adm_times"))
inmate_grp_dates <- lapply(list_inmate_id, function(x) tail(x, n=1))
inmate_grp_dates_dt <- rbindlist(inmate_grp_dates)

# check if the last date_ in the set is the last date before next admit
# change names
# order_adm_times is the first, second, etc time of admit for an id
setnames(inmate_grp_dates_dt, "date_", "release_date_approx")
# only take the approx release dates that are < date pulled?
write.csv(inmate_grp_dates_dt,"data/recids_daily_census_2_10_23_bf.csv")

#check, choose random date to explore - heavily skewed, choose median 
# ggplot(data_doc_wo_recids[admitted_dt %in% as.Date("2018-09-25")], aes(x=length_of_stay)) + geom_histogram()

data_doc_wo_recids[, median_length_stay_age := median(length_of_stay, na.rm = TRUE), by = c("admitted_dt", "50 Years and Over")]

plot_dt <- unique(data_doc_wo_recids[year_adm>2018 & !is.na(median_length_stay_age) & date_ != release_date_approx,.(admitted_dt, release_date_approx, `50 Years and Over`, year_adm, median_length_stay_age)])
write.csv(plot_dt, "data/non_recids_daily_census_2_13_23_bf.csv")

# look at N per day over time for length off stay 
ggplot(plot_dt[!is.na(`50 Years and Over`), ],  aes(x=admitted_dt, y = median_length_stay_age, group=`50 Years and Over`, color=`50 Years and Over`)) + 
  theme_bw()  + geom_smooth(se=FALSE) + xlab("Date of Admission") + ylab("Median Length of Stay (Days)")

# geting length of stay time for folks that were admitted on or after the date chron job started 
# trusted LOT 
lot_trust <- data_doc_wo_recids[release_date_approx >= date_, .(median_length_stay_age, `50 Years and Over`, date_)]






