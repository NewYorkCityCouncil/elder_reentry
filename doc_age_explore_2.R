library(data.table)
library(ggplot2)

# for melissa 
docmel <- fread("data/recids_daily_census_2_10_23_bf.csv")
doc_msub <- docmel[,.(inmateid, release_date_approx, order_adm_times)]
doc_msub <- unique(doc_msub)
write.csv(doc_msub, "bf_discharge_estimate_doc.csv")
