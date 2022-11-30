name_conv <- read.csv("data-raw/name_conversion.csv") %>% data.table()
save(name_conv, file = "R/sysdata.rda")
