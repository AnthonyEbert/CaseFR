
library(dplyr)
library(CaseFR)

regione = levels(
  COVID19data::covid19_sorted %>%
    filter(alpha3 == "ITA") %>%
    pull(Province.State) %>%
    factor()
  )

case_fr <- data.frame(regione = regione) %>%
  group_by(regione) %>%
  do(data.frame(tryCatch(absolute_CFR("Italy", .$regione, Country2 = "Germany", Province2 = "total", max.iter = 10000, enddate = "2020-04-05")[[1]], error=function(err) NA)))

case_fr <- lapply(regione, function(x){try(absolute_CFR("Italy", x, Country2 = "Germany", Province2 = "total")[[1]], TRUE)})
