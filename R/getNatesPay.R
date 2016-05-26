getNatesPayCashflow <- function(year = 2015, pay_file = "C:/Budget/Paystubs/NatesPay.csv",
                                map_file = "C:/Budget/Paystubs/map-Nates_pay.csv"){
  pay <- read.csv(pay_file, stringsAsFactors = FALSE)
  pay_map <- read.csv(map_file, stringsAsFactors = FALSE) %>%
    filter(Whose_Paystub == "Nate", !is.na(Cashflow_Field))

  pay_sum <- pay %>%
    filter(Year == year) %>%
    select(-(Year), -(Day)) %>%
    group_by(Month) %>%
    summarize_each(funs(sum))

  pay_long <- pay_sum %>%
    gather(key = pay_category, value = value, Total_Gross:After_Tax_Deduction_Jag_Tag) %>%
    right_join(pay_map, by = c("pay_category" = "Paystub_Field")) %>%
    group_by(Month, Cashflow_Field) %>%
    summarize(total = sum(value))

  pay_wide <- pay_long %>%
    spread(Month, total)

  da_months <- toupper(format(as.Date(sprintf("2010-%02i-01", 1:12)), format = "%b"))

  for(i in seq_along(da_months)){
    # i = 2
    if(i %in% names(pay_wide)){
      names(pay_wide)[i + 1] <- da_months[i]
    }
  }

  pay_wide %>% rename(CATEGORY = Cashflow_Field) %>% as.data.frame()
}


