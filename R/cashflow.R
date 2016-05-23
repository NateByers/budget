postCashflow <- function(year = 2015){
  cashflow <- readCashflow()
  statement_totals <- readGoogleStatement() %>% getMonthlyTotals(year = 2015)
}

readCashflow <- function(year = 2015){
  cashflow <- gs_title(paste0("Cashflow_", year, ".xlsx"))
  cash_df <- cashflow %>% gs_read()
  cash_df
}

loadCashflow <- function(data, year = 2015){
  # cashflow <- gs_title(paste0("Cashflow_", year, ".xlsx"))
  # cashflow %>% gs_edit_cells(input = "4000", anchor = "G6")
}

findCell <- function(data, row_name, month){
  # data = cash_df; row_name = "Dining Out"; month = "JAN"
  if(sum(grepl(row_name, data[["Cash Flow Statement"]])) != 1){
    stop("row name either does not exist or exists in more than one row")
  }

  row_number <- grep(row_name, data[["Cash Flow Statement"]]) + 1
  column_letter <- LETTERS[grep(month, data[1, ])]

  paste0(column_letter, row_number)

}


getMonthlyTotals <- function(statement_table, year){
  # statement_table = readGoogleStatement(); category = "Dining Out"; year = 2015

  da_months <- toupper(format(as.Date(sprintf("2010-%02i-01", 1:12)), format = "%b"))
  
  statement_table <- statement_table %>% processMultipleCategories()
  
  monthly <- statement_table %>%
    mutate(YEAR = str_split_fixed(DATE, "/", 3)[, 3],
           MONTH = months(as.Date(DATE, "%m/%d/%Y"), abbreviate = TRUE),
           MONTH = toupper(MONTH)) %>%
    filter(YEAR == year) %>%
    group_by(CATEGORY, MONTH) %>%
    summarize(Total = sum(AMOUNT, rm.na = TRUE)) %>%
    spread(MONTH, Total, fill = 0) %>%
    select_(.dots = c("CATEGORY", da_months))
  monthly
}

processMultipleCategories <- function(df){
  # df = statement_table
  df_multi_categories <- df %>% 
    filter(grepl("\\|", CATEGORY))
  
  df_single_categories_list <- lapply(df_multi_categories, function(row){
    # row = c(CATEGORY = df_multi_ca)
    categories <- str_split(row["CATEGORY"], "\\|")[[1]]
    percents <- str_split(row["CATEGORY_PERCENT"], "\\|")[[1]]
    category_n <- length(categories)
    df_list <- lapply(1:category_n, function(i){
      data.frame(DATE = row["DATE"], STATEMENT_TYPE = row["STATEMENT_TYPE"],
                 DESCRIPTION = row["DESCRIPTION"], 
                 AMOUNT = as.numeric(percents[i])*row["AMOUNT"],
                 CATEGORY = categories[i], CATEGORY_PERCENT = as.character(NA),
                 stringsAsFactors = FALSE)
    }, row = row, categories = categories, percents = percents)
    Reduce(rbind, df_list)
  })
  
  Reduce(rbind, df_single_categories_list)
  
  
}
