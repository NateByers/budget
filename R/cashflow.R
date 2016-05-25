postCashflow <- function(year = 2015){
  cashflow <- readCashflow()
  statement_totals <- readGoogleStatement() %>% getMonthlyTotals(year = 2015)
  
  # cashflow <- gs_title(paste0("Cashflow_", year, ".xlsx"))
  # cashflow %>% gs_edit_cells(input = "4000", anchor = "G6")
  #
  categories <- statement_totals$CATEGORY
  # categories <- statement_totals$CATEGORY[as.numeric(grep("^Other", statement_totals$CATEGORY)):dim(statement_totals)[1]]
  
  lapply(categories, addCashflowRow, 
         statement_totals = statement_totals, cashflow = cashflow, year = year)
  
  
}

addCashflowRow <- function(row, statement_totals, cashflow, year){
  print(row)
  start_cell <- findCashflowCell(cashflow, row, "JAN")
  row_values <- statement_totals %>%
    filter(grepl(paste0("^", row), CATEGORY)) %>%
    select(JAN:DEC) %>%
    as.data.frame() %>%
    as.vector() %>%
    as.numeric()
  gs_title(paste0("Cashflow_", year, ".xlsx")) %>%
    gs_edit_cells(input = row_values, anchor = start_cell, byrow = TRUE)
}


readCashflow <- function(year = 2015){
  cashflow <- gs_title(paste0("Cashflow_", year, ".xlsx"))
  cash_df <- cashflow %>% gs_read()
  cash_df
}



findCashflowCell <- function(data, row_name, month){
  # data = cash_df; row_name = "Other"; month = "JAN"
  if(sum(grepl(paste0("^", row_name), data[["Cash Flow Statement"]])) != 1){
    stop("row name either does not exist or exists in more than one row")
  }

  row_number <- grep(paste0("^", row_name), data[["Cash Flow Statement"]]) + 1
  column_letter <- LETTERS[grep(month, data[1, ])]

  paste0(column_letter, row_number)

}


getMonthlyTotals <- function(statement_table, year){
  # statement_table = readGoogleStatement(); category = "Other"; year = 2015

  da_months <- toupper(format(as.Date(sprintf("2010-%02i-01", 1:12)), format = "%b"))
  
  statement_table <- statement_table %>% processMultipleCategories()
  
  statement_table[is.na(statement_table$CATEGORY), "CATEGORY"] <- "Other"
  
  monthly <- statement_table %>%
    filter(CATEGORY != "Transfer") %>%
    mutate(YEAR = str_split_fixed(DATE, "/", 3)[, 3],
           MONTH = months(as.Date(DATE, "%m/%d/%Y"), abbreviate = TRUE),
           MONTH = toupper(MONTH)) %>%
    filter(YEAR == year) %>%
    group_by(CATEGORY, MONTH) %>%
    summarize(Total = sum(AMOUNT, rm.na = TRUE)) %>%
    spread(MONTH, Total, fill = 0) %>%
    select_(.dots = c("CATEGORY", da_months)) %>%
    ungroup()
  monthly
}

processMultipleCategories <- function(df){
  # df = statement_table
  df_single_categories <- df %>% 
    filter(!grepl("\\|", CATEGORY))
  
  df_multiple_categories <- df %>% 
    filter(grepl("\\|", CATEGORY))
  
  df_list <- apply(df_multiple_categories, 1, function(row){
    # i = 1
    # row = c(df[i, "CATEGORY"],
    #         df[i, "CATEGORY_PERCENT"],
    #         df[i, "AMOUNT"],
    #         df[i, "DATE"],
    #         df[i, "STATEMENT_TYPE"],
    #         df[i, "DESCRIPTION"])
    categories <- str_split(row["CATEGORY"], "\\|")[[1]]
    percents <- str_split(row["CATEGORY_PERCENT"], "\\|")[[1]]
    category_n <- length(categories)
    df_list <- lapply(1:category_n, function(i, row, categories, percents){
      data.frame(DATE = row["DATE"], STATEMENT_TYPE = row["STATEMENT_TYPE"],
                 DESCRIPTION = row["DESCRIPTION"], 
                 AMOUNT = as.numeric(percents[i])*as.numeric(row["AMOUNT"]),
                 CATEGORY = categories[i], CATEGORY_PERCENT = as.character(NA),
                 stringsAsFactors = FALSE)
    }, row = row, categories = categories, percents = percents)
    Reduce(rbind, df_list)
  })
  
  df_multiple_categories <- Reduce(rbind, df_list)
  
  rbind(df_single_categories, df_multiple_categories)
}
