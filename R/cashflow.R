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

getStatementTable <- function(statement_file = "C:/Budget/Statements/table/statement_table.csv"){
  read.csv(statement_file, stringsAsFactors = FALSE)
}

getMonthlyTotals <- function(statement_table, category, year){
  # statement_table = getStatementTable(); category = "Dining Out"; year = 2015

  monthly <- statement_table %>%
    mutate(year = substr(as.character(DATE, 1, 4)))
}
