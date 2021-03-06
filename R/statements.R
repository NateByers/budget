readGoogleStatement <- function(){
  statements <- gs_title("Statements")
  df <- statements %>% gs_read()
  df
}

writeStatementTable <- function(raw_statement_folder = "C:/Budget/Statements/raw",
                                statement_table_folder = "C:/Budget/Statements/table"){
  # raw_statement_folder = "D:/Statements/raw"; statement_table_folder = "D:/Statements/processed"
  statement_files <- list.files(raw_statement_folder)
  statement_files <- grep("\\.txt", statement_files, value = TRUE)

  statements <- lapply(paste(raw_statement_folder, statement_files, sep = "/"),
                       function(x){
                         # x = paste(raw_statement_folder, statement_files, sep = "/")[12]
                         df <- readStatement(x) %>%
                           mutate(DESCRIPTION = str_trim(DESCRIPTION),
                                  DESCRIPTION = sub("[[:space:]]{2,}", " ", DESCRIPTION),
                                  CATEGORY = as.character(NA),
                                  CATEGORY_PERCENT = as.character(NA)) %>%
                           select(DATE, STATEMENT_TYPE, DESCRIPTION, AMOUNT,
                                  CATEGORY, CATEGORY_PERCENT)
                         df
                       })

  statements <- Reduce(rbind, statements)

  statement_table_file <- paste(statement_table_folder, "statement_table.csv",
                           sep = "/")
  statement_table <- read.csv(statement_table_file, stringsAsFactors = FALSE) %>%
    mutate(DESCRIPTION = str_trim(DESCRIPTION),
           DESCRIPTION = sub("[[:space:]]{2,}", " ", DESCRIPTION),
           DATE = as.Date(DATE, "%m/%d/%Y"))

  new_statement <- anti_join(statements, statement_table,
                             by = c("DATE", "STATEMENT_TYPE", "DESCRIPTION", "AMOUNT"))

  if(dim(new_statement)[1] > 0){
    statement_table <- rbind(statement_table, new_statement) %>%
      arrange(desc(DATE), STATEMENT_TYPE, DESCRIPTION, AMOUNT) %>%
      filter(!duplicated(paste(DATE, DESCRIPTION, AMOUNT)))

    write.csv(statement_table, file = statement_table_file, row.names = FALSE)
  }
}

readStatement <- function(statement_file){
  # statement_file = x
  date <- strsplit(statement_file, "_")[[1]]
  year <- as.numeric(substr(date[2], 1, 4))
  month <- as.numeric(substr(date[2], 6, 7))
  january_statement <- month == 1

  statement <- readLines(statement_file)

  debit_table <- makeStatementTable(statement, year, january_statement, "debit")
  checks_table <- makeStatementTable(statement, year, january_statement, "checks")
  deposit_table <- makeStatementTable(statement, year, january_statement, "deposits")
  auto_withdraw_table <- makeStatementTable(statement, year, january_statement, "auto_withdraw")

  Reduce(rbind, list(debit_table, checks_table, deposit_table, auto_withdraw_table))
}

makeStatementTable <- function(statement_lines, year, january_statement,
                               type = c("debit", "checks", "deposits",
                                        "auto_withdraw")){

  # statement_lines = statement
  if(type == "debit"){
    begin <- grep("ATM & DEBIT CARD WITHDRAWALS", statement_lines)[1]
    end <- grep("Total ATM & Debit Card Withdrawals", statement_lines)[1]
    statement_type <- "DEBIT/ATM"
  }else if(type == "checks"){
    begin <- grep("CHECKS PAID", statement_lines)[1]
    end <- grep("Total Checks Paid", statement_lines)[1]
    statement_type <- "CHECKS PAID"
  }else if(type == "deposits"){
    begin <- grep("DEPOSITS AND ADDITIONS", statement_lines)[1]
    end <- grep("Total Deposits and Additions", statement_lines)[1]
    statement_type <- "DEPOSITS"
  }else if(type == "auto_withdraw"){
    begin <- grep("ELECTRONIC WITHDRAWALS", statement_lines)[1]
    end <- grep("Total Electronic Withdrawals", statement_lines)[1]
    statement_type <- "ELECTRONIC WITHDRAWALS"
  }
  statement_rows <- lapply(statement_lines[begin:end], processStatementLine,
                           type = type)
  statement <- Reduce(rbind, statement_rows)
  statement <- statement[!is.na(statement$DATE), ] %>%
    mutate(YEAR = ifelse(substr(DATE, 1, 2) == "12" & january_statement, year - 1, year),
           MONTH = substr(DATE, 1, 2), DAY = substr(DATE, 4, 5),
           DATE = as.Date(paste(YEAR, MONTH, DAY, sep = "-")),
           STATEMENT_TYPE = statement_type) %>%
    select(-(YEAR:DAY))
  statement
}

processStatementLine <- function(statement_line, 
                                 type = c("debit", "checks", "deposits",
                                          "auto_withdraw")){

  if(type == "debit"){
    # statement_line = statement_lines[4]
    if(grepl("^\\d{2}/\\d{2}", statement_line)){
      words <- strsplit(statement_line, " ")[[1]]
      date_position <- grep("^\\d{2}/\\d{2}", words)
      date_position <- date_position[length(date_position)]
      date <- words[date_position]
      dollars_position <- grep("\\.\\d{2}", words)
      dollars <- words[dollars_position]
      dollars <- sub("\\$", "", dollars)
      dollars <- sub(",", "", dollars)
      description <- paste(words[(date_position + 1):(dollars_position - 1)],
                           collapse = " ")
      return(data.frame(DATE = date, DESCRIPTION = description, AMOUNT = as.numeric(dollars),
                 stringsAsFactors = FALSE))
    }else{
      return(data.frame(DATE = NA, DESCRIPTION = NA, AMOUNT = NA, stringsAsFactors = FALSE))
    }
  }

  if(type == "checks"){
    # statement_line = statement_lines[55]
    if(grepl("^\\d{4}", statement_line)){
      words <- strsplit(statement_line, " ")[[1]]
      date_position <- grep("^\\d{2}/\\d{2}", words)
      date_position <- date_position[length(date_position)]
      date <- words[date_position]
      dollars_position <- grep("\\.\\d{2}", words)
      dollars <- words[dollars_position]
      dollars <- sub("\\$", "", dollars)
      dollars <- sub(",", "", dollars)
      if(grepl("Check #", paste(words, collapse = " "))){
        description <- paste(words[3:(dollars_position - 1)],
                             collapse = " ")
      }else{
        description <- paste("Check #", words[1])
      }
      return(data.frame(DATE = date, DESCRIPTION = description, AMOUNT = as.numeric(dollars),
                 stringsAsFactors = FALSE))
    }else{
      return(data.frame(DATE = NA, DESCRIPTION = NA, AMOUNT = NA, stringsAsFactors = FALSE))
    }
  }

  if(type == "deposits"){
    # statement_line = statement_lines[43]
    if(grepl("^\\d{2}/\\d{2}", statement_line)){
      words <- strsplit(statement_line, " ")[[1]]
      date_position <- grep("^\\d{2}/\\d{2}", words)
      date_position <- date_position[length(date_position)]
      date <- words[date_position]
      dollars_position <- grep("\\.\\d{2}", words)
      dollars <- words[dollars_position]
      dollars <- sub("\\$", "", dollars)
      dollars <- sub(",", "", dollars)
      description <- paste(words[3:(dollars_position - 1)],
                          collapse = " ")
      return(data.frame(DATE = date, DESCRIPTION = description, AMOUNT = as.numeric(dollars),
                        stringsAsFactors = FALSE))
    }else{
      return(data.frame(DATE = NA, DESCRIPTION = NA, AMOUNT = NA, stringsAsFactors = FALSE))
    }
  }
  
  if(type == "auto_withdraw"){
    # statement_line = statement_lines[137]
    if(grepl("^\\d{2}/\\d{2}", statement_line)){
      words <- strsplit(statement_line, " ")[[1]]
      date_position <- grep("^\\d{2}/\\d{2}", words)
      date_position <- date_position[length(date_position)]
      date <- words[date_position]
      dollars_position <- grep("\\.\\d{2}", words)
      dollars <- words[dollars_position]
      dollars <- sub("\\$", "", dollars)
      dollars <- sub(",", "", dollars)
      description <- paste(words[(date_position + 1):(dollars_position - 1)],
                           collapse = " ")
      return(data.frame(DATE = date, DESCRIPTION = description, AMOUNT = as.numeric(dollars),
                        stringsAsFactors = FALSE))
    }else{
      return(data.frame(DATE = NA, DESCRIPTION = NA, AMOUNT = NA, stringsAsFactors = FALSE))
    }
  }

}
