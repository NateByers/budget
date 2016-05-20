writeStatementTable <- function(raw_statement_folder = "C:/Statements/raw",
                                statement_table_folder = "C:/Users/nrbyers/Dropbox/Budget"){

  statement_files <- list.files(raw_statement_folder)
  statement_files <- grep("\\.txt", statement_files, value = TRUE)

  statements <- lapply(paste(raw_statement_folder, statement_files, sep = "/"),
                       function(x){
                         # x = paste(raw_statement_folder, statement_files, sep = "/")[1]
                         df <- readStatement(x)
                         statement_month <- strsplit(x, "_")[[1]][2]
                         statement_month <- strsplit(statement_month, "\\.")[[1]][1]
                         df$STATEMENT_MONTH <- statement_month
                         df$CATEGORY <- NA
                         df
                       })

  statements <- Reduce(rbind, statements)

  statement_table_file <- paste(statement_table_folder, "statement_table.csv",
                           sep = "/")
  statement_table <- read.csv(statement_table_file, stringsAsFactors = FALSE)
  statement_table$DATE <- as.Date(statement_table$DATE, "%m/%d/%Y")

  new_statement <- anti_join(statements, statement_table, by = "STATEMENT_MONTH")

  if(dim(new_statement)[1] > 0){
    statement_table <- rbind(statement_table, new_statement) %>%
      arrange(desc(DATE)) %>%
      filter(!duplicated(paste(DATE, DESCRIPTION, AMOUNT)))

    write.csv(statement_table, file = statement_table_file, row.names = FALSE)
  }
}

readStatement <- function(statement_file){
  # statement_file = x
  year <- strsplit(statement_file, "_")[[1]][2]
  year <- as.numeric(substr(year, 1, 4))

  statement <- readLines(statement_file)
  begin <- grep("ATM & DEBIT CARD WITHDRAWALS", statement)[1]
  end <- grep("Total ATM & Debit Card Withdrawals", statement)[1]
  table <- makeStatementTable(statement[begin:end]) %>%
    mutate(YEAR = ifelse(substr(DATE, 1, 2) == "12", year - 1, year),
           MONTH = substr(DATE, 1, 2), DAY = substr(DATE, 4, 5),
           DATE = as.Date(paste(YEAR, MONTH, DAY, sep = "-"))) %>%
    select(-(YEAR:DAY))
}

makeStatementTable <- function(statement_lines){
  # statement_lines = statement[begin:end]
  statement_rows <- lapply(statement_lines, processStatementLine)
  statement <- Reduce(rbind, statement_rows)
  statement[!is.na(statement$DATE), ]
}

processStatementLine <- function(statement_line){
  # statement_line = statement_lines[4]
  if(grepl("^\\d{2}/\\d{2}", statement_line)){
    words <- strsplit(statement_line, " ")[[1]]
    date_position <- grep("^\\d{2}/\\d{2}", words)
    date_position <- date_position[length(date_position)]
    date <- words[date_position]
    dollars_position <- grep("\\.\\d{2}", words)
    dollars <- words[dollars_position]
    dollars <- sub("\\$", "", dollars)
    description <- paste(words[(date_position + 1):(dollars_position - 1)],
                         collapse = " ")
    data.frame(DATE = date, DESCRIPTION = description, AMOUNT = as.numeric(dollars),
               stringsAsFactors = FALSE)
  }else{
    data.frame(DATE = NA, DESCRIPTION = NA, AMOUNT = NA, stringsAsFactors = FALSE)
  }
}
