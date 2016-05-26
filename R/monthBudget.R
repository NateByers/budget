makeMonthBudget <- function(budget_file){
  # budget_file = "D:/Budget/monthly_budget.csv"
  distributions <- getExpensesAvg("distributions")
  fixed <- getExpensesAvg("fixed")
  discretionary <- getExpensesAvg("discretionary")
  budget <- rbind(distributions, fixed, discretionary)
  
  write.csv(budget, file = budget_file, row.names = FALSE)
}

getFixedIncomeAvg <- function(){
  salary <- (63000 + 31000)/12
  taxes <- 8000/12
  salary - taxes
}

getExpensesAvg <- function(type = c("distributions", "fixed", "discretionary")){
  cashflow <- readCashflow()[, c(2, 5:16)]
  names(cashflow) <- c("CATEGORY", cashflow[1, 2:13])
  
  if(type == "distributions"){
    begin_text <- "Distributions:"
    end_text <- "Fixed Expenses:"
  }else if(type == "fixed"){
    begin_text <- "Fixed Expenses:"
    end_text <- "Discretionary Expenses:"
  }else if(type == "discretionary"){
    begin_text <- "Discretionary Expenses:"
    end_text <- "TOTAL DISTRIBUTIONS"
  }
  begin_position <- grep(begin_text, cashflow$CATEGORY) + 1
  end_position <- grep(end_text, cashflow$CATEGORY) - 1
  categories <- cashflow$CATEGORY[begin_position:end_position]
    
  category_averages <- cashflow %>%
    filter(!is.na(CATEGORY), CATEGORY %in% categories)
  
  category_averages[is.na(category_averages)] <- 0
  for(i in names(category_averages)[-1]){
    category_averages[[i]] <- gsub(",", "", category_averages[[i]])
    category_averages[[i]] <- as.numeric(category_averages[[i]])
  }
  
  category_averages <- category_averages %>%
    gather(Month, Value, JAN:DEC) %>%
    group_by(CATEGORY) %>%
    summarize(Average = mean(Value))
  
  category_averages
}