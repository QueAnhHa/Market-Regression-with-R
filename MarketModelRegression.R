# Package required: dplyr

# Install dplyr
install.packages("dplyr") 

# Create a function called MMR to generate two summary tables

MMR <- function(marketfile, stockfile){
  
# Call Packages
library(dplyr)
  
# Market Data Processing
mkt <- read.csv(marketfile, header = T)
mkt <- transform(mkt, DATE = as.Date(as.character(DATE), "%Y%m%d")) # transfrom Date of Market Data into Date Format

# Stock Data Processing
stocks <- read.csv(stockfile, header = T, na.string = "C")
stocks <- transform(stocks, date = as.Date(as.character(date), "%m%d%Y"))

# Data Merging
StockData <- data.frame(Date=stocks$date, Ticker=stocks$TICKER, FirmReturn=stocks$RET)
MarketData <- data.frame(Date=mkt$DATE, MarketReturn=mkt$sprtrn)
CombinedData <- merge(StockData, MarketData)

# Regression Output Generation
MyReg <- CombinedData %>%
  group_by(Ticker) %>%
  do(ols.model = lm(data = ., formula = FirmReturn ~ MarketReturn)) %>%  # estimate model
  mutate(Alpha = coef(ols.model)[1], # get Alpha
         Beta = coef(ols.model)[2]) # get Beta

# Genearte first table with tickers, alpha and beta information
Table1 <- data.frame(Ticker = MyReg$Ticker, Alpha = MyReg$Alpha, Beta = MyReg$Beta)

# Generate the second table highlighting the highest and lowest alphas and betas
Table2 <- data.frame(
  Category = c('Highest Alpha', 'Lowest Alpha', 'Highest Beta', 'Lowest Beta'),
  Ticker = c(
    as.character(MyReg[which.max(MyReg$Alpha),]$Ticker),
    as.character(MyReg[which.min(MyReg$Alpha),]$Ticker),
    as.character(MyReg[which.max(MyReg$Beta),]$Ticker),
    as.character(MyReg[which.min(MyReg$Beta),]$Ticker)),
  value = c(
    MyReg[which.max(MyReg$Alpha),]$Alpha,
    MyReg[which.min(MyReg$Alpha),]$Alpha,
    MyReg[which.max(MyReg$Beta),]$Beta,
    MyReg[which.min(MyReg$Beta),]$Beta))

FinalOutput <- list(summary = Table1, Highlights = Table2)
FinalOutput
}

# Run the function with csv file paths
MMR('C:/Projects/MarketRegressionwithR/CVS Files/B2 Market.csv', 'C:/Projects/MarketRegressionwithR/CVS Files/B2 Stocks.csv')
