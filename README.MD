# Captial Gains & Losses Calculator
## Description
This project is a simple a Haskell program that takes a CSV input of transactions and calculates the captial gains or losses for those transactions. Currently I am only applying the HiFo (Highest in First out) accounting method. This is a method that takes the highest cost items out first to be used for tax purposes. The buy transaction used must have taken place before the sell transaction that is triggering the realized gain/loss. I built this with the United States tax law in mind as well as making it for Crypto instead of securities. The biggest difference of having Crypto in mind is the Wash Sale rule. I did not include the Wash Sale rule as part of the algorithm because the rule does not apply for crytocurrencies, however I may add it in the future. The project was built for Emurgo's Professional Developer Program and nothing from this readme or program is financial advice.
## Usage
``` 
cabal run Capital-Gains-Loasses-Calculator
```
Supply a .csv following this format **without the headers**:
```
year, month, day, hour, mins, sec, Buy/Sell, qty, price
i.e:
2022, 03, 20, 10, 12, 01, Buy, 3.0, 2.00
```
Or hit Enter at the prompt to run a pre-built testcase CSV: 
```
(./Test Cases/Test01.csv)
```

## Result
Prints an aggregation of the transactions into their realized Buy/Sell gains/losses to the console as well as written to a CSV file. The remaining unrealized transactions will be printed to the console and a csv as well. Finally a Total Realized Gain/Loss number will be printed which adds up the realized transactions to give the total.
