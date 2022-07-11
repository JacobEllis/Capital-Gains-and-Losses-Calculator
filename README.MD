# Captial Gains & Losses Calculator
## Description
This project is a simple a Haskell program that takes a CSV input of transactions and calculates the captial gains or losses for those transactions. The project was built for Emurgo's Professional Developer Program
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