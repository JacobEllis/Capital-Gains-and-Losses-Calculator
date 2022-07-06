module Main where
import Data.Time
import Data.List
import Data.Binary
import Data.Char (isSpace)

main :: IO ()
main = do
    putStrLn "Enter the name and file path of a CSV containing your transactions. (Remove the header line)"
    fileName <- getLine
    transactions <- readCsv fileName
    let cgt = calculateHiFo transactions []
    print "Capital Gains/Losses: "
    print $ reverse $ fst cgt
    --writeCapitalGainsCsv "capitalGains.csv" $ fst cgt
    print "Remaining transactions: "
    print $ reverse $ snd cgt
    --writeTransactionsCsv "remainingTransactions.csv" $ snd cgt

data TxType = Buy | Sell deriving (Show, Eq)

data Transaction = Transaction {txDate :: Day, txType :: TxType, txQuantity :: Float, txPrice :: Float} deriving (Show, Eq)

data CapGainTx = CapGainTx {capGainTxdate :: Day, capGainTxdquantity :: Float, capGainTxdbuyTotal :: Float, capGainTxdsellTotal :: Float, capGainTxdgainOrLoss :: Float} deriving (Show)

profit :: Transaction -> Transaction -> [Transaction] -> (Maybe CapGainTx, [Transaction])
profit buy sell lst = let qb = txQuantity buy
                          qs = txQuantity sell
                          pb = txPrice buy
                          ps = txPrice sell
                          ds = txDate sell
                          db = txDate buy
                      in  if txType buy == Buy && txType sell == Sell && qs > qb then
                        ( Just $ CapGainTx (txDate sell) qb (qb * pb) (qb * ps) ((qb * ps) - (qb * pb)), addTranscation Transaction { txDate=ds, txType=Sell, txQuantity=qs-qb, txPrice=ps } (removeTransaction sell (removeTransaction buy lst)))
                      else
                          if txType buy == Buy && txType sell == Sell && qs == qb then
                             ( Just $ CapGainTx (txDate sell) qb (qb * pb) (qb * ps) ((qb * ps) - (qb * pb)), removeTransaction sell (removeTransaction buy lst))
                          else
                              if txType buy == Buy && txType sell == Sell && qs < qb then
                                ( Just $ CapGainTx (txDate sell) qs (qs * pb) (qs * ps) ((qs * ps) - (qs * pb)), addTranscation Transaction { txDate=db, txType=Buy, txQuantity=qb-qs, txPrice=pb } (removeTransaction sell (removeTransaction buy lst)))
                              else
                                (Nothing, lst)

sortTransactionsByDate :: [Transaction] -> [Transaction]
sortTransactionsByDate = sortBy (\t1 t2 -> compare (txDate t1) (txDate t2))

readCsv :: FilePath -> IO [Transaction]
readCsv path = do
  contents <- readFile path
  return $ map parseLine $ lines contents
  where
    parseLine :: String -> Transaction
    parseLine line = let [dateYear, dateMonth, dateDay, txTypeStr, quantityStr, priceStr] = wordsWhen (==',') line
                         date = fromGregorian (read dateYear :: Integer) (read dateMonth :: Int) (read dateDay :: Int)
                         txType = if trim txTypeStr == "Buy" then Buy else Sell
                         quantity = read quantityStr :: Float
                         price = read priceStr :: Float
                     in Transaction date txType quantity price

writeTransactionsCsv :: FilePath -> [Transaction] -> IO ()
writeTransactionsCsv path transactions = writeFile path $ unlines $ map (\t -> show (txDate t) ++ "," ++ show (txType t) ++ "," ++ show (txQuantity t) ++ "," ++ show (txPrice t)) transactions

writeCapitalGainsCsv :: FilePath -> [CapGainTx] -> IO ()
writeCapitalGainsCsv path transactions = writeFile path $ unlines $ map (\t -> show (capGainTxdate t) ++ "," ++ show (capGainTxdquantity t) ++ "," ++ show (capGainTxdbuyTotal t) ++ "," ++ show (capGainTxdsellTotal t) ++ "," ++ show (capGainTxdgainOrLoss t)) transactions

getFirstSellTransaction :: [Transaction] -> Maybe Transaction
getFirstSellTransaction [] = Nothing
getFirstSellTransaction (t:ts) = if txType t == Sell then Just t else getFirstSellTransaction ts

getBuyTransactionWithHighestPrice :: [Transaction] -> Maybe Transaction
getBuyTransactionWithHighestPrice [] = Nothing
getBuyTransactionWithHighestPrice (t:ts) = if txType t == Buy then Just t else getBuyTransactionWithHighestPrice ts

maximumPrice :: [Transaction] -> Transaction
maximumPrice = foldr1 (\x y ->if txPrice x >= txPrice y then x else y)


splitListAtTransaction :: Transaction -> [Transaction] -> ([Transaction], [Transaction])
splitListAtTransaction t [] = ([], [])
splitListAtTransaction t (t1:ts) = if t1 == t then ([t1], ts) else (t1:t1s, t2s)
  where
    (t1s, t2s) = splitListAtTransaction t ts

removeTransaction :: Transaction -> [Transaction] -> [Transaction]
removeTransaction _ [] = []
removeTransaction t (t1:ts) = if t == t1 then ts else t1 : removeTransaction t ts

addTranscation :: Transaction -> [Transaction] -> [Transaction]
addTranscation t ts = sortTransactionsByDate $ t : ts

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

containsSellTransaction :: [Transaction] -> Bool
containsSellTransaction = foldr (\ t -> (||) (txType t == Sell)) False

calculateHiFo :: [Transaction] -> [CapGainTx] -> ([CapGainTx], [Transaction])
calculateHiFo txs cgt = if containsSellTransaction txs then do
                          let fstSell = getFirstSellTransaction $ sortTransactionsByDate txs
                          case fstSell of
                              Nothing -> error "No sell transaction!"
                              Just t -> do
                                let splt = splitListAtTransaction t txs  in
                                  if not $ null (removeTransaction t (fst splt)) then
                                    let maxbuy = (maximumPrice $ removeTransaction t (fst splt)) in
                                          let result = profit maxbuy t txs in
                                            case result of
                                              (Nothing, ts) -> (cgt, ts)
                                              (Just cgtx, ts) -> calculateHiFo ts (cgtx:cgt)
                                  else
                                    (cgt, txs)
                        else
                          (cgt, txs)