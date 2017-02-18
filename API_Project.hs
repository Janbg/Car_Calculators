import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.HTTP.Server.HtmlForm as Form
import Network.HTTP.Server.Response
import Network.URL as URL
import Text.XHtml
import Codec.Binary.UTF8.String
import Data.List(isPrefixOf)
import Data.Maybe
import Numeric
import Data.String
import Text.Read

main :: IO ()
main = serverWith defaultConfig { srvLog = stdLogger, srvPort = 8888 }
     $ \_ url request ->

  case rqMethod request of

    POST ->
      return $
      case findHeader HdrContentType request of
        Just ty
          | "application/x-www-form-urlencoded" `isPrefixOf` ty ->
          case URL.importParams txt of
            Just fields -> sendText OK $
              --show fields
                formatOutput fields
            Nothing -> sendText BadRequest $
              "Could not understand URL encoded form data"


        x -> sendText BadRequest $
              "I don't know how to deal with POSTed content" ++
                      " of type " ++ show x

        -- we assume UTF8 encoding
        where txt = decodeString (rqBody request)

    _ -> return $ sendText BadRequest "I don't understand"



sendText       :: StatusCode -> String -> Response String
sendText s v    = (respond s :: Response String) { rspBody = txt }
  where txt       = encodeString v

getParamInt :: String -> [(String, String)] -> Int
getParamInt pName fields = read $ fromMaybe "1" $ lookup pName fields

getParamDouble :: String -> [(String, String)] -> Double
getParamDouble pName fields = read $ fromMaybe "1.5" $ lookup pName fields

getParamString :: String -> [(String, String)] -> String
getParamString pName fields = fromMaybe "" $ lookup pName fields

--------------------------------START of Leasing Calculator---------------------------
--Functions for constants
--gets the Annual Base by Type of Leasing
getAnnualBase :: String -> Double
getAnnualBase "Car" = 2.95
getAnnualBase "Truck" = 3.20
getAnnualBase "Agricultural" = 3.40
getAnnualBase "Construction" = 3.55
getAnnualBase "Machinery" = 4.10

--gets the Interest Rate by Type of Leasing
getInterestRate :: String -> Double
getInterestRate "Car" = 6.0
getInterestRate "Truck" = 6.5
getInterestRate "Agricultural" = 7.0
getInterestRate "Construction" = 7.5
getInterestRate "Machinery" = 8.0

--calculates the First Payment
calcFirstPaymentMoney :: [(String,String)] -> Double
calcFirstPaymentMoney fields = firstPayment/100 * buyPrice
    where 
        buyPrice = getParamDouble "BuyPrice" fields
        firstPayment = getParamDouble "FirstPayment" fields

--calculates the Financed Money by the Leasing
calcFinancedMoney :: [(String,String)] -> Double
calcFinancedMoney fields = (1 - firstPayment / 100)*buyPrice
    where 
        buyPrice = getParamDouble "BuyPrice" fields 
        firstPayment = getParamDouble "FirstPayment" fields

--calculates Due VAT
calcDueDDS :: [(String,String)] -> Double
calcDueDDS fields = 1 / 5 * buyPrice
    where
        buyPrice = getParamDouble "BuyPrice" fields

calcPaidDDSWithFirstPayment :: [(String,String)] -> Double
calcPaidDDSWithFirstPayment fields = 1 / 50 * firstPayment * buyPrice
    where 
        buyPrice = getParamDouble "BuyPrice" fields
        firstPayment = getParamDouble "FirstPayment" fields

calcFinancedDDS :: [(String,String)] -> Double
calcFinancedDDS fields = (1 - firstPayment/100) * 2 / 10 * buyPrice
    where 
        buyPrice = getParamDouble "BuyPrice" fields
        firstPayment = getParamDouble "FirstPayment" fields

calcLeasePayment :: [(String,String)] -> Double
calcLeasePayment fields = buyPrice*interestRate*factor/(1+interestRate*factor)
    where
        t = getParamString "Type" fields
        buyPrice = getParamDouble "BuyPrice" fields
        interestRate = getInterestRate t
        leaseTerm = getParamInt "LeaseTerm" fields
        factor = calcFactor fields

calcFactor :: [(String,String)] -> Double
calcFactor fields = (1+interestRate)**(fromIntegral leaseTerm*12)
    where
        t = getParamString "Type" fields
        interestRate = getInterestRate t
        leaseTerm = getParamInt "LeaseTerm" fields

calcTax :: [(String,String)] -> Double
calcTax fields = 1/100 * buyPrice
    where
        buyPrice = getParamDouble "BuyPrice" fields

calcLeaseMonths :: [(String,String)] -> Int
calcLeaseMonths fields = y*12
    where
        y = getParamInt "LeaseTerm" fields

calcAnnualAppreciation :: [(String,String)] -> Double
calcAnnualAppreciation fields = annualBase - firstPayment*32/1000
    where
        t = getParamString "Type" fields
        annualBase = getAnnualBase t
        firstPayment = getParamDouble "FirstPayment" fields

calcAppreciationMoney :: [(String,String)] -> Double
calcAppreciationMoney fields =  (fromIntegral leaseTerm)*12 + 1/10* firstPayment * annualAppreciation
    where 
        t = getParamString "Type" fields
        buyPrice = getParamDouble "BuyPrice" fields
        firstPayment = getParamDouble "FirstPayment" fields
        leaseTerm = getParamInt "LeaseTerm" fields
        annualAppreciation = calcAnnualAppreciation fields

isLeasing :: [(String,String)] -> Bool
isLeasing fields = (calcType == 1)
    where calcType = getParamInt "Calc" fields

formatOutput :: [(String,String)] -> String
formatOutput fields 
    | isLeasing fields = stringFirstPayment fields ++ stringFinancedMoney fields ++ stringDueDDS fields ++ stringPaidDDSWithFirstPayment fields ++ stringFinancedDDS fields ++ stringLeasePayment fields ++ stringTax fields ++ stringLeaseMonths fields ++ stringInterestRate fields ++ stringAnnualAppreciationMoney fields ++ stringAnnualAppreciation fields
    | otherwise = taxCalculator (getParamDouble "Area" fields) (getParamString "Catalyst" fields) (getParamInt "Hp" fields) (getParamString "Age" fields)

stringFirstPayment :: [(String,String)] -> String
stringFirstPayment fields  = "First Payment:                  " ++ (decimalPoint $ calcFirstPaymentMoney fields) ++ "~n"

stringFinancedMoney :: [(String,String)] -> String
stringFinancedMoney fields = "Financed Money:               " ++ (decimalPoint $ calcFinancedMoney fields) ++ "~n"

stringDueDDS :: [(String,String)] -> String
stringDueDDS fields = "Due DDS:                         " ++ (decimalPoint $ calcDueDDS fields) ++ "~n"

stringPaidDDSWithFirstPayment :: [(String,String)] -> String
stringPaidDDSWithFirstPayment fields = "Paid DDS:                         " ++ (decimalPoint $ calcPaidDDSWithFirstPayment fields) ++ "~n"

stringFinancedDDS :: [(String,String)] -> String
stringFinancedDDS fields = "Financed DDS:                  " ++ (decimalPoint $ calcFinancedDDS fields) ++ "~n"

stringLeasePayment :: [(String,String)] -> String
stringLeasePayment fields = "Lease Payment:                 " ++ (decimalPoint $ calcLeasePayment fields) ++ "~n"

stringTax :: [(String,String)] -> String
stringTax fields = "Tax:                                 " ++ (decimalPoint $ calcTax fields) ++ "~n"

stringLeaseMonths :: [(String,String)] -> String
stringLeaseMonths fields = "Lease Months:                   " ++ show (calcLeaseMonths fields) ++ "~n"

stringInterestRate :: [(String,String)] -> String
stringInterestRate fields = "Interest Rate:                    " ++ (decimalPoint (getInterestRate $ getParamString "Type" fields)) ++ "~n"

stringAnnualAppreciationMoney :: [(String,String)] -> String
stringAnnualAppreciationMoney fields = "Annual Apreciation Money:  " ++ (decimalPoint $ calcAppreciationMoney fields) ++ "~n"

stringAnnualAppreciation :: [(String,String)] -> String
stringAnnualAppreciation fields = "Annual Money:                   " ++ (decimalPoint $ calcAnnualAppreciation fields) ++ "~n"

--------------------------------END of Leasing Calculator-------------------------------
 
--------------------------------START of Tax Calculator---------------------------------- 

-- ------------------------------------------------------------------------------------
-- How to use:
--            >  taxCalculator 0.70 "No" 90 "over-5-to-14-years"
--    result:    "80.41"
-- ------------------------------------------------------------------------------------


 --               area -> catalyst-> Horse -> age
taxCalculator :: Double -> String -> Int-> String-> String
taxCalculator area catalyst hp age
    | checkUpTo5     age = (decimalPoint (formulaUpTo5     hp area catalyst))
    | checkOver5to14 age = (decimalPoint (formulaOver5to14 hp area catalyst))
    | otherwise          = (decimalPoint (formulaOver14    hp area catalyst))



--  ====================================== Helpers ======================================
decimalPoint floatNum = showFFloat (Just 2) floatNum ""

haveCatalyst ::   String-> Int
haveCatalyst   answ = if (compare answ "Yes") == EQ                then 2 else 1

checkUpTo5 ::     String -> Bool
checkUpTo5     answ = if (compare answ "up-to-5-years") == EQ      then True else False

checkOver5to14 :: String -> Bool
checkOver5to14 answ = if (compare answ "over-5-to-14-years") == EQ then True else False
-- ------------------------------------------------------------------------------------
formulaUpTo5 :: Int -> Double -> String -> Double
formulaUpTo5 hp area catalyst 
    | hp <= 50  = (fromIntegral hp) *  area / (fromIntegral cat)
    | hp <= 74  = (fromIntegral hp) * (area + 0.17647 * area) / (fromIntegral cat)
    | hp <= 100 = (fromIntegral hp) * (area + 1.38247 * area) / (fromIntegral cat)
    | hp <= 149 = (fromIntegral hp) * (area + 3.85261 * area)
    | otherwise = (fromIntegral hp) * (area + 4.41142 * area)
    where 
    cat = haveCatalyst catalyst

formulaOver5to14 :: Int -> Double -> String -> Double
formulaOver5to14 hp area catalyst
    | hp <= 50  = (fromIntegral hp) *  area * 0.535 / (fromIntegral cat)
    | hp <= 74  = (fromIntegral hp) *  area * 0.63  / (fromIntegral cat)
    | hp <= 100 = (fromIntegral hp) * (area + 0.27628 * area) / (fromIntegral cat)
    | hp <= 149 = (fromIntegral hp) * (area + 1.59981 * area)
    | otherwise = (fromIntegral hp) * (area + 1.89905 * area)
    where 
    cat = haveCatalyst catalyst


formulaOver14 :: Int -> Double -> String -> Double
formulaOver14 hp area catalyst
    | hp <= 50  = (fromIntegral hp) * area * 0.357 / (fromIntegral cat)
    | hp <= 74  = (fromIntegral hp) * area * 0.42  / (fromIntegral cat)
    | hp <= 100 = (fromIntegral hp) * area * 0.851 / (fromIntegral cat)
    | hp <= 149 = (fromIntegral hp) * (area + 0.73317 * area)
    | otherwise = (fromIntegral hp) * (area + 0.93266 * area)
    where 
    cat = haveCatalyst catalyst

--------------------------------END of Tax Calculator---------------------------------- 



  