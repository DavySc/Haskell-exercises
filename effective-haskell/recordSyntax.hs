{-# LANGUAGE RecordWildCards #-}

module RecordSyntax where

data CustomerInfo = CustomerInfo
  {firstName :: String, lastName :: String, widgetCount :: Int, balance :: Int}

customerGeorge :: CustomerInfo
customerGeorge = CustomerInfo {firstName = "George", lastName = "Bird", widgetCount = 10, balance = 100}

totalWidgetCount :: [CustomerInfo] -> Int
totalWidgetCount = sum . map widgetCount

emptyCart :: CustomerInfo -> CustomerInfo
emptyCart customer = customer {widgetCount = 0, balance = 0}

showCustomer :: CustomerInfo -> String
showCustomer CustomerInfo {..} =
  firstName <> " " <> lastName <> " " <> show widgetCount <> " " <> show balance

customerFactory firstName lastName =
  let widgetCount = 10
      balance = 100
   in CustomerInfo {..}
