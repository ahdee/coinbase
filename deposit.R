
deposit_this <- function (thismuch=20, accid, api.key, passphrase, secret  ){
  method = 'POST'
  req.url = '/deposits/payment-method/'
  #api.url <- "https://api-public.sandbox.pro.coinbase.com"
  
  url <- paste0(api.url, req.url)
  
  timestamp <-
    format(as.numeric(Sys.time()), digits = 13) # create nonce
  key <- base64Decode(secret, mode = "raw") # encode api secret
  
  
  payload <- list(
    "amount" = thismuch,
    "currency" = 'USD',
    "payment_method_id" = accid
  )
  
  
  order <- toJSON(payload, auto_unbox = TRUE)
  
  
  what <- paste0(timestamp, method, req.url, order)
  
  #create encoded signature----
  sign <-
    base64Encode(hmac(key, what, algo = "sha256", raw = TRUE)) # hash
  
  
  
  
  #define headers----
  httpheader <- c(
    'CB-ACCESS-KEY' = api.key,
    'CB-ACCESS-SIGN' = sign,
    'CB-ACCESS-TIMESTAMP' = timestamp,
    'CB-ACCESS-PASSPHRASE' = passphrase,
    'Content-Type' = 'application/json'
  )
  
  
  done = httr::content(httr::POST(url, add_headers(httpheader), body = order))
  
  return ( done )
  
}
