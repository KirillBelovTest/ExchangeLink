(* :Package: *)

BeginPackage["ExchangeLink`Binance`Public`"]


BinancePing::usage = 
"BinancePing[]"


BinanceTime::usage = 
"BinanceTime[]"


BinanceKlines::usage = 
"BinanceKlines[\"SYMBOL\", \"interval\"]
BinanceKlines[\"SYMBOL\", \"interval\", opts]"


BinanceTicker24hr::usage = 
"BinanceTicker24hr[]
BinanceTicker24hr[symbol]"


BinanceTickerPrice::usage = 
"BinanceTickerPrice[]
BinanceTickerPrice[symbol]"


Begin["`Private`"]


getSettings[] := 
$settings = 
ExchangeLinkSettings[]["Binance"]


End[] (*`Private`*)


EndPackage[] (*"ExchangeLink`Binance`Public`"*)


