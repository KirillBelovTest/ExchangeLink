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


$settings = 
ExchangeLinkSettings[]["Binance"]


Options[createRequest] = {
	"Endpoint" :> $settings["Endpoint"], 
	"Formatter" :> $formatter, 
	"HTTPMethod" :> "GET"
}


createRequest[{path__String, parameters: Rule[_String, _]...}, OptionsPattern[]] := 
Block[{endpoint, serializer, formatter, httpMethod, query, url}, 
	endpoint = OptionValue["Endpoint"]; 
	formatter = OptionValue["Formatter"]; 
	httpMethod = OptionValue["HTTPMethod"]; 

	query = Normal[Map[formatter, DeleteCases[{parameters}, _[_, Null|Automatic]]]]; 
		
	url = URLBuild[{endpoint, "api", path}, query]; 
	
	Return[HTTPRequest[url, <|Method -> httpMethod|>]]
]


Options[getResponse] = {
	"Deserializer" :> $deserializer, 
	"Timeout" :> 5
}


getResponse[request_HTTPRequest, OptionsPattern[]] := 
Module[{
	response, statusCode, retryAfter, result, 
	timeout = OptionValue["Timeout"], 
	deserialize = OptionValue["Deserializer"]
}, 
	
	TimeConstrained[response = URLRead[request], timeout, 
		Message[$Binance::timeout, request, timeout]; Return[$Failed]
	]; 
	
	statusCode = response["StatusCode"]; 
	
	Switch[statusCode, 
		200, 
			result = deserialize[response]; 
			Return[result], 
		429, 
			retryAfter = "retry-after" /. response["Headers"]; 
			Message[$Binance::ratelimit, request, response, retryAfter]; 
			Pause[retryAfter]; 
			Return[getResponse[request]], 
		_, 
			Message[$Binance::errcode, request, response, statusCode]; 
			Return[$Failed]
	]
]


End[] (*`Private`*)


EndPackage[] (*"ExchangeLink`Binance`Public`"*)


