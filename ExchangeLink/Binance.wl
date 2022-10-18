(* ::Package:: *)

BeginPackage["ExchangeLink`Binance`", {
	"ExchangeLink`Tools`Settings`", 
	"ExchangeLink`Tools`HMAC`", 
	"ExchangeLink`Tools`Serialization`", 
	"WebSocketJLink`"
}];


ClearAll["`*"]


BinancePing::usage = 
"BinancePing[]"


BinanceTime::usage = 
"BinanceTime[]"


BinanceKlines::usage = 
"BinanceKlines[\"SYMBOL\", \"interval\"]
BinanceKlines[\"SYMBOL\", \"interval\", opts]"; 


BinanceTicker24hr::usage = 
"BinanceTicker24hr[]
BinanceTicker24hr[symbol]"


BinanceTickerPrice::usage = 
"BinanceTickerPrice[]
BinanceTickerPrice[symbol]"


BinanceAggTradeStream::usage = 
"BinanceAggTradeStream[symbol]"


BinanceTradeStream::usage = 
"BinanceTradeStream[symbol]"


BinanceKlinesStream::usage = 
"BinanceKlinesStream[symbol, interval]"


BinanceMiniTickerStream::usage = 
"BinanceMiniTickerStream[] - all mini tickers
BinanceMiniTickerStream[symbol]
BinanceMiniTickerStream[{symbols}]"


BinanceTickerStream::usage = 
"BinanceTickerStream[] - all tickers
BinanceTickerStream[symbol]
BinanceTickerStream[{symbols}]"


$Biannce


Begin["`Private`"]


$settings = 
ExchangeLinkSettings[]["Binance"]


$formatter = 
URLQueryFormat


$deserializer = 
JSONDeserialize


$serializer = 
JSONSerialize


getTime[] := 
1000 * UnixTime[Now] + Round[1000 * FractionalPart[Last[DateList[Now]]]]


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


Options[createSignedRequest] = {
	"Auth" :> $settings[{"APIKey", "SecretKey"}], 
	"Endpoint" :> $settings["Endpoint"], 
	"Formatter" :> $formatter, 
	"HTTPMethod" :> "GET"
}


createSignedRequest[{path__String, parameters: Rule[_String, _]...}, OptionsPattern[]] := 
Module[{
	url, query, args, argsString, signature, apiKey, secretKey, 
	auth = OptionValue["Auth"], endpoint = OptionValue["Endpoint"], 
	httpMethod = OptionValue["HTTPMethod"], format = OptionValue["Formatter"]
}, 
	If[StringQ[auth] && FileExistsQ[auth], 
		auth = ExchangeLinkSettings[auth]["Binance", {"APIKey", "SecretKey"}]]; 
	
	apiKey = auth["APIKey"]; 
	secretKey = auth["SecretKey"]; 
	
	args = format[Join[parameters, <|"timestamp" -> Now|>]]; 
	argsString = StringTrim[URLBuild[{}, args], "?"]; 
	signature = HMACSignature[secretkey, argsString, "SHA256"]; 
	query = Append[args, "signature" -> signature]; 
	url = URLBuild[{endpoint, "api", path}, query]; 
	
	Return[HTTPRequest[url, 
		<|
			Method -> httpMethod, 
			"Headers" -> {
				"X-MBX-APIKEY" -> apikey
			}
		|>
	]]
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


options[func_Symbol, opts: OptionsPattern[{}]] := 
FilterRules[{opts}, Options[func]]


binancePublic[{func_Symbol, path__String, args___Rule}, opts: OptionsPattern[{}]] := 
Block[{request, response}, 
	request = createRequest[Flatten[{path, args, options[func, opts]}], options[createRequest, opts]]; 
	response = getResponse[request, options[getResponse, opts]]; 
	Return[response]
]


Options[BinanceStreamSubscribe] = {
	"WebSocketEndpoint" :> $settings["WebSocketEndpoint"], 
	"Deserializer" :> $deserializer, 
	"EventHandler" :> Function[#], 
	"Serializer" :> $serializer
}


SyntaxInformation[BinanceStreamSubscribe] = {
	"ArgumentsPattern" -> {_, _., OptionsPattern[BinanceStreamSubscribe]}, 
	"OptionNames" -> {"\"WebSocketEndpoint\"", "\"Deserializer\"", "\"EventHandler\"", "\"Serializer\""}
}


BinanceStreamSubscribe[streams: {__String}, OptionsPattern[]] := 
Module[{url, webSocketEndpoint = OptionValue["WebSocketEndpoint"], 
	deserialize = OptionValue["Deserializer"], eventHandler = OptionValue["EventHandler"]
}, 
	url = webSocketEndpoint <> "/stream?streams=" <> StringRiffle[streams, "/"]; 
	WebSocketConnect[url, "Deserializer" -> deserialize, "EventHandler" -> eventHandler]
]


BinanceStreamSubscribe[stream_String, opts: OptionsPattern[]] := 
BinanceStreamSubscribe[{stream}, opts]


BinanceStreamSubscribe[connection_WebSocketConnectionObject, streams: {__String}, OptionsPattern[]] :=  
Module[{id, frame, serialize}, 
	id = RandonInteger[{1, 4294967295}]; 
	frame = <|"method" -> "SUBSCRIBE", "params" -> streams, "id" -> id|>; 
	serialize = OptionValue["Serializer"]; 
	WebSocketSend[connection, frame, "Serializer" -> serialize]
]


BinanceStreamSubscribe[connection_WebSocketConnectionObject, stream_String, opts: OptionsPattern[]] := 
BinanceStreamSubscribe[connection, {stream}, opts]


Options[BinanceStreamUnsubscribe] = {
	"Serializer" -> $serializer
}


BinanceStreamUnsubscribe[connection_WebSocketConnectionObject, streams: {__String}, OptionsPattern[]] :=  
Module[{id, frame, serialize}, 
	id = RandomInteger[{1, 4294967295}]; 
	frame = <|"method" -> "UNSUBSCRIBE", "params" -> streams, "id" -> id|>; 
	serialize = OptionValue["Serializer"]; 
	WebSocketSend[connection, frame, "Serializer" -> serialize]
]


BinanceStreamUnsubscribe[connection_WebSocketConnectionObject, stream_String, opts: OptionsPattern[]] := 
BinanceStreamUnsubscribe[connection, {stream}, opts]


BinanceStreamSubscribtions[connection_WebSocketConnectionObject] := 
Module[{id = RandonInteger[{1, 4294967295}], result = Missing["NotFound"]}, 
	WebSocketSend[connection, <|"method" -> "LIST_SUBSCRIPTIONS", "id" -> id|>]; 
	TimeConstrained[While[MissingQ[result], result = BinanceStreamGetResultById[connection, id]], 2]; 
	result
]


BinanceStreamGetResultById[connection_WebSocketConnectionObject, id_Integer] := 
SelectFirst[KeyExistsQ[#, "id"] && #["id"] == id&] @ Reverse @ Normal[connection["Data"]]


BinanceStreamClose[connection_WebSocketConnectionObject] := 
WebSocketClose[connection]


SyntaxInformation[BinanceAggTradeStream] = {
	"ArgumentsPattern" -> {_, OptionsPattern[BinanceStreamSubscribe]}, 
	"OptionNames" -> Map["\"" <> ToString[#] <> "\""&, Keys[Options[BinanceStreamSubscribe]]]
}


BinanceAggTradeStream[symbol_String, opts: OptionsPattern[BinanceStreamSubscribe]] := 
BinanceStreamSubscribe[ToLowerCase[symbol] <> "@aggTrade", opts]


BinanceAggTradeStream[symbols: {__String}, opts: OptionsPattern[BinanceStreamSubscribe]] := 
BinanceStreamSubscribe[Map[# <> "@aggTrade"&, symbols], opts]


SyntaxInformation[BinanceTickerStream] = {
	"ArgumentsPattern" -> {_., _., OptionsPattern[BinanceStreamSubscribe]}, 
	"OptionNames" -> Map["\"" <> ToString[#] <> "\""&, Keys[Options[BinanceStreamSubscribe]]]
};


BinanceTickerStream[opts: OptionsPattern[BinanceStreamSubscribe]] := 
BinanceStreamSubscribe["!ticker@arr", opts]


BinanceTickerStream[symbol_String, opts: OptionsPattern[BinanceStreamSubscribe]] := 
BinanceStreamSubscribe[symbol <> "@ticker", opts]


BinanceTickerStream[symbols: {__String}, opts: OptionsPattern[BinanceStreamSubscribe]] := 
BinanceStreamSubscribe[Map[# <> "@ticker"&, symbols], opts]


BinanceTickerStream[connection_WebSocketConnectionObject, opts: OptionsPattern[BinanceStreamSubscribe]] := 
BinanceStreamSubscribe[connection, "!ticker@arr", opts]


BinanceTickerStream[connection_WebSocketConnectionObject, symbols: {__String}, opts: OptionsPattern[BinanceStreamSubscribe]] := 
BinanceStreamSubscribe[connection, Map[# <> "@ticker"&, symbols], opts]


BinanceTickerStream[connection_WebSocketConnectionObject, symbol_String, opts: OptionsPattern[BinanceStreamSubscribe]] := 
BinanceTickerStream[connection, {symbol}, opts]


SyntaxInformation[BinancePing] = {
	"ArgumentsPattern" -> {}
};


BinancePing[opts: OptionsPattern[{}]] := 
binancePublic[{BinancePing, "v3", "ping"}, opts]


SyntaxInformation[BinanceTime] = {
	"ArgumentsPattern" -> {}
};


BinanceTime[opts: OptionsPattern[{}]] := 
binancePublic[{BinanceTime, "v3", "time"}, opts]


SyntaxInformation[BinanceExchangeInfo] = {
	"ArgumentsPattern" -> {_.}
};


BinanceExchangeInfo[opts: OptionsPattern[{}]] := 
binancePublic[{BinanceExchangeInfo, "v3", "exchangeInfo"}, opts];


BinanceExchangeInfo[symbol_String, opts: OptionsPattern[{}]] := 
binancePublic[{BinanceExchangeInfo, "v3", "exchangeInfo", "symbol" -> symbol}, opts];


BinanceExchangeInfo[symbols: {__String}, opts: OptionsPattern[{}]] := 
Block[{$symbols = StringReplace[ExportString[symbols, "JSON"], WhitespaceCharacter -> ""]}, 
	binancePublic[{BinanceExchangeInfo, "v3", "exchangeInfo", "symbols" -> $symbols}, opts]
];


binanceExchangeInfoByDate[date_DateObject] := 
binanceExchangeInfoByDate[date] = 
BinanceExchangeInfo[]


$BinanceExchangeInfo := 
binanceExchangeInfoByDate[Today]


SyntaxInformation[BinanceKlines] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[BinanceKlines]}, 
	"OptionNames" -> Map["\"" <> # <> "\""&, Keys[Association[Options[BinanceKlines]]]]
};


Options[BinanceKlines] = { 
	"startTime" -> Automatic, 
	"endTime" -> Automatic, 
	"limit" -> Automatic
};


BinanceKlines[symbol_, interval_, opts: OptionsPattern[{}]] := 
binancePublic[{BinanceKlines, "v3", "klines", "symbol" -> symbol, "interval" -> interval}, opts];


SyntaxInformation[BinanceAveragePrice] = {
	"ArgumentsPattern" -> {_}
};


BinanceAveragePrice[symbol_String, opts: OptionsPattern[{}]] := 
binancePublic[{BinanceAveragePrice, "v3", "avgPrice", "symbol" -> symbol}, opts];


SyntaxInformation[BinanceTicker24hr] = {
	"ArgumentsPattern" -> {_.}
};


BinanceTicker24hr[opts: OptionsPattern[{}]] := 
binancePublic[{BinanceTicker24hr, "v3", "ticker", "24hr"}, opts, 
	"deserializer" -> nativeDeserialize @* $responseDeserializer
];


BinanceTicker24hr[symbol_String, opts: OptionsPattern[{}]] := 
binancePublic[{BinanceTicker24hr, "v3", "ticker", "24hr", "symbol" -> symbol}, opts, 
	"deserializer" -> nativeDeserialize @* $responseDeserializer
];


SyntaxInformation[BinanceTickerPrice] = {
	"ArgumentsPattern" -> {_.}
};


BinanceTickerPrice[opts: OptionsPattern[{}]] := 
binancePublic[{BinanceTickerPrice, "v3", "ticker", "price"}, opts];


BinanceTickerPrice[symbol_String, opts: OptionsPattern[{}]] := 
binancePublic[{BinanceTickerPrice, "v3", "ticker", "price", "symbol" -> symbol}, opts];


End[]


EndPackage[]
