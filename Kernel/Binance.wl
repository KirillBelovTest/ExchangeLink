(* ::Package:: *)

(* ::Chapter:: *)
(*Binance*)


(* ::Section:: *)
(*Begin package*)


BeginPackage["KirillBelov`ExchangeLink`Binance`", { 
    "KirillBelov`ExchangeLink`Settings`", 
    "KirillBelov`WebSocketJLink`"
}]


(* ::Section:: *)
(*Internal references*)


Get["KirillBelov`ExchangeLink`HMAC`"]
Get["KirillBelov`ExchangeLink`Tools`"]


(* ::Section::Closed:: *)
(*Clear all*)


ClearAll["`*"]


(* ::Section::Closed:: *)
(*Market data endpoints*)


BinancePing::usage = 
"BinancePing[]"


BinanceTime::usage = 
"BinanceTime[]"


BinanceExchangeInfo::usage = 
"BinanceExchangeInfo[]
BinanceExchangeInfo[SYMBOL]
BinanceExchangeInfo[{SYMBOLS}]
BinanceExchangeInfo[SYMBOLS, opts]"


$BinanceExchangeInfo::usage = 
"$BinanceExchangeInfo"


BinanceDepth::usage = 
"BinanceDepth[SYMBOL]
BinanceDepth[SYMBOL, opts]"


BinanceTrades::usage = 
"BinanceTrades[SYMBOL]
BinanceTrades[SYMBOL, opts]"


BinanceHistoricalTrades::usage = 
"BinanceHistoricalTrades[SYMBOL]
BinanceHistoricalTrades[SYMBOL, opts]"


BinanceAggTrades::usage = 
"BinanceAggTrades[SYMBOL]
BinanceAggTrades[SYMBOL, opts]"


BinanceKlines::usage = 
"BinanceKlines[SYMBOL, interval]
BinanceKlines[SYMBOL, interval, opts]"


BinanceUIKlines::usage = 
"BinanceUIKlines[SYMBOL, interval]
BinanceUIKlines[SYMBOL, interval, opts]"


BinanceAveragePrice::usage = 
"BinanceAveragePrice[SYMBOL]"


BinanceTicker24hr::usage = 
"BinanceTicker24hr[]
BinanceTicker24hr[SYMBOL]
BinanceTicker24hr[{SYMBOLS}]
BinanceTicker24hr[SYMBOLS, opts]"


BinanceTickerPrice::usage = 
"BinanceTickerPrice[]
BinanceTickerPrice[SYMBOL]
BinanceTickerPrice[{SYMBOLS}]"


BinanceBookTicker::usage = 
"BinanceBookTicker[]
BinanceBookTicker[SYMBOL]
BinanceBookTicker[{SYMBOLS}]"


BinanceTicker::usage = 
"BinanceTicker[SYMBOL]
BinanceTicker[{SYMBOLS}]
BinanceTicker[SYMBOLS, opts]"


(* ::Section::Closed:: *)
(*Wallet endpoints*)


BinanceSystemStatus::usage = 
"BinanceSystemStatus[]"


BinanceMyCapital::usage = 
"BinanceMyCapital[]"


BinanceAccountSnapshot::usage = 
"BinanceAccountSnapshot[type]"


BinanceMyAsset::usage = 
"BinanceMyAsset[]
BinanceMyAsset[ASSET]"


(* ::Section::Closed:: *)
(*Spot trade endpoints*)


BinanceOrderTest::usage = 
"BinanceOrderTest[SYMBOL, SIDE, TYPE, opts] check that order with the same parameters can be created"


BinanceOrderCreate::usage = 
"BinanceOrderCreate[SYMBOL, SIDE, TYPE, opts] create order"


BinanceBuy::usage = 
"BinanceBuy[SYMBOL, quantity] create MARKET order
BinanceBuy[SYMBOL, quantity, price] create LIMIT order"


BinanceSell::usage = 
"BinanceSell[SYMBOL, quantity] create MARKET order
BinanceSell[SYMBOL, quantity, price] create LIMIT order"


BinanceOrderCancel::usage = 
"BinanceOrderCancel[SYMBOL, orderID] cancel order useing integer ID
BinanceOrderCancel[SYMBOL, \"origClientOrderID\"] cancel order using string GUID"


BinanceOrderCancelAll::usage = 
"BinanceOrderCancelAll[SYMBOL] - cancel all orders of the specific currency pair"


BinanceOrderGet::usage = 
"BinanceOrderGet[SYMBOL, orderID] returns info about order by integer ID
BinanceOrderGet[SYMBOL, \"origClientOrderID\"] returns info about order using string GUID"


BinanceOrderCancelReplace::usage = 
"BinanceOrderCancelReplace[SYMBOL, side, type, cancelReplaceMode, orderID]
BinanceOrderCancelReplace[SYMBOL, side, type, cancelReplaceMode, \"cancelOrigClientOrderID\"]"


BinanceOrderGetOpened::usage = 
"BinanceOrderGetOpened[SYMBOL]"


BinanceOrderGetAll::usage = 
"BinanceOrderGetAll[SYMBOL]"


BinanceMyTrades::usage = 
"BinanceMyTrades[SYMBOL]"


BinanceOrderCount::usage = 
"BinanceOrderCount[]"


(* ::Section::Closed:: *)
(*Binance websocket market streams*)


BinanceStreamCreate::usage = 
"BinanceStreamCreate[stream]
BinanceStreamCreate[{streams}]"


BinanceStreamSubscribe::usage = 
"BinanceStreamSubscribe[connections, stream]
BinanceStreamSubscribe[connections, {streams}]"


BinanceStreamUnsubscribe::usage = 
"BinanceStreamUnsubscribe[connection]
BinanceStreamUnsubscribe[connection, stream]
BinanceStreamUnsubscribe[connection, {streams}]"


BinanceStreamSubscribtions::usage = 
"BinanceStreamSubscribtions[]"


BinanceAggTradesStream::usage = 
"BinanceAggTradesStream[SYMBOL]
BinanceAggTradesStream[connection, SYMBOL]"


BinanceTradesStream::usage = 
"BinanceTradesStream[SYMBOL]
BinanceTradesStream[connection, SYMBOL]"


BinanceKlinesStream::usage = 
"BinanceKlinesStream[SYMBOL, interval]
BinanceKlinesStream[connection, SYMBOL, interval]"


BinanceMiniTickerStream::usage = 
"BinanceMiniTickerStream[] - all mini tickers
BinanceMiniTickerStream[SYMBOL]
BinanceMiniTickerStream[{SYMBOLS}]
BinanceMiniTickerStream[connection]
BinanceMiniTickerStream[connection, SYMBOLS]"


BinanceTickerStream::usage = 
"BinanceTickerStream[] - all tickers
BinanceTickerStream[All -> windowSize]
BinanceTickerStream[SYMBOL]
BinanceTickerStream[SYMBOL -> windowSize]
BinanceTickerStream[{SYMBOLS}]
BinanceTickerStream[connection]
BinanceTickerStream[connection, SYMBOLS]"


BinanceBookTickerStream::usage = 
"BinanceBookTickerStream[]
BinanceBookTickerStream[SYMBOLS]
BinanceBookTickerStream[{SYMBOLS}]
BinanceBookTickerStream[connection]
BinanceBookTickerStream[connection, SYMBOLS]"


(* ::Section::Closed:: *)
(*Binance websocket user data streams*)


BinanceUserDataStreamCreateKey::usage = 
"BinanceUserDataStreamCreateKey[]"


BinanceUserDataStreamPing::usage = 
"BinanceUserDataStreamPing[listenKey]"


BinanceUserDataStreamClose::usage = 
"BinanceUserDataStreamClose[listenKey]"


BinanceUserDataStream::usage = 
"BinanceUserDataStream[]"


(* ::Section::Closed:: *)
(*Private*)


Begin["`Private`"]


(* ::Section::Closed:: *)
(*JSON values to expressions converter*)


$thisYearTimestamp = 
Timestamp[DateObject[Now, "Year"]]


$oneYearMilliseconds = 
QuantityMagnitude[UnitConvert[Quantity[1, "Years"], "Milliseconds"]]


toExpr[timestamp_Integer] /; 
$thisYearTimestamp - 3 * $oneYearMilliseconds < timestamp && 
timestamp < $thisYearTimestamp + 2 * $oneYearMilliseconds := 
FromUnixTime[timestamp / 1000]


toExpr[number_String] /; 
StringMatchQ[number, NumberString] := 
ToExpression[number]


toExpr[list_List] := 
Map[toExpr, list]


toExpr[assoc_Association] := 
<|KeyValueMap[toExpr, assoc]|>


toExpr[expr_?AtomQ] := 
expr


toExpr[key_String, value_] := 
key -> toExpr[value]


(* ::Section::Closed:: *)
(*Encoder*)


encode[assoc_?AssociationQ] := 
KeyValueMap[encode] @ assoc


encode[key_Symbol, value_] := 
encode[ToString[key], value]


encode[key_String, value_] := 
key -> encode[value]


encode[date_DateObject] := 
Timestamp[date]


encode[list_List] := 
StringDelete[ExportString[list, "RawJSON"], WhitespaceCharacter]


encode[number_Real] := 
StringJoin[StringSplit[ToString[DecimalForm[number]], "."] /. "" -> "0"]


encode[expr_] := 
ToString[expr]


(* ::Section::Closed:: *)
(*Deserializer*)


deserialize[body_String] := 
toExpr[ImportString[body, "RawJSON"]]


(* ::Section::Closed:: *)
(*Serializer*)


serialize[expr_] := 
ExportString[expr, "RawJSON"]


(* ::Section::Closed:: *)
(*Common public method*)


Options[publicMethod] = {
    "HTTPMethod" :> "GET", 
    "V" :> "v3", 
    "API" :> "api", 
    "Settings" :> $ExchangeLinkSettings, 
    "Encoder" :> encode, 
    "Deserializer" :> deserialize
}


publicMethod[{method__String, args_?AssociationQ}, OptionsPattern[]] := 
Module[{httpMethod, settings, endpoint, url, request, response, body, result, 
	encoder, deserializer, parameters, v, api}, 
	
	v = OptionValue["V"]; 
	api = OptionValue["API"]; 
    httpMethod = OptionValue["HTTPMethod"]; 
	settings = OptionValue["Settings"]; 
    endpoint = settings["Binance", "Endpoint"]; 
    encoder = OptionValue["Encoder"]; 
    deserializer = OptionValue["Deserializer"]; 


    parameters = encoder[DeleteCases[args, Automatic | Null]]; 
    url = URLBuild[{endpoint, api, v, method}, parameters]; 
    request = HTTPRequest[url, <|Method -> httpMethod|>]; 
    
    response = URLRead[request]; 
    saveToHistory[request, response]; 
    
    body = response["Body"]; 
    result = deserializer[body]; 

    Return[result]
]


publicMethod[{method__String, args: OptionsPattern[{}]}, opts: OptionsPattern[{}]] := 
publicMethod[
	{
		method, 
		<|FilterRules[Flatten[{args}], Except[Options[publicMethod]]]|>
	}, 
	FilterRules[Flatten[{opts}], Options[publicMethod]]
]


(* ::Section::Closed:: *)
(*Signed request*)


Options[signedMethod] = {
    "HTTPMethod" :> "POST", 
    "V" :> "v3", 
    "API" :> "api", 
    "recvWindow" :> 5000, 
    "timestamp" :> Timestamp[], 
    "signature" :> Automatic, 
    "Settings" :> $ExchangeLinkSettings, 
    "Encoder" :> encode, 
    "Serializer" :> URLQueryEncode, 
    "Deserializer" :> deserialize
}


signedMethod[{method__String, args_?AssociationQ}, OptionsPattern[]] := 
Module[{httpMethod, settings, endpoint, url, request, response, body, result, serializer, 
	encoder, deserializer, parameters, v, api, recvWindow, apiKey, secretKey, 
    timestamp, signature, requestBody}, 
	
	v = OptionValue["V"]; 
	api = OptionValue["API"]; 
    httpMethod = OptionValue["HTTPMethod"]; 
    recvWindow = OptionValue["recvWindow"]; 
    timestamp = OptionValue["timestamp"]; 
	settings = OptionValue["Settings"]; 
    endpoint = settings["Binance", "Endpoint"]; 
    apiKey = settings["Binance", "APIKey"]; 
    secretKey = settings["Binance", "SecretKey"]; 
    encoder = OptionValue["Encoder"]; 
    deserializer = OptionValue["Deserializer"]; 
    serializer = OptionValue["Serializer"]; 
    signature = OptionValue["signature"]; 

    parameters = Join[args, <|"timestamp" -> timestamp, "recvWindow" -> recvWindow|>];   
    requestBody = serializer[encoder[parameters]]; 
    If[signature == Automatic, signature = HMAC[requestBody, secretKey, "SHA256"]]; 
    AppendTo[parameters, "signature" -> signature]; 
    url = URLBuild[{endpoint, api, v, method}, encoder[parameters]]; 
    
    request = HTTPRequest[url, <|
		Method -> httpMethod, 
		"Headers" -> {"X-MBX-APIKEY" -> apiKey}
	|>]; 
    
    response = URLRead[request]; 
    saveToHistory[request, response]; 
    
    body = response["Body"]; 
    result = deserializer[body]; 

    Return[result]
]


signedMethod[{method__String, args: OptionsPattern[{}]}, opts: OptionsPattern[{}]] := 
signedMethod[
	{
		method, 
		<|FilterRules[Flatten[{args}], Except[Options[signedMethod]]]|>
	}, 
	FilterRules[Flatten[{opts}], Options[signedMethod]]
]


(* ::Section::Closed:: *)
(*Market data implementation*)


SyntaxInformation[BinancePing] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> OptionNames[publicMethod]
}


BinancePing[opts: OptionsPattern[publicMethod]] := 
publicMethod[{"ping"}, opts]


SyntaxInformation[BinanceTime] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> OptionNames[publicMethod]
}


BinanceTime[opts: OptionsPattern[publicMethod]] := 
publicMethod[{"time"}, opts]


Options[BinanceExchangeInfo] = {
	"permissions" -> Automatic
}


SyntaxInformation[BinanceExchangeInfo] = {
	"ArgumentsPattern" -> {_., OptionsPattern[]}, 
	"OptionNames" -> OptionNames[publicMethod, BinanceExchangeInfo]
}


BinanceExchangeInfo[opts: OptionsPattern[{publicMethod, BinanceExchangeInfo}]] := 
publicMethod[{"exchangeInfo", opts}, opts]


BinanceExchangeInfo[symbol_String, opts: OptionsPattern[{publicMethod, BinanceExchangeInfo}]] := 
publicMethod[{"exchangeInfo", "symbol" -> symbol, opts}, opts]


BinanceExchangeInfo[symbols: {__String}, opts: OptionsPattern[{publicMethod, BinanceExchangeInfo}]] := 
publicMethod[{"exchangeInfo", "symbols" -> symbols, opts}, opts]


$BinanceExchangeInfo := 
Cache[BinanceExchangeInfo[], 60 * 60]


Options[BinanceDepth] = {
	"limit" -> Automatic
}


SyntaxInformation[BinanceDepth] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[publicMethod, BinanceDepth]
}


BinanceDepth[symbol_String, opts: OptionsPattern[{publicMethod, BinanceDepth}]] := 
publicMethod[{"depth", "symbol" -> symbol, opts}, opts]


Options[BinanceTrades] = {
	"limit" -> Automatic
}


SyntaxInformation[BinanceTrades] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[publicMethod, BinanceTrades]
}


BinanceTrades[symbol_String, opts: OptionsPattern[{publicMethod, BinanceTrades}]] := 
publicMethod[{"trades", "symbol" -> symbol, opts}, opts]


Options[BinanceHistoricalTrades] = {
	"limit" -> Automatic, 
	"fromId" -> Automatic
}


SyntaxInformation[BinanceHistoricalTrades] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[publicMethod, BinanceHistoricalTrades]
}


BinanceHistoricalTrades[symbol_String, opts: OptionsPattern[{publicMethod, BinanceHistoricalTrades}]] := 
publicMethod[{"historicalTrades", "symbol" -> symbol, opts}, opts]


Options[BinanceAggTrades] = {
	"limit" -> Automatic, 
	"fromId" -> Automatic, 
	"startTime" -> Automatic, 
	"endTime" -> Automatic
}


SyntaxInformation[BinanceAggTrades] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[publicMethod, BinanceAggTrades]
}


BinanceAggTrades[symbol_String, opts: OptionsPattern[{publicMethod, BinanceAggTrades}]] := 
publicMethod[{"aggTrades", "symbol" -> symbol, opts}, opts]


Options[BinanceKlines] = {
	"limit" -> Automatic, 
	"startTime" -> Automatic, 
	"endTime" -> Automatic
}


SyntaxInformation[BinanceKlines] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[publicMethod, BinanceKlines]
}


BinanceKlines[symbol_String, interval_String, opts: OptionsPattern[{publicMethod, BinanceKlines}]] := 
publicMethod[{"klines", "symbol" -> symbol, "interval" -> interval, opts}, opts]


Options[BinanceUIKlines] = {
	"limit" -> Automatic, 
	"startTime" -> Automatic, 
	"endTime" -> Automatic
}


SyntaxInformation[BinanceUIKlines] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[publicMethod, BinanceUIKlines]
}


BinanceUIKlines[symbol_String, interval_String, opts: OptionsPattern[{publicMethod, BinanceUIKlines}]] := 
publicMethod[{"uiKlines", "symbol" -> symbol, "interval" -> interval, opts}, opts]


SyntaxInformation[BinanceAveragePrice] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[publicMethod]
}


BinanceAveragePrice[symbol_String, opts: OptionsPattern[publicMethod]] := 
publicMethod[{"avgPrice", "symbol" -> symbol}, opts]


Options[BinanceTicker24hr] = {
	"type" -> Automatic
}


SyntaxInformation[BinanceTicker24hr] = {
	"ArgumentsPattern" -> {_., OptionsPattern[]}, 
	"OptionNames" -> OptionNames[publicMethod, BinanceTicker24hr]
}


BinanceTicker24hr[opts: OptionsPattern[{publicMethod, BinanceTicker24hr}]] := 
publicMethod[{"ticker", "24hr", opts}, opts]


BinanceTicker24hr[symbol_String, opts: OptionsPattern[{publicMethod, BinanceTicker24hr}]] := 
publicMethod[{"ticker", "24hr", "symbol" -> symbol, opts}, opts]


BinanceTicker24hr[symbols: {__String}, opts: OptionsPattern[{publicMethod, BinanceTicker24hr}]] := 
publicMethod[{"ticker", "24hr", "symbols" -> symbols, opts}, opts]


SyntaxInformation[BinanceTickerPrice] = {
	"ArgumentsPattern" -> {_., OptionsPattern[]}, 
	"OptionNames" -> OptionNames[publicMethod]
}


BinanceTickerPrice[opts: OptionsPattern[publicMethod]] := 
publicMethod[{"ticker", "price"}, opts]


BinanceTickerPrice[symbol_String, opts: OptionsPattern[publicMethod]] := 
publicMethod[{"ticker", "price", "symbol" -> symbol}, opts]


BinanceTickerPrice[symbols: {__String}, opts: OptionsPattern[publicMethod]] := 
publicMethod[{"ticker", "price", "symbols" -> symbols}, opts]


SyntaxInformation[BinanceBookTicker] = {
	"ArgumentsPattern" -> {_., OptionsPattern[]}, 
	"OptionNames" -> OptionNames[publicMethod]
}


BinanceBookTicker[opts: OptionsPattern[publicMethod]] := 
publicMethod[{"ticker", "bookTicker"}, opts]


BinanceBookTicker[symbol_String, opts: OptionsPattern[publicMethod]] := 
publicMethod[{"ticker", "bookTicker", "symbol" -> symbol}, opts]


BinanceBookTicker[symbols: {__String}, opts: OptionsPattern[publicMethod]] := 
publicMethod[{"ticker", "bookTicker", "symbols" -> symbols}, opts]


Options[BinanceTicker] = {
	"windowSize" -> Automatic, 
	"type" -> Automatic
}


SyntaxInformation[BinanceTicker] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[publicMethod, BinanceTicker]
}


BinanceTicker[symbol_String, opts: OptionsPattern[{publicMethod, BinanceTicker}]] := 
publicMethod[{"ticker", "symbol" -> symbol, opts}, opts]


BinanceTicker[symbols: {__String}, opts: OptionsPattern[{publicMethod, BinanceTicker}]] := 
publicMethod[{"ticker", "symbols" -> symbols, opts}, opts]


(* ::Section::Closed:: *)
(*Wallet implementation*)


SyntaxInformation[BinanceSystemStatus] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> OptionNames[publicMethod]
}


BinanceSystemStatus[opts: OptionsPattern[publicMethod]] := 
publicMethod[{"system", "status"}, opts, "V" -> "v1", "API" -> "sapi"]


SyntaxInformation[BinanceMyCapital] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod]
}


BinanceMyCapital[opts: OptionsPattern[signedMethod]] := 
signedMethod[{"capital", "config", "getall"}, opts, "API" -> "sapi", "V" -> "v1", "HTTPMethod" -> "GET", 
	"Deserializer" -> (deserialize[ExportString[#, "Text"]]&)]


Options[BinanceAccountSnapshot] = {
	"startTime" -> Automatic, 
	"endTime" -> Automatic, 
	"limit" -> Automatic
}


SyntaxInformation[BinanceAccountSnapshot] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod, BinanceAccountSnapshot]
}


BinanceAccountSnapshot[type_String, opts: OptionsPattern[{BinanceAccountSnapshot, signedMethod}]] := 
signedMethod[{"accountSnapshot", "type" -> type, opts}, 
	opts, "API" -> "sapi", "V" -> "v1", "HTTPMethod" -> "GET"]


Options[BinanceMyAsset] = {
	"needBtcValuation" -> Automatic
}


SyntaxInformation[BinanceMyAsset] = {
	"ArgumentsPattern" -> {_., OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod, BinanceMyAsset]
}


BinanceMyAsset[asset_String: Automatic, opts: OptionsPattern[{signedMethod, BinanceMyAsset}]] := 
signedMethod[{"asset", "getUserAsset", "asset" -> asset, opts}, opts, "API" -> "sapi"]


(* ::Section:: *)
(*Binance spot trade implementation*)


Options[BinanceOrderTest] = {
	"timeInForce" -> Automatic, 
	"quantity" -> Automatic, 
	"price" -> Automatic, 
	"quoteOrderQty" -> Automatic, 
	"newClientOrderId" -> Automatic, 
	"strategyId" -> Automatic, 
	"strategyType" -> Automatic, 
	"stopPrice" -> Automatic, 
	"trailingDelta" -> Automatic, 
	"icebergQty" -> Automatic, 
	"newOrderRespType" -> Automatic
}


SyntaxInformation[BinanceOrderTest] = {
	"ArgumentsPattern" -> {_, _, _, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod, BinanceOrderTest]
}


BinanceOrderTest[symbol_String?binanceSymbolQ, side_String, orderType_String, 
	opts: OptionsPattern[{signedMethod, BinanceOrderTest}]] := 
signedMethod[{"order", "test", "symbol" -> symbol, "side" -> side, "type" -> orderType, opts}, opts, 
	"Encoder" -> binanceEncodeOrder[symbol]]


Options[BinanceOrderCreate] = {
	"timeInForce" -> Automatic, 
	"quantity" -> Automatic, 
	"price" -> Automatic, 
	"quoteOrderQty" -> Automatic, 
	"newClientOrderId" -> Automatic, 
	"strategyId" -> Automatic, 
	"strategyType" -> Automatic, 
	"stopPrice" -> Automatic, 
	"trailingDelta" -> Automatic, 
	"icebergQty" -> Automatic, 
	"newOrderRespType" -> Automatic
}


SyntaxInformation[BinanceOrderCreate] = {
	"ArgumentsPattern" -> {_, _, _, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod, BinanceOrderCreate]
}


BinanceOrderCreate[symbol_String, side_String, orderType_String, 
	opts: OptionsPattern[{signedMethod, BinanceOrderCreate}]] := 
signedMethod[{"order", "symbol" -> symbol, "side" -> side, "type" -> orderType, opts}, opts]


SyntaxInformation[BinanceBuy] = {
	"ArgumentsPattern" -> {_, _, _., OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod, BinanceOrder]
}


BinanceBuy[symbol_String, quantity_?NumericQ, price_?NumericQ, 
	opts: OptionsPattern[{signedMethod, BinanceOrderCreate}]] := 
BinanceOrderCreate[symbol, "BUY", "LIMIT", quantity, opts]


BinanceBuy[symbol_String, quantity_?NumericQ, opts: OptionsPattern[{signedMethod, BinanceOrderCreate}]] := 
BinanceOrderCreate[symbol, "BUY", "MARKET", quantity, opts, "timeInForce" -> "GTC"]


SyntaxInformation[BinanceSell] = {
	"ArgumentsPattern" -> {_, _, _., OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod, BinanceOrderCreate]
}


BinanceSell[symbol_String, quantity_?NumericQ, price_?NumericQ, 
	opts: OptionsPattern[{signedMethod, BinanceOrderCreate}]] := 
BinanceOrderCreate[symbol, "SELL", "LIMIT", quantity, opts]


BinanceSell[symbol_String, quantity_?NumericQ, opts: OptionsPattern[{signedMethod, BinanceOrderCreate}]] := 
BinanceOrderCreate[symbol, "SELL", "MARKET", quantity, opts, "timeInForce" -> "GTC"]


Options[BinanceOrderCancel] = {
	"newClientOrderId" -> Automatic
}


SyntaxInformation[BinanceOrderCancel] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod, BinanceOrderCancel]
}


BinanceOrderCancel[symbol_String, orderID_Integer, opts: OptionsPattern[{signedMethod, BinanceOrderCancel}]] := 
signedMethod[{"order", "symbol" -> symbol, "orderId" -> orderID, opts}, opts, "HTTPMethod" -> "DELETE"]


BinanceOrderCancel[symbol_String, origClientOrderID_String, 
	opts: OptionsPattern[{signedMethod, BinanceOrderCancel}]] := 
signedMethod[{"order", "symbol" -> symbol, "origClientOrderId" -> origClientOrderID, opts}, 
	opts, "HTTPMethod" -> "DELETE"]


SyntaxInformation[BinanceOrderCancelAll] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod]
}


BinanceOrderCancelAll[symbol_String, opts: OptionsPattern[signedMethod]] := 
signedMethod[{"openOrders", "symbol" -> symbol}, opts, "HTTPMethod" -> "DELETE"]


SyntaxInformation[BinanceOrderGet] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod]
}


BinanceOrderGet[symbol_String, orderID_Integer, opts: OptionsPattern[signedMethod]] := 
signedMethod[{"order", "symbol" -> symbol, "orderId" -> orderID, opts}, opts, "HTTPMethod" -> "GET"]


BinanceOrderGet[symbol_String, origClientOrderID_String, opts: OptionsPattern[signedMethod]] := 
signedMethod[{"order", "symbol" -> symbol, "origClientOrderId" -> origClientOrderID}, opts, "HTTPMethod" -> "GET"]


Options[BinanceOrderCancelReplace] = {
	"timeInForce" -> Automatic, 
	"quantity" -> Automatic, 
	"quoteOrderQty" -> Automatic, 
	"price" -> Automatic, 
	"cancelNewClientOrderId" -> Automatic, 
	"newClientOrderId" -> Automatic, 
	"strategyId" -> Automatic, 
	"strategyType" -> Automatic, 
	"stopPrice" -> Automatic, 
	"trailingDelta" -> Automatic, 
	"icebergQty" -> Automatic, 
	"newOrderRespType" -> Automatic
}


SyntaxInformation[BinanceOrderCancelReplace] = {
	"ArgumentsPattern" -> {_, _, _, _, _, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod, BinanceOrderCancelReplace]
}


BinanceOrderCancelReplace[symbol_String, side_String, type_String, cancelReplaceMode_String, 
	cancelOrigClientOrderID_String, opts: OptionsPattern[{signedMethod, BinanceOrderCancelReplace}]] := 
signedMethod[{"order", "cancelReplace", "symbol" -> symbol, "side" -> side, "type" -> type, 
	"cancelReplaceMode" -> cancelReplaceMode, "cancelOrigClientOrderId" -> cancelOrigClientOrderID, opts}, opts]


BinanceOrderCancelReplace[symbol_String, side_String, type_String, cancelReplaceMode_String, 
	cancelOrderID_Integer, opts: OptionsPattern[{signedMethod, BinanceOrderCancelReplace}]] := 
signedMethod[{"order", "cancelReplace", "symbol" -> symbol, "side" -> side, "type" -> type, 
	"cancelReplaceMode" -> cancelReplaceMode, "cancelOrderId" -> cancelOrderID, opts}, opts]


SyntaxInformation[BinanceOrderGetOpened] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod]
}


BinanceOrderGetOpened[symbol_String, opts: OptionsPattern[signedMethod]] := 
signedMethod[{"openOrders", "symbol" -> symbol}, opts, "HTTPMethod" -> "GET"]


Options[BinanceOrderGetAll] = {
	"orderId" -> Automatic, 
	"startTime" -> Automatic, 
	"endTime" -> Automatic, 
	"limit" -> Automatic
}


SyntaxInformation[BinanceOrderGetAll] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod, BinanceOrderGetAll]
}


BinanceOrderGetAll[symbol_String, opts: OptionsPattern[{signedMethod, BinanceOrderGetAll}]] := 
signedMethod[{"openOrders", "symbol" -> symbol, opts}, opts, "HTTPMethod" -> "GET"]


Options[BinanceMyTrades] = {
	"orderId" -> Automatic, 
	"startTime" -> Automatic, 
	"endTime" -> Automatic, 
	"fromId" -> Automatic, 
	"limit" -> Automatic
}


SyntaxInformation[BinanceMyTrades] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod, BinanceMyTrades]
}


BinanceMyTrades[symbol_String, opts: OptionsPattern[{signedMethod, BinanceMyTrades}]] := 
signedMethod[{"myTrades", "symbol" -> symbol, opts}, opts, "HTTPMethod" -> "GET"]


SyntaxInformation[BinanceOrderCount] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod]
}


BinanceOrderCount[opts: OptionsPattern[signedMethod]] := 
signedMethod[{"rateLimit", "order"}, opts, "HTTPMethod" -> "GET"]


(* ::Section::Closed:: *)
(*Binance websocket market streams implementation *)


Options[BinanceStreamCreate] = {
	"Settings" :> $ExchangeLinkSettings, 
	"Deserializer" :> deserialize, 
	"EventHandler" :> Print
}


SyntaxInformation[BinanceStreamCreate] = {
	"ArgumentsPattern" -> {_, _., OptionsPattern[]}, 
	"OptionNames" -> OptionNames[BinanceStreamCreate]
}


BinanceStreamCreate[stream_String, OptionsPattern[]] := 
Module[{address, settings, webSocketEndpoint, deserializer, eventHandler}, 
	deserializer = OptionValue["Deserializer"]; 
	eventHandler = OptionValue["EventHandler"]; 
	settings = OptionValue["Settings"]; 
	webSocketEndpoint = settings["Binance", "WebSocketEndpoint"]; 
	address = webSocketEndpoint <> "/ws/" <> stream; 
	
	WebSocketConnect[address, "Deserializer" -> deserializer, "EventHandler" -> eventHandler]
]


BinanceStreamCreate[streams: {__String}, OptionsPattern[]] := 
Module[{address, settings, webSocketEndpoint, deserializer, eventHandler, connection}, 
	deserializer = OptionValue["Deserializer"]; 
	eventHandler = OptionValue["EventHandler"]; 
	settings = OptionValue["Settings"]; 
	webSocketEndpoint = settings["Binance", "WebSocketEndpoint"]; 
	address = webSocketEndpoint <> "/stream?streams=" <> StringRiffle[streams, "/"]; 
	
	WebSocketConnect[address, "Deserializer" -> deserializer, "EventHandler" -> eventHandler]	
]


Options[BinanceStreamSubscribe] = {
	"Serializer" -> serialize
}


BinanceStreamSubscribe[connection_WebSocketConnectionObject, streams_List, OptionsPattern[]] := 
Module[{serializer, id, frame}, 
	serializer = OptionValue["Serializer"]; 
	id = RandomInteger[10^9]; 
	
	frame = <|
		"method" -> "SUBSCRIBE", 
		"params" -> streams, 
		"id" -> id
	|>; 
	
	WebSocketSend[connection, frame, "Serializer" -> serializer]
]


BinanceStreamSubscribe[connection_WebSocketConnectionObject, stream_String, opts: OptionsPattern[]] := 
BinanceStreamSubscribe[connection, {stream}, opts]


Options[BinanceStreamUnsubscribe] = {
	"Serializer" -> serialize
}


BinanceStreamUnsubscribe[connection_WebSocketConnectionObject, streams: {__String}, opts: OptionsPattern[]] := 
Module[{serializer, id, frame}, 
	serializer = OptionValue["Serializer"]; 
	id = RandomInteger[10^9]; 
	frame = <|
		"method" -> "UNSUBSCRIBE", 
		"params" -> streams, 
		"id" -> id
	|>; 
	
	WebSocketSend[connection, frame, "Serializer" -> serializer]
]


BinanceStreamUnsubscribe[connection_WebSocketConnectionObject, stream_String, opts: OptionsPattern[]] := 
BinanceStreamUnsubscribe[connection, {stream}, opts]


BinanceAggTradesStream[symbols: _String | {__String}, opts: OptionsPattern[BinanceStreamCreate]] := 
BinanceStreamCreate[Map[# <> "@aggTrade"&] @ Flatten[{symbols}], opts]


BinanceAggTradesStream[connection_WebSocketConnectionObject, symbols: _String | {__String}, 
	opts: OptionsPattern[BinanceStreamSubscribe]] := 
BinanceStreamSubscribe[connection, Map[# <> "@aggTrade"&] @ Flatten[{symbols}], opts]


BinanceTradesStream[symbols: _String | {__String}, opts: OptionsPattern[BinanceStreamCreate]] := 
BinanceStreamCreate[Map[# <> "@trade"&] @ Flatten[{symbols}], opts]


BinanceTradesStream[connection_WebSocketConnectionObject, symbols: _String | {__String}, 
	opts: OptionsPattern[BinanceStreamSubscribe]] := 
BinanceStreamSubscribe[connection, Map[# <> "@trade"&] @ Flatten[{symbols}], opts]


BinanceKlinesStream[symbols: _String | {__String}, intervals: _String | {__String}, 
	opts: OptionsPattern[BinanceStreamCreate]] /; Length[Flatten[{symbols}]] == Length[Flatten[{intervals}]] := 
BinanceStreamCreate[MapThread[#1 <> "@kline_" <> #2&] @ {Flatten[{symbols}], Flatten[{intervals}]}, opts]


BinanceKlinesStream[connection_WebSocketConnectionObject, 
	symbols: _String | {__String}, intervals: _String | {__String}, 
	opts: OptionsPattern[BinanceStreamSubscribe]] /; Length[Flatten[{symbols}]] == Length[Flatten[{intervals}]] := 
BinanceStreamSubscribe[connection, 
	MapThread[#1 <> "@kline_" <> #2&] @ {Flatten[{symbols}], Flatten[{intervals}]}, opts]


BinanceMiniTickerStream[opts: OptionsPattern[BinanceStreamCreate]] := 
BinanceStreamCreate[{"!miniTicker@arr"}, opts]


BinanceMiniTickerStream[connection_WebSocketConnectionObject, opts: OptionsPattern[BinanceStreamCreate]] := 
BinanceStreamSubscribe[connection, {"!miniTicker@arr"}, opts]


BinanceMiniTickerStream[symbols: _String | {__String}, opts: OptionsPattern[BinanceStreamCreate]] := 
BinanceStreamCreate[Map[# <> "@miniTicker"&] @ Flatten[{symbols}], opts]


BinanceMiniTickerStream[connection_WebSocketConnectionObject, symbols: _String | {__String}, 
	opts: OptionsPattern[BinanceStreamSubscribe]] := 
BinanceStreamSubscribe[connection, Map[# <> "@miniTicker"&] @ Flatten[{symbols}], opts]


BinanceTickerStream[opts: OptionsPattern[BinanceStreamCreate]] := 
BinanceStreamCreate[{"!ticker@arr"}, opts]


BinanceTickerStream[connection_WebSocketConnectionObject, opts: OptionsPattern[BinanceStreamCreate]] := 
BinanceStreamSubscribe[connection, {"!ticker@arr"}, opts]


BinanceTickerStream[symbols: _String | {__String}, opts: OptionsPattern[BinanceStreamCreate]] := 
BinanceStreamCreate[Map[# <> "@ticker"&] @ Flatten[{symbols}], opts]


BinanceTickerStream[connection_WebSocketConnectionObject, symbols: _String | {__String}, 
	opts: OptionsPattern[BinanceStreamSubscribe]] := 
BinanceStreamSubscribe[connection, Map[# <> "@ticker"&] @ Flatten[{symbols}], opts]


BinanceTickerStream[symbols: _String | {__String}, windowSizes: _String | {__String}, 
	opts: OptionsPattern[BinanceStreamCreate]] /; Length[Flatten[{symbols}]] == Length[Flatten[{windowSizes}]] := 
BinanceStreamCreate[MapThread[#1 <> "@ticker_" <> #2&] @ {Flatten[{symbols}], Flatten[{windowSizes}]}, opts]


BinanceTickerStream[connection_WebSocketConnectionObject, 
	symbols: _String | {__String}, windowSizes: _String | {__String}, 
	opts: OptionsPattern[BinanceStreamSubscribe]] /; Length[Flatten[{symbols}]] == Length[Flatten[{windowSizes}]] := 
BinanceStreamSubscribe[connection, 
	MapThread[#1 <> "@ticker_" <> #2&] @ {Flatten[{symbols}], Flatten[{windowSizes}]}, opts]



(* ::Section::Closed:: *)
(*Binance websocket user data rest methods implementation*)


SyntaxInformation[BinanceUserDataStreamCreateKey] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNanes" -> OptionNames[signedMethod]
}


BinanceUserDataStreamCreateKey[opts: OptionsPattern[signedMethod]] := 
signedMethod[{"userDataStream"}, opts, "timestamp" -> Null, "recvWindow" -> Null, "signature" -> Null]


SyntaxInformation[BinanceUserDataStreamPing] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNanes" -> OptionNames[signedMethod]
}


BinanceUserDataStreamPing[listenKey_String, opts: OptionsPattern[signedMethod]] := 
signedMethod[{"userDataStream", "listenKey" -> listenKey}, opts, 
	"HTTPMethod" -> "PUT", "timestamp" -> Null, "recvWindow" -> Null, "signature" -> Null]


SyntaxInformation[BinanceUserDataStreamClose] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNanes" -> OptionNames[signedMethod]
}


BinanceUserDataStreamClose[listenKey_String, opts: OptionsPattern[signedMethod]] := 
signedMethod[{"userDataStream", "listenKey" -> listenKey}, opts, 
	"HTTPMethod" -> "DELETE", "timestamp" -> Null, "recvWindow" -> Null, "signature" -> Null]


SyntaxInformation[BinanceUserDataStream] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNanes" -> OptionNames[signedMethod, BinanceStreamCreate]
}


BinanceUserDataStream[opts: OptionsPattern[{signedMethod, BinanceStreamCreate}]] := 
Module[{listenKey}, 
	listenKey = BinanceUserDataStreamCreateKey[filter[opts]]["listenKey"]; 
	<|"listemKey" -> listenKey, "stream" -> BinanceStreamCreate[listenKey, filter[opts]]|>
]


(* ::Section::Closed:: *)
(*End private*)


End[] (*`Prvate`*)


(* ::Section::Closed:: *)
(*End package*)


EndPackage[] (*KirillBelov`ExchangeLink`Binance`*)
