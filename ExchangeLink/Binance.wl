(* ::Package:: *)

(* ::Title:: *)
(*Binance*)


(* ::Section:: *)
(*Info*)


(* :Title: Binance *)
(* :Context: ExchangeLink`Binance` *)
(* :Author: Kirill Belov*)


(* ::Section:: *)
(*Begin package*)


BeginPackage["ExchangeLink`Binance`", {"ExchangeLink`Tools`Auth`"}]


(* ::Section:: *)
(*Clear names*)


Unprotect["`*"]
ClearAll["`*"]


(* ::Section:: *)
(*Public names declaration*)


BinancePing::usage = 
"BinancePing[]"


BinanceTime::usage = 
"BinanceTime[]"


BinanceExchangeInfo::usage = 
"BinanceExchangeInfo[]"


BinanceDepth::usage = 
"BinanceDepth[symbol]"


BinancePrice::usage = 
"BinancePrice[symbol]"


BinanceAveragePrice::usage = 
"BinanceAveragePrice[symbol]"


BinanceTicker::usage = 
"BinanceTicker[]
BinanceTicker[symbol]"


BinanceBookTicker::usage = 
"BinanceTicker[]
BinanceTicker[symbol]"


BinanceTrades::usage = 
"BinanceTrades[symbol]"


BinanceAggTrades::usage = 
"BinanceAggTrades[symbol]"


BinanceKlines::usage = 
"BinanceKlines[symbol, interval]
BinanceKlines[symbol, interval, options]"


BinanceHistoricalTrades::usage = 
"BinanceHistoricalTrades[symbol]"


BinanceOrderTest::usage = 
"BinanceOrderTest[symbol, side, type, quantity, price]"


BinanceOrderCreate::usage = 
"BinanceOrderCreate[symbol, side, type, quantity, price]"


BinanceOrderGet::usage = 
"BinanceOrderGet[symbol, orderID]"


BinanceOrderCancel::usage = 
"BinanceOrderCancel[symbol, orderID]"


BinanceOrders::usage = 
"BinanceOrders[]
BinanceOrders[symbol]"


BinanceOrdersAll::usage = 
"BinanceOrdersAll[symbol]"


BinanceOCOrderCreate::usage = 
"BinanceOCOrderCreate[symbol, side, quantity, price, stopPrice]"


BinanceOCOrderCancel::usage = 
"BinanceOCOrderCancel[symbol, orderListID]"


BinanceOCOrderGet::usage = 
"BinanceOCOrderGet[symbol, orderListID]"


BinanceOCOrders::usage = 
"BinanceOCOrders[]"


BinanceOCOrdersAll::usage = 
"BinanceOCOrdersAll[]"


BinanceAccountInfo::usage = 
"BinanceAccountInfo[]"


BinanceMyTrades::usage = 
"BinanceMyTrades[symbol]"


(* ::Section:: *)
(*Begin private context*)


Begin["`Private`"]


(* ::Section:: *)
(*Internal variables and functions*)


$binanceAPI = 
"https://api.binance.com"


binancePublicAPI::reqerr = 
"error during `1`\n`2`"


binancePublicAPI[version: "v1" | "v3", method: _String | PatternSequence[_String, _String], parameters: {___Rule}] := 
	Module[{
		url, request, response, status, body, result
	}, 
		url = URLBuild[{$binanceAPI, "api", version, method}, parameters];
		request = HTTPRequest[url];
		TimeConstrained[Check[response = URLRead[request], Message[binancePublicAPI::reqerr, "getting http-response", url]; Return[Null]], 1, Message[binancePublicAPI::reqerr, "executing request", request]; Return[Null]]; 
		status = response["StatusCode"];
		body = response["Body"];
		If[status =!= 200, Message[binancePublicAPI::reqerr, "checking response", body]; Return[Null]];
		Check[result = toExpr[ImportString[body, "RawJSON"]], Message[binancePublicAPI::reqerr, "import from JSON", body]; Return[Null]]; 
		
		Return[result];
	]


toBinanceTime[time_Real] := 
	Floor[AbsoluteTime[DatePlus[DatePlus[DateObject[time], Quantity[-70, "Years"]], Quantity[-4, "Hours"]]] * 1000]


toOptionNames[func_Symbol] := 
	Map["\"" <> # <> "\""&, ToString /@ Keys[Options[func]]]


toExpr[list_?ListQ] := 
	Map[toExpr, list]


toExpr[assoc_?AssociationQ] := 
	Association[KeyValueMap[toExpr, assoc]]


toExpr[key_String, list_?ListQ] := 
	key -> toExpr[list]


toExpr[key_String, assoc_?AssociationQ] := 
	key -> toExpr[assoc]


toExpr[key_String, value_String] := 
	Which[
		StringMatchQ[value, NumberString, IgnoreCase -> True], 
			key -> ToExpression[value], 
		True, 
			key -> value
	]


toExpr[key_String, value_Integer] := 
	Which[
		StringMatchQ[key, ___ ~~ "time" ~~ ___, IgnoreCase -> True], 
			key -> FromUnixTime[0.001 * value], 
		True, 
			key -> value
	]


toExpr[key_String, value: True|False] := 
	key -> value


toExpr[value_Integer] := 
	If[1.58 * 10^12 < value < 2.58 * 10^12, FromUnixTime[0.001 * value], value]


toExpr[value_Real] := 
	value


toExpr[value_String] := 
	If[StringMatchQ[value, NumberString, IgnoreCase -> True], ToExpression[value], value]


toParams[params: {___Rule}, opts: OptionsPattern[]] := 
	DeleteCases[Join[params, Flatten[{opts}]], _[_, Automatic | Null | None]]


Options[binanceTradeAPI] := 
	{
		"apikey" :> ExchangeLinkIniRead["Binance", "APIKey"], 
		"secretkey" :> ExchangeLinkIniRead["Binance", "SecretKey"], 
		"time" :> BinanceTime[]["serverTime"](*toBinanceTime[AbsoluteTime[]]*), 
		"httpmethod" -> "POST", 
		"version" -> "v3"
	}


binanceTradeAPI[method_String, parameters: <|___Rule|>, OptionsPattern[]] := 
	Block[{
		url, 
		query, 
		queryString, 
		postParams, 
		apikey = OptionValue["apikey"],
		secretkey = OptionValue["secretkey"],
		time = OptionValue["time"],
		httpmethod = OptionValue["httpmethod"],
		version = OptionValue["version"],  
		request, response
	}, 
		query = parameters ~ Join ~ <|"timestamp" -> time|>;
		queryString = StringTrim[URLBuild[{}, query], "?"];
		
		postParams = query ~ Join ~ <|"signature" -> ExchangeLinkHMAC[secretkey, queryString, "SHA256"]|>;
		
		url = URLBuild[{$binanceAPI, "api", version, method}, postParams];
		request = HTTPRequest[url, 
			<|
				Method -> httpmethod, 
				"Headers" -> {
					"X-MBX-APIKEY" -> apikey
				}
			|>
		];
		response = URLRead[request];
		Check[toExpr[ImportString[response["Body"], "RawJSON"]], response["Body"]]
	]


(* ::Section:: *)
(*public functions implementation*)


(* ::Subsubsection:: *)
(*General endpoints*)


(* ::Text:: *)
(*Test connectivity*)


BinancePing::doclnk = 
"https://github.com/binance-exchange/binance-official-api-docs/blob/master/rest-api.md#test-connectivity"


SyntaxInformation[BinancePing] = 
	{"ArgumentsPattern" -> {}}


BinancePing[] := 
	binancePublicAPI["v1", "ping", {}]


(* ::Text:: *)
(*Check server time*)


BinanceTime::doclnk = 
"https://github.com/binance-exchange/binance-official-api-docs/blob/master/rest-api.md#check-server-time"


SyntaxInformation[BinanceTime] = 
	{"ArgumentsPattern" -> {}}


BinanceTime[] := 
	binancePublicAPI["v1", "time", {}]


(* ::Text:: *)
(*BinanceExchangeInfo*)


BinanceExchangeInfo::doclnk = 
"https://github.com/binance-exchange/binance-official-api-docs/blob/master/rest-api.md#exchange-information"


SyntaxInformation[BinanceExchangeInfo] = 
	{"ArgumentsPattern" -> {}}


BinanceExchangeInfo[] := 
	binancePublicAPI["v1", "exchangeInfo", {}]


(* ::Subsubsection:: *)
(*market data endpoints*)


(* ::Text:: *)
(*Order book*)


BinanceDepth::doclnk = 
"https://github.com/binance-exchange/binance-official-api-docs/blob/master/rest-api.md#order-book"


Options[BinanceDepth] := 
	{"limit" -> 100}


SyntaxInformation[BinanceDepth] = 
	{
		"ArgumentsPattern" -> {_, OptionsPattern[]}, 
		"OptionNames" -> toOptionNames[BinanceDepth]
	}


BinanceDepth[symbol_String, OptionsPattern[]] := 
	binancePublicAPI["v1", "depth", {"symbol" -> symbol, "limit" -> OptionValue["limit"]}]


(* ::Text:: *)
(*Recent trades list*)


BinanceTrades::doclnk = 
"https://github.com/binance-exchange/binance-official-api-docs/blob/master/rest-api.md#recent-trades-list"


Options[BinanceTrades] := 
	{
		"limit" -> 500
	}


SyntaxInformation[BinanceTrades] = 
	{
		"ArgumentsPattern" -> {_, OptionsPattern[]}, 
		"OptionNames" -> toOptionNames[BinanceTrades]
	}


BinanceTrades[symbol_String, options: OptionsPattern[]] := 
	binancePublicAPI["v1", "trades", toParams[{"symbol" -> symbol}, options]]


(* ::Text:: *)
(*BinancePrice*)


BinancePrice::doclnk = 
"https://github.com/binance-exchange/binance-official-api-docs/blob/master/rest-api.md#symbol-price-ticker"


SyntaxInformation[BinancePrice] = 
	{
		"ArgumentsPattern" -> {_.}
	}


BinancePrice[] := 
	binancePublicAPI["v1", "ticker", "price", {}]


BinancePrice[symbol_String] := 
	binancePublicAPI["v1", "ticker", "price", {"symbol" -> symbol}]


(* ::Text:: *)
(*BinanceTicker*)


SyntaxInformation[BinanceTicker] = 
	{
		"ArgumentsPattern" -> {_.}
	}


BinanceTicker[] := 
	binancePublicAPI["v1", "ticker", "24hr", {}]


BinanceTicker[symbol_String] := 
	binancePublicAPI["v1", "ticker", "24hr", {"symbol" -> symbol}]


(* ::Text:: *)
(*BinanceAggTrades*)


BinanceAggTrades::doclnk = 
"https://github.com/binance-exchange/binance-official-api-docs/blob/master/rest-api.md#compressedaggregate-trades-list"


Options[BinanceAggTrades] := 
	{"fromId" -> "INCLUSIVE", "startTime" -> "INCLUSIVE", "endTime" -> "INCLUSIVE", "limit" -> 500}


SyntaxInformation[BinanceAggTrades] = 
	{
		"ArgumentsPattern" -> {_, OptionsPattern[]}, 
		"OptionNames" -> toOptionNames[BinanceAggTrades]
	}


BinanceAggTrades[symbol_String, options: OptionsPattern[]] := 
	binancePublicAPI["v1", "aggTrades", {"symbol" -> symbol} ~ Join ~ Flatten[{options}]]


(* ::Text:: *)
(*BinanceKlines*)


Options[BinanceKlines] := 
	{"limit" -> 500, "startTime" -> Null, "endTime" -> Null}


SyntaxInformation[BinanceKlines] = 
	{
		"ArgumentsPattern" -> {_, _, OptionsPattern[]}, 
		"OptionNames" -> toOptionNames[BinanceKlines]
	}


BinanceKlines[symbol_String, interval_String, opts: OptionsPattern[]] := 
	binancePublicAPI["v1", "klines", {"symbol" -> symbol, "interval" -> interval} ~ Join ~ DeleteCases[Flatten[{opts}], _ -> Null]]


(* ::Subsubsection:: *)
(*Account endpoints*)


BinanceAccountInfo[opts: OptionsPattern[binanceTradeAPI]] := 
	binanceTradeAPI["account", <||>, "httpmethod" -> "GET", opts]


(* ::Text:: *)
(*Current open orders (USER_DATA)*)


BinanceOrderList[symbol_String, opts: OptionsPattern[binanceTradeAPI]] := 
	binanceTradeAPI[
		"openOrders", 
		<|
			"symbol" -> symbol, 
			"recvWindow" -> 5000
		|>, 
		"httpmethod" -> "GET", 
		opts
	]


Options[BinanceMyTrades] = 
	{"limit" -> 500}


BinanceMyTrades[symbol_String, opts: OptionsPattern[{binanceTradeAPI, BinanceMyTrades}]] := 
	binanceTradeAPI["myTrades", <|"symbol" -> symbol, "limit" -> OptionValue["limit"] |>, "httpmethod" -> "GET", FilterRules[{opts}, Options[binanceTradeAPI]]]


BinanceOrderCreate[symbol_String, side_String, type_String, quantity: _Real | _Integer | _String, price: _Real | _Integer | _String, opts: OptionsPattern[binanceTradeAPI]] := 
	binanceTradeAPI[
		"order", 
		<|
			"symbol" -> symbol, 
			"side" -> side, 
			"type" -> type, 
			"timeInForce" -> "GTC", 
			"quantity" -> quantity, 
			"price" -> price, 
			"recvWindow" -> 5000
		|>, 
		opts
	]


BinanceOrderCancel[symbol_String, orderID_Integer, opts: OptionsPattern[binanceTradeAPI]] := 
	binanceTradeAPI[
		"order", 
		<|"symbol" -> symbol, "orderId" -> orderID|>, 
		"httpmethod" -> "DELETE", 
		opts
	]


(* ::Section:: *)
(*End private context*)


End[]


(* ::Section:: *)
(*From change protection*)


Protect["`*"]


(* ::Section:: *)
(*End package*)


EndPackage[] (*ExchangeLink`Binance`*)
