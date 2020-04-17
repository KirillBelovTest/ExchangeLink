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


$BinanceExchangeInfo::usage = 
"BinanceExchangeInfo cash for the one day"


$BinanceDomain::usage = 
"COM|US"


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


BinanceBuy::usage = 
"BinanceBuy[symbol, qty, price]"


BinanceSell::usage = 
"BinanceBuy[symbol, qty, price]"


BinanceOrderGet::usage = 
"BinanceOrderGet[symbol, orderID]"


BinanceOrderCancel::usage = 
"BinanceOrderCancel[symbol, orderID]"


BinanceOrdersNow::usage = 
"BinanceOrdersNow[symbol]"


BinanceOrdersAll::usage = 
"BinanceOrdersAll[symbol]"


BinanceOCOrderCreate::usage = 
"BinanceOCOrderCreate[symbol, side, quantity, price, stopPrice]"


BinanceOCOrderCancel::usage = 
"BinanceOCOrderCancel[symbol, orderListID]"


BinanceOCOrderGet::usage = 
"BinanceOCOrderGet[symbol, orderListID]"


BinanceOCOrdersNow::usage = 
"BinanceOCOrdersNow[]"


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


(* ::Subsubsection:: *)
(*Converters*)


toBinanceTime[time_DateObject] := 
	ToString[1000 * UnixTime[time] + Round[1000 * FractionalPart[Last[DateList[time]]]]]


toBinanceTime[] := 
	toBinanceTime[Now]


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


toQty[symbol_String, qty_?NumericQ] := 
	Block[{qtyStep = 
		SelectFirst[
			SelectFirst[
				$BinanceExchangeInfo["symbols"], 
				#symbol == symbol&
			]["filters"], 
			#filterType === "LOT_SIZE"&
		]["stepSize"]
	}, 
		ToString[Which[
			NumericQ[qtyStep] && qtyStep < 1, 
				NumberForm[qty, {16, Round[-Log10[qtyStep]]}], 
				
			NumericQ[qtyStep] && qtyStep >= 1, 
				NumberForm[Round[qty, qtyStep]], 
			
			True, 
				qty
		]]
	]


toPrice[symbol_String, price_?NumericQ] := 
	Block[{priceStep = 
		SelectFirst[
			SelectFirst[
				$BinanceExchangeInfo["symbols"], 
				#symbol == symbol&
			]["filters"], 
			#filterType === "PRICE_FILTER"&
		]["tickSize"]
	}, 
		ToString[Which[
			NumericQ[priceStep] && priceStep < 1, 
				NumberForm[price, {16, Round[-Log10[priceStep]]}], 
				
			NumericQ[priceStep] && priceStep >= 1, 
				NumberForm[Round[price, priceStep]], 
			
			True, 
				price
		]]
	]


(* ::Subsubsection:: *)
(*ENUMs*)


$orderTypes = 
	"LIMIT" | 
	"MARKET" | 
	"STOP_LOSS" | 
	"STOP_LOSS_LIMIT" | 
	"TAKE_PROFIT" | 
	"TAKE_PROFIT_LIMIT" | 
	"LIMIT_MAKET"


$orderSides = 
	"BUY" | 
	"SELL"


(* ::Subsubsection:: *)
(*Binance public API request*)


$BinanceDomain = 
"COM"


$BinanceDomain /: Set[$BinanceDomain, value: "COM" | "US"] := 
	(Unprotect[$BinanceDomain]; With[{val = value}, $BinanceDomain := val]; Protect[$BinanceDomain]; $BinanceDomain)


$binanceAPI := 
"https://api.binance." <> ToLowerCase[$BinanceDomain]


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


(* ::Subsubsection:: *)
(*Binance trade API request*)


Options[binanceTradeAPI] := 
	{
		"apikey" :> ExchangeLink`$ExchangeLink["Binance", "APIKey"], 
		"secretkey" :> ExchangeLink`$ExchangeLink["Binance", "SecretKey"], 
		"time" :> toBinanceTime[], 
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


SyntaxInformation[BinancePing] = 
	{"ArgumentsPattern" -> {}}


BinancePing[] := 
	binancePublicAPI["v1", "ping", {}]


(* ::Text:: *)
(*Check server time*)


SyntaxInformation[BinanceTime] = 
	{"ArgumentsPattern" -> {}}


BinanceTime[] := 
	binancePublicAPI["v1", "time", {}]


(* ::Text:: *)
(*BinanceExchangeInfo*)


SyntaxInformation[BinanceExchangeInfo] = 
	{"ArgumentsPattern" -> {}}


BinanceExchangeInfo[] := 
	binancePublicAPI["v1", "exchangeInfo", {}]


BinanceExchangeInfo[date_DateObject] := 
	Block[{info = Check[BinanceExchangeInfo[], Pause[1]; BinanceExchangeInfo[]]},
		Unprotect[BinanceExchangeInfo];
		If[AssociationQ[info], BinanceExchangeInfo[date] = info]; 
		Protect[BinanceExchangeInfo];
		info
	]


(* ::Text:: *)
(*Exchange Info is constant data during one day (or more than one day)*)


$BinanceExchangeInfo := 
	BinanceExchangeInfo[Today];


(* ::Subsubsection:: *)
(*Market data endpoints*)


(* ::Text:: *)
(*Order book*)


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


Options[BinanceAggTrades] := 
	{
		"fromId" -> "INCLUSIVE", 
		"startTime" -> "INCLUSIVE", 
		"endTime" -> "INCLUSIVE", 
		"limit" -> 500
	}


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
	{
		"limit" -> 500, 
		"startTime" -> Null, 
		"endTime" -> Null
	}


SyntaxInformation[BinanceKlines] = 
	{
		"ArgumentsPattern" -> {_, _, OptionsPattern[]}, 
		"OptionNames" -> toOptionNames[BinanceKlines]
	}


BinanceKlines[symbol_String, interval_String, opts: OptionsPattern[]] := 
	binancePublicAPI["v1", "klines", {"symbol" -> symbol, "interval" -> interval} ~ Join ~ 
		DeleteCases[Flatten[{opts}], _ -> Null]
	]


(* ::Subsubsection:: *)
(*Account endpoints*)


(* ::Text:: *)
(*Account info*)


SyntaxInformation[BinanceAccountInfo] = 
	{
		"ArgumentsPattern" -> {}
	}


BinanceAccountInfo[opts: OptionsPattern[binanceTradeAPI]] := 
	binanceTradeAPI["account", <||>, "httpmethod" -> "GET", opts]


(* ::Text:: *)
(*All my trades (USER_DATA)*)


Options[BinanceMyTrades] = 
	{
		"limit" -> 500
	}


SyntaxInformation[BinanceMyTrades] = 
	{
		"ArgumentsPattern" -> {_, OptionsPattern[]}, 
		"OptionNames" -> toOptionNames[BinanceMyTrades]
	}


BinanceMyTrades[symbol_String, opts: OptionsPattern[{binanceTradeAPI, BinanceMyTrades}]] := 
	binanceTradeAPI[
		"myTrades", 
		<|
			"symbol" -> symbol, 
			"limit" -> OptionValue["limit"]
		|>, 
		"httpmethod" -> "GET", 
		FilterRules[{opts}, Options[binanceTradeAPI]]
	]


(* ::Text:: *)
(*New order (TRADE)*)


SyntaxInformation[BinanceOrderCreate] = 
	{
		"ArgumentsPattern" -> {_, _, _, _, _}
	}


BinanceOrderCreate[
	symbol_String, 
	side: $orderSides, 
	type: $orderTypes, 
	quantity_?NumericQ, 
	price_?NumericQ, opts: OptionsPattern[binanceTradeAPI]] := 
	binanceTradeAPI[
		"order", 
		<|
			"symbol" -> symbol, 
			"side" -> side, 
			"type" -> type, 
			"timeInForce" -> "GTC", 
			"quantity" -> toQty[symbol, quantity], 
			"price" -> toPrice[symbol, price], 
			"recvWindow" -> 5000
		|>, 
		"httpmethod" -> "POST", 
		opts
	]


(* ::Text:: *)
(*New order BUY  (TRADE)*)


SyntaxInformation[BinanceBuy] = 
	{
		"ArgumentsPattern" -> {_, _, _}
	}


BinanceBuy[symbol_String, quantity_?NumericQ, price_?NumericQ, 
	opts: OptionsPattern[binanceTradeAPI]] := 
	BinanceOrderCreate[symbol, "BUY", "LIMIT", quantity, price, opts]


(* ::Text:: *)
(*New order SELL (TRADE)*)


SyntaxInformation[BinanceSell] = 
	{
		"ArgumentsPattern" -> {_, _, _}
	}


BinanceSell[symbol_String, quantity_?NumericQ, price_?NumericQ, 
	opts: OptionsPattern[binanceTradeAPI]] := 
	BinanceOrderCreate[symbol, "SELL", "LIMIT", quantity, price, opts]


(* ::Text:: *)
(*Cancel order (TRADE)*)


SyntaxInformation[BinanceOrderCancel] = 
	{
		"ArgumentsPattern" -> {_, _}
	}


BinanceOrderCancel[symbol_String, orderID_Integer, opts: OptionsPattern[binanceTradeAPI]] := 
	binanceTradeAPI[
		"order", 
		<|"symbol" -> symbol, "orderId" -> orderID|>, 
		"httpmethod" -> "DELETE", 
		opts
	]


(* ::Text:: *)
(*Query order (USER_DATA)*)


SyntaxInformation[BinanceOrderGet] = 
	{
		"ArgumentsPattern" -> {_, _}
	}


BinanceOrderGet[symbol_String, orderID_Integer, opts: OptionsPattern[binanceTradeAPI]] := 
	binanceTradeAPI[
		"order", 
		<|"symbol" -> symbol, "orderId" -> orderID|>, 
		"httpmethod" -> "GET", 
		opts
	]


(* ::Text:: *)
(*Test new order (TRADE)*)


BinanceOrderTest[args_Association: <||>, opts: OptionsPattern[binanceTradeAPI]] := 
	binanceTradeAPI[
		"order", 
		args, 
		"httpmethod" -> "GET", 
		opts
	]


(* ::Text:: *)
(*Current open orders (USER_DATA)*)


SyntaxInformation[BinanceOrdersNow] = 
	{
		"ArgumentsPattern" -> {_}
	}


BinanceOrdersNow[symbol_String, opts: OptionsPattern[binanceTradeAPI]] := 
	binanceTradeAPI[
		"openOrders", 
		<|
			"symbol" -> symbol, 
			"recvWindow" -> 5000
		|>, 
		"httpmethod" -> "GET", 
		opts
	]


(* ::Text:: *)
(*All orders (USER_DATA)*)


Options[BinanceOrdersAll] = 
	{
		"orderId" -> Null, 
		"startTime" -> Null, 
		"endTime" -> Null, 
		"limit" -> 500
	}


SyntaxInformation[BinanceOrdersAll] = 
	{
		"ArgumentsPattern" -> {_, OptionsPattern[]}, 
		"OptionNames" -> toOptionNames[BinanceOrdersAll]
	}


BinanceOrdersAll[symbol_String, opts: OptionsPattern[{BinanceOrdersAll, binanceTradeAPI}]] := 
	binanceTradeAPI[
		"allOrders", 
		<|
			"symbol" -> symbol, 
			"recvWindow" -> 5000
		|> ~ Join ~ <|FilterRules[Flatten[{opts}], Options[BinanceOrdersAll]]|>, 
		"httpmethod" -> "GET", 
		FilterRules[Flatten[{opts}], Options[binanceTradeAPI]]
	]


(* ::Text:: *)
(*New OCO (TRADE)*)


SyntaxInformation[BinanceOCOrderCreate] = 
	{
		"ArgumentsPattern" -> {_, _, _, _, _, _}
	}


BinanceOCOrderCreate[symbol_String, side_String, type_String, 
	quantity: _Real | _Integer | _String, 
	price: _Real | _Integer | _String, 
	stopPrice: _Real | _Integer | _String, 
	opts: OptionsPattern[binanceTradeAPI]
] := 
	binanceTradeAPI[
		"order/oco", 
		<|
			"symbol" -> symbol, 
			"side" -> side, 
			"type" -> type, 
			"timeInForce" -> "GTC", 
			"quantity" -> quantity, 
			"price" -> price, 
			"stopPrice" -> stopPrice, 
			"recvWindow" -> 5000
		|>, 
		"httpmethod" -> "POST", 
		opts
	]


(* ::Text:: *)
(*Query OCO (USER_DATA)*)


BinanceOCOrderGet[orderListID_Integer, opts: OptionsPattern[binanceTradeAPI]] := 
	binanceTradeAPI[
		"orderList", 
		<|"orderListId" -> orderListID|>, 
		"httpmethod" -> "GET", 
		opts
	]


(* ::Text:: *)
(*Cancel OCO (TRADE)*)


BinanceOCOrderCancel[symbol_String, orderListID_Integer, opts: OptionsPattern[binanceTradeAPI]] := 
	binanceTradeAPI[
		"orderList", 
		<|"symbol" -> symbol, "orderListId" -> orderListID, "recvWindow" -> 5000|>, 
		"httpmethod" -> "DELETE", 
		opts
	]


(* ::Text:: *)
(*Query Open OCO (USER_DATA)*)


BinanceOCOrdersNow[opts: OptionsPattern[binanceTradeAPI]] := 
	binanceTradeAPI[
		"openOrderList", 
		<|"recvWindow" -> 5000|>, 
		"httpmethod" -> "GET", 
		opts
	]


(* ::Text:: *)
(*Query all OCO (USER_DATA)*)


Options[BinanceOCOrdersAll] = 
	{
		"fromId" -> Null, 
		"startTime" -> Null, 
		"endTime" -> Null, 
		"limit" -> 500
	}


SyntaxInformation[BinanceOCOrdersAll] = 
	{
		"ArgumentsPattern" -> {OptionsPattern[]}, 
		"OptionNames" -> toOptionNames[BinanceOCOrdersAll]
	}


BinanceOCOrdersAll[opts: OptionsPattern[{BinanceOCOrdersAll, binanceTradeAPI}]] := 
	binanceTradeAPI[
		"openOrderList", 
		<|
			"recvWindow" -> 5000
		|> ~ Join ~ <|FilterRules[Flatten[{opts}], Options[BinanceOrdersAll]]|>, 
		"httpmethod" -> "GET", 
		FilterRules[Flatten[{opts}], Options[binanceTradeAPI]]
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
