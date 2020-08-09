(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["ExchangeLink`Kaiko`"]


Kaiko::usage = 
"just a symbol"


(* ::Text:: *)
(*Reference Data API*)


KaikoAssets::usage = 
"KaikoAssets[]"


KaikoExchanges::usage = 
"KaikoExchanges[]"


KaikoInstruments::usage = 
"KaikoInstruments[]"


(* ::Text:: *)
(*Market Data API*)


(* ::Text:: *)
(*Trade Data*)


KaikoTradeDataHist::usage =
"KaikoTradeDataHist[assoc]
KaikoTradeDataHist[assoc, {opts}]
KaikoTradeDataHist[exchangeCode, instrumentClass, instrumentCode]
KaikoTradeDataHist[exchangeCode, instrumentClass, instrumentCode, {opts}]

Arguments:
	assoc - <|exchangeCode -> bfnx, instrumentClass -> spot, instrumentCode -> btc-usd|>
	exchangeCode - String [result of KaikoExchages or KaikoInstruments]
	instrumentClass - String [result of KaikoInstruments]
	instrumentCode - String [result of KaikoInstruments]

Options:
	startTime - DateObject
	endTime - DateObject
	continuationToken - String
	pageSize - Integer"


KaikoTradeDataRecent::usage =
"KaikoTradeDataRecent[assoc]
KaikoTradeDataRecent[assoc, {opts}]
KaikoTradeDataRecent[exchangeCode, instrumentClass, instrumentCode]
KaikoTradeDataRecent[exchangeCode, instrumentClass, instrumentCode, {opts}]

Arguments:
	assoc - <|exchangeCode -> bfnx, instrumentClass -> spot, instrumentCode -> btc-usd|>
	exchangeCode - String [result of KaikoExchages or KaikoInstruments]
	instrumentClass - String [result of KaikoInstruments]
	instrumentCode - String [result of KaikoInstruments]

Options:
	limit - Integer"


KaikoTradeDataSpot::usage =
"KaikoTradeDataSpot[]
KaikoTradeDataSpot[{opts}]

Options:
	startTime - DateObject
	endTime - DateObject [ > startTime]
	continuationToken - String [Token]
	pageSize - Integer [Positive]
	pattern - String"


(* ::Text:: *)
(*Order Book data*)


KaikoOrderBookSnapshotsFull


KaikoOrderBookSnapshotsRaw


KaikoOrderBookSnapshotsDepth


KaikoOrderBookSnapshotsSlippage


(* ::Text:: *)
(*Aggregates*)


KaikoCandlesHist::usage =
"KaikoCandlesHist[assoc]
KaikoCandlesHist[assoc, {opts}]
KaikoCandlesHist[exchangeCode, instrumentClass, instrumentCode]
KaikoCandlesHist[exchangeCode, instrumentClass, instrumentCode, {opts}]

Arguments:
	assoc - <|exchangeCode -> bfnx, instrumentClass -> spot, instrumentCode -> btc-usd|>
	exchangeCode - String [result of KaikoExchages or KaikoInstruments]
	instrumentClass - String [result of KaikoInstruments]
	instrumentCode - String [result of KaikoInstruments]

Options:
	startTime - DateObject
	endTime - DateObject [ > startTime]
	continuationToken - String [Token]
	pageSize - Integer [Positive]
	sort - String [asc|desc]
	interval - String [1h|1d]"


KaikoCandlesRecent::usage =
"KaikoCandlesHist[assoc]
KaikoCandlesHist[assoc, {opts}]
KaikoCandlesHist[exchangeCode, instrumentClass, instrumentCode]
KaikoCandlesHist[exchangeCode, instrumentClass, instrumentCode, {opts}]

Arguments:
	assoc - <|exchangeCode -> bfnx, instrumentClass -> spot, instrumentCode -> btc-usd|>
	exchangeCode - String [result of KaikoExchages or KaikoInstruments]
	instrumentClass - String [result of KaikoInstruments]
	instrumentCode - String [result of KaikoInstruments]

Options:
	limit - Integer [Positive]
	interval - String [1h|1d]"


Begin["`Private`"] (* Begin Private Context *) 


(* ::Section:: *)
(*Reference Data API*)


$kaikoReferenceDataAPI = "https://reference-data-api.kaiko.io"


kaikoPublicExec[path: {__String}, query: {___Rule}: {}] := 
	Block[{url, request, response, status, body, json}, 
		url = URLBuild[Flatten[{$kaikoReferenceDataAPI, "v1", path}], query]; 
		request = HTTPRequest[url]; 
		response = URLRead[request]; 
		body = response["Body"]; 
		json = ImportString[ExportString[body, "Text"], "RawJSON"]; 
		Return[json]
	]


KaikoAssets[] := 
	kaikoPublicExec[{"assets"}]["data"]


KaikoExchanges[] := 
	Query[All, <|
		"code" -> "code", 
		"name" -> "name", 
		"assetClass" -> "asset_class", 
		"assetClasses" -> "asset_classes"
	|>] @ 
	kaikoPublicExec[{"exchanges"}]["data"]


KaikoInstruments[] := 
	Query["data", All, <|
		"kaikoLegacyExchangeSlug" -> "kaiko_legacy_exchange_slug",
		"tradeStartTime" -> "trade_start_time",
		"tradeEndTime" -> "trade_end_time",
		"code" -> "code",
		"exchangeCode" -> "exchange_code",
		"exchangePairCode" -> "exchange_pair_code",
		"baseAsset" -> "base_asset",
		"quoteAsset" -> "quote_asset",
		"kaikoLegacySymbol" -> "kaiko_legacy_symbol",
		"class" -> "class",
		"tradeStartTimestamp" -> "trade_start_timestamp",
		"tradeEndTimestamp" -> "trade_end_timestamp",
		"tradeCount" -> "trade_count",
		"tradeCompressedSize" -> "trade_compressed_size"
	|>] @ 
	kaikoPublicExec[{"instruments"}]


(* ::Section:: *)
(*Tools*)


toSnakecase[any_] := any


toSnakecase[camelcase_String] :=
    StringReplace[
	    camelcase,
	    Thread[CharacterRange["A", "Z"] -> Map[StringJoin["_", #]&] @ CharacterRange["a", "z"]]
    ]


toISO[digit_?NumericQ] := digit


toISO[date_?DateObjectQ] :=
    DateString[
	    TimeZoneConvert[date, 0],
	    {"Year", "-", "Month", "-", "Day", "T", "Hour", ":", "Minute", ":", "SecondExact", "Z"}
    ]


toISO[string_String] :=
    string


getOptions[function_Symbol, opts: OptionsPattern[{}]] := 
	Normal @
	Map[toISO] @
	KeyMap[toSnakecase] @
	Association @
	FilterRules[DeleteCases[Flatten[{opts}], _[_, Automatic | Null | None]], Options[function]]


(* ::Section:: *)
(*Market Data API*)


Kaiko::notscs = 
"Error during request to `1`"


Options[kaikoMarketDataAPI] = {
	"auth" :> <|"APIKey" -> "f00edcvicw9g1bq4v887zhmwjgmwsngv"|>
}


$kaikoMarketDataAPI = "https://us.market-api.kaiko.io/"


kaikoMarketDataAPI[v_String, path__String, query: {___Rule}: {}, OptionsPattern[]] := 
	Block[{url, request, key, response, body, json}, 
		url = URLBuild[{$kaikoMarketDataAPI, v, "data", path}, query]; 
		key = OptionValue["auth"]["APIKey"]; 
		request = HTTPRequest[url, <|
			"Headers" -> {"X-Api-Key" -> key}
		|>]; 
		response = URLRead[request]; 
		body = response["Body"]; 
		json = ImportString[body, "RawJSON"]; 
		Return[json]
	]


(* ::Text:: *)
(*Trade data*)


Options[KaikoTradeDataHist] = {
	"startTime" -> Automatic, 
	"endTime" -> Automatic, 
	"continuationToken" -> Automatic, 
	"pageSize" -> Automatic
}


KaikoTradeDataHist[exchangeCode_String, instrumentClass_String, instrumentCode_String,
	opts: OptionsPattern[{KaikoTradeDataHist, kaikoMarketDataAPI}]] :=
	Query["data", All, <|
		"timestamp" -> Function[FromUnixTime[#timestamp/1000]], 
		"tradeId" -> Function[ToExpression[#["trade_id"]]], 
		"price" -> Function[ToExpression[#price]], 
		"amount" -> Function[ToExpression[#amount]], 
		"takerSideSell" -> Function[#["taker_side_sell"]]
	|>] @ 
	kaikoMarketDataAPI["v2", "trades.v1", "exchanges", exchangeCode, instrumentClass, instrumentCode, "trades",
		getOptions[KaikoTradeDataHist, opts], getOptions[kaikoMarketDataAPI, opts]]


Options[KaikoTradeDataRecent] = {
	"limit" -> Automatic
}


KaikoTradeDataRecent[exchangeCode_String, instrumentClass_String, instrumentCode_String,
	opts: OptionsPattern[{KaikoTradeDataRecent, kaikoMarketDataAPI}]] :=
	Query["data", All, <|
		"timestamp" -> Function[FromUnixTime[#timestamp/1000]], 
		"tradeId" -> Function[ToExpression[#["trade_id"]]], 
		"price" -> Function[ToExpression[#price]], 
		"amount" -> Function[ToExpression[#amount]], 
		"takerSideSell" -> Function[#["taker_side_sell"]]
	|>] @ 
	kaikoMarketDataAPI["v1", "trades.v1", "exchanges", exchangeCode, instrumentClass, instrumentCode, "trades", "recent",
		getOptions[KaikoTradeDataRecent, opts], getOptions[kaikoMarketDataAPI, opts]]


Options[KaikoTradeDataSpot] = {
	"startTime" -> Automatic, 
	"endTime" -> Automatic, 
	"continuationToken" -> Automatic, 
	"pageSize" -> Automatic, 
	"pettern" -> Automatic
}


KaikoTradeDataSpot[opts: OptionsPattern[{KaikoTradeDataSpot, kaikoMarketDataAPI}]] := 
	kaikoMarketDataAPI["v2", "trades.v1", "exchanges", "spots", "recent",
		getOptions[KaikoTradeDataSpot, opts], getOptions[kaikoMarketDataAPI, opts]]


(* ::Text:: *)
(*Order book Data*)


Options[KaikoOrderBookSnapshotsFull] = {
	"startTime" -> Automatic, 
	"endTime" -> Automatic, 
	"limitOrders" -> Automatic, 
	"pageSize" -> Automatic, 
	"slippage" -> Automatic, 
	"slippageRef" -> Automatic
}


KaikoOrderBookSnapshotsFull[exchangeCode_String, instrumentClass_String, instrumentCode_String, 
	opts: OptionsPattern[{KaikoOrderBookSnapshotsFull, kaikoMarketDataAPI}]] := 
	Query["data"] @ 
	kaikoMarketDataAPI["v1", "order_book_snapshots.v1", "exchanges", exchangeCode, instrumentClass, instrument, "snapshots", "full", 
		getOptions[KaikoOrderBookSnapshotsFull, opts], getOptions[kaikoMarketDataAPI, opts]]


Options[KaikoOrderBookSnapshotsRaw] = {
	"continuationToken" -> Automatic, 
	"startTime" -> Automatic, 
	"endTime" -> Automatic, 
	"limitOrders" -> Automatic, 
	"pageSize" -> Automatic
}


KaikoOrderBookSnapshotsRaw[exchangeCode_String, instrumentClass_String, instrumentCode_String, 
	opts: OptionsPattern[{KaikoOrderBookSnapshotsRaw, kaikoMarketDataAPI}]] := 
	Query["data"] @ 
	kaikoMarketDataAPI["v1", "order_book_snapshots.v1", "exchanges", exchangeCode, instrumentClass, "snapshots", "raw", 
		getOptions[KaikoOrderBookSnapshotsRaw, opts], getOptions[kaikoMarketDataAPI, opts]]


Options[KaikoOrderBookSnapshotsDepth] = {
	"continuationToken" -> Automatic, 
	"startTime" -> Automatic, 
	"endTime" -> Automatic, 
	"pageSize" -> Automatic
}


KaikoOrderBookSnapshotsDepth[exchangeCode_, instrumentClass_, instrumentCode_, 
	opts: OptionsPattern[{KaikoOrderBookSnapshotsDepth, kaikoMarketDataAPI}]] := 
	Query["data"] @ 
	kaikoMarketDataAPI["v1", "order_book_snapshots.v1", "exchanges", exchangeCode, instrumentClass, "snapshots", "depth", 
		getOptions[KaikoOrderBookSnapshotsDepth, opts], getOptions[kaikoMarketDataAPI, opts]]


Options[KaikoOrderBookSnapshotsSlippage] = {
	"continuationToken" -> Automatic, 
	"startTime" -> Automatic, 
	"endTime" -> Automatic, 
	"limitOrders" -> Automatic, 
	"pageSize" -> Automatic, 
	"slippage" -> Automatic, 
	"slippageRef" -> Automatic
}


KaikoOrderBookSnapshotsSlippage[exchangeCode_, instrumentClass_, instrumentCode_, 
	opts: OptionsPattern[{KaikoOrderBookSnapshotsSlippage, kaikoMarketDataAPI}]] := 
	Query["data"] @ 
	kaikoMarketDataAPI["v1", "order_book_snapshots.v1", "exchanges", exchangeCode, instrumentClass, "snapshots", "slippage", 
		getOptions[KaikoOrderBookSnapshotsSlippage, opts], getOptions[kaikoMarketDataAPI, opts]]


(* ::Text:: *)
(*Order Book Aggregations: Full*)


Options[KaikoOrderBookAggregationsFull] = {
	"continuationToken" -> Automatic, 
	"startTime" -> Automatic, 
	"endTime" -> Automatic, 
	"interval" -> Automatic, 
	"pageSize" -> Automatic, 
	"slippage" -> Automatic, 
	"slippageRef" -> Automatic
}


KaikoOrderBookAggregationsFull[exchangeCode_, instrumentClass_, instrumentCode_, 
	opts: OptionsPattern[{KaikoOrderBookAggregationsFull, kaikoMarketDataAPI}]] := 
	Query["data"] @ 
	kaikoMarketDataAPI["v1", "order_book_snapshots.v1", "exchanges", exchangeCode, instrumentClass, "ob_aggregations", "full", 
		getOptions[KaikoOrderBookAggregationsFull, opts], getOptions[kaikoMarketDataAPI, opts]]


(* ::Text:: *)
(*Order Book Aggregations: Depth*)


Options[KaikoOrderBookAggregationsDepth] = {
	"continuationToken" -> Automatic, 
	"startTime" -> Automatic, 
	"endTime" -> Automatic, 
	"interval" -> Automatic, 
	"pageSize" -> Automatic
}


KaikoOrderBookAggregationsDepth[exchangeCode_, instrumentClass_, instrumentCode_, 
	opts: OptionsPattern[{KaikoOrderBookAggregationsDepth, kaikoMarketDataAPI}]] := 
	Query["data"] @ 
	kaikoMarketDataAPI["v1", "order_book_snapshots.v1", "exchanges", exchangeCode, instrumentClass, "ob_aggregations", "depth", 
		getOptions[KaikoOrderBookAggregationsDepth, opts], getOptions[kaikoMarketDataAPI, opts]]


(* ::Text:: *)
(*Order Book Aggregations: Slippage*)


Options[KaikoOrderBookAggregationsSlippage] = {
	"continuationToken" -> Automatic, 
	"startTime" -> Automatic, 
	"endTime" -> Automatic, 
	"interval" -> Automatic, 
	"pageSize" -> Automatic, 
	"slippage" -> Automatic, 
	"slippageRef" -> Automatic
}


KaikoOrderBookAggregationsSlippage[exchangeCode_, instrumentClass_, instrumentCode_, 
	opts: OptionsPattern[{KaikoOrderBookAggregationsSlippage, kaikoMarketDataAPI}]] := 
	Query["data"] @ 
	kaikoMarketDataAPI["v1", "order_book_snapshots.v1", "exchanges", exchangeCode, instrumentClass, "ob_aggregations", "slippage", 
		getOptions[KaikoOrderBookAggregationsSlippage, opts], getOptions[kaikoMarketDataAPI, opts]]


(* ::Text:: *)
(*Aggregates*)


Options[KaikoCandlesHist] = {
	"startTime" -> Automatic, 
	"endTime" -> Automatic, 
	"continuationToken" -> Automatic, 
	"pageSize" -> Automatic, 
	"interval" -> Automatic, 
	"sort" -> Automatic
}


KaikoCandlesHist[exchangeCode_String, instrumentClass_String, instrumentCode_String,
	opts: OptionsPattern[{KaikoCandlesHist, kaikoMarketDataAPI}]] :=
	Query["data", All, <|
		"timestamp" -> Function[FromUnixTime[#timestamp/1000]], 
		"open" -> "open" /* ToExpression, 
		"high" -> "high" /* ToExpression, 
		"low" -> "low" /* ToExpression, 
		"close" -> "close" /* ToExpression, 
		"volume" -> "volume" /* ToExpression
	|>] @ 
	kaikoMarketDataAPI["v2", "trades.v1", "exchanges", exchangeCode, instrumentClass, instrumentCode, "aggregations", "ohlcv",
		getOptions[KaikoCandlesHist, opts], getOptions[kaikoMarketDataAPI, opts]]


Options[KaikoCandlesRecent] = {
	"limit" -> Automatic,
	"interval" -> Automatic
}


KaikoCandlesRecent[exchangeCode_String, instrumentClass_String, instrumentCode_String,
	opts: OptionsPattern[{KaikoCandlesRecent, kaikoMarketDataAPI}]] :=
	Query["data", All, <|
		"timestamp" -> Function[FromUnixTime[#timestamp/1000]],
		"open" -> "open" /* ToExpression,
		"high" -> "high" /* ToExpression,
		"low" -> "low" /* ToExpression,
		"close" -> "close" /* ToExpression,
		"volume" -> "volume" /* ToExpression
	|>] @
	kaikoMarketDataAPI["v1", "trades.v1", "exchanges", exchangeCode, instrumentClass, instrumentCode, "aggregations", "ohlcv", "recent",
		getOptions[KaikoCandlesRecent, opts], getOptions[kaikoMarketDataAPI, opts]]


(* ::Section:: *)
(*End*)


End[] (* End Private Context *)


EndPackage[]
