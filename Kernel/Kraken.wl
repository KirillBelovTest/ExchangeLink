(* ::Package:: *)

(* ::Chapter:: *)
(*Kraken*)


BeginPackage["KirillBelov`ExchangeLink`Kraken`", { 
    "KirillBelov`ExchangeLink`Settings`", 
    "KirillBelov`WebSocketJLink`"
}]


Get["KirillBelov`ExchangeLink`HMAC`"]
Get["KirillBelov`ExchangeLink`Tools`"]


(* ::Section::Closed:: *)
(*Kraken market data endpoins*)


KrakenTime::usage = 
"KrakenTime[]"


KrakenSystemStatus::usage = 
"KrakenSystemStatus[]"


KrakenAssets::usage = 
"KrakenAssets[ASSETS]"


$KrakenAssets::usage = 
"$KrakenAssets"


KrakenAssetPairs::usage = 
"KrakenAssetPairs[PAIR]"


$KrakenAssetPairs::usage = 
"KrakenAssetPairs"


KrakenTicker::usage = 
"KrakenTicker[PAIR]"


KrakenOHLC::usage = 
"KrakenOHLC[PAIR]
KrakenOHLC[PAIR, interval]"


KrakenOrderBook::usage = 
"KrakenOrderBook[PAIR]"


KrakenTrades::usage = 
"KrakenTrades[PAIR]"


KrakenSpread::usage = 
"KrakenSpread[PAIR]"


(* ::Section::Closed:: *)
(*Kraken user data endpoints*)


KrakenBalance::usage = 
"KrakenBalance[]"


KrakenTradeBalance::usage = 
"KrakenTradeBalance[]
KrakenTradeBalance[ASSET]"


KrakenOpenOrders::usage = 
"KrakenOpenOrders[]"


KrakenClosedOrders::usage = 
"KrakenClosedOrders[]"


KrakenQueryOrders::usage = 
"KrakenQueryOrders[]"


KrakenTradeHistory::usage = 
"KrakenTradeHistory[]"


KrakenTradeHistory::usage = 
"KrakenTradeHistory[]"


KrakenQueryTrades::usage = 
"KrakenQueryTrades[]"


KrakenOpenPositions::usage = 
"KrakenOpenPositions[]"


KrakenLedgers::usage = 
"KrakenLedgers[]"


KrakenQueryLedgers::usage = 
"KrakenQueryLedgers[]"


KrakenTradeVolume::usage = 
"KrakenTradeVolume[]"


KrakenAddExport::usage = 
"KrakenAddExport[]"


KrakenExportStatus::usage = 
"KrakenExportStatus[]"


KrakenRetrieveExport::usage = 
"KrakenRetrieveExport[]"


KrakenRemoveExport::usage = 
"KrakenRemoveExport[]"


(* ::Section::Closed:: *)
(*Kraken user trading endpoints*)


KrakenAddOrder::usage = 
"KrakenAddOrder[PAIR, type, side, volume]
KrakenAddOrder[PAIR, type, side, volume, opts]"


KrakenAddOrderBatch::usage = 
"KrakenAddOrderBatch[PAIR, {orders}]
KrakenAddOrderBatch[PAIR, {orders}, opts]"


KrakenEditOrder::usage = 
"KrakenEditOrder[PAIR, orderId]
KrakenEditOrder[PAIR, orderId, opts]"


KrakenCancelOrder::usage = 
"KrakenCancelOrder[txid]"


KrakenCancelAll::usage = 
"KrakenCancelAll[]"


KrakenCancelAllOrdersAfter::usage = 
"KrakenCancelAllOrdersAfter[timeout]"


KrakenCancelAllOrdersBatch::usage = 
"KrakenCancelAllOrdersBatch[{orders}]"


(* ::Section::Closed:: *)
(*Kraken user funding endpoints*)


KrakenDepositMethods::usage = 
"KrakenDepositMethods[]"


KrakenDepositAddresses::usage = 
"KrakenDepositAddresses[]"


KrakenDepositStatus::usage = 
"KrakenDepositStatus[]"


KrakenWithdrawInfo::usage = 
"KrakenWithdrawInfo[]"


KrakenWithdraw::usage = 
"KrakenWithdraw[]"


KrakenWithdrawStatus::usage = 
"KrakenWithdrawStatus[]"


KrakenWithdrawCancel::usage = 
"KrakenWithdrawCancel[]"


KrakenWalletTransfer::usag = 
"KrakenWalletTransfer[]"


(* ::Section::Closed:: *)
(*Kraken user staking endpoints*)


KrakenStake::usage = 
"KrakenStake[]"


KrakenUnstake::usage = 
"KrakenUnstake[]"


KrakenStakingAssets::usage = 
"KrakenStakingAssets[]"


KrakenStakingPending::usage = 
"KrakenStakingPending[]"


KrakenStakingTransactions::usage = 
"KrakenStakingTransactions[]"


(* ::Section::Closed:: *)
(*Kraken websocket rest api endpoints*)


KrakenGetWebSocketToken::usage = 
"KrakenGetWebSocketToken[]"


(* ::Section::Closed:: *)
(*Kraken websocket market channels*)


KrakenChannelCreate::usage = 
"KrakenChannelCreate[]"


KrakenChannelSubscribe::usage = 
"KrakenChannelSubscribe[connection, channel]
KrakenChannelSubscribe[connection, {channels}]"


KrakenChannelUnsubscribe::usage = 
"KrakenChannelUnsubscribe[connection, channel]
KrakenChannelUnsubscribe[connection, {channels}]"


KrakenTickerChannel::usage = 
"KrakenTickerChannel[pair]
KrakenTickerChannel[{pairs}]"


KrakenOHLCChannel::usage = 
"KrakenOHLCChannel[pair, interval]
KrakenOHLCChannel[{pairs}, interval]"


KrakenTradesChannel::usage = 
"KrakenTradesChannel[pair]
KrakenTradesChannel[{pairs}]"


KrakenSpreadChannel::usage = 
"KrakenSpreadChannel[pair]
KrakenSpreadChannel[{pairs}]"


KrakenBookChannel::usage = 
"KrakenBookChannel[pair]
KrakenBookChannel[{pairs}]"


Begin["`Private`"]


(* ::Section::Closed:: *)
(*encoder*)


encode[assoc_Association] := 
KeyValueMap[encode] @ DeleteCases[assoc, Automatic | Null]


encode[key_String, value_] := 
key -> encode[value]


encode[list: {__?AtomQ}] := 
StringRiffle[list, ","]


encode[date_DateObject] := 
UnixTime[date]


encode[expr_] := 
expr


(* ::Section::Closed:: *)
(*Kraken serializer*)


serialize[expr_] := 
ExportString[expr, "RawJSON"]


(* ::Section::Closed:: *)
(*JSON to expression*)


toExpr[unixtime_Integer] /; 10^9 < unixtime < 2 * 10^9 := 
FromUnixTime[unixtime]


toExpr[number_String] /; StringMatchQ[number, NumberString] := 
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
(*Deserializer*)


deserialize[body_String] := 
krakenToExpr @ ImportString[body, "RawJSON"]


(* ::Section::Closed:: *)
(*Kraken signature*)


Options[krakenSignature] = {
	"Serializer" :> krakenSerialize
}


krakenSignature[urlpath_String, data_Association, secret_String, OptionsPattern[]] := 
Module[{postdata, encoded, message, mac, sig, serializer}, 
	serializer = OptionValue["Serializer"]; 
	postdata = serializer[data]; 
	encoded = StringToByteArray[ToString["nonce" /. data] <> postdata]; 
	message = Join[StringToByteArray[urlpath], Hash[encoded, "SHA256", "ByteArray"]]; 
	mac = HMAC[message, BaseDecode[secret, "Base64"], "SHA512"]; 
	sig = BaseEncode[BaseDecode[mac, "Base16"], "Base64"]; 
	Return[sig]
]


(* ::Section::Closed:: *)
(*Kraken public request*)


Options[publicMethod] = {
	"Settings" :> $ExchangeLinkSettings, 
	"HTTPMethod" :> "GET", 
	"V" :> "0", 
	"API" :> "public", 
	"Encoder" :> encode, 
	"Deserializer" :> deserialize
}


publicMethod[{path__String, args: <|Rule[_String, _]...|>}, OptionsPattern[]] := 
Module[{settings, endpoint, httpMethod, v, api, parameters, url, request, response, body, result, 
	deserializer, encoder}, 
	
	settings = OptionValue["Settings"]; 
	endpoint = settings["Kraken", "Endpoint"]; 
	httpMethod = OptionValue["HTTPMethod"]; 
	v = OptionValue["V"]; 
	api = OptionValue["API"]; 
	
	encoder = OptionValue["Encoder"]; 
	deserializer = OptionValue["Deserializer"]; 
	
	parameters = encoder[args]; 
	url = URLBuild[{endpoint, v, api, path}, parameters]; 
	request = HTTPRequest[url, <|Method -> httpMethod|>]; 
	
	response = URLRead[request]; 
	saveToHistory[request, response]; 
	body = response["Body"]; 
	result = deserializer[body]; 
	
	If[result["error"] == {}, 
		result["result"], 
		result
	]
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
(*Kraken signed request*)


Options[signedMethod] = {
	"Settings" :> $ExchangeLinkSettings, 
	"HTTPMethod" :> "POST", 
	"V" :> "0", 
	"API" :> "private", 
	"Serializer" :> serialize, 
	"Deserializer" :> deserialize
}


signedMethod[{path__String, args_Association}, OptionsPattern[]] := 
Module[{settings, endpoint, httpMethod, v, api, parameters, url, request, response, body, result, 
	deserializer, serializer, apiKey, secretKey, uri, signature, requestBody}, 
	
	settings = OptionValue["Settings"]; 
	endpoint = settings["Kraken", "Endpoint"]; 
	
	apiKey = settings["Kraken", "APIKey"]; 
	secretKey = settings["Kraken", "SecretKey"]; 
	
	httpMethod = OptionValue["HTTPMethod"]; 
	v = OptionValue["V"]; 
	api = OptionValue["API"]; 
	
	serializer = OptionValue["Serializer"]; 
	deserializer = OptionValue["Deserializer"]; 
	
	parameters = Prepend[args, "nonce" -> nonce[]]; 
	url = URLBuild[{endpoint, v, api, path}]; 
	uri = URLBuild[{"", v, api, path}];
	signature = krakenSignature[uri, parameters, secretKey, "Serializer" -> serializer]; 
	requestBody = serializer[parameters]; 
	
	request = HTTPRequest[url, <|
		Method -> httpMethod, 
		"Headers" -> {
			"API-Key" -> apiKey, 
			"API-Sign" -> signature, 
			"Content-Type" -> "application/x-www-form-urlencoded; charset=utf-8"
		}, 
		"Body" -> requestBody
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
(*Kraken objects*)


krakenTradeOrderTypes[] := 
"market" | "limit" | "stop-loss" | "take-profit" | "stop-loss-limit" | "take-profit-limit" | "settle-position"


krakenTradeTypes[] := 
"buy" | "sell"


krakenChannelNames[] := 
"book" | "ohlc" | "openOrders" |" ownTrades" | "spread" | "ticker" |" trade" | "*"


(* ::Section::Bold::Closed:: *)
(*Kraken market data implementation*)


SyntaxInformation[KrakenTime] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> OptionNames[publicMethod]
}


KrakenTime[opts: OptionsPattern[publicMethod]] := 
publicMethod[{"Time"}, opts]


SyntaxInformation[KrakenSystemStatus] = {
	"ArgumentsPattern" -> {OptionsPattern[publicMethod]}, 
	"OptionNames" -> OptionNames[publicMethod]
}


KrakenSystemStatus[opts: OptionsPattern[publicMethod]] := 
publicMethod[{"SystemStatus"}, opts]


Options[KrakenAssets] = {
	"aclass" -> Automatic
}


SyntaxInformation[KrakenAssets] = {
	"ArgumentsPattern" -> {_., OptionsPattern[]}, 
	"OptionNames" -> OptionNames[publicMethod, KrakenAssets]
}


KrakenAssets[assets: _String | {__String}: Automatic, opts: OptionsPattern[{publicMethod, KrakenAssets}]] := 
publicMethod[{"Assets", "asset" -> assets, opts}, opts]


$KrakenAssets := 
cache[KrakenAssets[], 60 * 60]


Options[KrakenAssetPairs] = {
	"info" -> Automatic
}


SyntaxInformation[KrakenAssetPairs] = {
	"ArgumentsPattern" -> {_., OptionsPattern[]}, 
	"OptionNames" -> OptionNames[publicMethod, KrakenAssetPairs]
}


KrakenAssetPairs[pair: _String | {__String}: Automatic, opts: OptionsPattern[{publicMethod, KrakenAssetPairs}]] := 
publicMethod[{"AssetPairs", "pair" -> pair, opts}, opts]


$KrakenAssetPairs := 
cache[KrakenAssetPairs[], 60 * 60]


SyntaxInformation[KrakenTicker] = {
	"ArgumentsPattern" -> {_., OptionsPattern[]}, 
	"OptionNames" -> OptionNames[publicMethod]
}


KrakenTicker[pair: _String | {__String}: Automatic, opts: OptionsPattern[publicMethod]] := 
publicMethod[{"Ticker", "pair" -> pair}, opts]


Options[KrakenOHLC] = {
	"interval" -> Automatic, 
	"since" -> Automatic
}


SyntaxInformation[KrakenOHLC] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[publicMethod, KrakenOHLC]
}


KrakenOHLC[pair_String, opts: OptionsPattern[{publicMethod, KrakenOHLC}]] := 
publicMethod[{"OHLC", "pair" -> pair, opts}, opts]


Options[KrakenOrderBook] = {
	"count" -> Automatic
}


SyntaxInformation[KrakenOrderBook] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[publicMethod, KrakenOrderBook]
}


KrakenOrderBook[pair_String, opts: OptionsPattern[{publicMethod, KrakenOrderBook}]] := 
publicMethod[{"Depth", "pair" -> pair, opts}, opts]


Options[KrakenTrades] = {
	"since" -> Automatic
}


SyntaxInformation[KrakenTrades] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[publicMethod, KrakenTrades]
}


KrakenTrades[pair_String, opts: OptionsPattern[{publicMethod, KrakenTrades}]] := 
publicMethod[{"Trades", "pair" -> pair, opts}, opts]


Options[KrakenSpread] = {
	"since" -> Automatic
}


SyntaxInformation[KrakenSpread] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[publicMethod, KrakenSpread]
}


KrakenSpread[pair_String, opts: OptionsPattern[{publicMethod, KrakenSpread}]] := 
publicMethod[{"Spread", "pair" -> pair, opts}, opts]


(* ::Section::Bold::Closed:: *)
(*Kraken user trading api implementation*)


AddCodeCompletion[KrakenAddOrder][
	{"market", "limit", "stop-loss", "take-profit", "stop-loss-limit", "take-profit-limit", "settle-position"}, 
	{"buy", "sell"}, 
	0, 
	{"BTCUSDT", "ETHUSDT", "ETHBTC"}
]


Options[KrakenAddOrder] = {
	"userref" -> Automatic, 
	"displayvol" -> Automatic, 
	"price" -> Automatic, 
	"price2" -> Automatic, 
	"trigger" -> Automatic, 
	"leverage" -> Automatic,
	"stptype" -> Automatic,
	"oflags" -> Automatic,
	"timeinforce" -> Automatic,
	"starttm" -> Automatic,
	"expiretm" -> Automatic,
	"close[ordertype]" -> Automatic,
	"close[price]" -> Automatic,
	"close[price2]" -> Automatic,
	"deadline" -> Automatic,
	"validate" -> Automatic
}


SyntaxInformation[KrakenAddOrder] = {
	"ArgumentsPattern" -> {_, _, _, _, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod, KrakenAddOrder]
}


KrakenAddOrder[ordertype_String, type_String, volume_?NumericQ, pair_String, 
	opts: OptionsPattern[{signedMethod, KrakenAddOrder}]] := 
signedMethod[{"AddOrder", "ordertype" -> ordertype, "type" -> type, "volume" -> volume, "pair" -> pair, opts}, opts]


Options[KrakenAddOrderBatch] = {
	"deadline" -> Automatic, 
	"validate" -> Automatic
}


SyntaxInformation[KrakenOpenOrders] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod, KrakenAddOrderBatch]
}


KrakenAddOrderBatch[pair_, orders_, opts: OptionsPattern[{signedMethod, KrakenAddOrderBatch}]] := 
signedMethod[{"AddOrderBatch", "orders" -> orders, "pair" -> pair, opts}, opts, "Serializer" -> krakenSerializeToJSON]


Options[KrakenEditOrder] = {
	"userref" -> Automatic, 
	"volume" -> Automatic, 
	"displayvol" -> Automatic, 
	"price" -> Automatic, 
	"price2" -> Automatic, 
	"oflags" -> Automatic, 
	"deadline" -> Automatic, 
	"cancel_response" -> Automatic, 
	"validate" -> Automatic
}


SyntaxInformation[KrakenEditOrder] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod, KrakenEditOrder]
}


KrakenEditOrder[txid: _String | _Integer, pair_String, opts: OptionsPattern[{signedMethod, KrakenEditOrder}]] := 
signedMethod[{"EditOrder", "txid" -> txid, "pair" -> pair, opts}, opts]


SyntaxInformation[KrakenCancelOrder] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod]
}


KrakenCancelOrder[txid: _String | _Integer, opts: OptionsPattern[signedMethod]] := 
signedMethod[{"CancelOrder", "txid" -> txid}, opts]


SyntaxInformation[KrakenCancelAll] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod]
}


KrakenCancelAll[opts: OptionsPattern[signedMethod]] := 
signedMethod[{"CancelAll"}, opts]


SyntaxInformation[KrakenCancelAllOrdersAfter] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod]
}


KrakenCancelAllOrdersAfter[timeout_, opts: OptionsPattern[{signedMethod}]] := 
signedMethod[{"CancelAllOrdersAfter", "timeout" -> timeout},  opts]


SyntaxInformation[KrakenCancelAllOrdersBatch] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod]
}


KrakenCancelAllOrdersBatch[orders_List, opts: OptionsPattern[signedMethod]] := 
signedMethod[{"CancelAllOrdersBatch", "orders" -> orders}, opts, "Serializer" -> krakenSerializeToJSON]


(* ::Section::Bold::Closed:: *)
(*Kraken user data implementation*)


SyntaxInformation[KrakenBalance] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod]
}


KrakenBalance[opts: OptionsPattern[{signedMethod}]] := 
signedMethod[{"Balance"}, opts]


SyntaxInformation[KrakenTradeBalance] = {
	"ArgumentsPattern" -> {_., OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod]
}


KrakenTradeBalance[opts: OptionsPattern[{signedMethod}]] := 
signedMethod[{"TradeBalance"}, opts]


KrakenTradeBalance[asset_String, opts: OptionsPattern[{signedMethod}]] := 
signedMethod[{"TradeBalance", "asset" -> asset}, opts]


Options[KrakenOpenOrders] = {
	"trades" -> Automatic, 
	"userref" -> Automatic
}


SyntaxInformation[KrakenOpenOrders] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod, KrakenOpenOrders]
}


KrakenOpenOrders[opts: OptionsPattern[{signedMethod, KrakenOpenOrders}]] := 
signedMethod[{"OpenOrders", opts}, opts]


Options[KrakenClosedOrders] = {
	"trades" -> Automatic, 
	"userref" -> Automatic, 
	"start" -> Automatic, 
	"end" -> Automatic, 
	"ofs" -> Automatic, 
	"closetime" -> Automatic
}


SyntaxInformation[KrakenOpenOrders] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod, KrakenClosedOrders]
}


KrakenClosedOrders[opts: OptionsPattern[{signedMethod, KrakenClosedOrders}]] := 
signedMethod[{"ClosedOrders", opts}, opts]


Options[KrakenQueryOrders] = {
	"trades" -> Automatic, 
	"userref" -> Automatic
}


SyntaxInformation[KrakenOpenOrders] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod, KrakenQueryOrders]
}


KrakenQueryOrders[txid_String, opts: OptionsPattern[{signedMethod, KrakenQueryOrders}]] := 
signedMethod[{"QueryOrders", "txid" -> txid, opts}, opts]


Options[KrakenTradeHistory] = {
	"type" -> Automatic, 
	"trades" -> Automatic, 
	"start" -> Automatic, 
	"end" -> Automatic, 
	"ofs" -> Automatic
}


SyntaxInformation[KrakenTradeHistory] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod, KrakenTradeHistory]
}


KrakenTradeHistory[opts: OptionsPattern[{signedMethod, KrakenTradeHistory}]] := 
signedMethod[{"TradeHistory", opts}, opts]


Options[KrakenQueryTrades] = {
	"txid" -> Automatic, 
	"trades" -> Automatic
}


SyntaxInformation[KrakenQueryTrades] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod, KrakenQueryTrades]
}


KrakenQueryTrades[opts: OptionsPattern[{signedMethod, KrakenQueryTrades}]] := 
signedMethod[{"QueryTrades", opts}, opts]


Options[KrakenOpenPositions] = {
	"txid" -> Automatic, 
	"docalcs" -> Automatic, 
	"consolidation" -> Automatic
}


SyntaxInformation[KrakenOpenPositions] = {
	"ArgumentsPattern" -> {OptionsPattern[{signedMethod, KrakenOpenPositions}]}, 
	"OptionNames" -> OptionNames[signedMethod, KrakenOpenPositions]
}


KrakenOpenPositions[opts: OptionsPattern[{signedMethod, KrakenOpenPositions}]] := 
signedMethod[{"OpenPositions", opts}, opts]


Options[KrakenLedgers] = {
	"asset" -> Automatic, 
	"aclass" -> Automatic, 
	"type" -> Automatic, 
	"start" -> Automatic, 
	"end" -> Automatic, 
	"ofs" -> Automatic
}


SyntaxInformation[KrakenLedgers] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod, KrakenLedgers]
}


KrakenLedgers[opts: OptionsPattern[{signedMethod, KrakenLedgers}]] := 
signedMethod[{"Ledgers", opts}, opts]


Options[KrakenQueryLedgers] = {
	"id" -> Automatic, 
	"trades" -> Automatic
}


SyntaxInformation[KrakenQueryLedgers] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod, KrakenQueryLedgers]
}


KrakenQueryLedgers[opts: OptionsPattern[{signedMethod, KrakenQueryLedgers}]] := 
signedMethod[{"QueryLedgers", opts}, opts]


Options[KrakenTradeVolume] = {
	"pair" -> Automatic, 
	"fee-info" -> Automatic
}


SyntaxInformation[KrakenTradeVolume] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod, KrakenTradeVolume]
}


KrakenTradeVolume[opts: OptionsPattern[{signedMethod, KrakenTradeVolume}]] := 
signedMethod[{"TradeVolume", opts}, opts]


Options[KrakenAddExport] = {
	"format" -> Automatic, 
	"fields" -> Automatic, 
	"starttm" -> Automatic, 
	"endtm" -> Automatic
}


SyntaxInformation[KrakenAddExport] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod, KrakenAddExport]
}


KrakenAddExport[report: "trades" | "ledgers", description_String, 
	opts: OptionsPattern[{signedMethod, KrakenAddExport}]] := 
signedMethod[{"AddExport", "report" -> report, "description" -> description, opts}, opts]


SyntaxInformation[KrakenExportStatus] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod]
}


KrakenExportStatus[report: "trades" | "ledgers", opts: OptionsPattern[signedMethod]] := 
signedMethod[{"ExportStatus", "report" -> report}, opts]


SyntaxInformation[KrakenRetrieveExport] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod]
}


KrakenRetrieveExport[id_String, opts: OptionsPattern[signedMethod]] := 
signedMethod[{"RetrieveExport", "id" -> id}, opts]


SyntaxInformation[KrakenOpenOrders] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[{signedMethod}]}, 
	"OptionNames" -> OptionNames[signedMethod]
}


KrakenRemoveExport[id_String, type: "cancel" | "delete", opts: OptionsPattern[signedMethod]] := 
signedMethod[{"RemoveExport", "id" -> id, "type" -> type}, opts]


(* ::Section::Bold::Closed:: *)
(*Kraken user funding api implementation*)


SyntaxInformation[KrakenDepositMethods] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod]
}


KrakenDepositMethods[asset_String, opts: OptionsPattern[]] := 
signedMethod[{"DepositMethods", "asset" -> asset}, opts]


Options[KrakenDepositAddresses] = {
	"new" -> Automatic
}


SyntaxInformation[KrakenDepositAddresses] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod, KrakenDepositAddresses]
}


KrakenDepositAddresses[asset_String, method_String, 
	opts: OptionsPattern[{signedMethod, KrakenDepositAddresses}]] := 
signedMethod[{"DepositAddresses", "asset" -> asset, "method" -> method, opts}, opts]


Options[KrakenDepositStatus] = {
	"method" -> Automatic
}


SyntaxInformation[KrakenDepositStatus] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod, KrakenDepositStatus]
}


KrakenDepositStatus[asset_String, opts: OptionsPattern[{signedMethod, KrakenDepositStatus}]] := 
signedMethod[{"DepositStatus", "asset" -> asset, opts}, opts]


SyntaxInformation[KrakenWithdrawInfo] = {
	"ArgumentsPattern" -> {_, _, _, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod]
}


KrakenWithdrawInfo[asset_String, key_String, amount_, opts: OptionsPattern[signedMethod]] := 
signedMethod[{"WithdrawInfo", "asset" -> asset, "key" -> key, "amount" -> amount}, opts]


SyntaxInformation[KrakenWithdraw] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod]
}


KrakenWithdraw[asset_String, key_String, amount_, opts: OptionsPattern[signedMethod]] := 
signedMethod[{"Withdraw", "asset" -> asset, "key" -> key, "amount" -> amount}, opts]


Options[KrakenWithdrawStatus] = {
	"method" -> Automatic
}


SyntaxInformation[KrakenWithdrawStatus] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod, KrakenWithdrawStatus]
}


KrakenWithdrawStatus[asset_String, opts: OptionsPattern[{signedMethod, KrakenWithdrawStatus}]] := 
signedMethod[{"WithdrawStatus", "asset" -> asset, opts}, opts]


SyntaxInformation[KrakenWithdrawCancel] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod]
}


KrakenWithdrawCancel[asset_String, refid_String, opts: OptionsPattern[signedMethod]] := 
signedMethod[{"WithdrawCancel", "asset" -> asset, "refid" -> refid}, opts]


SyntaxInformation[KrakenWalletTransfer] = {
	"ArgumentsPattern" -> {_, _, _, _, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod]
}


KrakenWalletTransfer[asset_, from_, to_, amount_, opts: OptionsPattern[signedMethod]] := 
signedMethod[{"WalletTransfer", "asset" -> asset, "from" -> from, "to" -> to, "amount" -> amount}, opts]


(* ::Section::Bold::Closed:: *)
(*Kraken user staking api implementation*)


SyntaxInformation[KrakenStake] = {
	"ArgumentsPattern" -> {_, _, _, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod]
}


KrakenStake[asset_, amount_, method_, opts: OptionsPattern[signedMethod]] := 
signedMethod[{"Stake", "asset" -> asset, "amount" -> amount, "method" -> method}, opts]


SyntaxInformation[KrakenUnstake] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod]
}


KrakenUnstake[asset_, amount_, opts: OptionsPattern[signedMethod]] := 
signedMethod[{"Unstake", "asset" -> asset, "amount" -> amount}, opts]


SyntaxInformation[KrakenStakingAssets] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod]
}


KrakenStakingAssets[opts: OptionsPattern[signedMethod]] := 
signedMethod[{"StakingAssets"}, opts]


SyntaxInformation[KrakenStakingPending] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod]
}


KrakenStakingPending[opts: OptionsPattern[signedMethod]] := 
signedMethod[{"StakingPending"}, opts]


SyntaxInformation[KrakenStakingTransactions] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod]
}


KrakenStakingTransactions[opts: OptionsPattern[signedMethod]] := 
signedMethod[{"StakingTransactions"}, opts]


(* ::Section::Bold::Closed:: *)
(*Kraken websocket rest api method*)


SyntaxInformation[KrakenGetWebSocketToken] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> OptionNames[signedMethod]
}


KrakenGetWebSocketToken[opts: OptionsPattern[signedMethod]] := 
signedMethod[{"GetWebSocketToken"}, opts]


(* ::Section::Bold::Closed:: *)
(*Kraken websocket channels*)


Options[KrakenChannelCreate] = {
	"Settings" :> $ExchangeLinkSettings, 
	"Deserializer" :> deserialize, 
	"EventHandler" :> Print
}


KrakenChannelCreate[opts: OptionsPattern[{}]] := 
Module[{settings, webSocketEndpoint, deserializer, eventHandler}, 
	settings = OptionValue[KrakenChannelCreate, opts, "Settings"]; 
	deserializer = OptionValue[KrakenChannelCreate, opts, "Deserializer"]; 
	eventHandler = OptionValue[KrakenChannelCreate, opts, "EventHandler"]; 
	webSocketEndpoint = settings["Kraken", "WebSocketEndpoint"]; 
	WebSocketConnect[webSocketEndpoint, "Deserializer" -> deserializer, "EventHandler" -> eventHandler]
]


Options[KrakenChannelSubscribe] = {
	"Serializer" :> krakenSerializeToJSON, 
	"depth" -> Automatic, 
	"interval" -> Automatic, 
	"ratecounter" -> Automatic, 
	"snapshot" -> Automatic, 
	"token" -> Automatic
}


KrakenChannelSubscribe[connection_WebSocketConnectionObject, 
	pairs: _String | {___String}, name: krakenChannelNames[]: "*", opts: OptionsPattern[{}]] := 
Module[{serializer, id, frame, subscription}, 
	serializer = OptionValue[KrakenChannelSubscribe, opts, "Serializer"]; 
	
	subscription = DeleteCases[<|
		"depth" -> OptionValue[KrakenChannelSubscribe, opts, "depth"], 
		"interval" -> OptionValue[KrakenChannelSubscribe, opts, "interval"], 
		"name" -> name, 
		"ratecounter" -> OptionValue[KrakenChannelSubscribe, opts, "ratecounter"], 
		"snapshot" -> OptionValue[KrakenChannelSubscribe, opts, "snapshot"], 
		"token" -> OptionValue[KrakenChannelSubscribe, opts, "token"]
	|>, Automatic | Null];
	
	frame = <|
		"event" -> "subscribe", 
		"pair" -> Flatten[{pairs}], 
		"subscription" -> subscription
	|>; 
	
	WebSocketSend[connection, frame, "Serializer" -> serializer]
]


(* ::Section::Bold::Closed:: *)
(*Kraken websocket public channels*)


KrakenTickerChannel[pairs: _String | {__String}, 
	opts: OptionsPattern[{KrakenChannelCreate, KrakenChannelSubscribe}]] := 
Module[{connection}, 
	connection = KrakenChannelCreate[opts]; 
	KrakenChannelSubscribe[connection, pairs, "ticker", opts]; 
	Return[connection]
]


KrakenOHLCChannel[pairs: _String | {__String}, interval_Integer, 
	opts: OptionsPattern[{KrakenChannelCreate, KrakenChannelSubscribe}]] := 
Module[{connection}, 
	connection = KrakenChannelCreate[opts]; 
	KrakenChannelSubscribe[connection, pairs, "ohlc", "interval" -> interval, opts]; 
	Return[connection]
]


KrakenTradesChannel[pairs: _String | {__String}, 
	opts: OptionsPattern[{KrakenChannelCreate, KrakenChannelSubscribe}]] := 
Module[{connection}, 
	connection = KrakenChannelCreate[opts]; 
	KrakenChannelSubscribe[connection, pairs, "trade", opts]; 
	Return[connection]
]


KrakenSpreadChannel[pairs: _String | {__String}, 
	opts: OptionsPattern[{KrakenChannelCreate, KrakenChannelSubscribe}]] := 
Module[{connection}, 
	connection = KrakenChannelCreate[opts]; 
	KrakenChannelSubscribe[connection, pairs, "spread", opts]; 
	Return[connection]
]


KrakenBookChannel[pairs: _String | {__String}, 
	opts: OptionsPattern[{KrakenChannelCreate, KrakenChannelSubscribe}]] := 
Module[{connection}, 
	connection = KrakenChannelCreate[opts]; 
	KrakenChannelSubscribe[connection, pairs, "book", opts]; 
	Return[connection]
]


(* ::Section::Bold::Closed:: *)
(*Kraken websocket user channels*)


KrakenOwnTradesChannel[opts: OptionsPattern[{signedMethod, KrakenChannelCreate, KrakenChannelSubscribe}]] := 
Module[{connection}, 
	connection = KrakenChannelCreate[opts]; 
	token = signedMethod[{"GetWebSocketToken"}, opts]; 
	KrakenChannelSubscribe[connection, {}, "ownTrades", opts]; 
	Return[connection]
]


KrakenOpenOrdersChannel[opts: OptionsPattern[{signedMethod, KrakenChannelCreate, KrakenChannelSubscribe}]] := 
Module[{connection}, 
	connection = KrakenChannelCreate[opts]; 
	token = signedMethod[{"GetWebSocketToken"}, opts]; 
	KrakenChannelSubscribe[connection, {}, "openOrders", opts]; 
	Return[connection]
]


End[] (*`Private`*)


EndPackage[] (*KirillBelov`ExchangeLink`Kraken`*)
