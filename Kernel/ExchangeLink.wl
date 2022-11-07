(* ::Package:: *)

(* ::Title:: *)
(*ExchangeLink*)


(* ::Chapter:: *)
(*Begin*)


(* ::Section::Closed:: *)
(*Begin package*)


BeginPackage["KirillBelov`ExchangeLink`", {"WebSocketJLink`"}]


(* ::Section::Closed:: *)
(*Clear names*)


ClearAll["`*"]


(* ::Chapter:: *)
(*ExchangeLink declaration*)


(* ::Section::Italic::Closed:: *)
(*ExchangeLink names*)


$ExchangeLinkSettings::usage = 
"$ExchangeLinkSettings"


$ExchangeLinkHistory::usage = 
"$ExchangeLinkHistory"


$ExchangeLinkDirectory::usage = 
"$ExchangeLinkDirectory"


(* ::Chapter:: *)
(*Binance declaration*)


(* ::Section::Italic::Closed:: *)
(*Binance market data endpoints*)


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


BinanceOrderBook::usage = 
"BinanceOrderBook[SYMBOL]
BinanceOrderBook[SYMBOL, opts]"


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


BinanceOrderBookTicker::usage = 
"BinanceOrderBookTicker[]
BinanceOrderBookTicker[SYMBOL]
BinanceOrderBookTicker[{SYMBOLS}]"


BinanceTicker::usage = 
"BinanceTicker[SYMBOL]
BinanceTicker[{SYMBOLS}]
BinanceTicker[SYMBOLS, opts]"


(* ::Section::Italic::Closed:: *)
(*Binance websocket market streams*)


BinanceStreamCreate::usage = 
"BinanceStreamCreate[stream]
BinanceStreamCreate[{streams}]"


BinanceStreamSubscribe::usage = 
"BinanceStreamSubscribe[connections, stream]
BinanceStreamSubscribe[connections, {streams}]"


BinanceStreamUnsubscribe::usage = 
"BinanceStreamUnsubscribe[connection] - unsubscribe all
BinanceStreamUnsubscribe[connection, stream] - specific stream
BinanceStreamUnsubscribe[connection, {streams}] - several streams"


BinanceStreamSubscribtions::usage = 
"BinanceStreamSubscribtions[]"


BinanceAggTradesStream::usage = 
"BinanceAggTradesStream[SYMBOL]
BinanceAggTradesStream[connection, SYMBOL]"


BinanceTradeStream::usage = 
"BinanceTradeStream[SYMBOL]
BinanceTradeStream[connection, SYMBOL]"


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


(* ::Section::Italic:: *)
(*Binance spot trade*)


(* ::Chapter:: *)
(*Kraken declaration*)


(* ::Section::Italic::Closed:: *)
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


(* ::Section::Italic::Closed:: *)
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


(* ::Section::Italic::Closed:: *)
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


(* ::Section::Italic::Closed:: *)
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


(* ::Section::Italic::Closed:: *)
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


(* ::Section::Italic::Closed:: *)
(*Kraken web socket rest api endpoints*)


KrakenGetWebSocketToken::usage = 
"KrakenGetWebSocketToken[]"


(* ::Chapter:: *)
(*Private*)


(* ::Section::Closed:: *)
(*Private*)


Begin["`Private`"]


(* ::Chapter:: *)
(*ExchangeLink*)


(* ::Section::Bold::Closed:: *)
(*ExchangeLink directory*)


$ExchangeLinkDirectory = 
ParentDirectory[DirectoryName[$InputFileName]]


(* ::Section::Bold::Closed:: *)
(*ExchangeLink history*)


$ExchangeLinkHistory := 
$ExchangeLinkHistory = 
CreateDataStructure["RingBuffer", 1024]


saveToHistory[request_, response_] := 
$ExchangeLinkHistory["PushBack", <|
	"Time" -> Now, 
	"Request" -> request, 
	"Response" -> response
|>]; 


(* ::Section::Bold::Closed:: *)
(*ExchangeLink settings*)


exchangeLinkSettings[] := 
Which[
	FileExistsQ[FileNameJoin[{$HomeDirectory, ".ExchangeLink"}]], 
		Get[FileNameJoin[{$HomeDirectory, ".ExchangeLink"}]], 
	FileExistsQ[FileNameJoin[{$ExchangeLinkDirectory, ".ExchangeLink"}]], 
		Get[FileNameJoin[{$ExchangeLinkDirectory, ".ExchangeLink"}]]
]


$ExchangeLinkSettings := 
cache[exchangeLinkSettings[], 60]


(* ::Chapter:: *)
(*Common internal functions*)


(* ::Section::Closed:: *)
(*Cache*)


SetAttributes[cache, HoldFirst]


cache[expr_, date_DateObject] := (
	cache[expr, {"Date"}] = date; 
	cache[expr, date] = expr
)


cache[expr_, period_Integer: 60] := 
Module[{time = AbsoluteTime[], roundedTime, previouseTime = cache[expr, {"Date"}]}, 
	roundedTime = DateObject[Round[time, period]]; 
	If[DateObjectQ[previouseTime] && roundedTime != previouseTime, 
		cache[expr, previouseTime] =.; 
		previouseTime = roundedTime; 
		cache[expr, {"Date"}] = previouseTime; 
		cache[expr, previouseTime] = expr, 
		
		previouseTime = roundedTime;
	]; 
	Return[cache[expr, previouseTime]]
]


(* ::Section::Closed:: *)
(*Options*)


optionNames[funcs__Symbol] := 
Map["\"" <> # <> "\""&] @ Keys[Flatten[Map[Options] @ {funcs}]]


(* ::Section::Closed:: *)
(*HMAC*)


$HashSizes = <|
	"MD2" -> {128, 128}, "MD4" -> {128, 512}, "MD5" -> {128, 512}, 
	"SHA" -> {160, 512}, "SHA1" -> {160, 512}, 
	"SHA224" -> {224, 512}, "SHA256" -> {256, 512}, 
	"SHA384" -> {384, 1024}, "SHA512" -> {512, 1024}, 
	"RIPEMD160" -> {160, 512}, "RIPEMD160SHA256" -> {160, 512}, 
	"SHA256SHA256" -> {256, 512}, "SHA3-224" -> {224, 1152}, "SHA3-256" -> {256, 1088}, "SHA3-384" -> {384, 832}, 
	"SHA3-512" -> {512, 576}, 
	"Keccak224" -> {224, 1152}, "Keccak256" -> {256, 1088}, "Keccak384" -> {384, 832}, "Keccak512" -> {512, 576}
|>


HMAC[data_, key_SymmetricKey, hash_String: "SHA256"] := 
HMAC[data, key["Key"], hash];


HMAC[data_, key: {___Integer}, hash_String: "SHA256"] := 
With[{bytes = ByteArray[key]}, HMAC[data, bytes, hash]/; ByteArrayQ@bytes];


HMAC[data_, key_String/; StringQ[Unevaluated[key]], hash_String: "SHA256"] := 
HMAC[data, StringToByteArray[key], hash];


HMAC[expr: Except[_String | _ByteArray], key_ByteArray/; ByteArrayQ[Unevaluated[key]], hash_String: "SHA256"] := 
HMAC[BinarySerialize[expr], key, hash];


HMAC[message_String/; StringQ[Unevaluated[message]], 
	key_ByteArray /; ByteArrayQ[Unevaluated[key]], hash_String: "SHA256"]:=
HMAC[StringToByteArray[message], key, hash];


HMAC[data_ByteArray/; ByteArrayQ[Unevaluated[data]], 
	key_ByteArray /; ByteArrayQ[Unevaluated[key]], hash_String: "SHA256"] := 
Module[{inkey, invec, key0, blocksize, ipad, opad, ipadkey, opadkey, temp, bhash}, 
	
	blocksize = Last[$HashSizes[hash]] * 1/8;
	invec = Developer`FromByteArray[data];
	
	If[Length[key] > blocksize,
		inkey = Developer`FromByteArray[Hash[key, hash, "ByteArray"]],
		inkey = Developer`FromByteArray[key]
	]; 
	
	key0 = PadRight[inkey, blocksize]; 
	ipad = Table[54, blocksize]; 
	opad = Table[92, blocksize]; 
	ipadkey = BitXor[key0, ipad]; 
	opadkey = BitXor[key0, opad]; 
	temp = Hash[ByteArray[Join[ipadkey, invec]], hash, "ByteArray"]; 
	bhash = Hash[ByteArray[Join[opadkey, Developer`FromByteArray[temp]]], hash, "ByteArray"]; 
	
	Return[ToLowerCase[BaseEncode[bhash, "Base16"]]]
]


(* ::Section::Closed:: *)
(*Completion*)


addCodeCompletion[function_Symbol][args___] := 
Module[{processed}, 
	processed = {args} /. {
		None -> 0, 
		"AbsoluteFileName" -> 2, 
		"RelativeFileName" -> 3, 
		"Color" -> 4, 
		"PackageName" -> 7, 
		"DirectoryName" -> 8, 
		"InterpreterType" -> 9
	}; 
	(FE`Evaluate[FEPrivate`AddSpecialArgCompletion[#]]&)[SymbolName[function] -> processed]
]


optionCompletionDir = 
FileNameJoin[{$InstallationDirectory, "SystemFiles", "Components", "AutoCompletionData", "Main", "OptionValues"}]


addOptionCompletion[func_Symbol, args: {__Rule}] := 
Module[{file = FileNameJoin[{optionCompletionDir, SymbolName[func] <> ".m"}]}, 
	Put[Map[First[#] -> Map[ToString] @ Last[#]&] @ args, file]
]


(* ::Section::Closed:: *)
(*Timestamp*)


timestamp[date_DateObject?DateObjectQ] := 
Round[1000 * (UnixTime[date] + FractionalPart[AbsoluteTime[date]])]


timestamp[] := 
timestamp[Now]


(* ::Section::Closed:: *)
(*Nonce*)


nonce[] := 
timestamp[]


(* ::Chapter:: *)
(*Binance internal functions*)


(* ::Section::Closed:: *)
(*Biannce JSON to expression*)


binanceToExpr[timestamp_Integer] /; timestamp > 1.5 * 10^12 := 
FromUnixTime[timestamp / 1000]


binanceToExpr[number_String] /; StringMatchQ[number, NumberString] := 
ToExpression[number]


binanceToExpr[list_List] := 
Map[binanceToExpr, list]


binanceToExpr[assoc_Association] := 
<|KeyValueMap[binanceToExpr, assoc]|>


binanceToExpr[expr_?AtomQ] := 
expr


binanceToExpr[key_String, value_] := 
key -> binanceToExpr[value]


(* ::Section::Closed:: *)
(*Binance encoder*)


binanceEncode[date_DateObject] := 
timestamp[date]


binanceEncode[assoc_Association] := 
KeyValueMap[binanceEncode] @ DeleteCases[assoc, Automatic | Null]


binanceEncode[rules: {___Rule}] := 
binanceEncode @ <|rules|>


binanceEncode[Rule[key_String, value_]] := 
key -> binanceEncode[value]


binanceEncode[list: {Except[_Rule]..}] := 
StringDelete[ExportString[list, "RawJSON"], WhitespaceCharacter]


binanceEncode[expr_] := 
ToString[expr]


binanceEncode[key_String, value_] := 
key -> binanceEncode[value]


(* ::Section::Closed:: *)
(*Binance serializer*)


binanceSerialize[assoc_Association] := 
ExportString[assoc, "RawJSON"]


(* ::Section::Closed:: *)
(*Binance deserialzer*)


binanceDeserialize[body_String] := 
binanceToExpr @ ImportString[body, "RawJSON"]


(* ::Section::Closed:: *)
(*Binance public request*)


Options[binancePublic] = {
    "HTTPMethod" :> "GET", 
    "V" :> "v3", 
    "API" :> "api", 
    "Settings" :> $ExchangeLinkSettings, 
    "Encoder" :> binanceEncode, 
    "Deserializer" :> binanceDeserialize
}


binancePublic[{method__String, args_?AssociationQ}, OptionsPattern[]] := 
Module[{httpMethod, settings, endpoint, url, request, response, body, result, 
	encoder, deserializer, parameters, v, api}, 
	
	v = OptionValue["V"]; 
	api = OptionValue["API"]; 
    httpMethod = OptionValue["HTTPMethod"]; 
	settings = OptionValue["Settings"]; 
    endpoint = settings["Binance", "Endpoint"]; 
    encoder = OptionValue["Encoder"]; 
    deserializer = OptionValue["Deserializer"]; 

    parameters = encoder[args]; 
    url = URLBuild[{endpoint, api, v, method}, parameters]; 
    request = HTTPRequest[url, <|Method -> httpMethod|>]; 
    
    response = URLRead[request]; 
    saveToHistory[request, response]; 
    
    body = response["Body"]; 
    result = deserializer[body]; 

    Return[result]
]


binancePublic[{method__String, args: OptionsPattern[{}]}, opts: OptionsPattern[{}]] := 
binancePublic[
	{
		method, 
		<|FilterRules[Flatten[{args}], Except[Options[binancePublic]]]|>
	}, 
	FilterRules[Flatten[{opts}], Options[binancePublic]]
]


(* ::Section:: *)
(*Binance signed request*)


(* ::Chapter:: *)
(*Binance*)


(* ::Section::Bold::Closed:: *)
(*Binance market data implementation*)


SyntaxInformation[BinancePing] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> optionNames[binancePublic]
}


BinancePing[opts: OptionsPattern[binancePublic]] := 
binancePublic[{"ping"}, opts]


SyntaxInformation[BinanceTime] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> optionNames[binancePublic]
}


BinanceTime[opts: OptionsPattern[binancePublic]] := 
binancePublic[{"time"}, opts]


Options[BinanceExchangeInfo] = {
	"permissions" -> Automatic
}


SyntaxInformation[BinanceExchangeInfo] = {
	"ArgumentsPattern" -> {_., OptionsPattern[]}, 
	"OptionNames" -> optionNames[binancePublic, BinanceExchangeInfo]
}


BinanceExchangeInfo[opts: OptionsPattern[{binancePublic, BinanceExchangeInfo}]] := 
binancePublic[{"exchangeInfo", opts}, opts]


BinanceExchangeInfo[symbol_String, opts: OptionsPattern[{binancePublic, BinanceExchangeInfo}]] := 
binancePublic[{"exchangeInfo", "symbol" -> symbol, opts}, opts]


BinanceExchangeInfo[symbols: {__String}, opts: OptionsPattern[{binancePublic, BinanceExchangeInfo}]] := 
binancePublic[{"exchangeInfo", "symbols" -> symbols, opts}, opts]


$BinanceExchangeInfo := 
cache[BinanceExchangeInfo[], 60 * 60]


Options[BinanceOrderBook] = {
	"limit" -> Automatic
}


SyntaxInformation[BinanceOrderBook] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> optionNames[binancePublic, BinanceOrderBook]
}


BinanceOrderBook[symbol_String, opts: OptionsPattern[{binancePublic, BinanceOrderBook}]] := 
binancePublic[{"depth", "symbol" -> symbol, opts}, opts]


Options[BinanceTrades] = {
	"limit" -> Automatic
}


SyntaxInformation[BinanceTrades] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> optionNames[binancePublic, BinanceTrades]
}


BinanceTrades[symbol_String, opts: OptionsPattern[{binancePublic, BinanceTrades}]] := 
binancePublic[{"trades", "symbol" -> symbol, opts}, opts]


Options[BinanceHistoricalTrades] = {
	"limit" -> Automatic, 
	"fromId" -> Automatic
}


SyntaxInformation[BinanceHistoricalTrades] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> optionNames[binancePublic, BinanceHistoricalTrades]
}


BinanceHistoricalTrades[symbol_String, opts: OptionsPattern[{binancePublic, BinanceHistoricalTrades}]] := 
binancePublic[{"historicalTrades", "symbol" -> symbol, opts}, opts]


Options[BinanceAggTrades] = {
	"limit" -> Automatic, 
	"fromId" -> Automatic, 
	"startTime" -> Automatic, 
	"endTime" -> Automatic
}


SyntaxInformation[BinanceAggTrades] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> optionNames[binancePublic, BinanceAggTrades]
}


BinanceAggTrades[symbol_String, opts: OptionsPattern[{binancePublic, BinanceAggTrades}]] := 
binancePublic[{"aggTrades", "symbol" -> symbol, opts}, opts]


Options[BinanceKlines] = {
	"limit" -> Automatic, 
	"startTime" -> Automatic, 
	"endTime" -> Automatic
}


SyntaxInformation[BinanceKlines] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[]}, 
	"OptionNames" -> optionNames[binancePublic, BinanceKlines]
}


BinanceKlines[symbol_String, interval_String, opts: OptionsPattern[{binancePublic, BinanceKlines}]] := 
binancePublic[{"klines", "symbol" -> symbol, "interval" -> interval, opts}, opts]


Options[BinanceUIKlines] = {
	"limit" -> Automatic, 
	"startTime" -> Automatic, 
	"endTime" -> Automatic
}


SyntaxInformation[BinanceUIKlines] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[]}, 
	"OptionNames" -> optionNames[binancePublic, BinanceUIKlines]
}


BinanceUIKlines[symbol_String, interval_String, opts: OptionsPattern[{binancePublic, BinanceUIKlines}]] := 
binancePublic[{"uiKlines", "symbol" -> symbol, "interval" -> interval, opts}, opts]


SyntaxInformation[BinanceAveragePrice] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> optionNames[binancePublic]
}


BinanceAveragePrice[symbol_String, opts: OptionsPattern[binancePublic]] := 
binancePublic[{"avgPrice", "symbol" -> symbol}, opts]


Options[BinanceTicker24hr] = {
	"type" -> Automatic
}


SyntaxInformation[BinanceTicker24hr] = {
	"ArgumentsPattern" -> {_., OptionsPattern[]}, 
	"OptionNames" -> optionNames[binancePublic, BinanceTicker24hr]
}


BinanceTicker24hr[opts: OptionsPattern[{binancePublic, BinanceTicker24hr}]] := 
binancePublic[{"ticker", "24hr", opts}, opts]


BinanceTicker24hr[symbol_String, opts: OptionsPattern[{binancePublic, BinanceTicker24hr}]] := 
binancePublic[{"ticker", "24hr", "symbol" -> symbol, opts}, opts]


BinanceTicker24hr[symbols: {__String}, opts: OptionsPattern[{binancePublic, BinanceTicker24hr}]] := 
binancePublic[{"ticker", "24hr", "symbols" -> symbols, opts}, opts]


SyntaxInformation[BinanceTickerPrice] = {
	"ArgumentsPattern" -> {_., OptionsPattern[]}, 
	"OptionNames" -> optionNames[binancePublic]
}


BinanceTickerPrice[opts: OptionsPattern[binancePublic]] := 
binancePublic[{"ticker", "price"}, opts]


BinanceTickerPrice[symbol_String, opts: OptionsPattern[binancePublic]] := 
binancePublic[{"ticker", "price", "symbol" -> symbol}, opts]


BinanceTickerPrice[symbols: {__String}, opts: OptionsPattern[binancePublic]] := 
binancePublic[{"ticker", "price", "symbols" -> symbols}, opts]


SyntaxInformation[BinanceOrderBookTicker] = {
	"ArgumentsPattern" -> {_., OptionsPattern[]}, 
	"OptionNames" -> optionNames[binancePublic]
}


BinanceOrderBookTicker[opts: OptionsPattern[binancePublic]] := 
binancePublic[{"ticker", "bookTicker"}, opts]


BinanceOrderBookTicker[symbol_String, opts: OptionsPattern[binancePublic]] := 
binancePublic[{"ticker", "bookTicker", "symbol" -> symbol}, opts]


BinanceOrderBookTicker[symbols: {__String}, opts: OptionsPattern[binancePublic]] := 
binancePublic[{"ticker", "bookTicker", "symbols" -> symbols}, opts]


Options[BinanceTicker] = {
	"windowSize" -> Automatic, 
	"type" -> Automatic
}


SyntaxInformation[BinanceTicker] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> optionNames[binancePublic, BinanceTicker]
}


BinanceTicker[symbol_String, opts: OptionsPattern[{binancePublic, BinanceTicker}]] := 
binancePublic[{"ticker", "symbol" -> symbol, opts}, opts]


BinanceTicker[symbols: {__String}, opts: OptionsPattern[{binancePublic, BinanceTicker}]] := 
binancePublic[{"ticker", "symbols" -> symbols, opts}, opts]


(* ::Section::Bold:: *)
(*Binance websocket market streams implementation *)


Options[BinanceStreamCreate] = {
	"Settings" :> $ExchangeLinkSettings, 
	"Deserializer" :> binanceDeserialize, 
	"EventHandler" :> Print, 
	"Serializer" :> binanceSerialize
}


SyntaxInformation[BinanceStreamCreate] = {
	"ArgumentsPattern" -> {_, _., OptionsPattern[]}, 
	"OptionNames" -> optionNames[BinanceStreamSubscribe]
}


BinanceStreamCreate[stream_String, OptionsPattern[]] := 
Module[{address, settings, webSocketEndpoint, deserializer, eventHandler, connection}, 
	eventHandler = OptionValue["EventHandler"]; 
	settings = OptionValue["Settings"]; 
	webSocketEndpoint = settings["Binance", "WebSocketEndpoint"]; 
	address = webSocketEndpoint <> "/ws/" <> stream; 
	
	WebSocketConnect[address, "Deserializer" -> deserializer, "EventHandler" -> eventHandler]
]


BinanceStreamCreate[streams: {__String}, OptionsPattern[]] := 
Module[{address, settings, webSocketEndpoint, deserializer, eventHandler, connection}, 
	eventHandler = OptionValue["EventHandler"]; 
	settings = OptionValue["Settings"]; 
	webSocketEndpoint = settings["Binance", "WebSocketEndpoint"]; 
	address = webSocketEndpoint <> "/stream?streams=" <> StringRiffle[streams, "/"]; 
	
	WebSocketConnect[address, "Deserializer" -> deserializer, "EventHandler" -> eventHandler]	
]


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


BinanceUnsubscribe[connection_WebSocketConnectionObject, streams: {__String}, opts: OptionsPattern[]] := 
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


BinanceUnsubscribe[connection_WebSocketConnectionObject, stream_String, opts: OptionsPattern[]] := 
BinanceUnsubscribe[connection, {stream}, opts]


BinanceAggTradesStream


(* ::Chapter:: *)
(*Kraken internal functions*)


(* ::Section::Closed:: *)
(*Kraken encoder*)


krakenEncode[assoc_Association] := 
KeyValueMap[krakenEncode] @ DeleteCases[assoc, Automatic | Null]


krakenEncode[key_String, value_] := 
key -> krakenEncode[value]


krakenEncode[list: {__?AtomQ}] := 
StringRiffle[list, ","]


krakenEncode[date_DateObject] := 
UnixTime[date]


krakenEncode[expr_] := 
expr


(* ::Section::Closed:: *)
(*Kraken serializer*)


krakenSerialize[assoc_Association] := 
URLQueryEncode[DeleteCases[assoc, Automatic | Null]]


krakenSerializeToJSON[expr_] := 
ExportString[expr, "RawJSON"]


(* ::Section::Closed:: *)
(*Kraken JSON to expression*)


krakenToExpr[unixtime_Integer] /; 10^9 < unixtime < 2 * 10^9 := 
FromUnixTime[unixtime]


krakenToExpr[number_String] /; StringMatchQ[number, NumberString] := 
ToExpression[number]


krakenToExpr[list_List] := 
Map[krakenToExpr, list]


krakenToExpr[assoc_Association] := 
<|KeyValueMap[krakenToExpr, assoc]|>


krakenToExpr[expr_?AtomQ] := 
expr


krakenToExpr[key_String, value_] := 
key -> krakenToExpr[value]


(* ::Section::Closed:: *)
(*Kraken deserializer*)


krakenDeserialize[body_String] := 
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


Options[krakenPublic] = {
	"Settings" :> $ExchangeLinkSettings, 
	"HTTPMethod" :> "GET", 
	"V" :> "0", 
	"API" :> "public", 
	"Encoder" :> krakenEncode, 
	"Deserializer" :> krakenDeserialize
}


krakenPublic[{path__String, args: <|Rule[_String, _]...|>}, OptionsPattern[]] := 
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


krakenPublic[{method__String, args: OptionsPattern[{}]}, opts: OptionsPattern[{}]] := 
krakenPublic[
	{
		method, 
		<|FilterRules[Flatten[{args}], Except[Options[krakenPublic]]]|>
	}, 
	FilterRules[Flatten[{opts}], Options[krakenPublic]]
]


(* ::Section::Closed:: *)
(*Kraken signed request*)


Options[krakenSigned] = {
	"Settings" :> $ExchangeLinkSettings, 
	"HTTPMethod" :> "POST", 
	"V" :> "0", 
	"API" :> "private", 
	"Serializer" :> krakenSerialize, 
	"Deserializer" :> krakenDeserialize
}


krakenSigned[{path__String, args_Association}, OptionsPattern[]] := 
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


krakenSigned[{method__String, args: OptionsPattern[{}]}, opts: OptionsPattern[{}]] := 
krakenSigned[
	{
		method, 
		<|FilterRules[Flatten[{args}], Except[Options[krakenSigned]]]|>
	}, 
	FilterRules[Flatten[{opts}], Options[krakenSigned]]
]


(* ::Chapter:: *)
(*Kraken*)


(* ::Section::Bold::Closed:: *)
(*Kraken market data implementation*)


SyntaxInformation[KrakenTime] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenPublic]
}


KrakenTime[opts: OptionsPattern[krakenPublic]] := 
krakenPublic[{"Time"}, opts]


SyntaxInformation[KrakenSystemStatus] = {
	"ArgumentsPattern" -> {OptionsPattern[krakenPublic]}, 
	"OptionNames" -> optionNames[krakenPublic]
}


KrakenSystemStatus[opts: OptionsPattern[krakenPublic]] := 
krakenPublic[{"SystemStatus"}, opts]


Options[KrakenAssets] = {
	"aclass" -> Automatic
}


SyntaxInformation[KrakenAssets] = {
	"ArgumentsPattern" -> {_., OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenPublic, KrakenAssets]
}


KrakenAssets[assets: _String | {__String}: Automatic, opts: OptionsPattern[{krakenPublic, KrakenAssets}]] := 
krakenPublic[{"Assets", "asset" -> assets, opts}, opts]


$KrakenAssets := 
cache[KrakenAssets[], 60 * 60]


Options[KrakenAssetPairs] = {
	"info" -> Automatic
}


SyntaxInformation[KrakenAssetPairs] = {
	"ArgumentsPattern" -> {_., OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenPublic, KrakenAssetPairs]
}


KrakenAssetPairs[pair: _String | {__String}: Automatic, opts: OptionsPattern[{krakenPublic, KrakenAssetPairs}]] := 
krakenPublic[{"AssetPairs", "pair" -> pair, opts}, opts]


$KrakenAssetPairs := 
cache[KrakenAssetPairs[], 60 * 60]


SyntaxInformation[KrakenTicker] = {
	"ArgumentsPattern" -> {_., OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenPublic]
}


KrakenTicker[pair: _String | {__String}: Automatic, opts: OptionsPattern[krakenPublic]] := 
krakenPublic[{"Ticker", "pair" -> pair}, opts]


Options[KrakenOHLC] = {
	"interval" -> Automatic, 
	"since" -> Automatic
}


SyntaxInformation[KrakenOHLC] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenPublic, KrakenOHLC]
}


KrakenOHLC[pair_String, opts: OptionsPattern[{krakenPublic, KrakenOHLC}]] := 
krakenPublic[{"OHLC", "pair" -> pair, opts}, opts]


Options[KrakenOrderBook] = {
	"count" -> Automatic
}


SyntaxInformation[KrakenOrderBook] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenPublic, KrakenOrderBook]
}


KrakenOrderBook[pair_String, opts: OptionsPattern[{krakenPublic, KrakenOrderBook}]] := 
krakenPublic[{"Depth", "pair" -> pair, opts}, opts]


Options[KrakenTrades] = {
	"since" -> Automatic
}


SyntaxInformation[KrakenTrades] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenPublic, KrakenTrades]
}


KrakenTrades[pair_String, opts: OptionsPattern[{krakenPublic, KrakenTrades}]] := 
krakenPublic[{"Trades", "pair" -> pair, opts}, opts]


Options[KrakenSpread] = {
	"since" -> Automatic
}


SyntaxInformation[KrakenSpread] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenPublic, KrakenSpread]
}


KrakenSpread[pair_String, opts: OptionsPattern[{krakenPublic, KrakenSpread}]] := 
krakenPublic[{"Spread", "pair" -> pair, opts}, opts]


(* ::Section::Bold::Closed:: *)
(*Kraken user trading api implementation*)


addCodeCompletion[KrakenAddOrder][
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
	"OptionNames" -> optionNames[krakenPrivate, KrakenAddOrder]
}


KrakenAddOrder[ordertype_String, type_String, volume_?NumericQ, pair_String, 
	opts: OptionsPattern[{krakenSigned, KrakenAddOrder}]] := 
krakenSigned[{"AddOrder", "ordertype" -> ordertype, "type" -> type, "volume" -> volume, "pair" -> pair, opts}, opts]


Options[KrakenAddOrderBatch] = {
	"deadline" -> Automatic, 
	"validate" -> Automatic
}


SyntaxInformation[KrakenOpenOrders] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned, KrakenAddOrderBatch]
}


KrakenAddOrderBatch[pair_, orders_, opts: OptionsPattern[{krakenSigned, KrakenAddOrderBatch}]] := 
krakenSigned[{"AddOrderBatch", "orders" -> orders, "pair" -> pair, opts}, opts, "Serializer" -> krakenSerializeToJSON]


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
	"OptionNames" -> optionNames[krakenSigned, KrakenEditOrder]
}


KrakenEditOrder[txid: _String | _Integer, pair_String, opts: OptionsPattern[{krakenSigned, KrakenEditOrder}]] := 
krakenSigned[{"EditOrder", "txid" -> txid, "pair" -> pair, opts}, opts]


SyntaxInformation[KrakenCancelOrder] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned]
}


KrakenCancelOrder[txid: _String | _Integer, opts: OptionsPattern[krakenSigned]] := 
krakenSigned[{"CancelOrder", "txid" -> txid}, opts]


SyntaxInformation[KrakenCancelAll] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned]
}


KrakenCancelAll[opts: OptionsPattern[krakenSigned]] := 
krakenSigned[{"CancelAll"}, opts]


SyntaxInformation[KrakenCancelAllOrdersAfter] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned]
}


KrakenCancelAllOrdersAfter[timeout_, opts: OptionsPattern[{krakenSigned}]] := 
krakenSigned[{"CancelAllOrdersAfter", "timeout" -> timeout},  opts]


SyntaxInformation[KrakenCancelAllOrdersBatch] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned]
}


KrakenCancelAllOrdersBatch[orders_List, opts: OptionsPattern[krakenSigned]] := 
krakenSigned[{"CancelAllOrdersBatch", "orders" -> orders}, opts, "Serializer" -> krakenSerializeToJSON]


(* ::Chapter:: *)
(*Coinbase*)


(* ::Chapter:: *)
(*End*)


(* ::Section::Closed:: *)
(*End private*)


End[] (* End Private Context *)


(* ::Section::Closed:: *)
(*End package*)


EndPackage[] (*ExchangeLink`*)
