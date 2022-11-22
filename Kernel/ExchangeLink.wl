(* ::Package:: *)

(* ::Title:: *)
(*ExchangeLink*)


(* ::Chapter:: *)
(*Begin*)


(* ::Section::Closed:: *)
(*Begin package*)


BeginPackage["KirillBelov`ExchangeLink`", {"KirillBelov`WebSocketJLink`"}]


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


(* ::Section::Italic::Closed:: *)
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


(* ::Section::Italic::Closed:: *)
(*Binance wallet endpoints*)


BinanceSystemStatus::usage = 
"BinanceSystemStatus[]"


BinanceMyCapital::usage = 
"BinanceMyCapital[]"


BinanceAccountSnapshot::usage = 
"BinanceAccountSnapshot[type]"


BinanceMyAsset::usage = 
"BinanceMyAsset[]
BinanceMyAsset[ASSET]"


(* ::Section::Italic::Closed:: *)
(*Binance spot trade endpoints*)


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


(* ::Section::Italic::Closed:: *)
(*Binance websocket user data streams*)


BinanceUserDataStreamCreateKey::usage = 
"BinanceUserDataStreamCreateKey[]"


BinanceUserDataStreamPing::usage = 
"BinanceUserDataStreamPing[listenKey]"


BinanceUserDataStreamClose::usage = 
"BinanceUserDataStreamClose[listenKey]"


BinanceUserDataStream::usage = 
"BinanceUserDataStream[]"


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
(*Kraken websocket rest api endpoints*)


KrakenGetWebSocketToken::usage = 
"KrakenGetWebSocketToken[]"


(* ::Section::Italic::Closed:: *)
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


(* ::Chapter:: *)
(*Coinbase declaration*)


(* ::Section::Closed:: *)
(*Coinbase websocket market streams*)


CoinbaseChannelCreate::usage = 
"CoinbaseChannelCreate[]"


CoinbaseChannelSubscribe::usage = 
"CoinbaseChannelSubscribe[connection, channel]
CoinbaseChannelSubscribe[connection, {channels}]
CoinbaseChannelSubscribe[connection, channels, producsId]
CoinbaseChannelSubscribe[connection, channels, {producsIds}]"


CoinbaseChannelUnsubscribe::usage = 
"CoinbaseChannelUnsubscribe[]"


(* ::Chapter:: *)
(*Private*)


(* ::Section::Closed:: *)
(*Private*)


Begin["`Private`"]


(* ::Section::Closed:: *)
(*Clear Private*)


ClearAll["`*"]


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
(*Binance*)


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
(*Binance symbols*)


binanceSymbols[] := 
cache[$BinanceExchangeInfo[["symbols", All, "symbol"]], 60 * 60 * 24]


(* ::Section::Closed:: *)
(*Binance symbol check*)


binanceSymbolQ[symbol_String] := 
MemberQ[binanceSymbols[], symbol]


(* ::Section::Closed:: *)
(*Binance price and quantity converters*)


binancePriceTickSize[symbol_String] := 
cache[Query[
	"symbols", SelectFirst[#symbol==symbol&], 
	"filters", SelectFirst[#filterType=="PRICE_FILTER"&], 
	"tickSize"] @ $BinanceExchangeInfo, 60 * 60 * 24]


binanceEncodePrice[symbol_String][price_?NumericQ] := 
Module[{tickSize = binancePriceTickSize[symbol], pointPosition}, 
	pointPosition = Round[Log10[tickSize]]; 
	Which[
		pointPosition < 0, ToString[DecimalForm[Round[price, tickSize], {Infinity, -pointPosition}]], 
		pointPosition >= 0, ToString[DecimalForm[Round[price, tickSize]]]
	]
]


binanceQtyStepSize[symbol_String] := 
binanceQtyStepSize[symbol] = 
Query[
	"symbols", SelectFirst[#symbol==symbol&], 
	"filters", SelectFirst[#filterType=="LOT_SIZE"&], 
	"stepSize"] @ $BinanceExchangeInfo


binanceEncodeQty[symbol_String][qty_?NumericQ] := 
Module[{stepSize = binanceQtyStepSize[symbol], pointPosition}, 
	pointPosition = Round[Log10[stepSize]]; 
	Which[
		pointPosition < 0, ToString[DecimalForm[Round[qty, stepSize], {Infinity, -pointPosition}]], 
		pointPosition >= 0, ToString[DecimalForm[Round[qty, Round[stepSize]]]]
	]
]


(* ::Section::Closed:: *)
(*Binance encoder*)


binanceEncode[assoc_Association] := 
KeyValueMap[binanceEncode] @ DeleteCases[assoc, Automatic | Null]


binanceEncode[date_DateObject] := 
timestamp[date]


binanceEncode[rules: {___Rule}] := 
binanceEncode @ <|rules|>


binanceEncode[Rule[key_String, value_]] := 
key -> binanceEncode[value]


binanceEncode[list: {Except[_Rule]..}] := 
StringDelete[ExportString[list, "RawJSON"], WhitespaceCharacter]


binanceEncode[number_Real] /; Round[number] == number := 
ToString[DecimalForm[number]] <> "0"


binanceEncode[number_Real] := 
ToString[DecimalForm[number]]


binanceEncode[expr_] := 
ToString[expr]


binanceEncode[key_String, value_] := 
key -> binanceEncode[value]


binanceEncodeOrder[symbol_String][assoc_Association] := 
KeyValueMap[binanceEncodeOrder[symbol]] @ DeleteCases[assoc, Automatic | Null]


binanceEncodeOrder[symbol_String]["price", number_?NumericQ] := 
"price" -> binanceEncodePrice[symbol][number]


binanceEncodeOrder[symbol_String]["quantity", number_?NumericQ] := 
"quantity" -> binanceEncodeQty[symbol][number]


binanceEncodeOrder[symbol_String][expr__] := 
binanceEncode[expr]


(* ::Section::Closed:: *)
(*Binance serializer*)


binanceSerialize[assoc_Association] := 
ExportString[assoc, "RawJSON"]


(* ::Section::Closed:: *)
(*Binance deserialzer*)


binanceDeserialize[body_String] := 
binanceToExpr @ ImportString[body, "RawJSON"]


binanceTextToJSONDeserialize[body_String] := 
binanceToExpr @ ImportString[ExportString[body, "Text"], "RawJSON"]


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


(* ::Section::Closed:: *)
(*Binance signed request*)


Options[binanceSigned] = {
    "HTTPMethod" :> "POST", 
    "V" :> "v3", 
    "API" :> "api", 
    "recvWindow" :> 5000, 
    "timestamp" :> timestamp[], 
    "signature" :> Automatic, 
    "Settings" :> $ExchangeLinkSettings, 
    "Encoder" :> binanceEncode, 
    "Serializer" :> URLQueryEncode, 
    "Deserializer" :> binanceDeserialize
}


binanceSigned[{method__String, args_?AssociationQ}, OptionsPattern[]] := 
Module[{httpMethod, settings, endpoint, url, request, response, body, result, serializer, 
	encoder, deserializer, parameters, v, api, recvWindow, apiKey, secretKey, timestamp, signature, requestBody}, 
	
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


binanceSigned[{method__String, args: OptionsPattern[{}]}, opts: OptionsPattern[{}]] := 
binanceSigned[
	{
		method, 
		<|FilterRules[Flatten[{args}], Except[Options[binanceSigned]]]|>
	}, 
	FilterRules[Flatten[{opts}], Options[binanceSigned]]
]


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


Options[BinanceDepth] = {
	"limit" -> Automatic
}


SyntaxInformation[BinanceDepth] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> optionNames[binancePublic, BinanceDepth]
}


BinanceDepth[symbol_String, opts: OptionsPattern[{binancePublic, BinanceDepth}]] := 
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


SyntaxInformation[BinanceBookTicker] = {
	"ArgumentsPattern" -> {_., OptionsPattern[]}, 
	"OptionNames" -> optionNames[binancePublic]
}


BinanceBookTicker[opts: OptionsPattern[binancePublic]] := 
binancePublic[{"ticker", "bookTicker"}, opts]


BinanceBookTicker[symbol_String, opts: OptionsPattern[binancePublic]] := 
binancePublic[{"ticker", "bookTicker", "symbol" -> symbol}, opts]


BinanceBookTicker[symbols: {__String}, opts: OptionsPattern[binancePublic]] := 
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


(* ::Section::Bold::Closed:: *)
(*Binance websocket market streams implementation *)


Options[BinanceStreamCreate] = {
	"Settings" :> $ExchangeLinkSettings, 
	"Deserializer" :> binanceDeserialize, 
	"EventHandler" :> Print
}


SyntaxInformation[BinanceStreamCreate] = {
	"ArgumentsPattern" -> {_, _., OptionsPattern[]}, 
	"OptionNames" -> optionNames[BinanceStreamCreate]
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
	"Serializer" -> binanceSerialize
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


(* ::Section::Bold::Closed:: *)
(*Binance wallet implementation*)


SyntaxInformation[BinanceSystemStatus] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> optionNames[binancePublic]
}


BinanceSystemStatus[opts: OptionsPattern[binancePublic]] := 
binancePublic[{"system", "status"}, opts, "V" -> "v1", "API" -> "sapi"]


SyntaxInformation[BinanceMyCapital] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> optionNames[binanceSigned]
}


BinanceMyCapital[opts: OptionsPattern[binanceSigned]] := 
binanceSigned[{"capital", "config", "getall"}, opts, "API" -> "sapi", "V" -> "v1", "HTTPMethod" -> "GET", 
	"Deserializer" -> binanceTextToJSONDeserialize]


Options[BinanceAccountSnapshot] = {
	"startTime" -> Automatic, 
	"endTime" -> Automatic, 
	"limit" -> Automatic
}


SyntaxInformation[BinanceAccountSnapshot] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> optionNames[binanceSigned, BinanceAccountSnapshot]
}


BinanceAccountSnapshot[type_String, opts: OptionsPattern[{BinanceAccountSnapshot, binanceSigned}]] := 
binanceSigned[{"accountSnapshot", "type" -> type, opts}, 
	opts, "API" -> "sapi", "V" -> "v1", "HTTPMethod" -> "GET"]


Options[BinanceMyAsset] = {
	"needBtcValuation" -> Automatic
}


SyntaxInformation[BinanceMyAsset] = {
	"ArgumentsPattern" -> {_., OptionsPattern[]}, 
	"OptionNames" -> optionNames[binanceSigned, BinanceMyAsset]
}


BinanceMyAsset[asset_String: Automatic, opts: OptionsPattern[{binanceSigned, BinanceMyAsset}]] := 
binanceSigned[{"asset", "getUserAsset", "asset" -> asset, opts}, opts, "API" -> "sapi"]


(* ::Section::Bold::Closed:: *)
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
	"OptionNames" -> optionNames[binanceSigned, BinanceOrderTest]
}


BinanceOrderTest[symbol_String?binanceSymbolQ, side_String, orderType_String, 
	opts: OptionsPattern[{binanceSigned, BinanceOrderTest}]] := 
binanceSigned[{"order", "test", "symbol" -> symbol, "side" -> side, "type" -> orderType, opts}, opts, 
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
	"OptionNames" -> optionNames[binanceSigned, BinanceOrderCreate]
}


BinanceOrderCreate[symbol_String?binanceSymbolQ, side_String, orderType_String, 
	opts: OptionsPattern[{binanceSigned, BinanceOrderCreate}]] := 
binanceSigned[{"order", "symbol" -> symbol, "side" -> side, "type" -> orderType, opts}, opts]


SyntaxInformation[BinanceBuy] = {
	"ArgumentsPattern" -> {_, _, _., OptionsPattern[]}, 
	"OptionNames" -> optionNames[binanceSigned, BinanceOrder]
}


BinanceBuy[symbol_String, quantity_?NumericQ, price_?NumericQ, 
	opts: OptionsPattern[{binanceSigned, BinanceOrderCreate}]] := 
BinanceOrderCreate[symbol, "BUY", "LIMIT", quantity, opts]


BinanceBuy[symbol_String, quantity_?NumericQ, opts: OptionsPattern[{binanceSigned, BinanceOrderCreate}]] := 
BinanceOrderCreate[symbol, "BUY", "MARKET", quantity, opts, "timeInForce" -> "GTC"]


SyntaxInformation[BinanceSell] = {
	"ArgumentsPattern" -> {_, _, _., OptionsPattern[]}, 
	"OptionNames" -> optionNames[binanceSigned, BinanceOrderCreate]
}


BinanceSell[symbol_String, quantity_?NumericQ, price_?NumericQ, 
	opts: OptionsPattern[{binanceSigned, BinanceOrderCreate}]] := 
BinanceOrderCreate[symbol, "SELL", "LIMIT", quantity, opts]


BinanceSell[symbol_String, quantity_?NumericQ, opts: OptionsPattern[{binanceSigned, BinanceOrderCreate}]] := 
BinanceOrderCreate[symbol, "SELL", "MARKET", quantity, opts, "timeInForce" -> "GTC"]


Options[BinanceOrderCancel] = {
	"newClientOrderId" -> Automatic
}


SyntaxInformation[BinanceOrderCancel] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[]}, 
	"OptionNames" -> optionNames[binanceSigned, BinanceOrderCancel]
}


BinanceOrderCancel[symbol_String, orderID_Integer, opts: OptionsPattern[{binanceSigned, BinanceOrderCancel}]] := 
binanceSigned[{"order", "symbol" -> symbol, "orderId" -> orderID, opts}, opts, "HTTPMethod" -> "DELETE"]


BinanceOrderCancel[symbol_String, origClientOrderID_String, 
	opts: OptionsPattern[{binanceSigned, BinanceOrderCancel}]] := 
binanceSigned[{"order", "symbol" -> symbol, "origClientOrderId" -> origClientOrderID, opts}, 
	opts, "HTTPMethod" -> "DELETE"]


SyntaxInformation[BinanceOrderCancelAll] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> optionNames[binanceSigned]
}


BinanceOrderCancelAll[symbol_String, opts: OptionsPattern[binanceSigned]] := 
binanceSigned[{"openOrders", "symbol" -> symbol}, opts, "HTTPMethod" -> "DELETE"]


SyntaxInformation[BinanceOrderGet] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[]}, 
	"OptionNames" -> optionNames[binanceSigned]
}


BinanceOrderGet[symbol_String, orderID_Integer, opts: OptionsPattern[binanceSigned]] := 
binanceSigned[{"order", "symbol" -> symbol, "orderId" -> orderID, opts}, opts, "HTTPMethod" -> "GET"]


BinanceOrderGet[symbol_String, origClientOrderID_String, opts: OptionsPattern[binanceSigned]] := 
binanceSigned[{"order", "symbol" -> symbol, "origClientOrderId" -> origClientOrderID}, opts, "HTTPMethod" -> "GET"]


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
	"OptionNames" -> optionNames[binanceSigned, BinanceOrderCancelReplace]
}


BinanceOrderCancelReplace[symbol_String, side_String, type_String, cancelReplaceMode_String, 
	cancelOrigClientOrderID_String, opts: OptionsPattern[{binanceSigned, BinanceOrderCancelReplace}]] := 
binanceSigned[{"order", "cancelReplace", "symbol" -> symbol, "side" -> side, "type" -> type, 
	"cancelReplaceMode" -> cancelReplaceMode, "cancelOrigClientOrderId" -> cancelOrigClientOrderID, opts}, opts]


BinanceOrderCancelReplace[symbol_String, side_String, type_String, cancelReplaceMode_String, 
	cancelOrderID_Integer, opts: OptionsPattern[{binanceSigned, BinanceOrderCancelReplace}]] := 
binanceSigned[{"order", "cancelReplace", "symbol" -> symbol, "side" -> side, "type" -> type, 
	"cancelReplaceMode" -> cancelReplaceMode, "cancelOrderId" -> cancelOrderID, opts}, opts]


SyntaxInformation[BinanceOrderGetOpened] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> optionNames[binanceSigned]
}


BinanceOrderGetOpened[symbol_String, opts: OptionsPattern[binanceSigned]] := 
binanceSigned[{"openOrders", "symbol" -> symbol}, opts, "HTTPMethod" -> "GET"]


Options[BinanceOrderGetAll] = {
	"orderId" -> Automatic, 
	"startTime" -> Automatic, 
	"endTime" -> Automatic, 
	"limit" -> Automatic
}


SyntaxInformation[BinanceOrderGetAll] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> optionNames[binanceSigned, BinanceOrderGetAll]
}


BinanceOrderGetAll[symbol_String, opts: OptionsPattern[{binanceSigned, BinanceOrderGetAll}]] := 
binanceSigned[{"openOrders", "symbol" -> symbol, opts}, opts, "HTTPMethod" -> "GET"]


Options[BinanceMyTrades] = {
	"orderId" -> Automatic, 
	"startTime" -> Automatic, 
	"endTime" -> Automatic, 
	"fromId" -> Automatic, 
	"limit" -> Automatic
}


SyntaxInformation[BinanceMyTrades] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> optionNames[binanceSigned, BinanceMyTrades]
}


BinanceMyTrades[symbol_String, opts: OptionsPattern[{binanceSigned, BinanceMyTrades}]] := 
binanceSigned[{"myTrades", "symbol" -> symbol, opts}, opts, "HTTPMethod" -> "GET"]


SyntaxInformation[BinanceOrderCount] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> optionNames[binanceSigned]
}


BinanceOrderCount[opts: OptionsPattern[binanceSigned]] := 
binanceSigned[{"rateLimit", "order"}, opts, "HTTPMethod" -> "GET"]


(* ::Section::Bold::Closed:: *)
(*Binance websocket user data rest methods implementation*)


SyntaxInformation[BinanceUserDataStreamCreateKey] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNanes" -> optionNames[binanceSigned]
}


BinanceUserDataStreamCreateKey[opts: OptionsPattern[binanceSigned]] := 
binanceSigned[{"userDataStream"}, opts, "timestamp" -> Null, "recvWindow" -> Null, "signature" -> Null]


SyntaxInformation[BinanceUserDataStreamPing] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNanes" -> optionNames[binanceSigned]
}


BinanceUserDataStreamPing[listenKey_String, opts: OptionsPattern[binanceSigned]] := 
binanceSigned[{"userDataStream", "listenKey" -> listenKey}, opts, 
	"HTTPMethod" -> "PUT", "timestamp" -> Null, "recvWindow" -> Null, "signature" -> Null]


SyntaxInformation[BinanceUserDataStreamClose] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNanes" -> optionNames[binanceSigned]
}


BinanceUserDataStreamClose[listenKey_String, opts: OptionsPattern[binanceSigned]] := 
binanceSigned[{"userDataStream", "listenKey" -> listenKey}, opts, 
	"HTTPMethod" -> "DELETE", "timestamp" -> Null, "recvWindow" -> Null, "signature" -> Null]


SyntaxInformation[BinanceUserDataStream] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNanes" -> optionNames[binanceSigned, BinanceStreamCreate]
}


BinanceUserDataStream[opts: OptionsPattern[{binanceSigned, BinanceStreamCreate}]] := 
Module[{listenKey}, 
	listenKey = BinanceUserDataStreamCreateKey[filter[opts]]["listenKey"]; 
	<|"listemKey" -> listenKey, "stream" -> BinanceStreamCreate[listenKey, filter[opts]]|>
]


(* ::Chapter:: *)
(*Kraken*)


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
	"OptionNames" -> optionNames[krakenSigned, KrakenAddOrder]
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


(* ::Section::Bold::Closed:: *)
(*Kraken user data implementation*)


SyntaxInformation[KrakenBalance] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned]
}


KrakenBalance[opts: OptionsPattern[{krakenSigned}]] := 
krakenSigned[{"Balance"}, opts]


SyntaxInformation[KrakenTradeBalance] = {
	"ArgumentsPattern" -> {_., OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned]
}


KrakenTradeBalance[opts: OptionsPattern[{krakenSigned}]] := 
krakenSigned[{"TradeBalance"}, opts]


KrakenTradeBalance[asset_String, opts: OptionsPattern[{krakenSigned}]] := 
krakenSigned[{"TradeBalance", "asset" -> asset}, opts]


Options[KrakenOpenOrders] = {
	"trades" -> Automatic, 
	"userref" -> Automatic
}


SyntaxInformation[KrakenOpenOrders] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned, KrakenOpenOrders]
}


KrakenOpenOrders[opts: OptionsPattern[{krakenSigned, KrakenOpenOrders}]] := 
krakenSigned[{"OpenOrders", opts}, opts]


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
	"OptionNames" -> optionNames[krakenSigned, KrakenClosedOrders]
}


KrakenClosedOrders[opts: OptionsPattern[{krakenSigned, KrakenClosedOrders}]] := 
krakenSigned[{"ClosedOrders", opts}, opts]


Options[KrakenQueryOrders] = {
	"trades" -> Automatic, 
	"userref" -> Automatic
}


SyntaxInformation[KrakenOpenOrders] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned, KrakenQueryOrders]
}


KrakenQueryOrders[txid_String, opts: OptionsPattern[{krakenSigned, KrakenQueryOrders}]] := 
krakenSigned[{"QueryOrders", "txid" -> txid, opts}, opts]


Options[KrakenTradeHistory] = {
	"type" -> Automatic, 
	"trades" -> Automatic, 
	"start" -> Automatic, 
	"end" -> Automatic, 
	"ofs" -> Automatic
}


SyntaxInformation[KrakenTradeHistory] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned, KrakenTradeHistory]
}


KrakenTradeHistory[opts: OptionsPattern[{krakenSigned, KrakenTradeHistory}]] := 
krakenSigned[{"TradeHistory", opts}, opts]


Options[KrakenQueryTrades] = {
	"txid" -> Automatic, 
	"trades" -> Automatic
}


SyntaxInformation[KrakenQueryTrades] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned, KrakenQueryTrades]
}


KrakenQueryTrades[opts: OptionsPattern[{krakenSigned, KrakenQueryTrades}]] := 
krakenSigned[{"QueryTrades", opts}, opts]


Options[KrakenOpenPositions] = {
	"txid" -> Automatic, 
	"docalcs" -> Automatic, 
	"consolidation" -> Automatic
}


SyntaxInformation[KrakenOpenPositions] = {
	"ArgumentsPattern" -> {OptionsPattern[{krakenSigned, KrakenOpenPositions}]}, 
	"OptionNames" -> optionNames[krakenSigned, KrakenOpenPositions]
}


KrakenOpenPositions[opts: OptionsPattern[{krakenSigned, KrakenOpenPositions}]] := 
krakenSigned[{"OpenPositions", opts}, opts]


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
	"OptionNames" -> optionNames[krakenSigned, KrakenLedgers]
}


KrakenLedgers[opts: OptionsPattern[{krakenSigned, KrakenLedgers}]] := 
krakenSigned[{"Ledgers", opts}, opts]


Options[KrakenQueryLedgers] = {
	"id" -> Automatic, 
	"trades" -> Automatic
}


SyntaxInformation[KrakenQueryLedgers] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned, KrakenQueryLedgers]
}


KrakenQueryLedgers[opts: OptionsPattern[{krakenSigned, KrakenQueryLedgers}]] := 
krakenSigned[{"QueryLedgers", opts}, opts]


Options[KrakenTradeVolume] = {
	"pair" -> Automatic, 
	"fee-info" -> Automatic
}


SyntaxInformation[KrakenTradeVolume] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned, KrakenTradeVolume]
}


KrakenTradeVolume[opts: OptionsPattern[{krakenSigned, KrakenTradeVolume}]] := 
krakenSigned[{"TradeVolume", opts}, opts]


Options[KrakenAddExport] = {
	"format" -> Automatic, 
	"fields" -> Automatic, 
	"starttm" -> Automatic, 
	"endtm" -> Automatic
}


SyntaxInformation[KrakenAddExport] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned, KrakenAddExport]
}


KrakenAddExport[report: "trades" | "ledgers", description_String, 
	opts: OptionsPattern[{krakenSigned, KrakenAddExport}]] := 
krakenSigned[{"AddExport", "report" -> report, "description" -> description, opts}, opts]


SyntaxInformation[KrakenExportStatus] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned]
}


KrakenExportStatus[report: "trades" | "ledgers", opts: OptionsPattern[krakenSigned]] := 
krakenSigned[{"ExportStatus", "report" -> report}, opts]


SyntaxInformation[KrakenRetrieveExport] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned]
}


KrakenRetrieveExport[id_String, opts: OptionsPattern[krakenSigned]] := 
krakenSigned[{"RetrieveExport", "id" -> id}, opts]


SyntaxInformation[KrakenOpenOrders] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[{krakenSigned}]}, 
	"OptionNames" -> optionNames[krakenSigned]
}


KrakenRemoveExport[id_String, type: "cancel" | "delete", opts: OptionsPattern[krakenSigned]] := 
krakenSigned[{"RemoveExport", "id" -> id, "type" -> type}, opts]


(* ::Section::Bold::Closed:: *)
(*Kraken user funding api implementation*)


SyntaxInformation[KrakenDepositMethods] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned]
}


KrakenDepositMethods[asset_String, opts: OptionsPattern[]] := 
krakenSigned[{"DepositMethods", "asset" -> asset}, opts]


Options[KrakenDepositAddresses] = {
	"new" -> Automatic
}


SyntaxInformation[KrakenDepositAddresses] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned, KrakenDepositAddresses]
}


KrakenDepositAddresses[asset_String, method_String, 
	opts: OptionsPattern[{krakenSigned, KrakenDepositAddresses}]] := 
krakenSigned[{"DepositAddresses", "asset" -> asset, "method" -> method, opts}, opts]


Options[KrakenDepositStatus] = {
	"method" -> Automatic
}


SyntaxInformation[KrakenDepositStatus] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned, KrakenDepositStatus]
}


KrakenDepositStatus[asset_String, opts: OptionsPattern[{krakenSigned, KrakenDepositStatus}]] := 
krakenSigned[{"DepositStatus", "asset" -> asset, opts}, opts]


SyntaxInformation[KrakenWithdrawInfo] = {
	"ArgumentsPattern" -> {_, _, _, OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned]
}


KrakenWithdrawInfo[asset_String, key_String, amount_, opts: OptionsPattern[krakenSigned]] := 
krakenSigned[{"WithdrawInfo", "asset" -> asset, "key" -> key, "amount" -> amount}, opts]


SyntaxInformation[KrakenWithdraw] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned]
}


KrakenWithdraw[asset_String, key_String, amount_, opts: OptionsPattern[krakenSigned]] := 
krakenSigned[{"Withdraw", "asset" -> asset, "key" -> key, "amount" -> amount}, opts]


Options[KrakenWithdrawStatus] = {
	"method" -> Automatic
}


SyntaxInformation[KrakenWithdrawStatus] = {
	"ArgumentsPattern" -> {_, OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned, KrakenWithdrawStatus]
}


KrakenWithdrawStatus[asset_String, opts: OptionsPattern[{krakenSigned, KrakenWithdrawStatus}]] := 
krakenSigned[{"WithdrawStatus", "asset" -> asset, opts}, opts]


SyntaxInformation[KrakenWithdrawCancel] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned]
}


KrakenWithdrawCancel[asset_String, refid_String, opts: OptionsPattern[krakenSigned]] := 
krakenSigned[{"WithdrawCancel", "asset" -> asset, "refid" -> refid}, opts]


SyntaxInformation[KrakenWalletTransfer] = {
	"ArgumentsPattern" -> {_, _, _, _, OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned]
}


KrakenWalletTransfer[asset_, from_, to_, amount_, opts: OptionsPattern[krakenSigned]] := 
krakenSigned[{"WalletTransfer", "asset" -> asset, "from" -> from, "to" -> to, "amount" -> amount}, opts]


(* ::Section::Bold::Closed:: *)
(*Kraken user staking api implementation*)


SyntaxInformation[KrakenStake] = {
	"ArgumentsPattern" -> {_, _, _, OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned]
}


KrakenStake[asset_, amount_, method_, opts: OptionsPattern[krakenSigned]] := 
krakenSigned[{"Stake", "asset" -> asset, "amount" -> amount, "method" -> method}, opts]


SyntaxInformation[KrakenUnstake] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned]
}


KrakenUnstake[asset_, amount_, opts: OptionsPattern[krakenSigned]] := 
krakenSigned[{"Unstake", "asset" -> asset, "amount" -> amount}, opts]


SyntaxInformation[KrakenStakingAssets] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned]
}


KrakenStakingAssets[opts: OptionsPattern[krakenSigned]] := 
krakenSigned[{"StakingAssets"}, opts]


SyntaxInformation[KrakenStakingPending] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned]
}


KrakenStakingPending[opts: OptionsPattern[krakenSigned]] := 
krakenSigned[{"StakingPending"}, opts]


SyntaxInformation[KrakenStakingTransactions] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned]
}


KrakenStakingTransactions[opts: OptionsPattern[krakenSigned]] := 
krakenSigned[{"StakingTransactions"}, opts]


(* ::Section::Bold::Closed:: *)
(*Kraken websocket rest api method*)


SyntaxInformation[KrakenGetWebSocketToken] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> optionNames[krakenSigned]
}


KrakenGetWebSocketToken[opts: OptionsPattern[krakenSigned]] := 
krakenSigned[{"GetWebSocketToken"}, opts]


(* ::Section::Bold::Closed:: *)
(*Kraken websocket channels*)


Options[KrakenChannelCreate] = {
	"Settings" :> $ExchangeLinkSettings, 
	"Deserializer" :> krakenDeserialize, 
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


KrakenOwnTradesChannel[opts: OptionsPattern[{krakenSigned, KrakenChannelCreate, KrakenChannelSubscribe}]] := 
Module[{connection}, 
	connection = KrakenChannelCreate[opts]; 
	token = krakenSigned[{"GetWebSocketToken"}, opts]; 
	KrakenChannelSubscribe[connection, {}, "ownTrades", opts]; 
	Return[connection]
]


KrakenOpenOrdersChannel[opts: OptionsPattern[{krakenSigned, KrakenChannelCreate, KrakenChannelSubscribe}]] := 
Module[{connection}, 
	connection = KrakenChannelCreate[opts]; 
	token = krakenSigned[{"GetWebSocketToken"}, opts]; 
	KrakenChannelSubscribe[connection, {}, "openOrders", opts]; 
	Return[connection]
]


(* ::Chapter:: *)
(*Coinbase*)


(* ::Section::Closed:: *)
(*Coinbase deserializer*)


coinbaseDeserialize[body_String] := 
ImportString[body, "RawJSON"]


(* ::Section::Closed:: *)
(*Coinbase serializer*)


coinbaseSerialize[frame_String] := 
ExportString[frame, "RawJSON"]


(* ::Section::Closed:: *)
(*Coinbase signed request*)


coinbaseSigned[] := {}


(* ::Section::Bold::Closed:: *)
(*Coinbase websocket channels*)


Options[CoinbaseChannelCreate] = {
	"Settings" :> $ExchangeLinkSettings, 
	"Deserializer" :> coinbaseDeserialize, 
	"EventHandler" :> Print
}


SyntaxInformation[CoinbaseChannelCreate] = {
	"ArgumentsPattern" -> {OptionsPattern[]}, 
	"OptionNames" -> optionNames[CoinbaseChannelCreate]
}


CoinbaseChannelCreate[opts: OptionsPattern[{}]] := 
Module[{settings, webSocketEndpoint, deserializer, eventHandler}, 
	settings = OptionValue[CoinbaseChannelCreate, opts, "Settings"]; 
	deserializer = OptionValue[CoinbaseChannelCreate, opts, "Deserializer"]; 
	eventHandler = OptionValue[CoinbaseChannelCreate, opts, "EventHandler"]; 
	webSocketEndpoint = settings["Coinbase", "WebSocketEndpoint"]; 
	WebSocketConnect[webSocketEndpoint, "Deserializer" -> deserializer, "EventHandler" -> eventHandler]
]


Options[CoinbaseChannelSubscribe] = {
	"Serializer" :> coinbaseSerialize
}


CoinbaseChannelSubscribe[connection_WebSocketConnectionObject, productIds_, channels_, opts: OptionsPattern[{}]] := 
Module[{frame, serializer}, 
	serializer = OptionValue[CoinbaseChannelSubscribe, {opts}, "Serializer"]; 
	frame = <|
		"type" -> "subscribe", 
		"product_ids" -> productIds, 
		"channels" -> channels
	|>; 
	WebSocketSend[connection, frame, "Serializer" -> serializer]
]


CoinbaseHeartbeatChannel[productIds: _String | {__String}, 
	opts: OptionsPattern[{CoinbaseChannelCreate, CoinbaseChannelSubscribe}]] := 
Module[{connection}, 
	connection = CoinbaseChannelCreate[opts]; 
	CoinbaseChannelSubscribe[connection, Flatten[{productIds}]]
]


(* ::Chapter:: *)
(*End*)


(* ::Section::Closed:: *)
(*End private*)


End[] (* End Private Context *)


(* ::Section::Closed:: *)
(*End package*)


EndPackage[] (*KirillBelov`ExchangeLink`*)
