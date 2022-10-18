(* ::Package:: *)

(* ::Title:: *)
(*ExchangeLink*)


(* ::Chapter:: *)
(*Begin package*)


(* ::Section::Closed:: *)
(*Begin package*)


BeginPackage["ExchangeLink`", {"WebSocketJLink`"}]


(* ::Chapter:: *)
(*ExchangeLink names*)


(* ::Section::Closed:: *)
(*ExchangeLink names*)


ClearAll["`*"]


$ExchangeLinkSettings::usage = 
"$ExchangeLinkSettings
$ExchangeLinkSettings[\"Biannce\"] - binance settings
$ExchangeLinkSettings[\"Coinbase\"] - coinbase settings"


$ExchangeLinkHistory::usage = 
"$ExchangeLinkHistory"


(* ::Chapter:: *)
(*Binance endpoints*)


(* ::Section::Closed:: *)
(*Binance wallet endpoints*)


BinanceSystemStatus::usage = 
"BinanceSystemStatus[] Fetch system status."


BinanceUserCoins::usage = 
"BinanceUserCoins[] Get information of coins (available for deposit and withdraw) for user."


BinanceAccountSnapshot::usage = 
"BinanceAccountSnapshot[accountType] \
args: {accountType -> SPOT|MARGIN|FUTURES}
BinanceAccountSnapshot[accountType, opts] \
opts: \
{startTime -> DateObject, \
endTime -> DateObject, \
limit -> (min -> 7, max -> 30, default -> 7)}"


BinanceDisableFastWithdrawSwitch::usage = 
"BinanceDisableFastWithdrawSwitch[]"


BinanceEnableFastWithdrawSwitch::usage = 
"BinanceEnableFastWithdrawSwitch[]"


BinanceWithdraw::usage = 
"BinanceWithdraw[COIN, amount, address] Submit a withdraw request.
BinanceWithdraw[COIN, amount, address, opts]"


BinanceDepositHistory::usage = 
"BinanceDepositHistory[]"


BinanceWithdrawHistory::usage = 
"BinanceWithdrawHistory[]"


BinanceDepositAddress::usage = 
"BinanceDepositAddress[COIN]"


BinanceAccountStatus::usage = 
"BinanceAccountStatus[]"


BinanceAccountAPITradingStatus::usage = 
"BinanceAccountAPITradingStatus[]"


BinanceDustLog::usage = 
"BinanceDustLog[]"


BinanceGetDustAssets::usage = 
"BinanceGetDustAssets[]"


BinanceConvertDustAssets::usage = 
"BinanceConvertDustAssets[ASSETS]"


(* ::Section::Closed:: *)
(*Binance market data endpoints*)


BinancePing::usage = 
"BinancePing[]"


BinanceTime::usage = 
"BinanceTime[]"


BinanceExchangeInfo::usage = 
"BinanceExchangeInfo[]
BinanceExchangeInfo[SYMBOL]
BinanceExchangeInfo[{SYMBOLS}]
BinanceExchangeInfo[SYMBOLS, opts]
  options: 
  - permissions: {\"SPOT\",\"MARGIN\",\"LEVERAGED\",\"TRD_GRP_002\",\
\"TRD_GRP_003\",\"TRD_GRP_004\",\"TRD_GRP_005\"}"


$BinanceExchangeInfo::usage = 
"$BinanceExchangeInfo"


BinanceOrderBook::usage = 
"BinanceOrderBook[SYMBOL]
BinanceOrderBook[SYMBOL, opts]
  options: 
  - limit: {default -> 100, max -> 5000}"


BinanceTrades::usage = 
"BinanceTrades[SYMBOL]
BinanceTrades[SYMBOL, opts]
  options: 
  - limit: {default -> 500, max -> 1000}"


BinanceHistoricalTrades::usage = 
"BinanceHistoricalTrades[SYMBOL] - Get older marker trades
BinanceHistoricalTrades[SYMBOL, opts]
  options  : 
  - limit  : {default -> 500, max -> 1000}
  - fromId : trade id"


BinanceAggTrades::usage = 
"BinanceAggTrades[SYMBOL]
BinanceAggTrades[SYMBOL, opts]
  options    : 
  - fromId   : trade id
  - startTime: DateObject
  - endTime  : DateObject
  - limit    : {default -> 500, max -> 1000}"


BinanceKlines::usage = 
"BinanceKlines[SYMBOL, interval]
BinanceKlines[SYMBOL, interval, opts]
  options    : 
  - startTime: DateObject
  - endTime  : DateObject
  - limit    : {default -> 500, max -> 1000}"


BinanceUIKlines::usage = 
"BinanceUIKlines[SYMBOL, interval]
BinanceUIKlines[SYMBOL, interval, opts]
  options    : 
  - startTime: DateObject
  - endTime  : DateObject
  - limit    : {default -> 500, max -> 1000}"


BinanceAveragePrice::usage = 
"BinanceAveragePrice[SYMBOL]"


BinanceTicker24hr::usage = 
"BinanceTicker24hr[]
BinanceTicker24hr[SYMBOL]
BinanceTicker24hr[{SYMBOLS}]
BinanceTicker24hr[SYMBOLS, opts]
  optsions: 
  - type: {default -> FULL, MINI}"


BinanceTickerPrice::usage = 
"BinanceTickerPrice[]
BinanceTickerPrice[SYMBOL]
BinanceTickerPrice[{SYMBOLS}]"


BinanceOrderBookTicker::usage = 
"BinanceOrderBookTicker[SYMBOL]
BinanceOrderBookTicker[{SYMBOLS}]"


BinanceTicker::usage = 
"BinanceTicker[SYMBOL]
BinanceTicker[{SYMBOLS}]
BinanceTicker[SYMBOLS, opts]
  options: 
  - windowSize: {minutes -> 1m, 2m .. 59m, hours -> 1h, 2h .. 23h, days -> 1d .. 7d}
  - type -> {default -> FULL MINI}"


(* ::Section::Closed:: *)
(*Binance spot trade enpoints*)


BinanceBuy::usage = 
"BinanceBuy[SYMBOL, quantity]
BinanceBuy[SYMBOL, quantity, price]"


BinanceSell::usage = 
"BinanceSell[SYMBOL, quantity]
BinanceSell[SYMBOL, quantity, price]"


BinanceTestOrder::usage = 
"BinanceTestOrder[SYMBOL, side, orderType, quantity, price]"


BinanceCreateOrder::usage = 
"BinanceCreateOrder[SYMBOL, side, orderType, quantity, price]"


BinanceGetOrder::usage = 
"BinanceGetOrder[SYMBOL, orderId]
BinanceGetOrder[SYMBOL, origClientOrderId]"


BinanceGetOrders::usage = 
"BinanceGetOrders[]
BinanceGetOrders[SYMBOL]"


BinanceGetAllOrders::usage = 
"BinanceGetAllOrders[SYMBOL]"


BinanceCancelOrder::usage =
"BinanceCancelOrder[SYMBOL, orderId]
BinanceCancelOrder[SYMBOL, origClientOrderId]"


BinanceCancelAllOrders::usage = 
"BinanceCancelAllOrders[SYMBOL]"


BinanceCancelReplaceOrder::usage = 
"BinanceCancelReplaceOrder[cancelOrderId, SYMBOL, side, orderType, quantity, price]"


(* ::Section::Closed:: *)
(*Binance streams*)


BinanceStreamSubscribe::usage = 
"BinanceStreamSubscribe[streams]
BinanceStreamSubscribe[connection, streams]"


BinanceStreamUnsubscribe::usage = 
"BinanceStreamUnsubscribe[conection, streams]"


BinanceStreams::usage = 
"BinanceStreams[]
BinanceStreams[connection]"


BinanceConnections::usage = 
"BinanceConnections[]"


BinanceTickerStream::usage = 
"BinanceTickerStream[SYMBOLS]
BinanceTickerStream[connection, SYMBOLS]"


BinanceMiniTickerStream::usage = 
"BinanceMiniTickerStream[SYMBOLS]
BinanceMiniTickerStream[connection, SYMBOLS]"


BinanceKlinesStream::usage = 
"BinanceKlinesStream[SYMBOLS, interval]"


(* ::Chapter:: *)
(*Kraken endpoints*)


(* ::Section::Closed:: *)
(*Kraken marken data endpoins*)


KrakenTime::usage = 
"KrakenTime[]"


KrakenSystemStatus::usage = 
"KrakenSystemStatus[]"


KrakenAssetInfo::usage = 
"KrakenAssetInfo[ASSETS]"


KrakenAssetPairs::usage = 
"KrakenAssetPairs[PAIR]"


KrakenTicker::usage = 
"KrakenTicker[PAIR]"


KrakenOHLC::usage = 
"KrakenOHLC[PAIR]
KrakenOHLC[PAIR, interval]"


KrakenDepth::usage = 
"KrakenDepth[PAIR]"


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
"KrakenAddOrder[]"


KrakenAddOrderBatch::usage = 
"KrakenAddOrderBatch[]"


KrakenEditOrder::usage = 
"KrakenEditOrder[]"


KrakenCancelOrder::usage = 
"KrakenCancelOrder[]"


KrakenCancelAll::usage = 
"KrakenCancelAll[]"


KrakenCancelAllOrdersAfter::usage = 
"KrakenCancelAllOrdersAfter[]"


KrakenCancelAllOrdersBatch::usage = 
"KrakenCancelAllOrdersBatch[]"


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
(*Kraken web socket rest api endpoints*)


KrakenGetWebSocketToken::usage = 
"KrakenGetWebSocketToken[]"


(* ::Chapter:: *)
(*begin private*)


(* ::Section::Closed:: *)
(*private*)


Begin["`Private`"] (* Begin Private Context *)


(* ::Chapter:: *)
(*internal functions*)


(* ::Section::Closed:: *)
(*settings*)


$directory = 
ParentDirectory[DirectoryName[$InputFileName]]


getSettings[time_DateObject] := getSettings[time] = 
Import[FileNameJoin[{$directory, ".ExchangeLink"}], "Package"]


getSettings[] := 
getSettings[DateObject[Round[AbsoluteTime[], 10]]]


(* ::Section::Closed:: *)
(*cache*)


SetAttributes[cache, HoldRest]


cache[{date_DateObject, period_Integer}, expr_] := 
cache[{date, period}, expr] = 
expr


cache[period_Integer, expr_] := 
cache[{DateObject[Round[AbsoluteTime[Now], period]], period}, expr]


(* ::Section:: *)
(*options*)


func_Symbol[args___, filter[opts: OptionsPattern[{}]]] ^:= 
func[args, options[func, opts]]


filter[func_Symbol, opts: OptionsPattern[{}]] := 
FilterRules[Flatten[{opts}], Options[func]]


optionNames[funcs__Symbol] := 
Map["\"" <> # <> "\""&] @ Keys[Flatten[Map[Options] @ {funcs}]]


autoOptions[names__String] := 
Map[# -> Automatic&] @ {names}


(* ::Section::Closed:: *)
(*hmac*)


HMAC[data_, key_SymmetricKey, hash_String: "SHA256"] := 
HMAC[data, key["Key"], hash];


HMAC[data_, key: {___Integer}, hash_String: "SHA256"] := 
With[{bytes = ByteArray[key]}, HMAC[data, bytes, hash]/; ByteArrayQ@bytes];


HMAC[data_, key_String/; StringQ[Unevaluated[key]], hash_String: "SHA256"] := 
HMAC[data, StringToByteArray[key], hash];


HMAC[expr: Except[_String | _ByteArray], key_ByteArray/; ByteArrayQ[Unevaluated[key]], hash_String: "SHA256"] := 
HMAC[BinarySerialize[expr], key, hash];


HMAC[message_String/; StringQ[Unevaluated[message]], key_ByteArray /; ByteArrayQ[Unevaluated[key]], hash_String: "SHA256"]:=
HMAC[StringToByteArray[message], key, hash];


HMAC[data_ByteArray/; ByteArrayQ[Unevaluated[data]], key_ByteArray /; ByteArrayQ[Unevaluated[key]], hash_String: "SHA256"] := 
Module[{$HashSizes, inkey, invec, key0, blocksize, ipad, opad, ipadkey, opadkey, temp, bhash}, 
	$HashSizes = <|
		"MD2" -> {128, 128}, "MD4" -> {128, 512}, "MD5" -> {128, 512}, 
		"SHA" -> {160, 512}, "SHA1" -> {160, 512}, 
		"SHA224" -> {224, 512}, "SHA256" -> {256, 512}, 
		"SHA384" -> {384, 1024}, "SHA512" -> {512, 1024}, 
		"RIPEMD160" -> {160, 512}, "RIPEMD160SHA256" -> {160, 512}, 
		"SHA256SHA256" -> {256, 512}, "SHA3-224" -> {224, 1152}, "SHA3-256" -> {256, 1088}, "SHA3-384" -> {384, 832}, 
		"SHA3-512" -> {512, 576}, 
		"Keccak224" -> {224, 1152}, "Keccak256" -> {256, 1088}, "Keccak384" -> {384, 832}, "Keccak512" -> {512, 576}
	|>; 
	
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
(*nonce*)


nonce[] := 
ToString[Round[1000 * (UnixTime[] + FractionalPart[AbsoluteTime[]])]]


(* ::Section::Closed:: *)
(*save to history*)


saveToHistory[exchange_, request_, response_] := 
$ExchangeLinkHistory["PushBack", <|
	"Time" -> Now, 
	"Exchange" -> exchange, 
	"Request" -> request, 
	"Response" -> response
|>]; 


(* ::Section::Closed:: *)
(*completion*)


addCodeCompletion[function_String][args___] := 
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
	(FE`Evaluate[FEPrivate`AddSpecialArgCompletion[#1]]&)[function -> processed]
]


optionsCompletionDir = 
FileNameJoin[{$InstallationDirectory, "SystemFiles", "Components", "AutoCompletionData", "Main", "OptionValues"}]


addOptionsCompletion[func_Symbol, args: {__Rule}] := 
Module[{file = FileNameJoin[{optionsCompletionDir, SymbolName[func] <> ".m"}]}, 
	Put[Map[First[#] -> Map[ToString] @ Last[#]&] @ args, file]
]


(* ::Chapter:: *)
(*ExchangeLink implementation*)


(* ::Section::Closed:: *)
(*ExchangeLink public implementation*)


$ExchangeLinkSettings := 
getSettings[]


$ExchangeLinkHistory = 
If[ValueQ[$ExchangeLinkHistory], 
	$ExchangeLinkHistory, 
	CreateDataStructure["RingBuffer", 1000]
]


(* ::Chapter:: *)
(*Binance implementation*)


(* ::Section::Closed:: *)
(*binance time*)


binanceTime[] := 
Round[1000 * (UnixTime[] + FractionalPart[AbsoluteTime[]])]


(* ::Section::Closed:: *)
(*binance serialization*)


binanceQueryStringSerialize[rules_] := 
URLQueryEncode[binanceSerialize[rules]]


binanceSerialize[args: {Rule[_String, _]...}] := 
Map[binanceSerialize, args]


binanceSerialize[Rule[_, Null | Automatic]] := 
Nothing


binanceSerialize[Rule["symbol", symbol_String]] := 
"symbol" -> ToUpperCase[symbol]


binanceSerialize[Rule["symbols", symbols: {__String}]] := 
"symbols" -> StringReplace[ExportString[ToUpperCase /@ symbols, "RawJSON"], WhitespaceCharacter->""]


binanceSerialize[Rule[key_String, value_String]] := 
key -> value


binanceSerialize[Rule[key_String, number_?NumericQ]] := 
key -> number


binanceDeserialize[body_String] := 
ImportString[ExportString[body, "Text"], "RawJSON"]


binanceStreamToURL[streams: {__String}] := 
"/stream?streams=" <> StringRiffle[streams, "/"]


binanceStreamDeserialize[frame_String] := 
ImportString[frame, "RawJSON"]


(* ::Section:: *)
(*binance public request executer*)


Options[binancePublic] = {
    "HTTPMethod" :> "GET", 
    "V" :> "v3", 
    "API" :> "api", 
    "Settings" :> $ExchangeLinkSettings, 
    "Serializer" :> binanceSerialize, 
    "Deserializer" :> binanceDeserialize
}


binancePublic[{method__String, args: {Rule[_String, _]...}}, OptionsPattern[]] := 
Module[{httpMethod, settings, endpoint, url, request, response, body, result, 
	serializer, deserializer, parameters, v, api}, 
	
	v = OptionValue["V"]; 
	api = OptionValue["API"]; 
    httpMethod = OptionValue["HTTPMethod"]; 
	settings = OptionValue["Settings"]; 
    endpoint = settings["Binance", "Endpoint"]; 
    serializer = OptionValue["Serializer"]; 
    deserializer = OptionValue["Deserializer"]; 

	Print[args]; 

    parameters = serializer[args]; 
    url = URLBuild[{endpoint, api, v, method}, parameters]; 
    request = HTTPRequest[url, <|Method -> httpMethod|>]; 
    
    response = URLRead[request]; 
    body = response["Body"]; 
    result = deserializer[body]; 
    
    $ExchangeLinkHistory["PushBack", <|
		"Time" -> Now, 
		"Exchange" -> "Binance", 
		"Request" -> request, 
		"Response" -> response
    |>]; 

    Return[result]
]


(* ::Section::Closed:: *)
(*binance signed request executer*)


Options[binanceSigned] = {
    "HTTPMethod" :> "POST", 
    "V" :> binanceAPIVersion, 
    "API" :> binanceAPIType, 
    "Serializer" :> binanceSerialize, 
    "Deserializer" :> binanceDeserialize, 
    "Settings" :> $ExchangeLinkSettings, 
    "RecvWindow" :> 5000
}


binanceSigned[{method__String, args: Rule[_String, _]...}, OptionsPattern[]] := 
Module[{body, settings, apiKey, secretKey, endpoint, uri, request, response, parameters, 
	httpMethod, recvWindow, timestamp, signature, serializer, api, v, result, requestBody, deserializer, fullParameters},

	api = OptionValue["API"]; 
	
	v = OptionValue["V"]; 

	serializer = OptionValue["Serializer"];
	 
	deserializer = OptionValue["Deserializer"]; 

	httpMethod = OptionValue["HTTPMethod"];

	settings = OptionValue["Settings"]; 
	
	endpoint = settings["Binance", "Endpoint"]; 

	apiKey = settings["Binance", "APIKey"]; 
	
	secretKey = settings["Binance", "SecretKey"]; 
	
	recvWindow = OptionValue["RecvWindow"]; 
	
	timestamp = binanceTime[]; 
	
	parameters = {args, 
		"timestamp" -> timestamp, 
		"recvWindow" -> recvWindow
	}; 
	
	requestBody = URLQueryEncode[serialize[parameters]]; 
	
	signature = HMAC[requestBody, secretKey, "SHA256"]; 
	
	fullParameters = serializer[Join[parameters, {"signature" -> signature}]]; 
	
	uri = URLBuild[{endpoint, api, v, method}, fullParameters]; 
	
	request = HTTPRequest[uri, <|
		Method -> httpMethod, 
		"Headers" -> {"X-MBX-APIKEY" -> apiKey}
	|>]; 
	
	response = URLRead[request]; 
	
	$ExchangeLinkHistory["PushBack", <|
		"Time" -> Now, 
		"Exchange" -> "Binance", 
		"Request" -> request, 
		"Response" -> response
    |>]; 
	
	body = response["Body"]; 
	
	result = deserializer[body]; 
	
	Return[result]
]


binanceSigned[{method__String, {args: Rule[_String, _]...}}, opts: OptionsPattern[]] := 
binanceSigned[{method, args}, opts]


options /: 
binanceSigned[path: {__}, options[opts: OptionsPattern[{}]]] := 
binanceSigned[path, options[binanceSigned, opts]]


(* ::Section::Closed:: *)
(*binance patterns*)


binanceKlinesIntervals[] := 
"1m" | "3m" | "5m" | "15m" | "30m" | "1h" | "2h" | "4h" | "6h" | "12h" | "1d" | "1w" | "1M"


(* ::Section:: *)
(*Binance market data implementation*)


(* ::Subsection:: *)
(*BinancePing*)


SyntaxInformation[BinancePing] = {
	"ArgumentsPattern" -> {OptionsPattern[binancePublic]}, 
	"OptionNames" -> optionNames[binancePublic]
}


BinancePing[opts: OptionsPattern[{binancePublic}]] := 
binancePublic[{"ping"}, opts]


(* ::Subsection:: *)
(*BinanceTime*)


SyntaxInformation[BinanceTime] = {
	"ArgumentsPattern" -> {OptionsPattern[binancePublic]}, 
	"OptionNames" -> optionNames[binancePublic]
}


BinanceTime[opts: OptionsPattern[{binancePublic}]] := 
binancePublic[{"time"}, opts]


(* ::Subsection:: *)
(*BinanceExchangeInfo*)


Options[BinanceExchangeInfo] = {
	"permissions" -> Automatic
}


SyntaxInformation[BinanceExchangeInfo] = {
	"ArgumentsPattern" -> {OptionsPattern[{binancePublic, BinanceExchangeInfo}]}, 
	"OptionNames" -> optionNames[binancePublic, BinanceExchangeInfo]
}


BinanceExchangeInfo[opts: OptionsPattern[{binancePublic, BinanceExchangeInfo}]] := 
binancePublic[{"exchangeInfo", filter[BinanceExchangeInfo, opts]}, filter[opts]]


BinanceExchangeInfo[symbol_String, opts: OptionsPattern[{binancePublic, BinanceExchangeInfo}]] := 
binancePublic[{"exchangeInfo", filter[BinanceExchangeInfo, "symbol" -> symbol, opts]}, filter[opts]]


BinanceExchangeInfo[symbols: {__String}, opts: OptionsPattern[{binancePublic, BinanceExchangeInfo}]] := 
binancePublic[{"exchangeInfo", filter[BinanceExchangeInfo, "symbols" -> symbols, opts]}, filter[opts]]


$BinanceExchangeInfo := 
cache[60 * 60, BinanceExchangeInfo[]]


(* ::Subsection:: *)
(*BinanceOrderBook*)


SyntaxInformation[BinanceOrderBook] = {
	"ArgumentsPattern" -> {OptionsPattern[binancePublic]}, 
	"OptionNames" -> optionNames[binancePublic]
}


BinanceOrderBook[symbol_String] := 
binancePublic[{"depth", "symbol" -> symbol}]


(* ::Subsection::Closed:: *)
(*BinanceTrades*)


SyntaxInformation[BinanceTime] = {
	"ArgumentsPattern" -> {OptionsPattern[binancePublic]}, 
	"OptionNames" -> optionNames[binancePublic]
}


BinanceTrades[symbol_String] := 
binancePublic[{"trades", "symbol" -> symbol}]


(* ::Subsection::Closed:: *)
(*BinanceTickerPrice*)


SyntaxInformation[BinanceTickerPrice] = {
	"ArgumentsPattern" -> {OptionsPattern[binancePublic]}, 
	"OptionNames" -> optionNames[binancePublic]
}


BinanceTickerPrice[opts: OptionsPattern[{binancePublic}]] := 
binancePublic[{"ticker", "price"}, opts]


BinanceTickerPrice[symbol_String, opts: OptionsPattern[{binancePublic}]] := 
binancePublic[{"ticker", "price", "symbol" -> symbol}, opts]


BinanceTickerPrice[symbols: {__String}] := 
binancePublic[{"ticker", "price", "symbols" -> symbols}]


(* ::Subsection::Closed:: *)
(*BinanceTicker24hr*)


SyntaxInformation[BinanceTime] = {
	"ArgumentsPattern" -> {OptionsPattern[binancePublic]}, 
	"OptionNames" -> optionNames[binancePublic]
}


BinanceTicker24hr[symbol_String] := 
binancePublic[{"ticker", "24hr", "symbol" -> symbol}]


BinanceTicker24hr[symbols: {__String}] := 
binancePublic[{"ticker", "24hr", "symbols" -> symbols}]


(* ::Subsection::Closed:: *)
(*BinanceTicker*)


SyntaxInformation[BinanceTime] = {
	"ArgumentsPattern" -> {OptionsPattern[binancePublic]}, 
	"OptionNames" -> optionNames[binancePublic]
}


BinanceTicker[symbol_String] := 
binancePublic[{"ticker", "symbol" -> symbol}]


BinanceTicker[symbols: {__String}] := 
binancePublic[{"ticker", "symbols" -> symbols}]


(* ::Subsection:: *)
(*BinanceKlines*)


addCodeCompletion["BinanceKlines"][{
	"BTCUSDT", "ETHUSDT", "ETHUSDT", "BTCETH", "BTCLTC"
}, List @@ binanceKlinesIntervals[]]


addOptionsCompletion[BinanceKlines, {"limit" -> {100, 200, 500}}]


Options[BinanceKlines] = 
autoOptions["limit"]


SyntaxInformation[BinanceKlines] = {
	"ArgumentsPattern" -> {OptionsPattern[{binancePublic, BinanceKlines}]}, 
	"OptionNames" -> optionNames[binancePublic, BinanceKlines]
}


BinanceKlines[symbol_String, interval: binanceKlinesIntervals[],
	opts: OptionsPattern[{binancePublic, BinanceKlines}]] := 
binancePublic[{"klines", filter[BinanceKlines, "symbol" -> symbol, "interval" -> interval, opts]}, 
	filter[opts]]


(* ::Subsection::Closed:: *)
(*BinanceUIKlines*)


SyntaxInformation[BinanceTime] = {
	"ArgumentsPattern" -> {OptionsPattern[binancePublic]}, 
	"OptionNames" -> optionNames[binancePublic]
}


BinanceUIKlines[symbol_String, interval_String] := 
binancePublic[{"uiKlines", "symbol" -> symbol, "interval" -> interval}]


(* ::Subsection::Closed:: *)
(*BinanceOrderBookTicker*)


SyntaxInformation[BinanceOrderBookTicker] = {
	"ArgumentsPattern" -> {OptionsPattern[binancePublic]}, 
	"OptionNames" -> optionNames[binancePublic]
}


BinanceOrderBookTicker[symbol_String] := 
binancePublic[{"ticker", "bookTicker", "symbol" -> symbol}]


(* ::Subsection::Closed:: *)
(*BinanceAveragePrice*)


SyntaxInformation[BinanceTime] = {
	"ArgumentsPattern" -> {OptionsPattern[binancePublic]}, 
	"OptionNames" -> optionNames[binancePublic]
}


BinanceAveragePrice[symbol_String] := 
binancePublic[{"avgPrice", "symbol" -> symbol}]


(* ::Subsection::Closed:: *)
(*BinanceAggTrades*)


SyntaxInformation[BinanceTime] = {
	"ArgumentsPattern" -> {OptionsPattern[binancePublic]}, 
	"OptionNames" -> optionNames[binancePublic]
}


BinanceAggTrades[symbol_String] := 
binancePublic[{"aggTrades", "symbol" -> symbol}]


(* ::Subsection::Closed:: *)
(*BinanceHistoricalTrades*)


SyntaxInformation[BinanceHistoricalTrades] = {
	"ArgumentsPattern" -> {OptionsPattern[{binancePublic}]}, 
	"OptionNames" -> optionNames[binancePublic]
}


BinanceHistoricalTrades[symbol_String] := 
binancePublic[{"historicalTrades", "symbol" -> symbol}]


(* ::Section::Closed:: *)
(*Binance wallet implementation*)


(* ::Subsection:: *)
(*BinanceSystemStatus*)


SyntaxInformation[BinanceSystemStatus] = {
	"ArgumentsPattern" -> {OptionsPattern[binancePublic]}, 
	"OptionNames" -> optionNames[binancePublic]
}


BinanceSystemStatus[opts: OptionsPattern[{binancePublic}]] := 
binancePublic[{"system", "status"}, options["V" -> "v1", "API" -> "sapi", opts]]


(* ::Subsection:: *)
(*BinanceUserCoins*)


SyntaxInformation[BinanceUserCoins] = {
	"ArgumentsPattern" -> {OptionsPattern[binanceSigned]}, 
	"OptionNames" -> optionNames[binanceSigned]
}


BinanceUserCoins[opts: OptionsPattern[{binanceSigned}]] := 
binanceSigned[{"capital", "config", "getall"}, options["API" -> "sapi", "V" -> "v1", "HTTPMethod" -> "GET", opts]]


(* ::Subsection:: *)
(*BinanceAccountSnapshot*)


Options[BinanceAccountSnapshot] = {
	"startTime" -> Automatic, 
	"endTime" -> Automatic, 
	"limit" -> Automatic
}


SyntaxInformation[BinanceAccountSnapshot] = {
	"ArgumentsPattern" -> {_, OptionsPattern[{binanceSigned, BinanceAccountSnapshot}]}, 
	"OptionNames" -> optionNames[binanceSigned, BinanceAccountSnapshot]
}


BinanceAccountSnapshot[accountType_String, opts: OptionsPattern[{BinanceAccountSnapshot, binanceSigned}]] := 
binanceSigned[{"accountSnapshot"}, "API" -> "sapi", "V" -> "v1", "HTTPMethod" -> "GET", options[binanceSigned, opts]]


(* ::Section::Closed:: *)
(*Binance spot trade implementation*)


(* ::Subsection:: *)
(*BinanceTestOrder*)


Options[BinanceTestOrder] = {
	"TimeInForce" -> "GTC"
}


SyntaxInformation[BinanceTestOrder] = {
	"ArgumentsPattern" -> {_, _, _, _, _., OptionsPattern[{binanceSigned, BinanceTestOrder}]}, 
	"OptionNames" -> optionNames[binanceSigned, BinanceTestOrder]
}


BinanceTestOrder[symbol_String, side_String, orderType_String, quantity_?NumericQ, price: Automatic | _?NumericQ, OptionsPattern[]] := 
binanceSigned[{"order", "test", 
	"symbol" -> symbol, 
	"side" -> side, 
	"type" -> orderType, 
	"quantity" -> quantity, 
	"price" -> price, 
	"timeInForce" -> OptionValue["TimeInForce"]
}]


(* ::Subsection:: *)
(*BinanceCreateOrder*)


Options[BinanceCreateOrder] = {
	"TimeInForce" -> "GTC"
}


SyntaxInformation[BinanceCreateOrder] = {
	"ArgumentsPattern" -> {_, _, _, _, _., OptionsPattern[{binanceSigned, BinanceCreateOrder}]}, 
	"OptionNames" -> optionNames[binanceSigned, BinanceCreateOrder]
}


BinanceCreateOrder[symbol_String, side_String, orderType_String, quantity_?NumericQ, price: Automatic | _?NumericQ, OptionsPattern[]] := 
binanceSigned[{"order", 
	"symbol" -> symbol, 
	"side" -> side, 
	"type" -> orderType, 
	"quantity" -> quantity, 
	"price" -> price, 
	"timeInForce" -> OptionValue["TimeInForce"]
}]


(* ::Subsection:: *)
(*BinanceBuy*)


Options[BinanceBuy] = {
	"OrderType" -> "LIMIT"
}


BinanceBuy[symbol_String, quantity_?NumericQ, OptionsPattern[]] := 
BinanceCreateOrder[symbol, "BUY", "MARKET", quantity, Automatic, "TimeInForce" -> Automatic]


BinanceBuy[symbol_String, quantity_?NumericQ, price_?NumericQ, OptionsPattern[]] := 
BinanceCreateOrder[symbol, "BUY", OptionValue["OrderType"], quantity, price]


BinanceBuy[symbol_String, quantity_?NumericQ, price_?NumericQ, OptionsPattern[]] := 
BinanceCreateOrder[symbol, "SELL", OptionValue["OrderType"], quantity, price]


(* ::Subsection:: *)
(*BinanceSell*)


Options[BinanceSell] = {
	"OrderType" -> "LIMIT"
}


BinanceSell[symbol_String, quantity_?NumericQ, OptionsPattern[]] := 
BinanceCreateOrder[symbol, "SELL", "MARKET", quantity, Automatic, "TimeInForce" -> Automatic]


(* ::Subsection:: *)
(*BinanceGetOrder*)


BinanceGetOrder[symbol_String, orderId_Integer, OptionsPattern[]] := 
binanceSigned[{"order", "symbol" -> symbol, "orderId" -> orderId}, "HTTPMethod" -> "GET"]


BinanceGetOrder[symbol_String, origClientOrderId_String, OptionsPattern[]] := 
binanceSigned[{"order", "symbol" -> symbol, "origClientOrderId" -> origClientOrderId}, "HTTPMethod" -> "GET"]


(* ::Subsection:: *)
(*BinanceGetOrders*)


BinanceGetOrders[symbol_String: Automatic] := 
binanceSigned[{"openOrders", "symbol" -> symbol}, "HTTPMethod" -> "GET"]


(* ::Subsection:: *)
(*BinanceGetAllOrders*)


BinanceGetAllOrders[symbol_String] := 
binanceSigned[{"allOrders", "symbol" -> symbol}, "HTTPMethod" -> "GET"]


(* ::Subsection:: *)
(*BinanceCancelOrder*)


BinanceCancelOrder[symbol_String, orderId_Integer, OptionsPattern[]] := 
binanceSigned[{"order", "symbol" -> symbol, "orderId" -> orderId}, "HTTPMethod" -> "DELETE"]


BinanceCancelOrder[symbol_String, origClientOrderId_String, OptionsPattern[]] := 
binanceSigned[{"order", "symbol" -> symbol, "origClientOrderId" -> origClientOrderId}, "HTTPMethod" -> "DELETE"]


(* ::Subsection:: *)
(*BinanceCancelAllOrders*)


BinanceCancelAllOrders[symbol_String, OptionsPattern[]] := 
binanceSigned[{"openOrders", "symbol" -> symbol}, "HTTPMethod" -> "DELETE"]


(* ::Subsection:: *)
(*BinanceCancelReplaceOrder*)


BinanceCancelReplaceOrder[cancelOrderId_Integer, symbol_String, side_String, orderType_String, quantity_?NumericQ, price_?NumericQ] := 
binanceSigned[{"order", "cancelReplace", 
	"origClientOrderId" -> cancelOrderId, 
	"symbol" -> symbol
}, "HTTPMethod" -> "GET"]


(* ::Section::Closed:: *)
(*Binance market streams implementation*)


(* ::Subsection:: *)
(*BinanceStreamSubscribe*)


Options[BinanceStreamSubscribe] = {
	"Settings" :> $ExchangeLinkSettings, 
	"Serializer" :> binanceStreamToURL, 
	"Deserializer" :> binanceStreamDeserialize, 
	"EventHandler" :> Identity
}


SyntaxInformation[BinanceStreamSubscribe] = {
	"ArgumentsPattern" -> {_, _., OptionsPattern[BinanceStreamSubscribe]}, 
	"OptionNames" -> optionNames[BinanceStreamSubscribe]
}


BinanceStreamSubscribe[streams_List, opts: OptionsPattern[{BinanceStreamSubscribe}]] := 
Module[{settings, streamsString, url, webSocketEndpoint, serialize, deserialize, eventHandler, connection}, 
	
	settings = OptionValue["Settings"]; 
	webSocketEndpoint = settings["Binance", "WebSocketEndpoint"]; 
	serialize = OptionValue["Serializer"]; 
	deserialize = OptionValue["Deserializer"]; 
	eventHandler = OptionValue["EventHandler"]; 
	
	streamsString = serialize[streams]; 
	url = StringJoin[{webSocketEndpoint, streamsString}]; 
	connection = WebSocketConnect[url, "Deserializer" -> deserialize, "EventHandler" -> eventHandler]; 
	
	Return[connection]
]


BinanceStreamSubscribe[connection_WebSocketConnectionObject, streams_List, OptionsPattern[{BinanceStreamSubscribe}]] := 
Module[{frame, serializer}, 
	serializer = OptionValue["Serializer"]; 
	frame = serializer[streams]; 
	WebSocketSend[connection, frame]; 
	
	Return[connection]
]


(* ::Subsection:: *)
(*BinanceMiniTickerStream*)


SyntaxInformation[BinanceMiniTickerStream] = {
	"ArgumentsPattern" -> {_., _., OptionsPattern[{BinanceStreamSubscribe}]}
}


BinanceMiniTickerStream[symbols: {__String}] := 
BinanceStreamSubscribe[Map[ToLowerCase[#] <> "@miniTicker"&] @ symbols]


BinanceMiniTickerStream[symbol_String] := 
BinanceMiniTickerStream[{symbol}]


BinanceMiniTickerStream[connection_WebSocketConnectionObject, symbols: {__String}] := 
BinanceStreamSubscribe[connection, Map[ToLowerCase[#] <> "@miniTicker"&] @ symbols]


BinanceMiniTickerStream[connection_WebSocketConnectionObject, symbol_String] := 
BinanceMiniTickerStream[connection, {symbol}]


(* ::Subsection:: *)
(*BinanceTickerStream*)


BinanceTickerStream[]


(* ::Chapter:: *)
(*Kraken implementation*)


(* ::Section::Closed:: *)
(*Kraken serialization*)


krakenSerializer[args: {Rule[_String, _]...}] := 
DeleteCases[Flatten[{args}], Rule[_String, Automatic | Null]]


krakenDeserializer[body_String] := 
ImportString[body, "RawJSON"]


(* ::Section::Closed:: *)
(*Kraken public request executer*)


Options[krakenPublic] = {
	"Settings" :> getSettings[], 
	"HTTPMethod" :> "GET", 
	"V" :> "0", 
	"API" :> "public", 
	"Serializer" :> krakenSerializer, 
	"Deserializer" :> krakenDeserializer
}


krakenPublic[{path__String, args: Rule[_String, _]...}, OptionsPattern[]] := 
Module[{settings, endpoint, httpMethod, v, api, parameters, url, request, response, body, result, 
	deserializer, serializer}, 
	
	settings = OptionValue["Settings"]; 
	endpoint = settings["Kraken", "Endpoint"]; 
	httpMethod = OptionValue["HTTPMethod"]; 
	v = OptionValue["V"]; 
	api = OptionValue["API"]; 
	
	serializer = OptionValue["Serializer"]; 
	deserializer = OptionValue["Deserializer"]; 
	
	parameters = serializer[args]; 
	url = URLBuild[{endpoint, v, api, path}, parameters]; 
	request = HTTPRequest[url, <|Method -> httpMethod|>]; 
	
	response = URLRead[request]; 
	body = response["Body"]; 
	result = deserializer[body]; 
	
	Return[result]
]


krakenPublic[{path__String, {args: Rule[_String, _]...}}, opts: OptionsPattern[]] := 
krakenPublic[{path, args}, opts]


krakenPublic[{path__String, options[args: OptionsPattern[{}]]}, opts: OptionsPattern[]] := 
krakenPublic[{path, FilterRules[Flatten[{args}], Except[Options[krakenPublic]]]}, opts]


(* ::Section::Closed:: *)
(*Kraken signature*)


krakenSignature[urlpath_, data_, secret_] := 
Module[{postdata, encoded, message, mac, sig}, 
	postdata = URLQueryEncode[data]; 
	encoded = StringToByteArray[("nonce" /. data) <> postdata]; 
	message = Join[StringToByteArray[urlpath], Hash[encoded, "SHA256", "ByteArray"]]; 
	mac = HMAC[message, BaseDecode[secret, "Base64"], "SHA512"]; 
	sig = BaseEncode[BaseDecode[mac, "Base16"], "Base64"]; 
	Return[sig]
]


(* ::Section::Closed:: *)
(*Kraken private request executer*)


Options[krakenPrivate] = {
	"Settings" :> getSettings[], 
	"HTTPMethod" :> "POST", 
	"V" :> "0", 
	"API" :> "private", 
	"Serializer" :> krakenSerializer, 
	"Deserializer" :> krakenDeserializer
}


krakenPrivate[{path__String, args: Rule[_String, _]...}, OptionsPattern[]] := 
Module[{settings, endpoint, httpMethod, v, api, parameters, url, request, response, body, result, 
	deserializer, serializer, apiKey, secretKey, uri, signature}, 
	
	settings = OptionValue["Settings"]; 
	endpoint = settings["Kraken", "Endpoint"]; 
	
	apiKey = settings["Kraken", "APIKey"]; 
	secretKey = settings["Kraken", "SecretKey"]; 
	
	httpMethod = OptionValue["HTTPMethod"]; 
	v = OptionValue["V"]; 
	api = OptionValue["API"]; 
	
	serializer = OptionValue["Serializer"]; 
	deserializer = OptionValue["Deserializer"]; 
	
	parameters = serializer[Flatten[{"nonce" -> nonce[], args}]]; 
	url = URLBuild[{endpoint, v, api, path}]; 
	uri = URLBuild[{"", v, api, path}];
	signature = krakenSignature[uri, parameters, secretKey]; 
	
	request = HTTPRequest[url, <|
		Method -> httpMethod, 
		"Headers" -> {
			"API-Key" -> settings["Kraken", "APIKey"], 
			"API-Sign" -> signature, 
			"Content-Type" -> "application/x-www-form-urlencoded; charset=utf-8"
		}, 
		"Body" -> URLQueryEncode[parameters]
	|>]; 
	
	response = URLRead[request]; 
	
	saveToHistory["Kraken", request, response]; 
	
	body = response["Body"]; 
	result = deserializer[body]; 
	
	Return[result]
]


krakenPrivate[{path__String, {args: Rule[_String, _]...}}, opts: OptionsPattern[]] := 
krakenPrivate[{path, args}, opts]


krakenPrivate[{path__String, options[args: OptionsPattern[{}]]}, opts: OptionsPattern[]] := 
krakenPrivate[{path, FilterRules[Flatten[{args}], Except[Options[krakenPrivate]]]}, opts]


(* ::Section::Closed:: *)
(*Kraken objects*)


krakenTradeOrderTypes[] := 
"market" | "limit" | "stop-loss" | "take-profit" | "stop-loss-limit" | "take-profit-limit" | "settle-position"


krakenTradeTypes[] := 
"buy" | "sell"


(* ::Section::Closed:: *)
(*Kraken public implementation*)


(* ::Subsection::Closed:: *)
(*KrakenTime*)


SyntaxInformation[KrakenTime] = {
	"ArgumentsPattern" -> {OptionsPattern[{krakenPublic}]}, 
	"OptionNames" -> optionNames[krakenPublic]
}


KrakenTime[opts: OptionsPattern[{krakenPublic}]] := 
krakenPublic[{"Time"}, opts]


(* ::Subsection::Closed:: *)
(*KrakenSystemStatus*)


SyntaxInformation[KrakenSystemStatus] = {
	"ArgumentsPattern" -> {OptionsPattern[{krakenPublic}]}, 
	"OptionNames" -> optionNames[krakenPublic]
}


KrakenSystemStatus[opts: OptionsPattern[{krakenPublic}]] := 
krakenPublic[{"SystemStatus"}, opts]


(* ::Subsection::Closed:: *)
(*KrakenAssetInfo*)


Options[KrakenAssetInfo] = 
autoOptions["aclass"]


SyntaxInformation[KrakenAssetInfo] = {
	"ArgumentsPattern" -> {_., OptionsPattern[{krakenPublic, KrakenAssetInfo}]}
}


KrakenAssetInfo[opts: OptionsPattern[{krakenPublic, KrakenAssetInfo}]] := 
krakenPublic[{"AssetInfo", options[opts]}, options[opts]]


KrakenAssetInfo[asset_String, opts: OptionsPattern[{krakenPublic, KrakenAssetInfo}]] := 
krakenPublic[{"AssetInfo", options[{"assets" -> asset, opts}]}, opts]


KrakenAssetInfo[assets: {__String}, opts: OptionsPattern[{krakenPublic, KrakenAssetInfo}]] := 
KrakenAssetInfo[StringRiffle[assets, ","], opts]


(* ::Subsection::Closed:: *)
(*KrakenAssetPairs*)


Options[KrakenAssetPairs] = 
autoOptions["info"]


SyntaxInformation[KrakenAssetPairs] = {
	"ArgumentsPattern" -> {_., OptionsPattern[{krakenPublic, KrakenAssetPairs}]}, 
	"OptionNames" -> optionNames[krakenPublic, KrakenAssetPairs]
}


KrakenAssetPairs[opts: OptionsPattern[{krakenPublic, KrakenAssetPairs}]] := 
krakenPublic[{"AssetPairs", options[opts]}, options[opts]]


KrakenAssetPairs[pair_String, opts: OptionsPattern[{krakenPublic, KrakenAssetPairs}]] := 
krakenPublic[{"AssetPairs", options[{"pair" -> pair, opts}]}, options[opts]]


KrakenAssetPairs[pairs: {__String}, opts: OptionsPattern[{krakenPublic, KrakenAssetPairs}]] := 
KrakenAssetPairs[StringRiffle[pairs, ","], opts]


(* ::Subsection::Closed:: *)
(*KrakenTicker*)


SyntaxInformation[KrakenTicker] = {
	"ArgumentsPattern" -> {_, OptionsPattern[krakenPublic]}, 
	"OptionNames" -> optionNames[krakenPublic]
}


KrakenTicker[pair_String, opts: OptionsPattern[{krakenPublic}]] := 
krakenPublic[{"Ticker", "pair" -> pair}, opts]


(* ::Subsection::Closed:: *)
(*KrakenOHLC*)


Options[KrakenOHLC] = 
autoOptions["interval", "since"]


SyntaxInformation[KrakenOHLC] = {
	"ArgumentsPattern" -> {_, OptionsPattern[krakenPublic]}, 
	"OptionNames" -> optionNames[krakenPublic, KrakenOHLC]
}


KrakenOHLC[pair_String, opts: OptionsPattern[{krakenPublic, KrakenOHLC}]] := 
krakenPublic[{"Ticker", options[{"pair" -> pair, opts}]}, options[opts]]


(* ::Subsection::Closed:: *)
(*KrakenDepth*)


Options[KrakenDepth] = 
autoOptions["count"]


SyntaxInformation[KrakenDepth] = {
	"ArgumentsPattern" -> {_, OptionsPattern[{krakenPublic, KrakenDepth}]}, 
	"OptionNames" -> optionNames[krakenPublic, KrakenDepth]
}


KrakenDepth[pair_String, opts: OptionsPattern[{krakenPublic, KrakenDepth}]] := 
krakenPublic[{"Depth", options[{"pair" -> pair, opts}]}, options[opts]]


(* ::Subsection::Closed:: *)
(*KrakenTrades*)


Options[KrakenTrades] = 
autoOptions["since"]


SyntaxInformation[KrakenTrades] = {
	"ArgumentsPattern" -> {_, OptionsPattern[{krakenPublic, KrakenTrades}]}
}


KrakenTrades[pair_String, opts: OptionsPattern[{krakenPublic, KrakenTrades}]] := 
krakenPublic[{"Trades", options[{"pair" -> pair, opts}]}, options[opts]]


(* ::Subsection::Closed:: *)
(*KrakenRecentSpreads*)


Options[KrakenSpread] = 
autoOptions["since"]


SyntaxInformation[KrakenSpread] = {
	"ArgumentsPattern" -> {_, OptionsPattern[{krakenPublic, KrakenSpread}]}
}


KrakenSpread[pair_String, opts: OptionsPattern[{krakenPublic, KrakenSpread}]] := 
krakenPublic[{"Spread", options[{"pair" -> pair, opts}]}, options[opts]]


(* ::Section::Closed:: *)
(*Kraken user data implementation*)


(* ::Subsection::Closed:: *)
(*KrakenBalance*)


SyntaxInformation[KrakenUserBalance] = {
	"ArgumentsPattern" -> {OptionsPattern[krakenPrivate]}, 
	"OptionNames" -> optionNames[krakenPrivate]
}


KrakenBalance[opts: OptionsPattern[{krakenPrivate}]] := 
krakenPrivate[{"Balance"}, opts]


(* ::Subsection::Closed:: *)
(*KrakenTradeBalance*)


SyntaxInformation[KrakenTradeBalance] = {
	"ArgumentsPattern" -> {_., OptionsPattern[krakenPrivate]}, 
	"OptionNames" -> optionNames[krakenPrivate]
}


KrakenTradeBalance[opts: OptionsPattern[{krakenPrivate}]] := 
krakenPrivate[{"TradeBalance"}, opts]


KrakenTradeBalance[asset_String, opts: OptionsPattern[{krakenPrivate}]] := 
krakenPrivate[{"TradeBalance", "asset" -> asset}, opts]


(* ::Subsection::Closed:: *)
(*KrakenOpenOrders*)


Options[KrakenOpenOrders] = 
autoOptions["trades", "userref"]


SyntaxInformation[KrakenOpenOrders] = {
	"ArgumentsPattern" -> {OptionsPattern[{krakenPrivate, KrakenOpenOrders}]}, 
	"OptionNames" -> optionNames[krakenPrivate, KrakenOpenOrders]
}


KrakenOpenOrders[opts: OptionsPattern[{krakenPrivate, KrakenOpenOrders}]] := 
krakenPrivate[{"OpenOrders", options[opts]}, options[opts]]


(* ::Subsection::Closed:: *)
(*KrakenClosedOrders*)


Options[KrakenClosedOrders] = 
autoOptions["trades", "userref", "start", "end", "ofs", "closetime"]


SyntaxInformation[KrakenOpenOrders] = {
	"ArgumentsPattern" -> {OptionsPattern[{krakenPrivate, KrakenClosedOrders}]}, 
	"OptionNames" -> optionNames[krakenPrivate, KrakenClosedOrders]
}


KrakenClosedOrders[opts: OptionsPattern[{krakenPrivate, KrakenClosedOrders}]] := 
krakenPrivate[{"ClosedOrders", options[opts]}, options[opts]]


(* ::Subsection::Closed:: *)
(*KrakenQueryOrdes*)


Options[KrakenQueryOrders] = 
autoOptions["trades", "userref"]


SyntaxInformation[KrakenOpenOrders] = {
	"ArgumentsPattern" -> {_, OptionsPattern[{krakenPrivate, KrakenQueryOrders}]}, 
	"OptionNames" -> optionNames[krakenPrivate, KrakenQueryOrders]
}


KrakenQueryOrders[txid_String, opts: OptionsPattern[{krakenPrivate, KrakenQueryOrders}]] := 
krakenPrivate[{"QueryOrders", options[{"txid" -> txid, opts}]}, options[opts]]


(* ::Subsection::Closed:: *)
(*KrakenTradesHistory*)


Options[KrakenTradeHistory] = 
autoOptions["type", "trades", "start", "end", "ofs"]


SyntaxInformation[KrakenTradeHistory] = {
	"ArgumentsPattern" -> {OptionsPattern[{krakenPrivate, KrakenTradeHistory}]}, 
	"OptionNames" -> optionNames[krakenPrivate, KrakenTradeHistory]
}


KrakenTradeHistory[opts: OptionsPattern[{krakenPrivate, KrakenTradeHistory}]] := 
krakenPrivate[{"TradeHistory", options[opts]}, options[opts]]


(* ::Subsection::Closed:: *)
(*KrakenQueryTrades*)


Options[KrakenQueryTrades] = 
autoOptions["txid", "trades"]


SyntaxInformation[KrakenQueryTrades] = {
	"ArgumentsPattern" -> {OptionsPattern[{krakenPrivate, KrakenQueryTrades}]}, 
	"OptionNames" -> optionNames[krakenPrivate, KrakenQueryTrades]
}


KrakenQueryTrades[opts: OptionsPattern[{krakenPrivate, KrakenQueryTrades}]] := 
krakenPrivate[{"QueryTrades", options[opts]}, options[opts]]


(* ::Subsection::Closed:: *)
(*KrakenOpenPositions*)


Options[KrakenOpenPositions] = 
autoOptions["txid", "docalcs", "consolidation"]


SyntaxInformation[KrakenOpenPositions] = {
	"ArgumentsPattern" -> {OptionsPattern[{krakenPrivate, KrakenOpenPositions}]}, 
	"OptionNames" -> optionNames[krakenPrivate, KrakenOpenPositions]
}


KrakenOpenPositions[opts: OptionsPattern[{krakenPrivate, KrakenOpenPositions}]] := 
krakenPrivate[{"OpenPositions", options[opts]}, options[opts]]


(* ::Subsection::Closed:: *)
(*KrakenLedgers*)


Options[KrakenLedgers] = 
autoOptions["asset", "aclass", "type", "start", "end", "ofs"]


SyntaxInformation[KrakenLedgers] = {
	"ArgumentsPattern" -> {OptionsPattern[{krakenPrivate, KrakenLedgers}]}, 
	"OptionNames" -> optionNames[krakenPrivate, KrakenLedgers]
}


KrakenLedgers[opts: OptionsPattern[{krakenPrivate, KrakenLedgers}]] := 
krakenPrivate[{"Ledgers", options[opts]}, options[opts]]


(* ::Subsection::Closed:: *)
(*KrakenQueryLedgers*)


Options[KrakenQueryLedgers] = 
autoOptions["id", "trades"]


SyntaxInformation[KrakenQueryLedgers] = {
	"ArgumentsPattern" -> {OptionsPattern[{krakenPrivate, KrakenQueryLedgers}]}, 
	"OptionNames" -> optionNames[krakenPrivate, KrakenQueryLedgers]
}


KrakenQueryLedgers[opts: OptionsPattern[{krakenPrivate, KrakenQueryLedgers}]] := 
krakenPrivate[{"QueryLedgers", options[opts]}, options[opts]]


(* ::Subsection::Closed:: *)
(*KrakenTradeVolume*)


Options[KrakenTradeVolume] = 
autoOptions["pair", "fee-info"]


SyntaxInformation[KrakenTradeVolume] = {
	"ArgumentsPattern" -> {OptionsPattern[{krakenPrivate, KrakenTradeVolume}]}, 
	"OptionNames" -> optionNames[krakenPrivate, KrakenTradeVolume]
}


KrakenTradeVolume[opts: OptionsPattern[{krakenPrivate, KrakenTradeVolume}]] := 
krakenPrivate[{"TradeVolume", options[opts]}, options[opts]]


(* ::Subsection::Closed:: *)
(*KrakenAddExport*)


Options[KrakenAddExport] = 
autoOptions["format", "fields", "starttm", "endtm"]


SyntaxInformation[KrakenAddExport] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[{krakenPrivate, KrakenAddExport}]}, 
	"OptionNames" -> optionNames[krakenPrivate, KrakenAddExport]
}


KrakenAddExport[report: "trades" | "ledgers", description_String, 
	opts: OptionsPattern[{krakenPrivate, KrakenAddExport}]] := 
krakenPrivate[{"AddExport", options["report" -> report, "description" -> description, opts]}, options[opts]]


(* ::Subsection::Closed:: *)
(*KrakenExportStatus*)


SyntaxInformation[KrakenExportStatus] = {
	"ArgumentsPattern" -> {_, OptionsPattern[{krakenPrivate}]}, 
	"OptionNames" -> optionNames[krakenPrivate]
}


KrakenExportStatus[report: "trades" | "ledgers", opts: OptionsPattern[krakenPrivate]] := 
krakenPrivate[{"ExportStatus", "report" -> report}, opts]


(* ::Subsection::Closed:: *)
(*KrakenRetrieveExport*)


SyntaxInformation[KrakenOpenOrders] = {
	"ArgumentsPattern" -> {_, OptionsPattern[{krakenPrivate}]}, 
	"OptionNames" -> optionNames[krakenPrivate]
}


KrakenRetrieveExport[id_String, opts: OptionsPattern[krakenPrivate]] := 
krakenPrivate[{"RetrieveExport", "id" -> id}, opts]


(* ::Subsection::Closed:: *)
(*KrakenRemoveExport*)


SyntaxInformation[KrakenOpenOrders] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[{krakenPrivate}]}, 
	"OptionNames" -> optionNames[krakenPrivate]
}


KrakenRemoveExport[id_String, type: "cancel" | "delete", opts: OptionsPattern[krakenPrivate]] := 
krakenPrivate[{"RemoveExport", "id" -> id, "type" -> type}, opts]


(* ::Section::Closed:: *)
(*Kraken user trading api implementation*)


(* ::Subsection::Closed:: *)
(*KrakenAddOrder*)


Options[KrakenOpenOrders] = 
autoOptions["userref", "displayvol", "price", "price2", "trigger", "leverage", "stptype", "oflags", 
	"timeinforce", "starttm", "expiretm", "close[ordertype]", "close[price]", "close[price2]", 
	"deadline", "validate"]


SyntaxInformation[KrakenOpenOrders] = {
	"ArgumentsPattern" -> {_, _, _, _, OptionsPattern[{krakenPrivate, KrakenOpenOrders}]}, 
	"OptionNames" -> optionNames[krakenPrivate, KrakenOpenOrders]
}


KrakenAddOrder[
	ordertype: krakenTradeOrderTypes[], 
	type: krakenTradeTypes[], 
	volume_?NumericQ, 
	pair_String, 
	opts: OptionsPattern[{krakenPrivate, KrakenAddOrder}]] := 
krakenPrivate[{"AddOrder", "ordertype" -> ordertype, "type" -> type, "volume" -> volume, "pair" -> pair, 
	options[opts]}, options[opts]]


(* ::Subsection::Closed:: *)
(*KrakenAddOrderBatch*)


Options[KrakenAddOrderBatch] = 
autoOptions["deadline", "validate"]


SyntaxInformation[KrakenOpenOrders] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[{krakenPrivate, KrakenAddOrderBatch}]}, 
	"OptionNames" -> optionNames[krakenPrivate, KrakenAddOrderBatch]
}


KrakenAddOrderBatch[pair_, orders_, opts: OptionsPattern[{krakenPrivate, KrakenAddOrderBatch}]] := 
krakenPrivate[{"AddOrderBatch", options["orders" -> orders, "pair" -> pair, opts]}, options[opts]]


(* ::Subsection::Closed:: *)
(*KrakenEditOrder*)


Options[KrakenEditOrder] = 
autoOptions["userref", "volume", "displayvol", "price", "price2", "oflags", "deadline", 
	"cancel_response", "validate"]


SyntaxInformation[KrakenEditOrder] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[{krakenPrivate, KrakenEditOrder}]}, 
	"OptionNames" -> optionNames[krakenPrivate, KrakenEditOrder]
}


KrakenEditOrder[txid: _String | _Integer, pair_String, opts: OptionsPattern[{krakenPrivate, KrakenEditOrder}]] := 
krakenPrivate[{"EditOrder", options["txid" -> txid, "pair" -> pair, opts]}, options[opts]]


(* ::Subsection::Closed:: *)
(*KrakenCancelOrder*)


SyntaxInformation[KrakenCancelOrder] = {
	"ArgumentsPattern" -> {_, OptionsPattern[{krakenPrivate}]}, 
	"OptionNames" -> optionNames[krakenPrivate]
}


KrakenCancelOrder[txid: _String | _Integer, opts: OptionsPattern[{krakenPrivate}]] := 
krakenPrivate[{"CancelOrder", "txid" -> txid}, opts]


(* ::Subsection::Closed:: *)
(*KrakenCancelAll*)


SyntaxInformation[KrakenCancelAll] = {
	"ArgumentsPattern" -> {OptionsPattern[{krakenPrivate}]}, 
	"OptionNames" -> optionNames[krakenPrivate]
}


KrakenCancelAll[opts: OptionsPattern[krakenPrivate]] := 
krakenPrivate[{"CancelAll"}, opts]


(* ::Subsection::Closed:: *)
(*KrakenCancelAllOrdersAfter*)


SyntaxInformation[KrakenCancelAllOrdersAfter] = {
	"ArgumentsPattern" -> {_, OptionsPattern[{krakenPrivate}]}, 
	"OptionNames" -> optionNames[krakenPrivate]
}


KrakenCancelAllOrdersAfter[timeout_, opts: OptionsPattern[{krakenPrivate}]] := 
krakenPrivate[{"CancelAllOrdersAfter", "timeout" -> timeout},  opts]


(* ::Subsection::Closed:: *)
(*KrakenCancelOrdersBatch*)


SyntaxInformation[KrakenCancelAllOrdersBatch] = {
	"ArgumentsPattern" -> {_, OptionsPattern[{krakenPrivate}]}, 
	"OptionNames" -> optionNames[krakenPrivate]
}


KrakenCancelAllOrdersBatch[orders_, opts: OptionsPattern[{krakenPrivate}]] := 
krakenPrivate[{"CancelAllOrdersBatch", "orders" -> orders}, opts]


(* ::Section::Closed:: *)
(*Kraken user funding api implementation*)


(* ::Subsection::Closed:: *)
(*KrakenDepositMethods*)


SyntaxInformation[KrakenDepositMethods] = {
	"ArgumentsPattern" -> {_, OptionsPattern[{krakenPrivate}]}, 
	"OptionNames" -> optionNames[krakenPrivate]
}


KrakenDepositMethods[asset_String, opts: OptionsPattern[{krakenPrivate}]] := 
krakenPrivate[{"DepositMethods", "asset" -> asset}, opts]


(* ::Subsection::Closed:: *)
(*KrakenDepositAddresses*)


Options[KrakenDepositAddresses] = 
autoOptions["new"]


SyntaxInformation[KrakenDepositAddresses] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[{krakenPrivate, KrakenDepositAddresses}]}, 
	"OptionNames" -> optionNames[krakenPrivate, KrakenDepositAddresses]
}


KrakenDepositAddresses[asset_String, method_String, 
	opts: OptionsPattern[{krakenPrivate, KrakenDepositAddresses}]] := 
krakenPrivate[{"DepositAddresses", options["asset" -> asset, "method" -> method, opts]}, options[opts]]


(* ::Subsection::Closed:: *)
(*KrakenDepositStatus*)


Options[KrakenDepositStatus] = 
autoOptions["method"]


SyntaxInformation[KrakenDepositStatus] = {
	"ArgumentsPattern" -> {_, OptionsPattern[{krakenPrivate, KrakenDepositStatus}]}, 
	"OptionNames" -> optionNames[krakenPrivate, KrakenDepositStatus]
}


KrakenDepositStatus[asset_String, opts: OptionsPattern[{krakenPrivate, KrakenDepositStatus}]] := 
krakenPrivate[{"DepositStatus", options["asset" -> asset, opts]}, options[opts]]


(* ::Subsection::Closed:: *)
(*KrakenWithdrawInfo*)


SyntaxInformation[KrakenWithdrawInfo] = {
	"ArgumentsPattern" -> {_, _, _, OptionsPattern[{krakenPrivate}]}, 
	"OptionNames" -> optionNames[krakenPrivate]
}


KrakenWithdrawInfo[asset_String, key_String, amount_, opts: OptionsPattern[{krakenPrivate}]] := 
krakenPrivate[{"WithdrawInfo", "asset" -> asset, "key" -> key, "amount" -> amount}, opts]


(* ::Subsection::Closed:: *)
(*KrakenWithdraw*)


SyntaxInformation[KrakenWithdraw] = {
	"ArgumentsPattern" -> {_, OptionsPattern[{krakenPrivate}]}, 
	"OptionNames" -> optionNames[krakenPrivate]
}


KrakenWithdraw[asset_String, key_String, amount_, opts: OptionsPattern[{krakenPrivate}]] := 
krakenPrivate[{"Withdraw", "asset" -> asset, "key" -> key, "amount" -> amount}, opts]


(* ::Subsection::Closed:: *)
(*KrakenWithdrawStatus*)


Options[KrakenWithdrawStatus] = 
autoOptions["method"]


SyntaxInformation[KrakenWithdrawStatus] = {
	"ArgumentsPattern" -> {_, OptionsPattern[{krakenPrivate, KrakenWithdrawStatus}]}, 
	"OptionNames" -> optionNames[krakenPrivate, KrakenWithdrawStatus]
}


KrakenWithdrawStatus[asset_String, opts: OptionsPattern[{krakenPrivate, KrakenWithdrawStatus}]] := 
krakenPrivate[{"WithdrawStatus", options["asset" -> asset, opts]}, options[opts]]


(* ::Subsection::Closed:: *)
(*KrakenWithdrawCancel*)


SyntaxInformation[KrakenWithdrawCancel] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[{krakenPrivate}]}, 
	"OptionNames" -> optionNames[krakenPrivate]
}


KrakenWithdrawCancel[asset_String, refid_String, opts: OptionsPattern[{krakenPrivate}]] := 
krakenPrivate[{"WithdrawCancel", "asset" -> asset, "refid" -> refid}, opts]


(* ::Subsection::Closed:: *)
(*KrakenWalletTransfer*)


SyntaxInformation[KrakenWalletTransfer] = {
	"ArgumentsPattern" -> {_, _, _, _, OptionsPattern[{krakenPrivate}]}, 
	"OptionNames" -> optionNames[krakenPrivate]
}


KrakenWalletTransfer[asset_, from_, to_, amount_, opts: OptionsPattern[{krakenPrivate}]] := 
krakenPrivate[{"WalletTransfer", "asset" -> asset, "from" -> from, "to" -> to, "amount" -> amount}, opts]


(* ::Section::Closed:: *)
(*Kraken user staking api implementation*)


(* ::Subsection::Closed:: *)
(*KrakenStake*)


SyntaxInformation[KrakenStake] = {
	"ArgumentsPattern" -> {_, _, _, OptionsPattern[{krakenPrivate}]}, 
	"OptionNames" -> optionNames[krakenPrivate]
}


KrakenStake[asset_, amount_, method_, opts: OptionsPattern[{krakenPrivate}]] := 
krakenPrivate[{"Stake", "asset" -> asset, "amount" -> amount, "method" -> method}, opts]


(* ::Subsection::Closed:: *)
(*KrakenUnstake*)


SyntaxInformation[KrakenUnstake] = {
	"ArgumentsPattern" -> {_, _, OptionsPattern[{krakenPrivate}]}, 
	"OptionNames" -> optionNames[krakenPrivate]
}


KrakenUnstake[asset_, amount_, opts: OptionsPattern[{krakenPrivate}]] := 
krakenPrivate[{"Unstake", "asset" -> asset, "amount" -> amount}, opts]


(* ::Subsection::Closed:: *)
(*KrakenStakingAssets*)


SyntaxInformation[KrakenStakingAssets] = {
	"ArgumentsPattern" -> {OptionsPattern[{krakenPrivate}]}, 
	"OptionNames" -> optionNames[krakenPrivate]
}


KrakenStakingAssets[opts: OptionsPattern[{krakenPrivate}]] := 
krakenPrivate[{"StakingAssets"}, opts]


(* ::Subsection::Closed:: *)
(*KrakenStakingPending*)


SyntaxInformation[KrakenStakingPending] = {
	"ArgumentsPattern" -> {OptionsPattern[{krakenPrivate}]}, 
	"OptionNames" -> optionNames[krakenPrivate]
}


KrakenStakingPending[opts: OptionsPattern[{krakenPrivate}]] := 
krakenPrivate[{"StakingPending"}, opts]


(* ::Subsection::Closed:: *)
(*KrakenStakingTransactions*)


SyntaxInformation[KrakenStakingTransactions] = {
	"ArgumentsPattern" -> {OptionsPattern[{krakenPrivate}]}, 
	"OptionNames" -> optionNames[krakenPrivate]
}


KrakenStakingTransactions[opts: OptionsPattern[{krakenPrivate}]] := 
krakenPrivate[{"StakingTransactions"}, opts]


(* ::Section::Closed:: *)
(*Kraken web socket rest api method*)


(* ::Subsection::Closed:: *)
(*KrakenGetWebSocketToken*)


SyntaxInformation[KrakenGetWebSocketToken] = {
	"ArgumentsPattern" -> {OptionsPattern[{krakenPrivate}]}, 
	"OptionNames" -> optionNames[krakenPrivate]
}


KrakenGetWebSocketToken[opts: OptionsPattern[krakenPrivate]] := 
krakenPrivate[{"GetWebSocketToken"}, opts]


(* ::Section::Closed:: *)
(*Kraken web socket public channels*)


Options[KrakenChannelCreate] = {
	"Settings" -> getSettings[]
}


KrakenChannelCreate[opts: OptionsPattern[{WebSocketConnect, KrakenChannelCreate}]] := 
Module[{settings, webSocketEndpoint}, 
	settings = OptionValue["Settings"]; 
	webSocketEndpoint = settings["Kraken", "WebSocketEndpoint"]; 
	WebSocketConnect[webSocketEndpoint]
]


(* ::Chapter:: *)
(*Coinbase implementation*)


(* ::Chapter:: *)
(*End private*)


(* ::Section::Closed:: *)
(*End private*)


End[] (* End Private Context *)


(* ::Chapter:: *)
(*End package*)


(* ::Section::Closed:: *)
(*End package*)


EndPackage[] (*ExchangeLink`*)
