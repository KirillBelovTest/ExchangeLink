BeginTestSection["BinanceGeneralEndpointTests"]

VerificationTest[(* 1 *)
	Get["ExchangeLink`"]
	,
	Null	
	,
	TestID->"Get the package"
]

VerificationTest[(* 2 *)
	BinancePing[]
	,
	Association[]	
	,
	TestID->"Binance ping"
]

VerificationTest[(* 3 *)
	BinanceTime[]["serverTime"]
	,
	Now	
	,
	SameTest->(Round@AbsoluteTime@#1 === Round@AbsoluteTime@#2&), TestID->"Check Binance time"
]

VerificationTest[(* 4 *)
	BinanceExchangeInfo[]
	,
	Association[Rule["timezone", "UTC"], Rule["serverTime", Blank[DateObject]], Rule["rateLimits", List[BlankSequence[Association]]], Rule["exchangeFilters", List[]], Rule["symbols", List[Repeated[Association[Rule["symbol", Blank[String]], BlankSequence[Rule]]]]]]	
	,
	SameTest->(MatchQ[#1,  #2]&), TestID->"Check Binance server time"
]

EndTestSection[]
