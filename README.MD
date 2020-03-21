# ExchangeLink

## Introduction

In this section Jon has to write about the project. 

More about the project - goals

- general architecture
- API implementation of top exchanges
- all available methods
- using native types (for example converting UTC to DateObject)

## Using

1. Open any notebook
2. Evaluate code: <code>PacletInstall["<path/to/file.paclet>"]</code> <!--not uploaded-->
3. Evaluate code: <code>Get["ExchangeLink`"]</code>
4. All functions are available

## Binance

### Information

After initialization Binance API methods are available in the current notebook. 
You can see list of functions/methods using following command:

```wolfram
?ExchangeLink`Binance`*
```

>|                             |                          |                          |
>| --------------------------- | ------------------------ | ------------------------ |
>| **BinanceAccountInfo**      | **BinanceOCOrderCancel** | **BinanceOrderTest**     |
>| **BinanceAggTrades**        | **BinanceOCOrderCreate** | **BinancePing**          |
>| **BinanceAveragePrice**     | **BinanceOCOrderGet**    | **BinancePrice**         |
>| **BinanceBookTicker**       | **BinanceOCOrdersAll**   | **BinanceSell**          |
>| **BinanceBuy**              | **BinanceOCOrdersNow**   | **BinanceTicker**        |
>| **BinanceDepth**            | **BinanceOrderCancel**   | **BinanceTime**          |
>| **BinanceExchangeInfo**     | **BinanceOrderCreate**   | **BinanceTrades**        |
>| **BinanceHistoricalTrades** | **BinanceOrderGet**      | **$BinanceExchangeInfo** |
>| **BinanceKlines**           | **BinanceOrdersAll**     |                          |
>| **BinanceMyTrades**         | **BinanceOrdersNow**     |                          |

### General data

General information from Binance server.

Check that server is available and get server time. This need if you want use trade API. Server time used in the creating of e-sign.

```wolfram
BinancePing[]
BinanceTime[]
```

><||>  
><|"serverTime" -> DateObject[<...>]|>

Information about order limits, precision. List of all pairs available on Binance and current statuses

```wolfram
BinanceExchangeInfo[]
```

><| 
>   "timezone" -> "UTC", 
>   ..., 
>   "symbols" -> { 
>       <|"symbol" -> "ETHBTC", ..., "filters" -> {...}|>, 
>       ... 
>   } 
>|> 

ExchangeInfo cache 

```wolfram
Dataset @ 
Query[All, <|
	"symbol" -> "symbol", 
	"minPrice" -> "filters" /* 1 /* 2, 
	"maxPrice" -> "filters" /* 1 /* 3, 
	"tickSize" -> "filters" /* 1 /* 4, 
	"minQty" -> "filters" /* 3 /* 2, 
	"maxQty" -> "filters" /* 3 /* 3, 
	"stepSize" -> "filters" /* 3 /* 4|>] @ 
$BinanceExchangeInfo["symbols"]
```

>| symbol  | minPrice | maxPrice | tickSize | minQty  | maxQty  | stepSize |
>| ------- | -------- | -------- | -------- | ------- | ------- | -------- |
>| ETHBTC  | 1.e-6    | 100000.  | 1.e-6    | 0.001   | 100000. | 0.001    |
>| LTCBTC  | 1.e-6    | 100000.  | 1.e-6    | 0.01    | 100000. | 0.01     |
>| BNBBTC  | 1.e-7    | 100000.  | 1.e-7    | 0.01    | 100000. | 0.01     |
>| NEOBTC  | 1.e-6    | 100000.  | 1.e-6    | 0.01    | 100000. | 0.01     |
>| QTUMETH | 1.e-6    | 1000.    | 1.e-6    | 0.01    | 9.e7    | 0.01     |
>| EOSETH  | 1.e-6    | 1000.    | 1.e-6    | 0.01    | 9.e7    | 0.01     |
>| SNTETH  | 1.e-8    | 1000.    | 1.e-8    | 1.      | 9.e7    | 1.       |
>| BNTETH  | 1.e-6    | 1000.    | 1.e-6    | 0.01    | 9.e7    | 0.01     |
>| BCCBTC  | 1.e-6    | 100000.  | 1.e-6    | 0.001   | 100000. | 0.001    |
>| GASBTC  | 1.e-7    | 1000.    | 1.e-7    | 0.01    | 9.e7    | 0.01     |
>| BNBETH  | 1.e-6    | 1000.    | 1.e-6    | 0.01    | 9.e7    | 0.01     |
>| BTCUSDT | 0.01     | 1.e6     | 0.01     | 1.e-6   | 9000.   | 1.e-6    |
>| ETHUSDT | 0.01     | 1.e6     | 0.01     | 0.00001 | 9000.   | 0.00001  |
>| HSRBTC  | 1.e-6    | 100000.  | 1.e-6    | 0.01    | 1.e7    | 0.01     |
>| OAXETH  | 1.e-7    | 100000.  | 1.e-7    | 1.      | 9.e7    | 1.       |
>| DNTETH  | 1.e-8    | 1000.    | 1.e-8    | 1.      | 9.e7    | 1.       |
>| MCOETH  | 1.e-6    | 1000.    | 1.e-6    | 0.01    | 9.e7    | 0.01     |
>| ICNETH  | 1.e-7    | 100000.  | 1.e-7    | 1.      | 9.e7    | 1.       |
>| MCOBTC  | 1.e-7    | 1000.    | 1.e-7    | 0.01    | 9.e7    | 0.01     |
>| WTCBTC  | 1.e-7    | 1000.    | 1.e-7    | 0.01    | 9.e7    | 0.01     |

### Market data

Current order book

```wolfram
depth = BinanceDepth["BTCUSDT"];
bids = depth["bids"]; 
asks = depth["asks"]; 
min = Min[bids[[All, 2]] ~ Join ~ asks[[All, 2]]]; 
bidNormalizaer = Round[bids[[All, 2]] / min];
askNormalizaer = Round[asks[[All, 2]] / min];
```

```wolfram
PairedHistogram[
	Flatten[Table[ConstantArray[bids[[i, 1]], bidNormalizaer[[i]]], {i, 1, Length@bidNormalizaer}]], 
	Flatten[Table[ConstantArray[asks[[i, 1]], askNormalizaer[[i]]], {i, 1, Length@askNormalizaer}]], 
	ImageSize -> Large, Frame -> True, 
	ChartLabels -> {
		Placed[{"BIDS", "ASKS"}, Above],
		Placed[{"r1", "r2"}, Tooltip]
	}, 
	LabelingFunction -> (If[!(#1 == 0), Placed[#1*min, Center], None]&), 
	FrameTicks -> {None, Automatic}
]
```

>![](https://wolfr.am/LdqAAGTu)

Last trades for the pair - order size, time and other

```wolfram
BinanceTrades["BTCUSDT"]
```

>{<|"id" -> 275374890, price -> 6232.51, qty -> quoteQty -> 267.026, time -> DateObject[<..>], ...|>, ...}

### Market prices

Get current price of Bitcoin

```wolfram
BinancePrice["BTCUSDT"]
```

><|"symbol" -> "BTCUSDT", "price" -> 6231.45|>

More information about cryptocurrency pair. Result has information about 24h change, 
volume and last prices

```wolfram
BinanceTicker["BTCUSDT"]
```

><|"symbol" -> "BTCUSDT", "priceChange" -> -287.64, 
> "priceChangePercent" -> -4.405, "weightedAvgPrice" -> 6207.84, 
> "prevClosePrice" -> 6529.23, "lastPrice" -> 6242.19, 
> "lastQty" -> 0.150057, "bidPrice" -> 6241.39, "bidQty" -> 0.083302, 
> "askPrice" -> 6243.39, "askQty" -> 1.71051, "openPrice" -> 6529.83, 
> "highPrice" -> 6714.53, "lowPrice" -> 5670., "volume" -> 182195., 
> "quoteVolume" -> 1.1310340689753455*10^9, 
> "openTime" -> 
>  DateObject<>, "closeTime" -> 
>  DateObject<>, "firstId" -> 274022160, "lastId" -> 275354648, 
> "count" -> 1332489|>

And for all currency pairs

```wolfram
BinanceTicker[]
```

>{<|"symbol" -> "ETHBTC", "priceChange" -> -0.00007, 
> "priceChangePercent" -> -0.327, "weightedAvgPrice" -> 0.0213236, 
> "prevClosePrice" -> 0.021413, "lastPrice" -> 0.021351, 
> "lastQty" -> 1.204, "bidPrice" -> 0.021351, <...>, 
> "volume" -> 312333., "quoteVolume" -> 6660.05, 
> "openTime" -> 
>  DateObject<>, "closeTime" -> 
>  DateObject<>, "firstId" -> 169520504, "lastId" -> 169718743, 
> "count" -> 198240|>, < .. 767 ..>}

### Chart

Get historical data from Binance

* "BTCUSDT" - cryptocurrency pair
* "15m" - time interval of the one "candle". Available intervals: 1m, 3m, 5m, 15m, 30m, 1h, 2h, 4h, 6h, 12h, 1d
* "limit" -> 96 - number of candles. Max value is 1000, default - 500

Interactive chart with different indicators

```wolfram
InteractiveTradingChart[
	Query[All, {1, {2, 3, 4, 5, 6}}] @ BinanceKlines["BTCUSDT", "15m", "limit" -> 192], 
	ImageSize -> Large, PlotTheme -> "Marketing"
]
```

>![](https://wolfr.am/LdrufmFh)

### Account

### Trade

## Contributing

After uploading to github

