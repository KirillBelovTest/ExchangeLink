# ExchangeLink

## Introduction

Hi Wolfram Community! 

We are going to try to see if we can do a community project.
ExchangeLink is an attempt to cover the top 10 crypto exchanges 
with Wolfram Language connectivity.

The start is the Binance example library.

There are several ways to contribute. 

1. Code Editting
	
	* Suggestions on better function names and coding style. 
	* Documentation
	* Tests
	* Examples

2. Create New Libraries
	
	* Follow the Binance Library Example. 

3. Suggestions
	* New Exchange Libraries (see list)
	* Examples

We look forward to seeing how this project goes!

Current Suggestions of exchanges:

Exchanges:

1. Binance (.com, .us)
2. Bitfinex
3. Coinbase
4. Kraken

Futures: 
1. Derbit
2. Bitmex

## Using

1. Open any notebook
2. Evaluate code: <code>PacletInstall["<dowloads/ExchageLink.paclet>"]</code>
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

Check that server is available and get server time. 
This need if you want use trade API. Server time used in the creating of e-sign.

```wolfram
BinancePing[]
BinanceTime[]
```

><||>  
><|"serverTime" -> DateObject[<...>]|>

Information about order limits, precision. 
List of all pairs available on Binance and current statuses. 
This information needs if you want to create own orders. 
Binance has restrictions for aprecision of prices or quantities in orders. 

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

ExchangeInfo cache. `BinanceExchangeInfo[]` is nearly constant data. 
It changes about one time per week. So you can save current value of this variable 
in the session and you can use it without request to binance. Value that saved in memory 
exists during one day. After time 00:00 `$BinanceExchangeInfo` will be updated. 


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

Representation of the order book using `Histogram`:

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
* "15m" - time interval of the one "candle". 
  Available intervals: 1m, 3m, 5m, 15m, 30m, 1h, 2h, 4h, 6h, 12h, 1d
* "limit" -> 96 - number of candles. Max value is 1000, default - 500

Interactive chart with different indicators

```wolfram
InteractiveTradingChart[
	Query[All, {1, {2, 3, 4, 5, 6}}] @ BinanceKlines["BTCUSDT", "15m", "limit" -> 192], 
	ImageSize -> Large, PlotTheme -> "Marketing"
]
```

>![](https://wolfr.am/LdrufmFh)



### API Keys

If you want to use account and trade API endpoint, you must create your 
own API Key and API sercret key. Use following way to do this:

1. Go and login to [binance.com](https://www.binance.com)
2. Click to the menu item "API Managment" in your account actions menu  
![](https://wolfr.am/LA5pbcHc)
3. Click to the button "Create"  
![](https://wolfr.am/LA6N8OcM)
4. Follow the instructions of Binance. You have to conform process in your email

After previous operation you have to copy Keys and save it in a spetial file - 
*.ExchangeLink*. Data in the file must be a Wolfram Association like this: 

```wolfram
<|
	"Binance" -> 
	<|
		"APIKey" -> "JqbsI8dNvn4wQ2tQ00bzChAdIiKVxas98ZMQPxO7Iykr0zrp32mPMOBO2tFbrdw5", 
		"SecretKey" -> "nwVaV8d25Btl9PqSJgks0IUBtZMj44xIdJY2wHceAcGqH3zpagEx9R6mn1G7rbbo"
	|> 
|>
```

This file maybe store in the current working directory or in the 
project directory. If you cloned the project from github - *.ExchangeLink* already 
exists in the project directory. 

### Account

Non zero balances

```wolfram
SortBy[#free&] @ 
Select[#free > 0&] @ 
BinanceAccountInfo[]["balances"]
```

>{<|"asset" -> "BTC", "free" -> 1.0*10^6, "locked" -> 0.0|>}

History of all trades

```wolfram
BinanceMyTrades["BTCUSDT"]
```

>{<|"symbol" -> "BTCUSDT", "id" -> 17056810, "orderId" -> 44470524, 
>  "orderListId" -> -1, "price" -> 8750., "qty" -> 0.013617, 
>  "quoteQty" -> 119.149, "commission" -> 0.0357446, 
>  "commissionAsset" -> "USDT", 
>  "time" -> 
>   DateObject[{2018, 2, 14, 8, 7, 40.198}, "Instant", "Gregorian", 
>    4.], "isBuyer" -> False, "isMaker" -> True, 
>  "isBestMatch" -> True|>, ...}

History of all ordes. Like trades but in this statistics you can see 
what ordes was canceled. 

```wolfram
BinanceOrdersAll["BTCUSDT"]
```

><|"symbol" -> "BTCUSDT", "orderId" -> 44470524, "orderListId" -> -1, 
> "clientOrderId" -> "ItBhjKwekNUlg8wRiLrzFT", "price" -> 8750., 
> "origQty" -> 0.013617, "executedQty" -> 0.013617, 
> "cummulativeQuoteQty" -> 119.149, "status" -> "FILLED", 
> "timeInForce" -> "GTC", "type" -> "LIMIT", "side" -> "SELL", 
> "stopPrice" -> 0., "icebergQty" -> 0., 
> "time" -> 
>  DateObject[{2018, 2, 14, 8, 6, 50.655}, "Instant", "Gregorian", 4.],
>  "updateTime" -> 
>  DateObject[{2018, 2, 14, 8, 7, 40.198}, "Instant", "Gregorian", 4.],
>  "isWorking" -> True, "origQuoteOrderQty" -> 0.|>

### Trade

One of the important part for thi guide - trading. 
Binance exists for it! You can create simple order using following. 
First - get current price of BTC:

```wolfram
price = BinancePrice["BTCUSDT"]["price"]
```

>6808.69

And try to sell BTC 10% more expensive:

```wolfram
BinanceSell["BTCUSDT", 0.001, price * 1.1]
```

><|"symbol" -> "BTCUSDT", "orderId" -> 1739082090, "orderListId" -> -1,
>  "clientOrderId" -> "LbwGlsBinxRtNRsabubXCR", 
> "transactTime" -> 
>  DateObject[{2020, 4, 5, 23, 10, 35.547}, "Instant", "Gregorian", 
>   4.], "price" -> 7487.77., "origQty" -> 0.001, "executedQty" -> 0., 
> "cummulativeQuoteQty" -> 0., "status" -> "NEW", 
> "timeInForce" -> "GTC", "type" -> "LIMIT", "side" -> "SELL", 
> "fills" -> {}|>

Now you can check that order was created. Try to get all opened orders:

```wolfram
orders = BinanceOrdersNow["BTCUSDT"]
```

Get first order info

```wolfram
BinanceOrderGet["BTCUSDT", orders[[1, "orderId"]]]
```

><|"symbol" -> "BTCUSDT", "orderId" -> 1739082090, "orderListId" -> -1,
>  "clientOrderId" -> "LbwGlsBinxRtNRsabubXCR", "price" -> 7487.77., 
> "origQty" -> 0.001, "executedQty" -> 0., 
> "cummulativeQuoteQty" -> 0., "status" -> "NEW", 
> "timeInForce" -> "GTC", "type" -> "LIMIT", "side" -> "SELL", 
> "stopPrice" -> 0., "icebergQty" -> 0., 
> "time" -> 
>  DateObject[{2020, 4, 5, 23, 10, 35.547}, "Instant", "Gregorian", 
>   4.], "updateTime" -> 
>  DateObject[{2020, 4, 5, 23, 10, 35.547}, "Instant", "Gregorian", 
>   4.], "isWorking" -> True, "origQuoteOrderQty" -> 0.|>

If you changed the decision then you can cancel the order:

```wolfram
BinanceOrderCancel["BTCUSDT", orders[[1, "orderId"]]]
```

><|"symbol" -> "BTCUSDT", 
> "origClientOrderId" -> "LbwGlsBinxRtNRsabubXCR", 
> "orderId" -> 1739082090, "orderListId" -> -1, 
> "clientOrderId" -> "x5uDpwDlIcnHn2rdTcZgnR", "price" -> 7484.77., 
> "origQty" -> 0.001, "executedQty" -> 0., 
> "cummulativeQuoteQty" -> 0., "status" -> "CANCELED", 
> "timeInForce" -> "GTC", "type" -> "LIMIT", "side" -> "SELL"|>

## Contributing

After uploading to github