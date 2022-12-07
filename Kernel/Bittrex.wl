(* ::Package:: *)

(* ::Title:: *)
(*Bittrex*)


(* ::Section:: *)
(*info block*)


(* :Title: Bittrex *)
(* :Context: CryptoWatcher`DataCollector`Bittrex` *)
(* :Version: 0.0.4 *)
(* :Author: Kirill Belov *)
(* :Description: 
	bittrex api documentation: https://bittrex.com/home/api
*)
(* :Discussion: *)


(* ::Section:: *)
(*package*)


BeginPackage["KirillBelov`ExchangeLink`Bittrex`"]


(* ::Section:: *)
(*name clearing*)


Unprotect["`*"]
ClearAll["`*"]


(* ::Section:: *)
(*declaration of public names*)


(* ::Section:: *)
(*private context*)


Begin["`Private`"]


(* ::Section:: *)
(*internal variables*)


$bittrexURL = 
	"https://bittrex.com/api/v1.1"; 


(* ::Section:: *)
(*public functions implementation*)


(* ::Text:: *)
(*bittrex api methods implementation*)


(* ::Section:: *)
(*end private*)


End[] (*`Private`*)


(* ::Section:: *)
(*from change protection*)


Protect["`*"]; 


(* ::Section:: *)
(*end*)


EndPackage[]; (*CryptoWatcher`DataCollector`Bittrex`*)
