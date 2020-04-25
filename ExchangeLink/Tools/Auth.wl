(* ::Package:: *)

(* ::Title:: *)
(*Auth*)


(* ::Section:: *)
(*Info*)


(* ::Section:: *)
(*Begin package*)


BeginPackage["ExchangeLink`Tools`Auth`"]


(* ::Section:: *)
(*Clear names*)


Unprotect["`*"]


ClearAll["`*"]


(* ::Section:: *)
(*Public names declaration*)


ExchangeLinkIniRead::usage = 
"ExchangeLinkIniRead[section, key]
ExchangeLinkIniRead[section, key, \"File\" -> \"~/.ExchangeLink.wl\"]"


$ExchangeLinkConfig::usage = 
"$ExchangeLinkConfig[\"Binance\", \"Domain\"] = \"binance.com\""


ExchangeLinkHMAC::usage = 
"ExchangeLinkHMAC[key, message, method]"


ExchangeLinkNonce::usage = 
"ExchangeLinkNonce[folder, key]"


(* ::Section:: *)
(*Begin private context*)


Begin["`Private`"]


(* ::Section:: *)
(*Public functions implementation*)


(* ::Subsubsection:: *)
(*ExchangeLinkIniRead*)


$ExchangeLinkConfig := 
	(Unprotect[$ExchangeLinkConfig]; 
	$ExchangeLinkConfig = 
	Block[{path = FileNameJoin[{$HomeDirectory, ".ExchangeLink", "Config"}]}, 
		If[Not[AssociationQ[LocalSymbol[File[path]]]], 
			LocalSymbol[File[path]] = <|
				"Binance" -> <|"Domain" -> "binance.us", "APIKey" -> Automatic, "SecretKey" -> Automatic|>
			|>
		];
		LocalSymbol[File[path]]
	]; 
	Protect[$ExchangeLinkConfig]; 
	$ExchangeLinkConfig)


$ExchangeLinkConfig /: 
Set[$ExchangeLinkConfig["Binance", key: "Domain" | "APIKey" | "SecretKey"], value_String] := 
	Block[{copy = $ExchangeLinkConfig, path = FileNameJoin[{$HomeDirectory, ".ExchangeLink", "Config"}]}, 
		copy["Binance", key] = value; 
		Unprotect[$ExchangeLinkConfig];
		$ExchangeLinkConfig = copy;
		LocalSymbol[File[path]] = $ExchangeLinkConfig;
		Protect[$ExchangeLinkConfig];
		
		Return[value]
	]


(* ::Subsubsection:: *)
(*ExchangeLinkHMAC*)


ExchangeLinkHMAC[key_String, message_String, method: ("SHA512" | "SHA256")] :=
	Block[{
		keyLen = StringLength[key],
		hBlockSize, 
		dkey, opad, ipad,
		stringXor, blocksize, hIn,
		magic36, magic5c, hash
	},

		hBlockSize["SHA512"] = 128;
		hBlockSize["SHA256"] = 64;

		hash = If[$VersionNumber >= 11.3, Developer`LegacyHash, Hash];

		blocksize = hBlockSize[method];

		magic36 = StringJoin[ConstantArray[FromCharacterCode[FromDigits["36", 16]], blocksize]];
		magic5c = StringJoin[ConstantArray[FromCharacterCode[FromDigits["5c", 16]], blocksize]];

		dkey =
			If[keyLen > blocksize,
				IntegerString[hash[key, method], 16],
			(*Else*)
				StringJoin[key, Array[FromCharacterCode[0]&, blocksize - StringLength[key]]]
			];

		stringXor[s_, S_] := FromCharacterCode[BitXor[ToCharacterCode[s], ToCharacterCode[S]]];

		ipad = stringXor[magic36, dkey];

		opad = stringXor[magic5c, dkey];

		hIn = ExportString[IntegerDigits[hash[StringJoin[ipad, message], method], 256], "Binary"];

		(*Return*)
		IntegerString[hash[StringJoin[opad, hIn], method], 16]
	];


(* ::Subsubsection:: *)
(*ExchangeLinkNonce*)


ExchangeLinkNonce[folder_String, key_String] := 
    Block[{path, file}, 
		path = FileNameJoin[{$HomeDirectory, ".ExchangeLink", "Nonce", folder, key}];
		file = File[path]; 
		
		If[Not[IntegerQ[LocalSymbol[file]]], LocalSymbol[file] = 0];
		If[IntegerQ[LocalSymbol[file]] && LocalSymbol[file] >= 0, LocalSymbol[file] = LocalSymbol[file] + 1];
		
		Return[LocalSymbol[file]]
	];


ExchangeLinkNonce /: 
Set[ExchangeLinkNonce[folder_, key_], value_Integer] /; 
StringQ[folder] && StringQ[key] && ExchangeLinkNonce[folder, key] < value := 
	With[{folder0 = folder, key0 = key}, 
		Block[{path, file}, 
			file = File[path]; 
			path = FileNameJoin[{$HomeDirectory, ".ExchangeLink", "Nonce", folder0, key0}]; 
			
			LocalSymbol[file] = value
		]
	];


(* ::Section:: *)
(*End private context*)


End[]


(* ::Section:: *)
(*From change protection*)


Protect["`*"]


(* ::Section:: *)
(*End package*)


EndPackage[] (*ExchangeLink`Tools`Auth`*)
