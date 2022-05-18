(* ::Package:: *)

BeginPackage["ExchangeLink`Tools`HMAC`"]


ClearAll["`*"]


HMACSignature::usage = 
"HMACSignature[message, key, method]"


Begin["`Private`"]


hBlockSize["SHA512"] = 128
hBlockSize["SHA384"] = 128
hBlockSize["SHA256"] = 64


stringXor[s_, S_] := 
FromCharacterCode[BitXor[ToCharacterCode[s], ToCharacterCode[S]]];


HMACSignature[key_String, message_String, method: ("SHA512" | "SHA384" | "SHA256")] :=
Block[{
	keyLen = StringLength[key],
	dkey, opad, ipad,
	blocksize, hIn,
	magic36, magic5c
},

	blocksize = hBlockSize[method];

	magic36 = StringJoin[ConstantArray[FromCharacterCode[FromDigits["36", 16]], blocksize]];
	magic5c = StringJoin[ConstantArray[FromCharacterCode[FromDigits["5c", 16]], blocksize]];

	dkey = If[keyLen > blocksize,
		IntegerString[Hash[key, method], 16],
	(*Else*)
		StringJoin[key, Array[FromCharacterCode[0]&, blocksize - StringLength[key]]]
	];

	ipad = stringXor[magic36, dkey];
	opad = stringXor[magic5c, dkey];

	hIn = ExportString[IntegerDigits[Hash[StringJoin[ipad, message], method], 256], "Binary"];

	Return[IntegerString[Hash[StringJoin[opad, hIn], method], 16]]
]


End[]


EndPackage[]