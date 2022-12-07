(* ::Package:: *)

BeginPackage["KirillBelov`ExchangeLink`Internal`HMAC`"]


HMAC::usage = 
"HMAC[data, key]
HMAC[data, secretKey, method]"


Begin["`Private`"]


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


End[] (*`Private`*)


EndPackage[] (*KirillBelov`ExchangeLink`Internal`HMAC`*)
