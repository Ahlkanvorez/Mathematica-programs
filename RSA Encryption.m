(* Assumption: p_ and q_ are distinct large primes. *)

(* a random integer e coprime to n where a < e < b *)

RandomCoprimeInteger[n_, {a0_, b0_}] :=
  
  With[{a = a0 + 1, b = b0 - 1},
   e = RandomInteger[{a, b}];
   While[GCD[e, n] > 1, e = RandomInteger[{a, b}]];
   e];

(* Produces a pair {{n, e}, {n, d}} for a given pair of primes p, q, \
which are the public and private keys respectively. *)

RSAKeys[p_, q_] :=
  
  Module[{n = p q, (* n is the modulus for public & private keys *)
  
      totient = (p - 1) (q - 1)(* Euler's totient function \[Phi](n), 
    the number of integers less than n relatively prime to n. *)
    },
   (* d is the private key exponent. *)
   
   d = RandomPrime[{Max[p, q] + 1, p q}];
   (* e is the public key exponent. *)
   e = PowerMod[d, -1, totient];
   {{n, e}, {n, d}}];

(* M is an integer to be encrypted. *)

RSAEncrypt[M_, {n_, e_}] := PowerMod[M, e, n];

(* C is the cypher-text to be decrypted. *)

RSADecrypt[C_, {n_, d_}] := PowerMod[C, d, n];

(* Divides a string s into a list of substrings of length n. *)

StringChunk[s_, n_] := 
  If[StringLength[s] <= n, {s},
   With[{head = StringTake[s, n], tail = StringDrop[s, n]},
    If[StringLength[tail] == 0,
     {head},
     Join[{head}, StringChunk[tail, n]]]]];
(* Given a string, this function returns an integer representing the \
concatenation of the hex-values of the character codes for each \
character. *)

EncodeString[s_] := 
  FromDigits[StringJoin[IntegerString[#, 16] & /@ ToCharacterCode[s]],
    16];
DecodeString[s_] :=
  
  FromCharacterCode[
   FromDigits[#, 16] & /@ StringChunk[IntegerString[s, 16], 2]];

Clear[keys, M, c, p, q]
p = RandomPrime[{2^256, 2^512}];
q = RandomPrime[{2^256, 2^512}];
keys = RSAKeys[p, q];
M = "This is a secret message that you shouldn't be reading. If you \
can read this, we're in big trouble. This is some more text that \
doesn't really say anything at all other than saying it doesn't say \
anything. But if it says it doesn't say anything, then it is saying \
something; therefore it's false.";

EncodeString /@ StringChunk[M, 16];
c = (RSAEncrypt[#, keys[[1]]] & /@ (EncodeString /@ 
      StringChunk[M, 16]));
StringJoin[DecodeString /@ c]

StringJoin[DecodeString /@ (RSADecrypt[#, keys[[2]]] & /@ c)]

keys[[1]]
