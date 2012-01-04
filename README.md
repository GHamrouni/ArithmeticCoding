ArithmeticCoding
=======================

A naïve implementation of arithmetic coding in Haskell (no scaling, ...)


Using this code
------------------

Create a set of different symbols

    syms = [ (Symbol "A"), (Symbol "B"), (Symbol "C"), (Symbol "D") ]

One must create a probablity function, for each symbol in the alphabet

    p0 :: Symbol -> Int

    p0 (Symbol "A") = 60

    p0 (Symbol "B") = 20

    p0 (Symbol "C") = 10

    p0 (Symbol "D") = 10

An alphabet is a set of different symbols. Each symbol has a probablity of occurence.
	
    alph = Alphabet syms p0

Encode a message 
	
    code = arithmeticEncode alph [0,0,1] (0,1000)
	
Decode a message given a termination symbol

    decoded = arithmeticDecode alph (0,1000) code 1




