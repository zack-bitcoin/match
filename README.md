# match
erlang-style pattern matching in scheme. 

I am writing a blockchain in erlang. Pattern matching is one of the features of erlang that makes it very easy to make blockchains. If I had to re-write the blockchain in scheme, I would use something like this.

I used the SCM implementation of scheme to write this.

Here is an example of how pattern matching looks with this tool:

``
(runget `(((a b (c d)) '(1 2 (3 4)))
	  (e (+ a b c d)))
	'e)))

> 10 ``