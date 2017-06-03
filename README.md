# whyfp-scala

This is an attempt at reimplementing code examples from "Why Functional Programming Matters" by John Hughes in Scala.

I have implemented examples from chapters 3 and 4. Decided to skip chapter 5 because there's not enough context.

Notes:
* Generally a majority of the code is a 1 to 1 rewrite
* At some point I noticed that writing functions that take multiple parameter lists gives even better results because this feature of Haskell/Miranda is heavily relied on in the paper
* I had to play around with `Cons` / `List` object to help with a) laziness b) `apply/unapply`
