# Week 8 Solution

The first way I tried it was the correct solution (`Slow.hs`) but it takes an unreasonably long time
to compute the answer (>4 hours on my machine). Even though is not necessarily in the spirit of the
problem I decided to try and learn how to write parallel code to solve the problem faster
(`MultiThread.hs`)

`MultiThread.hs` does work great, running 4 times faster. This could still be improved a lot.

Another way to do it in future would be to realise that `-10000 <= t <= 10000` and `x+y=t`
=> `-10000 - x <= y <= 10000` and then you can use a binary search to search through the array of
elements to get the lower and upper bounds of the array that satisfies what `y` could be for each
`x`. You would then map these values with `x+y=t` and then store the computed sum in a hashmap
because you only care about `distinct` numbers for sums. The final answer is then simply the number
of values in that hashmap.
