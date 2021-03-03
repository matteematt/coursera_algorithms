# Week 14

The implementation works correctly, but uses too much memory to run on the entire dataset.

However, if you plot the points you can realise that points 11 and 12 overlap. You can then split
the dataset into 2 halves, one containing points 0-12 and the second containing 12-23. This computes
almost instantly, you can then add the values you get and then subtract the eucilian distance
between points 11 and 12 twice to account for the fact that you have them once in each sub-tor, but
never in the final tor.
The main method of the program *does not* account for this, it simply calculates the minimum tor
length of the input. So I had to manually split the datasets, run on each one and do the adding and
subtracting and rounding down to get the correct answer.
