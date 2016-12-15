From the command line the program can be run like this:

Rscript autocomplete.R baby-names.txt Lynn 6

With output:

      weight     term
3144     116     Lynn
6402      42  Lynnlee
7701      32   Lynnea
10065     22 Lynnette
12110     17    Lynne
13193     15   Lynnae

or it can also handle missing capitalization such as:

Rscript autocomplete.R movies.txt das 6

resulting in:

       weight                                                   term
1132 57058000                                    Dasavatharam (2008)
3951 11487676                                        Das Boot (1981)
3972 11286112                           Das Leben der Anderen (2006)
6163  2281569                        Das schreckliche Mädchen (1990)
6199  2222647 Das weiße Band - Eine deutsche Kindergeschichte (2009)
9816   141072                                  Das Experiment (2001)

The submission also includes one test script for autocomplete, one for just the binary search functions, and one to compare the results of the binary search to linear search. This sources from a script that has the equivalent linear search functions. In addition, there is a script for the performance tests.

Summary of the results from performance_tests.R:

For movies.txt the reading time of the file is around 3.4 sec and therefore much longer, than autocomplete where words required in average 2 milliseconds to be found. In my ten test cases however, this varies between 10 milliseconds for "The" (which has a lot of matches), and 0.6 milliseconds for "Emper" (which probably has very few matches). If a query has a lot of matches the binary search functions iterate until they find they points bounding those matches. Therefore they have more iterations and take longer to run.

The binary search functions take up each about half of the run time of autocomplete. This is expected, and if we would instead perform a linear search, autocomplete would take much longer. Both functions are expected to go with O(log(n)). When plotted over the number of rows in the data set, I find approximately a log function, even though there is considerable variability in the results.
Since the binary search functions are expected to go with O(log(n)), we expect to see something similar for autocomplete, unless there is another operation in autocomplete that dominates the timing. The plot here shows approximately a log function, even though also here there is considerable variability.
The sort by weights function does not take much time if there are not many words that start with the query. The time is increasing with n, the number of query matches, but it is difficult to say what the exact functional form is. For movies, there are many entries that start with "the", so here we might have instances where this sorting takes time of the order of the binary search. 
