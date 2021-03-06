### This is a repository for generating keyness values using R. Keyness is a common calculation in corpus linguistics that compares word frequencies in one corpus (a target corpus) to another (a reference corpus). It is a feature of off-the-shelf concordancers like AntConc and WordSmith. This code has a couple of advantages over using concordancing software. First, it can more efficiently handle larger collections of text. Second, it produces a .csv table that compiles a number of potentially relevant measures by column:

###1) a word list of both corpora
###2 word frequencies for corpus x
###3) word frequencies for corpus y
###4) word frequency percentages (or normalized per 100) for corpus x
###5) word frequency percentages (or normalized per 100) for corpus y
###6) keyness values as calculated by log-likelihood
###7 associated p-values for the keyness values in column 6
###8) the log-ratio as an effect size of log-likelihood
###9) an indictor showing either x>y or x<y for normalized frequencies

###The log-ratio and its purpose is described by Andrew Hardie [here](http://cass.lancs.ac.uk/?p=1133).
###The code produces outputs similar to the online calculator authored by Paul Rayson, which is [here](http://ucrel.lancs.ac.uk/llwizard.html).

###Included in the repository is a corpus of campaign speeches by Clinton and Trump, as well as a sample output file based on that corpus.
###Note that the R code requires the following packages: tm, tm.plugin.webmining, and dplyr. The second of these is used to strip out tagged data between angle brackets that we do not want to be counted.
