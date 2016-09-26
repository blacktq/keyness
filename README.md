# This is a repository for generating keyness values using R. Keyness is a common caluculation in corpus linguistics that compares word frequencies in one corpus (a target corpus) to another (a reference corpus). It is a feature of off-the-shelf concordancers like AntConc and WordSmith. This code has a couple of advantages over using concordancing software. First, it can more efficiently handle larger collections of text. Second, it produces a .csv table that compliles a number of potentially relevant measures by column:

##1) a word list of both corpora
##2 word frequencies for corpus x
##3) word frequencies for corpus y
##4) word frequency percentages (or normalized per 100) for corpus x
##5) word frequency percentages (or normalized per 100) for corpus y
##6) keyness values as calculated by log-likelihood
##7 associated p-values for the keyness values in column 6
##8) the log-ratio as an effect size of log-likelihood
##9) an indictor showing either x>y or x<y for normalized frequencies
