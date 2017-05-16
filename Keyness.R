library(tm)
library(dplyr)

#after downloading and unzippling the sample corpus, point tm to the two file folders that contain your target and reference corpora
#specify the relevant path
docs.a <- Corpus(DirSource("~/Politics of English/Clinton-Trump Corpus/Clinton"))
docs.b <- Corpus(DirSource("~/Politics of English/Clinton-Trump Corpus/Trump"))

#create a replace function for processing texts
replace <- content_transformer(stringr::str_replace_all)

#process the files in the first corpus
docs.a = docs.a %>%
  tm_map(replace, "<.*?>", "") %>%
  tm_map(removeNumbers) %>%
  tm_map(tolower) %>%
  tm_map(PlainTextDocument) %>%
  tm_map(replace, "'", " ") %>%
  tm_map(replace, "-", " ") %>%
  tm_map(replace, "\\.", " ") %>%
  tm_map(removePunctuation) %>%
  tm_map(tolower) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace)

#process the files in the second corpus
docs.b = docs.b %>%
  tm_map(replace, "<.*?>", "") %>%
  tm_map(removeNumbers) %>%
  tm_map(tolower) %>%
  tm_map(PlainTextDocument) %>%
  tm_map(replace, "'", " ") %>%
  tm_map(replace, "-", " ") %>%
  tm_map(replace, "\\.", " ") %>%
  tm_map(removePunctuation) %>%
  tm_map(tolower) %>%
  tm_map(removeNumbers) %>%
  tm_map(stripWhitespace)

#create a term matrix
dtm.a <- DocumentTermMatrix(docs.a, control=list(wordLengths=c(1,1000)))
wf.a <- data.frame(word = colnames(dtm.a), freq=colSums(as.matrix(dtm.a))) %>% 
  arrange(desc(freq))

dtm.b <- DocumentTermMatrix(docs.b, control=list(wordLengths=c(1,1000)))
wf.b <- data.frame(word = colnames(dtm.b), freq=colSums(as.matrix(dtm.b))) %>% 
  arrange(desc(freq))

wf <- merge(wf.a,wf.b,by="word", all=TRUE)
wf[is.na(wf)] <- 0

#calculate totals and normalized frequencies
total.a <- sum(wf.a$freq)
total.b <- sum(wf.b$freq)

normalize.a <- function (frequency.a) { 
  normal.a <- (frequency.a/total.a)*100
  return(normal.a)
}

percent.x <- mapply(normalize.a, wf$freq.x)
wf$percent.x <- percent.x

normalize.b <- function (frequency.b) { 
  normal.b <- (frequency.b/total.b)*100
  return(normal.b)
}

percent.y <- mapply(normalize.b, wf$freq.y)
wf$percent.y <- percent.y

#calculate keyness
log.like <- function(frequency.a, frequency.b) { 
  expected.a <- total.a*((frequency.a+frequency.b)/(total.a+total.b))
  expected.b <- total.b*((frequency.a+frequency.b)/(total.a+total.b))
  L1 <- if(frequency.a == 0) 0 else (frequency.a*log(frequency.a/expected.a))
  L2 <- if(frequency.b == 0) 0 else (frequency.b*log(frequency.b/expected.b))
  likelihood <- 2*(L1 + L2)
  return(likelihood)
}

keyness <- mapply(log.like, wf$freq.x, wf$freq.y)
wf$keyness <- keyness

p.value <- mapply ((function(x) pchisq(x,1,lower.tail=FALSE)), wf$keyness)
wf$p.value <- p.value

#calculate log-ratio as an effect size
lr <- function(frequency.a, frequency.b) { 
percent.a <- if(frequency.a == 0) (.5/total.a) else (frequency.a/total.a)
percent.b <- if(frequency.b == 0) (.5/total.b) else (frequency.b/total.b)
ratio <- if (percent.b>percent.a) (log2(percent.b/percent.a)) else (log2(percent.a/percent.b))
return(ratio)
}

log.ratio <- mapply(lr, wf$freq.x, wf$freq.y)
wf$log.ratio <- log.ratio

compare <- function (x,y) {
  if(x > y) return("x>y")
  if(x < y) return("x<y")
  if (x==y) return ("x=y")
}

x.vs.y <- mapply(compare, wf$percent.x, wf$percent.y)
wf$x.vs.y <- x.vs.y

wf <- wf[with(wf, order(-keyness)), ]

#the matrix can now be downloaded to a specified path
#write.csv(wf, file="~/keyness_table.csv")

