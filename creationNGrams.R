require(knitr)
require(stringi)
library(data.table)


# The https method does not work in all machines and so I have replaced it with http. It is only necessary to download this file once. So we check to see if the file already exists, and in that case we skip the download . 

blogs <-readLines("./final/en_US/en_US.blogs.txt", encoding = "UTF-8", skipNul=TRUE)
news <- readLines("./final/en_US/en_US.news.txt", encoding = "UTF-8", skipNul=TRUE)
twitter <- readLines("./final/en_US/en_US.twitter.txt", encoding = "UTF-8", skipNul=TRUE)


textSource = c(blogs, news, twitter)
format(object.size(textSource), units="GB")
#save(textSource, file = "./backupData/textsource.RData")


filteredWordsFile = "./data/badwords.txt"
wordsToRemove = read.table(filteredWordsFile, stringsAsFactors = FALSE)
profanityList = as.character(wordsToRemove[, 1])


set.seed(2015)
############### Atencion
p = 0.80
###############

numSampleLines = round(length(textSource) * p, 0)
sampledLines = sort(sample(1:length(textSource), numSampleLines, replace = FALSE))

textData = textSource[sampledLines]
#save("textData", file = "./data/textData.RData")

nonSampledLines = (1:length(textSource))[-sampledLines]

p2 = 0.50
numHeldOutLines = round(length(nonSampledLines) * p2, 0)

heldOutLines = sort(sample(nonSampledLines, numHeldOutLines, replace = FALSE))
sum(heldOutLines %in% sampledLines)

testLines = (1:length(textSource))[-c(sampledLines, heldOutLines)]
sum(testLines %in% heldOutLines)
sum(testLines %in% sampledLines)


sum(sapply(list(sampledLines, heldOutLines, testLines), length))
length(textSource)

heldOutText = textSource[heldOutLines]


#############################################################################
#############################################################################
#############################################################################
## Processing sampled text lines
#############################################################################
#############################################################################
#############################################################################

sentences = stri_split_boundaries(textData, type="sentence")
sentences = unlist(sentences)
names(sentences) = NULL
head(sentences, 40)
rm(textData)
sentences = tolower(sentences)
sentences = paste(" BS ", sentences)
names(sentences) = NULL
head(sentences, 20)

#############################################################################
## From sentences to words
#############################################################################

listWords = stri_extract_all_words(sentences)
listWords = unlist(listWords)
head(listWords, 200)
rm(sentences)


#############################################################################
## Detecting years, numbers 
#############################################################################


regexYEAR = "^[12]{1}[0-9]{3}$"
listWords = stri_replace_all(listWords, replacement = "YEAR", regex = regexYEAR)

regexNUM = "^[[:digit:]]+(,[[:digit:]]*)*\\.*[[:digit:]]*$"
listWords = stri_replace_all(listWords, replacement = "NUM", regex = regexNUM)

names(listWords) = NULL
head(listWords, 2000)

#############################################################################
## Removing punctuation
#############################################################################

punctuationRegex = "[\\[\\]\\$\\*\\+\\.\\?\\^\\{\\}\\|\\(\\)\\#%&~_/<=>✬!,:;❵@]"
listWords = stri_replace_all(listWords, replacement = "", regex = punctuationRegex)
listWords = stri_replace_all(listWords, replacement = "'", fixed = "’")


#############################################################################
## Processing non-English characters
#############################################################################

nonEnglishChars = "[^A-Za-z'-[:space:]]"
listWords = stri_replace_all(listWords, replacement = "", regex = nonEnglishChars)

#############################################################################
## Removing empty words
#############################################################################

emptyWords = (listWords == "")|(listWords=="'")
listWords = listWords[!emptyWords]
rm(emptyWords)


#############################################################################
#############################################################################
#############################################################################
## Creating a data table with the preprocessed data
#############################################################################
#############################################################################
#############################################################################

wordsDT = data.table(x = listWords)
wordsDT
rm(listWords)

#############################################################################
## Adding the counts
#############################################################################

wordsDT[ , cX:=.N, by=x]
wordsDT
format(object.size(wordsDT), units="Mb")


wordsDTRemove = data.table(x = wordsToRemove$V1)
wordsDTRemove
wordsDTRemove[ , cX:=.N, by=x]
format(object.size(wordsDTRemove), units="Mb")
#############################################################################
## Creating a dictionary of numerical codes for words
#############################################################################

codes = copy(wordsDT)
nrow(codes)
codes = unique(codes)
nrow(codes)

setkey(codes, cX)
codes[ , code:= nrow(codes):1]
codes
setkey(codes, x)

codes2Remove = copy(wordsDTRemove)
nrow(codes2Remove)
codes2Remove = unique(codes2Remove)
nrow(codes2Remove)

setkey(codes2Remove, cX)
codes2Remove[ , code:= nrow(codes2Remove):1]
codes2Remove
setkey(codes2Remove, x)
#############################################################################
## and the functiosn to translate from word to code and back
#############################################################################

word2code = function(w){
  l = list(w)
  codes[l]$code
}

code2word = function(a){
  where = sapply(a, function(x)which(codes$code == x))
  codes[where]$x
}

#############################################################################
## Do no set a key for this data.table, r the sentence order will be destroyed
wordsByCodeDT = copy(wordsDT) 
## Do no set a key for this data.table, r the sentence order will be destroyed
#############################################################################
wordsByCodeDT[ , codeX:= word2code(x)]

wordsByCodeDT[ , x:=NULL]
setnames(wordsByCodeDT, old = "codeX", new = "x")
setcolorder(wordsByCodeDT, neworder = c("x", "cX"))
wordsByCodeDT

## N-gram model construction

```{r}

#############################################################################
#############################################################################
## Unigrams
#############################################################################
#############################################################################

unigrams=copy(wordsByCodeDT)
format(object.size(unigrams), units="Mb")
head(unigrams, 50) 
setkey(unigrams, x)
unigrams = unique(unigrams)
format(object.size(unigrams), units="Mb")
nrow(unigrams)
unigrams = unigrams[x != word2code("BS")]
(numUnigrams = sum(unigrams$cX))
nrow(unigrams)
head(table(unigrams$cX))
# lowFreq = 0
# unigrams = unigrams[cX > lowFreq]
nrow(unigrams)
head(unigrams)
unigrams[ , pX:= cX/numUnigrams]
unigrams
sum(unigrams$pX)
##unigrams[ , x:=NULL]
unigrams


#############################################################################
#############################################################################
## Bigrams
#############################################################################
#############################################################################


rotateNum = function(x) (1:x %% x) +1 
rotateVec = function(vec) vec[rotateNum(length(vec))] 

bigrams = data.table(
  x = wordsByCodeDT$x, 
  cX = wordsByCodeDT$cX,
  y= rotateVec(wordsByCodeDT$x),
  cY= rotateVec(wordsByCodeDT$cX)
)

r = 8
code2word(c(bigrams[r]$x, bigrams[r]$y))
r = 34
code2word(c(bigrams[r]$x, bigrams[r]$y))
r = 41
code2word(c(bigrams[r]$x, bigrams[r]$y))


setkey(bigrams, x, y)
head(bigrams,20)
# bigrams = bigrams[x != word2code("BS")] 
bigrams = bigrams[y != word2code("BS")] 
head(bigrams, 20)
bigrams[, cXY:= .N, by = .(x, y)]
bigrams
nrow(bigrams)
bigrams = unique(bigrams)
nrow(bigrams)
bigrams
(numBigrams = sum(bigrams$cXY))
format(object.size(bigrams), units="Mb")
#nrow(bigrams)
#head(table(bigrams$cXY))
#format(object.size(bigrams), units="Mb")

#############################################################################

library(edgeR)
GT2 = goodTuring(bigrams$cXY)
sum(GT2$proportion * GT2$n) + GT2$P0
setkey(bigrams, cXY)

freqGT2 = rep(GT2$proportion, GT2$n)
length(freqGT2)
nrow(bigrams)

bigrams[ , dcXY:= freqGT2 * numBigrams]
rm(freqGT2)

sum(bigrams$cXY) - sum(bigrams$dcXY)

setkey(unigrams, x)
setkey(bigrams, x)

setkey(bigrams, x)
bigrams
unigrams

bigrams[ , pXY:= dcXY/cX]

bigrams
setkey(bigrams, x, y)
bigrams[ , xy:=paste(x, y, sep = " ")]
bigrams[ , beta1:= 1 - sum(pXY), by=x]
bigrams

bigrams[ , alpha1:= beta1/(1 - sum(pY1g)), by=x]
bigrams

# Check for the betas
sum(bigrams[x==word2code("the")]$pXY) + bigrams[x==word2code("the")]$beta1[1]
sum(bigrams[x==word2code("party")]$pXY) + bigrams[x==word2code("party")]$beta1[1]


setkey(bigrams, x, y)
bigrams[ , codeBigram:= 1:nrow(bigrams)]
bigrams

bigram2code = function(x0, y0){
  l = list(x =x0, y=y0)
  bigrams[l]$codeBigram
}

code2bigram = function(a){
  where = sapply(a, function(x)which(bigrams$codeBigram == x))
  return(unlist(bigrams[where, .(x, y)]))
}

code2bigram(1520635)
bigram2code(code2bigram(145327)[1], code2bigram(145327)[2])

#############################################################################
#############################################################################
## Trigrams
#############################################################################
#############################################################################

trigrams = data.table(
  x = wordsByCodeDT$x, 
  cX = wordsByCodeDT$cX,
  y = rotateVec(wordsByCodeDT$x),
  cY = rotateVec(wordsByCodeDT$cX),
  z=  rotateVec(rotateVec(wordsByCodeDT$x)),
  cZ= rotateVec(rotateVec(wordsByCodeDT$cX))  
)

r = 8
code2word(c(trigrams[r]$x, trigrams[r]$y, trigrams[r]$z))
r = 34
code2word(c(trigrams[r]$x, trigrams[r]$y, trigrams[r]$z))
r = 41
code2word(c(trigrams[r]$x, trigrams[r]$y, trigrams[r]$z))

trigrams

setkey(trigrams, x, y, z)

trigrams[, cXYZ:= .N, by = .(x, y, z)]

# head(trigrams)

trigrams = trigrams[y != word2code("BS")] 
trigrams = trigrams[z != word2code("BS")] 

nrow(trigrams)
trigrams = unique(trigrams)
nrow(trigrams)
# head(trigrams)
(numTrigrams = sum(trigrams$cXYZ))
format(object.size(trigrams), units="Mb")
nrow(trigrams)
head(table(trigrams$cXYZ))

#############################################################################

GT3 = goodTuring(trigrams$cXYZ)
sum(GT3$proportion * GT3$n) + GT3$P0
head(trigrams)
setkey(trigrams, cXYZ)
# head(trigrams, 50)

pGT3 = rep(GT3$proportion, GT3$n)
# length(pGT3)
# nrow(trigrams)
rm(pGT3)

freqGT3 = rep(GT3$proportion, GT3$n)
# length(freqGT3)
# nrow(trigrams)


trigrams[, dcXYZ:= freqGT3 * numTrigrams]
# head(trigrams, 50)


rm(freqGT3)


sum(trigrams$cXYZ) - sum(trigrams$dcXYZ)

#############################################################################
#############################################################################
#############################################################################
## Bigram information in trigrams table
#############################################################################
#############################################################################
#############################################################################

setkey(trigrams, x, y)
setkey(bigrams, x, y)
trigrams[ , xy2gCode:= bigram2code(x, y)]

setkey(bigrams, codeBigram)
setkey(trigrams, x, y)
trigrams[ , cXY2g:=bigrams[.(xy2gCode)]$cXY]
setkey(bigrams, x, y)

setkey(trigrams, x,y,z)
trigrams[ , pXYZ:= dcXYZ/cXY2g]
trigrams

trigrams[ , beta2g:= 1 - sum(pXYZ), by=xy2gCode]
trigrams

trigrams[ , alpha2g:= beta2g/(1 - sum(pYZ2g)), by=xy2gCode]
trigrams

format(object.size(trigrams), units="Mb")

# Check for the betas
sum(trigrams[x==word2code("the")][y==word2code("party")]$pXYZ) + trigrams[x==word2code("the")][y==word2code("party")]$beta2g[1]

#############################################################################

setkey(trigrams, x, y, z)
trigrams[ , codeTrigram:= 1:nrow(trigrams)]
trigrams

trigram2code = function(x0, y0, z0){
  l = list(x =x0, y=y0, z=z0)
  trigrams[l]$codeTrigram
}

(x0 = word2code("at"))
(y0 = word2code("the"))
(z0 = word2code("party"))
trigram2code(x0, y0, z0)
trigrams[trigram2code(x0, y0, z0)]


code2trigram = function(a){
  where = sapply(a, function(x)which(trigrams$codeTrigram == x))
  return(unlist(trigrams[where, .(x, y, z)]))
}

code2trigram(1520635)
trigram2code(code2trigram(1520635)[1], code2trigram(1520635)[2], code2trigram(1520635)[3])

#############################################################################
#############################################################################
## Fourgrams
#############################################################################
#############################################################################

fourgrams = data.table(
  x = wordsByCodeDT$x, 
  cX = wordsByCodeDT$cX,
  y = rotateVec(wordsByCodeDT$x),
  cY = rotateVec(wordsByCodeDT$cX),
  z =  rotateVec(rotateVec(wordsByCodeDT$x)),
  cZ = rotateVec(rotateVec(wordsByCodeDT$cX)),
  t =  rotateVec(rotateVec(rotateVec(wordsByCodeDT$x))),
  cT = rotateVec(rotateVec(rotateVec(wordsByCodeDT$cX)))
)

r = 8
code2word(c(fourgrams[r]$x, fourgrams[r]$y, fourgrams[r]$z, fourgrams[r]$t))

r = 34
code2word(c(fourgrams[r]$x, fourgrams[r]$y, fourgrams[r]$z, fourgrams[r]$t))

r = 41
code2word(c(fourgrams[r]$x, fourgrams[r]$y, fourgrams[r]$z, fourgrams[r]$t))

setkey(fourgrams, x, y, z, t)

fourgrams[, cXYZT:= .N, by = .(x, y, z, t)]

fourgrams = fourgrams[y != word2code("BS")] 
fourgrams = fourgrams[z != word2code("BS")] 
fourgrams = fourgrams[t != word2code("BS")] 


nrow(fourgrams)
fourgrams = unique(fourgrams)
nrow(fourgrams)
# head(trigrams)
(numFourgrams = sum(fourgrams$cXYZT))

format(object.size(fourgrams), units="Mb")

nrow(fourgrams)
head(table(fourgrams$cXYZT))

#############################################################################

GT4 = goodTuring(fourgrams$cXYZT)
sum(GT4$proportion * GT4$n) + GT4$P0
setkey(fourgrams, cXYZT)

freqGT4 = rep(GT4$proportion, GT4$n)

fourgrams[, dcXYZT:= freqGT4 * numFourgrams]
rm(freqGT4)


sum(fourgrams$cXYZ) - sum(fourgrams$dcXYZT)

#############################################################################
#############################################################################
#############################################################################
## Trigram information in fourgrams table
#############################################################################
#############################################################################
#############################################################################

setkey(fourgrams, x, y, z)
setkey(trigrams, x, y, z)

fourgrams[ , xyz3gCode:= trigram2code(x, y, z)]

trigrams[.(123, 32, 27)]
fourgrams[.(123, 32, 27)]

setkey(fourgrams, y, z, t)
setkey(trigrams, x, y, z)

fourgrams[ , yzt3gCode:= trigram2code(y, z, t)]

trigrams[.(123, 32, 27)]
fourgrams[.(123, 32, 27)]


setkey(trigrams, codeTrigram)
setkey(fourgrams, x, y, z)
fourgrams[ , cXYZ3g:=trigrams[.(xyz3gCode)]$cXYZ]
setkey(trigrams, x, y, z)
trigrams[.(123, 32, 2)]
fourgrams[.(123, 32, 2)]

setkey(trigrams, codeTrigram)
setkey(fourgrams, y, z, t)
fourgrams[ , pYZT3g:=trigrams[.(yzt3gCode)]$pXYZ]
setkey(trigrams, x, y, z)
trigrams[.(123, 32, 2)]
fourgrams[.(123, 32, 2)]

setkey(fourgrams, x,y,z,t)
fourgrams[ , pXYZT:= dcXYZT/cXYZ3g]
fourgrams

fourgrams[ , beta3g:= 1 - sum(pXYZT), by=xyz3gCode]
fourgrams

fourgrams[ , alpha3g:= beta3g/(1 - sum(pYZT3g)), by=xyz3gCode]
fourgrams

format(object.size(fourgrams), units="Mb")

#############################################################################

setkey(fourgrams, x, y, z, t)
fourgrams[ , codeFourgram:= 1:nrow(fourgrams)]
fourgrams

fourgram2code = function(x0, y0, z0, t0){
  l = list(x =x0, y=y0, z=z0, t=t0)
  fourgrams[l]$codeFourgram
}

(x0 = word2code("been"))
(y0 = word2code("busy"))
(z0 = word2code("together"))
(t0 = word2code("playing"))

fourgram2code(x0, y0, z0, t0)
fourgrams[fourgram2code(x0, y0, z0, t0)]


code2fourgram = function(a){
  where = sapply(a, function(x)which(fourgrams$codeFourgram == x))
  return(unlist(fourgrams[where, .(x, y, z, t)]))
}

code2fourgram(25964706)
fourgram2code(code2fourgram(25964706)[1], code2fourgram(25964706)[2], code2fourgram(25964706)[3], code2fourgram(25964706)[4])

#####################################################
#####################################################
#####################################################
## Preparing the language model data
#####################################################
#####################################################
#####################################################

setkey(fourgrams, x, y, z, t)
names(fourgrams)
format(object.size(fourgrams), units="Mb")
fourgrams[ , c("cX", "cY", "cZ", "cT", "dcXYZT", "cXYZ3g", "xyz3gCode", "yzt3gCode", "pYZT3g") := NULL]
format(object.size(fourgrams), units="Mb")

setkey(trigrams, x, y, z)
names(trigrams)
format(object.size(trigrams), units="Mb")
trigrams[ , c("cX", "cY", "cZ", "dcXYZ", "xy2gCode", "cXY2g", "yz2gCode", "pYZ2g") := NULL]
format(object.size(trigrams), units="Mb")

setkey(bigrams, x, y)
names(bigrams)
format(object.size(bigrams), units="Mb")
bigrams[ , c("cX", "cY", "dcXY", "cX1g", "pX1g", "pY1g", "xy") := NULL]
format(object.size(bigrams), units="Mb")

setkey(unigrams, x)
names(unigrams)
format(object.size(unigrams), units="Mb")

#############################################################################
#############################################################################
#############################################################################
# Now for the reduced version
#############################################################################
#############################################################################
#############################################################################

lowFreq = 2

format(object.size(unigrams), units="Mb")
unigrams2 = copy(unigrams[cX > lowFreq])
nrow(unigrams2)
format(object.size(unigrams2), units="Mb")

format(object.size(bigrams), units="Mb")
bigrams2 = copy(bigrams[cXY > lowFreq])
nrow(bigrams2)
format(object.size(bigrams2), units="Mb")

format(object.size(trigrams), units="Mb")
trigrams2 = copy(trigrams[cXYZ > lowFreq])
nrow(trigrams2)
format(object.size(trigrams2), units="Mb")

format(object.size(fourgrams), units="Mb")
fourgrams2 = copy(fourgrams[cXYZT > lowFreq])
nrow(fourgrams2)
format(object.size(fourgrams2), units="Mb")

format(object.size(codes), units="Mb")
codes2 = copy(codes[cX > lowFreq])
format(object.size(codes2), units="Mb")

format(object.size(codes2Remove), units="Mb")
codes2Remove2 = copy(codes2Remove[cX > lowFreq])
format(object.size(codes2Remove2), units="Mb")


#############################################################################
## Saving the model data and removing the files from memory
#############################################################################

saveRDS(unigrams2, file = "./data/unigrams.Rds")
saveRDS(bigrams2, file = "./data/bigrams.Rds")
saveRDS(trigrams2, file = "./data/trigrams.Rds")
saveRDS(fourgrams2, file = "./data/fourgrams.Rds")
saveRDS(codes2, file = "./data/codes.Rds")
saveRDS(codes2Remove2, file = "./data/codes2Remove.Rds")

```
