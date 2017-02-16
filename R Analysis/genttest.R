

```{r}

selectedpred <- sample_frac(predwithCNN)

predwithCNNsortedpred <- arrange(selectedpred[1:20425,], RelevantPred)

bin <- rep(1:817, each=25)

predwithCNNsortedpred <- cbind(predwithCNNsortedpred, bin)

predwithCNNsortedpred$Response <- as.numeric(predwithCNNsortedpred$Response)-1

predwithCNNsortedpred <- group_by(predwithCNNsortedpred, bin)

summaryPred <- summarize(predwithCNNsortedpred, Pyes = mean(Response), cor = mean(RelevantPred), cnnDumb = mean(RevelantCNN), cnnTrained = mean(CNNSigmoidRelPred))

model <- lm(Pyes ~ cor, data = summaryPred)

```

```{r}

predwithCNNsortedpred <- arrange(selectedpred[20426:22695,], PooledPred)

bin <- rep(1:227, each=10)

predwithCNNsortedpred <- cbind(predwithCNNsortedpred, bin)

predwithCNNsortedpred$Response <- as.numeric(predwithCNNsortedpred$Response)-1

predwithCNNsortedpred <- group_by(predwithCNNsortedpred, bin)

SecondsummaryPred <- summarize(predwithCNNsortedpred, Pyes = mean(Response), cor = mean(RelevantPred), cnnDumb = mean(RevelantCNN), cnnTrained = mean(CNNSigmoidRelPred))

model2 <- lm(Pyes ~ cor, data = SecondsummaryPred)

```


## Slide with Plot

```{r summaryPredCorPlot}
plot(summaryPred$cor, summaryPred$Pyes, pch=16)

abline(lm(summaryPred$Pyes~summaryPred$cor))
text(paste("r = ", round(cor(summaryPred$cor, summaryPred$Pyes),2)), x = -0.05, y = 0.2)

```

##How good are the internal models?


```{r, echo=TRUE}
cor.test(SecondsummaryPred$cor, SecondsummaryPred$Pyes)
```

##How good are the internal models?
```{r, echo=TRUE}
cor.test(summaryPred$cor, summaryPred$Pyes)
```


## now let's cross validate
```{r, echo=TRUE}
cor.test(predict(model, newdata = data.frame(cor = SecondsummaryPred$cor)), SecondsummaryPred$Pyes)
```

## now let's cross validate
```{r, echo=TRUE}
cor.test(predict(model2, summaryPred), summaryPred$Pyes)
```
