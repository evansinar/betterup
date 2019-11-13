library(stm)        # Package for sturctural topic modeling
library(igraph)     # Package for network analysis and visualisation
library(stmCorrViz) # Package for hierarchical correlation view of STMs
library(wordcloud)
library(broom)

data <- read.csv("datafiles/WebsiteFront.csv") 

processed <- textProcessor(data$text, metadata=data)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

docs <- out$documents
vocab <- out$vocab
meta <- out$meta

plotRemoved(processed$documents, lower.thresh=seq(1,200, by=100))

poliblogPrevFit <- stm(out$documents, out$vocab, K=10, 
                       max.em.its=75, data=out$meta, init.type="Spectral", 
                       seed=8458159)

plot(poliblogPrevFit, type="summary", xlim=c(0,.4))

plot(poliblogPrevFit, type="labels", topics=c(3,7,10))

plot(poliblogPrevFit, type="hist")

plot(poliblogPrevFit, type="perspectives", topics=c(7,10))

poliblogSelect <- selectModel(out$documents, out$vocab, K=20, 
                              max.em.its=75, data=meta, runs=20, seed=8458159)

plotModels(poliblogSelect)

topicQuality(model=poliblogPrevFit, documents=docs)

selectedModel3 <- poliblogSelect$runout[[1]]

storage <- manyTopics(out$documents, out$vocab, K=c(7:10), 
                      data=meta, runs=10)
storageOutput1 <- storage$out[[1]] # For example, choosing the model with 7 topics
plot(storageOutput1)

kResult <- searchK(out$documents, out$vocab, K=c(7,10), 
                   data=meta)
plot(kResult)

labelTopicsSel <- labelTopics(poliblogPrevFit, c(3,7,10))

print(sageLabels(poliblogPrevFit))

mod.out.corr <- topicCorr(poliblogPrevFit)
plot(mod.out.corr)

cloud(poliblogContent, topic=7)

plot(poliblogPrevFit$convergence$bound, type="l", ylab="Approximate Objective", 
     main="Convergence")

stmCorrViz(poliblogPrevFit, "stm-interactive-correlation.html", 
           documents_raw=data$documents, documents_matrix=out$documents)

#NOT SURE WHY THIS NEXT PART ISN'T WORKING

td_beta <- tidy(poliblogPrevFit)

td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")
