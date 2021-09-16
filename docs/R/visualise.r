# visualize.r

library(stm)
library(dplyr)
library(tidyverse)
library(tidytext)
library(tidystm)
#devtools::install_github("mikajoh/tidystm")
library(ggplot2)
library(ggthemes)

args <- commandArgs(trailingOnly=TRUE)
rawfn <- args[1]
modelfn <- args[2]
metafn <- args[3]
CLUSTER <- as.integer(args[4])

raw <- read.csv("tiabList.csv")
model <- readRDS("model_12.obj")
meta <- readRDS("meta.obj")
data <- na.omit(raw)

summary(model)

# set save imgdir
imgdir <- paste("../result/K",CLUSTER,sep="")
if(!file.exists(imgdir)){
    dir.create(imgdir)
}

# estimate effect (covariance: published year)
# overall evaluation
#prep2 <- estimateEffect(c(1:CLUSTER) ~ poly(py,2), model, metadata=meta)
prep3 <- estimateEffect(c(1:12) ~ poly(py,3), model, metadata=meta)
#prep4 <- estimateEffect(c(1:CLUSTER) ~ poly(py,4), model, metadata=meta)
#anova(prep2,prep3,prep4)

bbb<-stm::labelTopics(model)


bbb<-?tidystm::extract.estimateEffect(prep3)

library(broom)
library(tidytext)
td_beta <- tidy(model)


table(top_terms$topic)

top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) 

prep3$parameters

effect <- extract.estimateEffect(prep3, 'py', model = model,
                                method="pointestimate", labeltype="prob")
effect$topic <- as.factor(effect$topic)
print(effect$topic)
topic12 <- c("1:teleservice","2:telerihabilitation","3:technology of telehealth",
             "4:support for stroke","5:support for children","6:monitoring and prevention for high-risk symptom",
             "7:support for caregiver","8:mobile app","9:mental health","10:support for smoker",
             "11:support for chronic disorder","12:support for depression/anxiety") 
levels(effect$topic) <- topic12

meta$py

g <- ggplot(effect, aes(x = covariate.value, y=estimate,
            ymin = ci.lower, ymax = ci.upper))+
     facet_wrap(~ effect$topic) +
     geom_ribbon(alpha=.5) +
     geom_line() +
     labs(x="Year",y="Expected Relative Topic Proportion") +
     theme(legend.position="bottom")+
     theme_bw() +
     theme(strip.text.x=element_text(size=8,colour="black"))

savefn <- paste(imgdir,"/estimateEffect.png",sep="")
png(savefn, width=1000, height=1000)
#plot(prep, "py", model=model, method="continuous")
print(g)
dev.off()

topics <- model %>% tidy %>% as.data.frame

topics_sort <- group_by(topics,topic) %>% 
                top_n(20,beta) %>% 
                ungroup() %>% 
                mutate(term=reorder(term,beta)) %>% 
                arrange(topic,-beta)

topics4plot <- topics_sort %>% 
    mutate(term=reorder(term,beta)) %>% 
    group_by(topic,term) %>% 
    arrange(desc(beta)) %>% 
    ungroup() 

head(topics4plot)
savedir <- paste("../data/K",CLUSTER, sep="")
if(!file.exists(savedir)){
    dir.create(savedir)
}

savefn <- paste(savedir,"/topics4plot.csv", sep="")
write.csv(topics4plot, savefn, row.names=FALSE)


print("#### make bar plot ####")
p <- topics4plot %>% 
        mutate(term=factor(paste(term,topic,sep="_"),
                     levels=rev(paste(term,topic,sep="_")))) %>%
        ggplot(aes(x=term,y=beta,fill=beta))+
        geom_bar(stat="identity")+
        facet_wrap(~topic,scales = "free")+
        coord_flip()+
        scale_x_discrete(labels = function(x)gsub("_.+$","",x))

savefn <- paste(imgdir,"/topic_rank.png",sep="")
png(savefn, width=1000,height=1000)
print(p)
dev.off()
print("DONE: make bar plot")
?stm::calcfrex()

stm::cloud(model, topic=1)

temp<-textProcessor(documents=data$ab,metadata=meta)

stm::toLDAvis(model,data$ab)


print("START: make wordcloud")
for (i in 1:CLUSTER){
    print(paste("CLUSTER : ",i,sep=""))

    df <- filter(topics4plot, topic==i)

    savefn <- paste(imgdir,"/wordcloud_",i,".png", sep="")
    png(savefn, width=1000, height=1000, res=216)
    stm::cloud(model, topic=i)
    dev.off()
    print(paste("DONE: make wordcloud",i, sep=" "))
}
print("DONE: make wordcloud")

print("START: make topicCorr")
tcModel <- topicCorr(model)
savefn <- paste(imgdir,"/topic_corr.png",sep="")
png(savefn,width=1000, height=1000)
plot(tcModel)
dev.off()
print("DONE: make topicCorr")

# top 20 topics
td_beta <- model %>% tidy
td_gamma <- model %>% tidy(matrix="gamma")

top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(7, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>%
  unnest()

print(top_terms)

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

print(gamma_terms)

theme_set(theme_classic(base_family = "Helvetica")) 
g <- gamma_terms %>%
  top_n(10, gamma) %>%
  ggplot(aes(topic, gamma, label = terms)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 6) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.30)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=20, face="bold"))
  
savefn <- paste(imgdir,"/top_topic.png",sep="")
png(savefn,width=1000,height=1000)
print(g)
dev.off()


warnings()


