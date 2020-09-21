require(ggplot2)
# why is word learning thought to be so hard?
# - complex visual scenes, multiword utterances (word segmentation problem), infrequent naming of objects
# what are possible solutions to each of these?
# what does word learning look like in the real world?
# are learning situations really that complicated?
# what happens when the learner can influence the topic of conversation? 
#  (how do they do that? are caregivers sensitive?)
# links to curiobaby:

# 12 nonce words, 12 categories of 5 referents (5 diff pictures of each)
# varying referential uncertainty per trial: High Informative (HI): 2 objects (chance=50%)
# Low Informative (LI): 5 objects (chance=20%)
# 4 learning conditions: 
HIobj = cbind(1:12, c(3:12,1,2), NA, NA, NA)
HIobj2 = cbind(1:12, c(4:12,1:3), NA, NA, NA)
HIobj3 = cbind(1:12, c(5:12,1:4), NA, NA, NA)
HIobj4 = cbind(1:12, c(6:12,1:5), NA, NA, NA)
HIobj5 = cbind(1:12, c(7:12,1:6), NA, NA, NA)
LIobj = cbind(1:12, c(3:12,1:2), c(4:12,1:3), c(5:12,1:4), c(6:12,1:5)) 
LIobj2 = cbind(1:12, c(7:12,1:6), c(8:12,1:7), c(9:12,1:8), c(10:12,1:9)) 
LIobj3 = cbind(1:12, c(11:12,1:10), c(2:12,1), c(3:12,1:2), c(4:12,1:3)) 
LIobj4 = cbind(1:12, c(5:12,1:4), c(7:12,1:6), c(9:12,1:8), c(11:12,1:10)) 
LIobj5 = cbind(1:12, c(6:12,1:5), c(8:12,1:7), c(10:12,1:9), c(12,1:11)) 
words = matrix(rep(1:12,5), ncol=1, byrow=T) 
Medina = list()
Medina[["HIfirst"]]$train = list(words=words, objs=rbind(HIobj, LIobj, LIobj2, LIobj3, LIobj4))
Medina[["HImid"]]$train = list(words=words, objs=rbind(LIobj, LIobj2, HIobj, LIobj3, LIobj4))
Medina[["HIlast"]]$train = list(words=words, objs=rbind(LIobj, LIobj2, LIobj3, LIobj4, HIobj))
Medina[["HIabsent"]]$train = list(words=words, objs=rbind(LIobj, LIobj2, LIobj3, LIobj4, LIobj5))

save(Medina, file="Medina2011orders.RData")

source("models/kachergis.R")
pars = c(.03, 1, 1)
HI1 = model(pars, Medina[["HIfirst"]]$train)
HIm = model(pars, Medina[["HImid"]]$train)
HIl = model(pars, Medina[["HIlast"]]$train)
HIabs = model(pars, Medina[["HIabsent"]]$train)

# graph performance per block (1-5)
get_block_perf <- function(modperf) {
  perf_inds = seq(12,60,12)
  perf = c()
  for(i in perf_inds) {
    perf = c(perf, mean(diag(modperf$traj[[i]]) / rowSums(modperf$traj[[i]])))
  }
  return(perf)
}

mdat = data.frame(rbind(cbind(Condition="HI first", Vignette=1:5, Model=get_block_perf(HI1)),
        cbind(Condition="HI middle", Vignette=1:5, Model=get_block_perf(HIm)),
        cbind(Condition="HI last", Vignette=1:5, Model=get_block_perf(HIl)),
        cbind(Condition="HI absent", Vignette=1:5, Model=get_block_perf(HIabs)) ))

mdat$Model = as.numeric(as.character(mdat$Model))

ggplot(mdat, aes(x=Vignette, y=Model, group=Condition, shape=Condition, color=Condition)) + 
  geom_point() + 
  geom_line() + theme_bw()
ggsave("Medina2013")