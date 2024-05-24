library("ggtree")
library("ggrepel")
library("ggnetwork")
library(dplyr)

#Make PHT1 tree
tree <- read.tree("RAxML_bipartitions.proteaceae_tree_mafft_rn")
groupInfo <- split(tree$tip.label, gsub("[_-_].+|PHT.*", "", tree$tip.label))
names(groupInfo) <- c("Amborella trichopoda", "Hakea prostrata", "Macadamia integrifolia", "Nelumbo nucifera", "Telopea speciosissima")
tree <- groupOTU(tree, groupInfo)

#Colour Amborella,Hakea,Macadamia,Nelumbo,Telopea
mycol <- kelly()[c(9,10,6,11,19)]
names(mycol) <- names(groupInfo)

#Draw tree
pht1_tree <- ggtree(tree, layout='rectangular', size=0.8) + 
  xlim(0, 2.1) +
  geom_nodelabel(aes(label=as.numeric(label)), size=2, color = "black", alpha=0.7, nudge_x=-0.04, nudge_y=0.6, label.size=0.1, label.padding=unit(0.1,"lines")) +
  geom_tiplab(aes(subset=(grepl('HpPHT1|TsPHT1|MiPHT1', label))==TRUE,color=group), size=2.7, align=F, show.legend=FALSE, offset = 0.02) +
  geom_tippoint(aes(color=group,), size=2) +
  scale_color_manual(values=mycol, name="") +
  theme_tree2(legend.position = c(0.84,0.12), legend.background = element_rect(), legend.text = element_text(face = "italic", size=8), legend.key.size=unit(0.4, 'cm')) +
  guides(color=guide_legend(ncol=1, bycol=T))
flip(pht1_tree, 48, 47) %>% ggtree::rotate(53) %>% flip(54, 56) %>% flip(101, 57) %>% ggtree::rotate(98) %>% ggtree::rotate(100) %>% flip(39, 94) %>% ggtree::rotate(62) %>% 
  ggtree::rotate(64) %>% ggtree::rotate(66) %>% ggtree::rotate(68) %>% flip(88, 73) %>% ggtree::rotate(81) %>% flip(83, 85) %>% ggtree::rotate(88) %>% ggtree::rotate(90) %>% ggtree::rotate(54)
