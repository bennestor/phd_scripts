library("ggtree")
library("ggrepel")
library("ggnetwork")
library(dplyr)

#Make PHT1 all tree
tree <- read.tree("RAxML_bipartitions.all_tree_mafft")
groupInfo <- split(tree$tip.label, gsub("[_-].+|PHT.*", "", tree$tip.label))
names(groupInfo) <- c("Amborella trichopoda", "Arabidopsis thaliana", "Eucalyptus grandis", "Hakea prostrata", "Macadamia integrifolia", "Medicago truncatula", "Nelumbo nucifera", "Oryza sativa", "Telopea speciosissima", "Vitis vinifera", "Zea mays")
tree <- groupOTU(tree, groupInfo)

#Colour species
mycol <- kelly()[c(9,12,13,10,6,8,11,5,19,4,3)]
names(mycol) <- names(groupInfo)

#Draw tree
pht1_all <- ggtree(tree, layout='circular', size=0.3, branch.length="none") + 
  geom_nodelabel(aes(label=as.numeric(label)), size=2, color = "black", alpha=0.5, nudge_x=-0.04, nudge_y=0.6, label.size=0.1, label.padding=unit(0.1,"lines")) +
  #geom_text(aes(label=node), color="red", hjust=-.3, size=1) + #for node numbers
  geom_tiplab(aes(subset=(grepl('AtPHT1|HpPHT1|OsPHT1', label))==TRUE,color=group), size=3, align=F, show.legend=FALSE, offset = 0.8) +
  geom_tippoint(aes(color=group,), size=2) +
  scale_color_manual(values=mycol, name="") +
  theme_tree(legend.position = c(0.88,-0.04), legend.background = element_rect(fill='transparent'), legend.text = element_text(face = "italic", size=9), legend.key.size=unit(0.4, 'cm')) +
  guides(color=guide_legend(ncol=1, bycol=T))
flip(pht1_all, 171, 175) %>% flip(53, 186) %>% ggtree::rotate(174) %>% flip(222, 230) %>% flip(94, 229) %>% ggtree::rotate(204) %>% ggtree::rotate(167) %>% ggtree::rotate(151) %>% ggtree::rotate(166) %>% 
  ggtree::rotate(193) %>% flip(208,192) %>% flip(194,193)
