#BiocManager::install("ggtree")
library("ggtree")
#install.packages("pals")
library("pals")
#install.packages("ggnetwork")
library("ggnetwork")
#install.packages("ggrepel")
library("ggrepel")
#install.packages("dplyr")
library(dplyr)

#Make NPF all tree
tree <- read.tree("RAxML_bipartitions.npf_all_rn")
groupInfo <- split(tree$tip.label, gsub("[_-_].+|NPF.*", "", tree$tip.label))
names(groupInfo) <- c("Arabidopsis thaliana", "Amborella trichopoda", "Eucalyptus grandis", "Hakea prostrata", "Macadamia integrifolia", "Medicago truncatula", "Nelumbo nucifera", "Telopea speciosissima", "Vitis vinifera", "Zea mays")
tree <- groupOTU(tree, groupInfo)

#Species colours
mycol <- kelly()[c(12,9,13,10,6,8,11,19,4,3)]
names(mycol) <- names(groupInfo)

#Draw NPF all tree
npf_all_tree <- ggtree(tree, layout='circular', size=0.3, branch.length = "all") + 
  geom_tippoint(aes(color=group,), size=0.7, alpha=1) +
  geom_label_repel(data = . %>% mutate(label = ifelse(grepl('AtNPF|HpNPF', label), label, "")),
                   aes(label=label, color=group), show.legend=F, max.overlaps=150, size=1.4, 
                   box.padding=0.1, label.padding=0.1, max.time=0.5, force_pull=0.5) +
  #geom_text(aes(label=node), color="red", hjust=-.3, size=1) + #for node numbers
  scale_color_manual(values=mycol, name="") +
  theme_tree(legend.position = c(0.73,0.13), legend.background = element_rect(fill='transparent'), legend.text = element_text(face = "italic", size=8), legend.key.size=unit(0.4, 'cm')) +
  guides(color=guide_legend(ncol=1, bycol=T))
npf_all_tree 
