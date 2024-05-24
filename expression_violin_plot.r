library(ggplot2)
library(tidyr)

#Load in TPM expression data
tpm <- read.table("kallisto.isoform.TPM.not_cross_norm", header=TRUE)

#State order of RNAseq libraries
tpm_order <- tpm[,c("Gene","R1","R10","R50","CR0","CR1","CR4","CR7","CR12","ML1",'ML10',"ML50","WL1","WL2","WL3","WL4","WL5","WL6","WL7","WL8","WL9","WL10","WL11","WL12","WL13","WL14","WL15")]

#Change to long format
tpm_long <- gather(tpm_order, Sample, TPM, R1:WL15, factor_key=TRUE)

#Subset to representative samples
tpm_comp <- subset(tpm_long, Sample == "R1" | Sample == "CR4" | Sample == "ML1" | Sample == "WL1" | Sample == "WL4" | Sample == "WL7" | Sample == "WL10" | Sample == "WL13", select = c("Gene", "Sample", "TPM"))

#Filter to TPM >= 1
tpm_comp_filt <- subset(tpm_comp, TPM >= 1)

#Make log of TPM
tpm_comp_filt$logTPM = log(tpm_comp_filt$TPM)

#Make violin plot of logged TPM expression data
ggplot(tpm_comp_filt, aes(x = Sample, y = logTPM, fill = Sample)) + 
  geom_violin(trim = FALSE) +
  ylab("log(TPM)") +
  xlab("") +
  scale_fill_manual(values=c("white","lightgrey","#4A820A","#760000", "#AD795C", "#D9BF76", "#CAE78A", "#9DDF18")) +
  stat_summary(fun=mean, geom="point", shape=23, size=2, show.legend = FALSE) +
  theme_classic()
