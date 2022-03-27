setwd('/working/lab_stuartma/nathanI/AGING/corrplot_res')

#function to conver DF into matrix

ldsc_matrices <- function(ldsc_results, stats){
  library(reshape2)
  traits <- unique(c(ldsc_results$p1, ldsc_results$p2))
  ldsc_results$p_fdr <- p.adjust(ldsc_results$p, method='fdr')
  matrices <- lapply(stats, function(x){
    mat <- dcast(ldsc_results, p1 ~ p2, value.var = x)
    rownames(mat) <- mat$p1
    mat[,1] <- 0
    colnames(mat)[1] <- rownames(mat)[1]
    mat[length(traits),] <- 0
    rownames(mat)[length(traits)] <- colnames(mat)[length(traits)]
    mat <- mat[traits,traits] # same order of traits as long format (ensures perfect upper triangle matrix)
    diag(mat) <- 1
    mat[lower.tri(mat)] <- t(mat)[lower.tri(mat)]
    mat <- as.matrix(mat)
  })
  names(matrices) <- stats
  matrices
}

library(data.table)
setwd("/working/lab_stuartma/nathanI/AGING/corrplot_res_2")
LDSC_data <- fread('test_combined_LDSC_res.txt', header = T, stringsAsFactors = F)

ldscmat <- ldsc_matrices(LDSC_data, stats = c("rg","se","p", "p_fdr", "gcov_int", "gcov_int_se"))

#reorder rows
traits_order <- c("Perceived_Aging", "Perceived_Aging_Females", "Perceived_Aging_Males", "BG6_Skin_Aging"
                                 ,"CLSA_healthy_aging", "Pigmentation_spots", "Melanoma", "Hair_col_no_red", "Red_hair"
                                 ,"CH_sunburn", "Skin_colour_UKBB", "Tanning", "Time_spent_out_doors", "SCC"
                                 , "BCC", "Telomere_length","Nevus_count" )


ldscmat_ord <- lapply(names(ldscmat), function(x){
  ldscmat[[x]][traits_order,traits_order]
})

nam <- names(ldscmat)
names(ldscmat_ord) <- nam

library(corrplot)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

jpeg(filename = "/working/lab_stuartma/nathanI/AGING/corrplot_res_2/Corrplot_ordered_rows.jpeg"
     ,width = 400
     ,height = 400)
pdf(file = "/working/lab_stuartma/nathanI/AGING/corrplot_res_2/Corrplot_ordered_rows.pdf",  
    width = 8.3, 
    height = 8.3,
    paper = "a4r")
corrplot(ldscmat_ord[["rg"]], method="color",col=col(200),
         type = "upper",addCoef.col = "black", order = 'original' , p.mat = ldscmat_ord[["p"]], insig = "blank",
         sig.level = c(.05), pch.cex=1, pch.col = "white",number.cex=0.75, diag = F, tl.col = "black", outline = T)
dev.off()
