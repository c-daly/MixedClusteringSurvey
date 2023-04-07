library(kamila)
library(aricode)
library(rlist)
library(sfsmisc)

DS_IDX <- 1
PC_IDX <- 2
CLUSTER_IDX <- 3

D_CAT2_COLS <- list("C1", "C2")
D_CAT4_COLS <- list("C1", "C2", "C3", "C4")
D_CAT8_COLS <- list("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8")

D_CONT2_COLS <- list("x1", "x2")
D_CONT4_COLS <- list("x1", "x2", "x3", "x4")
D_CONT8_COLS <- list("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8")
D_CONT10_COLS <- list("x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9")

# N varies
D_300N <- readRDS(url("https://mbi-ds4h.loria.fr/wp-content/uploads/2021/02/Design1_N300.rds","rb"))
D_1200N <- readRDS(url("https://mbi-ds4h.loria.fr/wp-content/uploads/2021/02/Design1_N1200.rds", "rb"))
D_600N <- readRDS(url("https://mbi-ds4h.loria.fr/wp-content/uploads/2021/02/Design1_N600.rds","rb"))

DEFAULT_CONT = list("x1", "x2", "x3", "x4")
DEFAULT_CAT = list("C1", "C2", "C3", "C4")
DEFAULT_COLUMNS_LIST = list(DEFAULT_CONT, DEFAULT_CAT)  

D_300N_DS <- list(D_300N, DEFAULT_COLUMNS_LIST)
D_600N_DS <- list(D_600N, DEFAULT_COLUMNS_LIST)
D_1200N_DS <- list(D_1200N, DEFAULT_COLUMNS_LIST)

D_NVARIES <- list(D_300N_DS, D_600N_DS, D_1200N_DS)
SMALL_NVARIES <- list(D_300N_DS)
D_2k <- readRDS(url("https://mbi-ds4h.loria.fr/wp-content/uploads/2021/02/Design2_Nb2.rds"))
D_6k <- readRDS(url("https://mbi-ds4h.loria.fr/wp-content/uploads/2021/02/Design2_Nb6.rds"))
D_10k <- readRDS(url("https://mbi-ds4h.loria.fr/wp-content/uploads/2021/02/Design2_Nb10.rds"))

D_2k_DS <- list(D_2k, DEFAULT_COLUMNS_LIST)
D_6k_DS <- list(D_6k, DEFAULT_COLUMNS_LIST)
D_10k_DS <- list(D_10k, DEFAULT_COLUMNS_LIST)

D_KVARIES <- list(D_2k_DS, D_6k_DS, D_10k_DS) 

# CONT Varies
D_CAT2_CONT2 <- readRDS(url("https://mbi-ds4h.loria.fr/wp-content/uploads/2021/02/Design3_Cat2_Cont2.rds"))
D_CAT2_CONT4 <- readRDS(url("https://mbi-ds4h.loria.fr/wp-content/uploads/2021/02/Design3_Cat2_Cont4.rds"))
D_CAT2_CONT8 <- readRDS(url("https://mbi-ds4h.loria.fr/wp-content/uploads/2021/02/Design3_Cat2_Cont8.rds"))

D_CAT2_CAT = list("C1", "C2")
D_CAT2_CONT2_CONT = list("x1", "x2")
D_CAT2_CONT4_CONT = list("x1", "x2", "x3", "x4")
D_CAT2_CONT8_CONT = list("x1", "x2", "x3","x4", "x5", "x6", "x7", "x8")

D_CAT2_CONT2_DS = list(D_CAT2_CONT2, list(D_CAT2_CONT2_CONT, D_CAT2_CAT))
D_CAT2_CONT4_DS = list(D_CAT2_CONT4, list(D_CAT2_CONT4_CONT, D_CAT2_CAT))
D_CAT2_CONT8_DS = list(D_CAT2_CONT8, list(D_CAT2_CONT8_CONT, D_CAT2_CAT))

D_CONTVARIES = list(D_CAT2_CONT2_DS, D_CAT2_CONT4_DS, D_CAT2_CONT8_DS) 

# Cont variable relevance
D_CONT_LOW_REL <-readRDS(url("https://mbi-ds4h.loria.fr/wp-content/uploads/2021/02/Design4_DegreeOfRelCont_low.rds"))
D_CONT_MED_REL <-readRDS(url("https://mbi-ds4h.loria.fr/wp-content/uploads/2021/02/Design4_DegreeOfRelCont_mild.rds"))
D_CONT_HIGH_REL <- readRDS(url("https://mbi-ds4h.loria.fr/wp-content/uploads/2021/02/Design4_DegreeOfRelCont_high.rds"))

D_CONT_LOW_REL_DS <- list(D_CONT_LOW_REL, DEFAULT_COLUMNS_LIST)
D_CONT_MED_REL_DS <- list(D_CONT_MED_REL, DEFAULT_COLUMNS_LIST)
D_CONT_HIGH_REL_DS <- list(D_CONT_HIGH_REL, DEFAULT_COLUMNS_LIST)

D_CONT_REL_VARIES <- list(D_CONT_LOW_REL_DS, D_CONT_MED_REL_DS, D_CONT_HIGH_REL_DS)

# CAT Variable relevance
D_CAT_LOW_REL <- readRDS(url("https://mbi-ds4h.loria.fr/wp-content/uploads/2021/02/Design5_DegreeOfRelCat_low.rds"))
D_CAT_MED_REL <- readRDS(url("https://mbi-ds4h.loria.fr/wp-content/uploads/2021/02/Design5_DegreeOfRelCat_mild.rds"))
D_CAT_HIGH_REL <- readRDS(url("https://mbi-ds4h.loria.fr/wp-content/uploads/2021/02/Design5_DegreeOfRelCat_high.rds"))

D_CAT_LOW_REL_DS <- list(D_CAT_LOW_REL, DEFAULT_COLUMNS_LIST)
D_CAT_MED_REL_DS <- list(D_CAT_MED_REL, DEFAULT_COLUMNS_LIST)
D_CAT_HIGH_REL_DS <- list(D_CAT_HIGH_REL, DEFAULT_COLUMNS_LIST)

D_CAT_REL_VARIES <- list(D_CAT_LOW_REL_DS, D_CAT_MED_REL_DS, D_CAT_HIGH_REL_DS)

# Num cont vars waries with 4 cat vars
D_CONT2_CAT4 <- readRDS(url("https://mbi-ds4h.loria.fr/wp-content/uploads/2021/02/Design3bis_Cat4_Cont2.rds"))
D_CONT4_CAT4 <- readRDS(url("https://mbi-ds4h.loria.fr/wp-content/uploads/2021/02/Design3bis_Cat4_Cont4.rds"))
D_CONT8_CAT4 <- readRDS(url("https://mbi-ds4h.loria.fr/wp-content/uploads/2021/02/Design3bis_Cat4_Cont8.rds"))

D_CONT2_CAT4_COLS <- list(D_CONT2_COLS, D_CAT4_COLS)
D_CONT4_CAT4_COLS <- list(D_CONT4_COLS, D_CAT4_COLS)
D_CONT8_CAT4_COLS <- list(D_CONT8_COLS, D_CAT4_COLS)

D_CONT2_CAT4_DS <- list(D_CONT2_CAT4, D_CONT2_CAT4_COLS)
D_CONT4_CAT4_DS <- list(D_CONT4_CAT4, D_CONT4_CAT4_COLS)
D_CONT8_CAT4_DS <- list(D_CONT8_CAT4, D_CONT8_CAT4_COLS)

D_NUM_CONT_VARIES_CAT4 <- list(D_CONT2_CAT4_DS, D_CONT4_CAT4_DS, D_CONT8_CAT4_DS)
# Num cont vars varies with 8 cat vars
D_CONT2_CAT8 <- readRDS(url("https://mbi-ds4h.loria.fr/wp-content/uploads/2021/02/Design3ter_Cat8_Cont2.rds"))
D_CONT4_CAT8 <- readRDS(url("https://mbi-ds4h.loria.fr/wp-content/uploads/2021/02/Design3ter_Cat8_Cont4.rds"))
D_CONT8_CAT8 <- readRDS(url("https://mbi-ds4h.loria.fr/wp-content/uploads/2021/02/Design3ter_Cat8_Cont8.rds"))

D_CONT2_CAT8_COLS <- list(D_CONT2_COLS, D_CAT8_COLS)
D_CONT4_CAT8_COLS <- list(D_CONT4_COLS, D_CAT8_COLS)
D_CONT8_CAT8_COLS <- list(D_CONT8_COLS, D_CAT8_COLS)

D_CONT2_CAT8_DS <- list(D_CONT2_CAT8, D_CONT2_CAT8_COLS)
D_CONT4_CAT8_DS <- list(D_CONT4_CAT8, D_CONT4_CAT8_COLS)
D_CONT8_CAT8_DS <- list(D_CONT8_CAT8, D_CONT8_CAT8_COLS)

D_NUM_CONT_VARIES_CAT8 <- list(D_CONT2_CAT8_DS, D_CONT4_CAT8_DS, D_CONT8_CAT8_DS)
# Overall dataset
data <- list(D_NVARIES, D_CONTVARIES, D_KVARIES, D_CAT_REL_VARIES, D_CONT_REL_VARIES, D_NUM_CONT_VARIES_CAT4, D_NUM_CONT_VARIES_CAT8)

ylab <- "ARI"
nvaries_plot_characteristics <- list(names = c("n = 300", "n = 600", "n = 1200"), ylab = "ARI", xlab="Population Size")
contvaries_plot_characteristics <- list(names = c("2", "4", "8"), ylab = "ARI", xlab="Number of Continuous Variables w/ 2 Categorical")
kvaries_plot_characteristics <- list(names = c("k = 2", "k = 6", "k = 10"), ylab = ylab, xlab="Number of Clusters")
cat_rel_varies_plot_characteristics <- list(names = c("low", "med", "high"), ylab = ylab, xlab="Categorical Variable Relevance")
cont_rel_varies_plot_characteristics <- list(names = c("low", "med", "high"), ylab = ylab, xlab="Continuous Variable Relevance")
contvaries_plot_characteristics_cat4 <- list(names = c("2", "4", "8"), ylab = "ARI", xlab="Number of Continuous Variables w/ 4 Categorical)")
contvaries_plot_characteristics_cat8 <- list(names = c("2", "4", "8"), ylab = "ARI", xlab="Number of Continuous Variables w/ 8 Categorical")

default_cluster <- list(6, 6, 6)
plot_characteristics <- list(nvaries_plot_characteristics, contvaries_plot_characteristics, kvaries_plot_characteristics, cat_rel_varies_plot_characteristics, cont_rel_varies_plot_characteristics, contvaries_plot_characteristics_cat4, contvaries_plot_characteristics_cat8)
cluster_params <- list(default_cluster, default_cluster, list(2, 6, 10), default_cluster, default_cluster, default_cluster, default_cluster)

data_collection <- list(data, plot_characteristics, cluster_params)
DS_COUNT <- length(data)
