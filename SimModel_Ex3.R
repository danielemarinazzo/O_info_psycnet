# EXAMPLE 3: 9 LATENT VARIABLES, ALL TRIPLETS, CLUSTERS OF THREE TRIPLETS (EACH NODE CONNECTED TO TWO OTHER TRIPLETS)

# Loading packages
library(lavaan)
library(semPlot)
library(qgraph)
library(dplyr)

#set seed for reproducibility
set.seed(100)

#parameters for simulation
REP <- 10L #repetitions
N <- 2000L #sample size

#=-=-CASE SELECTION=-=-=-#
#e.covs are now arranged in a vector of size 10
#input of ii_choice in order: redundancy/zero interaction/synergy
ii_choice = c(0.22, -0.15, -0.39)

#CASE - ZERO INTERACTION
ecov <- rep(-0.15, 9L)

#CASE - REDUNDANCY
#ecov <- rep(0.22, 9L)

#CASE - SYNERGY
#ecov <- rep(-0.39, 9L)

#CASE - CUSTOM
#ecov <- c(0.22, -0.15, -0.39, 0.22, -0.15, -0.39, 0.22, -0.15, -0.39)

#CASE - RANDOM 
#ecov = sample(ii_choice, 9, replace = TRUE)


#=-=-FUNCTIONS=-=-=-#
#Function 1: calculate II from 3x3 correlation matrix
lav_interaction_information_cor_triplet <- function(triplet.cor = NULL) {
  # mi.xy
  cor.xy <- triplet.cor[2,1]
  mi.xy <- -1/2 * log(1 - (cor.xy*cor.xy))
  
  # mi.xy_z
  mi.xy_z <- as.numeric(NA)
  res.cov <- ( triplet.cor[1:2,1:2] -
                 tcrossprod(triplet.cor[1:2,3]) * (1/triplet.cor[3,3]) )
  if(all(diag(res.cov) > 0)) {
    res.cor <- cov2cor(res.cov)[2,1]
    if(abs(res.cor) < 0.999) {
      mi.xy_z <- -1/2 * log(1 - (res.cor*res.cor))
    }
  }
  
  mi.xy_z - mi.xy
}


#=-=-CREATING MODEL VARIABLES-=-=-=-=-=#
#L and T-values
l1 <- sqrt(0.99)
l2 <- sqrt(0.70)
l3 <- sqrt(0.30)

t1 <- 1 - l1^2
t2 <- 1 - l2^2
t3 <- 1 - l3^2

#alphabet string is prepared, substring function to be in used in for-loop to create variable names for the model
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

#for-loop to create the model components
for(i in 1:length(ecov)) {
  #first, take the appropriate capital letter of the alphabet
  letter = substr(alphabet, i, i)
  
  #temporary variables to store the new variable names in
  #first iteration will create A.ecor, A.l2s, ... , second iteration will create B.ecor, B.l2s, ... , etc.
  nam_ecor <- paste(letter, ".ecor", sep="")
  nam_l2s <- paste(letter, ".l2s", sep="")
  nam_l3s <- paste(letter, ".l3s", sep="")
  nam_t2s <- paste(letter, ".t2s", sep="")
  nam_t3s <- paste(letter, ".t3s", sep="")
  
  #convert the string of the variable names ("A.ecor") to actual variables (A.ecor) with numeric values attached to them
  assign(nam_ecor, ecov[i] / (sqrt(t2) * sqrt(t3)))
  assign(nam_l2s, 1 * sqrt(abs(eval(as.name(paste(nam_ecor))))*t2))
  assign(nam_l3s, sign(eval(as.name(paste(nam_ecor)))) * sqrt(abs(eval(as.name(paste(nam_ecor))))*t3))
  assign(nam_t2s, t2 - abs(eval(as.name(paste(nam_ecor))))*t2)
  assign(nam_t3s, t3 - abs(eval(as.name(paste(nam_ecor))))*t3)
  
  #clear the workspace of 'junk' variables after the last iteration
  if(i == length(ecov)) {
    rm(i, letter, nam_ecor, nam_l2s, nam_l3s, nam_t2s, nam_t3s)
  }
}


#=-=-LAVAAN MODEL SYTNAX-=-=-=-=-=-#
pop.model <- c("

    # model A
    A  =~ (", l1, ")*A.x + (", l2, ")*A.y + (", l3, ")*A.z
    A.bf =~ (", A.l2s, ")*A.y + (", A.l3s, ")*A.z

    A.x ~~ (", t1,  ")*A.x
    A.y ~~ (", A.t2s, ")*A.y 
    A.z ~~ (", A.t3s, ")*A.z

    A    ~~ 1*A
    A.bf ~~ 1*A.bf
    A    ~~ 0*A.bf

    # model B
    B  =~ (", l1, ")*B.x + (", l2, ")*B.y + (", l3, ")*B.z
    B.bf =~ (", B.l2s, ")*B.y + (", B.l3s, ")*B.z

    B.x ~~ (", t1,  ")*B.x
    B.y ~~ (", B.t2s, ")*B.y 
    B.z ~~ (", B.t3s, ")*B.z

    B    ~~ 1*B
    B.bf ~~ 1*B.bf
    B.bf ~~ 0*B

    # model C
    C  =~ (", l1, ")*C.x + (", l2, ")*C.y + (", l3, ")*C.z
    C.bf =~ (", C.l2s, ")*C.y + (", C.l3s, ")*C.z

    C.x ~~ (", t1,  ")*C.x
    C.y ~~ (", C.t2s, ")*C.y 
    C.z ~~ (", C.t3s, ")*C.z

    C    ~~ 1*C
    C.bf ~~ 1*C.bf
    C.bf ~~ 0*C
    
    # model D
    D  =~ (", l1, ")*D.x + (", l2, ")*D.y + (", l3, ")*D.z
    D.bf =~ (", D.l2s, ")*D.y + (", D.l3s, ")*D.z

    D.x ~~ (", t1,  ")*D.x
    D.y ~~ (", D.t2s, ")*D.y 
    D.z ~~ (", D.t3s, ")*D.z

    D    ~~ 1*D
    D.bf ~~ 1*D.bf
    D.bf ~~ 0*D
    
    # model E
    E  =~ (", l1, ")*E.x + (", l2, ")*E.y + (", l3, ")*E.z
    E.bf =~ (", E.l2s, ")*E.y + (", E.l3s, ")*E.z

    E.x ~~ (", t1,  ")*E.x
    E.y ~~ (", E.t2s, ")*E.y 
    E.z ~~ (", E.t3s, ")*E.z

    E    ~~ 1*E
    E.bf ~~ 1*E.bf
    E.bf ~~ 0*E
    
    # model F
    F  =~ (", l1, ")*F.x + (", l2, ")*F.y + (", l3, ")*F.z
    F.bf =~ (", F.l2s, ")*F.y + (", F.l3s, ")*F.z

    F.x ~~ (", t1,  ")*F.x
    F.y ~~ (", F.t2s, ")*F.y 
    F.z ~~ (", F.t3s, ")*F.z

    F    ~~ 1*F
    F.bf ~~ 1*F.bf
    F.bf ~~ 0*F
    
    # model G
    G  =~ (", l1, ")*G.x + (", l2, ")*G.y + (", l3, ")*G.z
    G.bf =~ (", G.l2s, ")*G.y + (", G.l3s, ")*G.z

    G.x ~~ (", t1,  ")*G.x
    G.y ~~ (", G.t2s, ")*G.y 
    G.z ~~ (", G.t3s, ")*G.z

    G    ~~ 1*G
    G.bf ~~ 1*G.bf
    G.bf ~~ 0*G
    
    # model H
    H  =~ (", l1, ")*H.x + (", l2, ")*H.y + (", l3, ")*H.z
    H.bf =~ (", H.l2s, ")*H.y + (", H.l3s, ")*H.z

    H.x ~~ (", t1,  ")*H.x
    H.y ~~ (", H.t2s, ")*H.y 
    H.z ~~ (", H.t3s, ")*H.z

    H    ~~ 1*H
    H.bf ~~ 1*H.bf
    H.bf ~~ 0*H
    
    # model I
    I  =~ (", l1, ")*I.x + (", l2, ")*I.y + (", l3, ")*I.z
    I.bf =~ (", I.l2s, ")*I.y + (", I.l3s, ")*I.z

    I.x ~~ (", t1,  ")*I.x
    I.y ~~ (", I.t2s, ")*I.y 
    I.z ~~ (", I.t3s, ")*I.z

    I    ~~ 1*I
    I.bf ~~ 1*I.bf
    I.bf ~~ 0*I
    
    # residual correlations
    A.z ~~ 0.06*B.y
    A.z ~~ 0.06*C.y
    
    B.z ~~ 0.06*A.y
    B.z ~~ 0.06*C.y
    
    C.z ~~ 0.06*A.y
    C.z ~~ 0.06*B.y
    
    
    D.z ~~ 0.06*E.y
    D.z ~~ 0.06*F.y
    
    E.z ~~ 0.06*D.y
    E.z ~~ 0.06*F.y
    
    F.z ~~ 0.06*D.y
    F.z ~~ 0.06*E.y
    
    
    G.z ~~ 0.06*H.y
    G.z ~~ 0.06*I.y
    
    H.z ~~ 0.06*G.y
    H.z ~~ 0.06*I.y
    
    I.z ~~ 0.06*G.y
    I.z ~~ 0.06*H.y
")

fit <- lavaan(pop.model)
Sigma <- lavInspect(fit, "Sigma")

#visualize lavaan SEM
semPaths(fit)


#=-=-BASELINE EDGELIST-=-=-=-=-=-=#
# get indices lower-half of Sigma
idx <- lav_matrix_vech_idx(n = nrow(Sigma), diagonal = FALSE)
node_from  <- col(Sigma)[idx]
node_to    <- row(Sigma)[idx]
# programmed: non-zero edge
programmed <- ifelse(abs(Sigma[idx]) > 0, 1, 0)
# within-cluster edges
set1 <- 1:9
set2 <- 1:9 + 9
set3 <- 1:9 + 9 + 9
in_cluster <- ifelse((node_from %in% set1 & node_to %in% set1) |
                       (node_from %in% set2 & node_to %in% set2) |
                       (node_from %in% set3 & node_to %in% set3), 1, 0)
# create df_edgelist
df_edgelist <- data.frame(node_from, node_to, programmed, in_cluster)


#=-=-SIMULATION PROCESS-=-=-=-=-=-=#
#prepare master dataframe
df_master <- data.frame(node_from=as.integer(),
                        node_to=as.integer(),
                        programmed=as.integer(),
                        in_cluster=as.integer(),
                        weight=as.numeric(),
                        run_id=as.integer())

#prepare list of vectors
ii_list <- list()

#prepare vectors for KPIs
sensitivity_vector <- specificity_vector <- incluster_vector <- outcluster_vector <-
  programmed_vector <- nonprogrammed_vector <- size_vector <- c(numeric(REP))

for(i in 1:length(ecov)){
  #create dynamic string for variable name for vector
  nam_ii <- paste("ii",i,sep="")
  #append vector to list
  ii_list[[i]] <- numeric(REP)
}

for(j in seq_len(REP)) {
  #STEP 1: simulate 'REP' times a dataset of size N and find the correlation matrix
  Data <- simulateData(pop.model, sample.nobs = N)
  COR <- cor(Data)
  
  
  #STEP 2: calculate interaction information per triplet
  #length of ecov also translates in the number of triplets in the model
  for(i in 1:length(ecov)){
    #for each triplet
    #find index numbers to subset the correlation matrix into the relevant 3x3 matrix (per triplet)
    m_low <- ((i-1) * 3) + 1
    m_high <- m_low + 2
    
    #use function lav_interaction_information_cor_triplet
    #and assign for each iteration of REP the value into the 'dynamic value'
    ii_list[[i]][j] <- lav_interaction_information_cor_triplet(COR[m_low:m_high, m_low:m_high])
  }
  
  
  #STEP 3: retrieve edgelist from glasso
  qgraph_glasso <- qgraph(cor(Data), layout="spring", graph="glasso", sampleSize=N, 
                          threshold=0.015, DoNotPlot=TRUE)$Edgelist
  
  glasso_edges <- data.frame(qgraph_glasso$from, qgraph_glasso$to, qgraph_glasso$weight)
  #rename column names to match with df_edgelist
  colnames(glasso_edges) <- c("node_from", "node_to", "weight")
  
  
  #STEP 4: merge the glasso edges with the baseline edgelist, left outer join
  df_edgelist_merged <-merge(x=df_edgelist,y=glasso_edges, all.x=TRUE)
  #add run_id to know from which iteration the data comes from
  df_edgelist_merged$run_id <- j
  
  
  #STEP 5: save results of iteration in master dataframe
  df_master <- rbind(df_master, df_edgelist_merged)
  
  
  #STEP 6: calculate KPIs per iteration
  #prepare a 2x2 matrix for sensitivity/specificity
  kpi_matrix <- matrix(c(0,0,0,0),nrow=2,ncol=2)
  
  # (A) True Positives - programmed = 1 and weight != NA
  kpi_matrix[1,1] <- length(which(df_edgelist_merged$programmed == 1
                                  & !is.na(df_edgelist_merged$weight)))
  
  # (B) False Negatives - programmed = 1 and weight = NA
  kpi_matrix[2,1] <- length(which(df_edgelist_merged$programmed == 1
                                  & is.na(df_edgelist_merged$weight)))
  
  # (C) True Negatives - programmed = 0 and weight = NA
  kpi_matrix[2,2] <- length(which(df_edgelist_merged$programmed == 0
                                  & is.na(df_edgelist_merged$weight)))
  
  # (D) False Positives - programmed = 0 and weight != NA
  kpi_matrix[1,2] <- length(which(df_edgelist_merged$programmed == 0
                                  & !is.na(df_edgelist_merged$weight)))
  
  #calculate sensitivity & specificity
  sensitivity_vector[j] <- kpi_matrix[1,1] / (kpi_matrix[1,1] + kpi_matrix[2,1])
  specificity_vector[j] <- kpi_matrix[2,2] / (kpi_matrix[2,2] + kpi_matrix[1,2])
  
  #calculate percentage of edges found within/outside cluster
  edges_incluster <- length(which(df_edgelist_merged$in_cluster == 1 & !is.na(df_edgelist_merged$weight)))
  incluster_vector[j] <- edges_incluster / length(which(!is.na(df_edgelist_merged$weight)))
  outcluster_vector[j] <- 1 - incluster_vector[j]
  
  #calculate percentage of edges found that were programmed/non-programmed
  #edges programmed: TP / (TP + FP)
  programmed_vector[j] <- kpi_matrix[1,1] / (kpi_matrix[1,1] + kpi_matrix[1,2])
  nonprogrammed_vector[j] <- 1 - programmed_vector[j]
  
  #calculate number of edges to represent size of network
  size_vector[j] <- length(which(!is.na(df_edgelist_merged$weight)))
  
  print(j) #to keep track in console
}

#store KPI vectors into list
list_kpi <- list(size = size_vector, sensitivity = sensitivity_vector, specificity = specificity_vector,
                 '%_programmed' = programmed_vector, '%_nonprogrammed' = nonprogrammed_vector,
                 '%_incluster' = incluster_vector, '%_outcluster' = outcluster_vector)

#convert KPI vectors into data-frame
df_kpi <- as.data.frame(do.call(cbind, list_kpi))
df_kpi <- format(df_kpi, digits=3, nsmall=0)
df_kpi <- sapply(df_kpi, as.numeric)

df_kpi_avg <- colMeans(df_kpi)
df_kpi_avg <- format(df_kpi_avg, digits=3, nsmall=0)


#=-=-RENAMING NODES FROM NUMBERS TO VARIABLE NAMES-=-=-=-=-=-=#
#transform numeric values for nodes in edgelist to actual variable names of model
#sequential order, 1/2/3 are A.x/A.y/A.z, etc.
number <- seq(3*length(ecov))

for(i in 1:length(ecov)) {
  #first, take the appropriate capital letter of the alphabet
  letter <- substr(alphabet, i, i)
  
  #inner loop, create three variables for each iteration
  for (j in 1:3) {
    if (j == 1){
      #create A.x, B.x, etc.
      name <- paste(letter, ".x", sep="")
    }
    else if (j == 2){
      #create A.y, B.y, etc.
      name <- paste(letter, ".y", sep="")
    }
    else {
      #create A.z, B.z, etc.
      name <- paste(letter, ".z", sep="")
    }
    #replace each occurrence of current number (first one in vector) to newly created name
    df_master$node_from[df_master$node_from == number[1]] <- name
    df_edgelist$node_from[df_edgelist$node_from == number[1]] <- name
    #repeat the same for the 'node_to' column
    df_master$node_to[df_master$node_to == number[1]] <- name
    df_edgelist$node_to[df_edgelist$node_to == number[1]] <- name
    #delete first value of vector, similar to number += 1 in Python
    number <- number[-1]
  }
  if (length(number) == 0) { #if all iterations are complete
    #remove 'junk' variables from the workspace
    rm(number, i, j, name, letter, alphabet)
  }
}


#=-=-OVERALL KPIs-=-=-=-=-=-=#
#having calculated KPIs per iteration, now to calculate KPIs from df_master after all iterations
#KPIs could be related to subsets of the master dataframe

# (A) % of replications where particular edge was found (via group by)
#filter the master dataframe with only records including weights
df_master_filtered <- df_master[!is.na(df_master$weight),1:2]

#aggregate edges by number of occurrences, named 'count'
df_master_filtered_agg <- aggregate(df_master_filtered, by=list(df_master_filtered$node_from, df_master_filtered$node_to), 
                                    FUN=length)[1:3]
#rename columns
colnames(df_master_filtered_agg) <- c("node_from", "node_to", "count")

#calculate % of occurrence, named 'occur'
for (i in 1:nrow(df_master_filtered_agg)) {
  df_master_filtered_agg$occur[i] <- df_master_filtered_agg$count[i] / REP
}

#merge grouped-by dataframe of edges with metadata of 'programmed' and 'in_cluster'
df_master_filtered_agg <-merge(x=df_edgelist,y=df_master_filtered_agg, all.x=TRUE)

#some edges may have a 'NA' value for columns 'count' and 'occur', which is plausible
#replace 'NA' in columns 'count' and 'occur' with 0
df_master_filtered_agg$count[is.na(df_master_filtered_agg$count)] <- 0
df_master_filtered_agg$occur[is.na(df_master_filtered_agg$occur)] <- 0

# (A-1) full list of all edges with occurrence, descending order
df_master_filtered_agg_occurlist <- df_master_filtered_agg[order(-df_master_filtered_agg$occur),][,c(1,2,6)]

# (A-2) occurrences split by programmed and non-programmed edges
df_master_filtered_agg_programmed <- df_master_filtered_agg[df_master_filtered_agg$programmed == 1,c(1,2,6)]
#descending sort by occurrence %
df_master_filtered_agg_programmed <- df_master_filtered_agg_programmed[order(-df_master_filtered_agg_programmed$occur),]

df_master_filtered_agg_nonprogrammed <- df_master_filtered_agg[df_master_filtered_agg$programmed == 0,c(1,2,6)]
#descending sort by occurrence %
df_master_filtered_agg_nonprogrammed <- df_master_filtered_agg_nonprogrammed[order(-df_master_filtered_agg_nonprogrammed$occur),]


# (A-3) split by those within and outside cluster
df_master_filtered_agg_incluster <- df_master_filtered_agg[df_master_filtered_agg$in_cluster == 1,c(1,2,6)]
#descending sort by occurrence %
df_master_filtered_agg_incluster <- df_master_filtered_agg_incluster[order(-df_master_filtered_agg_incluster$occur),]

df_master_filtered_agg_outcluster <- df_master_filtered_agg[df_master_filtered_agg$in_cluster == 0,c(1,2,6)]
#descending sort by occurrence %
df_master_filtered_agg_outcluster <- df_master_filtered_agg_outcluster[order(-df_master_filtered_agg_outcluster$occur),]


# (A-4) combination: non-programmed, in cluster
#those that are non-programmed and outside of cluster are already covered by 'outcluster'
#because no edges between clusters would be programmed with intention
df_master_filtered_agg_nonprogrammed_incluster <- df_master_filtered_agg[df_master_filtered_agg$programmed == 0 
                                                                         & df_master_filtered_agg$in_cluster == 1 ,c(1,2,6)]
#descending sort by occurrence %
df_master_filtered_agg_nonprogrammed_incluster <- df_master_filtered_agg_nonprogrammed_incluster[order(-df_master_filtered_agg_nonprogrammed_incluster$occur),]


# (B) overall specificity and sensitivity KPIs
kpi_matrix_agg <- matrix(c(0,0,0,0),nrow=2,ncol=2)

# (1) True Positives - sum of edge counts where programmed = 1
kpi_matrix_agg[1,1] <- length(which(df_master$programmed == 1
                                    & !is.na(df_master$weight)))

# (2) False Negatives - number of records where programmed = 1 and count = 0
kpi_matrix_agg[2,1] <- length(which(df_master$programmed == 1
                                    & is.na(df_master$weight)))

# (3) True Negatives - number of records where programmed = 0 and count = 0
kpi_matrix_agg[2,2] <- length(which(df_master$programmed == 0
                                    & !is.na(df_master$weight)))

# (4) False Positives - sum of edge counts where programmed = 0
kpi_matrix_agg[1,2] <- length(which(df_master$programmed == 0
                                    & is.na(df_master$weight)))

#calculate sensitivity & specificity
sensitivity_agg <- kpi_matrix_agg[1,1] / (kpi_matrix_agg[1,1] + kpi_matrix_agg[2,1])
specificity_agg <- kpi_matrix_agg[2,2] / (kpi_matrix_agg[2,2] + kpi_matrix_agg[1,2])

# (C) Average weight of edges
df_master_filtered_weight <- df_master[!is.na(df_master$weight),c(1,2,5)]
#aggregate edges by average weight
df_master_filtered_weight_agg <- aggregate(df_master_filtered_weight[,3], by=list(df_master_filtered_weight$node_from, 
                                                                                  df_master_filtered_weight$node_to), FUN=mean)
df_master_filtered_weight_agg[,3] <- format(df_master_filtered_weight_agg[,3], digits=3, nsmall=0)
colnames(df_master_filtered_weight_agg) <- c("node_from", "node_to", "avg_weight")

#add metadata about % of occurrences in list
df_master_filtered_weight_agg <- merge(x=df_master_filtered_weight_agg,y=df_master_filtered_agg[,c(1,2,6)], all.x=TRUE)
#descending sort by occurrence %
df_master_filtered_weight_agg <- df_master_filtered_weight_agg[order(-df_master_filtered_weight_agg$occur),]

# (C-2) Filter by only significant edges (< -0.02 or > 0.02)
df_weight_sig <- df_master_filtered_weight_agg[abs(as.numeric(df_master_filtered_weight_agg$avg_weight)) >= 0.02,]


#=-=-CALCULATING II PER TRIPLET-=-=-=-=-=-=#
#preparing dataframe for the output, two columns for scores and description of level of II intended
df_ii <- data.frame(ii_score=as.numeric(9),
                    ii_programmed=character(9),
                    stringsAsFactors = FALSE)

for(i in 1:length(ecov)){
  #add mean II values per triplet
  df_ii$ii_score[i] <- mean(ii_list[[i]])
  
  #add description of programmed intention of level of II
  if (ecov[i] == ii_choice[1]) {
    df_ii$ii_programmed[i] <- "redundancy"
  }
  else if (ecov[i] == ii_choice[2]) {
    df_ii$ii_programmed[i] <- "zero interaction"
  }
  else if (ecov[i] == ii_choice[3]) {
    df_ii$ii_programmed[i] <- "synergy"
  }
  else {
    df_ii$ii_programmed[i] <- "custom"
  }
}

#=-=-FINAL LIST OF RESULTS-=-=-=-=-=-=#
list_results <- list('Iteration Summary (Avg. KPIs)' = df_kpi_avg, 'Iteration KPIs' = df_kpi,
                     'II per Triplet' = df_ii,
                     'Occurrences of All Edges' = df_master_filtered_agg_occurlist,
                     'Occurrences of All Programmed Edges' = df_master_filtered_agg_programmed,
                     'Occurrences of All Non-Programmed Edges' = df_master_filtered_agg_nonprogrammed,
                     'Occurrences of Edges within Cluster' = df_master_filtered_agg_incluster,
                     'Occurrences of Edges between Clusters' = df_master_filtered_agg_outcluster,
                     'Occurrences of Non-Programmed Edges within Cluster' = df_master_filtered_agg_nonprogrammed_incluster,
                     'Avg. Edge Weights & Occurrences' = df_master_filtered_weight_agg,
                     'Only Significant Avg. Edge Weights & Occurrences' = df_weight_sig)

print(list_results$`Iteration Summary (Avg. KPIs)`)
print(list_results$`II per Triplet`)

library(qgraph)
library(bootnet)
net <- estimateNetwork(Data, default = "EBICglasso")
plot(net, layout = "spring",
     palette = "pastel", 
     legend.cex = 0.38, 
     vsize = 6,
     label.cex = 1.25)