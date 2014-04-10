setwd("/Users/MacOwner/Desktop/Cook/SBTree")

####################### NECESSARY PACKAGES AND RESOURCES  ####################### 
library(plyr)
load("tree.rda")

###################### TEST GENERATIONAL AGE DIFF #############################

# This produces year_rltnshp, which is a data.frame with 412 rows from the original tree file
# However, there are 5 columns that list (in order) child name, parent name, year of child,
# year of parent, and the difference between the year of child minus year of parent
# (which should be positive)

year_rltnshp <- data.frame(child= character(), par= character(), child_yr = integer(0), par_yr = integer(0), diff = integer(0), stringsAsFactors=FALSE)

for (i in 1:dim(tree)[1]){
  child = tree$child[i]
  par = tree$parent[i]
  child_yr = tree$year[i]
  par_yr = subset(tree, tree$child == par)$year[1]
  age_diff = child_yr - par_yr
  new_row = c(child, par, child_yr, par_yr, age_diff)
  year_rltnshp[i,] = new_row  
}

# This produces year_rltnshp_noNA, which is a subset of year_rltnshp, except that it only contains
# the 301 rows where there is a calculated value for the column representing the difference between
# child year minus parent year

year_rltnship_noNA = subset(year_rltnshp, year_rltnshp$diff != "NA")

# I did summary of the generation difference for the 301 cases where its value could be calculated.
# Oddly, the minimum value was negative, (I think) meaning that the child is older than the parent!

min(as.numeric(year_rltnship_noNA$diff)) #-16.5
max(as.numeric(year_rltnship_noNA$diff)) #54
mean(as.numeric(year_rltnship_noNA$diff)) #9.174419
median(as.numeric(year_rltnship_noNA$diff)) #8.5
table(as.numeric(year_rltnship_noNA$diff))

#-16.5   -14    -8    -6    -5    -2  -1.5    -1     0     1   1.5     2   2.5     3   3.5 
#1     1     3     1     1     1     1     1     6     4     1    56     3     3     1 
#4   4.5     5   5.5     6   6.5     7  7.25   7.5     8   8.5     9    10  10.5    11 
#5     1    14     8     8     7     9     1     1    12    17    12    15     6    12 
#12  12.5    13  13.5    14  14.5 14.75    15  15.5    16  16.5    17    18  18.5    19 
#8     5    12     4     5     2     1     4     2     1     1     6     3     3     1 
#20  20.5    21    22    23    25    26  27.5    28  28.5    32    33    35    41    43 
#6     1     5     2     2     2     1     1     1     1     2     1     1     1     2 
#46    54 
#1     1

# I created negRltnshp, a subset of the year_rltnship_noNA where the generational difference was less
# than or equal to zero. This resulted in 16 pairs. I also looked at these pairs and tried to determine
# if using the min.repro.year instead from the original tree would fix these problems, but it did not always.

negRltshp = subset(year_rltnship_noNA, year_rltnship_noNA$diff <= 0)
# Note: min.repro.year does not seem to solve the problem
# Ex. Davis * Peking = 1958/1960
#              Davis = 1966/1968 

# This produces year_rltnshp_NA, which is a subset of year_rltnshp, except that it only contains
# the 111 rows where there is no calculated value for the column representing the difference between
# child year minus parent year (instead it is just "NA")

year_rltnship_NA = subset(year_rltnshp, is.na(year_rltnshp$diff))

# To determine why the age difference could not be calculated for these 111 pairs of parent and child,
# I looked at which children and parents had age values of "NA". I first created childNA, which is th
# subset of 10 children with a year value of "NA"

childNA = sort(unique(subset(year_rltnship_NA, is.na(year_rltnship_NA$child_yr))$child))
# [1] "Arksoy 2913"      "IA 3023"          "L37-1355"         "M327"             "N44-92"          
# [6] "N45-2994"         "N45-2994 x Ogden" "N55-3818"         "NC55"             "PI 1945654" 

# I then created parNA, which is th subset of 26 parents with a year value of "NA" (Note: Some of these
# may be due to grammatical error, but many are simply only in the tree as parents and not as children;
# hence they have no year available). See the example of "Biloxi", which is a "father" to D55-4159 and
# D55-4168, but is not listed as a child anywhere (i.e. is not listed with its year)

parNA = sort(unique(subset(year_rltnship_NA, is.na(year_rltnship_NA$par_yr))$par))
# [1] "Amsoy-71"          "Biloxi"            "Dairyland DSR-365" "FC31745"           "Flambeau"         
# [6] "Haberlandt"        "Hale 3"            "J74-47"            "Kanro"             "Korean"           
# [11] "N44-92 x N48-1867" "N45-2994"          "N45-2994 x Ogden"  "No. 171"           "OX383"            
# [16] "Peking"            "PI 171442"         "PI 180501"         "PI171450"          "PI54615-1"        
# [21] "PI86962-1"         "PI86972-1"         "PI88788"           "Ralsoy"            "S 100 x CNS"      
# [26] "Scott" 

# Ex: Biloxi
# > subset(tree, tree$par =="Biloxi")
# [1] child          year           yield          year.imputed   min.repro.year parent.type    parent        
#<0 rows> (or 0-length row.names)
#> subset(tree, tree$parent =="Biloxi")
#child year yield year.imputed min.repro.year parent.type parent
#273 D55-4159 1955    NA        FALSE           1957      father Biloxi
#274 D55-4168 1955    NA        FALSE           1957      father Biloxi



############################# NUMBER OF PARENTS ####################################

# This shows that each child in the "tree" has exactly one father and one mother
# There are 206 unique "child" in the "tree"
for (i in 1:length(count(tree$child)$x)){
  j = which(tree$child == count(tree$child)$x[i])
  k=tree[j[1],]$parent.type
  l=tree[j[2],]$parent.type
  if (k == l){
    print("SAME")
  }
  else{
    print(paste(i, "Two different parents"))
  } 
}

# There are 166 unique "parents" in tree
# These 166 unique parents account for 340 children
# 72 cases have parents of "NA"
# 340 cases have 2 parents
total=0
maxJ=0
listJ = list()
for (i in 1:(length(count(tree$parent)$x))-1){
  j = which(tree$parent == count(tree$parent)$x[i])
  print(paste(i, tree$parent[i],":"))
  print(j)
  listJ <- c(listJ, j)
  total = total + length(j)
}
print(paste("total is:", total))
print(paste("maxJ is:", maxJ))

listJ = as.numeric(listJ)
listJ = sort(listJ)

count(tree$parent=="NA")
which(tree$parent=="NA")

# There are 72 nodes that have a parent as "NA"
length(tree[setdiff(c(1:412),which(tree$parent!="NA")),]$child)

# 50 of those 72 nodes are unique
length(unique(tree[setdiff(c(1:412),which(tree$parent!="NA")),]$child))

table(tree[setdiff(c(1:412),which(tree$parent!="NA")),]$child)
# 22 nodes have both parents as "NA", 28 nodes have one parent as "NA"
table(table(tree[setdiff(c(1:412),which(tree$parent!="NA")),]$child))

# 206 soy beans
# - Each has two parent$ID
# - 170 soy beans have 2 parents
# - 14 soy beans have 1 parent
# - 22 soy beans have 0 parents

############################# NUMBER OF CHILDREN ####################################

tab=table(tree$parent)
table(tab)

# 165 soy beans have at least one child
#   1   2   3   4   5   6   7   8   9  10  12 
# 102  30  10   5   6   3   1   4   1   2   1

for (i in 1:165){
  if (tab[[i]] == 12){
    print(tab[i])
  } 
}

################## TEST TOTAL NODES IN ALL-ENCOMPASSING TREE ###########################

# There are 206 unique "child" in the "tree"
uniqueChild = sort(unique(tree$child))
# There are 165 unique "parent" in the "tree"
uniqueParent = sort(unique(tree$parent))
# There are 230 unique nodes in the "tree"
uniqueNode = sort(union(uniqueChild,uniqueParent))

# These lines were used to generate the nodes from the two large trees (Tokyo and AK_004).
# The resulting variables were saved to the working directory
#tokyoNodes = vals$df
#AK004Nodes = vals$df
#save(tokyoNodes, file="tokyoNodes")
#save(AK004Nodes, file="AK004Nodes")

load("tokyoNodes")
load("AK004Nodes")

# There are 91 unique child in "Tokyo"
uniqueTokyoChild = unique(tokyoNodes$label)
# There are 61 unique parents in "Tokyo"
uniqueTokyoParent = unique(tokyoNodes$root)
# There are 91 unique nodes in "Tokyo"
uniqueTokyoNode = union(uniqueTokyoChild, uniqueTokyoParent)

# There are 104 unique child in "AK_004"
uniqueAK004Child = unique(AK004Nodes$label)
# There are 67 unique child in "AK_004"
uniqueAK004Parent = unique(AK004Nodes$root)
# There are 104 unique nodes in "AK_004"
uniqueAK004Node = union(uniqueAK004Child, uniqueAK004Parent)

# There are 143 unique nodes in "AK_004" and "Tokyo"
tokyoAK004Union = sort(union(uniqueTokyoNode,uniqueAK004Node))

# There are 52 nodes in the intersection
tokyoAK004Int = intersect(uniqueTokyoNode, uniqueAK004Node)

# There are 87 nodes not in Tokyo and AK_004 (230-143)
notInTokyoAK004 = uniqueNode[!(uniqueNode %in% tokyoAK004Union)]
