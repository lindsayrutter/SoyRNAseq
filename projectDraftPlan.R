\documentclass{article}
\usepackage{float, hyperref}
\usepackage[margin=1in]{geometry}
\usepackage{graphicx}

\begin{document}

\author{Lindsay Rutter}
\title{Stat.585X: Draft of Project Plan}

\maketitle

<<options, echo=FALSE>>=
  opts_chunk$set(cache=TRUE)
@

\section*{Project Topic}

\begin{itemize}

\item There are many soybean nodes that do not have a year ({\tt NA}). I was thinking of simply taking the average of all relationships and using that for these cases (which seems to be ~9 years). 

<<>>=
  setwd("/Users/MacOwner/Desktop/Cook/SBTree")

####################### NECESSARY PACKAGES AND RESOURCES  ####################### 
library(plyr)
load("tree.rda")

###################### TEST GENERATIONAL AGE DIFF #############################

# This produces year_rltnshp, which is a data.frame with 412 rows from the original tree file However, there are 5 columns that list (in order) child name, parent name, year of child, year of parent, and the difference between the year of child minus year of parent (which should be positive)

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

# This produces year_rltnshp_noNA, which is a subset of year_rltnshp, except that it only contains the 301 rows where there is a calculated value for the column representing the difference between child year minus parent year

year_rltnship_noNA = subset(year_rltnshp, year_rltnshp$diff != "NA")

# I did summary of the generation difference for the 301 cases where its value could be calculated. Oddly, the minimum value was negative, (I think) meaning that the child is older than the parent!

min(as.numeric(year_rltnship_noNA$diff)) #-16.5
max(as.numeric(year_rltnship_noNA$diff)) #54
mean(as.numeric(year_rltnship_noNA$diff)) #9.174419
@

\item However, I run into an issue because there are 16 cases where the parent {\tt year} is younger than (or equal to) the child {\tt year}. For these cases, I also looked at the variable {\tt min.repro.year} in {\tt tree.rda}, but that did not solve it. 

<<>>=
  # I created negRltnshp, a subset of the year_rltnship_noNA where the generational difference was less than or equal to zero. This resulted in 16 pairs. I also looked at these pairs and tried to determine if using the min.repro.year instead from the original tree would fix these problems, but it did not always.
  
  negRltshp = subset(year_rltnship_noNA, year_rltnship_noNA$diff <= 0)
@

\item What I did to discover this issue is in {\tt testTree.R}. The 16 cases are stored in the variable {\tt negRltshp}.

<<>>=
  negRltshp
@

\item Susan will look into it, and suggested I work more on the algorithm of combining varieties. 

\end{itemize}

\section*{3.14.2014 Meeting}

\begin{itemize}

\item Worked on combining two varieties {\tt Murden} and {\tt Merit} by hard-coding

\item Disregarded x-axis positioning by {\tt year} simply kept it at {\tt generation} for now

\item Added ~50 lines of code in {\tt Non-reactiveGenealogyPlot-4-Lindsay.R} and commented out/changed several lines of code in {\tt SelectGenealogy.R}

\item Excepting one soy bean {\tt Capital}, the varieties seemed to abide by correct x-axis positions by generations. However, two soy beans were at the same y-value {\tt No.171} and {\tt Richland}

\item The connecting lines in the combined graph were sometimes absent, and even incorrect. I might need to rewrite more parts of code from scratch, rather than tailoring the present code.

\begin{figure}[ht!]
\centering
\includegraphics[width=90mm]{Images/MukdenMerit.png}
\caption{Mukden and Merit}
\label{overflow}
\end{figure}

\begin{figure}[ht!]
\centering
\includegraphics[width=90mm]{Images/MukdenMeritCombined.png}
\caption{Mukden and Merit Combined}
\label{overflow}
\end{figure}

\item Dr. Cook suggested that I work from the other direction. That is, while I had considered merging varietes from bottom-up, maybe I should merge varieties from top-bottom, by first creating one large, all-encompossing tree from grandparent soy bean lines {\tt Tokyo} and {\tt AK\_004}, and then erasing edges and nodes that do not belong to the inputted varieties.

\item Dr. Cook suggested that I update my notes in markdown language instead of a simple plain text document

\end{itemize}

\section*{3.25.2014 Meeting}

\begin{itemize}

\item Susan explained that the year introduced and year registered can be different by up to 30 years for a given variety, and that this can explain the cases of children being older than their parents

\item Dr. Cook suggested that we throw a flag (message) to the user if a child is older than the parent, and indicate to them that the years were forced (imputated) such that the variety is younger than its parents and older than its children

\item Susan suggested reviewing the script
\\{\tt Shiny/Cultivar-genealogy/pedigree-diagram-expt/CombineSoybeanGenealogyCSVs.R}

\item Dr. Cook suggested to use the Git account (Genealogy information can all be placed there as it is published data. However, the yield data is unpublished private data and should be in Dropbox)

\item Dr. Cook suggested
\begin{itemize}
\item Develop a layout of the whole tree
\item Determine which names of the union of {\tt Tokyo} and {\tt AK\_004} are not in the whole tree
\item Create a function the determines whether or not one can get from one variety to another variety via some connection between parents and children (like the six-degrees of separation problem). If a path exists, it could also show the directions to get from one to the other.
\end{itemize}
\end{itemize}

\end{document}