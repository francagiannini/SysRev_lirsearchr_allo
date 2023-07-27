#lit search
#install.packages("remotes")
library(remotes)
#install_github("elizagrames/litsearchr", ref="main")
library(litsearchr)
library(ggraph)
library(igraph)
library(tidyverse)


# import from multiple files 
import <-import_results(
    "C:/Users/au710823/OneDrive - Aarhus universitet/sist_meta/on_allo_sr/SysRev_lirsearchr_allo/bib_data/cinp/read/")

# Remove duplicates by title
naiveresults <-
  litsearchr::remove_duplicates(
    import, field = "title", method = "string_osa")

#remove(import)

dim(naiveresults)

stopwrd <- c(get_stopwords("English"),
             c("rights reserved",
               "positively correlated", "negatively correlated",
               "article", 
               "poorly understood",
               "rights reserved",
               #"rice","maize","face", "isotope",
               "significantly", "significant","advances","analyse","analysed","analyses",       
                "analysing","analysis","analyze","analyzes",       
                 "analyzed","analyzing","assess","assessed",       
                 "assesses","assessing","assessment","assessments",    
                  "benefit","based","benefits","change",
                  "changed","changes","changing","characteristic", 
                 "characteristics", "characterize","characterized","characterizes",  
                 "characterizing","clinical","cluster","combine",        
                  "combined","combines","combining","comorbid",       
                  "comorbidity","compare","compared","compares",       
                  "comparing","comparison","control","controlled",     
                  "controlling","controls","design","designed",
                  "designing","effect","effective","effectiveness",  
                  "effects","efficacy","feasible","feasibility",   
                  "follow","followed","following","follows",        
                  "group","groups","impact","intervention",   
                  "interventions","longitudinal","moderate","moderated",      
                  "moderates","moderating","moderator","moderators",     
                  "outcome","outcomes","patient","patients",       
                  "people","pilot","practice","predict",
                  "predicted","predicting","predictor","predictors",     
                  "predicts","preliminary","primary","protocol",       
                  "quality","random","randomise","randomised",     
                  "randomising","randomize","randomized","randomizing",    
                  "rationale","reduce","reduced","reduces" ,       
                 "reducing","related","report","reported",       
                 "reporting","reports","response","responses",
                 "result","resulted","resulting","results",
                 "review","studied","studies","study",          
                 "studying","systematic","treat","treated",        
                 "treating","treatment","treatments","treats",         
                 "trial","trials","versus"#,
               #"united kingdom", "united states", "canada"
               ))

# Identify potential keywords

rakedkeywords <-
  litsearchr::extract_terms(
    text = paste(naiveresults$title, naiveresults$abstract),
    method = "fakerake",
    min_freq = 10,
    ngrams = TRUE,
    min_n = 2,
    max_n = 3,
    language = "English",
    stopwords=stopwrd
  )
# Loading required namespace: stopwords

taggedkeywords <-
  litsearchr::extract_terms(
    keywords = naiveresults$keywords,
    method = "tagged",
    min_freq = 10,
    ngrams = TRUE,
    min_n = 1,
    max_n = 3,
    language = "English",
    stopwords=stopwrd
  )

# Build the keyword co-occurrence network

special_char <- "a1~!@#$%^&*(){}_+:\"<>?,./;'[]-=" 
 
all_keywords <-
  gsub("[[:punct:]]", "", unique(append(taggedkeywords, rakedkeywords)))

#remove(taggedkeywords, rakedkeywords)

length(all_keywords)

all_keywords <- all_keywords[
  !all_keywords %in% c("article", 
                      "poorly understood",
                      "rights reserved",
                      "positively correlated", "negatively correlated"
                      )]

elements <- paste(as.character(naiveresults[, "title"]), 
                  as.character(naiveresults[, "abstract"]))

naivedfm <- create_dfm(
    elements = elements,
    features = all_keywords)

remove(elements)

g <- create_network(naivedfm, min_studies = 3, min_occ = 3)

ggraph(g, layout = "stress") +
  coord_fixed() +
  expand_limits(x = c(-3, 3)) +
  geom_edge_link(aes(alpha = weight)) +
  geom_node_point(shape = "circle filled", fill = "white") +
  geom_node_text(
    aes(label = name, col="keywords"),
    hjust = "outward",
    check_overlap = TRUE
  ) +
  guides(edge_alpha = FALSE)

# Pruning

strengths <- strength(g)

term_strengths <- 
  data.frame(term=names(strengths), strength=strengths, row.names=NULL) |> 
  mutate(rank=rank(strength, ties.method="min")) |> 
  arrange(strength) 
  
term_strengths

cutoff_change <- find_cutoff(g, method="changepoint", knot_num=3)

cutoff_change

cutoff_fig <- ggplot(term_strengths, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=filter(term_strengths, rank>5), 
            hjust="right", 
            nudge_y=20, 
            check_overlap=TRUE, col="red")+theme_bw()


cutoff_fig +
  geom_hline(yintercept=cutoff_change, 
             linetype="dashed")

g_redux <- reduce_graph(g, cutoff_change[2])

selected_terms <- get_keywords(g_redux)

selected_terms

selected_terms_tunned <- c("belowground biomass", "root",
                           "roots",
                           "root biomass"
                           )

match2 <- check_recall(naiveresults[, "abstract"],paste(selected_terms_tunned) )

match2 <- as.data.frame(match2)

sys_results <- bind_cols(naiveresults,match2[,2:3])

library(openxlsx)
write.xlsx(sys_results, 
                "C:/Users/au710823/OneDrive - Aarhus universitet/sist_meta/sys_results_cinp2707.xlsx")

sys_results |> select(author,title,doi,journal,year,Best_Match,Similarity,abstract,times_cited) |> 
  mutate(Similarity=as.numeric(Similarity)) |> 
  arrange(Similarity) |> 
write.xlsx("C:/Users/au710823/OneDrive - Aarhus universitet/sist_meta/sys_results_cinp2707_short.xlsx")


#### Write a search----


# grouped_terms <-list(
#   medication=selected_terms[c(14, 28)],
#   cbt=selected_terms[c(4, 6, 7, 17, 24, 25, 26, 29, 30)],
#   phobia=selected_terms[c(2, 3, 9, 12, 15, 19, 20, 21, 23, 27)]
# )
# 
# grouped_terms
# 
# 
# write_search(
#   grouped_terms,
#   languages="English",
#   exactphrase=TRUE,
#   stemming=FALSE,
#   closure="left",
#   writesearch=TRUE
# )


