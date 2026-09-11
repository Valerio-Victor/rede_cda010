library(magrittr)
library(visNetwork)

googlesheets4::gs4_auth(
  email = Sys.getenv("GOOGLE_EMAIL")
)

bd_bruto <- googlesheets4::read_sheet(
  ss = Sys.getenv("GOOGLE_SHEET_ID")
) %>% 
  janitor::clean_names()

colnames(bd_bruto) <- c('carimbo','votante',
                        'idade','genero',
                        'jeitinho','hierarquia',
                        'cotas','conservadorismo',
                        'indicado_01','indicado_02')

nos <- bd_bruto %>% 
  dplyr::select(
    votante, idade, genero,
    jeitinho, hierarquia,
    cotas, conservadorismo) %>% 
  dplyr::mutate(
    jeitinho_c = ifelse(jeitinho >= 4, 'Favorável','Contrário'), 
    hierarquia_c = ifelse(hierarquia >= 4, 'Favorável','Contrário'),
    cotas_c = ifelse(cotas >= 4, 'Favorável','Contrário'), 
    conservadorismo_c = ifelse(conservadorismo >= 4, 'Favorável','Contrário')) %>% 
  dplyr::rename('id' = votante) %>% 
  dplyr::mutate(color = dplyr::case_when(
    genero == 'Mulher cisgênero' ~ '#EAD7B7',
    TRUE ~ '#2F4F4F'),
    title = id)

arestas <- bd_bruto %>% 
  dplyr::select(votante, indicado_01, indicado_02) %>% 
  tidyr::pivot_longer(cols = -votante,
                      names_to = 'ref',
                      values_to = 'indicador') %>% 
  dplyr::transmute(from = votante,
                   to = indicador) %>% 
  dplyr::filter(to != 'CAUE MORAIS DA SILVA') %>% 
  dplyr::filter(to != 'ITALO DIAS REIS')

visNetwork(nodes = nos_genero, 
           edges = arestas) %>% 
  visEdges(arrows = "to") %>% 
  visPhysics(stabilization = TRUE)

######
library(igraph)

rede <- igraph::graph_from_data_frame(
  d = arestas,
  directed = TRUE,
  vertices = nos
)

igraph::assortativity(
  rede,
  values = igraph::V(rede)$jeitinho,
  directed = TRUE
)

igraph::assortativity(
  rede,
  values = igraph::V(rede)$hierarquia,
  directed = TRUE
)

igraph::assortativity(
  rede,
  values = igraph::V(rede)$cotas,
  directed = TRUE
)

igraph::assortativity(
  rede,
  values = igraph::V(rede)$conservadorismo,
  directed = TRUE
)

igraph::assortativity(
  rede,
  values = igraph::V(rede)$idade,
  directed = TRUE
)

genero <- as.integer(
  as.factor(igraph::V(rede)$genero)
)

igraph::assortativity_nominal(
  rede,
  types = genero,
  directed = TRUE
)

# Indicações recebidas
# Indicações recebidas
grau_entrada <- igraph::degree(rede, mode = "in")

# Indicações realizadas
grau_saida <- igraph::degree(rede, mode = "out")

# Total de conexões
grau_total <- igraph::degree(rede, mode = "all")

graus <- data.frame(
  estudante = names(grau_entrada),
  grau_entrada = grau_entrada,
  grau_saida = grau_saida,
  grau_total = grau_total
)


