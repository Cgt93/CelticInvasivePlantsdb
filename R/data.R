#Primero las dependencias:
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(purrr)
library(systemfonts)


#' @title Raw Celtic Invasive Plants database (CIPdb)
#' @return Raw Celtic Invasive Plants database loaded from a CSV (tab separated) file as table object.
#' @references
#'González-Toral, C., Madrazo-Frías, L., Estrada Fernández, A., López-Alonso, R., Sanna, M., Cuesta, C., Cires, E. & Viruel, J. (202) Celtic Invasive Plants database. Version December 2025. Zenodo.org https://doi.org/10.5281/zenodo.17871899
#' @examples
#' CIPdb <- CIPdb()
#' head(CIPdb)
#' ##Or
#' Data = CIPdb()
#' head(Data)
#'
#' @export


CIPdb <- function() {
  df <- read.csv(system.file("extdata", "DB_V6_APRIL_2025_LABELS.csv", package = "CelticInvasivePlantsdb"),
                 header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  df <- df %>%
    mutate(across(where(is.character), ~ ifelse(.x == "" | .x == "nan", NA_character_, .x)))
  return(df)
}




#' @title  Description table of Celtic Invasive Plants Checklist (Description_CIPdb)
#' @return This functions provides a table describing the content of all the columns formind the  Celtic Invasive Plants database. This is table object.
#' @references
#'Biodiversity in Ireland, 2025. Invasive species of Ireland. Biodiversity in Ireland. Maps. https://maps.biodiversityireland.ie/Species (accessed 12.6.25).
#'Boyle, B., Hopkins, N., Lu, Z., Raygoza Garay, J.A., Mozzherin, D., Rees, T., Matasci, N., Narro, M.L., Piel, W.H., Mckay, S.J., Lowry, S., Freeland, C., Peet, R.K., Enquist, B.J., 2013. The taxonomic name resolution service: An online tool for automated standardization of plant names. BMC Bioinformatics 14, 1–15. https://doi.org/10.1186/1471-2105-14-16
#'Boyle, B.L., Matasci, N., Mozzherin, D., Rees, T., Barbosa, G.C., Kumar Sajja, R., Enquist, B.J., 2021. Taxonomic Name Resolution Service, version 5.1 . Botanical Information and Ecology Network. https://tnrs.biendata.org/ (accessed 12.6.25).
#'Department for Environment Food & Rural Affairs and Animal and Plant Health, 2024. Invasive non-native (alien) plant species: rules in England and Wales. Gov.UK. https://www.gov.uk/guidance/invasive-non-native-alien-plant-species-rules-in-england-and-wales#list-of-invasive-plant-species (accessed 12.6.25).
#'Dudley, N. (Ed.), 2008. Guidelines for Applying Protected Area Management Categories. IUCN Publications Services, Gland, Switzerland.
#'European Commission, 2016. Commission Implementing Regulation (EU) 2016/1141 of 13 July 2016 adopting a list of invasive alien species of Union concern pursuant to Regulation (EU) No 1143/2014 of the European Parliament and of the Council. OJ L 189, 14.7.2016, pp. 4–8. C/2016/4295. http://data.europa.eu/eli/reg_impl/2016/1141/oj
#'European Commission, 2017. Commission Implementing Regulation (EU) 2017/1263 of 12 July 2017 updating the list of invasive alien species of Union concern established by Implementing Regulation (EU) 2016/1141 pursuant to Regulation (EU) No 1143/2014 of the European Parliament and of the Council. OJ L 182, 13.7.2017, pp. 37–39. C/2017/4755. http://data.europa.eu/eli/reg_impl/2017/1263/oj
#'European Commission, 2019. Commission Implementing Regulation (EU) 2019/1262 of 25 July 2019 amending Implementing Regulation (EU) 2016/1141 to update the list of invasive alien species of Union concern. OJ L 199, 26.7.2019, pp. 1–4. C/2019/5360. http://data.europa.eu/eli/reg_impl/2019/1262/oj
#'European Commission, 2022. Commission Implementing Regulation (EU) 2022/1203 of 12 July 2022 amending Implementing Regulation (EU) 2016/1141 to update the list of invasive alien species of Union concern. OJ L 186, 13.7.2022, pp. 10–13. C/2022/4773. http://data.europa.eu/eli/reg_impl/2022/1203/oj
#'European Commission, 2025. Commission Implementing Regulation (EU) 2025/1422 of 17 July 2025 amending Implementing Regulation (EU) 2016/1141 to update the list of invasive alien species of Union concern. OJ L, 2025/1422, 18.7.2025. C/2025/4769. http://data.europa.eu/eli/reg_impl/2025/1422/oj
#'European Union, 1992. Council Directive 92/43/EEC of 21 May 1992 on the conservation of natural habitats and of wild fauna and flora. OJ L 206, 22.7.1992, pp. 7–50. http://data.europa.eu/eli/dir/1992/43/oj
#'European Union, 2009. Directive 2009/147/EC of the European Parliament and of the Council of 30 November 2009 on the conservation of wild birds (Codified version). OJ L 20, 26.1.2010, pp. 7–25. http://data.europa.eu/eli/dir/2009/147/oj
#'European Environment Agency (EEA), 2025a. Emerald Network data (vector) - the Pan-European network of protected sites version 2024 https://doi.org/10.2909/135a0bb6-c611-4c2c-823d-a564be119ad8
#'European Environment Agency (EEA), 2024. Nationally designated areas for public access (vector data) - May 2024 https://doi.org/10.2909/616ef48f-7196-4e30-b201-6c97808fa68a
#'European Environment Agency (EEA), 2025b. Natura 2000 (tabular) - version end 2023 https://www.eea.europa.eu/en/datahub/datahubitem-view/6fc8ad2d-195d-40f4-bdec-576e7d1268e4
#'Fernández Prieto, J.A., Amigo, J., Bueno, A., Herrera, M., Rodríguez-Guitián, M.A., Loidi, J., 2020. Notas sobre el Catálogo de comunidades de plantas vasculares de los territorios iberoatlánticos (I). Nat. Cantab. 8, 17–37. https://www.indurot.uniovi.es/actividades/publicaciones/caturalia-cantabricae/volumen-8
#'GB non-native species secretariat 2025. Non-Native Species Secretariat (NNSS) Species of Special Concern. Non-Native Species Secretariat (NNSS). https://www.nonnativespecies.org/legislation/species-of-special-concern#List-plants (accessed 12.3.25).
#'Govaerts, R. (ed.), 2023. WCVP: World Checklist of Vascular Plants, Version 12. Royal Botanic Gardens, Kew. https://sftp.kew.org/pub/data-repositories/WCVP/ (accessed 12.7.25).
#'Govaerts, R., Nic Lughadha, E., Black, N., Turner, R., Paton, A., 2021. The World Checklist of Vascular Plants, a continuously updated resource for exploring global plant diversity. Sci. Data 8, 1–10. https://doi.org/10.1038/s41597-021-00997-6
#'Instituto Geográfico Nacional, 2024. España. Regiones biogeográficas.Instituto Geográfico Nacional. Centro de descargas. URL https://centrodedescargas.cnig.es/CentroDescargas/busquedaRedirigida.do?ruta=PUBLICACION_CNIG_DATOS_VARIOS/aneTematico/Espana_Regiones-biogeograficas_2024_mapa_19246_spa.zip (accessed 11.1.25).
#'Inventaire National du Patrimoine Naturel (INPN), 2025. ERéférentiel taxonomique des espèces des territoires français. Référentiel taxonomique (Tax Ref) version 18. https://www.patrinat.fr/fr/page-temporaire-de-telechargement-des-referentiels-de-donnees-lies-linpn-7353 (accessed 12.6.25).
#'Minister for Arts Heritage and the Gaeltacht, 2011. European Communities (Birds and Natural Habitats) Regulations 2011. Wt. (B28719). 500. 9/11. https://www.irishstatutebook.ie/eli/2011/si/477
#'Minister for Housing Local Government and Heritage, 2024. Statutory Instruments. European Union (Invasive Alien Species) Regulations 2024. Iris Oifigiúil (IEAD-1) 30. 7/24. Propylon. https://www.irishstatutebook.ie/eli/2024/si/374/made/en/print
#'Ministère de la Transition Écologique et Solidaire, 2018. Arrêté du 14 février 2018 relatif à la prévention de l’introduction et de la propagation des espèces végétales exotiques envahissantes sur le territoire métropolitain. JORF n°0044 du 22 février 2018.NOR : TREL1704132A. https://www.legifrance.gouv.fr/loda/id/JORFTEXT000036629837/
#'Ministère de la Transition Écologique et Solidaire, 2020. Arrêté du 10 mars 2020 portant mise à jour de la liste des espèces animales et végétales exotiques envahissantes sur le territoire métropolitain. JORF n°0118 du 14 mai 2020, Texte n° 7. NOR : TREL1924265A. https://www.legifrance.gouv.fr/jorf/id/JORFTEXT000041875937
#'Ministeriet for Fødevarer Landbrug og Fiskeri, 2018. Bekendtgørelse om forebyggelse og håndtering af introduktion og spredning af invasive ikkehjemmehørende arter på EU-listen og om en national liste med handelsforbud m.v. over for invasive arter. BEK nr 1285 af 12/11/2018. https://www.retsinformation.dk/eli/lta/2018/1285
#'Ministeriet for Grøn Trepart, 2025. De invasive arter. De invasive artslister. Arter. https://sgavmst.dk/arter/artsforvaltning/invasive-arter/de-invasive-arter (accessed 12.5.25).
#'Ministerio para la Transición Ecológica, 2019. Real Decreto 216/2019, de 29 de marzo, por el que se aprueba la lista de especies exóticas invasoras preocupantes para la región ultraperiférica de las islas Canarias y por el que se modifica el Real Decreto 630/2013, de 2 de agosto, por el que se regula el Catálogo español de especies exóticas invasoras. «BOE» núm. 77, de 30/03/2019. BOE-A-2019-4675. https://www.boe.es/eli/es/rd/2019/03/29/216/con
#'Ministerio para la Transición Ecológica y el Reto Demográfico, 2020. Orden TED/1126/2020, de 20 de noviembre, por la que se modifica el Anexo del Real Decreto 139/2011, de 4 de febrero, para el desarrollo del Listado de Especies Silvestres en Régimen de Protección Especial y del Catálogo Español de Especies Amenazadas, y el Anexo del Real Decreto 630/2013, de 2 de agosto, por el que se regula el Catálogo Español de Especies Exóticas Invasoras.BOE-A-2020-15296. «BOE» núm. 314, de 1 de diciembre de 2020, páginas 108167 a 108171 (5 págs.). https://www.boe.es/eli/es/o/2020/11/20/ted1126
#'Ministerio para la Transición Ecológica y el Reto Demográfico, 2023a. Orden TED/339/2023, de 30 de marzo, por la que se modifica el anexo del Real Decreto 139/2011, de 4 de febrero, para el desarrollo del Listado de Especies Silvestres en Régimen de Protección Especial y del Catálogo Español de Especies Amenazadas, y el anexo del Real Decreto 630/2013, de 2 de agosto, por el que se regula el Catálogo Español de Especies Exóticas Invasoras.«BOE» núm. 83, de 7 de abril de 2023, páginas 50910 a 50915 (6 págs.). BOE-A-2023-8751. https://www.boe.es/eli/es/o/2023/03/30/ted339
#'Ministerio para la Transición Ecológica y el Reto Demográfico, 2023b. Catálogo Español de Especies Exóticas Invasoras. MITECO. URL https://www.miteco.gob.es/es/biodiversidad/temas/conservacion-de-especies/especies-exoticas-invasoras/ce-eei-catalogo.aspx (accessed 6.11.23).
#'National Biodiversity Data Centre Of the Republic of Ireland, 2023. Discrete vascular plant surveys. Data.Gov.IE. https://data.gov.ie/dataset/discrete-vascular-plant-surveys (accessed 3.10.23).
#'Presidência do Conselho de Ministros Ambiente e Transição Energética, 2019. Assegura a execução, na ordem jurídica nacional, do Regulamento (UE) n.o 1143/2014, estabelecendo o regime jurídico aplicável ao controlo, à detenção, à introdução na natureza e ao repovoamento de espécies exóticas da flora e da fauna. Diário da República n.º 130/2019, Série I de 2019-07-10. Decreto-Lei n.º 92/2019. https://diariodarepublica.pt/dr/legislacao-consolidada/decreto-lei/2019-124568069
#'Rees, T., 2014. Taxamatch, an Algorithm for Near (‘Fuzzy’) Matching of Scientific Names in Taxonomic Databases. PLoS One 9, e107510. https://doi.org/10.1371/journal.pone.0107510
#'Rivas-Martínez, S., Penas, A., Díaz; T. E., 2001. Biogeographic map of Europe. Cartographic Service University of León, León.
#'Royal Botanic Gardens Kew, 2025. Plants of the World Online (POWO). Facilitated by the Royal Botanic Gardens, Kew. http://www.plantsoftheworldonline.org/ (accessed 4.1.25).
#'Royal Horticultural Society (RHS), 2025. Invasive plants covered by legislation. RHS.org https://www.rhs.org.uk/prevention-protection/invasive-non-native-plants (accessed 12.4.25).
#'Stolton, S., Shadie, P., Dudley, N., 2013. IUCN WCPA Best Practice Guidance on Recognising Protected Areas and Assigning Management Categories and Governance Types. Best Practice Protected Area Guidelines Series 21. https://portals.iucn.org/library/sites/library/files/documents/pag-021.pdf
#'The Angiosperm Phylogeny Group, 2016. An update of the Angiosperm Phylogeny Group classification for the orders and families of flowering plants: APG IV. Bot. J. Linn. Soc. 181, 399–436. https://doi.org/10.1111/boj.12385
#'The World Flora Online Consortium, Elliott, A., Hyam, R., Ulate, W., 2023. World Flora Online Plant List June 2023. Version 2023-06. Zenodo.org. https://zenodo.org/records/8079052 (accessed 12.7.25). https://doi.org/10.5281/zenodo.8079052
#'Thomas, S., 2011. Natural England Commissioned Report NECR053: Horizon-scanning for invasive non-native plants in Great Britain (NECR053). Natural England. https://publications.naturalengland.org.uk/publication/40015
#' @examples
#' CIP_Description_ <- Description_CIPdb()
#' head(Description_CIPdb)
#'
#' @export

Description_CIPdb <- function() {
  df <- read.csv(system.file("extdata", "CIP_DB_APRIL_2025_DATA_DESCRIPTION.csv", package = "CelticInvasivePlantsdb"),
                 header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  df <- df %>%
    mutate(across(where(is.character), ~ ifelse(.x == "" | .x == "nan", NA_character_, .x)))
  return(df)
}




#' @title  Raw Celtic Invasive Plants Checklist (CIP_Checklist)
#' @return Raw Celtic Invasive Plants checklist loaded from a CSV (tab separated) file as table object.
#' @references
#' EU_Concern:
#'
#'European Commission, 2016. Commission Implementing Regulation (EU) 2016/1141 of 13 July 2016 adopting a list of invasive alien species of Union concern pursuant to Regulation (EU) No 1143/2014 of the European Parliament and of the Council. OJ L 189, 14.7.2016, pp. 4–8. C/2016/4295. http://data.europa.eu/eli/reg_impl/2016/1141/oj
#'European Commission, 2017. Commission Implementing Regulation (EU) 2017/1263 of 12 July 2017 updating the list of invasive alien species of Union concern established by Implementing Regulation (EU) 2016/1141 pursuant to Regulation (EU) No 1143/2014 of the European Parliament and of the Council. OJ L 182, 13.7.2017, pp. 37–39. C/2017/4755. http://data.europa.eu/eli/reg_impl/2017/1263/oj
#'European Commission, 2019. Commission Implementing Regulation (EU) 2019/1262 of 25 July 2019 amending Implementing Regulation (EU) 2016/1141 to update the list of invasive alien species of Union concern. OJ L 199, 26.7.2019, pp. 1–4. C/2019/5360. http://data.europa.eu/eli/reg_impl/2019/1262/oj
#'European Commission, 2022. Commission Implementing Regulation (EU) 2022/1203 of 12 July 2022 amending Implementing Regulation (EU) 2016/1141 to update the list of invasive alien species of Union concern. OJ L 186, 13.7.2022, pp. 10–13. C/2022/4773. http://data.europa.eu/eli/reg_impl/2022/1203/oj
#'European Commission, 2025. Commission Implementing Regulation (EU) 2025/1422 of 17 July 2025 amending Implementing Regulation (EU) 2016/1141 to update the list of invasive alien species of Union concern. OJ L, 2025/1422, 18.7.2025. C/2025/4769. http://data.europa.eu/eli/reg_impl/2025/1422/oj
#'
#' Official checklist Portugal:
#'
#' Presidência do Conselho de Ministros Ambiente e Transição Energética, 2019. Assegura a execução, na ordem jurídica nacional, do Regulamento (UE) n.o 1143/2014, estabelecendo o regime jurídico aplicável ao controlo, à detenção, à introdução na natureza e ao repovoamento de espécies exóticas da flora e da fauna. Diário da República n.º 130/2019, Série I de 2019-07-10. Decreto-Lei n.º 92/2019. https://diariodarepublica.pt/dr/legislacao-consolidada/decreto-lei/2019-124568069
#'
#' Official checklist Spain:
#'
#'Ministerio para la Transición Ecológica, 2019. Real Decreto 216/2019, de 29 de marzo, por el que se aprueba la lista de especies exóticas invasoras preocupantes para la región ultraperiférica de las islas Canarias y por el que se modifica el Real Decreto 630/2013, de 2 de agosto, por el que se regula el Catálogo español de especies exóticas invasoras. «BOE» núm. 77, de 30/03/2019. BOE-A-2019-4675. https://www.boe.es/eli/es/rd/2019/03/29/216/con
#'Ministerio para la Transición Ecológica y el Reto Demográfico, 2020. Orden TED/1126/2020, de 20 de noviembre, por la que se modifica el Anexo del Real Decreto 139/2011, de 4 de febrero, para el desarrollo del Listado de Especies Silvestres en Régimen de Protección Especial y del Catálogo Español de Especies Amenazadas, y el Anexo del Real Decreto 630/2013, de 2 de agosto, por el que se regula el Catálogo Español de Especies Exóticas Invasoras.BOE-A-2020-15296. «BOE» núm. 314, de 1 de diciembre de 2020, páginas 108167 a 108171 (5 págs.). https://www.boe.es/eli/es/o/2020/11/20/ted1126
#'Ministerio para la Transición Ecológica y el Reto Demográfico, 2023a. Orden TED/339/2023, de 30 de marzo, por la que se modifica el anexo del Real Decreto 139/2011, de 4 de febrero, para el desarrollo del Listado de Especies Silvestres en Régimen de Protección Especial y del Catálogo Español de Especies Amenazadas, y el anexo del Real Decreto 630/2013, de 2 de agosto, por el que se regula el Catálogo Español de Especies Exóticas Invasoras.«BOE» núm. 83, de 7 de abril de 2023, páginas 50910 a 50915 (6 págs.). BOE-A-2023-8751. https://www.boe.es/eli/es/o/2023/03/30/ted339
#'Ministerio para la Transición Ecológica y el Reto Demográfico, 2023b. Catálogo Español de Especies Exóticas Invasoras. MITECO. URL https://www.miteco.gob.es/es/biodiversidad/temas/conservacion-de-especies/especies-exoticas-invasoras/ce-eei-catalogo.aspx (accessed 6.11.23).
#'
#' Official checklist France:
#'
#'Inventaire National du Patrimoine Naturel (INPN), 2025. ERéférentiel taxonomique des espèces des territoires français. Référentiel taxonomique (Tax Ref) version 18. https://www.patrinat.fr/fr/page-temporaire-de-telechargement-des-referentiels-de-donnees-lies-linpn-7353 (accessed 12.6.25).
#'Ministère de la Transition Écologique et Solidaire, 2018. Arrêté du 14 février 2018 relatif à la prévention de l’introduction et de la propagation des espèces végétales exotiques envahissantes sur le territoire métropolitain. JORF n°0044 du 22 février 2018.NOR : TREL1704132A. https://www.legifrance.gouv.fr/loda/id/JORFTEXT000036629837/
#'Ministère de la Transition Écologique et Solidaire, 2020. Arrêté du 10 mars 2020 portant mise à jour de la liste des espèces animales et végétales exotiques envahissantes sur le territoire métropolitain. JORF n°0118 du 14 mai 2020, Texte n° 7. NOR : TREL1924265A. https://www.legifrance.gouv.fr/jorf/id/JORFTEXT000041875937
#'
#' Official checklist UK:
#'
#'Department for Environment Food & Rural Affairs and Animal and Plant Health, 2024. Invasive non-native (alien) plant species: rules in England and Wales. Gov.UK. https://www.gov.uk/guidance/invasive-non-native-alien-plant-species-rules-in-england-and-wales#list-of-invasive-plant-species (accessed 12.6.25).
#'GB non-native species secretariat 2025. Non-Native Species Secretariat (NNSS) Species of Special Concern. Non-Native Species Secretariat (NNSS). https://www.nonnativespecies.org/legislation/species-of-special-concern#List-plants (accessed 12.3.25).
#'Royal Horticultural Society (RHS), 2025. Invasive plants covered by legislation. RHS.org https://www.rhs.org.uk/prevention-protection/invasive-non-native-plants (accessed 12.4.25).
#'Thomas, S., 2011. Natural England Commissioned Report NECR053: Horizon-scanning for invasive non-native plants in Great Britain (NECR053). Natural England. https://publications.naturalengland.org.uk/publication/40015
#'
#' Official checklist Republic of Ireland:
#'
#'Biodiversity in Ireland, 2025. Invasive species of Ireland. Biodiversity in Ireland. Maps. https://maps.biodiversityireland.ie/Species (accessed 12.6.25).
#'Minister for Arts Heritage and the Gaeltacht, 2011. European Communities (Birds and Natural Habitats) Regulations 2011. Wt. (B28719). 500. 9/11. https://www.irishstatutebook.ie/eli/2011/si/477
#'Minister for Housing Local Government and Heritage, 2024. Statutory Instruments. European Union (Invasive Alien Species) Regulations 2024. Iris Oifigiúil (IEAD-1) 30. 7/24. Propylon. https://www.irishstatutebook.ie/eli/2024/si/374/made/en/print
#'
#' Official checklist Denmark:
#'
#'Ministeriet for Fødevarer Landbrug og Fiskeri, 2018. Bekendtgørelse om forebyggelse og håndtering af introduktion og spredning af invasive ikkehjemmehørende arter på EU-listen og om en national liste med handelsforbud m.v. over for invasive arter. BEK nr 1285 af 12/11/2018. https://www.retsinformation.dk/eli/lta/2018/1285
#'Ministeriet for Grøn Trepart, 2025. De invasive arter. De invasive artslister [WWW Document]. Arter. https://sgavmst.dk/arter/artsforvaltning/invasive-arter/de-invasive-arter (accessed 12.5.25).
#'
#' Species details:
#'
#'Boyle, B., Hopkins, N., Lu, Z., Raygoza Garay, J.A., Mozzherin, D., Rees, T., Matasci, N., Narro, M.L., Piel, W.H., Mckay, S.J., Lowry, S., Freeland, C., Peet, R.K., Enquist, B.J., 2013. The taxonomic name resolution service: An online tool for automated standardization of plant names. BMC Bioinformatics 14, 1–15. https://doi.org/10.1186/1471-2105-14-16
#'Boyle, B.L., Matasci, N., Mozzherin, D., Rees, T., Barbosa, G.C., Kumar Sajja, R., Enquist, B.J., 2021. Taxonomic Name Resolution Service, version 5.1 . Botanical Information and Ecology Network. https://tnrs.biendata.org/ (accessed 12.6.25).
#'Govaerts, R. (ed.), 2023. WCVP: World Checklist of Vascular Plants, Version 12. Royal Botanic Gardens, Kew. https://sftp.kew.org/pub/data-repositories/WCVP/ (accessed 12.7.25).
#'Govaerts, R., Nic Lughadha, E., Black, N., Turner, R., Paton, A., 2021. The World Checklist of Vascular Plants, a continuously updated resource for exploring global plant diversity. Sci. Data 8, 1–10. https://doi.org/10.1038/s41597-021-00997-6
#'#'Rees, T., 2014. Taxamatch, an Algorithm for Near (‘Fuzzy’) Matching of Scientific Names in Taxonomic Databases. PLoS One 9, e107510. https://doi.org/10.1371/journal.pone.0107510
#'Royal Botanic Gardens Kew, 2025. Plants of the World Online (POWO). Facilitated by the Royal Botanic Gardens, Kew. http://www.plantsoftheworldonline.org/ (accessed 4.1.25).
#'The Angiosperm Phylogeny Group, 2016. An update of the Angiosperm Phylogeny Group classification for the orders and families of flowering plants: APG IV. Bot. J. Linn. Soc. 181, 399–436. https://doi.org/10.1111/boj.12385
#'The World Flora Online Consortium, Elliott, A., Hyam, R., Ulate, W., 2023. World Flora Online Plant List June 2023. Version 2023-06. Zenodo.org. https://zenodo.org/records/8079052 (accessed 12.7.25). https://doi.org/10.5281/zenodo.8079052
#'
#' @examples
#' CIP_Checklist <- CIP_Checklist()
#' head(CIP_Checklist)
#' @export

CIP_Checklist <- function() {
  df <- read.csv(system.file("extdata", "DB_V6_APRIL_2025_CHECKLIST_&_VERIFICATION.csv", package = "CelticInvasivePlantsdb"),
                 header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  df <- df %>%
    mutate(across(where(is.character), ~ ifelse(.x == "" | .x == "nan", NA_character_, .x)))
  return(df)
}

#' @title  Native & Invasive Alien Species (NAIS) within the Celtic Invasive Plants Checklist verification (CIP_NAIS_Ver)
#' @return A table detailing the Native & Invasive Alien Species (NAIS) found within within the Celtic Invasive Plants Checklist and the references used to verify their native status by country.
#' @references
#' Börgesen, F. (1908). Gardening and Tree-planting. In:  Botany of the Faeröes, based upon Danish investigations Part III. Nordisk Forlag. Copenhagen (Denmark), pp. 1027-1043.  https://doi.org/10.5962/bhl.title.8101
#' Castroviejo, S., Laínz, M., López González, G., Montserrat, P., Muñoz Garmendia, F., Paiva, J. & Villar, L. (1986-2020) Flora iberica. Plantas vasculares de la Peninsula Ibérica e Islas Baleares. Madrid, Real Jardín Botánico de Madrid, Consejo Superior de Investigaciones Científicas (CSIC).
#' Domínguez Lozano, F. (2000). Atlas y Libro Rojo de la Flora Vascular Amenazada de España. Conservación vegetal 6. https://www.miteco.gob.es/content/dam/miteco/es/biodiversidad/temas/inventarios-nacionales/lista_roja_2000_tcm30-99751.pdf
#' Fosaa, A. M. (2001). A Review of the Plant Communities of the Faroe Islands. Fróðskaparrit - Faroese Scientific Journal, 48, 41-54. https://doi.org/10.18602/fsj.v48i.756
#' Henniges, M. C., Powell, R. F., Mian, S., Stace, C. A., Walker, K. J., Gornall, R. J., ... & Leitch, I. J. (2022). A taxonomic, genetic and ecological data resource for the vascular plants of Britain and Ireland. Scientific Data, 9(1), 1. https://doi.org/10.1038/s41597-021-01104-5
#' Ostenfeld, C.H. (1901). Flora of the Faeröes: Phanerogamae and Pteridophyta. In: Botany of the Faeröes, based upon Danish investigations Part I. Nordisk Forlag. Copenhagen (Denmark), pp 41-100. https://doi.org/10.5962/bhl.title.8101
#' Ostenfeld, C.H. (1908). Additions and corrections of the List of Phanerogamae and Pteridophyta of Faeröes In:  Botany of the Faeröes, based upon Danish investigations Part III. Nordisk Forlag. Copenhagen (Denmark), pp. 835-864. https://www.biodiversitylibrary.org/page/8496529
#' Royal Botanic Gardens Kew (2025) Plants of the World Online (POWO). Facilitated by the Royal Botanic Gardens, Kew. 2023. http://www.plantsoftheworldonline.org/ [Accessed: 1 April 2025].
#' Ramos-Gutiérrez, I., Lima, H., Pajarón, S., Romero-Zarco, C., Sáez, L., Pataro, L., Molina-Venegas, R., Rodríguez, M.A. & Moreno-Saiz, J.C. (2021) Atlas of the vascular flora of the Iberian Peninsula biodiversity hotspot (AFLIBER). Global Ecology and Biogeography. 30, 1951–1957. https://doi.org/10.1111/geb.13363
#' Tela Botanica (2022) Tela Botanica–Les bases de données botaniques. 2022. https://www.tela-botanica.org/ressources/donnees/telechargements/#donnes-observation [Accessed: 14 September 2022].
#'
#' @examples
#' NAIS_ver <- CIP_NAIS_Ver()
#' head(CNAIS_ver)
#'
#' @export

CIP_NAIS_Ver <- function() {
  df <- read.csv(system.file("extdata", "DB_V6_APRIL_2025_NAIS_VERIFICATION.csv", package = "CelticInvasivePlantsdb"),
                 header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  df <- df %>%
    mutate(across(where(is.character), ~ ifelse(.x == "" | .x == "nan", NA_character_, .x)))
  return(df)
}



#' @title  Grids details on mergers and relocations (CIP_Grids_details)
#' @return A table detailing the mergers and relocations of the UTM grids.
#' @references
#' Greater London Authority. The London Plan. Spatial Development Strategy for Greater London. (London, 2004).
#' Greater London Authority. The London Plan. Spatial Development Strategy for Greater London.Consolidated with Alterations since 2004 (London, 2008)
#' Greater London Authority. The London Plan. Spatial Development Strategy for Greater London. (London, 2011).
#' @examples
#' Grids_details <- CIP_Grids_details()
#' head(Grids_details)
#'
#' @export

CIP_Grids_details <- function() {
  df <- read.csv(system.file("extdata", "CIP_DB_APRIL_2025_Grids_merges_and_relocations.csv", package = "CelticInvasivePlantsdb"),
                 header = TRUE, sep = "\t", stringsAsFactors = FALSE)
  df <- df %>%
    mutate(across(where(is.character), ~ ifelse(.x == "" | .x == "nan", NA_character_, .x)))
  return(df)
}




#AHora una función para hacer queries de valores únicos

.Value_general <- read.csv(system.file("extdata", "DB_V6_APRIL_2025_UNIQUE_VALUES.csv", package = "CelticInvasivePlantsdb"),
                  header = TRUE, sep = "\t", stringsAsFactors = FALSE)
.Value_Portugal <- read.csv(system.file("extdata", "unique_values_Portugal.csv", package = "CelticInvasivePlantsdb"),
                           header = TRUE, sep = "\t", stringsAsFactors = FALSE)
.Value_Spain <- read.csv(system.file("extdata", "unique_values_Spain _ España.csv", package = "CelticInvasivePlantsdb"),
                            header = TRUE, sep = "\t", stringsAsFactors = FALSE)
.Value_France <- read.csv(system.file("extdata", "unique_values_France.csv", package = "CelticInvasivePlantsdb"),
                         header = TRUE, sep = "\t", stringsAsFactors = FALSE)
.Value_Ireland <- read.csv(system.file("extdata", "unique_values_Ireland _ Éire.csv", package = "CelticInvasivePlantsdb"),
                          header = TRUE, sep = "\t", stringsAsFactors = FALSE)
.Value_UK <- read.csv(system.file("extdata", "unique_values_United Kingdom.csv", package = "CelticInvasivePlantsdb"),
                           header = TRUE, sep = "\t", stringsAsFactors = FALSE)
.Value_Denmark <- read.csv(system.file("extdata", "unique_values_Denmark _ Danmark _ Danmørk.csv", package = "CelticInvasivePlantsdb"),
                      header = TRUE, sep = "\t", stringsAsFactors = FALSE)



#' @title Conducting a value query within the  Celtic Invasive Plants database (CIP_value_query)
#' @description This function allows to conduct a value query within the Celtic Invasive Plants database. This can be performed in the whole database or in a specific country.
#' @param 'Area' must specify the Area of interest. It can be the whole data base if the value is set to "ALL" or an specific country ("Portugal", Spain", "France", "Ireland", "United_Kingdom" or "Denmark". This is set by default to "ALL".
#' @param 'query' must be either a character string or a vector of character strings specifying the term or terms of interest.
#' @param 'Import' option allows the user to import a table containing the Variables (columns) and Values (as list of unique values) of the Area of interest if set to TRUE. By default it is set to FALSE.
#' @return This function prints on the screen whether the term or terms have been found and the column where it was found. If Import = TRUE a table containing the Variables (columns) and Values (as list of unique values) of the Area of interest will be imported.
#' @examples
#' #This will provide a negative result for the query
#'CIP_value_query(Area = "Portugal", "29VPJ19", Import = FALSE)
#'
#' #This will provide a positve result for the query and import the dataframe with the unique values
#' CIP_value_query(Area = "Denmark", "29VPJ19", Import = TRUE)
#' @export
#'
CIP_value_query <- function(Area = "ALL", query, Import = FALSE) {
  dfs <- list(
    General = .Value_general,
    Portugal = .Value_Portugal,
    Spain = .Value_Spain,
    France = .Value_France,
    Ireland = .Value_Ireland,
    United_Kingdom = .Value_UK,
    Denmark = .Value_Denmark
  )

  if (Area == "ALL") {
    df <- .Value_general
    Area <- "General"
  } else if (Area %in% names(dfs)) {
    df <- dfs[[Area]]
  } else {
    stop(paste("Are not found", Area))
  }

  # Asignar el dataframe al entorno global si Import = TRUE
  if (Import) {
    assign(Area, df, envir = .GlobalEnv)
  }

  # Realizar la búsqueda
  mensajes <- character()
  for (q in query) {
    resultados <- df[grep(paste0("\\b", q, "\\b"), df$Values), ]
    if (nrow(resultados) > 0) {
      mensajes <- c(mensajes, paste("The value", q, "is present in the column", resultados$Variable))
    } else {
      mensajes <- c(mensajes, paste("The value", q, "was not found"))
    }
  }
  return(mensajes)
}


#Vector de columnas de query

.query_columnas <- c("Taxa_ID", "Phylum", "Subphylum", "Order", "Family", "Genus", "Species_with_Author", "Taxa", "UTM_grid", "Subprovince", "Country", "Constituent_Country_OR_Crown_Dependency", "Admin_units_II", "Admin_units_III", "National_Nature_Reserve_Name", "Natura_2000_Name", "National_Designation", "Designation_in_English", "Designation_Type", "UNEP_WCMC_Cat", "Governance_Type", "Management_Authority", "IUCN_Cat")
.ANDcolumns <- c( "National_Designation", "Designation_in_English", "Designation_Type", "Governance_Type", "Management_Authority")




#' @title Selecting data within the Celtic Invasive Plants database based on a query (Select_CIPdb)
#' @description This selects data from a CIPdb table, excluding the Natural Reserves IDs (WDPA_PID columns). Queries can be: Taxa_ID, species, species with author, Genus, Family, Order, Subphylum, Phylum,Taxa, UTM_grid, Subprovince, Country, Constituent_Country_OR_Crown_Dependency, Admin_units_II, Admin_units_III, National_Nature_Reserve_Name, Natura_2000_Name, National_Designation, Designation_in_English or Designation_Type.
#' @param 'data' must be the table obtained from CIPdb() or another selection of this.
#' @param 'query' must be either a character string or a vector of character strings.This parameter is set to NULL by default. This must coincide with the ID columns (Taxa_ID or any WDPA_PID),Taxonomy columns, UTM_grid, Protected area columns or Administrative area columns
#' @param 'EU_Concern' & 'Local_Origin' possible values: 'YES' or 'NO'
#' @param 'Conservation_Habitats_Directive', 'Birds_Directive'  & 'Community_Importance_Habitats_Directive' possible value: 'YES'
#' @param 'Celtic_Fringe_Origin' possible values: 'Alien' or 'Native'
#' @param 'Local_Origin' possible values: 'Alien', 'Extinct' or 'Native'
#' @param 'Celtic_Fringe_Taxa_Category' possible values: 'AIS' or 'NAIS'
#' @param 'Presence_Protected_Area', 'Natura_2000' & 'National_Nature_Reserve' possible values: '1' or '0'
#' @details 'EU_Concern', 'Officially_listed', 'Local_Origin', 'Celtic_Fringe_Origin', 'Celtic_Fringe_Taxa_Category', 'Conservation_Habitats_Directive', 'Birds_Directive', 'Community_Importance_Habitats_Directive', 'Presence_Protected_Area', 'Natura_2000' and 'National_Nature_Reserve' arguments can be used to filter the selection based on the possible values of these columns. They are set to "ALL" by defect, this will NOT filter your data.
#' If the user wishes to perform a selection using these parameters,they must specify a valid value for these columns
#' @return This function returns a table object corresponding with the selection of the user.
#'
#' @seealso Raw Celtic Invasive Plants database (CIPdb) & Selecting data within the Celtic Invasive Plants database based on a WDPA PID query (WDPA_PID_select_CIPdb)
#'
#' @examples
#'
#' #Example of selecting only the EU_Concern entries
#' Data = CIPdb()
#' My_selection <- Select_CIPdb(Data, EU_Concern = "YES")
#'
#' #Example of selecting only the entries of the genus Cotoneaster
#' Data = CIPdb()
#' My_selection <- Select_CIPdb(Data, query = "Cotoneaster")
#'
#' #Example of selecting only the entries of Cotoneaster bullatus
#' My_selection <- Select_CIPdb(Data, query = "Cotoneaster bullatus")
#'
#' #Example of selecting only the entries of Cotoneaster bullatus by its taxa_ID 974983
#' My_selection <- Select_CIPdb(Data, query = "974983")
#'
#' #Example of selecting Cortaderia species occurring in Protected areas
#'  My_selection <- Select_CIPdb(Data, query = "Cortaderia", Presence_Protected_Area = 1)
#'
#' #Example of selecting Cortaderia species occurring outside Protected areas
#'  My_selection <- Select_CIPdb(Data, query = "Cortaderia", Presence_Protected_Area = 0)
#'
#' #Example of selecting Cortaderia species occurring in Natura 2000 area
#'  My_selection <- Select_CIPdb(Data, query = "Cortaderia", Natura_2000 = 1)
#'
#' #Example of selecting only the entries of the Rosaceae family
#' My_selection <- Select_CIPdb(Data, query = "Rosaceae")
#'
#' #Example of selecting only the entries of the order Malvales
#' My_selection <- Select_CIPdb(Data, query =  "Malvales")
#'
#' #Example of selecting only the entries from France
#' My_selection <- Select_CIPdb(Data, query =  "France")
#'
#' #Example of selecting only the entries from France and Spain
#'
#' My_countries = c("Spain", "France")
#' My_selection <- Select_CIPdb(Data, query = My_countries)
#'
#' #Example of selecting only the entries from France and Spain listed in the EU_Concern list
#'
#' My_countries = c("Spain", "France")
#' My_selection <- Select_CIPdb(Data, query = My_countries, EU_Concern = "YES")
#'
#' #Example of selecting only the NAIS in  from France and Spain listed as Native in these countries
#'
#' My_countries = c("Spain", "France")
#' My_selection <- Select_CIPdb(Data, query = My_countries, Local_Origin = "Native", Celtic_Fringe_Taxa_Category = "NAIS")
#'
#' #Example of selecting a Protected Area by name
#' My_selection <- Select_CIPdb(Data, query = "Picos de Europa", Officially_listed = "ALL", Celtic_Fringe_Taxa_Category = "AIS")
#'
#'For getting all the national Parks
#' My_selection <- Select_CIPdb(Data, query = "Picos de Europa", Officially_listed = "ALL", Celtic_Fringe_Taxa_Category = "AIS")
#'
#'For getting subprovince within a country
#'Country_subprov = c("Cantabrian Atlantic", "France")
#'
#'My_selection <- Select_CIPdb(Data, query = Country_subprov)
#'
#'For getting specific grids
#'
#'my_grids = c("31TCL81","30UWE74","30TWN27")
#'
#'My_selection <- Select_CIPdb(Data, query = my_grids)
#'
#' @export



Select_CIPdb <- function(data, query = NULL, EU_Concern = "ALL", Officially_listed = "ALL", Local_Origin = "ALL", Celtic_Fringe_Origin = "ALL", Celtic_Fringe_Taxa_Category = "ALL", Conservation_Habitats_Directive = "ALL", Birds_Directive = "ALL", Community_Importance_Habitats_Directive = "ALL", Presence_Protected_Area = "ALL", Natura_2000 = "ALL", National_Nature_Reserve = "ALL") {
  columnas <- intersect(.query_columnas, colnames(data))
  if (length(columnas) == 0) {
    stop("Could not find the specified column(s)")
  }
  data[, columnas] <- lapply(data[, columnas], as.character)

  if (!is.null(query)) {
    pattern <- paste(query, collapse = "|")
    resultado <- data[apply(data[, columnas, drop = FALSE], 1, function(x) any(grepl(pattern, as.character(x), ignore.case = TRUE))), ]
  } else {
    resultado <- data
  }

  filtro <- function(columna, valor, opciones) {
    if (valor == "ALL") return(rep(TRUE, nrow(resultado)))
    if (all(is.na(resultado[[columna]])) & valor %in% c("YES")) return(rep(FALSE, nrow(resultado)))
    if (valor %in% opciones) {
      if (all(c("YES", "NO") %in% opciones)) {
        return(resultado[[columna]] == valor)
      } else if (all(c("YES") %in% opciones)) {
        return(!is.na(resultado[[columna]]) & resultado[[columna]] == "YES")
      } else {
        return(resultado[[columna]] == valor)
      }
    }
    stop(paste0("Invalid ", columna, " value. Must be 'ALL' or one of: ", paste(opciones, collapse = ", ")))
  }

  # Filtros
  resultado <- resultado[filtro("EU_Concern", EU_Concern, c("YES", "NO")), ]
  resultado <- resultado[filtro("Officially_listed", Officially_listed, c("YES", "NO")), ]
  resultado <- resultado[filtro("Local_Origin", Local_Origin, c("Native", "Alien", "Extinct")), ]
  resultado <- resultado[filtro("Celtic_Fringe_Origin", Celtic_Fringe_Origin, c("Alien", "Native")), ]
  resultado <- resultado[filtro("Celtic_Fringe_Taxa_Category", Celtic_Fringe_Taxa_Category, c("AIS", "NAIS")), ]
  resultado <- resultado[filtro("Special_Areas_of_Conservation_(Habitats_Directive)", Conservation_Habitats_Directive, c("YES")), ]
  resultado <- resultado[filtro("Special_Protection_Area_(Birds_Directive)", Birds_Directive, c("YES")), ]
  resultado <- resultado[filtro("Site_of_Community_Importance_(Habitats_Directive)", Community_Importance_Habitats_Directive, c("YES")), ]
  resultado <- resultado[filtro("Presence_Protected_Area", Presence_Protected_Area, c("1", "0")), ]
  resultado <- resultado[filtro("Presence_Natura_2000", Natura_2000, c("1", "0")), ]
  resultado <- resultado[filtro("Presence_National_Nature_Reserve", National_Nature_Reserve, c("1", "0")), ]

  return(resultado)
}





#Ahora creamos un diccionario para sociar los UTMs con las IDs de las zonas naturales

.Data = CIPdb()
.WDPA_PID_Columns <- c("Regional_Protected_Landscape_WDPA_PID", "Local_Nature_Reserve_WDPA_PID", "Marine_Protected_Area_(OSPAR)_WDPA_PID", "Nature_Park_WDPA_PID", "Site_of_Community_Importance_(Habitats_Directive)_WDPA_PID", "National_Park_WDPA_PID", "Protected_Landscape_WDPA_PID", "Natural_Park_WDPA_PID", "Natura_2000_WDPA_PID", "Special_Areas_of_Conservation_(Habitats_Directive)_WDPA_PID", "Special_Protection_Area_(Birds_Directive)_WDPA_PID", "Natural_Monument_WDPA_PID", "Protected_Wetland_WDPA_PID", "Nature_Reserve_WDPA_PID", "Regional_Nature_Park_WDPA_PID", "Regional_Park_WDPA_PID", "National_Park_-_Buffer_zone/Area_of_adhesion_WDPA_PID", "Biotope_Protection_Order_WDPA_PID", "Nature_enclave_WDPA_PID", "Land_acquired_by_a_regional_conservatory_of_natural_areas_WDPA_PID", "Regional_Nature_Reserve_WDPA_PID", "UNESCO-MAB_Biosphere_Reserve_WDPA_PID", "Natural_Area_(recreational)_WDPA_PID", "Biosphere_Reserve_WDPA_PID", "Nature_Reserve_(Parcial)_WDPA_PID", "Natural_Area_of_Special_Interest_WDPA_PID", "Land_acquired_by_Conservatoire_du_Littoral_(national_seaside_and_lakeside_conservancy)_WDPA_PID", "Ramsar_Site,_Wetland_of_International_Importance_WDPA_PID", "National_Nature_Reserve_WDPA_PID", "Site_of_national_interest_WDPA_PID", "Marine_Nature_Park_WDPA_PID", "Protected_perimeter_around_a_national_nature_reserve_WDPA_PID", "Forest_Managed_Biological_Reserve_WDPA_PID", "Forest_Integral_Biological_Reserve_WDPA_PID", "Geotope_Protection_Order_WDPA_PID", "Natural_habitats_protection_Order_WDPA_PID", "National_Hunting_and_Wildlife_Reserve_WDPA_PID", "Natural_Heritage_Area_WDPA_PID")
.CAT_Columns =  c("UNEP_WCMC_Cat", "IUCN_Cat")

# Separar los valores en las columnas numéricas que tienen "&"
#Esto genera dependencia con dyplr y tidyr
.Data = CIPdb()
.Dict_WDPA_PID =.Data %>%
  dplyr::select(UTM_grid, ends_with("_WDPA_PID")) %>%
  mutate(across(ends_with("_WDPA_PID"), as.character)) %>%
  pivot_longer(cols = -UTM_grid, names_to = "columna", values_to = "valor") %>%
  mutate(valor = str_split(valor, " & ")) %>%
  unnest(valor) %>%
  filter(!is.na(valor) & valor != "") %>%
  distinct()

#' @title Selecting data within the Celtic Invasive Plants database based on a WDPA PID query (WDPA_PID_select_CIPdb)
#' @description  Selection of entries of the Celtic Invasive Plants database based on a WDPA PID query.
#' @param 'data' argument must be the table obtained from CIPdb() or another selection of this (i. e. Select_CIPdb).
#' @param 'query' argument must be either a character string or a vector of character strings.This must coincide with the WDPA PIDs (classification and data descriptor for more detail).
#' @return  This function returns a table object corresponding with the selection of the user.
#' @details This can be applied before or after the Select_CIPdb() function.
#' @references
#' Protected areas details:
#'European Environment Agency (EEA), 2025a. Emerald Network data (vector) - the Pan-European network of protected sites version 2024 https://doi.org/10.2909/135a0bb6-c611-4c2c-823d-a564be119ad8
#'European Environment Agency (EEA), 2024. Nationally designated areas for public access (vector data) - May 2024 https://doi.org/10.2909/616ef48f-7196-4e30-b201-6c97808fa68a
#'European Environment Agency (EEA), 2025b. Natura 2000 (tabular) - version end 2023 https://www.eea.europa.eu/en/datahub/datahubitem-view/6fc8ad2d-195d-40f4-bdec-576e7d1268e4
#'World Database of Protected Area (WDPA) (https://www.protectedplanet.net/en)
#' @seealso Selecting data within the Celtic Invasive Plants database based on a query (Select_CIPdb) & Selecting data within the Celtic Invasive Plants database based on a WDPA PID query (WDPA_PID_select_CIPdb)
#'
#' @examples
#'Example of selecting a Protected Area by ID in this case Picos de EUropa  555722929
#'My_selection <- WDPA_PID_select_CIPdb(Data, query = "555722929")
#'
#'#Example for selecting a various Protected Areas by ID
#'WDPA_PID_IDs = c("555722929", "860")
#'My_selection <- WDPA_PID_select_CIPdb(Data, WDPA_PID_IDs)
#'
#' @export

WDPA_PID_select_CIPdb <- function(data, query) {
  # Filtrar el diccionario según el query
  UTM_seleccionados <- .Dict_WDPA_PID$UTM_grid[.Dict_WDPA_PID$valor %in% as.character(query)]
  # Filtrar Data según los UTM seleccionados
  resultado <- data[data$UTM_grid %in% UTM_seleccionados, ]
  return(resultado)
}


.Dict_IUCN_Cat <- .Data %>%
  dplyr::select(UTM_grid, IUCN_Cat) %>%
  mutate(IUCN_Cat = str_split(IUCN_Cat, " & ")) %>%
  unnest(IUCN_Cat) %>%
  filter(!is.na(IUCN_Cat) & IUCN_Cat != "") %>%
  rename(valor = IUCN_Cat) %>%
  mutate(columna = "IUCN_Cat")

.Dict_UNEP_WCMC_Cat <- .Data %>%
  dplyr::select(UTM_grid, UNEP_WCMC_Cat) %>%
  mutate(UNEP_WCMC_Cat = str_extract_all(UNEP_WCMC_Cat, "\\((.*?)\\)")) %>%
  unnest(UNEP_WCMC_Cat) %>%
  filter(!is.na(UNEP_WCMC_Cat) & UNEP_WCMC_Cat != "") %>%
  mutate(UNEP_WCMC_Cat = str_replace_all(UNEP_WCMC_Cat, "[\\(\\)]", "")) %>%
  rename(valor = UNEP_WCMC_Cat) %>%
  mutate(columna = "UNEP_WCMC_Cat")


#' @title Selecting data within the Celtic Invasive Plants database based on international protected areas categories  (ICat_select_CIPdb)
#' @description  Selection of entries of the Celtic Invasive Plants database based on a WDPA PID query.
#' @param 'data'  must be the table obtained from CIPdb() or another selection of this (i. e. Select_CIPdb).
#' @param 'UNEP_WCMC_Cat'  must be either a character string or a vector of character strings.This must coincide with unique values found in the column UNEP_WCMC_Cat.
#' @param 'IUCN_Cat'  must be either a character string or a vector of character strings.This must coincide with unique values found in the column IUCN_Cat.
#' @return  This function returns a table object corresponding with the selection of the user.
#' @details This can be applied before or after the Select_CIPdb() and the WDPA_PID_select_CIPdb() functions.
#' @references
#' Protected areas details: World Database of Protected Area (WDPA) (https://www.protectedplanet.net/en)
#' @seealso Selecting data within the Celtic Invasive Plants database based on a query (Select_CIPdb) & Selecting data within the Celtic Invasive Plants database based on a WDPA PID query (WDPA_PID_select_CIPdb)
#'
#' @examples
#' #This provide with the Protected areas within the IUCN categories IV" and "Ia"
#'Data = CIPdb()
#' My_IUCN_Cat = c("IV", "Ia")
#'My_selection <- ICat_select_CIPdb(Data, UNEP_WCMC_Cat = NULL, IUCN_Cat = My_IUCN_Cat)
#'
#'#This provide with the Protected areas within the UNEP_WCMC categories "i and "iii"
#'Data = CIPdb()
#' My_UNEP_WCMC = c("i", "iii")
#'My_selection <- ICat_select_CIPdb(Data, UNEP_WCMC_Cat = My_UNEP_WCMC, IUCN_Cat = NULL)

ICat_select_CIPdb<- function(data, UNEP_WCMC_Cat = NULL, IUCN_Cat = NULL) {
  UTM_seleccionados <- NULL

  if (!is.null(IUCN_Cat)) {
    for (cat in IUCN_Cat) {
      UTM_seleccionados_IUCN <- .Dict_IUCN_Cat$UTM_grid[grepl(cat, .Dict_IUCN_Cat$valor, ignore.case = TRUE)]
      UTM_seleccionados <- c(UTM_seleccionados, UTM_seleccionados_IUCN)
    }
  }

  if (!is.null(UNEP_WCMC_Cat)) {
    for (cat in UNEP_WCMC_Cat) {
      UTM_seleccionados_UNEP <- .Dict_UNEP_WCMC_Cat$UTM_grid[.Dict_UNEP_WCMC_Cat$valor %in% cat]
      UTM_seleccionados <- c(UTM_seleccionados, UTM_seleccionados_UNEP)
    }
  }

  UTM_seleccionados <- unique(na.omit(UTM_seleccionados))

  resultado <- data[data$UTM_grid %in% UTM_seleccionados, ]

  return(resultado)
}



#Primero las columhnas que quiero excluir:
.report_exclude = c("Taxa_URL", "Latitude", "Longitude")

.report_unique = c("Taxa_ID", "Phylum", "Subphylum", "Order", "Family", "Genus", "Species_with_Author", "UTM_grid", "Subprovince", "Country", "Constituent_Country_OR_Crown_Dependency", "Admin_units_II", "Admin_units_III", "National_Nature_Reserve_Name", "Natura_2000_Name", "National_Designation", "Designation_in_English")

.report_IDs_Only = c("Regional_Protected_Landscape_WDPA_PID", "Local_Nature_Reserve_WDPA_PID", "Marine_Protected_Area_.OSPAR._WDPA_PID", "Nature_Park_WDPA_PID", "Site_of_Community_Importance_.Habitats_Directive._WDPA_PID", "National_Park_WDPA_PID", "Protected_Landscape_WDPA_PID", "Natural_Park_WDPA_PID", "Natura_2000_WDPA_PID", "Special_Areas_of_Conservation_.Habitats_Directive._WDPA_PID", "Special_Protection_Area_.Birds_Directive._WDPA_PID", "Natural_Monument_WDPA_PID", "Protected_Wetland_WDPA_PID", "Nature_Reserve_WDPA_PID", "Regional_Nature_Park_WDPA_PID", "Regional_Park_WDPA_PID", "National_Park_._Buffer_zone.Area_of_adhesion_WDPA_PID", "Biotope_Protection_Order_WDPA_PID", "Nature_enclave_WDPA_PID", "Land_acquired_by_a_regional_conservatory_of_natural_areas_WDPA_PID", "Regional_Nature_Reserve_WDPA_PID", "UNESCO-MAB_Biosphere_Reserve_WDPA_PID", "Natural_Area_(recreational)_WDPA_PID", "Biosphere_Reserve_WDPA_PID", "Nature_Reserve_.Parcial._WDPA_PID", "Natural_Area_of_Special_Interest_WDPA_PID", "Land_acquired_by_Conservatoire_du_Littoral_.national_seaside_and_lakeside_conservancy._WDPA_PID", "Ramsar_Site,_Wetland_of_International_Importance_WDPA_PID", "National_Nature_Reserve_WDPA_PID", "Site_of_national_interest_WDPA_PID", "Marine_Nature_Park_WDPA_PID", "Protected_perimeter_around_a_national_nature_reserve_WDPA_PID", "Forest_Managed_Biological_Reserve_WDPA_PID", "Forest_Integral_Biological_Reserve_WDPA_PID", "Geotope_Protection_Order_WDPA_PID", "Natural_habitats_protection_Order_WDPA_PID", "National_Hunting_and_Wildlife_Reserve_WDPA_PID", "Natural_Heritage_Area_WDPA_PID")



#' @title Automatic General Report of Celtic Invasive Plants database or selections (General_Report_CIPdb)
#' @description  Generate an automatic general report of the Celtic Invasive Plants database or a selection of this of the unique values of the categorical columns.
#' These are the columns: "Taxa_ID", "Phylum", "Subphylum", "Order", "Family", "Genus", "Species_with_Author", "UTM_grid", "Subprovince", "Country", "Constituent_Country_OR_Crown_Dependency", "Admin_units_II", "Admin_units_III", "National_Nature_Reserve_Name", "Natura_2000_Name", "National_Designation", "Designation_in_English" and all the "WDPA_PID" columns.
#' @param 'data' argument must be the table obtained from CIPdb() or another selection of this (Select_CIPdb, WDPA_PID_select_CIPdb).
#' @return  This function returns a table object with 3 columns (Category, Number and Value) detailing the number of unique values of the categorical columns within the selection of the user.
#' The Category column refers to the name of the categorical column, the Number columns refers to the number of unique values (categories) within that column in the data and the Value column provides a specific list of those values separated by "&".
#' @details The optimal way of using this function is in combination with the  Select_CIPdb() or WDPA_PID_select_CIPdb() functions.
#' @examples
#' #This provide a General Report on the whole database
#' Data = CIPdb()
#' Whole_report = General_Report_CIPdb(Data)
#'
#' #This provide a General Report on Cantabrian Atlantic of France
#' Country_subprov = c("Cantabrian Atlantic", "France")
#' My_selection <- Select_CIPdb(Data, query =  Country_subprov)
#' My_report = General_Report_CIPdb(My_selection)


#' @export


General_Report_CIPdb <- function(data) {
  columnas_unicas <- intersect(.report_unique, setdiff(colnames(data), .report_exclude))
  columnas_ids <- intersect(.report_IDs_Only, setdiff(colnames(data), .report_exclude))

  resultado_unicas <- map_dfr(columnas_unicas, ~ {
    tibble(
      Category = .x,
      Number = n_distinct(data[[.x]]),
      Values = list(unique(data[[.x]]))
    )
  }) %>%
    mutate(
      Values = map_chr(Values, ~ .x[!is.na(.x)] %>% str_c(collapse = " & "))
    )

  resultado_ids <- map_dfr(columnas_ids, ~ {
    tibble(
      Category = .x,
      Number = n_distinct(data[[.x]]),
      Values = list(unique(data[[.x]]))
    )
  }) %>%
    mutate(
      Values = map_chr(Values, ~ .x[!is.na(.x)] %>% str_c(collapse = " & "))
    )

  bind_rows(resultado_unicas, resultado_ids) %>%
    filter(Values != "" & !is.na(Values)) %>%
    mutate(
      Values = str_replace(Values, "^NaN & ", ""),
      Values = str_replace(Values, " & NaN", ""),
      Values = str_replace(Values, "NaN", ""),
      Values = str_replace(Values, "nan & ", "")
    )  %>%
    mutate(Values = ifelse(str_detect(Values, "^ & "), str_sub(Values, 3), Values))
}





.admin_repot = c("Subprovince", "Country", "Constituent_Country_OR_Crown_Dependency", "Admin_units_II", "Admin_units_III", "National_Nature_Reserve_Name", "Natura_2000_Name")




.simple_counts <- c(
  "Species_with_Author", "Phylum", "Subphylum", "Order", "Family", "Genus",
  "Subprovince", "Constituent_Country_OR_Crown_Dependency",
  "Admin_units_II", "Admin_units_III", "National_Nature_Reserve_Name",
  "Natura_2000_Name", "UTM_grid", "Country")

#' @title Automatic Area Report of Celtic Invasive Plants database or selections (Area_Report_CIPdb)
#' @description  Generate an automatic report of the taxa occurring in an administrative, protected or biogeographic area of the Celtic Invasive Plants database or a selection of this of the unique values of the categorical columns.
#' The possible areas include: "Subprovince", "Country", "Constituent_Country_OR_Crown_Dependency", "Admin_units_II", "Admin_units_III", "National_Nature_Reserve_Name" and "Natura_2000_Name".
#' This function retrieves the unique values for 22 categories obtained combining "Species_with_author" with the status and protected areas columns: "Species_AIS", "Species_AIS_&_EU_Concern", "Species_AIS_&_Listed", "Species_AIS_&_NOT_Listed", "Species_AIS_&_EU_Concern_&_Listed", "Species_AIS_&_EU_Concern_&_NOT_Listed","Species_AIS_&_Listed_in_Protected_Area", "Species_AIS_&_EU_Concern_&_Listed_in_Protected_Area","Species_AIS_&_Listed_in_National_Protected_Area", "Species_AIS_&_EU_Concern_&_National_Protected_Area", "Species_AIS_&_Listed_in_Natura_2000", "Species_AIS_&_EU_Concern_&_Natura_2000","Species_AIS_&_NOT_Listed_in_Protected_Area", "Species_AIS_&_EU_Concern_&_NOT_Listed_in_Protected_Area","Species_AIS_&_NOT_Listed_in_National_Protected_Area","Species_AIS_&_EU_Concern_&_NOT_LIsted_in_National_Protected_Area","Species_AIS_&_NOT_Listed_in_Natura_2000","Species_AIS_&_EU_Concern_&_NOT_Listed_in_Natura_2000","Species_NAIS_&_Native","Species_NAIS_&_Alien","Species_NAIS_&_Alien_&_Listed" and "Species_NAIS_&_Alien_&_NOT_Listed".
#' @param 'data' must be the table obtained from CIPdb() or another selection of this (Select_CIPdb, WDPA_PID_select_CIPdb).
#' @param 'Scope' must correspond with one of the possible area: Subprovince", "Country", "Constituent_Country_OR_Crown_Dependency", "Admin_units_II", "Admin_units_III", "National_Nature_Reserve_Name" and "Natura_2000_Name".
#' The value in this argument must exactly correspond with the name of only one of these columns for this function to work.
#' @param 'Values' must be be either a character string or a vector of character strings.This must coincide with the potential values of the 'Scope' columns.
#' @return  This function returns a table object with 3 columns: Category, Number and Value.
#' The Category columnn refers to: (1) area categories (Subprovince", "Country", "Constituent_Country_OR_Crown_Dependency", "Admin_units_II", "Admin_units_III", "National_Nature_Reserve_Name" and "Natura_2000_Name") and (2) taxa name and status and protected areas columns ("Species_AIS", "Species_AIS_&_EU_Concern", "Species_AIS_&_Listed", "Species_AIS_&_NOT_Listed", "Species_AIS_&_EU_Concern_&_Listed", "Species_AIS_&_EU_Concern_&_NOT_Listed","Species_AIS_&_Listed_in_Protected_Area", "Species_AIS_&_EU_Concern_&_Listed_in_Protected_Area","Species_AIS_&_Listed_in_National_Protected_Area", "Species_AIS_&_EU_Concern_&_National_Protected_Area", "Species_AIS_&_Listed_in_Natura_2000", "Species_AIS_&_EU_Concern_&_Natura_2000","Species_AIS_&_NOT_Listed_in_Protected_Area", "Species_AIS_&_EU_Concern_&_NOT_Listed_in_Protected_Area","Species_AIS_&_NOT_Listed_in_National_Protected_Area","Species_AIS_&_EU_Concern_&_NOT_LIsted_in_National_Protected_Area","Species_AIS_&_NOT_Listed_in_Natura_2000","Species_AIS_&_EU_Concern_&_NOT_Listed_in_Natura_2000","Species_NAIS_&_Native","Species_NAIS_&_Alien","Species_NAIS_&_Alien_&_Listed" and "Species_NAIS_&_Alien_&_NOT_Listed").
#' The Number column refers to the number of unique values of each category.
#' The Values column refers to a list of the detected unique values in each category in which the different elements are separated by " & ".
#' If the "Value" argument is fed with a vector containing more than one elements, this function will automatically create two dataframes (tables) named after the 'data' argument value and each of the vector element fed to the argument 'Value'.
#' @details The optimal way of using this function is in combination with the  Select_CIPdb() or WDPA_PID_select_CIPdb() functions.
#' @examples
#' #This provides an Area report for France
#'
#' Data = CIPdb()
#' My_area_Report = Area_Report_CIPdb(Data, "Country", c("France"))
#'
#' #This provides an Area report for France  and Spain
#'
#' Data = CIPdb()
#' My_countries = c("Spain / España", "France")
#'#This will generate  a "Spain / España" dataframe and a "France" dataframe
#' My_area_Report = Area_Report_CIPdb(Data, "Country", My_countries)
#'
#' #This provides an Area report for the Cantabrian Atlantic subprovince
#'
#' My_area_Report = Area_Report_CIPdb(Data, "Subprovince", "Cantabrian Atlantic")
#'
#'
#' #This provides an Area report for the Cantabrian Atlantic subprovince in France
#'
#' Country_subprov = c("Cantabrian Atlantic", "France")
#' My_selection <- Select_CIPdb(Data, query = Country_subprov)
#' My_area_Report = Area_Report_CIPdb(My_selection, "Subprovince", "Cantabrian Atlantic")
#'
#' #This provides an Area report for Wales and Scotland
#'
#' W_S = c("Wales / Cymru", "Scotland / Alba")
#' My_area_Report = Area_Report_CIPdb(Data, "Constituent_Country_OR_Crown_Dependency", W_S)
#'
#'#This provides an Area report of Asturias, York, Devon and Dordogne
#'
#' admin_III = c("Asturias / Asturies", "York", "Dordogne", "Devon" )
#'My_area_Report = Area_Report_CIPdb(Data, "Admin_units_III", admin_III )
#'
#' @export
Area_Report_CIPdb <- function(data, Scope, Values, output_name = deparse(substitute(data))) {
  resultados <- map(Values, function(value) {
    if (Scope %in% .admin_repot) {
      columnas_analisis <- setdiff(.admin_repot, Scope)
    } else {
      columnas_analisis <- .admin_repot
    }

    df_simple_counts <- map_dfr(.simple_counts, function(x) {
      data %>%
        filter(!!sym(Scope) == value) %>%
        summarise(
          Category = x,
          Number = n_distinct(.data[[x]][.data[[x]] != ""], na.rm = TRUE),
          Values = paste(na.omit(unique(.data[[x]][.data[[x]] != ""])), collapse = " & ")
        )
    })

    combinaciones <- list(
      list(
        nombre = "Species_AIS",
        filtro = list(Celtic_Fringe_Taxa_Category = "AIS", Local_Origin = "Alien")
      ),
      list(
        nombre = "Species_AIS_&_EU_Concern",
        filtro = list(Celtic_Fringe_Taxa_Category = "AIS", EU_Concern = "YES")
      ),
      list(
        nombre = "Species_AIS_&_Listed",
        filtro = list(Celtic_Fringe_Taxa_Category = "AIS", Officially_listed = "YES")
      ),
      list(
        nombre = "Species_AIS_&_NOT_Listed",
        filtro = list(Celtic_Fringe_Taxa_Category = "AIS", Officially_listed = "NO")
      ),
      list(
        nombre = "Species_AIS_&_EU_Concern_&_Listed",
        filtro = list(Celtic_Fringe_Taxa_Category = "AIS", EU_Concern = "YES", Officially_listed = "YES")
      ),
      list(
        nombre = "Species_AIS_&_EU_Concern_&_NOT_Listed",
        filtro = list(Celtic_Fringe_Taxa_Category = "AIS", EU_Concern = "YES", Officially_listed = "NO")
      ),
      list(
        nombre = "Species_AIS_&_Listed_in_Protected_Area",
        filtro = list(Celtic_Fringe_Taxa_Category = "AIS", Officially_listed = "YES", Presence_Protected_Area = "1")
      ),
      list(
        nombre = "Species_AIS_&_EU_Concern_&_Listed_in_Protected_Area",
        filtro = list(Celtic_Fringe_Taxa_Category = "AIS", EU_Concern = "YES", Officially_listed = "YES", Presence_Protected_Area = "1")
      ),
      list(
        nombre = "Species_AIS_&_Listed_in_National_Protected_Area",
        filtro = list(Celtic_Fringe_Taxa_Category = "AIS", Officially_listed = "YES", Presence_National_Nature_Reserve = "1")
      ),
      list(
        nombre = "Species_AIS_&_EU_Concern_&_National_Protected_Area",
        filtro = list(Celtic_Fringe_Taxa_Category = "AIS", EU_Concern = "YES", Officially_listed = "YES", Presence_National_Nature_Reserve = "1")
      ),
      list(
        nombre = "Species_AIS_&_Listed_in_Natura_2000",
        filtro = list(Celtic_Fringe_Taxa_Category = "AIS", Officially_listed = "YES", Presence_Natura_2000 = "1")
      ),
      list(
        nombre = "Species_AIS_&_EU_Concern_&_Natura_2000",
        filtro = list(Celtic_Fringe_Taxa_Category = "AIS", EU_Concern = "YES", Officially_listed = "YES", Presence_Natura_2000 = "1")
      ),
      list(
        nombre = "Species_AIS_&_NOT_Listed_in_Protected_Area",
        filtro = list(Celtic_Fringe_Taxa_Category = "AIS", Officially_listed = "NO", Presence_Protected_Area = "1")
      ),
      list(
        nombre = "Species_AIS_&_EU_Concern_&_NOT_Listed_in_Protected_Area",
        filtro = list(Celtic_Fringe_Taxa_Category = "AIS", EU_Concern = "YES", Officially_listed = "NO", Presence_Protected_Area = "1")
      ),
      list(
        nombre = "Species_AIS_&_NOT_Listed_in_National_Protected_Area",
        filtro = list(Celtic_Fringe_Taxa_Category = "AIS", Officially_listed = "NO", Presence_National_Nature_Reserve = "1")
      ),
      list(
        nombre = "Species_AIS_&_EU_Concern_&_NOT_LIsted_in_National_Protected_Area",
        filtro = list(Celtic_Fringe_Taxa_Category = "AIS", EU_Concern = "YES", Officially_listed = "NO", Presence_National_Nature_Reserve = "1")
      ),
      list(
        nombre = "Species_AIS_&_NOT_Listed_in_Natura_2000",
        filtro = list(Celtic_Fringe_Taxa_Category = "AIS", Officially_listed = "NO", Presence_Natura_2000 = "1")
      ),
      list(
        nombre = "Species_AIS_&_EU_Concern_&_NOT_Listed_in_Natura_2000",
        filtro = list(Celtic_Fringe_Taxa_Category = "AIS", EU_Concern = "YES", Officially_listed = "NO", Presence_Natura_2000 = "1")
      ),
      list(
        nombre = "Species_NAIS_&_Native",
        filtro = list(Celtic_Fringe_Taxa_Category = "NAIS", Local_Origin = "Native")
      ),
      list(
        nombre = "Species_NAIS_&_Alien",
        filtro = list(Celtic_Fringe_Taxa_Category = "NAIS", Local_Origin = "Alien")
      ),
      list(
        nombre = "Species_NAIS_&_Alien_&_Listed",
        filtro = list(Celtic_Fringe_Taxa_Category = "NAIS", Local_Origin = "Alien", Officially_listed = "YES")
      ),
      list(
        nombre = "Species_NAIS_&_Alien_&_NOT_Listed",
        filtro = list(Celtic_Fringe_Taxa_Category = "NAIS", Local_Origin = "Alien", Officially_listed = "NO")
      )
    )

    df_combinaciones <- map_dfr(combinaciones, function(x) {
      filtro <- x$filtro
      condiciones <- map2(names(filtro), filtro, ~ quo(!!sym(.x) == !!.y))
      data %>%
        filter(
          !!!condiciones,
          !!sym(Scope) == value
        ) %>%
        summarise(
          Category = x$nombre,
          Number = n_distinct(Species_with_Author[Species_with_Author != ""], na.rm = TRUE),
          Values = paste(na.omit(unique(Species_with_Author[Species_with_Author != ""])), collapse = " & ")
        )
    })

    resultado <- bind_rows(df_simple_counts, df_combinaciones)
    return(resultado)
  })

  names(resultados) <- Values

  for (i in names(resultados)) {
    assign(paste0(output_name, "_", i), resultados[[i]], envir = .GlobalEnv)
  }
}



#Now taxa report

.Checklist = CIP_Checklist()

.simple_counts <- c(
  "Species_with_Author", "Taxa_ID", "Taxa_URL", "Phylum", "Subphylum", "Order", "Family", "Genus",
  "Subprovince", "Constituent_Country_OR_Crown_Dependency",
  "Admin_units_II", "Admin_units_III", "National_Nature_Reserve_Name",
  "Natura_2000_Name", "UTM_grid", "Country", "EU_Concern", "Celtic_Fringe_Origin", "Celtic_Fringe_Taxa_Category")

.Taxa_report = c("Species_with_Author", "Taxa", "Taxa_ID", "Genus")

.report_Grids = c("Subprovince", "Presence_Natura_2000", "Presence_National_Nature_Reserve", "Special_Areas_of_Conservation_.Habitats_Directive.", "Special_Protection_Area_.Birds_Directive.", "Site_of_Community_Importance_.Habitats_Directive.", "Country", "Constituent_Country_OR_Crown_Dependency", "Admin_units_II", "Admin_units_III", "National_Nature_Reserve_Name", "Natura_2000_Name")

#' @title Automatic Taxa Report of Celtic Invasive Plants database or selections (Taxa_Report_CIPdb)
#' @description  Generate an automatic Taxa report of the Celtic Invasive Plants database or a selection of this of the unique values of the categorical columns, given a  value of the columns "Species_with_Author", "Taxa", "Taxa_ID" or "Genus".
#' @param 'data' must be the table obtained from CIPdb() or another selection of this (Select_CIPdb, WDPA_PID_select_CIPdb).
#' @param 'Scope' must correspond with one of the possible taxa categories: "Species_with_Author", "Taxa", "Taxa_ID" and "Genus".
#' The value in this argument must exactly correspond with the name of only one of these columns for this function to work.
#' @param 'Values' must be be either a character string or a vector of character strings.This must coincide with the potential values of the 'Scope' columns.
#' @return  This function returns a table object with 3 columns: Category, Number and Value.
#' The Category columnn refers to: (1) area categories ("Species_with_Author", "Taxa_ID", "Taxa_URL", "Phylum", "Subphylum", "Order", "Family", "Genus", Subprovince", "Country", "Constituent_Country_OR_Crown_Dependency", "Admin_units_II", "Admin_units_III", "National_Nature_Reserve_Name",  "Natura_2000_Name""EU_Concern", "Celtic_Fringe_Origin", "Celtic_Fringe_Taxa_Category") and (2) the country's listing and taxa category status of the taxa of interest ("Countries_AIS", "Countries_NAIS_&_Alien", "Countries_NAIS_&_Native", "Countries_AIS_&_Listed", "Countries_AIS_&_NOT_Listed") and (3) the occupied protected areas and their national status withing these area ("Occupied_National_Protected_Area_AIS_&_Listed_&_EU_Concern", "Occupied_National_Protected_Area_AIS_&_NOT_Listed_&_NOT_EU_Concern", "Occupied_Natura_2000_AIS_&_Listed_&_EU_Concern", "Occupied_Natura_2000_AIS_&_NOT_Listed_&_NOT_EU_Concern").
#' The Number column refers to the number of unique values of each category.
#' The Values column refers to a list of the detected unique values in each category in which the different elements are separated by " & ".
#' If the "Value" argument is fed with a vector containing more than one elements, this function will automatically create two dataframes (tables) named after the 'data' argument value and each of the vector element fed to the argument 'Value'.
#' @examples
#'
#'
#'#This provides a species report for Cotoneaster bullatus
#' Data = CIPdb()
#' My_Tax_report <- Taxa_Report_CIPdb(Data, "Taxa", "Cotoneaster bullatus")
#'
#'#This provides a genus report for Cotoneaster
#'Data = CIPdb()
#'My_Tax_report <- Taxa_Report_CIPdb(Data, "Genus", "Cotoneaster")
#'
#' #This provides a species a separate report for each species
#'My_species = c("Abutilon theophrasti Medik.", "Myriophyllum aquaticum (Vell.) Verdc.")
#'My_Tax_report <- Taxa_Report_CIPdb(Data, "Species_with_Author", My_species)
#'
#'#This provides a species a separate report for each species using their IDs
#' My_species = c("368166", "505938")
#' My_Tax_report <- Taxa_Report_CIPdb(Data, "Taxa_ID", My_species)
#'
#' @export
Taxa_Report_CIPdb <- function(data, Scope, Values, output_name = deparse(substitute(data))) {
  resultados <- map(Values, function(value) {
    if (Scope %in% .Taxa_report) {
      columnas_analisis <- setdiff(.Taxa_report, Scope)
    } else {
      columnas_analisis <- .Taxa_report
    }

    df_simple_counts <- map_dfr(.simple_counts, function(x) {
      data %>%
        filter(!!sym(Scope) == value) %>%
        summarise(
          Category = x,
          Number = n_distinct(.data[[x]][.data[[x]] != ""], na.rm = TRUE),
          Values = paste(na.omit(unique(.data[[x]][.data[[x]] != ""])), collapse = " & ")
        )
    })

    combinaciones_Countries <- list(
      list(
        nombre = "Countries_AIS",
        filtro = list(Celtic_Fringe_Taxa_Category = "AIS", Local_Origin = "Alien")
      ),
      list(
        nombre = "Countries_NAIS_&_Alien",
        filtro = list(Celtic_Fringe_Taxa_Category = "NAIS", Local_Origin = "Alien")
      ),
      list(
        nombre = "Countries_NAIS_&_Native",
        filtro = list(Celtic_Fringe_Taxa_Category = "NAIS", Local_Origin = "Native")
      ),
      list(
        nombre = "Countries_AIS_&_Listed",
        filtro = list(Celtic_Fringe_Taxa_Category = "AIS", Officially_listed = "YES")
      ),
      list(
        nombre = "Countries_AIS_&_NOT_Listed",
        filtro = list(Celtic_Fringe_Taxa_Category = "AIS", Officially_listed = "NO")
      )
    )

    df_combinaciones_Countries <- map_dfr(combinaciones_Countries, function(x) {
      filtro <- x$filtro
      condiciones <- map2(names(filtro), filtro, ~ {
        if (grepl("%in%", as.character(.y))) {
          values <- gsub("%in%c\\((.*)\\)", "\\1", .y)
          quo(!!sym(.x) %in% strsplit(values, ",")[[1]])
        } else {
          quo(!!sym(.x) == !!.y)
        }
      })
      data %>%
        filter(
          !!!condiciones,
          !!sym(Scope) == value
        ) %>%
        summarise(
          Category = x$nombre,
          Number = n_distinct(Country[Country != ""], na.rm = TRUE),
          Values = paste(na.omit(unique(Country[Country != ""])), collapse = " & ")
        )
    })

    combinaciones_NPs <- list(
      list(
        nombre = "Occupied_National_Protected_Area_AIS_&_Listed_&_EU_Concern",
        filtro = list(
          Celtic_Fringe_Taxa_Category = "AIS",
          Officially_listed = "%in%c(YES, NO)",
          EU_Concern = "YES",
          Presence_National_Nature_Reserve = "1"
        )
      ),
      list(
        nombre = "Occupied_National_Protected_Area_AIS_&_NOT_Listed_&_NOT_EU_Concern",
        filtro = list(
          Celtic_Fringe_Taxa_Category = "AIS",
          Officially_listed = "NO",
          EU_Concern = "NO",
          Presence_National_Nature_Reserve = "1"
        )
      )
    )

    df_combinaciones_NPs <- map_dfr(combinaciones_NPs, function(x) {
      filtro <- x$filtro
      condiciones <- map2(names(filtro), filtro, ~ {
        if (grepl("%in%", as.character(.y))) {
          values <- gsub("%in%c\\((.*)\\)", "\\1", .y)
          quo(!!sym(.x) %in% strsplit(values, ",")[[1]])
        } else {
          quo(!!sym(.x) == !!.y)
        }
      })
      data %>%
        filter(
          !!!condiciones,
          !!sym(Scope) == value
        ) %>%
        summarise(
          Category = x$nombre,
          Number = n_distinct(National_Nature_Reserve_Name[National_Nature_Reserve_Name != ""], na.rm = TRUE),
          Values = paste(na.omit(unique(National_Nature_Reserve_Name[National_Nature_Reserve_Name != ""])), collapse = " & ")
        )
    })

    combinaciones_Nat2000 <- list(
      list(
        nombre = "Occupied_Natura_2000_AIS_&_Listed_&_EU_Concern",
        filtro = list(
          Celtic_Fringe_Taxa_Category = "AIS",
          Officially_listed = "%in%c(YES, NO)",
          EU_Concern = "YES",
          Presence_Natura_2000 = "1"
        )
      ),
      list(
        nombre = "Occupied_Natura_2000_AIS_&_NOT_Listed_&_NOT_EU_Concern",
        filtro = list(
          Celtic_Fringe_Taxa_Category = "AIS",
          Officially_listed = "NO",
          EU_Concern = "NO",
          Presence_Natura_2000 = "1"
        )
      )
    )

    df_combinaciones_Nat2000 <- map_dfr(combinaciones_Nat2000, function(x) {
      filtro <- x$filtro
      condiciones <- map2(names(filtro), filtro, ~ {
        if (grepl("%in%", as.character(.y))) {
          values <- gsub("%in%c\\((.*)\\)", "\\1", .y)
          quo(!!sym(.x) %in% strsplit(values, ",")[[1]])
        } else {
          quo(!!sym(.x) == !!.y)
        }
      })
      data %>%
        filter(
          !!!condiciones,
          !!sym(Scope) == value
        ) %>%
        summarise(
          Category = x$nombre,
          Number = n_distinct(Natura_2000_Name[Natura_2000_Name != ""], na.rm = TRUE),
          Values = paste(na.omit(unique(Natura_2000_Name[Natura_2000_Name != ""])), collapse = " & ")
        )
    })

    resultado <- bind_rows(df_simple_counts, df_combinaciones_Countries, df_combinaciones_NPs, df_combinaciones_Nat2000)
    return(resultado)
  })

  names(resultados) <- Values

  for (i in names(resultados)) {
    assign(paste0(output_name, "_", i), resultados[[i]], envir = .GlobalEnv)
  }
}




.Taxa_Scope_Occup = c("Species_with_Author", "Taxa", "Taxa_ID", "Genus", "Phylum", "Subphylum", "Order", "Family")

.Admin_scope_Occup = c("Subprovince", "Country", "Constituent_Country_OR_Crown_Dependency", "Admin_units_II", "Admin_units_III", "National_Nature_Reserve_Name", "Natura_2000_Name")










