library(dplyr)
library(openxlsx)
library(stringi)
library(magrittr)

### -------------------------------------------------------
correspondence <- read.xlsx("Regest_Schwab_v2.xlsx")

for(i in seq_len(ncol(correspondence))){
  if(class(correspondence[,i]) == "character")
    correspondence[,i] <- stri_trim_both(correspondence[,i])
}

### -------------------------------------------------------
transform_date <- function(x){
  x <- gsub("\\.", "\\-", x)
  x <- as.Date(x, format = "%d-%m-%Y")
  x  
}

dates_correct <- correspondence$Datum[grep("^\\d{3,8}$", correspondence$Datum)]
dates_correct <- as.Date(as.numeric(dates_correct)-25569, origin = "1970-01-01")
correspondence$Datum[grep("^\\d{3,8}$", correspondence$Datum)] <- as.character(dates_correct)


fdata_format <- correspondence$Datum[!grepl("\\d{4}\\-\\d{2}\\-\\d{2}", correspondence$Datum)]
fdata_format[grep("[kKnN]\\.( )?[aAvVlL]", fdata_format)] <- NA

dates_dot <- stri_trim_both(fdata_format[grepl("\\d{2}\\.\\d{2}\\.\\d{4}", fdata_format)])
dates_dot <- gsub("\\[add\\.\\]|Hand Notiz: ca ", "", dates_dot)
dates_dot <- stri_trim_both(dates_dot)
dates_dot <- gsub("/.*$", "", dates_dot)
dates_dot <- gsub("\\.", "\\-", dates_dot)


dates_flawed <- fdata_format[!grepl("\\d{2}\\.\\d{2}\\.\\d{4}", fdata_format)]
dates_flawed[!is.na(dates_flawed)] <- c(NA,
                                        "24.04.1947",
                                        "17.11.1950",
                                        "19.03.1952",
                                        "14.09.1960",
                                        NA)

a <- grepl("\\d{2}\\.\\d{2}\\.\\d{4}", fdata_format)
b <- !grepl("\\d{2}\\.\\d{2}\\.\\d{4}", fdata_format)


dates_dot <- transform_date(dates_dot)
dates_flawed <- transform_date(dates_flawed)

fdata_format[a] <- dates_dot
fdata_format[b] <- dates_flawed

correspondence$Datum[!grepl("\\d{4}\\-\\d{2}\\-\\d{2}", correspondence$Datum)] <- fdata_format
correspondence$Datum <- as.Date(correspondence$Datum)


### -------------------------------------------------------
addColumn_suffix <- function(column_name, suffix) {
  new_col_name <- paste0(suffix, "_", column_name)
  correspondence[,new_col_name] <- FALSE
  correspondence[,new_col_name][grep(suffix, correspondence[,column_name])] <- TRUE
  return(correspondence[,new_col_name])
}


correspondence$add_Absender <- addColumn_suffix("Absender", "add")
correspondence$hs_Absender <- addColumn_suffix("Absender", "hs")
correspondence$add_Empfänger <- addColumn_suffix("Empfänger", "add")
correspondence$hs_Empfänger <- addColumn_suffix("Empfänger", "hs")
correspondence$add_Ort <- addColumn_suffix("Ort", "add")
correspondence$note <- FALSE
correspondence$note[grep("[nN]otiz", correspondence$handschriftlich.)] <- TRUE
correspondence$add_Antwort_auf_Brief <- addColumn_suffix("Antwort.auf.Brief.v..Datum", "add")
correspondence$nv_Antwort_auf_Brief <- addColumn_suffix("Antwort.auf.Brief.v..Datum", "\\([nN]\\.(\\s)?[vV](\\.)?\\)")


### -------------------------------------------------------
cleanCol <- function(column_name){
  correspondence[,column_name] <- stri_trim_both(correspondence[,column_name]) %>%
    gsub("[jJ]a", "ja", .) %>%
    gsub("[kK](\\.)?( )?[aA](\\.)?", "k.A.", .) %>%
    gsub("[nN](\\.)?( )?[lL](\\.)?", "k.A.", .) %>%
    gsub("[jJ]a(.)*[kK](.+)?[aA](.+)?", NA, .)
  return(as.factor(correspondence[,column_name]))
}

names_forcleaning <- colnames(correspondence)[which(grepl("[jJ]a([.\\s+\\/])*[kK](.+)?[aA](.+)?", correspondence[1,]))]

for(i in seq_along(names_forcleaning)){
  correspondence[,names_forcleaning[i]] <- cleanCol(names_forcleaning[i])
}

### -------------------------------------------------------
correspondence$Sprache <- correspondence$Sprache %>%
  gsub("\\s*", "", .) %>%
  gsub("[dD]t", "dt", .) %>%
  gsub("eng(l)?", "engl", .) %>%
  gsub("/", "+", .) %>%
  gsub("engl\\+dt|dt\\+engl",
       "engl+dt", .) %>%
  gsub("[kKnN](\\.)?( )?[aAvV](\\.)?",
       "k.A.", .) %>%
  as.factor()

### -------------------------------------------------------
correspondence$`handschriftlich?` <- correspondence$`handschriftlich?` %>%
  gsub("\\s*", "", .) %>%
  gsub("(/)?( )?[nN]otiz", "", .) %>%
  gsub("hand/masch|masch/hand", "beides", .) %>%
  gsub("mas(c)?h( )?", "masch", .)  %>%
  as.factor()

### -------------------------------------------------------
correspondence$FINALE.VERSION <- correspondence$FINALE.VERSION %>%
  gsub("[Jj]a", "ja", .) %>%
  as.factor()

### -------------------------------------------------------
correspondence$`Finale.Version.der.Transkription?` <- correspondence$`Finale.Version.der.Transkription?` %>%
  as.factor()

### -------------------------------------------------------
letter_ids <- lapply(stri_extract_all(correspondence$Antwort.auf.Brief.v..Datum, regex = "\\(\\d{3,7}(-\\d{3,7})?\\)"), paste0, collapse = " ")
correspondence$Antwort_auf_Brief_ID <- unlist(letter_ids)

correspondence$Antwort.auf.Brief.v..Datum <- correspondence$Antwort.auf.Brief.v..Datum %>%
  gsub("\\(\\d{3,7}(-\\d{3,7})?\\)", "", .) %>%
  gsub("\\[add\\.\\]", "", .) %>%
  gsub("\\([Nn]\\.(\\s)?[vV](\\.)?\\)", "", .) %>%
  gsub("[nNkK]\\.( )?[vVlLaA]\\.", NA, .)

### -------------------------------------------------------
## contents <- as.data.frame(matrix(nrow = nrow(correspondence)))
## contents$tags <- correspondence$SCHLAGWORTE
## contents$tags <- gsub(";", ",", contents$tags)

## tags <- strsplit(contents$tags, ",")
## tags <- lapply(tags, stri_trim_both) %>%
##   lapply(tolower)
## tmp <- unlist(tags)

## file <- "schlagworte.txt"
## writeLines(sort(unique(tmp)), file)

### -------------------------------------------------------
correspondence$Empfänger_zusatz <- stri_trim_both(stri_extract(correspondence$Empfänger, regex = "((\\s*)?\\(.*?\\))")) 
correspondence$Empfänger[grep("(\\s)?\\[add\\.(\\s*)?\\]", correspondence$Empfänger)]
correspondence$Empfänger <- gsub("(\\s)?\\[add\\.(\\s*)?\\]", "", correspondence$Empfänger)
correspondence$Empfänger <- stri_trim_both(correspondence$Empfänger)

correspondence$Empfänger <- correspondence$Empfänger %>%
  gsub("Amt für kontrollierte Vermögen|Amt für kontrolliertes Vermögen", "Amt für kontrolliertes Vermögen", correspondence$Empfänger, .) %>%
  gsub("Amt für Vermögenskontrolle und Wiedergutmachung|Amt für Vermögenskontrolle und Wiedergutmachung Offenbach", "Amt für Vermögenskontrolle und Wiedergutmachung Offenbach", correspondence$Empfänger, .) %>%
  gsub("Braeuer,  Leni und Prof. Dr. Walter|Braeuer, Leni und Walter", "Braeuer, Leni und Walter", .) %>%
  gsub("Tuteur, Rosa und Siegfried|Tuteur, Rosa und Sigfried|Tuteur, Rosa  und Tuteur, Siegfried|Tuteur, Siegfried und Rosa|Tuteur,RosaundSiegfried", "Tuteur, Rosa und Siegfried", .) %>%
  gsub("Walter,August|Walter, August|Walter, Ernst August", "Walter, August", .) %>%
  gsub("Walter,August Fam.|Walter, August und Familie (\"Liebe Walters\")|Walter, August und Familie \"Meine lieben Walters\"", "Walter, August und Familie", .) %>%
  gsub("Walter, August undIda|Walter, August und Ida|Walter Ida und August", "Walter, August und Ida", .) %>%
  gsub("Wormser Irene und Schwab, Rudolf|Wormser, Irene und Schwab Rudolf|Wormser, Irene und Schwab, Rudolf", "Wormser, Irene und Schwab, Rudolf", .) %>%
  gsub("Zentralamt für Vermögensverwaltung|Zentralamt für Vermögensverwaltung Bad Nenndorf", "Zentralamt für Vermögensverwaltung Bad Nenndorf", .) %>%
  gsub("Rhein-Main Bank|Rhein-Main Bank AG|Rhein-Main-Bank AG", "Rhein-Main-Bank", .) %>%
  gsub("Demuth, Armand und Helen|Demuth, Armund und Helen", "Demuth, Armand und Helen", .) %>%
  gsub("Herbold,Georg|Herbold, Georg", "Herbold, Georg", .) %>%
  gsub("Kommission der Deutschen Zentralfinanzverwaltung|Kommission derDeutschen Zentralfinanzverwaltung", "Kommission der Deutschen Zentralfinanzverwaltung", .) %>%
  gsub("Legal Aid Department.+$", "Legal Aid Department", .) %>%
  gsub("Tighy, Sarel|Tighy, Serel", "Tighy, Sarel", .) %>%
  gsub("  | ", "", .)

# correspondence$Empfänger <- gsub("[a-z][\\,][A-Z]", "[a-z][\\,][\\s][A-Z]", correspondence$Empfänger)
correspondence$Empfänger <- gsub("((\\s*)?\\(.*?\\))", "", correspondence$Empfänger)
correspondence$Empfänger[grep("[kKnN]\\.( )?[aAvVlL]", correspondence$Empfänger)] <- NA

### -------------------------------------------------------
correspondence$Absender[grep("(\\s)?\\[add(\\.)?(.*?)?\\]", correspondence$Absender)]
correspondence$Absender <- gsub("(\\s)?\\[add(\\.)?(.*?)?\\]", "", correspondence$Absender)
correspondence$Absender <- stri_trim_both(correspondence$Absender)

correspondence$Absender <- correspondence$Absender %>%
  gsub("Schwab, Hans Ferdinand|Schwab, Hans-Ferdinand", "Schwab, Hans-Ferdinand", .) %>%
  gsub("Rudolph|Rudolf", "Rudolf", .) %>%
  gsub("Tuteur, Siegfried|Tuteur,Siegfried|Siegfried, Tuteur|Tuteur, Sigfried", "Tuteur, Siegfried", .) %>%
  gsub("Tuteur, Rosa|Tuteur, Rosaa", "Tuteur, Rosa", .) %>%
  gsub("Kipfer, Karl F. J.|Kiper, Karl|Kipfer, Karl|Kipferl, Karl", "Kipfer, Karl", .) %>%
  gsub("Walter, August|Walter, Ernst August","Walter, August", .) %>%
  gsub("Familie, Walter|Fam. Walter", "Familie, Walter", .) %>%
  gsub("Anglo Continental Exchange|Anglo Continental Exchange Ltd.", "Anglo Continental Exchange Ltd.", .) %>%
  gsub("Braeuer, Leni|Brauer, Leni", "Braeuer, Leni", .) %>%
  gsub("Braeuer, Prof. Dr. Walter|Braeuer, Walter", "Braeuer, Walter", .) %>%
  gsub("Fischbein, Bernard|Fischbein, Bernard (Zentralanmeldeamt)", "Fischbein, Bernard", .) %>%
  gsub("Fleischmann, Antonie|Fleischmann, Toni", "Fleischmann, Antonie", .) %>%
  gsub("Hausmann, Johanna|Hausmann, Johanna Sara", "Hausmann, Johanna", .) %>%
  gsub("Herbold,Georg|Herbold, Georg", "Herbold, Georg", .) %>%
  gsub("Herrmann, Heinz|Herrmann, Heinz (United Restitution Organization)", "Herrmann, Heinz (United Restitution Organization)", .) %>%
  gsub("Kommission der Deutschen Zentralfinanzverwaltung|Kommission der Zentralfinanzverwaltung", "Kommission der Deutschen Zentralfinanzverwaltung", .) %>%    
  gsub("Landgericht-Restitutionskammer|Mainz, Landgericht-Restitutionskammer", "Mainz, Landgericht-Restitutionskammer", .) %>%
  gsub("Rhein-Main Bank|Rhein-Main-Bank|Rhein-Main Bank AG", "Rhein-Main Bank", .) %>%
  gsub("Schmitt, Greta|Schmitt, Gretchen", "Schmitt, Greta", .) %>%
  gsub("Tighy, Sarel|Tighy, Serel", "Tighy, Sarel", .) %>%
  gsub("Wormaor, Irene|Wormser Irene|Wormser, Irene", "Wormser, Irene", .) %>%
  gsub("  | ", "", .)

# correspondence$Absender <- gsub("[a-z][\\,][A-Z]", "[a-z][\\,][\\s][A-Z]", correspondence$Absender)
correspondence$Absender[grep("[kKnN]\\.( )?[aAvVlL]", correspondence$Absender)] <- NA

### -------------------------------------------------------
names <- c("seite_von",
           "seite_bis",
           "pdf_seite_von",
           "pdf_seite_bis",
           "datum",
           "absender",
           "ort",
           "empfänger",
           "antwort_auf_brief",
           "sprache",
           "schreibart",
           "ns_politik",
           "soz_situation_benachteiligung",
           "ermord_deport_kz_ghetto",
           "ss_sa_wehr_gest",
           "enteignung",
           "wussten_dt_v_vernicht",
           "wiedergutmachung",
           "schlagworte",
           "kurzbeschr_inhalt",
           "X21",
           "finale_version",
           "transkription",
           "finale_version_tr",
           "add_absender",
           "hs_absender",
           "add_empfänger",
           "hs_empfänger",
           "add_ort",
           "note",
           "add_antwort_auf_brief",
           "nv_antwort_auf_brief",
           "antwort_auf_brief_ID",
           "empfänger_zusatz")

colnames(correspondence) <- names
# correspondence <- dplyr::select(correspondence, -(seite_von:pdf_seite_bis))


### -------------------------------------------------------
correspondence$ort[grep("[kKnN]\\.( )?[aAvVlL]", correspondence$ort)] <- NA
correspondence$ort <- gsub("(\\s)?\\[add\\.(\\s*)?\\]", "", correspondence$ort)

correspondence$ort <- correspondence$ort %>%
  gsub("Clairac|Clairac(L.&G.)|Clairae (Lot et Gar.)", "Clairac", .) %>%
  gsub("Frankfurt a.M.|Frankfurt a. Main|Frankfurt am Main|Frankfurt (Main)", "Frankfurt a.M.", .) %>%
  gsub("Hampstead|Hamstead", "Hampstead", .) %>%
  gsub("Hanau|Hanau a. Main|Hanau/Main|Hanau ", "Hanau", .) %>%
  gsub("Johannesburg|Johannesburg |Johannisburg", "Johannesburg", .) %>%
  gsub("Langendiebach|Langendiebach Hanau/Main", "Langendiebach", .) %>%
  gsub("Marburg|Marburg/ Lahn", "Marburg", .) %>%
  gsub("Offenbach am Main|Offenbach|Offenbach/Main", "Offenbach am Main", .) %>%
  gsub("Sao Paulo|São Paulo|S.Paulo|S. Paulo", "Sao Paulo", .) %>%
  gsub("Worms|Worms a. Rh.", "Worms", .)

### -------------------------------------------------------
save(correspondence, file = "data/correspondence_preprocessed.RData")