
library(xml2)
library(stringr)


url <- "http://scorecard.lcv.org/members-of-congress"

score_tbl <- read_html(url)
xml_children(score_tbl)

j <- xml_find_all(score_tbl,  "//div[@id='moc-list-table-data']/div")


parseMOC <- function(x) {
  l <- xml_find_all(x, "span")
  m <- xml_attr(l[1], "sort")
  j <- xml_text(l[3])
  n <- xml_attr(l[5], "sort")
  result <- list(moc=m, district = j, lifetime_rating=as.numeric(n))
  #result <- data.frame(moc=m, lifetime_rating=as.numeric(n))
  return(result)
}

ss <- lapply(j, parseMOC)
df <- data.frame(matrix(unlist(ss), nrow=542, byrow=T), stringsAsFactors = FALSE)

df[,3] <- as.numeric(df[,3])
df[,1] <- toupper(df[,1])
#aa <- regexpr("([A-Z]{2})-(\\d){2}", df[,2])
#jj <- regmatches(df[,2], aa)

#### MANUAL CORRECTIONS BEFORE PARSING
df[which(df[1]=="GIANFORTE, GREG"),2] <- 'MT-00'
df[which(df[1]=="BLUNT ROCHESTER, LISA"),2] <- 'DE-00'
df[which(df[1]=="YOUNG, DON"),2] <- 'AK-00'
df[which(df[1]=="WELCH, PETER F."),2] <- 'VT-00'
df[which(df[1]=="CHENEY, LIZ"),2] <- 'WY-00'
df[which(df[1]=="CRAMER, KEVIN"),2] <- 'ND' #running for Senate so marking as Senator

df <- df[df[1] != "NOEM, KRISTI",] #running for Governor so removing
df <- df[df[1] != "BECERRA, XAVIER",]
df <- df[df[1] != "PRICE, TOM",]
df <- df[df[1] != "CHAFFETZ, JASON",]
df <- df[df[1] != "FARENTHOLD, BLAKE",]
df <- df[df[1] != "MULVANEY, MICK",]
df <- df[df[1] != "MURPHY, TIM",]
df <- df[df[1] != "BRADY, ROBERT A.",]
df <- df[df[1] != "DENT, CHARLIE",]
df <- df[df[1] != "BARLETTA, LOU",]
df <- df[df[1] != "TIBERI, PAT",]
df <- df[df[1] != "POMPEO, MIKE",]
df <- df[df[1] != "ZINKE, RYAN",]

# PA redrew lines in 2018
df[which(df[1]=="BOYLE, BRENDAN"),2] <- "PA-02"
df[which(df[1]=="THOMPSON, GLENN W."),2] <- "PA-04"
df[which(df[1]=="PERRY, SCOTT"),2] <- "PA-10"
df[which(df[1]=="DOYLE, MIKE"),2] <- "PA-18"
df[which(df[1]=="CARTWRIGHT, MATT A."),2] <- "PA-08"
df[which(df[1]=="FITZPATRICK, BRIAN"),2] <- "PA-01"
df[which(df[1]=="ROTHFUS, KEITH J."),2] <- "PA-17"
df[which(df[1]=="SMUCKER, LLOYD"),2] <- "PA-11"
df[which(df[1]=="MARINO, TOM"),2] <- "PA-12"
df[which(df[1]=="EVANS, DWIGHT"),2] <- "PA-03"
df[which(df[1]=="KELLY, MIKE"),2] <- "PA-16"

#https://stackoverflow.com/questions/18620571/extract-capture-group-matches-from-regular-expressions-or-where-is-gregexec
#str_extract(df[,2], "[A-Z]{2}-\\d{2}")
df$reg <- str_match_all(df[,2], "([A-Z]{2})-(\\d{2})")


reps <- subset(df, sapply(reg, length) > 0)
others <- subset(df, sapply(reg, length) <= 0)
others[,c(4)] <- NULL
colnames(others) <- c("MOC", "State", "Lifetime_Rating")

reps <- cbind(reps, matrix(unlist(reps$reg), nrow=nrow(reps), byrow=T))
reps[,c(2,4:5)] <- NULL
colnames(reps) <- c("MOC", "Lifetime_Rating", "State", "District")
reps[,c(3)] <- as.character(reps[,c(3)])
reps[,c(4)] <- as.numeric(levels(reps[,c(4)]))[reps[,c(4)]]
#as.numeric(levels(f))[f]

#### MANUAL CORRECTIONS in CONSERVANCY

candidates <- read.csv("cn.txt", sep="|", header=FALSE, stringsAsFactors = FALSE)
# Am looking only at incumbents who have a track record
candidates <- subset(candidates, V4 >= 2018 & V8=="I")

house_candidates <- subset(candidates, V6 == "H")

#### MANUAL CORRECTIONS in CONSERVANCY
house_candidates <- house_candidates[house_candidates$V2 != "MURPHY, TIMOTHY",]
house_candidates <- house_candidates[house_candidates$V2 != "FRANKS, TRENT",]
house_candidates <- house_candidates[house_candidates$V2 != "BECERRA, XAVIER",]
house_candidates <- house_candidates[house_candidates$V2 != "PRICE, THOMAS EDMUNDS",]
house_candidates <- house_candidates[house_candidates$V2 != "CHAFFETZ, JASON",]
house_candidates <- house_candidates[house_candidates$V2 != "FARENTHOLD, RANDOLPH BLAKE",]
house_candidates <- house_candidates[house_candidates$V2 != "MULVANEY, JOHN MICHAEL 'MICK'",]
house_candidates <- house_candidates[house_candidates$V2 != "BRADY, ROBERT A",]
# LAMB is running but has no record in conservancy so removing him
house_candidates <- house_candidates[house_candidates$V2 != "LAMB, CONOR",]
house_candidates <- house_candidates[house_candidates$V2 != "DENT, CHARLES WIEDER",]
house_candidates <- house_candidates[house_candidates$V2 != "BARLETTA, LOU",]
house_candidates <- house_candidates[house_candidates$V2 != "TIBERI, PATRICK J.",]
house_candidates <- house_candidates[house_candidates$V2 != "POMPEO, MICHAEL R",]
house_candidates <- house_candidates[house_candidates$V2 != "ZINKE, RYAN K",]


hc <- house_candidates[,c(1:2,5,7)]
together <- merge(hc, reps, all=TRUE, by.x=c("V5","V7"), by.y=c("State", "District"))
together[duplicated(together[,c(1:2)]),]

#######
## SENATE CANDIDATES
senate_candidates <- subset(candidates, V6 == "S")
tog2 <- merge(senate_candidates[,c(1:2,5)], others, by.x=c("V5"), by.y=c("State"))


candidates[,1:2]
str(candidates)

write.csv(tog2, "conservancy_ratings.csv", row.names = FALSE)

write.csv(together, "conservancy_ratings_house.csv", row.names = FALSE)
