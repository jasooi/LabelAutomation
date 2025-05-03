setwd("C://Users//OOJ2SGP//Desktop")
library(data.table)

PStestdata <- fread("PH Jul Shopee YTD csv.csv")
PStestdata1000 <- fread("PH Jul Shopee YTD csv.csv")

#PREPROCESSING - concatenate categories & convert to lowercase
PStestdata <- PStestdata[, allcat:= paste(`cat 1`, `cat 2`, `cat 3`)]
PStestdata$allcat <- tolower(PStestdata$allcat)
PStestdata <- PStestdata[, productName := tolower(productName)]

#Keywords regexes for AA/PT/BSH/0
keyword_0 <- "sticker|sunglass|flask|drum|massage|jersey|shirt|blouse|shoe|hair|mug|cloth mask|pm2.5|filter mask|leather|decal|alarm|haze|clothing|paint roller|towel|dog|magnifier|padlock|earphone|lunch|growler|fender reflector|thermos|tumbler|car cover|cutlery"
keyword_AA <- "spark plug|honda|toyota|mercede|mitsubi|iridium|bmw|proton|windscreen|wiper blade|aerotwin blade|plus blade|oil filter|horn|automotive|headlight|avanza|spectra|mitsubishi|cabin filter|volkswagen|fuel|mazda|brake|exora|altis|audi |motorcycle"
keyword_PT <- "angle grind|mounting system|pressure washer|floodlight|trolley|wood |glove|spray.*gun|safety goggles|battery|charger|screw|glue gun|high pressure|table saw|drill|hammer|circular saw|wrench|rivet|sander|grass trimmer|screwdrive| plier| spanner| trimmer|heat gun|measuring tape| cutter|carrying case|torch|flashlight|air gun|laser measure|hexkey|hex key|allen key|multi tool| blower|laser rangefind|planer|socket set|impact driver|polisher|worklight|carbon brush|x-line|aquatak| aqt |rotary |cut-off|wet.{,5}dry|dust extractor|sanding|plunge router|chisel"
keyword_PT_forAA <- "angle grind|mounting system|floodlight|trolley|wood |glove|safety goggles|screw|glue gun|high pressure|table saw|drill|hammer|circular saw|wrench|rivet|sander|grass trimmer|screwdrive| plier| spanner| trimmer|heat gun|measuring tape| cutter|carrying case|torch|flashlight|air gun|laser measure|hexkey|hex key|allen key|multi tool| blower|laser rangefind|planer|impact driver|polisher|worklight|x-line|aquatak|wet.{,5}dry|dust extractor|sanding|chisel"
Keyword_BSH <- "stove|kitchen|meat grind|siemen|microwave|dryer|handheld vacuum|load wash|fridge|oven|kettle|coffee|cooker|toaster|dishwasher|barbecue|hob| hood|blender|food processor|juice|washing machine|burner box|steam|picnic|grill|mixer|car vacuum"

keyword_non0 <- paste(keyword_AA,"|", keyword_PT,"|", Keyword_BSH, collapse = "")
keyword_nonAA <- "angle grind|mounting system|trolley|glove|safety goggles|glue gun|high pressure|table saw|drill|hammer|circular saw|grass trimmer|heat gun|measuring tape| cutter|carrying case|torch|flashlight|air gun|laser measure|planer|impact driver|polisher|worklight|x-line|aquatak|dust extractor|sanding|chisel|sticker|sunglass|flask|drum|massage|jersey|shirt|blouse|shoe|hair|pm2.5|filter mask|leather|decal|haze|clothing|paint roller|towel|dog|magnifier|padlock|earphone|lunch|growler|fender reflector|thermos|tumbler|car cover|cutlery |stove|kitchen|meat grind|siemen|microwave|dryer|handheld vacuum|load wash|fridge|oven|kettle|coffee|cooker|toaster|dishwasher|barbecue|hob|blender|food processor|juice|washing machine|burner box|steam|picnic|grill|mixer|car vacuum"


#irrelevant categories
irrelevant_cat1 <- c('Babies & Kids', 'Health & Personal Care','Makeup & Fragrances',"Men's Apparel","Men's Bags & Accessories","Men's Shoes",'Pet Care',"Women's Accessories","Women's Apparel","Women's Bags","Women's Shoes","Toys, Games & Collectibles")
irrelevant_cat2 <- c('Books & Magazines','Books','Musical Instruments','Bath','Dinnerware','Kitchenware','Garment Care','Earphones',"Men's Activewear","Women's Activewear","Weather Protection")
irrelevant_cat3 <- c('Yarn Threads','Wall Decor',' Keys, Locks, & Safes','Door Bells & Chimes','Fire Alarms & Carbon Monoxide Detectors','Paint Supplies','Seeds & Seedlings','Shelves','Speakers/Home Theaters','Audio Adapters','Wireless USB Adapters','Screen Protector','Earphones','Dent Repairs & Paint Protection','Spray Paints','Waxes & Polishes','Car Decals & Stickers','Moto Decals & Stickers','Motorcycle Covers','Fishing Gear','Pollution Masks','Flooring',' Baskets ,Bins, & Containers','Wall Mounts')
#concat cat1,2,3

#GB categories
AA_cat <- c("Motors",PStestdata[`cat 1`== "Motors"][["cat 2"]],PStestdata[`cat 1`== "Motors"][["cat 3"]])
PT_cat3 <- PStestdata[PrevGB == "PT"][["cat 3"]] 

#First, categorise GB = 0
PStestdata1 <- PStestdata
seq1 <- seq(1,nrow(PStestdata1))
check1 <- seq1[!(seq1 %in% grep(pattern = keyword_non0, x = PStestdata1$productName, value = F, fixed = F))]
check2 <- PStestdata1[((`cat 1` %in% irrelevant_cat1)|(`cat 2` %in% irrelevant_cat2)|(`cat 3` %in% irrelevant_cat3)),which = TRUE]
check3 <- PStestdata1[PrevGB == "*", which = T]
PStestdata1 <- PStestdata1[intersect(intersect(check1,check2), check3), c("PrevGB", "PrevBrand"):= 0]

check4 <- PStestdata1[PrevGB == "*", which = T]
PStestdata1 <- PStestdata1[intersect(intersect(check4, grep(pattern = keyword_0, x = PStestdata1$productName, value = F, fixed = F)),check1),c("PrevGB", "PrevBrand"):= 0 ]

check5 <- PStestdata1[PrevGB == "*" &  ((`cat 1` %in% irrelevant_cat1)|(`cat 2` %in% irrelevant_cat2)|(`cat 3` %in% irrelevant_cat3)), which = T]
PStestdata1 <- PStestdata1[intersect(check5, grep(pattern = "hammertone|sunglass|hoodie|sticker|earphone|anti-haze|drum sticks|sleeve t", x = PStestdata1$productName,value = F, fixed = F)), c("PrevGB", "PrevBrand"):= 0]

check12 <- intersect(PStestdata1[PrevGB == "*" , which= T ], grep("fear the deer|thermometer|dog food|shirt|giannis", PStestdata1$productName, value = F, fixed = F))
PStestdata1 <- PStestdata1[check12, c("PrevBrand","PrevGB") := 0]

####Next, categorise GB = AA
PStestdata2 <- PStestdata1
PStestdata2$Brand <- tolower(PStestdata2$Brand)
check6 <- PStestdata2[!grep(pattern = keyword_nonAA, x = PStestdata2$productName, value = F, fixed = F), which = T]
PStestdata2 <- PStestdata2[PrevGB == "*" & Brand %in% c("denso", "yuasa", "gs yuasa",  "ngk", "nwb", "vic","vic filter"), PrevGB := "AA"]
check7 <- PStestdata2[PrevGB == "*", which = T]
PStestdata2 <- PStestdata2[intersect(check7, intersect(grep(pattern = "denso|yuasa", x = PStestdata2$productName, value = F, fixed = F), check6)), PrevGB := "AA"]
check8 <- PStestdata2[PrevGB == "*", which = T]
PStestdata2 <- PStestdata2[intersect(check8,  intersect(grep(".*[^a-zA-Z0-9](vic).*|.*[^a-zA-Z0-9](ngk).*|.*[^a-zA-Z0-9](nwb).*|^vic|^ngk|^nwb",x = PStestdata2$productName, value = F, fixed = F), check6)), PrevGB := "AA"]

PStestdata2 <- PStestdata2[intersect(PStestdata2[PrevGB == "*", which = T],intersect(check6, grep(pattern = keyword_AA, x = PStestdata2$productName, value = F, fixed = F))), PrevGB := "AA"]
check8_1 <- PStestdata2[`cat 1` == "Motors" , which = T]
check8_2 <- PStestdata2[PrevGB == "*",which = T]
check8_3 <- grep(keyword_AA,PStestdata2$productName,value = F, fixed = F)
PStestdata2 <- PStestdata2[intersect(check8_1,intersect(check8_2, check8_3)), PrevGB := "AA"]

##Where sellers are exclusive AA (& preferably Preferred/Shopee Mall)
PStestdata2 <- PStestdata2[PrevGB == "*" & seller %in% c("anamalloautomotive","grandluxor", "3y.ph","federalmogulphils", "blade101", "chelsea_car_care","cytmotohub","kabayanautosupply.ph", "powermaxxph", "purplequeenarcher", "reddragon.ph", "zkrmotorparts"), PrevGB := "AA"]

#checkback (PT / BSH brands should not be under AA)
keyword_checkback_AA <- "makita|ingco|stanley|black.*decker|polisher|angle grind|snow foam|aquatak|hex key"
check8_4 <- PStestdata2[PrevGB == "AA", which = T]
PStestdata2 <- PStestdata2[intersect(check8_4, grep(keyword_checkback_AA, PStestdata2$productName, value = F, fixed = F)), PrevGB:= "*"]



###Then, categorise PT
PStestdata3 <- PStestdata2
check9 <- PStestdata3[PrevGB == "*" & Brand %in% c("ingco", "stanley",  "makita"), which = T]
check10 <- PStestdata3[!grep(pattern = keyword_0, x = PStestdata3$productName, value = F, fixed = F), which = T]
PStestdata3 <- PStestdata3[intersect(check9, check10 ), PrevGB := "PT"]

check11 <- PStestdata3[PrevGB == "*", which = T]
PStestdata3 <- PStestdata3[intersect(check11, intersect(grep(pattern = "ingco|stanley|makita", x = PStestdata3$productName, value = F, fixed = F), check10)), PrevGB := "PT"]


check13 <- grep(pattern = keyword_PT, x = PStestdata3$productName, value = F, fixed = F)
check14 <- PStestdata3[`cat 3` %in% PT_cat3, which = T]
check15 <- PStestdata3[PrevGB == "*", which = T]
PStestdata3 <- PStestdata3[intersect(check15,intersect(check13, check14)), PrevGB:= "PT"]
PStestdata3 <- PStestdata3[seller %in% c("boschbybge", "makita_ph", "buildmate","mabuhay.ph", "tools2018", "powermarkfortune", "industrialsupplyph", "stanleyhandtoolsph", "wilconstructenterprise"), PrevGB := "PT"]

# keyword_PT_refined <- "jigsaw|cutting|"
PTbrandlist <- unique(tolower(PStestdata[PrevGB == "PT"][["PrevBrand"]]))
PTbrandlist <- PTbrandlist[-which(PTbrandlist== "no brand")]
PTbrandlist <- c(PTbrandlist, "blackdecker","black decker", "black+decker", "black + decker", "black and decker")
PStestdata3 <- PStestdata3[PrevGB == "*" & Brand %in% PTbrandlist & `cat 3`=="Power Tools", PrevGB:= "PT" ]


#checkback for PT
keyword_checkback_PT <- "yuasa|relay horn|denso|smoke alarm"
check16 <- PStestdata3[PrevGB == "PT", which = T]
PStestdata3 <- PStestdata3[intersect(check16, grep(keyword_checkback_PT, PStestdata3$productName, value = F, fixed = F)), PrevGB:= "*"]

#final keywords elimination for AA, PT
fkeyword_AA <- "bosch.+relay|cabin|air filter|sparkplug|spark plug|clear advantage|bosch.+advantage|aerotwin| aero twin"
fkeyword_PT <- "aquatak|car washer|saw|li-ion.+battery|pressure washer|snow foam|wet.*dry.+vacuum|vacuum.+wet.*dry|tool box|laser|concrete|oscillating|lawnmower|grinder|car.+polisher"
check17 <- grep(fkeyword_AA, PStestdata3$productName, value = F, fixed = F)
check18 <- grep(fkeyword_PT, PStestdata3$productName, value = F, fixed = F)
check19 <- PStestdata3[PrevGB == "*", which = T]
PStestdata3 <- PStestdata3[intersect(check17,check19), PrevGB := "AA"]
check20 <- PStestdata3[PrevGB == "*", which = T]
PStestdata3 <- PStestdata3[intersect(check18,check20), PrevGB := "PT"]


#BSH
keyword_BSH_broad <- "cooker|handheld.+vacuum|dishwasher|dust.*buster|steam mop|induction hob|car.+vacuum|washing machine|hand vacuum|household vacuum"
check21 <- PStestdata3[PrevGB == "*", which = T]
check22 <- grep(keyword_BSH_broad, PStestdata3$productName, value = F, fixed = F)
PStestdata3 <- PStestdata3[intersect(check21,check22), PrevGB:="BSH"]


###Brand
PStestdata4 <- PStestdata3
PStestdata4$PrevBrand <- tolower(PStestdata4$PrevBrand)
#start with sellers first
sellerlist <- c("anamallo_corporation", "anamalloautomotive", "boschbybge", "ingco", "stanleytools", "makita_ph", "boschmy", "stanley.os","boschautomotive.os", "blackdecker")
PStestdata4[seller %in% sellerlist & PrevBrand == "#N/A"]$PrevBrand <- PStestdata4[seller %in% sellerlist & PrevBrand == "#N/A"]$Brand

#Contains [for BRAND]
brandlist_regex <- c('bosch','ingco','makita','stanley','black.*decker','dewalt','maktec','denso','ngk','nwb','yuasa','varta','motolite','mann','sakura','vic','aki gs')
brandregex_0_1 <- paste0("for.+",brandlist_regex[-length(brandlist_regex)],"|")
brandregex_forBrand <- paste(c(brandregex_0_1,paste0("for.+",brandlist_regex[length(brandlist_regex)])), collapse = "")

check22_0 <- PStestdata4[PrevBrand == "#n/a", which = T]
PStestdata4 <- PStestdata4[intersect(check22_0,grep(pattern = brandregex_forBrand, x = PStestdata4$productName, value = F, fixed = F)), PrevBrand := "no brand"]


#Starts with [BRAND]
library(stringr)
brandlist <- c('bosch','makita','ingco','stanley','dewalt','maktec','denso','ngk','nwb','yuasa','varta','motolite','mann','sakura')


# brandregex0_1 <- paste0("^(",brandlist[-length(brandlist)],")|")
# brandregex1 <- paste(c(brandregex0_1,paste0("^(",brandlist[length(brandlist)],")")), collapse = "")
check22_1 <- which(word(PStestdata4$productName,1) %in% brandlist)
check22_2 <- PStestdata4[PrevBrand == "#n/a", which = T]
check22_3 <- PStestdata4[PrevGB != "0", which = T]
firstwordvector <- word(PStestdata4$productName,1)
positionsvector <- intersect(check22_3,intersect(check22_1,check22_2))
PStestdata4[intersect(check22_3,intersect(check22_1,check22_2))]$PrevBrand <- firstwordvector[positionsvector]

check23 <- grep("^black.*decker", x = PStestdata4$productName, value = F, fixed = F)
check24 <- PStestdata4[PrevBrand == "#n/a", which = T]
PStestdata4 <- PStestdata4[intersect(check23, check24), PrevBrand := "black & decker"]

#run for "vic" & "gs" separately
check25 <- PStestdata4[!grep("^vic firth", x = PStestdata4$productName, value = F, fixed = F), which = T]
check26 <- grep("^vic", x = PStestdata4$productName, value = F, fixed = F)
check27 <- PStestdata4[PrevBrand == "#n/a", which = T]
PStestdata4 <- PStestdata4[intersect(check25, intersect(check26, check27)), PrevBrand := "vic"]

#multi brand
grepBrandnames <- c()
compiledContains <- c()
for (i in 1:length(brandlist)){
  assign(paste0("contains",brandlist[i]), grep(brandlist[i],x = PStestdata4$productName,value = F, fixed = F))
  grepBrandnames <- c(grepBrandnames, paste0("contains",brandlist[i]))
  compiledContains <- c(compiledContains, get(paste0("contains",brandlist[i])))
}
multiplebrands <- compiledContains[duplicated(compiledContains)==TRUE]
check28 <- PStestdata4[PrevBrand == "#n/a", which = T]
PStestdata4 <- PStestdata4[intersect(check28, multiplebrands), PrevBrand := "no brand"]


#PREFIXES - Contains [original/genuine/cod BRAND]
brandregex_2 <- paste0("original.+",brandlist_regex[-length(brandlist_regex)],"|")
brandregex_2_1 <- paste0("genuine.+",brandlist_regex,"|")
brandregex_2_2 <- paste0("cod.",brandlist_regex,"|")
brandregex_forBrand2 <- paste(c(brandregex_2_1,brandregex_2_2,brandregex_2,paste0("original.+",brandlist_regex[length(brandlist_regex)])), collapse = "")

matchback_brandregex <- paste(c(paste0(brandlist_regex[-length(brandlist_regex)],"|"),brandlist_regex[length(brandlist_regex)]), collapse = "")

check29 <- PStestdata4[PrevBrand == "#n/a", which = T]
PStestdata4 <- PStestdata4[intersect(check29,grep(pattern = brandregex_forBrand2, x = PStestdata4$productName, value = F, fixed = F)), PrevBrand :=  str_extract(productName, matchback_brandregex)]



#code back brands to Uppercase.
uppercaseBrand <- c("#n/a", "ngk", "dca", "vic", "nwb","ingco","gs")
regularBrand <- c("makita","stanley","sakura","no brand","maktec", "mann", "mann filter", "denso","bosch", "dewalt","black & decker", "yuasa")
PStestdata4 <- PStestdata4[PrevBrand %in% uppercaseBrand ,PrevBrand:= toupper(PrevBrand)]
PStestdata4 <- PStestdata4[PrevBrand %in% regularBrand, PrevBrand := str_to_title(PrevBrand)]


#export
if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)
library(xlsx)


write.xlsx(x = PStestdata4,                       
           file = "CategprisedPH_Shopee.xlsx",      
           sheetName = "Consolidated")

