##### CITYMD VACCINATION DATA ########3
library(dplyr)
library(data.table)
library(readr)
library(tidyr)
library(ggplot2)

library(tidyverse)
library(Hmisc)
library(Gmisc)
library(knitr)

#Pull in data
Visit <- read_delim("/Users/madhurarane/Documents/CityMD/CUNY_Visit.csv","|", escape_double = FALSE, trim_ws = TRUE) #10273173 
COVIDResults <- read_delim("/Users/madhurarane/Documents/CityMD/CUNY_COVIDResults.csv", "|",escape_double = FALSE, 
                           col_types = list(col_character(),col_character(),col_character(),
                                            col_character(),col_character(),col_character(),col_character(),
                                            col_character(),col_double(),col_double()),
                           trim_ws = TRUE) #8109375
ChiefComplaint  <- read_delim("/Users/madhurarane/Documents/CityMD/CUNY_ChiefComplaint.csv","|", escape_double = FALSE, trim_ws = TRUE) #4236024




#Rename vars 
ChiefComplaint <- as.data.table(ChiefComplaint) 

ChiefComplaint %>% 
  rename(Complaint = `Chief Complaint`,
         Vax_date = `Adjusted Visit Date`,
         Vax_rec = `COVID-19 Vaccine?`,
         Vax_manu = `Which vaccine did you receive?`,
         Fully_vax = `> 2 weeks since final dose?`) -> ChiefComplaint

Visit <- as.data.table(Visit)

Visit %>%
  rename(Visit_date = `Adjusted Visit Date`,
         Facility_Address = `Facility Address`,
         Facility_Name = `Facility Name`,
         Facility_City =  `Facility City`,
         Facility_State =  `Facility State`) -> Visit


COVIDResults <- as.data.table(COVIDResults)

COVIDResults %>%
  rename(Test_date = `Adjusted Visit Date`,
         Lab.Result.Interpretation = `Lab Result Interpretation`
  ) -> COVIDResults


COVIDResults$Test_date <- as.Date(COVIDResults$Test_date, format="%m/%d/%Y")
Visit$Visit_date <- as.Date(Visit$Visit_date, format="%m/%d/%Y")
ChiefComplaint$Vax_date <- as.Date(ChiefComplaint$Vax_date, format="%m/%d/%Y")

Visit %>%
  filter(Facility_State=="NY") %>%
  filter(Visit_date >= "2020-03-01") -> Visit #7555936

COVIDResults %>%
  filter(Lab.Result.Interpretation=="POSITIVE" | Lab.Result.Interpretation=="NEGATIVE") %>%
  filter(Test_date >= "2020-03-01") -> COVIDResults     #7988377                        

COVIDResults <- COVIDResults %>%
  group_by(PatientID) %>%
  arrange(Test_date, .by_group=TRUE)%>%
  mutate(Visit = 1:n())
#Some pts got PCR + antigen tests on the same day with same VistID so don't pull out distinct Visit IDs 

#Restrict chief complaint data to vaccine visits only 
Vaccine <- ChiefComplaint %>%
  group_by(VisitID) %>%
  arrange(Vax_date, .by_group=TRUE)%>%
  mutate(Visit = 1:n())

Vaccine %>%
  group_by(Visit) %>%
  summarise(n=n_distinct(VisitID))

#Vaccine visits get included as an extra row under category "DOH Additional HPI required"
Vaccine <-
  ChiefComplaint %>% group_by(VisitID) %>% filter(`System Category`== "DOH (Additional HPI required)") #2068286

#Remove duplicates in Visit data
n_distinct(Visit$VisitID) #some duplicates
Visit <- distinct(Visit) #7555922

#Create separate wide datasets for each variable and combine at the end; unwieldy otherwise
Visit <- Visit %>%
  group_by(PatientID) %>%
  arrange(Visit_date, .by_group=TRUE)%>%
  mutate(Visit = 1:n())

Visit<- as.data.table(Visit)
Visit.w <- dcast(Visit, PatientID  ~ Visit, value.var = "Visit_date")

Visit.w.age <- dcast(Visit, PatientID  ~ Visit, value.var = "PatientAge")
Visit.w.age <- Visit.w.age[,1:2]
Visit.w <- left_join(Visit.w, Visit.w.age, by="PatientID")

Visit.w.gender <- dcast(Visit, PatientID  ~ Visit, value.var = "PatientGender")
Visit.w.gender <- Visit.w.gender[,1:2]
Visit.w <- left_join(Visit.w, Visit.w.gender, by="PatientID")

Visit.w.re <- dcast(Visit, PatientID  ~ Visit, value.var = "Race")
Visit.w.race <- Visit.w.re[,1:2]
Visit.w <- left_join(Visit.w, Visit.w.race, by="PatientID")

Visit.w.ethnicity <-  dcast(Visit, PatientID  ~ Visit, value.var = "Ethnicity")
Visit.w.ethnicity <- Visit.w.ethnicity[,1:2]
Visit.w <- left_join(Visit.w, Visit.w.ethnicity, by="PatientID")

Visit.w.PIG <-  dcast(Visit, PatientID  ~ Visit, value.var = "PrimaryInsuranceGroup")
Visit.w.PIG <- Visit.w.PIG[,1:6]
Visit.w.PIG <- Visit.w.PIG %>%
  rename(PI1 = 2)
Visit.w.PIG <- Visit.w.PIG %>%
  rename(PI2 = 3)
Visit.w.PIG <- Visit.w.PIG %>%
  rename(PI3 = 4)
Visit.w.PIG <- Visit.w.PIG %>%
  rename(PI4 = 5)
Visit.w.PIG <- Visit.w.PIG %>%
  rename(PI5 = 6)
Visit.w <- left_join(Visit.w, Visit.w.PIG, by="PatientID")

Visit.w.UHF <- dcast(Visit, PatientID  ~ Visit, value.var = "UHF_Neighborhood")
Visit.w.UHF <- Visit.w.UHF[,1:2]

Visit.w <- left_join(Visit.w, Visit.w.UHF, by="PatientID")

Visit.w.facility <- dcast(Visit, PatientID  ~ Visit, value.var = "Facility_City")
Visit.w.facility <- Visit.w.facility[,1:6]
Visit.w.facility <- Visit.w.facility %>%
  rename(Fac1 = 2)
Visit.w.facility <- Visit.w.facility %>%
  rename(Fac2 = 3)
Visit.w.facility <- Visit.w.facility %>%
  rename(Fac3 = 4)
Visit.w.facility <- Visit.w.facility %>%
  rename(Fac4 = 5)
Visit.w.facility <- Visit.w.facility %>%
  rename(Fac5 = 6)
Visit.w <- left_join(Visit.w, Visit.w.facility, by="PatientID")

Visit.w.region <- dcast(Visit, PatientID  ~ Visit, value.var = "Geograpic Region")
Visit.w.region <- Visit.w.region[,1:2]
Visit.w <- left_join(Visit.w, Visit.w.region, by="PatientID")

names(Visit.w)
#rename vars
Visit.w <- setnames(Visit.w, old = c("1.x","1.y","1.x.x","1.y.y","1.x.x.x","1.y.y.y",1), new=c("1","Age","Gender","Race","Ethnicity","UHF","Region"))

#select out demographic vars 
Visit.demo <- Visit.w[, c(1,106:121)]

#Remove extra datasets
rm(Visit.w, Visit.w.age, Visit.w.ethnicity, Visit.w.facility, Visit.w.gender, Visit.w.PIG, Visit.w.race, Visit.w.re, Visit.w.region, Visit.w.UHF)

Visit.demo$racecat <- NA
Visit.demo$racecat[Visit.demo$Race %in% c("Abenaki","Absentee Shawnee", "Apache", "Arapaho", "Caddo","Acoma", 
                                          "Alamo Navajo", "Canoncito Navajo", "Agdaagux", "Agua Caliente", "Agua Caliente Cahuilla", 
                                          "Augustine", "Bishop", "Bridgeport", "Cabazon", "Cahto", "Cahuilla", 
                                          "California Tribes", "Campo", "Capitan Grande", "Ak-Chin", "Arizona Tewa", 
                                          "Barrio Libre", "Birch Creek", "Brevig Mission", "Ak-Chin", "Arizona Tewa", "Barrio Libre", 
                                          "Birch Creek", "Brevig Mission", "Alabama Coushatta", "Alabama Creek", "Alabama Quassarte",
                                          "Allen Canyon", "Alsea", "Arikara", "Aroostook", "Assiniboine", "Assiniboine Sioux", "Atsina", 
                                          "Blackfoot Sioux", "Attacapa", "Bad River", "Brotherton", "Bannock", "Battle Mountain", 
                                          "Carson", "Bay Mills Chippewa", "Burt Lake Band", "Burt Lake Chippewa", "Burt Lake Ottawa",
                                          "Big Cypress", "Brighton", "Biloxi", "Blackfeet", "Bois Forte", "Brule Sioux", "Burns Paiute",
                                          "Catawba", "Cayuga", "Cayuse", "Cedarville", "Celilo", "Central Pomo", "Chehalis", "Chemakuan",
                                          "Chemehuevi", "Cherokee", "Cherokee Alabama", "Cherokee Shawnee", "Cherokees of Northeast Alabama",
                                          "Cherokees of Southeast Alabama", "Cheyenne", "Cheyenne River Sioux", "Cheyenne-Arapaho",
                                          "Chickahominy", "Chickasaw", "Chimariko", "Chinook", "Chippewa", "Chippewa Cree", 
                                          "Chiricahua", "Chitimacha", "Choctaw", "Chukchansi", "Chumash", "Citizen Band Potawatomi",
                                          "Clatsop", "Clear Lake", "Clifton Choctaw", "Coast Miwok", "Coast Yurok", "Cochiti", "Cocopah",
                                          "Coeur D'Alene", "Coharie", "Colorado River", "Columbia River Chinook", "Colville",
                                          "Comanche", "Coos", "Coos; Lower Umpqua; Siuslaw", "Coquilles", "Costanoan", "Coushatta",
                                          "Cow Creek Umpqua", "Cowlitz", "Craig", "Cree", "Creek", "Croatan", "Crow", "Crow Creek Sioux",
                                          "Cupeno", "Cuyapaipe", "Dakota Sioux", "Delaware", "Diegueno", "Digger", "Dresslerville",
                                          "Dry Creek", "Duck Valley", "Duckwater", "Duwamish", "Eastern Cherokee", "Eastern Chickahominy",
                                          "Eastern Creek", "Eastern Delaware", "Eastern Muscogee", "Eastern Pomo", "Eastern Shawnee",
                                          "Echota Cherokee", "Elko", "Ely", "Esselen", "Etowah Cherokee", "Fallon", "Flandreau Santee",
                                          "Florida Seminole", "Fond du Lac", "Forest County", "Fort Belknap", "Fort Berthold", "Fort Bidwell",
                                          "Fort Hall", "Fort Independence", "Fort McDermitt", "Fort Mcdowell", "Fort Peck", 
                                          "Fort Peck Assiniboine Sioux", "Fort Sill Apache", "French American Indian", "Gabrieleno",
                                          "Gay Head Wampanoag", "Georgetown (Eastern Tribes)", "Gila Bend", "Gila River Pima-Maricopa",
                                          "Goshute", "Grand Portage", "Grand Ronde", "Grand Traverse Band of Ottawa/Chippewa",
                                          "Gros Ventres", "Haliwa", "Hannahville", "Havasupai", "Hidatsa", "Ho-chunk", "Hoh", "Hollywood Seminole",
                                          "Hoopa", "Hoopa Extension", "Hopi", "Houma", "Hualapai", "Huron Potawatomi", "Illinois Miami",
                                          "Inaja-Cosmit", "Indian Township", "Indiana Miami", "Iowa", "Iowa of Kansas-Nebraska", 
                                          "Iowa of Oklahoma", "Iowa Sac and Fox", "Iroquois", "Isleta", "Jamestown", "Jemez",
                                          "Jena Choctaw", "Jicarilla Apache", "Juaneno", "Kaibab", "Kalapuya", "Kalispel", 
                                          "Karuk", "Kashia", "Kathlamet", "Kaw", "Kawaiisu", "Keres", "Kern River", "Keweenaw",
                                          "Kialegee", "Kickapoo", "Kikiallus", "Kiowa", "Klallam", "Klamath", "Konkow", "Kootenai",
                                          "La Jolla", "La Posta", "Lac Courte Oreilles", "Lac du Flambeau", "Lac Vieux Desert Chippewa",
                                          "Laguna", "Lake Superior", "Lake Traverse Sioux", "Las Vegas", "Lassik", "Leech Lake", 
                                          "Lenni-Lenape", "Lipan Apache", "Little Shell Chippewa", "Lone Pine", "Long Island", "Los Coyotes",
                                          "Lovelock", "Lower Brule Sioux", "Lower Elwha", "Lower Muscogee", "Lower Sioux", "Lower Skagit", 
                                          "Luiseno", "Lumbee", "Lummi", "Machis Lower Creek Indian", "Maidu", "Makah", "Malheur Paiute",
                                          "Maliseet", "Mandan", "Manzanita", "Maricopa", "Marshantucket Pequot", "Mashpee Wampanoag",
                                          "Matinecock", "Mattaponi", "Mattole", "Mdewakanton Sioux", "Menominee", "Mesa Grande", 
                                          "Mescalero Apache", "Miami", "Miccosukee", "Michigan Ottawa", "Algonquian", "Beaver", 
                                          "Canadian Indian", "Greenland Eskimo", "Haida", "Micmac", "Mille Lacs", "Miniconjou",
                                          "Minnesota Chippewa", "Mission Indians", "Mississippi Choctaw", "Missouri Sac and Fox", 
                                          "Miwok", "Modoc", "Mohave", "Mohawk", "Mohegan", "Molala", "Mono", "Montauk", "Morongo",
                                          "Mountain Maidu", "Mowa Band of Choctaw", "Muckleshoot", "Munsee", "Nambe", "Narragansett",
                                          "Natchez", "Nausu Waiwash", "Navajo", "Nebraska Ponca", "Nebraska Winnebago", "Nez Perce", 
                                          "Nipmuc", "Nishinam", "Nisqually", "Nomalaki", "Nooksack", "Northern Arapaho", "Northern Cherokee",
                                          "Northern Cheyenne", "Northern Paiute", "Northern Pomo", "Northwest Tribes", "Oglala Sioux",
                                          "Oklahoma Apache", "Oklahoma Cado", "Oklahoma Choctaw", "Oklahoma Comanche", "Oklahoma Kickapoo",
                                          "Oklahoma Kiowa", "Oklahoma Miami", "Oklahoma Ottawa", "Oklahoma Pawnee", "Oklahoma Peoria",
                                          "Oklahoma Ponca", "Oklahoma Sac and Fox", "Oklahoma Seminole", "Omaha", "Oneida", "Onondaga",
                                          "Ontonagon", "Oregon Athabaskan", "Osage", "Otoe-Missouria", "Ottawa", "Owens Valley", "Paiute", 
                                          "Pala", "Palauan", "Pamunkey", "Panamint", "Pascua Yaqui", "Passamaquoddy", "Paugussett", "Pauma", 
                                          "Pawnee", "Payson Apache", "Pawnee", "Payson Apache", "Pechanga", "Pelican", "Penobscot", "Peoria",
                                          "Pequot", "Perryville", "Picuris", "Pima", "Pine Ridge Sioux", "Pipestone Sioux", "Piro", 
                                          "Piscataway", "Pit River", "Pleasant Point Passamaquoddy", "Poarch Band", "Pocomoke Acohonock", 
                                          "Pojoaque", "Pokagon Potawatomi", "Pomo", "Ponca", "Poospatuck", "Port Madison", "Potawatomi", 
                                          "Powhatan", "Prairie Band", "Prairie Island Sioux", "Principal Creek Indian Nation", "Prior Lake Sioux",
                                          "Pueblo", "Puget Sound Salish", "Puyallup", "Pyramid Lake", "Quapaw", "Quechan", "Quileute", 
                                          "Quinault", "Ramah Navajo", "Rampough Mountain", "Red Cliff Chippewa", "Red Lake Chippewa", 
                                          "Red Wood", "Reno-Sparks", "Rocky Boy's Chippewa Cree", "Rosebud Sioux", "Round Valley",
                                          "Sac and Fox", "Saginaw Chippewa", "Salinan", "Salish", "Salish and Kootenai", "Salt River Pima-Maricopa",
                                          "Samish", "San Carlos Apache", "San Felipe", "San Ildefonso", "San Juan Pueblo", "San Juan Southern Paiute",
                                          "San Manual", "San Pasqual", "Sand Hill", "Sand Point", "Sandia", "Santa Ana", "Santa Clara",
                                          "Santa Rosa", "Santa Rosa Cahuilla", "Santa Ynez", "Santa Ysabel", "Santee Sioux", "Sauk-Suiattle",
                                          "Sault Ste. Marie Chippewa", "Schaghticoke", "Scotts Valley", "Seminole", "Seneca", "Seneca Nation",
                                          "Serrano", "Setauket", "Shasta", "Shawnee", "Shinnecock", "Shoshone", "Shoshone Paiute", "Sioux", 
                                          "Sisseton-Wahpeton", "Skokomish", "Skull Valley", "Snohomish", "Soboba", "Sokoagon Chippewa",
                                          "South Fork Shoshone", "Southeastern Indians", "Southern Arapaho", "Southern Cheyenne",
                                          "Southern Paiute", "Spirit Lake Sioux", "Spokane", "Squaxin Island", "St. Croix Chippewa",
                                          "Standing Rock Sioux", "Star Clan of Muscogee Creeks", "Steilacoom", "Stillaguamish",
                                          "Stockbridge", "Sulphur Bank", "Summit Lake", "Suquamish", "Susanville", "Susquehanock",
                                          "Sycuan", "Table Bluff", "Tachi", "Takelma", "Taos", "Te-Moak Western Shoshone", "Temecula",
                                          "Tenino", "Tesuque", "Teton Sioux", "Tewa", "Texas Kickapoo", "Thlopthlocco", "Tigua", 
                                          "Timbi-Sha Shoshone", "Tohono O'Odham", "Tolowa", "Tonawanda Seneca", "Torres-Martinez",
                                          "Tsimshian", "Tuckabachee", "Tulalip", "Tule River", "Tunica Biloxi", "Turtle Mountain",
                                          "Tuscarora", "Tuscola", "Twenty-Nine Palms", "Two Kettle Sioux", "Tygh", "Uintah Ute", 
                                          "Umatilla", "Umpqua", "United Keetowah Band of Cherokee, Upper Chinook", "Upper Sioux",
                                          "Upper Skagit", "Ute", "Ute Mountain Ute", "Utu Utu Gwaitu Paiute", "Waccamaw-Siousan", 
                                          "Wahpekute Sioux", "Wahpeton Sioux", "Wailaki", "Wakiakum Chinook", "Walker River", 
                                          "Walla-Walla", "Wampanoag", "Wappo", "Warm Springs", "Wascopum", "Washakie", "Washoe",
                                          "Wazhaza Sioux", "Wenatchee", "Western Cherokee", "Western Chickahominy", "Whilkut", "White Earth",
                                          "White Mountain", "White Mountain Apache", "White Mountain Inupiat", "Wichita", "Wicomico",
                                          "Willapa Chinook", "Wind River", "Wind River Arapaho", "Wind River Shoshone", "Winnebago",
                                          "Winnemucca", "Wintun", "Wisconsin Potawatomi", "Wishram", "Wiyot", "Wyandotte", "Yahooskin",
                                          "Yakama", "Yakama Cowlitz", "Yana", "Yankton Sioux", "Yanktonai Sioux", "Yaqui", "Yavapai",
                                          "Yavapai Apache", "Yerington Paiute", "Yokuts", "Yomba", "Yuchi", "Yuki", "Yuman", "Yurok",
                                          "Zia", "Zuni", "Eastern Tribes","Ahtna", "Akhiok", "Akiachak", "Akiak", "Akutan", "Alakanuk", "Alanvik", "Alaska Indian", 
                                          "Alaska Native", "Alaskan Athabascan", "Alatna", "Aleknagik", "Aleut", "Aleut Corporation",
                                          "Aleutian", "Aleutian Islander", "Alexander", "Allakaket", "Alutiiq Aleut", "Ambler", 
                                          "Anaktuvuk", "Anaktuvuk Pass", "Andreafsky", "Angoon", "Aniak", "Anvik", "Arctic", 
                                          "Arctic Slope Corporation", "Arctic Slope Inupiat", "Atka", "Atmautluak", "Atqasuk",
                                          "Barrow", "Belkofski", "Bering Straits Inupiat", "Bethel", "Bill Moore's Slough", "Bristol Bay Aleut", 
                                          "Bristol Bay Yupik", "Buckland", "Calista Yupik", "Cantwell", "Central Council of Tlingit and Haida Tribes",
                                          "Chefornak", "Chalkyitsik","Chenega", "Chevak", "Chickaloon", "Chignik", "Chignik Lagoon",
                                          "Chignik Lagoon", "Chignik Lake", "Chilkat", "Chilkoot", "Chinik", "Chistochina", "Chitina",
                                          "Chuathbaluk", "Chugach Aleut", "Chugach Corporation", "Clark's Point", "Cook Inlet", 
                                          "Copper Center", "Copper River", "Crooked Creek", "Deering", "Dillingham", "Dot Lake",
                                          "Doyon", "Eek", "Egegik", "Eklutna", "Ekuk", "Ekwok", "Elim", "Emmonak", "English Bay",
                                          "Eskimo", "Evansville", "Eyak", "False Pass", "Fort Yukon", "Gakona", "Galena", "Gambell", 
                                          "Georgetown (Yupik-Eskimo)", "Golovin", "Goodnews Bay", "Grayling", "Gulkana", "Healy Lake",
                                          "Holy Cross", "Hoonah", "Hooper Bay", "Hughes", "Huslia", "Hydaburg", "Igiugig", "Iliamna",
                                          "Inalik Diomede", "Inupiaq", "Inupiat Eskimo", "Iqurmuit (Russian Mission)", "Ivanof Bay",
                                          "Kake", "Kalskag", "Kaltag", "Kasaan", "Kasigluk", "Kawerak", "Kenaitze", "Ketchikan", "Kiana",
                                          "King Cove", "King Salmon", "Kipnuk", "Kivalina", "Klawock", "Knik", "Kobuk", 
                                          "Kodiak", "Kokhanok", "Koliganek", "Kongiganak", "Koniag Aleut", "Kotlik", "Kotzebue",
                                          "Koyuk", "Koyukuk", "Kwethluk", "Kwigillingok", "Kwiguk", "Lake Minchumina", "Larsen Bay",
                                          "Levelock", "Manley Hot Springs", "Manokotak", "Mary's Igloo", "Mauneluk Inupiat", "Mekoryuk", 
                                          "Mentasta Lake", "Metlakatla", "Minto", "Mountain Village", "Nana Inupiat", "Napakiak", 
                                          "Napaskiak", "Napaumute", "Nelson Lagoon", "Nenana", "New Stuyahok", "Newhalen", "Newtok", "Nikolai",
                                          "Ninilchik", "Noatak", "Nome", "Nondalton", "Noorvik", "Northway", "Nulato", "Nunapitchukv", 
                                          "Old Harbor", "Oscarville", "Ouzinkie", "Pauloff Harbor", "Pedro Bay", "Petersburg", "Pilot Point",
                                          "Pitkas Point", "Point Hope", "Point Lay", "Port Graham", "Port Heiden", "Port Lions", "Portage Creek",
                                          "Qagan Toyagungin", "Qawalangin", "Quinhagak", "Rampart", "Ruby", "Ruby Valley", "Salamatof", "Savoonga",
                                          "Saxman", "Scammon Bay", "Selawik", "Seldovia", "Shageluk", "Shaktoolik", "Sheldon's Point", "Shishmaref",
                                          "Shungnak", "Siberian Eskimo", "Siberian Yupik", "Sitka", "Slana", "Sleetmute", "South Naknek", 
                                          "Southeast Alaska", "St. George", "St. Mary's", "St. Michael", "St. Paul", "Stebbins", "Stevens",
                                          "Stony River", "Sugpiaq", "Tanaina", "Tanana", "Tanana Chiefs", "Tazlina", "Telida", "Teller",
                                          "Tenakee Springs", "Tlingit", "Tlingit-Haida", "Tok", "Toksook", "Tulukskak", "Tuntutuliak", "Tununak",
                                          "Twin Hills", "Tyonek", "Ugashik", "Umkumiate", "Unalakleet", "Unalaska", "Unangan Aleut", "Unga",
                                          "Venetie", "Wainwright", "Wrangell", "Yakutat", "Yupik Eskimo","Central American Indian", "Mexican American Indian",
                                          "South American Indian","American Indian", "American Indian or Alaska Native", "Canadian and Latin American Indian", 
                                          "Chamorro", "Chuukese", "Fijian", "Guamanian", "Kiribati", "Kosraean", "Mariana Islander",
                                          "Marshall", "Marshallese", "Melanesian", "Micronesian", "Native Hawaiian", 
                                          "Native Hawaiian or Other Pacific Islander", "New Hebrides", "Other Pacific Islander",
                                          "Papua New Guinean", "Pohnpeian", "Polynesian", "Saipanese", "Samoan", "Solomon", "Solomon Islander",
                                          "Tahitian", "Tokelauan", "Tongan", "Yapese", "Guamanian or Chamorro", "Spanish American Indian",
                                          "United Keetowah Band of Cherokee","Red Devil","Upper Chinook", "Kluti Kaah"," Lower Kalskag", "Nanticoke",
                                          "Nightmute","Nuiqsut"," Port Gamble Klallam","San Xavier","Scott Valley","Seneca-Cayuga","Siuslaw","Talakamish",
                                          "Tanacross","Togiak", "Lower Kalskag", "Port Gamble Klallam", "Tetlin")] <- "Native American/Alaskan Native/Pacific Islander"

Visit.demo$racecat[Visit.demo$Race %in% c("Asian","Bangladeshi", "Bhutanese", "Asian Indian", "Maldivian", "Nepalese", "Pakistani",
                                          "Sri Lankan","Burmese", "Cambodian", "Indonesian", "Hmong", "Laotian", "Malaysian", "Singaporean",
                                          "Thailand", "Vietnamese","Chinese", "Iwo Jiman", "Japanese", "Korean", "Okinawan", "Taiwanese","Thai")] <- "Asian"                     

Visit.demo$racecat[Visit.demo$Race %in% c("African", "Botswanan", "Ethiopian", "Liberian", "Madagascar", "Namibian", "Nigerian",
                                          "Zairean","African American","Bahamian", "Barbadian", "Douglas", "Haitian", "Jamaican", "Tobagoan", "Trinidadian",
                                          "West Indian","Black", "Black or African American")] <- "Black/AfrAm"

Visit.demo$racecat[Visit.demo$Race %in% c("Alpine", "English", "European", "French", "German", "Irish", "Italian", "Moor",
                                          "Polish", "Scottish", "Wales","Iranian", "Iraqi", "Armenian", "Arab", "Assyrian", "Afghanistani", 
                                          "Israeili", "Karluk", "Lebanese", "Egyptian", "Middle Eastern or North African", 
                                          "Palestinian", "Syrian","White")] <-"White"


Visit.demo$racecat[Visit.demo$Race %in% c("Columbia","Dominica Islander", "Dominican", "Santo Domingo","Filipino","San Juan","Hispanic", "San Juan De")] <-"Hispanic"

Visit.demo$racecat[Visit.demo$Race %in% c("Declined to Report", "Declined to Specify", "Unreported/Refuse to Report", "Unreported/Refused to Report",
                                          "Unreported/Refused To Report","Other Race","Carolinian", "Circle", "Council", "Eagle", "Lime", "Mcgrath", "Platinum", "Stewart",
                                          "Trinity", "Wiseman","Oklahoma Delaware","Siletz","Stonyford","", "Suqpigaq", "Unreported/Refuse To Report")] <- "Other/Unknown"
# If ethnicity is Hispanic, change race to Hispanic
Visit.demo$Ethnicity[Visit.demo$Ethnicity == "Unreported/Refused to Report"] <- NA 
Visit.demo$Ethnicity[Visit.demo$Ethnicity == ""] <- NA 

Visit.demo$racecat[!is.na(Visit.demo$Ethnicity) & Visit.demo$Ethnicity != "Not Hispanic or Latino"] <- "Hispanic"
summary(as.factor(Visit.demo$racecat))
Visit.demo$racecat[is.na(Visit.demo$racecat)] <- "Other/Unknown"
summary(as.factor(Visit.demo$racecat))

####### Updating race variable for citymd data based on Saba's code####
updatedrace_orig_race_Oct$racecat <- NA
updatedrace_orig_race_Oct$racecat[updatedrace_orig_race_Oct$Race %in% c("Abenaki","Absentee Shawnee", "Apache", "Arapaho", "Caddo","Acoma", 
                                                                "Alamo Navajo", "Canoncito Navajo", "Agdaagux", "Agua Caliente", "Agua Caliente Cahuilla", 
                                                                "Augustine", "Bishop", "Bridgeport", "Cabazon", "Cahto", "Cahuilla", 
                                                                "California Tribes", "Campo", "Capitan Grande", "Ak-Chin", "Arizona Tewa", 
                                                                "Barrio Libre", "Birch Creek", "Brevig Mission", "Ak-Chin", "Arizona Tewa", "Barrio Libre", 
                                                                "Birch Creek", "Brevig Mission", "Alabama Coushatta", "Alabama Creek", "Alabama Quassarte",
                                                                "Allen Canyon", "Alsea", "Arikara", "Aroostook", "Assiniboine", "Assiniboine Sioux", "Atsina", 
                                                                "Blackfoot Sioux", "Attacapa", "Bad River", "Brotherton", "Bannock", "Battle Mountain", 
                                                                "Carson", "Bay Mills Chippewa", "Burt Lake Band", "Burt Lake Chippewa", "Burt Lake Ottawa",
                                                                "Big Cypress", "Brighton", "Biloxi", "Blackfeet", "Bois Forte", "Brule Sioux", "Burns Paiute",
                                                                "Catawba", "Cayuga", "Cayuse", "Cedarville", "Celilo", "Central Pomo", "Chehalis", "Chemakuan",
                                                                "Chemehuevi", "Cherokee", "Cherokee Alabama", "Cherokee Shawnee", "Cherokees of Northeast Alabama",
                                                                "Cherokees of Southeast Alabama", "Cheyenne", "Cheyenne River Sioux", "Cheyenne-Arapaho",
                                                                "Chickahominy", "Chickasaw", "Chimariko", "Chinook", "Chippewa", "Chippewa Cree", 
                                                                "Chiricahua", "Chitimacha", "Choctaw", "Chukchansi", "Chumash", "Citizen Band Potawatomi",
                                                                "Clatsop", "Clear Lake", "Clifton Choctaw", "Coast Miwok", "Coast Yurok", "Cochiti", "Cocopah",
                                                                "Coeur D'Alene", "Coharie", "Colorado River", "Columbia River Chinook", "Colville",
                                                                "Comanche", "Coos", "Coos; Lower Umpqua; Siuslaw", "Coquilles", "Costanoan", "Coushatta",
                                                                "Cow Creek Umpqua", "Cowlitz", "Craig", "Cree", "Creek", "Croatan", "Crow", "Crow Creek Sioux",
                                                                "Cupeno", "Cuyapaipe", "Dakota Sioux", "Delaware", "Diegueno", "Digger", "Dresslerville",
                                                                "Dry Creek", "Duck Valley", "Duckwater", "Duwamish", "Eastern Cherokee", "Eastern Chickahominy",
                                                                "Eastern Creek", "Eastern Delaware", "Eastern Muscogee", "Eastern Pomo", "Eastern Shawnee",
                                                                "Echota Cherokee", "Elko", "Ely", "Esselen", "Etowah Cherokee", "Fallon", "Flandreau Santee",
                                                                "Florida Seminole", "Fond du Lac", "Forest County", "Fort Belknap", "Fort Berthold", "Fort Bidwell",
                                                                "Fort Hall", "Fort Independence", "Fort McDermitt", "Fort Mcdowell", "Fort Peck", 
                                                                "Fort Peck Assiniboine Sioux", "Fort Sill Apache", "French American Indian", "Gabrieleno",
                                                                "Gay Head Wampanoag", "Georgetown (Eastern Tribes)", "Gila Bend", "Gila River Pima-Maricopa",
                                                                "Goshute", "Grand Portage", "Grand Ronde", "Grand Traverse Band of Ottawa/Chippewa",
                                                                "Gros Ventres", "Haliwa", "Hannahville", "Havasupai", "Hidatsa", "Ho-chunk", "Hoh", "Hollywood Seminole",
                                                                "Hoopa", "Hoopa Extension", "Hopi", "Houma", "Hualapai", "Huron Potawatomi", "Illinois Miami",
                                                                "Inaja-Cosmit", "Indian Township", "Indiana Miami", "Iowa", "Iowa of Kansas-Nebraska", 
                                                                "Iowa of Oklahoma", "Iowa Sac and Fox", "Iroquois", "Isleta", "Jamestown", "Jemez",
                                                                "Jena Choctaw", "Jicarilla Apache", "Juaneno", "Kaibab", "Kalapuya", "Kalispel", 
                                                                "Karuk", "Kashia", "Kathlamet", "Kaw", "Kawaiisu", "Keres", "Kern River", "Keweenaw",
                                                                "Kialegee", "Kickapoo", "Kikiallus", "Kiowa", "Klallam", "Klamath", "Konkow", "Kootenai",
                                                                "La Jolla", "La Posta", "Lac Courte Oreilles", "Lac du Flambeau", "Lac Vieux Desert Chippewa",
                                                                "Laguna", "Lake Superior", "Lake Traverse Sioux", "Las Vegas", "Lassik", "Leech Lake", 
                                                                "Lenni-Lenape", "Lipan Apache", "Little Shell Chippewa", "Lone Pine", "Long Island", "Los Coyotes",
                                                                "Lovelock", "Lower Brule Sioux", "Lower Elwha", "Lower Muscogee", "Lower Sioux", "Lower Skagit", 
                                                                "Luiseno", "Lumbee", "Lummi", "Machis Lower Creek Indian", "Maidu", "Makah", "Malheur Paiute",
                                                                "Maliseet", "Mandan", "Manzanita", "Maricopa", "Marshantucket Pequot", "Mashpee Wampanoag",
                                                                "Matinecock", "Mattaponi", "Mattole", "Mdewakanton Sioux", "Menominee", "Mesa Grande", 
                                                                "Mescalero Apache", "Miami", "Miccosukee", "Michigan Ottawa", "Algonquian", "Beaver", 
                                                                "Canadian Indian", "Greenland Eskimo", "Haida", "Micmac", "Mille Lacs", "Miniconjou",
                                                                "Minnesota Chippewa", "Mission Indians", "Mississippi Choctaw", "Missouri Sac and Fox", 
                                                                "Miwok", "Modoc", "Mohave", "Mohawk", "Mohegan", "Molala", "Mono", "Montauk", "Morongo",
                                                                "Mountain Maidu", "Mowa Band of Choctaw", "Muckleshoot", "Munsee", "Nambe", "Narragansett",
                                                                "Natchez", "Nausu Waiwash", "Navajo", "Nebraska Ponca", "Nebraska Winnebago", "Nez Perce", 
                                                                "Nipmuc", "Nishinam", "Nisqually", "Nomalaki", "Nooksack", "Northern Arapaho", "Northern Cherokee",
                                                                "Northern Cheyenne", "Northern Paiute", "Northern Pomo", "Northwest Tribes", "Oglala Sioux",
                                                                "Oklahoma Apache", "Oklahoma Cado", "Oklahoma Choctaw", "Oklahoma Comanche", "Oklahoma Kickapoo",
                                                                "Oklahoma Kiowa", "Oklahoma Miami", "Oklahoma Ottawa", "Oklahoma Pawnee", "Oklahoma Peoria",
                                                                "Oklahoma Ponca", "Oklahoma Sac and Fox", "Oklahoma Seminole", "Omaha", "Oneida", "Onondaga",
                                                                "Ontonagon", "Oregon Athabaskan", "Osage", "Otoe-Missouria", "Ottawa", "Owens Valley", "Paiute", 
                                                                "Pala", "Palauan", "Pamunkey", "Panamint", "Pascua Yaqui", "Passamaquoddy", "Paugussett", "Pauma", 
                                                                "Pawnee", "Payson Apache", "Pawnee", "Payson Apache", "Pechanga", "Pelican", "Penobscot", "Peoria",
                                                                "Pequot", "Perryville", "Picuris", "Pima", "Pine Ridge Sioux", "Pipestone Sioux", "Piro", 
                                                                "Piscataway", "Pit River", "Pleasant Point Passamaquoddy", "Poarch Band", "Pocomoke Acohonock", 
                                                                "Pojoaque", "Pokagon Potawatomi", "Pomo", "Ponca", "Poospatuck", "Port Madison", "Potawatomi", 
                                                                "Powhatan", "Prairie Band", "Prairie Island Sioux", "Principal Creek Indian Nation", "Prior Lake Sioux",
                                                                "Pueblo", "Puget Sound Salish", "Puyallup", "Pyramid Lake", "Quapaw", "Quechan", "Quileute", 
                                                                "Quinault", "Ramah Navajo", "Rampough Mountain", "Red Cliff Chippewa", "Red Lake Chippewa", 
                                                                "Red Wood", "Reno-Sparks", "Rocky Boy's Chippewa Cree", "Rosebud Sioux", "Round Valley",
                                                                "Sac and Fox", "Saginaw Chippewa", "Salinan", "Salish", "Salish and Kootenai", "Salt River Pima-Maricopa",
                                                                "Samish", "San Carlos Apache", "San Felipe", "San Ildefonso", "San Juan Pueblo", "San Juan Southern Paiute",
                                                                "San Manual", "San Pasqual", "Sand Hill", "Sand Point", "Sandia", "Santa Ana", "Santa Clara",
                                                                "Santa Rosa", "Santa Rosa Cahuilla", "Santa Ynez", "Santa Ysabel", "Santee Sioux", "Sauk-Suiattle",
                                                                "Sault Ste. Marie Chippewa", "Schaghticoke", "Scotts Valley", "Seminole", "Seneca", "Seneca Nation",
                                                                "Serrano", "Setauket", "Shasta", "Shawnee", "Shinnecock", "Shoshone", "Shoshone Paiute", "Sioux", 
                                                                "Sisseton-Wahpeton", "Skokomish", "Skull Valley", "Snohomish", "Soboba", "Sokoagon Chippewa",
                                                                "South Fork Shoshone", "Southeastern Indians", "Southern Arapaho", "Southern Cheyenne",
                                                                "Southern Paiute", "Spirit Lake Sioux", "Spokane", "Squaxin Island", "St. Croix Chippewa",
                                                                "Standing Rock Sioux", "Star Clan of Muscogee Creeks", "Steilacoom", "Stillaguamish",
                                                                "Stockbridge", "Sulphur Bank", "Summit Lake", "Suquamish", "Susanville", "Susquehanock",
                                                                "Sycuan", "Table Bluff", "Tachi", "Takelma", "Taos", "Te-Moak Western Shoshone", "Temecula",
                                                                "Tenino", "Tesuque", "Teton Sioux", "Tewa", "Texas Kickapoo", "Thlopthlocco", "Tigua", 
                                                                "Timbi-Sha Shoshone", "Tohono O'Odham", "Tolowa", "Tonawanda Seneca", "Torres-Martinez",
                                                                "Tsimshian", "Tuckabachee", "Tulalip", "Tule River", "Tunica Biloxi", "Turtle Mountain",
                                                                "Tuscarora", "Tuscola", "Twenty-Nine Palms", "Two Kettle Sioux", "Tygh", "Uintah Ute", 
                                                                "Umatilla", "Umpqua", "United Keetowah Band of Cherokee, Upper Chinook", "Upper Sioux",
                                                                "Upper Skagit", "Ute", "Ute Mountain Ute", "Utu Utu Gwaitu Paiute", "Waccamaw-Siousan", 
                                                                "Wahpekute Sioux", "Wahpeton Sioux", "Wailaki", "Wakiakum Chinook", "Walker River", 
                                                                "Walla-Walla", "Wampanoag", "Wappo", "Warm Springs", "Wascopum", "Washakie", "Washoe",
                                                                "Wazhaza Sioux", "Wenatchee", "Western Cherokee", "Western Chickahominy", "Whilkut", "White Earth",
                                                                "White Mountain", "White Mountain Apache", "White Mountain Inupiat", "Wichita", "Wicomico",
                                                                "Willapa Chinook", "Wind River", "Wind River Arapaho", "Wind River Shoshone", "Winnebago",
                                                                "Winnemucca", "Wintun", "Wisconsin Potawatomi", "Wishram", "Wiyot", "Wyandotte", "Yahooskin",
                                                                "Yakama", "Yakama Cowlitz", "Yana", "Yankton Sioux", "Yanktonai Sioux", "Yaqui", "Yavapai",
                                                                "Yavapai Apache", "Yerington Paiute", "Yokuts", "Yomba", "Yuchi", "Yuki", "Yuman", "Yurok",
                                                                "Zia", "Zuni", "Eastern Tribes","Ahtna", "Akhiok", "Akiachak", "Akiak", "Akutan", "Alakanuk", "Alanvik", "Alaska Indian", 
                                                                "Alaska Native", "Alaskan Athabascan", "Alatna", "Aleknagik", "Aleut", "Aleut Corporation",
                                                                "Aleutian", "Aleutian Islander", "Alexander", "Allakaket", "Alutiiq Aleut", "Ambler", 
                                                                "Anaktuvuk", "Anaktuvuk Pass", "Andreafsky", "Angoon", "Aniak", "Anvik", "Arctic", 
                                                                "Arctic Slope Corporation", "Arctic Slope Inupiat", "Atka", "Atmautluak", "Atqasuk",
                                                                "Barrow", "Belkofski", "Bering Straits Inupiat", "Bethel", "Bill Moore's Slough", "Bristol Bay Aleut", 
                                                                "Bristol Bay Yupik", "Buckland", "Calista Yupik", "Cantwell", "Central Council of Tlingit and Haida Tribes",
                                                                "Chefornak", "Chalkyitsik","Chenega", "Chevak", "Chickaloon", "Chignik", "Chignik Lagoon",
                                                                "Chignik Lagoon", "Chignik Lake", "Chilkat", "Chilkoot", "Chinik", "Chistochina", "Chitina",
                                                                "Chuathbaluk", "Chugach Aleut", "Chugach Corporation", "Clark's Point", "Cook Inlet", 
                                                                "Copper Center", "Copper River", "Crooked Creek", "Deering", "Dillingham", "Dot Lake",
                                                                "Doyon", "Eek", "Egegik", "Eklutna", "Ekuk", "Ekwok", "Elim", "Emmonak", "English Bay",
                                                                "Eskimo", "Evansville", "Eyak", "False Pass", "Fort Yukon", "Gakona", "Galena", "Gambell", 
                                                                "Georgetown (Yupik-Eskimo)", "Golovin", "Goodnews Bay", "Grayling", "Gulkana", "Healy Lake",
                                                                "Holy Cross", "Hoonah", "Hooper Bay", "Hughes", "Huslia", "Hydaburg", "Igiugig", "Iliamna",
                                                                "Inalik Diomede", "Inupiaq", "Inupiat Eskimo", "Iqurmuit (Russian Mission)", "Ivanof Bay",
                                                                "Kake", "Kalskag", "Kaltag", "Kasaan", "Kasigluk", "Kawerak", "Kenaitze", "Ketchikan", "Kiana",
                                                                "King Cove", "King Salmon", "Kipnuk", "Kivalina", "Klawock", "Knik", "Kobuk", 
                                                                "Kodiak", "Kokhanok", "Koliganek", "Kongiganak", "Koniag Aleut", "Kotlik", "Kotzebue",
                                                                "Koyuk", "Koyukuk", "Kwethluk", "Kwigillingok", "Kwiguk", "Lake Minchumina", "Larsen Bay",
                                                                "Levelock", "Manley Hot Springs", "Manokotak", "Mary's Igloo", "Mauneluk Inupiat", "Mekoryuk", 
                                                                "Mentasta Lake", "Metlakatla", "Minto", "Mountain Village", "Nana Inupiat", "Napakiak", 
                                                                "Napaskiak", "Napaumute", "Nelson Lagoon", "Nenana", "New Stuyahok", "Newhalen", "Newtok", "Nikolai",
                                                                "Ninilchik", "Noatak", "Nome", "Nondalton", "Noorvik", "Northway", "Nulato", "Nunapitchukv", 
                                                                "Old Harbor", "Oscarville", "Ouzinkie", "Pauloff Harbor", "Pedro Bay", "Petersburg", "Pilot Point",
                                                                "Pitkas Point", "Point Hope", "Point Lay", "Port Graham", "Port Heiden", "Port Lions", "Portage Creek",
                                                                "Qagan Toyagungin", "Qawalangin", "Quinhagak", "Rampart", "Ruby", "Ruby Valley", "Salamatof", "Savoonga",
                                                                "Saxman", "Scammon Bay", "Selawik", "Seldovia", "Shageluk", "Shaktoolik", "Sheldon's Point", "Shishmaref",
                                                                "Shungnak", "Siberian Eskimo", "Siberian Yupik", "Sitka", "Slana", "Sleetmute", "South Naknek", 
                                                                "Southeast Alaska", "St. George", "St. Mary's", "St. Michael", "St. Paul", "Stebbins", "Stevens",
                                                                "Stony River", "Sugpiaq", "Tanaina", "Tanana", "Tanana Chiefs", "Tazlina", "Telida", "Teller",
                                                                "Tenakee Springs", "Tlingit", "Tlingit-Haida", "Tok", "Toksook", "Tulukskak", "Tuntutuliak", "Tununak",
                                                                "Twin Hills", "Tyonek", "Ugashik", "Umkumiate", "Unalakleet", "Unalaska", "Unangan Aleut", "Unga",
                                                                "Venetie", "Wainwright", "Wrangell", "Yakutat", "Yupik Eskimo","Central American Indian", "Mexican American Indian",
                                                                "South American Indian","American Indian", "American Indian or Alaska Native", "Canadian and Latin American Indian", 
                                                                "Chamorro", "Chuukese", "Fijian", "Guamanian", "Kiribati", "Kosraean", "Mariana Islander",
                                                                "Marshall", "Marshallese", "Melanesian", "Micronesian", "Native Hawaiian", 
                                                                "Native Hawaiian or Other Pacific Islander", "New Hebrides", "Other Pacific Islander",
                                                                "Papua New Guinean", "Pohnpeian", "Polynesian", "Saipanese", "Samoan", "Solomon", "Solomon Islander",
                                                                "Tahitian", "Tokelauan", "Tongan", "Yapese", "Guamanian or Chamorro", "Spanish American Indian",
                                                                "United Keetowah Band of Cherokee","Red Devil","Upper Chinook", "Kluti Kaah"," Lower Kalskag", "Nanticoke",
                                                                "Nightmute","Nuiqsut"," Port Gamble Klallam","San Xavier","Scott Valley","Seneca-Cayuga","Siuslaw","Talakamish",
                                                                "Tanacross","Togiak", "Lower Kalskag", "Port Gamble Klallam", "Tetlin")] <- "Native American/Alaskan Native/Pacific Islander"

updatedrace_orig_race_Oct$racecat[updatedrace_orig_race_Oct$Race %in% c("Asian","Bangladeshi", "Bhutanese", "Asian Indian", "Maldivian", "Nepalese", "Pakistani",
                                                                "Sri Lankan","Burmese", "Cambodian", "Indonesian", "Hmong", "Laotian", "Malaysian", "Singaporean",
                                                                "Thailand", "Vietnamese","Chinese", "Iwo Jiman", "Japanese", "Korean", "Okinawan", "Taiwanese","Thai")] <- "Asian"                     

updatedrace_orig_race_Oct$racecat[updatedrace_orig_race_Oct$Race %in% c("African", "Botswanan", "Ethiopian", "Liberian", "Madagascar", "Namibian", "Nigerian",
                                                                "Zairean","African American","Bahamian", "Barbadian", "Douglas", "Haitian", "Jamaican", "Tobagoan", "Trinidadian",
                                                                "West Indian","Black", "Black or African American")] <- "Black/AfrAm"

updatedrace_orig_race_Oct$racecat[updatedrace_orig_race_Oct$Race %in% c("Alpine", "English", "European", "French", "German", "Irish", "Italian", "Moor",
                                                                "Polish", "Scottish", "Wales","Iranian", "Iraqi", "Armenian", "Arab", "Assyrian", "Afghanistani", 
                                                                "Israeili", "Karluk", "Lebanese", "Egyptian", "Middle Eastern or North African", 
                                                                "Palestinian", "Syrian","White")] <-"White"


updatedrace_orig_race_Oct$racecat[updatedrace_orig_race_Oct$Race %in% c("Columbia","Dominica Islander", "Dominican", "Santo Domingo","Filipino","San Juan","Hispanic", "San Juan De")] <-"Hispanic"

updatedrace_orig_race_Oct$racecat[updatedrace_orig_race_Oct$Race %in% c("Declined to Report", "Declined to Specify", "Unreported/Refuse to Report", "Unreported/Refused to Report",
                                                                "Unreported/Refused To Report","Other Race","Carolinian", "Circle", "Council", "Eagle", "Lime", "Mcgrath", "Platinum", "Stewart",
                                                                "Trinity", "Wiseman","Oklahoma Delaware","Siletz","Stonyford","", "Suqpigaq", "Unreported/Refuse To Report")] <- "Other/Unknown"
# If ethnicity is Hispanic, change race to Hispanic
updatedrace_orig_race_Oct$Ethnicity[updatedrace_orig_race_Oct$Ethnicity == "Unreported/Refused to Report"] <- NA 
updatedrace_orig_race_Oct$Ethnicity[updatedrace_orig_race_Oct$Ethnicity == ""] <- NA 

updatedrace_orig_race_Oct$racecat[!is.na(updatedrace_orig_race_Oct$Ethnicity) & updatedrace_orig_race_Oct$Ethnicity != "Not Hispanic or Latino"] <- "Hispanic"
summary(as.factor(updatedrace_orig_race_Oct$racecat))
updatedrace_orig_race_Oct$racecat[is.na(updatedrace_orig_race_Oct$racecat)] <- "Other/Unknown"
summary(as.factor(updatedrace_orig_race_Oct$racecat))

#Remove rows that are Other/Unknown
updated.race %>%
  filter(racecat!="Other/Unknown") -> updated.race

#merged updated races with original data
updated.race %>%
  select(PatientID, racecat) -> updated.race

updated.race %>%
  rename(new_racecat = racecat) -> updated.race

Visit.demo <- left_join(Visit.demo, updated.race, by = "PatientID")

#update racecat var
Visit.demo$racecat <- if_else(Visit.demo$racecat=="Other/Unknown" & !(is.na(Visit.demo$new_racecat)), Visit.demo$new_racecat, Visit.demo$racecat)


#Remove duplicates in COVID results
COVIDResults<- distinct(COVIDResults) #7988377
#Merge Visit data with covid results to get patient IDs for all results 
merged.data1 <- inner_join(COVIDResults, Visit.demo, by="PatientID") #innerjoin because out of state covid results need to be filtered out (6994684)


#merge vaccine and visit data
merged.data2 <- left_join(merged.data1, Vaccine, by="VisitID") #get patient IDs for vaccination data; #6994821

#In incongruent tests, then use PCR results 

merged.data2 <- merged.data2 %>% 
  group_by(PatientID, Test_date) %>% 
  mutate(labresult = ifelse(n_distinct(Lab.Result.Interpretation) > 1, Lab.Result.Interpretation[Grouping=="COVID PCR (Active)"], Lab.Result.Interpretation))

merged.data2 %>%
  filter(Grouping != "Not Mapped")-> merged.data2 #6978732

#Create a variable for first and last date pts recieved a positive test (Ab or diagnostic)
merged.data2 <- as.data.table(merged.data2)
merged.data2[,
             first.pos := min(Test_date[labresult=="POSITIVE"])
             , by =.(PatientID)]

merged.data2[,
             last.pos := max(Test_date[labresult=="POSITIVE"])
             , by =.(PatientID)]
beep()
#This will include first positive tests for patients who were tested multiple times after study started
#vax.data1 %>%
# group_by(PatientID)%>%
# mutate(final_lab_status = ifelse(n_distinct(labresult) > 1, labresult[Test_date==max(Test_date)], labresult))%>%
# as.data.table()-> vax.data1

#No. of test before 1/4/2021
merged.data2 <- as.data.table(merged.data2)
merged.data2[,
             npriortests := n_distinct(Test_date[Test_date<"2021-04-01"])
             , by =.(PatientID)]
beep()

#Start with ppl who had a test after April 1 2021
merged.data2 <- as.data.table(merged.data2)
merged.data2[Test_date>="2021-04-01", n_distinct(PatientID)] #1200103

merged.data2.trunc <- merged.data2[Test_date>="2021-04-01"] #2023158

#In incongruent vax status, use latest vax status
#New final vaccination variables had to be created using the truncated dataset otherwise variables cannot be formed 

##Restrict to those who had a vaccine reported
merged.data2.trunc %>%
  filter(is.na(Vax_rec) | Vax_rec=="No response") %>%
  summarise(n= n_distinct(PatientID))

merged.data2.trunc %>%
  filter(Vax_rec=="Yes" | Vax_rec == "No") -> merged.data2.trunc #1965988

#Create data set for those with missing vaccination status 
merged.data2 %>%
  filter(Test_date>="2021-04-01" & (is.na(Vax_rec) | Vax_rec=="No response"))-> Missing_vax

#Exclude patients under 12 
Ped.vaxdata <- merged.data2.trunc %>%
  filter(Age<19) #306581

table(Ped.vaxdata$Age, Ped.vaxdata$Fully_vax, useNA = "ifany")
table(Ped.vaxdata$Age, Ped.vaxdata$labresult, useNA = "ifany")

merged.data2.trunc %>%
  filter(Age>=12) -> merged.data2.trunc #1807116

#Restrict data after 2021-04-01 to those with PCR/POC tests
merged.data2.trunc %>%
  filter(Grouping!="Antibody IgG" & Grouping != "Not Mapped")-> merged.data2.trunc #1799790
#Those not mapped are test for Ab spike protein 



#Use only first positive test (this is since 2021-04-01)
twopostest<- test %>%
  group_by(PatientID) %>% 
  filter(n_distinct(Test_date[labresult == "POSITIVE"]) > 1)

n_distinct(twopostest$PatientID) # 3161 patients had multiple positive tests
twopostest <- as.data.table(twopostest)

twopostest <- twopostest %>%
  group_by(PatientID) %>%
  mutate(interval = last.pos - Test_date[labresult == "POSITIVE"])
  
twopostest[interval>90, n_distinct(PatientID)] #15

#Only 15 patients had second positive test >90 days after first positive so exclude all second positive tests 
test %>%
  group_by(PatientID) %>%
  filter(Test_date <= min(Test_date[labresult=="POSITIVE"])) -> test #1763212

##Remove false negative tests
merged.data2.trunc %>% 
  arrange(PatientID, Test_date) -> merged.data2.trunc

merged.data2.trunc <- as.data.table(merged.data2.trunc)

merged.data2.trunc[,
                   interval := difftime(Test_date, lag(Test_date), units="days"),
                   , by =.(PatientID)] 
beep()

merged.data2.trunc %>%
  group_by(PatientID) %>%
  mutate(tmp = ifelse(all(labresult=="NEGATIVE") & interval>0 & interval <8, 1, 0)) %>%
  filter(tmp ==0 | is.na(tmp))-> merged.data2.trunc #remove negative tests done within one week of each other
beep()

merged.data2.trunc <- as.data.table(merged.data2.trunc)
merged.data2.trunc[,
                   interval1 := difftime(Test_date[labresult=="POSITIVE"], Test_date, units="days"),
                   , by =.(PatientID)] # remove negative tests done within 21 days of positive tests #1678721
beep()

merged.data2.trunc <- merged.data2.trunc %>%
  group_by(PatientID) %>%
  mutate(tmp1 = ifelse(any(labresult=="POSITIVE") & interval>0 & interval <21, 1, 0)) %>%
  filter(tmp1 == 0 | is.na(tmp1)) #Remove any negative test done within 3 weeks before or after a positive test #1667699
beep()

##Remove those who tested positive 90 days before 2021/04/01
merged.data2.trunc <- as.data.table(merged.data2.trunc)
merged.data2.trunc[,
  time.since.inf := difftime(Test_date, first.pos, units="days")] 

merged.data2.trunc$time.since.inf[merged.data2.trunc$time.since.inf== "-Inf"] <- NA

test <-merged.data2.trunc[time.since.inf<45,]
#length(unique(test$PatientID))
merged.data2.trunc<- as.data.table(merged.data2.trunc)
merged.data2.trunc <- merged.data2.trunc[time.since.inf<1 | time.since.inf>45 | is.na(time.since.inf),] #Keep those who tests positive the first time after study start, who tested more than 45 days before study start, and who never received a previous positive test
#1659068 

mean(merged.data2.trunc$time.since.inf, na.rm=T)
median(vax.data$time.since.inf[vax.data$time.since.inf>0], na.rm=T)
hist(as.numeric(vax.data$time.since.inf[vax.data$time.since.inf>0]), na.rm=T)


#Resolve incongruent vaccination info on different days 
merged.data2.trunc <- merged.data2.trunc %>% 
  group_by(PatientID, Vax_date) %>% 
  mutate(vaxstatus = ifelse(( n_distinct(Vax_rec) > 1), Vax_rec[Vax_date==max(Vax_date)], Vax_rec),
         vaxname = ifelse((n_distinct(Vax_manu) > 1), Vax_manu[Vax_date==max(Vax_date)], Vax_manu),
         fully_vax = ifelse(( n_distinct(Fully_vax) > 1), Fully_vax[Vax_date==max(Vax_date)], Fully_vax))

merged.data2.trunc %>%
  group_by(PatientID, Vax_date) %>% 
  mutate(incongruent =  ifelse(n_distinct(Vax_rec) > 1, 1,0)) %>%
  filter(incongruent>0) -> test


merged.data2.trunc <- merged.data2.trunc %>%
  group_by(PatientID) %>%
  mutate(final_vaxstatus = ifelse((n_distinct(vaxstatus) ==1), vaxstatus,
                                  ifelse((n_distinct(vaxstatus) > 1), vaxstatus[Vax_date==max(Vax_date)], NA_character_)),
         final_vaxdate = ifelse((n_distinct(vaxstatus) ==1), min(Vax_date),
                                ifelse((n_distinct(vaxstatus) > 1), max(Vax_date), NA_character_)), 
         final_fully_vax = ifelse((n_distinct(fully_vax) == 1), fully_vax, 
                                  ifelse(( n_distinct(fully_vax) > 1), fully_vax[Vax_date==max(Vax_date)], NA_character_)))

merged.data2.trunc %>%
  group_by(PatientID) %>% 
  mutate(incongruent.vaxstatus =  ifelse(n_distinct(vaxstatus) > 1, 1,0),
         incongruent.fullyvax =  ifelse(n_distinct(fully_vax) > 1, 1,0)) %>%
  filter(incongruent.vaxstatus>0 | incongruent.fullyvax>0) -> test


str(merged.data2.trunc$final_vaxdate)                            
merged.data2.trunc$final_vaxdate <- as.Date(merged.data2.trunc$final_vaxdate, origin = "1970-01-01")      


merged.data2.trunc <- as.data.table(merged.data2.trunc)


merged.data2.trunc %>%
  group_by(final_vaxstatus) %>%
  summarise(n=n_distinct(PatientID))
merged.data2.trunc %>%
  group_by(vaxstatus) %>%
  summarise(n=n_distinct(PatientID))

merged.data2.trunc[,n_distinct(PatientID)]
merged.data2.trunc %>%
  group_by(vaxstatus) %>%
  summarise(n=n_distinct(PatientID))

#3.9% of the patients don't have a vaccine recorded

#Previous infection based on any test
merged.data2.trunc %>%
  group_by(PatientID) %>%
  mutate(prev.inf = ifelse((first.pos>="2021-04-01" | is.na(first.pos)), "No", "Yes")) ->merged.data2.trunc #get date of first Ab positive test to get prev infection based on Ab test


table(merged.data2.trunc$prev.inf, useNA = "ifany")

#Create variable for total number of previous tests
merged.data2.trunc %>%
  group_by(PatientID) %>%
  mutate(Exp.risk = ifelse(npriortests== 0, "0",
                           ifelse(npriortests==1, "1",
                                  ifelse(npriortests>1,">=2")))) -> merged.data2.trunc
merged.data2.trunc %>%
  group_by(Exp.risk) %>%
  summarise(n=n_distinct(PatientID))


#Tabulate cases and vaccinated
merged.data2.trunc %>%
  group_by(final_vaxstatus, labresult) %>%
  summarise(n=n_distinct(PatientID))

#Create biweekly variable 
merged.data2.trunc$biweekly.period = NA
merged.data2.trunc$biweekly.period[merged.data2.trunc$Test_date>="2021-04-01" & merged.data2.trunc$Test_date<="2021-04-15"] <- 1
merged.data2.trunc$biweekly.period[merged.data2.trunc$Test_date>"2021-04-15" & merged.data2.trunc$Test_date<="2021-04-29"] <- 2
merged.data2.trunc$biweekly.period[merged.data2.trunc$Test_date>"2021-04-29" & merged.data2.trunc$Test_date<="2021-05-13"] <- 3
merged.data2.trunc$biweekly.period[merged.data2.trunc$Test_date>"2021-05-13" & merged.data2.trunc$Test_date<="2021-05-27"] <- 4
merged.data2.trunc$biweekly.period[merged.data2.trunc$Test_date>"2021-05-27" & merged.data2.trunc$Test_date<="2021-06-10"] <- 5
merged.data2.trunc$biweekly.period[merged.data2.trunc$Test_date>"2021-06-10" & merged.data2.trunc$Test_date<="2021-06-24"] <- 6
merged.data2.trunc$biweekly.period[merged.data2.trunc$Test_date>"2021-06-24" & merged.data2.trunc$Test_date<="2021-07-08"] <- 7
merged.data2.trunc$biweekly.period[merged.data2.trunc$Test_date>"2021-07-08" & merged.data2.trunc$Test_date<="2021-07-22"] <- 8
merged.data2.trunc$biweekly.period[merged.data2.trunc$Test_date>"2021-07-22" & merged.data2.trunc$Test_date<="2021-08-05"] <- 9
merged.data2.trunc$biweekly.period[merged.data2.trunc$Test_date>"2021-08-05" & merged.data2.trunc$Test_date<="2021-08-19"] <- 10
merged.data2.trunc$biweekly.period[merged.data2.trunc$Test_date>"2021-08-19" & merged.data2.trunc$Test_date<="2021-09-02"] <- 11
merged.data2.trunc$biweekly.period[merged.data2.trunc$Test_date>"2021-09-02" & merged.data2.trunc$Test_date<="2021-09-16"] <- 12
merged.data2.trunc$biweekly.period[merged.data2.trunc$Test_date>"2021-09-16" & merged.data2.trunc$Test_date<="2021-09-30"] <- 13
merged.data2.trunc$biweekly.period[merged.data2.trunc$Test_date>"2021-09-30" & merged.data2.trunc$Test_date<="2021-10-14"] <- 14
merged.data2.trunc$biweekly.period[merged.data2.trunc$Test_date>="2021-10-14"] <- 15




#check if duplicates in each period 
merged.data2.trunc%>%
  group_by(biweekly.period) %>%
  summarise(n=n())

merged.data2.trunc%>%
  group_by(biweekly.period) %>%
  summarise(n=n_distinct(PatientID))

test <- merged.data2.trunc %>%
  filter(biweekly.period==15)

test.dup <- test[duplicated((test$PatientID)),]

##The duplicates in each period are due to ppl getting PCR + antigen test on the same day and coming positive in one or both tests

#Merge Vital stats data
VitalSigns <- read_delim("/Users/madhurarane/Documents/CityMD/CUNY_VitalSigns.csv", delim="|",escape_double = FALSE, trim_ws = TRUE)
#restrict dates 
VitalSigns$`Adjusted Visit Date` <- as.Date(VitalSigns$`Adjusted Visit Date`, format="%m/%d/%Y")

VitalSigns %>%
  filter(`Adjusted Visit Date`>="2021-04-01") -> VitalSigns
#2627815

#select temp, oxygen, BMI
VitalSigns %>%
  select(PatientID, VisitID, `Adjusted Visit Date`,`Oxygen sat %`, Temp, BMI)%>%
  rename(Adjusted.Visit.Date = `Adjusted Visit Date`,
         O2sat = `Oxygen sat %`) -> VitalSigns
#7602099

#removed records if NA for all
VitalSigns <- VitalSigns[!is.na(VitalSigns$O2sat) & !is.na(VitalSigns$Temp) & !is.na(VitalSigns$BMI),]
#2208884

#check if any missing
sum(is.na(VitalSigns$O2sat))
sum(is.na(VitalSigns$Temp))
sum(is.na(VitalSigns$BMI))
#0s for all

##clean up the values 
VitalSigns$BMI[VitalSigns$BMI<10 | VitalSigns$BMI>186] <- NA
VitalSigns$O2sat[VitalSigns$O2sat==9.0] <- 95.0
VitalSigns$O2sat[VitalSigns$O2sat==101.0] <- 100.0
VitalSigns$Temp[VitalSigns$Temp==9.0] <- 98.0
VitalSigns$Temp[VitalSigns$Temp>115.0] <- NA


#Check for unique visit ID
n_distinct(VitalSigns$VisitID)

#check overlap in patient ID
mean(merged.data2.trunc$PatientID %in% VitalSigns$PatientID)
#[1] 90.3%

#merge with covidresults
merged.data2.trunc <- left_join(merged.data2.trunc, VitalSigns, by = c("PatientID", "VisitID"))

#Create a variable for fever (Y/N)
merged.data2.trunc %>%
  mutate(Fever.vitalsigns = ifelse(Temp>=100.4, 1,0))-> merged.data2.trunc

#Create a variable for O2 saturation
merged.data2.trunc %>%
  mutate(O2.vitalsigns = ifelse(O2sat<=95, 1,0))-> merged.data2.trunc

#Categorize BMI 
merged.data2.trunc$BMI_cat = NA
merged.data2.trunc$BMI_cat[merged.data2.trunc$BMI<18.5] <- "Underweight"
merged.data2.trunc$BMI_cat[merged.data2.trunc$BMI>=18.5 & merged.data2.trunc$BMI<=24.9] <- "Healthy"
merged.data2.trunc$BMI_cat[merged.data2.trunc$BMI>24.9 & merged.data2.trunc$BMI<=29.9] <- "Overweight"
merged.data2.trunc$BMI_cat[merged.data2.trunc$BMI>29.9] <- "Obese"

##Merge comorbidities data
MedHist <- read_delim("/Users/madhurarane/Documents/CityMD/CUNY_PastMedicalHistory.csv", delim="|",escape_double = FALSE, trim_ws = TRUE)

##Past medical history
MedHist %>%
  filter(!is.na(MedicalHistory))-> MedHist

#restrict dates 
MedHist$`Adjusted Visit Date` <- as.Date(MedHist$`Adjusted Visit Date`, format="%m/%d/%Y")

MedHist %>%
  filter(`Adjusted Visit Date`>="2021-04-01") -> MedHist
#1885357


MedHist %>%
  filter(MedicalHistory %in% c("Hypertension", "Hypercholesterolemia", "Asthma","Anxiety","	Diabetes","Depression", "HTN", "asthma",
                               "hypertension","anxiety","diabetes","depression","Hypercholesteremia","COPD","Hypercholesteremia",
                               "Atrial Fibrillation","HTN (hypertension)","HIV Infection","HIV","Heart Disease (CAD)","Breast Cancer",
                               "DM","heart murmur","hypercholesteremia","HLD","CAD","htn","childhood asthma","Childhood asthma",
                               "Heart murmur","high cholesterol","Stroke","breast cancer","Breast cancer","High cholesterol","Prostate Cancer",
                               "Heart Murmur","Childhood Asthma","CHF","HIV positive","Anxiety and depression","Tachycardia",
                               "h/o breast cancer","SVT","High Cholesterol","Diabetes type 2","Atrial fibrillation","Diabetes Type 2",
                               "GAD (generalized anxiety disorder)","Anxiety/Depression","Vitamin D deficiency","Diabetes Type II",
                               "anxiety/depression","Pacemaker","prostate cancer","COPD (chronic obstructive pulmonary disease)",
                               "Type 2 Diabetes","tachycardia","Prostate cancer","Type II Diabetes","diabetes type 2","high blood pressure",
                               "Heart disease","Elevated blood pressure","hypercholestrolemia","Asthma exacerbation","stroke","Thyroid cancer",
                               "CAD (coronary artery disease)","Type 2 diabetes mellitus","hypercholestemia","MI","HIV (human immunodeficiency virus infection)",
                               "heart disease","hypercholestermia","DM2 (diabetes mellitus, type 2)","h/o asthma","Hypercholestermia","h/o stroke",
                               "palpitations","HIV infection","arrhythmia","Depression/Anxiety","ASTHMA","Hypercholestrolemia","Cancer","H/o breast cancer",
                               "Heart attack","Arrythmia","High blood pressure","Type 2 diabetes","atrial fibrillation","Diabetes type II","Hypercholestemia",
                               "Diabetes Type 1","heart attack","depression/anxiety","type 2 diabetes","Cardiomyopathy","ANXIETY","Heart Disease",
                               "h/o prostate cancer","HIV+","Diabetes II","Hypercholestremia","Coronary artery disease","kidney disease","Lymphoma",
                               "anxiety and depression","skin cancer","colon cancer","Lung cancer","Leukemia","Angina","Asthma (childhood)","DM II",
                               "Thyroid Cancer","lymphoma","HYPERCHOLESTREMIA","Congestive heart failure","HIV Positive","HIV Positive","Skin cancer",
                               "cholesterol","h/o heart attack","Anxiety/depression","diabetic","History of breast cancer","Hypercholesterol","Hypercholestoremia",
                               "Diabetic","cancer","h/o thyroid cancer","Hypertriglyceridemia","hypercholesterol","Anxiety and Depression","Cholesterol",
                               "diabetes type II","Hx of breast cancer","Colon cancer","h/o childhood asthma","h/o DVT","coronary artery disease",
                               "congestive heart failure","hx of breast cancer","irregular heartbeat","high triglycerides","Hypertention","copd",
                               "Coronary Artery Disease","Diabetes 2","heart failure","vitamin d deficiency","h/o MI","Lung Cancer","Congestive Heart Failure",
                               "Heart Attack","bladder cancer","heart condition","Kidney failure","leukemia","Bladder cancer","vitamin D deficiency",
                               "Depression/anxiety","Colon Cancer","DM (diabetes mellitus)","Type II diabetes","Depression with anxiety","Heart failure",
                               "Vitamin D Deficiency","h/o colon cancer","Diabetes Mellitus","Diabeties","Anixety","kidney failure","HYPERTENSION",
                               "hypercholestremia","ovarian cancer","irregular heart beat","Asthma as a child","bradycardia","h/o skin cancer","testicular cancer",
                               "Kidney Disease","H/o asthma","anixety","brain tumor","Irregular heart beat","Mild asthma","h/o lung cancer","Cardiac stent",
                               "Skin Cancer","DM 2","stent","cardiac stents","Ovarian cancer","depression and anxiety","Heart condition","Anxiety/ Depression",
                               "High Blood Pressure","DEPRESSION","Diabetes mellitus II","Hx of stroke","pace maker","Elevated BP","Heart stent","Hx of Breast Cancer",
                               "Breast CA","Chronic kidney disease","SVT (supraventricular tachycardia)","CKD","diabeties","Diabetes - Type II","DIABETES",
                               "heart palpitations","hx of stroke","childhood Asthma","H/o DVT","Hypocholesterolemia","Heart palpitations","Cardiomegaly","Pituitary tumor",
                               "DM-2","heart arrhythmia","pituitary tumor","h/o hypertension","Stent","Testicular cancer","H/o thyroid cancer","Ovarian Cancer",
                               "cervical cancer","h/o cancer","type II diabetes","high cholestrol","Hx of asthma","aortic stenosis","Bladder Cancer","Depression and Anxiety",
                               "kidney cancer","hx of asthma","Hx of heart attack","Myocardial infarction","H/o prostate cancer","Type II DM","h/o bladder cancer",
                               "Diabetes Mellitus II","Hx of DVT","hypertrophic cardiomyopathy","H/o heart attack","CHF (congestive heart failure)","H/o MI","heart stents",
                               "Pace maker","Stents","Anxiety disorder","Diabetes type 2, controlled","Murmur","Breast Cancer (remission)","diabetes II","H/o Breast Cancer",
                               "Mild Asthma","Asthma (seasonal)","History of heart artery stent","Cervical cancer","enlarged heart","polycystic kidney disease",
                               "Diabetes (type 2)","HIV disease","Uterine cancer","ashtma","h/o HTN","h/o testicular cancer","Hypertrophic cardiomyopathy",
                               "Atrial Fibrilation","Cardiac stents","History of stroke","Hypercholestrolnemia","chronic kidney disease","Heart Failure",
                               "uterine cancer","High BP","Hypetension","Aortic stenosis","h/o Breast Cancer","Supraventricular tachycardia","Myocardial Infarction",
                               "General Anxiety Disorder","Hypercholesterioma","basal cell carcinoma","Heart stents","Hypercholesterimia","h/o heart murmur",
                               "hx of heart attack","H/o Asthma","DM Type 2","cardiomegaly","Enlarged heart","Major depressive disorder","Renal failure","Atrial fibrilation",
                               "murmur","COPD exacerbation","Type II diabetes mellitus","stents","TIA (transient ischemic attack)","Kidney Failure","atrial fibrilation",
                               "Heart Condition","Kidney cancer","Breast cancer (remission)","Diabetes mellitus","hx of DVT","major depressive disorder","Pulmonary hypertension",
                               "breast cancer (remission)","h/o Asthma","h/o Breast cancer","ASthma","h/o depression","Diabetes - type 2","h/o kidney cancer","hypertention",
                               "h/o cervical cancer","Hx of MI","Uterine Cancer","DM Type II")) -> MedHist

table(MedHist$MedicalHistory)

#Categorize medical history
MedHist$category = NA
MedHist$category[MedHist$MedicalHistory %in% c("Angina","aortic stenosis","Aortic stenosis","arrhythmia",
                                               "Arrythmia","atrial fibrilation","Atrial fibrilation","Atrial Fibrilation",
                                               "atrial fibrillation","Atrial fibrillation","Atrial Fibrillation","bradycardia",
                                               "CAD","CAD (coronary artery disease)","Cardiac stent","cardiac stents",
                                               "Cardiac stents","cardiomegaly","Cardiomegaly","Cardiomyopathy","CHF",
                                               "CHF (congestive heart failure)","congestive heart failure",
                                               "Congestive heart failure","Congestive Heart Failure","coronary artery disease",
                                               "Coronary artery disease","Coronary Artery Disease","Elevated blood pressure",
                                               "Elevated BP","enlarged heart","Enlarged heart","h/o DVT","H/o DVT","h/o heart attack",
                                               "H/o heart attack","h/o heart murmur","h/o HTN","h/o hypertension","h/o stroke",
                                               "heart arrhythmia","heart attack","Heart attack","Heart Attack","heart condition",
                                               "Heart condition","Heart Condition","heart disease","Heart disease","Heart Disease",
                                               "Heart Disease (CAD)","heart failure","Heart failure","Heart Failure","heart murmur",
                                               "Heart murmur","Heart Murmur","heart palpitations","Heart palpitations","Heart stent",
                                               "heart stents","Heart stents","high blood pressure","High blood pressure","High Blood Pressure",
                                               "High BP","History of heart artery stent","History of stroke","htn","HTN","HTN (hypertension)",
                                               "hx of DVT","Hx of DVT","hx of heart attack","Hx of heart attack","Hx of MI","hx of stroke",
                                               "Hx of stroke","hypertension","Hypertension","HYPERTENSION","hypertention","Hypertention",
                                               "hypertrophic cardiomyopathy","Hypertrophic cardiomyopathy","Hypetension","irregular heart beat",
                                               "Irregular heart beat","irregular heartbeat","murmur","Murmur","Myocardial infarction",
                                               "Myocardial Infarction","pace maker","Pace maker","Pacemaker","palpitations","stent ","Stent",
                                               "Stents","stroke","Stroke","Supraventricular tachycardia","SVT","SVT (supraventricular tachycardia)",
                                               "tachycardia","Tachycardia","TIA (transient ischemic attack)","h/o MI","H/o MI","MI",
                                               "Pulmonary hypertension","stent","stents")] <- "Heart Disease"


MedHist$category[MedHist$MedicalHistory %in% c("ashtma","asthma","Asthma","ASthma","ASTHMA","Asthma (seasonal)","Asthma exacerbation",
                                               "h/o asthma","h/o Asthma","H/o asthma","H/o Asthma","hx of asthma","Hx of asthma",
                                               "copd","COPD","COPD (chronic obstructive pulmonary disease)","COPD exacerbation","Mild Asthma",
                                               "Mild asthma","HLD")] <- "Asthma/COPD"



MedHist$category[MedHist$MedicalHistory %in% c("chronic kidney disease","Chronic kidney disease","CKD","kidney disease",
                                               "Kidney Disease","kidney failure","Kidney failure","Kidney Failure",
                                               "Renal failure","polycystic kidney disease")] <- "Kidney disease"

MedHist$category[MedHist$MedicalHistory %in% c("bladder cancer","Bladder cancer","Bladder Cancer","brain tumor","Breast CA",
                                               "breast cancer","Breast cancer","Breast Cancer","breast cancer (remission)",
                                               "Breast cancer (remission)","Breast Cancer (remission)","cancer","Cancer",
                                               "cervical cancer","Cervical cancer","colon cancer","Colon cancer","Colon Cancer",
                                               "h/o bladder cancer","h/o breast cancer","h/o Breast cancer","h/o Breast Cancer",
                                               "H/o breast cancer","H/o Breast Cancer","h/o cancer","h/o cervical cancer",
                                               "h/o colon cancer","h/o kidney cancer","h/o lung cancer","h/o prostate cancer",
                                               "H/o prostate cancer","h/o testicular cancer","h/o thyroid cancer","H/o thyroid cancer",
                                               "hx of breast cancer","Hx of breast cancer","Hx of Breast Cancer","kidney cancer",
                                               "Kidney cancer","leukemia","Leukemia","Lung cancer","Lung Cancer","lymphoma",
                                               "Lymphoma","ovarian cancer","Ovarian cancer","Ovarian Cancer","prostate cancer",
                                               "Prostate cancer","Prostate Cancer","skin cancer","Skin cancer","Skin Cancer",
                                               "h/o skin cancer","testicular cancer","Testicular cancer","Thyroid cancer","Thyroid Cancer",
                                               "uterine cancer","Uterine cancer","Uterine Cancer","basal cell carcinoma","History of breast cancer",
                                               "pituitary tumor","Pituitary tumor")] <- "Cancer"

MedHist$category[MedHist$MedicalHistory %in% c("anixety","Anixety","anxiety","Anxiety","ANXIETY","anxiety and depression",
                                               "Anxiety and depression","Anxiety and Depression","Anxiety disorder","Anxiety/ Depression",
                                               "anxiety/depression","Anxiety/depression","Anxiety/Depression","depression","Depression",
                                               "DEPRESSION","depression and anxiety","Depression and Anxiety","Depression with anxiety",
                                               "depression/anxiety","Depression/anxiety","Depression/Anxiety","h/o depression",
                                               "major depressive disorder","Major depressive disorder","GAD (generalized anxiety disorder)",
                                               "General Anxiety Disorder")] <-"Depression/anxiety"


MedHist$category[MedHist$MedicalHistory %in% c("HIV","HIV (human immunodeficiency virus infection)","HIV disease",
                                               "HIV infection","HIV Infection","HIV positive","HIV Positive","HIV+")] <-"HIV"



MedHist$category[MedHist$MedicalHistory %in% c("cholesterol","Cholesterol","high cholesterol","High cholesterol",
                                               "High Cholesterol","high cholestrol","high triglycerides","hypercholestemia",
                                               "Hypercholestemia","hypercholesteremia","Hypercholesteremia","Hypercholesterimia",
                                               "Hypercholesterioma","hypercholestermia","Hypercholestermia","hypercholesterol",
                                               "Hypercholesterol","Hypercholesterolemia","Hypercholestoremia","hypercholestremia",
                                               "Hypercholestremia","HYPERCHOLESTREMIA","hypercholestrolemia","Hypercholestrolemia",
                                               "Hypercholestrolnemia","Hypertriglyceridemia","Hypocholesterolemia")] <- "Cholesterol"


MedHist$category[MedHist$MedicalHistory %in% c("vitamin d deficiency","vitamin D deficiency",
                                               "Vitamin D deficiency","Vitamin D Deficiency")] <- "VitD"


MedHist$category[MedHist$MedicalHistory %in% c("diabetes","DIABETES","Diabetes - type 2","Diabetes - Type II","Diabetes (type 2)",
                                               "Diabetes 2","diabetes II","Diabetes II","Diabetes mellitus","Diabetes Mellitus",
                                               "Diabetes mellitus II","Diabetes Mellitus II","diabetes type 2","Diabetes type 2",
                                               "Diabetes Type 2","Diabetes type 2, controlled","diabetes type II","Diabetes type II",
                                               "Diabetes Type II","Diabetes Type II","diabetic","Diabetic","diabeties","Diabeties",
                                               "DM","DM (diabetes mellitus)","DM 2","DM II","DM Type 2","DM Type II","DM-2",
                                               "DM2 (diabetes mellitus, type 2)","type 2 diabetes","Type 2 diabetes","Type 2 Diabetes",
                                               "Type 2 diabetes mellitus","type II diabetes","Type II diabetes","Type II Diabetes",
                                               "Type II diabetes mellitus","Type II DM")] <- "Diabetes"
table(MedHist$category, useNA = "ifany")

#Remove diabetes 1 and childhood asthma
MedHist %>%
  filter(!is.na(category)) -> MedHist

#Merge medical history data with covid results

#Make data wide
MedHist <- MedHist %>%
  group_by(VisitID) %>%
  arrange(`Adjusted Visit Date`, .by_group=TRUE)%>%
  mutate(Visit = 1:n())

MedHist <- MedHist %>% rename(Date=`Adjusted Visit Date`) 
MedHist <- as.data.table(MedHist)
MedHist.w <- dcast(MedHist, VisitID  ~ Visit, value.var = c("Date","category"))
MedHist.w <- MedHist.w[,c(1,12:21)]


#check overlap in patient ID
mean(merged.data2.trunc$VisitID %in% MedHist$VisitID)
#[1]  21.9%

#merge with covidresults
merged.data2.trunc <- left_join(merged.data2.trunc, MedHist.w, by = c("VisitID"))

#No. of pts with comorbidities
merged.data2.trunc <- as.data.table(merged.data2.trunc)
merged.data2.trunc[ (category_1=="Heart Disease" | category_2=="Heart Disease"|category_3=="Heart Disease"|
                       category_4=="Heart Disease" | category_5=="Heart Disease" | category_6=="Heart Disease" |
                       category_7=="Heart Disease" | category_8=="Heart Disease" | category_9=="Heart Disease" |
                       category_10=="Heart Disease"), n_distinct(PatientID)] #35,288

merged.data2.trunc[(category_1=="Cholesterol" | category_2=="Cholesterol"|category_3=="Cholesterol"|
                      category_4=="Cholesterol" | category_5=="Cholesterol" | category_6=="Cholesterol" |
                      category_7=="Cholesterol" | category_8=="Cholesterol" | category_9=="Cholesterol" |
                      category_10=="Cholesterol"), n_distinct(PatientID)] #15,901

merged.data2.trunc[(category_1=="Asthma/COPD" | category_2=="Asthma/COPD"|category_3=="Asthma/COPD"|
                      category_4=="Asthma/COPD" | category_5=="Asthma/COPD" | category_6=="Asthma/COPD" |
                      category_7=="Asthma/COPD" | category_8=="Asthma/COPD" | category_9=="Asthma/COPD" |
                      category_10=="Asthma/COPD"), n_distinct(PatientID)] #20,881

merged.data2.trunc[(category_1=="Depression/anxiety" | category_2=="Depression/anxiety"|category_3=="Depression/anxiety"|
                      category_4=="Depression/anxiety" | category_5=="Depression/anxiety" | category_6=="Depression/anxiety" |
                      category_7=="Depression/anxiety" | category_8=="Depression/anxiety" | category_9=="Depression/anxiety" |
                      category_10=="Depression/anxiety"), n_distinct(PatientID)] #16,594



##Symptoms data based on Emily S. code
##Re-run the following code on the entire ChiefCompliants dataset and then merge with truncated vaccine dataset using VisitID. This is because of the way vaccination data was abstrated from chiefcomplaints earlier
#Renaming components of the merged chief complaints table and coercing all character values to upper case
ChiefComplaint %>%
  rename(D1=`Diagnosis 1`, D2=`Diagnosis 2`, D3=`Diagnosis 3`, D4=`Diagnosis 4`)%>%
  mutate_if(is.character, str_to_upper)-> ChiefComplaint

#Generating lists of chosen symptoms
chills_terms=c("CHILLS", "CHILLS (WITHOUT FEVER)", "CHILLS WITH FEVER",
               "CHILLS WITHOUT FEVER", "FEVER AND CHILLS", "FEVER CHILLS",
               "FEVER WITH CHILLS")
cough_terms=c("COUGH", "COUGHING BLOOD", "COUGH HEADACHE", "COUGH IN ADULT", "COUGH IN ADULT PATIENT",
              "COUGH WITH HEMOPTYSIS", "COUGH WITH SPUTUM", "COUGH, PERSISTENT", "COUGHING", "DRY COUGH", "PERSISTENT COUGH",
              "PRODUCTIVE COUGH", "UPPER AIRWAY COUGH SYNDROME", "UPPER RESPIRATORY INFECTION WITH COUGH AND CONGESTION",
              "URI WITH COUGH AND CONGESTION", "VIRAL URI WITH COUGH", "COUGH", "VIRAL UPPER RESPIRATORY TRACT INFECTION WITH COUGH",
              "COUGH IN ADULT", "COUGH IN PEDIATRIC PATIENT", "COUGHING", "PRODUCTIVE COUGH",
              "UPPER AIRWAY COUGH SYNDROME", "URI WITH COUGH AND CONGESTION")
fever_terms=c("FEVER",
              "FEVER CHILLS", "FEVER IN PEDIATRIC PATIENT", "CHILLS WITH FEVER",
              "FEVER IN ADULT", "FEVER, UNSPECIFIED FEVER CAUSE", "FEVER IN CHILD",
              "FEVER OF UNKOWN ORIGIN (FUO)", "CHILLS WITH FEVER", "FEVER AND CHILLS",
              "FEVER IN PEDIATRIC PATIENT", "FEVER, UNSPECIFIED",
              "FEELS FEVERISH", "FEVER IN ADULT", "FEVER OF UNKNOWN ORIGIN",
              "LOW GRADE FEVER", "HIGH FEVER", "INTERMITTENT FEVER", "RECENT UNEXPLAINED FEVER",
              "PERSISTENT FEVER")
fatigue_terms=c("FATIGUE", "TIREDNESS","MALAISE AND FATIGUE",
                "FATIGUE, UNSPECIFIED TYPE", "OTHER FATIGUE", "TIRED")
body.ache_terms=c("MYALGIAS", "MYALGIA", "BODY ACHES","MUSCLE ACHE", "GENERALIZED BODY ACHES",
                  "MUSCLE ACHE", "MUSCLE SORENESS", "MYALGIA, UNSPECIFIED SITE", "MUSCLE ACHE OF EXTREMITY")
headache_terms=c("ACUTE HEADACHE", "HEADACHE", "ACUTE INTRACTABLE HEADACHE, UNSPECIFIED HEADACHE TYPE",
                 "ACUTE NON INTRACTABLE TENSION-TYPE HEADACHE", "COUGH HEADACHE",
                 "NEW PERSISTENT DAILY HEADACHE", "OTHER HEADACHE SYNDROME", "HEADACHE ABOVE THE EYE REGION")
taste.smell_terms=c("ABNORMAL SENSE OF TASTE", "ABNORMAL SMELL", "ALTERED TASTE",
                    "DECREASED SENSE OF SMELL", "DECREASED SENSE OF TASTE", "DISTURBANCE OF SMELL",
                    "DISTURBANCE OF SMELL AND TASTE", "BITTER TASTE", "LOSS OF SMELL", "LOSS OF TASTE",
                    "SENSE OF SMELL ALTERED", "SMELL DISTURBANCE", "SMELL OR TASTE SENSATION DISTURBANCE",
                    "TASTE ABSENT", "TASTE IMPAIRMENT", "UNSPECIFIED DISTURBANCES OF SMELL AND TASTE", "SMELL, IMPAIRED")
sore.throat_terms=c("ACUTE INFECTIVE PHARYNGITIS", "ACUTE NASOPHARYNGITIS", "ACUTE NASOPHARYNGITIS (COMMON COLD)",
                    "ACUTE NASOPHARYNGITIS [COMMON COLD]", "ACUTE PHARYNGITIS", "ACUTE PHARYNGITIS, UNSPECIFIED",
                    "ACUTE PHARYNGITIS, UNSPECIFIED ETIOLOGY",  "ACUTE SORE THROAT", "ACUTE VIRAL PHARYNGITIS",
                    "EXUDATIVE PHARYNGITIS", "NASOPHARYNGITIS", "NASOPHARYNGITIS ACUTE", "PHARYNGITIS", "PHARYNGITIS, ACUTE",
                    "RHINOPHARYNGITIS", "SORE THROAT", "SORE THROAT (VIRAL)", "SORE THROAT AND LARYNGITIS",
                    "VIRAL PHARYNGITIS", "VIRAL SORE THROAT")
congestion.nose_terms=c("CHEST CONGESTION", "CONGESTION OF NASAL SINUS", "CONGESTION OF PARANASAL SINUS",
                        "CONGESTION OF RESPIRATORY TRACT", "CONGESTION OF UPPER AIRWAY",
                        "MILD NASAL CONGESTION", "NASAL CONGESTION", "NASAL CONGESTION WITH RHINORRHEA",
                        "NASAL SINUS CONGESTION", "NOSE CONGESTION", "PULMONARY CONGESTION", "RUNNY NOSE",
                        "SINUS CONGESTION", "STUFFY AND RUNNY NOSE", "UPPER RESPIRATORY INFECTION WITH COUGH AND CONGESTION",
                        "URI WITH COUGH AND CONGESTION")
nausea.vomit_terms=c("ABDOMINAL PAIN, VOMITING, AND DIARRHEA", "ACUTE VOMITING", "CYCLICAL VOMITING",
                     "HEMATEMESIS WITH NAUSEA",
                     "INTRACTABLE VOMITING", "INTRACTABLE VOMITING WITH NAUSEA",  
                     "NAUSEA", "NAUSEA & VOMITING", "NAUSEA AND VOMITING",
                     "NAUSEA ALONE", "NAUSEA AND VOMITING IN CHILD", "PROJECTILE VOMITING WITH NAUSEA",
                     "PERSISTENT VOMITING", "VOMITING", "NAUSEA (WITHOUT VOMITING)", "NAUSEA WITH VOMITING",
                     "NAUSEA WITHOUT VOMITING", "POST-TUSSIVE VOMITING", "VOMITING (WITHOUT DIARRHEA)",
                     "VOMITING AND DIARRHEA", "VOMITINGS", "VOMITING IN PEDIATRIC PATIENT", "VOMITING ALONE")
diarrhea_terms=c("ABDOMINAL PAIN, VOMITING, AND DIARRHEA", "ACUTE DIARRHEA", "DIARRHEA", "DIARRHEA (WITHOUT VOMITING)",
                 "DIARRHEA IN ADULT PATIENT", "DIARRHEA, UNSPECIFIED", "NAUSEA VOMITING AND DIARRHEA", "INFECTIOUS DIARRHEA",
                 "VOMITING AND DIARRHEA")
sob_terms=c("ABNORMALITY OF BREATHING", "BREATHING DIFFICULTY", "BREATHING PROBLEM", "CHEST PAIN MADE WORSE BY BREATHING",
            "CHEST PAIN ON BREATHING", "DIFFICULTY BREATHING", "EXERTIONAL SHORTNESS OF BREATH", "MILD SHORTNESS OF BREATH",
            "HEAVY BREATHING", "MILD SHORTNESS OF BREATH", "PAIN AGGRAVATED BY COUGHING AND DEEP BREATHING", "PAINFUL BREATHING",
            "FAST BREATHING", "SENSATION OF BREATHLESSNESS", "SHORTNESS OF BREATH", "SHORTNESS OF BREATH AT REST", "SOB (SHORTNESS OF BREATH)",
            "SOB (SHORTNESS OF BREATH) ON EXERTION", "SHORTNESS OF BREATH ON EXERTION", "SOBOE (SHORTNESS OF BREATH ON EXERTION)")
chest.pain_terms=c("ACUTE CHEST PAIN", "ATYPICAL CHEST PAIN", "CHEST PAIN", "CHEST PAIN AT REST", "CHEST PAIN IN ADULT",
                   "CHEST PAIN IN PATIENT YOUNGER THAN 17 YEARS", "CHEST PAIN MADE WORSE BY BREATHING",
                   "CHEST PAIN OF UNCERTAIN ETIOLOGY", "CHEST PAIN ON BREATHING", "CHEST PAIN ON EXERTION",
                   "CHEST PAIN ON RESPIRATION", "CHEST PAIN SYNDROME", "ATYPICAL CHEST PAIN, EXERTIONAL",  
                   "CHEST PAIN, MID STERNAL", "CHEST PAIN, NON-CARDIAC", "CHEST PAIN, UNSPECIFIED", "CHEST PAIN, UNSPECIFIED TYPE",
                   "CHEST PRESSURE", "CHEST TIGHTNESS", "CHEST TIGHTNESS OR PRESSURE", "FEELING OF CHEST TIGHTNESS",
                   "INTERMITTENT CHEST PAIN", "NONSPECIFIC CHEST PAIN", "OTHER CHEST PAIN", "PLEURITIC CHEST PAIN",
                   "SENSATION OF CHEST TIGHTNESS")
confusion_terms=c("ALTERED MENTAL STATE", "ALTERED MENTAL STATUS", "ALTERED MENTAL STATUS, UNSPECIFIED", "CONFUSION")

#Creating symptom categories out of the chief complaints dataset using the symptoms lists
ChiefComplaint<-ChiefComplaint %>%
  mutate(chills = ifelse(Complaint %in% chills_terms|
                           D1 %in% chills_terms|
                           D2 %in% chills_terms|
                           D3 %in% chills_terms|
                           D4 %in% chills_terms, "Yes", NA),
         cough = ifelse(Complaint %in% cough_terms|
                          D1 %in% cough_terms|
                          D2 %in% cough_terms|
                          D3 %in% cough_terms|
                          D4 %in% cough_terms, "Yes", NA),
         fever= ifelse(Complaint %in% fever_terms|
                         D1 %in% fever_terms|
                         D2 %in% fever_terms|
                         D3 %in% fever_terms|
                         D4 %in% fever_terms, "Yes", NA),
         fatigue = ifelse(Complaint %in% fatigue_terms|
                            D1 %in% fatigue_terms|
                            D2 %in% fatigue_terms|
                            D3 %in% fatigue_terms|
                            D4 %in% fatigue_terms, "Yes", NA),
         body.ache = ifelse(Complaint %in% body.ache_terms|
                              D1 %in% body.ache_terms|
                              D2 %in% body.ache_terms|
                              D3 %in% body.ache_terms|
                              D4 %in% body.ache_terms, "Yes", NA),
         headache = ifelse(Complaint %in% headache_terms|
                             D1 %in% headache_terms|
                             D2 %in% headache_terms|
                             D3 %in% headache_terms|
                             D4 %in% headache_terms, "Yes", NA),
         taste.smell = ifelse(Complaint %in% taste.smell_terms|
                                D1 %in% taste.smell_terms|
                                D2 %in% taste.smell_terms|
                                D3 %in% taste.smell_terms|
                                D4 %in% taste.smell_terms, "Yes", NA),
         sore.throat = ifelse(Complaint %in% sore.throat_terms|
                                D1 %in% sore.throat_terms|
                                D2 %in% sore.throat_terms|
                                D3 %in% sore.throat_terms|
                                D4 %in% sore.throat_terms, "Yes", NA),
         congestion.nose = ifelse(Complaint %in% congestion.nose_terms|
                                    D1 %in% congestion.nose_terms|
                                    D2 %in% congestion.nose_terms|
                                    D3 %in% congestion.nose_terms|
                                    D4 %in% congestion.nose_terms, "Yes", NA),
         nausea.vomit = ifelse(Complaint %in% nausea.vomit_terms|
                                 D1 %in% nausea.vomit_terms|
                                 D2 %in% nausea.vomit_terms|
                                 D3 %in% nausea.vomit_terms|
                                 D4 %in% nausea.vomit_terms, "Yes", NA),
         diarrhea = ifelse(Complaint %in% diarrhea_terms|
                             D1 %in% diarrhea_terms|
                             D2 %in% diarrhea_terms|
                             D3 %in% diarrhea_terms|
                             D4 %in% diarrhea_terms, "Yes", NA),
         sob = ifelse(Complaint %in% sob_terms|
                        D1 %in% sob_terms|
                        D2 %in% sob_terms|
                        D3 %in% sob_terms|
                        D4 %in% sob_terms, "Yes", NA),
         chest.pain = ifelse(Complaint %in% chest.pain_terms|
                               D1 %in% chest.pain_terms|
                               D2 %in% chest.pain_terms|
                               D3 %in% chest.pain_terms|
                               D4 %in% chest.pain_terms, "Yes", NA),
         confusion = ifelse(Complaint %in% confusion_terms|
                              D1 %in% confusion_terms|
                              D2 %in% confusion_terms|
                              D3 %in% confusion_terms|
                              D4 %in% confusion_terms, "Yes", NA))

#Removing symptoms lists
rm(body.ache_terms, chest.pain_terms, chills_terms, confusion_terms, cough_terms, diarrhea_terms, fatigue_terms, fever_terms, headache_terms, nausea.vomit_terms, sob_terms, sore.throat_terms,
   taste.smell_terms, congestion.nose_terms)

#Creating a variable for symptomatic case using CSTE case definition
#COVID case definition: At least two of the following symptoms: fever (measured or subjective), chills, rigors, myalgia, headache, sore throat, new olfactory and taste disorder(s)
#OR At least one of the following symptoms: cough, shortness of breath, or difficulty breathing

tmp <- ChiefComplaint %>%
  select(VisitID, chills:confusion) 

tmp %>%
  group_by(VisitID) %>%
  slice_tail() -> tmp


merged.data2.trunc  <- left_join(merged.data2.trunc, tmp, by="VisitID")



merged.data2.trunc %>%
  mutate(fever.final = ifelse(fever=="Yes" | Fever.vitalsigns==1, "Yes", "No"),
         symptomatic.case = ifelse((fever.final=="Yes" | chills=="Yes" | cough =="Yes" | sore.throat=="Yes" | fatigue=="Yes" | headache=="Yes"|
                                      confusion=="Yes" | taste.smell=="Yes" | sob=="Yes" | diarrhea=="Yes" | chest.pain=="Yes" | nausea.vomit=="Yes" |
                                      congestion.nose=="Yes" | body.ache=="Yes" | O2.vitalsigns==1), 1,0)) -> merged.data2.trunc

table(merged.data2.trunc$symptomatic.case) #196276

#Create variable for no. of visits per person
merged.data2.trunc %>%
  group_by(PatientID) %>%
  arrange(Test_date, .by_group=TRUE)%>%
  mutate(Visit = 1:n()) -> merged.data2.trunc




############ Demographic tables ###################
merged.data2.trunc$agecat = NA
merged.data2.trunc$agecat[merged.data2.trunc$Age<19] <- "12-18"
merged.data2.trunc$agecat[merged.data2.trunc$Age>=19 & merged.data2.trunc$Age<30] <- "19-29"
merged.data2.trunc$agecat[merged.data2.trunc$Age>=30 & merged.data2.trunc$Age<40] <- "30-39"
merged.data2.trunc$agecat[merged.data2.trunc$Age>=40 & merged.data2.trunc$Age<50] <- "40-49"
merged.data2.trunc$agecat[merged.data2.trunc$Age>=50 & merged.data2.trunc$Age<60] <- "50-59"
merged.data2.trunc$agecat[merged.data2.trunc$Age>=60 & merged.data2.trunc$Age<70] <- "60-69"
merged.data2.trunc$agecat[merged.data2.trunc$Age>=70 & merged.data2.trunc$Age<80] <- "70-79"
merged.data2.trunc$agecat[merged.data2.trunc$Age>=80] <- ">=80"

#rename merged.data2.trunc
vax.data <- merged.data2.trunc

library(boot)
library(table1)

###Prepare the long dataset for table 1

#ever covid positive
vax.data %>%
  group_by(PatientID)%>%
  mutate(ever.positive =  ifelse(labresult=="POSITIVE", 1,0)) -> vax.data
vax.data<- as.data.table(vax.data)
vax.data[ever.positive==1, n_distinct(PatientID)] #62210

#ever symptomatic
vax.data %>%
  group_by(PatientID)%>%
  mutate(ever.symptomatic =  ifelse(symptomatic.case==1, 1,0)) -> vax.data

vax.data %>%
  group_by(PatientID)%>%
  mutate(comorbidity =  ifelse((!is.na(category_1) | !is.na(category_2 )| !is.na(category_3) | !is.na(category_4) |
                                  !is.na(category_5) | !is.na(category_6) | !is.na(category_7) |
                                  !is.na(category_8) | !is.na(category_9) | !is.na(category_10)), 1, 0)) -> vax.data

table(vax.data$comorbidity)

#ever covid positive
merged.data2.trunc %>%
  group_by(PatientID)%>%
  mutate(ever.positive =  ifelse(labresult=="POSITIVE", 1,0)) -> merged.data2.trunc
merged.data2.trunc<- as.data.table(merged.data2.trunc)
merged.data2.trunc[ever.positive==1, n_distinct(PatientID)] #62210

#ever symptomatic
merged.data2.trunc %>%
  group_by(PatientID)%>%
  mutate(ever.symptomatic =  ifelse(symptomatic.case==1, 1,0)) -> merged.data2.trunc

merged.data2.trunc %>%
  group_by(PatientID)%>%
  mutate(comorbidity =  ifelse((!is.na(category_1) | !is.na(category_2 )| !is.na(category_3) | !is.na(category_4) |
                                  !is.na(category_5) | !is.na(category_6) | !is.na(category_7) |
                                  !is.na(category_8) | !is.na(category_9) | !is.na(category_10)), 1, 0)) -> merged.data2.trunc

table(merged.data2.trunc$comorbidity)

#pick last row by ID to create table 1
merged.data2.trunc %>%
  group_by(PatientID) %>%
  slice_tail() -> vax.data.tbl1

#partial vaccinated status
table(vax.data.tbl1$final_fully_vax, useNA="ifany")
table(vax.data.tbl1$final_vaxstatus, useNA="ifany")

vax.data.tbl1 %>%
  group_by(PatientID)%>%
  mutate(final_fully_vax.cat = ifelse(final_vaxstatus=="Yes" & final_fully_vax=="Yes", "Fully",
                                      ifelse(final_vaxstatus=="Yes" & (final_fully_vax=="No" | is.na(final_fully_vax)), "Partial",
                                             ifelse(final_vaxstatus=="No" & (final_fully_vax=="No" | is.na(final_fully_vax) | final_fully_vax=="Yes") , "Unvaccinated", NA_character_)))) -> vax.data.tbl1

table(vax.data.tbl1$final_fully_vax.cat, useNA="ifany")

#123491  are NA but have at least one vaccine so recategorize as partial

#Some of the partially vaccinated could be J&J vaccines 
vax.data.tbl1$final_fully_vax.cat[is.na(vax.data.tbl1$final_fully_vax.cat) & vax.data.tbl1$vaxname=="Johnson & Johnson"] <- "Fully"

vax.data.tbl1$final_fully_vax.cat[is.na(vax.data.tbl1$final_fully_vax.cat)] <- "Partial"


vax.data.tbl1$final_vaxstatus <- 
  factor(vax.data.tbl1$final_vaxstatus, 
         levels=c("Yes","No"),
         labels=c("Vaccinated", 
                  "Unvaccinated"))
vax.data.tbl1$final_fully_vax.cat <- 
  factor(vax.data.tbl1$final_fully_vax.cat, 
         levels=c("Fully", "Partial", "Unvaccinated"),
         labels=c("Fully", "Partial", "Unvaccinated"))

vax.data.tbl1$ever.positive <- 
  factor(vax.data.tbl1$ever.positive, 
         levels=c(1,0),
         labels=c("Test Positive", 
                  "Test Negative"))

vax.data.tbl1$agecat <-
  factor(vax.data.tbl1$agecat,
         levels = c("12-18", "19-29","30-39","40-49","50-59", "60-69","70-79",">=80"),
         labels = c("12-18", "19-29","30-39","40-49","50-59", "60-69","70-79",">=80"))


vax.data.tbl1$Gender <-
  factor(vax.data.tbl1$Gender,
         levels = c("M","F","U"),
         labels = c("Male","Female","Unknown"))

vax.data.tbl1$racecat <-
  factor(vax.data.tbl1$racecat,
         levels = c("White", "Black/AfrAm", "Hispanic", "Native American/Alaskan Native/Pacific Islander", "Asian", "Other/Unknown"),
         labels = c("White NH","Black NH","Hispanic", "Nat Am./Pac Is./Al Nat.","Asian","Other/Unknown"))

vax.data.tbl1$Region <-
  factor(vax.data.tbl1$Region,
         levels = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island", "Long Island", "Metro North"),
         labels = c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island", "Long Island", "Westchester"))

vax.data.tbl1$ever.positive <-
  factor(vax.data.tbl1$ever.positive,
         levels = c("Test Positive", "Test Negative"),
         labels = c("Test Positive", "Test Negative"))

vax.data.tbl1$ever.symptomatic <-
  factor(vax.data.tbl1$ever.symptomatic,
         levels = c(1),
         labels = c("Symptomatic"))

vax.data.tbl1$prev.inf <-
  factor(vax.data.tbl1$prev.inf,
         levels = c("Yes","No"),
         labels = c("Yes","No"))


vax.data.tbl1$Exp.risk <-
  factor(vax.data.tbl1$Exp.risk,
         levels = c("0","1",">=2"),
         labels = c("0","1",">=2"))


vax.data.tbl1$`Rapid Order Patient Symptomatic` <- 
  factor(vax.data.tbl1$`Rapid Order Patient Symptomatic`,
         levels = c("Yes","No"),
         labels = c("Yes","No"))

vax.data.tbl1$comorbidity <- 
  factor(vax.data.tbl1$comorbidity,
         levels = c(1,0),
         labels = c("Yes", "No"))

vax.data.tbl1$BMI_cat <- 
  factor(vax.data.tbl1$BMI_cat,
         levels = c("Healthy","Obese",  "Overweight", "Underweight"),
         labels = c("Healthy","Obese",  "Overweight", "Underweight"))

label(vax.data.tbl1$agecat) <- "Age"
label(vax.data.tbl1$Gender) <- "Sex"
label(vax.data.tbl1$racecat) <- "Race/Ethnicity"
label(vax.data.tbl1$Region) <- "Region"
label(vax.data.tbl1$ever.positive) <- "COVID-19 positive"
label(vax.data.tbl1$ever.symptomatic) <- "Symptomatic at testing (reported)"
label(vax.data.tbl1$`Rapid Order Patient Symptomatic`) <- "Symptomatic at rapid testing"
label(vax.data.tbl1$prev.inf) <- "Previous COVID infection"
label(vax.data.tbl1$Exp.risk) <- "High exposure to COVID"
label(vax.data.tbl1$comorbidity) <- "Comorbidities"
label(vax.data.tbl1$final_vaxstatus) <- "COVID-19 vaccination status"
label(vax.data.tbl1$BMI_cat) <- "BMI"

pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- kruskal.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}


table1(~ agecat+ Gender + racecat + Region + ever.positive +
         + ever.symptomatic + prev.inf + Exp.risk + `Rapid Order Patient Symptomatic` + comorbidity + BMI_cat + vaxname| final_fully_vax.cat , 
       overall=F, extra.col=list(`P-value`=pvalue),data=vax.data.tbl1)


chisq.test(vax.data.tbl1$final_vaxstatus, vax.data.tbl1$prev.inf)


table1(~ agecat+ Gender +  Region + final_fully_vax.cat +racecat
       + ever.symptomatic + prev.inf + Exp.risk + `Rapid Order Patient Symptomatic` + comorbidity + BMI_cat + vaxname| ever.positive , 
       overall=F, extra.col=list(`P-value`=pvalue),data=vax.data.tbl1)

#Missing vaccine data 
Missing_vax$agecat = NA
Missing_vax$agecat[Missing_vax$Age<19] <- "12-18"
Missing_vax$agecat[Missing_vax$Age>=19 & Missing_vax$Age<30] <- "19-29"
Missing_vax$agecat[Missing_vax$Age>=30 & Missing_vax$Age<40] <- "30-39"
Missing_vax$agecat[Missing_vax$Age>=40 & Missing_vax$Age<50] <- "40-49"
Missing_vax$agecat[Missing_vax$Age>=50 & Missing_vax$Age<60] <- "50-59"
Missing_vax$agecat[Missing_vax$Age>=60 & Missing_vax$Age<70] <- "60-69"
Missing_vax$agecat[Missing_vax$Age>=70 & Missing_vax$Age<80] <- "70-79"
Missing_vax$agecat[Missing_vax$Age>=80] <- ">=80"


Missing_vax %>%
  group_by(PatientID)%>%
  mutate(ever.positive =  ifelse(labresult=="POSITIVE", 1,0)) -> Missing_vax
Missing_vax<- as.data.table(Missing_vax)

#ever symptomatic
Missing_vax %>%
  group_by(PatientID)%>%
  mutate(ever.symptomatic =  ifelse(symptomatic.case==1, 1,0)) -> Missing_vax

Missing_vax %>%
  group_by(PatientID)%>%
  mutate(comorbidity =  ifelse((!is.na(category_1) | !is.na(category_2 )| !is.na(category_3) | !is.na(category_4) |
                                  !is.na(category_5) | !is.na(category_6) | !is.na(category_7) |
                                  !is.na(category_8) | !is.na(category_9) | !is.na(category_10)), 1, 0)) -> Missing_vax


table1(~ agecat+ Gender +  Region + racecat
       + ever.symptomatic + prev.inf + Exp.risk + `Rapid Order Patient Symptomatic` + comorbidity + BMI_cat  , 
       overall=F, extra.col=list(`P-value`=pvalue),data=Missing_vax)

vax.data.tbl1$final_fully_vax[is.na(vax.data.tbl1$final_fully_vax)] <- "NO"

###UHF data
NYC_UHF_demog$UHF <-toupper(NYC_UHF_demog$UHF)

#merge some smaller UHFs with larger UHFs
vax.data$UHF[vax.data$UHF=="RICHMOND HILL - WOODHAVEN" | vax.data$UHF=="HOWARD BEACH - OZONE PARK"] <- "SOUTHWEST QUEENS"
vax.data$UHF[vax.data$UHF=="HAMILTON HEIGHTS - MANHATTANVILLE - WEST HARLEM"] <- "WASHINGTON HEIGHTS - INWOOD"
vax.data$UHF[vax.data$UHF=="SUNNYSIDE - WOODSIDE"] <- "WEST QUEENS"

#rename UHFs to match
vax.data$UHF[vax.data$UHF=="DOWNTOWN - HEIGHTS - PARK SLOPE"] <- "DOWNTOWN  - HEIGHTS - SLOPE"

#merge UHF data with vax.data
vax.data<- left_join(vax.data, NYC_UHF_demog, by="UHF")


##Separate out J&J vaccine 
JandJ <- merged.data2.trunc %>%
  filter(vaxname=="Johnson & Johnson" | is.na(vaxname))

vax.data %>%
  filter(vaxname!= "Johnson & Johnson" | is.na(vaxname)) -> vax.data #1596864

##partial vs. fully vax for 2 dose mRNA vaccines
vax.data$vaxstatus1 = NA
vax.data$vaxstatus1[(vax.data$vaxstatus=="Yes" & vax.data$fully_vax=="Yes")] <- "Fully"
vax.data$vaxstatus1[vax.data$vaxstatus=="Yes" & (vax.data$fully_vax=="No" | is.na(vax.data$fully_vax))] <- "Partially"
vax.data$vaxstatus1[vax.data$vaxstatus=="No"] <- "Unvaccinated"

table(vax.data$vaxstatus1, useNA = "ifany")

vax.data<- as.data.table(vax.data)
vax.data %>%
  group_by(vaxstatus1, labresult) %>%
  summarise(n=n_distinct(PatientID))


vax.data.tbl1<- as.data.table(vax.data.tbl1)
vax.data.tbl1 %>%
  group_by(vaxstatus1, labresult) %>%
  summarise(n=n_distinct(PatientID))

##### GLM models #########
#Model 1 
vax.data$vaxstatus1 <- relevel(factor(vax.data$vaxstatus1), ref="Unvaccinated")

model1 <- glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & biweekly.period ==1,], family = binomial(link="logit"))
summary(model1)
1-exp(coef(model1))
1-exp(confint(model1))


model1.1 <- glm(factor(labresult) ~ factor(vaxstatus1) + factor(agecat) + factor(Gender) + factor(Region) + factor(Exp.risk)
                + factor(comorbidity) + factor(racecat) + factor(BMI_cat), data=vax.data[prev.inf=="No" & biweekly.period ==1,], family = binomial(link="logit"))
summary(model1.1)
1-exp(coef(model1.1))
1-exp(confint.default(model1.1))


model2 <- glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & biweekly.period == 2,], family = binomial(link="logit"))
summary(model2)
1-exp(coef(model2))
1-exp(confint(model2))
beep()

model2.1 <- glm(factor(labresult) ~ factor(vaxstatus1) + factor(agecat) + factor(Gender) + factor(Region) + factor(Exp.risk)
                + factor(comorbidity) + factor(racecat) + factor(BMI_cat), data=vax.data[prev.inf=="No" & biweekly.period ==2,], family = binomial(link="logit"))
summary(model2.1)
1-exp(coef(model2.1))
1-exp(confint.default(model2.1))
beep()

model3 <- glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & biweekly.period==3,], family = binomial(link="logit"))
summary(model3)
1-exp(coef(model3))
1-exp(confint(model3))
beep()

model3.1 <- glm(factor(labresult) ~ factor(vaxstatus1) + factor(vaxstatus1) + factor(agecat) + factor(Gender) + factor(Region) + factor(Exp.risk)
                + factor(comorbidity) + factor(racecat) + factor(BMI_cat), data=vax.data[prev.inf=="No" & biweekly.period==3,], family = binomial(link="logit"))
summary(model3.1)
1-exp(coef(model3.1))
1-exp(confint.default(model3.1))


model4 <- glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & biweekly.period==4,], family = binomial(link="logit"))
summary(model4)
1-exp(coef(model4))
1-exp(confint(model4))

model4.1<- glm(factor(labresult) ~ factor(vaxstatus1) + factor(agecat) + factor(Gender) + factor(Region) + factor(Exp.risk) + factor(comorbidity)
               + factor(racecat)+ factor(BMI_cat), data=vax.data[prev.inf=="No" & biweekly.period==4,], family = binomial(link="logit"))
summary(model4.1)
1-exp(coef(model4.1))
1-exp(confint.default(model4.1))


model5 <- glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & biweekly.period==5,], family = binomial(link="logit"))
summary(model5)
1-exp(coef(model5))
1-exp(confint(model5))

model5.1 <- glm(factor(labresult) ~ factor(vaxstatus1) + factor(agecat) + factor(Gender) + factor(Region) + factor(Exp.risk) + factor(comorbidity)
                + factor(racecat) + factor(BMI_cat), data=vax.data[prev.inf=="No" & biweekly.period==5,], family = binomial(link="logit"))
summary(model5.1)
1-exp(coef(model5.1))
1-exp(confint.default(model5.1))


model6 <- glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & biweekly.period==6,], family = binomial(link="logit"))
summary(model6)
1-exp(coef(model6))
1-exp(confint(model6))

model6.1 <- glm(factor(labresult) ~ factor(vaxstatus1)  + factor(agecat) + factor(Gender) + factor(Region) + factor(Exp.risk) + factor(comorbidity)
                + factor(racecat) + factor(BMI_cat), data=vax.data[prev.inf=="No" & biweekly.period==6,], family = binomial(link="logit"))
summary(model6.1)
1-exp(coef(model6.1))
1-exp(confint.default(model6.1))

model7 <- glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & biweekly.period==7,], family = binomial(link="logit"))
summary(model7)
1-exp(coef(model7))
1-exp(confint(model7))

model7.1 <- glm(factor(labresult) ~ factor(vaxstatus1) + factor(agecat) + factor(Gender) + factor(Region) + factor(Exp.risk) + factor(comorbidity)
                + factor(racecat) + factor(BMI_cat), data=vax.data[prev.inf=="No" & biweekly.period==7,], family = binomial(link="logit"))
summary(model7.1)
1-exp(coef(model7.1))
1-exp(confint.default(model7.1))

model8 <- glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & biweekly.period==8,], family = binomial(link="logit"))
summary(model8)
1-exp(coef(model8))
1-exp(confint(model8))


model8.1 <- glm(factor(labresult) ~ factor(vaxstatus1) + factor(agecat) + factor(Gender) + factor(Region) + factor(Exp.risk) + factor(comorbidity)
                + factor(racecat) + factor(BMI_cat), data=vax.data[prev.inf=="No" & biweekly.period==8 ,], family = binomial(link="logit"))
summary(model8.1)
1-exp(coef(model8.1))
1-exp(confint.default(model8.1))



model9 <- glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & biweekly.period==9,], family = binomial(link="logit"))
summary(model9)
1-exp(coef(model9))
1-exp(confint(model9))


model9.1 <- glm(factor(labresult) ~ factor(vaxstatus1) + factor(agecat) + factor(Gender) + factor(Region) + factor(Exp.risk) + factor(comorbidity) +
                  factor(racecat) + factor(BMI_cat), data=vax.data[prev.inf=="No" & biweekly.period==9,], family = binomial(link="logit"))
summary(model9.1)
1-exp(coef(model9.1))
1-exp(confint.default(model9.1))


model10 <- glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & biweekly.period==10,], family = binomial(link="logit"))
summary(model10)
1-exp(coef(model10))
1-exp(confint.default(model10))


model10.1 <- glm(factor(labresult) ~ factor(vaxstatus1) + factor(agecat) + factor(Gender) + factor(Region) + factor(Exp.risk) + factor(comorbidity) +
                   factor(racecat) + factor(BMI_cat)
                , data=vax.data[prev.inf=="No" & biweekly.period==10,], family = binomial(link="logit"))
summary(model10.1)
1-exp(coef(model10.1))
1-exp(confint.default(model10.1))

model11 <- glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & biweekly.period==11,], family = binomial(link="logit"))
summary(model11)
1-exp(coef(model11))
1-exp(confint.default(model11))


model11.1 <- glm(factor(labresult) ~ factor(vaxstatus1) + factor(agecat) + factor(Gender) + factor(Region) + factor(Exp.risk) + factor(comorbidity) +
                   factor(racecat) + factor(BMI_cat)
                 , data=vax.data[prev.inf=="No" & biweekly.period==11,], family = binomial(link="logit"))
summary(model11.1)
1-exp(coef(model11.1))
1-exp(confint.default(model11.1))

model12 <- glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & biweekly.period==12,], family = binomial(link="logit"))
summary(model12)
1-exp(coef(model12))
1-exp(confint.default(model12))

model12.1 <- glm(factor(labresult) ~ factor(vaxstatus1) + factor(agecat) + factor(Gender) + factor(Region) + factor(Exp.risk) + factor(comorbidity) +
                   factor(racecat) + factor(BMI_cat)
                 , data=vax.data[prev.inf=="No" & biweekly.period==12,], family = binomial(link="logit"))
summary(model12.1 )
1-exp(coef(model12.1 ))
1-exp(confint.default(model12.1 ))


model13 <- glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & biweekly.period==13,], family = binomial(link="logit"))
summary(model13)
1-exp(coef(model13))
1-exp(confint.default(model13))

model13.1 <- glm(factor(labresult) ~ factor(vaxstatus1) + factor(agecat) + factor(Gender) + factor(Region) + factor(Exp.risk) + factor(comorbidity) +
                   factor(racecat) + factor(BMI_cat)
                 , data=vax.data[prev.inf=="No" & biweekly.period==13,], family = binomial(link="logit"))
summary(model13.1)
1-exp(coef(model13.1))
1-exp(confint.default(model13.1))

model14 <- glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & biweekly.period==14,], family = binomial(link="logit"))
summary(model14)
1-exp(coef(model14))
1-exp(confint(model14))

model14.1 <- glm(factor(labresult) ~ factor(vaxstatus1) + factor(agecat) + factor(Gender) + factor(Region) + factor(Exp.risk) + factor(comorbidity) +
                   factor(racecat) + factor(BMI_cat)
                 , data=vax.data[prev.inf=="No" & biweekly.period==14,], family = binomial(link="logit"))
summary(model14.1)
1-exp(coef(model14.1))
1-exp(confint.default(model14.1))

model15 <- glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & biweekly.period==15,], family = binomial(link="logit"))
summary(model15)
1-exp(coef(model15))
1-exp(confint(model15))

model15.1 <- glm(factor(labresult) ~ factor(vaxstatus1) + factor(agecat) + factor(Gender) + factor(Region) + factor(Exp.risk) +  factor(comorbidity) +
                   factor(racecat) + factor(BMI_cat) 
                 , data=vax.data[prev.inf=="No" & biweekly.period==15,], family = binomial(link="logit"))
summary(model15.1)
1-exp(coef(model15.1))
1-exp(confint.default(model15.1))

vax.data[prev.inf=="No" & biweekly.period==15, n_distinct(PatientID)]

vax.data[prev.inf=="No" & biweekly.period==15 & labresult=="POSITIVE" & vaxstatus1 == "Fully", n_distinct(PatientID)]
vax.data[prev.inf=="No" & biweekly.period==15 & labresult=="POSITIVE" & vaxstatus1 == "Unvaccinated", n_distinct(PatientID)]
vax.data[prev.inf=="No" & biweekly.period==15 & labresult=="POSITIVE" & vaxstatus1 == "Partially", n_distinct(PatientID)]

vax.data[prev.inf=="No" & biweekly.period==15 & labresult=="NEGATIVE" & vaxstatus1 == "Fully", n_distinct(PatientID)]
vax.data[prev.inf=="No" & biweekly.period==15 & labresult=="NEGATIVE" & vaxstatus1 == "Unvaccinated", n_distinct(PatientID)]
vax.data[prev.inf=="No" & biweekly.period==15 & labresult=="NEGATIVE" & vaxstatus1 == "Partially", n_distinct(PatientID)]


##Model set: Pre. vs. post delta 

vax.data[prev.inf=="No" & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Unvaccinated",n_distinct(PatientID)]
vax.data[prev.inf=="No" & biweekly.period %in% c(1,2,3,4,5) & labresult=="POSITIVE" & vaxstatus1=="Unvaccinated",n_distinct(PatientID)]

vax.data[prev.inf=="No" & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Partially",n_distinct(PatientID)]
vax.data[prev.inf=="No" & biweekly.period %in% c(1,2,3,4,5) & labresult=="POSITIVE" & vaxstatus1=="Partially",n_distinct(PatientID)]

vax.data[prev.inf=="No" & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Fully",n_distinct(PatientID)]
vax.data[prev.inf=="No" & biweekly.period %in% c(1,2,3,4,5) & labresult=="POSITIVE" & vaxstatus1=="Fully",n_distinct(PatientID)]

vax.data[prev.inf=="No" & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Unvaccinated",n_distinct(PatientID)]
vax.data[prev.inf=="No" & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & labresult=="POSITIVE" & vaxstatus1=="Unvaccinated",n_distinct(PatientID)]

vax.data[prev.inf=="No" & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Partially",n_distinct(PatientID)]
vax.data[prev.inf=="No" & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & labresult=="POSITIVE" & vaxstatus1=="Partially",n_distinct(PatientID)]

vax.data[prev.inf=="No" & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Fully",n_distinct(PatientID)]
vax.data[prev.inf=="No" & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & labresult=="POSITIVE" & vaxstatus1=="Fully",n_distinct(PatientID)]


model.predelta <- glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.predelta)
1-exp(coef(model.predelta))
1-exp(confint.default(model.predelta))


model.predelta.1 <- glm(factor(labresult) ~ factor(vaxstatus1) + factor(agecat) + factor(Gender) + factor(Region) + factor(Exp.risk) +  factor(comorbidity) +
                           factor(racecat) + factor(BMI_cat) 
                         , data=vax.data[prev.inf=="No" & biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.predelta.1)
1-exp(coef(model.predelta.1))
1-exp(confint.default(model.predelta.1))


model.postdelta <- glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.postdelta)
1-exp(coef(model.postdelta))
1-exp(confint.default(model.postdelta))

model.postdelta.1 <- glm(factor(labresult) ~ factor(vaxstatus1) + factor(agecat) + factor(Gender) + factor(Region) + factor(Exp.risk) +  factor(comorbidity) +
                   factor(racecat) + factor(BMI_cat) 
                 , data=vax.data[prev.inf=="No" & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.postdelta.1)
1-exp(coef(model.postdelta.1))
1-exp(confint.default(model.postdelta.1))




##Model set: symptomatic vs. asymptomatic
vax.data %>%
  mutate(fever.final = ifelse(fever=="Yes" | Fever.vitalsigns==1, "Yes", "No"),
         symptomatic.case = ifelse((fever.final=="Yes" | chills=="Yes" | cough =="Yes" | sore.throat=="Yes" | fatigue=="Yes" | headache=="Yes"|
                                      confusion=="Yes" | taste.smell=="Yes" | sob=="Yes" | diarrhea=="Yes" | chest.pain=="Yes" | nausea.vomit=="Yes" |
                                      congestion.nose=="Yes" | body.ache=="Yes" | O2.vitalsigns==1), 1,0)) -> vax.data

vax.data$symptomatic.case[is.na(vax.data$symptomatic.case)] <- 0

vax.data[prev.inf=="No" & symptomatic.case==1 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Unvaccinated", n_distinct(PatientID)] 
vax.data[prev.inf=="No" & symptomatic.case==1 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE",n_distinct(PatientID)] 

vax.data[prev.inf=="No" & symptomatic.case==1 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Partially", n_distinct(PatientID)] 
vax.data[prev.inf=="No" & symptomatic.case==1 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Partially" & labresult=="POSITIVE", n_distinct(PatientID)] 

vax.data[prev.inf=="No" & symptomatic.case==1 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Fully", n_distinct(PatientID)] 
vax.data[prev.inf=="No" & symptomatic.case==1 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Fully" & labresult=="POSITIVE", n_distinct(PatientID)] 

vax.data[prev.inf=="No" & symptomatic.case==1 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Unvaccinated", n_distinct(PatientID)] 
vax.data[prev.inf=="No" & symptomatic.case==1 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE",n_distinct(PatientID)] 

vax.data[prev.inf=="No" & symptomatic.case==1 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Partially", n_distinct(PatientID)] 
vax.data[prev.inf=="No" & symptomatic.case==1 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Partially" & labresult=="POSITIVE", n_distinct(PatientID)] 

vax.data[prev.inf=="No" & symptomatic.case==1 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Fully", n_distinct(PatientID)] 
vax.data[prev.inf=="No" & symptomatic.case==1 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Fully" & labresult=="POSITIVE", n_distinct(PatientID)] 

vax.data[prev.inf=="No" & symptomatic.case==0 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Unvaccinated", n_distinct(PatientID)] 
vax.data[prev.inf=="No" & symptomatic.case==0 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE",n_distinct(PatientID)] 

vax.data[prev.inf=="No" & symptomatic.case==0 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Partially", n_distinct(PatientID)] 
vax.data[prev.inf=="No" & symptomatic.case==0 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Partially" & labresult=="POSITIVE", n_distinct(PatientID)] 

vax.data[prev.inf=="No" & symptomatic.case==0 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Fully", n_distinct(PatientID)] 
vax.data[prev.inf=="No" & symptomatic.case==0 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Fully" & labresult=="POSITIVE", n_distinct(PatientID)] 

vax.data[prev.inf=="No" & symptomatic.case==0 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Unvaccinated", n_distinct(PatientID)] 
vax.data[prev.inf=="No" & symptomatic.case==0 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE",n_distinct(PatientID)] 

vax.data[prev.inf=="No" & symptomatic.case==0 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Partially", n_distinct(PatientID)] 
vax.data[prev.inf=="No" & symptomatic.case==0 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Partially" & labresult=="POSITIVE", n_distinct(PatientID)] 

vax.data[prev.inf=="No" & symptomatic.case==0 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Fully", n_distinct(PatientID)] 
vax.data[prev.inf=="No" & symptomatic.case==0 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Fully" & labresult=="POSITIVE", n_distinct(PatientID)] 



model.symp <- glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & symptomatic.case==1 & biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.symp)
1-exp(coef(model.symp))
1-exp(confint.default(model.symp))


model.symp.1 <- glm(factor(labresult) ~  factor(vaxstatus1) + factor(agecat) + factor(Gender) + factor(Region) + factor(Exp.risk)
                    + factor(racecat) + factor(comorbidity) + factor(biweekly.period) + factor(BMI_cat), data=vax.data[prev.inf=="No" & symptomatic.case==1  & biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.symp.1)
1-exp(coef(model.symp.1))
1-exp(confint.default(model.symp.1))
beep()


model.asymp <- glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & symptomatic.case==0  & biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.asymp)
1-exp(coef(model.asymp))
1-exp(confint.default(model.asymp))


model.asymp.1 <- glm(factor(labresult) ~  factor(vaxstatus1) + factor(agecat) + factor(Gender) + factor(Region) + factor(Exp.risk)
                    + factor(racecat) + factor(comorbidity) + factor(biweekly.period) + factor(BMI_cat), data=vax.data[prev.inf=="No" & symptomatic.case==0 & biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.asymp.1)
1-exp(coef(model.asymp.1))
1-exp(confint.default(model.asymp.1))
beep()

model.symp.delta <- glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & symptomatic.case==1 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.symp.delta)
1-exp(coef(model.symp.delta))
1-exp(confint.default(model.symp.delta))
beep()

model.symp.1.delta <- glm(factor(labresult) ~  factor(vaxstatus1) + factor(agecat) + factor(Gender) + factor(Region) + factor(Exp.risk)
                    + factor(racecat) + factor(comorbidity) + factor(biweekly.period) + factor(BMI_cat), data=vax.data[prev.inf=="No" & symptomatic.case==1  & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.symp.1.delta)
1-exp(coef(model.symp.1.delta))
1-exp(confint.default(model.symp.1.delta))
beep()

model.asymp.delta <- glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & symptomatic.case==0  & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.asymp.delta )
1-exp(coef(model.asymp.delta))
1-exp(confint.default(model.asymp.delta))
beep()


model.asymp.1.delta <- glm(factor(labresult) ~  factor(vaxstatus1) + factor(agecat) + factor(Gender) + factor(Region) + factor(Exp.risk)
                     + factor(racecat) + factor(comorbidity) + factor(biweekly.period) + factor(BMI_cat), data=vax.data[prev.inf=="No" & symptomatic.case==0 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.asymp.1.delta)
1-exp(coef(model.asymp.1.delta))
1-exp(confint.default(model.asymp.1.delta))
beep()



##By sex
vax.data[prev.inf=="No" & Gender=="M" &  biweekly.period %in% c(1,2,3,4,5) & vaxstatus1 =="Unvaccinated",n_distinct(PatientID)]
vax.data[prev.inf=="No" & Gender=="M" &  biweekly.period %in% c(1,2,3,4,5) & vaxstatus1 =="Unvaccinated" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[prev.inf=="No" & Gender=="M" &  biweekly.period %in% c(1,2,3,4,5) & vaxstatus1 =="Partially",n_distinct(PatientID)]
vax.data[prev.inf=="No" & Gender=="M" &  biweekly.period %in% c(1,2,3,4,5) & vaxstatus1 =="Partially" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[prev.inf=="No" & Gender=="M" &  biweekly.period %in% c(1,2,3,4,5) & vaxstatus1 =="Fully",n_distinct(PatientID)]
vax.data[prev.inf=="No" & Gender=="M" &  biweekly.period %in% c(1,2,3,4,5) & vaxstatus1 =="Fully" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[prev.inf=="No" & Gender=="F" &  biweekly.period %in% c(1,2,3,4,5) & vaxstatus1 =="Unvaccinated",n_distinct(PatientID)]
vax.data[prev.inf=="No" & Gender=="F" &  biweekly.period %in% c(1,2,3,4,5) & vaxstatus1 =="Unvaccinated" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[prev.inf=="No" & Gender=="F" &  biweekly.period %in% c(1,2,3,4,5) & vaxstatus1 =="Partially",n_distinct(PatientID)]
vax.data[prev.inf=="No" & Gender=="F" &  biweekly.period %in% c(1,2,3,4,5) & vaxstatus1 =="Partially" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[prev.inf=="No" & Gender=="F" &  biweekly.period %in% c(1,2,3,4,5) & vaxstatus1 =="Fully",n_distinct(PatientID)]
vax.data[prev.inf=="No" & Gender=="F" &  biweekly.period %in% c(1,2,3,4,5) & vaxstatus1 =="Fully" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[prev.inf=="No" & Gender=="M" &  biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1 =="Unvaccinated",n_distinct(PatientID)]
vax.data[prev.inf=="No" & Gender=="M" &  biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1 =="Unvaccinated" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[prev.inf=="No" & Gender=="M" &  biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1 =="Partially",n_distinct(PatientID)]
vax.data[prev.inf=="No" & Gender=="M" &  biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1 =="Partially" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[prev.inf=="No" & Gender=="M" &  biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1 =="Fully",n_distinct(PatientID)]
vax.data[prev.inf=="No" & Gender=="M" &  biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1 =="Fully" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[prev.inf=="No" & Gender=="F" &  biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1 =="Unvaccinated",n_distinct(PatientID)]
vax.data[prev.inf=="No" & Gender=="F" &  biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1 =="Unvaccinated" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[prev.inf=="No" & Gender=="F" &  biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1 =="Partially",n_distinct(PatientID)]
vax.data[prev.inf=="No" & Gender=="F" &  biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1 =="Partially" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[prev.inf=="No" & Gender=="F" &  biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1 =="Fully",n_distinct(PatientID)]
vax.data[prev.inf=="No" & Gender=="F" &  biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1 =="Fully" & labresult=="POSITIVE",n_distinct(PatientID)]



model.male <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & Gender=="M" &  biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.male)
1-exp(coef(model.male))
1-exp(confint.default(model.male))
beep()

model.male.1 <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(agecat)  + factor(Region) + factor(Exp.risk)
                     + factor(racecat) + factor(comorbidity) + factor(biweekly.period) + factor(BMI_cat), data=vax.data[prev.inf=="No" & Gender=="M" & biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.male.1)
1-exp(coef(model.male.1))
1-exp(confint.default(model.male.1))
beep()

model.female <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & Gender=="F" & biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.female)
1-exp(coef(model.female))
1-exp(confint(model.female))

model.female.1 <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(agecat)  + factor(Region) + factor(Exp.risk)
                     + factor(racecat) + factor(comorbidity) + factor(biweekly.period) + factor(BMI_cat), data=vax.data[prev.inf=="No" & Gender=="F" & biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.female.1)
1-exp(coef(model.female.1))
1-exp(confint.default(model.female.1))
beep()


model.male.delta <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & Gender=="M" &  biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.male.delta)
1-exp(coef(model.male.delta))
1-exp(confint.default(model.male.delta))
beep()

model.male.1.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(agecat)  + factor(Region) + factor(Exp.risk)
                     + factor(racecat) + factor(comorbidity) + factor(biweekly.period) + factor(BMI_cat), data=vax.data[prev.inf=="No" & Gender=="M" & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.male.1.delta)
1-exp(coef(model.male.1.delta))
1-exp(confint.default(model.male.1.delta))
beep()

model.female.delta <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & Gender=="F" & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.female.delta)
1-exp(coef(model.female.delta))
1-exp(confint.default(model.female.delta))
beep()

model.female.1.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(agecat)  + factor(Region) + factor(Exp.risk)
                       + factor(racecat) + factor(comorbidity) + factor(biweekly.period) + factor(BMI_cat), data=vax.data[prev.inf=="No" & Gender=="F" & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.female.1.delta)
1-exp(coef(model.female.1.delta))
1-exp(confint.default(model.female.1.delta))
beep()



#By age groups
vax.data[prev.inf=="No" & Age>=12 & Age<=15 & Test_date>="2021-06-15" & vaxstatus1=="Unvaccinated", n_distinct(PatientID)]
vax.data[prev.inf=="No" & Age>=12 & Age<=15 & Test_date>="2021-06-15" & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE", n_distinct(PatientID)]

vax.data[prev.inf=="No" & Age>=12 & Age<=15 & Test_date>="2021-06-15" & vaxstatus1=="Partially", n_distinct(PatientID)]
vax.data[prev.inf=="No" & Age>=12 & Age<=15 & Test_date>="2021-06-15" & vaxstatus1=="Partially" & labresult=="POSITIVE", n_distinct(PatientID)]

vax.data[prev.inf=="No" & Age>=12 & Age<=15 & Test_date>="2021-06-15" & vaxstatus1=="Fully", n_distinct(PatientID)]
vax.data[prev.inf=="No" & Age>=12 & Age<=15 & Test_date>="2021-06-15" & vaxstatus1=="Fully" & labresult=="POSITIVE", n_distinct(PatientID)]

vax.data[prev.inf=="No" & Age>=16 & Age<=30 & biweekly.period %in% c(3,4,5) & vaxstatus1=="Unvaccinated", n_distinct(PatientID)]
vax.data[prev.inf=="No" & Age>=16 & Age<=30 & biweekly.period %in% c(3,4,5) & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE", n_distinct(PatientID)]

vax.data[prev.inf=="No" & Age>=16 & Age<=30 & biweekly.period %in% c(3,4,5) & vaxstatus1=="Partially", n_distinct(PatientID)]
vax.data[prev.inf=="No" & Age>=16 & Age<=30 & biweekly.period %in% c(3,4,5) & vaxstatus1=="Partially" & labresult=="POSITIVE", n_distinct(PatientID)]

vax.data[prev.inf=="No" & Age>=16 & Age<=30 & biweekly.period %in% c(3,4,5) & vaxstatus1=="Fully", n_distinct(PatientID)]
vax.data[prev.inf=="No" & Age>=16 & Age<=30 & biweekly.period %in% c(3,4,5) & vaxstatus1=="Fully" & labresult=="POSITIVE", n_distinct(PatientID)]

vax.data[prev.inf=="No" & Age>=16 & Age<=30 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Unvaccinated", n_distinct(PatientID)]
vax.data[prev.inf=="No" & Age>=16 & Age<=30 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE", n_distinct(PatientID)]

vax.data[prev.inf=="No" & Age>=16 & Age<=30 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Partially", n_distinct(PatientID)]
vax.data[prev.inf=="No" & Age>=16 & Age<=30 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Partially" & labresult=="POSITIVE", n_distinct(PatientID)]

vax.data[prev.inf=="No" & Age>=16 & Age<=30 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Fully", n_distinct(PatientID)]
vax.data[prev.inf=="No" & Age>=16 & Age<=30 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Fully" & labresult=="POSITIVE", n_distinct(PatientID)]

vax.data[prev.inf=="No" & Age>=31 & Age<=50 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Unvaccinated", n_distinct(PatientID)]         
vax.data[prev.inf=="No" & Age>=31 & Age<=50 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE", n_distinct(PatientID)]         

vax.data[prev.inf=="No" & Age>=31 & Age<=50 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Partially", n_distinct(PatientID)]         
vax.data[prev.inf=="No" & Age>=31 & Age<=50 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Partially" & labresult=="POSITIVE", n_distinct(PatientID)]         

vax.data[prev.inf=="No" & Age>=31 & Age<=50 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Fully", n_distinct(PatientID)]         
vax.data[prev.inf=="No" & Age>=31 & Age<=50 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Fully" & labresult=="POSITIVE", n_distinct(PatientID)]         

vax.data[prev.inf=="No" & Age>=31 & Age<=50 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Unvaccinated", n_distinct(PatientID)]         
vax.data[prev.inf=="No" & Age>=31 & Age<=50 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE", n_distinct(PatientID)]         

vax.data[prev.inf=="No" & Age>=31 & Age<=50 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Partially", n_distinct(PatientID)]         
vax.data[prev.inf=="No" & Age>=31 & Age<=50 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Partially" & labresult=="POSITIVE", n_distinct(PatientID)]         

vax.data[prev.inf=="No" & Age>=31 & Age<=50 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Fully", n_distinct(PatientID)]         
vax.data[prev.inf=="No" & Age>=31 & Age<=50 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Fully" & labresult=="POSITIVE", n_distinct(PatientID)]         

vax.data[prev.inf=="No" & Age>=51 & Age<=64 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Unvaccinated", n_distinct(PatientID)]         
vax.data[prev.inf=="No" & Age>=51 & Age<=64 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE", n_distinct(PatientID)]         

vax.data[prev.inf=="No" & Age>=51 & Age<=64 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Partially", n_distinct(PatientID)]         
vax.data[prev.inf=="No" & Age>=51 & Age<=64 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Partially" & labresult=="POSITIVE", n_distinct(PatientID)]         

vax.data[prev.inf=="No" & Age>=51 & Age<=64 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Fully", n_distinct(PatientID)]         
vax.data[prev.inf=="No" & Age>=51 & Age<=64 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Fully" & labresult=="POSITIVE", n_distinct(PatientID)]         

vax.data[prev.inf=="No" & Age>=51 & Age<=64 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Unvaccinated", n_distinct(PatientID)]         
vax.data[prev.inf=="No" & Age>=51 & Age<=64 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE", n_distinct(PatientID)]         

vax.data[prev.inf=="No" & Age>=51 & Age<=64 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Partially", n_distinct(PatientID)]         
vax.data[prev.inf=="No" & Age>=51 & Age<=64 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Partially" & labresult=="POSITIVE", n_distinct(PatientID)]         

vax.data[prev.inf=="No" & Age>=51 & Age<=64 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Fully", n_distinct(PatientID)]         
vax.data[prev.inf=="No" & Age>=51 & Age<=64 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Fully" & labresult=="POSITIVE", n_distinct(PatientID)]         

vax.data[prev.inf=="No" & Age>=65 & Age<=80 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Unvaccinated", n_distinct(PatientID)]         
vax.data[prev.inf=="No" & Age>=65 & Age<=80 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE", n_distinct(PatientID)]         

vax.data[prev.inf=="No" & Age>=65 & Age<=80 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Partially", n_distinct(PatientID)]         
vax.data[prev.inf=="No" & Age>=65 & Age<=80 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Partially" & labresult=="POSITIVE", n_distinct(PatientID)]         

vax.data[prev.inf=="No" & Age>=65 & Age<=80 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Fully", n_distinct(PatientID)]         
vax.data[prev.inf=="No" & Age>=65 & Age<=80 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Fully" & labresult=="POSITIVE", n_distinct(PatientID)]         

vax.data[prev.inf=="No" & Age>=65 & Age<=80 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Unvaccinated", n_distinct(PatientID)]         
vax.data[prev.inf=="No" & Age>=65 & Age<=80 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE", n_distinct(PatientID)]         

vax.data[prev.inf=="No" & Age>=65 & Age<=80 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Partially", n_distinct(PatientID)]         
vax.data[prev.inf=="No" & Age>=65 & Age<=80 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Partially" & labresult=="POSITIVE", n_distinct(PatientID)]         

vax.data[prev.inf=="No" & Age>=65 & Age<=80 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Fully", n_distinct(PatientID)]         
vax.data[prev.inf=="No" & Age>=65 & Age<=80 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Fully" & labresult=="POSITIVE", n_distinct(PatientID)]         

vax.data[prev.inf=="No" & Age>=81 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Unvaccinated", n_distinct(PatientID)]         
vax.data[prev.inf=="No" & Age>=81 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE", n_distinct(PatientID)]         

vax.data[prev.inf=="No" & Age>=81 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Partially", n_distinct(PatientID)]         
vax.data[prev.inf=="No" & Age>=81 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Partially" & labresult=="POSITIVE", n_distinct(PatientID)]         

vax.data[prev.inf=="No" & Age>=81 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Fully", n_distinct(PatientID)]         
vax.data[prev.inf=="No" & Age>=81 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Fully" & labresult=="POSITIVE", n_distinct(PatientID)]         

vax.data[prev.inf=="No" & Age>=81 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Unvaccinated", n_distinct(PatientID)]         
vax.data[prev.inf=="No" & Age>=81 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE", n_distinct(PatientID)]         

vax.data[prev.inf=="No" & Age>=81 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Partially", n_distinct(PatientID)]         
vax.data[prev.inf=="No" & Age>=81 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Partially" & labresult=="POSITIVE", n_distinct(PatientID)]         

vax.data[prev.inf=="No" & Age>=81 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Fully", n_distinct(PatientID)]         
vax.data[prev.inf=="No" & Age>=81 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Fully" & labresult=="POSITIVE", n_distinct(PatientID)]         


model.12to15 <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & Age>=12 & Age<=15 & Test_date>="2021-06-15",], family = binomial(link="logit"))
summary(model.12to15)
1-exp(coef(model.12to15))
1-exp(confint(model.12to15))
beep()

model.12to15.1 <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                       + factor(racecat) + factor(comorbidity) + factor(biweekly.period) + factor(BMI_cat), data=vax.data[prev.inf=="No" &  Age>=12 & Age<=15 & Test_date>="2021-06-15",], family = binomial(link="logit"))
summary(model.12to15.1)
1-exp(coef(model.12to15.1))
1-exp(confint.default(model.12to15.1))
beep()



model.16to30 <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & Age>=16 & Age<=30 & biweekly.period %in% c(3,4,5),], family = binomial(link="logit"))
summary(model.16to30)
1-exp(coef(model.16to30))
1-exp(confint(model.16to30))
beep()

model.16to30.1 <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                       + factor(racecat) + factor(comorbidity) + factor(biweekly.period) + factor(BMI_cat), data=vax.data[prev.inf=="No" &  Age>=16 & Age<=30 & biweekly.period %in% c(3,4,5) ,], family = binomial(link="logit"))
summary(model.16to30.1)
1-exp(coef(model.16to30.1))
1-exp(confint.default(model.16to30.1))
beep()

model.16to30.delta <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & Age>=16 & Age<=30 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.16to30.delta)
1-exp(coef(model.16to30.delta))
1-exp(confint.default(model.16to30.delta))
beep()

model.16to30.1.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                       + factor(racecat) + factor(comorbidity) + factor(biweekly.period) + factor(BMI_cat), data=vax.data[prev.inf=="No" &  Age>=16 & Age<=30 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) ,], family = binomial(link="logit"))
summary(model.16to30.1.delta)
1-exp(coef(model.16to30.1.delta))
1-exp(confint.default(model.16to30.1.delta))
beep()


model.31to50 <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & Age>=31 & Age<=50 & biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.31to50)
1-exp(coef(model.31to50))
1-exp(confint.default(model.31to50))
beep()

model.31to50.1 <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                       + factor(racecat) + factor(comorbidity) + factor(biweekly.period)+ factor(BMI_cat), data=vax.data[prev.inf=="No" & Age>=31 & Age<=50 & biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.31to50.1)
1-exp(coef(model.31to50.1))
1-exp(confint.default(model.31to50.1))
beep()


model.31to50.delta <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & Age>=31 & Age<=50 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.31to50.delta)
1-exp(coef(model.31to50.delta))
1-exp(confint.default(model.31to50.delta))
beep()

model.31to50.1.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                       + factor(racecat) + factor(comorbidity) + factor(biweekly.period) + factor(BMI_cat), data=vax.data[prev.inf=="No" & Age>=31 & Age<=50 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.31to50.1.delta)
1-exp(coef(model.31to50.1.delta))
1-exp(confint.default(model.31to50.1.delta))
beep()


model.51to64 <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & Age>=51 & Age<=64 & biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.51to64)
1-exp(coef(model.51to64))
1-exp(confint.default(model.51to64))
beep()

model.51to64.1 <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                       + factor(racecat) + factor(comorbidity) + factor(biweekly.period) + factor(BMI_cat), data=vax.data[prev.inf=="No" & Age>=51 & Age<=64 & biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.51to64.1)
1-exp(coef(model.51to64.1))
1-exp(confint.default(model.51to64.1))
beep()

model.51to64.delta <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & Age>=51 & Age<=64 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.51to64.delta)
1-exp(coef(model.51to64.delta))
1-exp(confint.default(model.51to64.delta))
beep()

model.51to64.1.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                       + factor(racecat) + factor(comorbidity) + factor(biweekly.period) + factor(BMI_cat), data=vax.data[prev.inf=="No" & Age>=51 & Age<=64 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.51to64.1.delta)
1-exp(coef(model.51to64.1.delta))
1-exp(confint.default(model.51to64.1.delta))
beep()

model.65to80 <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & Age>=65 & Age<=80 & biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.65to80)
1-exp(coef(model.65to80))
1-exp(confint.default(model.65to80))
beep()

model.65to80.1 <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                       + factor(racecat) + factor(comorbidity) + factor(biweekly.period) + factor(BMI_cat), data=vax.data[prev.inf=="No" & Age>=65 & Age<=80 & biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.65to80.1)
1-exp(coef(model.65to80.1))
1-exp(confint.default(model.65to80.1))
beep()


model.65to80.delta <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & Age>=65 & Age<=80 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.65to80.delta)
1-exp(coef(model.65to80.delta))
1-exp(confint.default(model.65to80.delta))
beep()

model.65to80.1.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                       + factor(racecat) + factor(comorbidity) + factor(biweekly.period) + factor(BMI_cat), data=vax.data[prev.inf=="No" & Age>=65 & Age<=80 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.65to80.1.delta)
1-exp(coef(model.65to80.1.delta))
1-exp(confint.default(model.65to80.1.delta))
beep()


model.over80 <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & Age>=81 & biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.over80)
1-exp(coef(model.over80))
1-exp(confint.default(model.over80))

model.over80.1 <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                       + factor(racecat) + factor(comorbidity) + factor(biweekly.period) + factor(BMI_cat), data=vax.data[prev.inf=="No" & Age>=81 & biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.over80.1)
1-exp(coef(model.over80.1))
1-exp(confint.default(model.over80.1))
beep()


model.over80.delta <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & Age>=81 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.over80.delta)
1-exp(coef(model.over80.delta))
1-exp(confint.default(model.over80.delta))
beep()

model.over80.1.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                       + factor(racecat) + factor(comorbidity) + factor(biweekly.period) + factor(BMI_cat), data=vax.data[prev.inf=="No" & Age>=81 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.over80.1.delta)
1-exp(coef(model.over80.1.delta))
1-exp(confint.default(model.over80.1.delta))
beep()



#Comorbidities

vax.data[prev.inf=="No" & comorbidity==1 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Unvaccinated", n_distinct(PatientID)]
vax.data[prev.inf=="No" & comorbidity==1 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[prev.inf=="No" & comorbidity==1 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Partially", n_distinct(PatientID)]
vax.data[prev.inf=="No" & comorbidity==1 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Partially" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[prev.inf=="No" & comorbidity==1 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Fully", n_distinct(PatientID)]
vax.data[prev.inf=="No" & comorbidity==1 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Fully" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[prev.inf=="No" & comorbidity==1 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Unvaccinated", n_distinct(PatientID)]
vax.data[prev.inf=="No" & comorbidity==1 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[prev.inf=="No" & comorbidity==1 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Partially", n_distinct(PatientID)]
vax.data[prev.inf=="No" & comorbidity==1 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Partially" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[prev.inf=="No" & comorbidity==1 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Fully", n_distinct(PatientID)]
vax.data[prev.inf=="No" & comorbidity==1 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Fully" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[prev.inf=="No" & comorbidity==0 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Unvaccinated", n_distinct(PatientID)]
vax.data[prev.inf=="No" & comorbidity==0 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[prev.inf=="No" & comorbidity==0 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Partially", n_distinct(PatientID)]
vax.data[prev.inf=="No" & comorbidity==0 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Partially" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[prev.inf=="No" & comorbidity==0 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Fully", n_distinct(PatientID)]
vax.data[prev.inf=="No" & comorbidity==0 & biweekly.period %in% c(1,2,3,4,5) & vaxstatus1=="Fully" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[prev.inf=="No" & comorbidity==0 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Unvaccinated", n_distinct(PatientID)]
vax.data[prev.inf=="No" & comorbidity==0 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[prev.inf=="No" & comorbidity==0 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Partially", n_distinct(PatientID)]
vax.data[prev.inf=="No" & comorbidity==0 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Partially" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[prev.inf=="No" & comorbidity==0 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Fully", n_distinct(PatientID)]
vax.data[prev.inf=="No" & comorbidity==0 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & vaxstatus1=="Fully" & labresult=="POSITIVE",n_distinct(PatientID)]


model.comorbid <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & comorbidity==1 & biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.comorbid)
1-exp(coef(model.comorbid))
1-exp(confint.default(model.comorbid))
beep()

model.comorbid.1 <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                         + factor(racecat) + factor(agecat) + factor(biweekly.period) + factor(BMI_cat), data=vax.data[prev.inf=="No" & comorbidity==1 & biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.comorbid.1)
1-exp(coef(model.comorbid.1))
1-exp(confint.default(model.comorbid.1))
beep()

model.comorbid.delta <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & comorbidity==1 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.comorbid.delta)
1-exp(coef(model.comorbid.delta))
1-exp(confint.default(model.comorbid.delta))

model.comorbid.1.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                         + factor(racecat) + factor(agecat) + factor(biweekly.period) + factor(BMI_cat), data=vax.data[prev.inf=="No" & comorbidity==1 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.comorbid.1.delta)
1-exp(coef(model.comorbid.1.delta))
1-exp(confint.default(model.comorbid.1.delta))
beep()


model.nocomorbid <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & comorbidity==0 & biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.nocomorbid)
1-exp(coef(model.nocomorbid))
1-exp(confint.default(model.nocomorbid))

model.nocomorbid.1 <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                                 + factor(racecat) + factor(agecat) + factor(biweekly.period) + factor(BMI_cat), data=vax.data[prev.inf=="No" & comorbidity==0 & biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.nocomorbid.1)
1-exp(coef(model.nocomorbid.1))
1-exp(confint.default(model.nocomorbid.1))
beep()

model.nocomorbid.delta <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[prev.inf=="No" & comorbidity==0 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.nocomorbid.delta)
1-exp(coef(model.nocomorbid.delta))
1-exp(confint.default(model.nocomorbid.delta))

model.nocomorbid.1.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                         + factor(racecat) + factor(agecat) + factor(biweekly.period) + factor(BMI_cat), data=vax.data[prev.inf=="No" & comorbidity==0 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.nocomorbid.1.delta)
1-exp(coef(model.nocomorbid.1.delta))
1-exp(confint.default(model.nocomorbid.1.delta))
beep()


##Vaccine manufacturer
vax.data <- as.data.table(vax.data)

vax.data$vaxname[vax.data$Vax_rec=="Yes" & is.na(vax.data$vaxname)] <- "Unknown"
table(vax.data$vaxname)

Pfizer <- vax.data %>%
  filter(vaxname=="Pfizer" | is.na(vaxname))

Moderna <- vax.data %>%
  filter(vaxname=="Moderna" | is.na(vaxname))


table(Pfizer$prev.inf, Pfizer$vaxstatus1, useNA = "ifany")
table(Moderna$prev.inf, Moderna$vaxstatus1, useNA = "ifany")
table(JandJ$prev.inf, JandJ$vaxstatus1, useNA = "ifany")

Pfizer$exposure = NA
Pfizer$exposure[Pfizer$vaxstatus1 %in% c("Fully") & Pfizer$prev.inf=="No"] <- "Fully only"
Pfizer$exposure[Pfizer$vaxstatus1 %in% c("Fully") & Pfizer$prev.inf=="Yes"] <- "Fully + Natural"
Pfizer$exposure[Pfizer$vaxstatus1 %in% c("Partially") & Pfizer$prev.inf=="Yes"] <- "Partial + Natural"
Pfizer$exposure[Pfizer$vaxstatus1 %in% c("Partially") & Pfizer$prev.inf=="No"] <- "Partial only"
Pfizer$exposure[Pfizer$vaxstatus1 %in% c("Unvaccinated") & Pfizer$prev.inf=="Yes"] <- "No Pfizer + Natural"
Pfizer$exposure[Pfizer$vaxstatus1 %in% c("Unvaccinated") & Pfizer$prev.inf=="No"] <- "No Pfizer + No Natural"

table(Pfizer$exposure, useNA="ifany")

Moderna$exposure = NA
Moderna$exposure[Moderna$vaxstatus1 %in% c("Fully") & Moderna$prev.inf=="No"] <- "Fully only"
Moderna$exposure[Moderna$vaxstatus1 %in% c("Fully") & Moderna$prev.inf=="Yes"] <- "Fully + Natural"
Moderna$exposure[Moderna$vaxstatus1 %in% c("Partially") & Moderna$prev.inf=="Yes"] <- "Partial + Natural"
Moderna$exposure[Moderna$vaxstatus1 %in% c("Partially") & Moderna$prev.inf=="No"] <- "Partial only"
Moderna$exposure[Moderna$vaxstatus1 %in% c("Unvaccinated") & Moderna$prev.inf=="Yes"] <- "No Moderna + Natural"
Moderna$exposure[Moderna$vaxstatus1 %in% c("Unvaccinated") & Moderna$prev.inf=="No"] <- "No Moderna + No Natural"

table(Moderna$exposure, useNA="ifany")



Pfizer[biweekly.period %in% c(1,2,3,4,5) & exposure=="No Pfizer + No Natural" & symptomatic.case==1, n_distinct(PatientID)]
Pfizer[biweekly.period %in% c(1,2,3,4,5) & exposure=="No Pfizer + No Natural" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

Pfizer[biweekly.period %in% c(1,2,3,4,5) & exposure=="Fully only" & symptomatic.case==1, n_distinct(PatientID)]
Pfizer[biweekly.period %in% c(1,2,3,4,5) & exposure=="Fully only"  & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

Pfizer[biweekly.period %in% c(1,2,3,4,5) & exposure=="Fully + Natural" & symptomatic.case==1, n_distinct(PatientID)]
Pfizer[biweekly.period %in% c(1,2,3,4,5) & exposure=="Fully + Natural" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

Pfizer[biweekly.period %in% c(1,2,3,4,5) & exposure=="Partial + Natural" & symptomatic.case==1, n_distinct(PatientID)]
Pfizer[biweekly.period %in% c(1,2,3,4,5) & exposure=="Partial + Natural" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

Pfizer[biweekly.period %in% c(1,2,3,4,5) & exposure=="Partial only" & symptomatic.case==1, n_distinct(PatientID)]
Pfizer[biweekly.period %in% c(1,2,3,4,5) & exposure=="Partial only" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

Pfizer[biweekly.period %in% c(1,2,3,4,5) & exposure=="No Pfizer + Natural" & symptomatic.case==1, n_distinct(PatientID)]
Pfizer[biweekly.period %in% c(1,2,3,4,5) & exposure=="No Pfizer + Natural" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

Pfizer[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="No Pfizer + No Natural" & symptomatic.case==1, n_distinct(PatientID)]
Pfizer[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="No Pfizer + No Natural" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

Pfizer[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Fully only" & symptomatic.case==1, n_distinct(PatientID)]
Pfizer[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Fully only" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

Pfizer[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Fully + Natural" & symptomatic.case==1, n_distinct(PatientID)]
Pfizer[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Fully + Natural" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

Pfizer[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Partial + Natural" & symptomatic.case==1, n_distinct(PatientID)]
Pfizer[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Partial + Natural" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

Pfizer[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Partial only" & symptomatic.case==1, n_distinct(PatientID)]
Pfizer[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Partial only" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

Pfizer[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="No Pfizer + Natural" & symptomatic.case==1, n_distinct(PatientID)]
Pfizer[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="No Pfizer + Natural" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]



Pfizer[biweekly.period %in% c(1,2,3,4,5) & exposure=="No Pfizer + No Natural" & symptomatic.case==0, n_distinct(PatientID)]
Pfizer[biweekly.period %in% c(1,2,3,4,5) & exposure=="No Pfizer + No Natural" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

Pfizer[biweekly.period %in% c(1,2,3,4,5) & exposure=="Fully only" & symptomatic.case==0, n_distinct(PatientID)]
Pfizer[biweekly.period %in% c(1,2,3,4,5) & exposure=="Fully only"  & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

Pfizer[biweekly.period %in% c(1,2,3,4,5) & exposure=="Fully + Natural" & symptomatic.case==0, n_distinct(PatientID)]
Pfizer[biweekly.period %in% c(1,2,3,4,5) & exposure=="Fully + Natural" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

Pfizer[biweekly.period %in% c(1,2,3,4,5) & exposure=="Partial + Natural" & symptomatic.case==0, n_distinct(PatientID)]
Pfizer[biweekly.period %in% c(1,2,3,4,5) & exposure=="Partial + Natural" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

Pfizer[biweekly.period %in% c(1,2,3,4,5) & exposure=="Partial only" & symptomatic.case==0, n_distinct(PatientID)]
Pfizer[biweekly.period %in% c(1,2,3,4,5) & exposure=="Partial only" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

Pfizer[biweekly.period %in% c(1,2,3,4,5) & exposure=="No Pfizer + Natural" & symptomatic.case==0, n_distinct(PatientID)]
Pfizer[biweekly.period %in% c(1,2,3,4,5) & exposure=="No Pfizer + Natural" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

Pfizer[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="No Pfizer + No Natural" & symptomatic.case==0, n_distinct(PatientID)]
Pfizer[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="No Pfizer + No Natural" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

Pfizer[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Fully only" & symptomatic.case==0, n_distinct(PatientID)]
Pfizer[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Fully only" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

Pfizer[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Fully + Natural" & symptomatic.case==0, n_distinct(PatientID)]
Pfizer[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Fully + Natural" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

Pfizer[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Partial + Natural" & symptomatic.case==0, n_distinct(PatientID)]
Pfizer[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Partial + Natural" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

Pfizer[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Partial only" & symptomatic.case==0, n_distinct(PatientID)]
Pfizer[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Partial only" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

Pfizer[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="No Pfizer + Natural" & symptomatic.case==0, n_distinct(PatientID)]
Pfizer[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="No Pfizer + Natural" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]



Pfizer$exposure <- relevel(factor(Pfizer$exposure), ref="No Pfizer + No Natural")

model.pfizer <-  glm(factor(labresult) ~ factor(exposure), data=Pfizer[biweekly.period %in% c(1,2,3,4,5) & symptomatic.case==0,], family = binomial(link="logit"))
summary(model.pfizer)
1-exp(coef(model.pfizer ))
1-exp(confint.default(model.pfizer))

model.pfizer.1 <-  glm(factor(labresult) ~ factor(exposure) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                                 + factor(racecat) + factor(agecat) + factor(biweekly.period) + factor(comorbidity) + factor(BMI_cat), data=Pfizer[biweekly.period %in% c(1,2,3,4,5) & symptomatic.case==0,], family = binomial(link="logit"))
summary(model.pfizer.1)
1-exp(coef(model.pfizer.1))
1-exp(confint.default(model.pfizer.1))
beep()

model.pfizer.delta <-  glm(factor(labresult) ~ factor(exposure), data=Pfizer[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & symptomatic.case==0,], family = binomial(link="logit"))
summary(model.pfizer.delta)
1-exp(coef(model.pfizer.delta))
1-exp(confint.default(model.pfizer.delta))

model.pfizer.1.delta <-  glm(factor(labresult) ~ factor(exposure) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                       + factor(racecat) + factor(agecat) + factor(biweekly.period) + factor(comorbidity) + factor(BMI_cat), data=Pfizer[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & symptomatic.case==0,], family = binomial(link="logit"))
summary(model.pfizer.1.delta)
1-exp(coef(model.pfizer.1.delta))
1-exp(confint.default(model.pfizer.1.delta))
beep()


Moderna[biweekly.period %in% c(1,2,3,4,5) & exposure=="No Moderna + No Natural" & symptomatic.case==1, n_distinct(PatientID)]
Moderna[biweekly.period %in% c(1,2,3,4,5) & exposure=="No Moderna + No Natural" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

Moderna[biweekly.period %in% c(1,2,3,4,5) & exposure=="Fully only" & symptomatic.case==1, n_distinct(PatientID)]
Moderna[biweekly.period %in% c(1,2,3,4,5) & exposure=="Fully only" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

Moderna[biweekly.period %in% c(1,2,3,4,5) & exposure=="Fully + Natural" & symptomatic.case==1, n_distinct(PatientID)]
Moderna[biweekly.period %in% c(1,2,3,4,5) & exposure=="Fully + Natural" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

Moderna[biweekly.period %in% c(1,2,3,4,5) & exposure=="Partial + Natural" & symptomatic.case==1, n_distinct(PatientID)]
Moderna[biweekly.period %in% c(1,2,3,4,5) & exposure=="Partial + Natural" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

Moderna[biweekly.period %in% c(1,2,3,4,5) & exposure=="Partial only" & symptomatic.case==1, n_distinct(PatientID)]
Moderna[biweekly.period %in% c(1,2,3,4,5) & exposure=="Partial only" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

Moderna[biweekly.period %in% c(1,2,3,4,5) & exposure=="No Moderna + Natural" & symptomatic.case==1, n_distinct(PatientID)]
Moderna[biweekly.period %in% c(1,2,3,4,5) & exposure=="No Moderna + Natural" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

Moderna[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="No Moderna + No Natural" & symptomatic.case==1, n_distinct(PatientID)]
Moderna[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="No Moderna + No Natural" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

Moderna[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Fully only" & symptomatic.case==1, n_distinct(PatientID)]
Moderna[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Fully only" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

Moderna[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Fully + Natural" & symptomatic.case==1, n_distinct(PatientID)]
Moderna[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Fully + Natural" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

Moderna[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Partial + Natural" & symptomatic.case==1, n_distinct(PatientID)]
Moderna[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Partial + Natural" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

Moderna[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Partial only" & symptomatic.case==1, n_distinct(PatientID)]
Moderna[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Partial only" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

Moderna[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="No Moderna + Natural" & symptomatic.case==1, n_distinct(PatientID)]
Moderna[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="No Moderna + Natural" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]


Moderna[biweekly.period %in% c(1,2,3,4,5) & exposure=="No Moderna + No Natural" & symptomatic.case==0, n_distinct(PatientID)]
Moderna[biweekly.period %in% c(1,2,3,4,5) & exposure=="No Moderna + No Natural" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

Moderna[biweekly.period %in% c(1,2,3,4,5) & exposure=="Fully only" & symptomatic.case==0, n_distinct(PatientID)]
Moderna[biweekly.period %in% c(1,2,3,4,5) & exposure=="Fully only" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

Moderna[biweekly.period %in% c(1,2,3,4,5) & exposure=="Fully + Natural" & symptomatic.case==0, n_distinct(PatientID)]
Moderna[biweekly.period %in% c(1,2,3,4,5) & exposure=="Fully + Natural" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

Moderna[biweekly.period %in% c(1,2,3,4,5) & exposure=="Partial + Natural" & symptomatic.case==0, n_distinct(PatientID)]
Moderna[biweekly.period %in% c(1,2,3,4,5) & exposure=="Partial + Natural" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

Moderna[biweekly.period %in% c(1,2,3,4,5) & exposure=="Partial only" & symptomatic.case==0, n_distinct(PatientID)]
Moderna[biweekly.period %in% c(1,2,3,4,5) & exposure=="Partial only" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

Moderna[biweekly.period %in% c(1,2,3,4,5) & exposure=="No Moderna + Natural" & symptomatic.case==0, n_distinct(PatientID)]
Moderna[biweekly.period %in% c(1,2,3,4,5) & exposure=="No Moderna + Natural" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

Moderna[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="No Moderna + No Natural" & symptomatic.case==0, n_distinct(PatientID)]
Moderna[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="No Moderna + No Natural" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

Moderna[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Fully only" & symptomatic.case==0, n_distinct(PatientID)]
Moderna[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Fully only" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

Moderna[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Fully + Natural" & symptomatic.case==0, n_distinct(PatientID)]
Moderna[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Fully + Natural" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

Moderna[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Partial + Natural" & symptomatic.case==0, n_distinct(PatientID)]
Moderna[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Partial + Natural" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

Moderna[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Partial only" & symptomatic.case==0, n_distinct(PatientID)]
Moderna[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="Partial only" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

Moderna[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="No Moderna + Natural" & symptomatic.case==0, n_distinct(PatientID)]
Moderna[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure=="No Moderna + Natural" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]




Moderna$exposure <- relevel(factor(Moderna$exposure), ref="No Moderna + No Natural")

model.moderna <-  glm(factor(labresult) ~ factor(exposure), data=Moderna[biweekly.period %in% c(1,2,3,4,5) & symptomatic.case==0,], family = binomial(link="logit"))
summary(model.moderna)
1-exp(coef(model.moderna ))
1-exp(confint.default(model.moderna))

model.moderna.1 <-  glm(factor(labresult) ~ factor(exposure) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                       + factor(racecat) + factor(agecat) + factor(biweekly.period) + factor(comorbidity) + factor(BMI_cat), data=Moderna[biweekly.period %in% c(1,2,3,4,5) & symptomatic.case==0,], family = binomial(link="logit"))
summary(model.moderna.1)
1-exp(coef(model.moderna.1))
1-exp(confint.default(model.moderna.1))
beep()


model.moderna.delta <-  glm(factor(labresult) ~ factor(exposure), data=Moderna[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & symptomatic.case==0,], family = binomial(link="logit"))
summary(model.moderna.delta)
1-exp(coef(model.moderna.delta))
1-exp(confint.default(model.moderna.delta))


model.moderna.1.delta <-  glm(factor(labresult) ~ factor(exposure) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                        + factor(racecat) + factor(agecat) + factor(biweekly.period) + factor(comorbidity) + factor(BMI_cat), data=Moderna[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & symptomatic.case==0,], family = binomial(link="logit"))
summary(model.moderna.1.delta)
1-exp(coef(model.moderna.1.delta))
1-exp(confint.default(model.moderna.1.delta))
beep()


JandJ$vaxname[JandJ$Vax_rec=="Yes" & is.na(JandJ$vaxname)] <- "Unknown"


JandJ %>%
  filter(vaxname=="Johnson & Johnson" | is.na(vaxname))-> JandJ


JandJ$vaxstatus1 = NA
JandJ$vaxstatus1[JandJ$vaxstatus=="Yes"] <- "Fully"
JandJ$vaxstatus1[JandJ$vaxstatus== "No"] <- "Unvaccinated"


JandJ$exposure = NA
JandJ$exposure[JandJ$vaxstatus1 %in% c("Fully") & JandJ$prev.inf=="No"] <- "JandJ only"
JandJ$exposure[JandJ$vaxstatus1 %in% c("Fully") & JandJ$prev.inf=="Yes"] <- "JandJ + Natural"
JandJ$exposure[JandJ$vaxstatus1 %in% c("Unvaccinated") & JandJ$prev.inf=="Yes"] <- "No JandJ + Natural"
JandJ$exposure[JandJ$vaxstatus1 %in% c("Unvaccinated") & JandJ$prev.inf=="No"] <- "No JandJ + No Natural"

table(JandJ$exposure, useNA="ifany")

JandJ %>%
  mutate(fever.final = ifelse(fever=="Yes" | Fever.vitalsigns==1, "Yes", "No"),
         symptomatic.case = ifelse((fever.final=="Yes" | chills=="Yes" | cough =="Yes" | sore.throat=="Yes" | fatigue=="Yes" | headache=="Yes"|
                                      confusion=="Yes" | taste.smell=="Yes" | sob=="Yes" | diarrhea=="Yes" | chest.pain=="Yes" | nausea.vomit=="Yes" |
                                      congestion.nose=="Yes" | body.ache=="Yes" | O2.vitalsigns==1), 1,0)) -> JandJ

Pfizer %>%
  mutate(fever.final = ifelse(fever=="Yes" | Fever.vitalsigns==1, "Yes", "No"),
         symptomatic.case = ifelse((fever.final=="Yes" | chills=="Yes" | cough =="Yes" | sore.throat=="Yes" | fatigue=="Yes" | headache=="Yes"|
                                      confusion=="Yes" | taste.smell=="Yes" | sob=="Yes" | diarrhea=="Yes" | chest.pain=="Yes" | nausea.vomit=="Yes" |
                                      congestion.nose=="Yes" | body.ache=="Yes" | O2.vitalsigns==1), 1,0)) -> Pfizer


JandJ[biweekly.period %in% c(1,2,3,4,5) & exposure == "JandJ only" & symptomatic.case==1, n_distinct(PatientID)]
JandJ[biweekly.period %in% c(1,2,3,4,5) & exposure == "JandJ only" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

JandJ[biweekly.period %in% c(1,2,3,4,5) & exposure == "JandJ + Natural" & symptomatic.case==1, n_distinct(PatientID)]
JandJ[biweekly.period %in% c(1,2,3,4,5) & exposure == "JandJ + Natural" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

JandJ[biweekly.period %in% c(1,2,3,4,5) & exposure == "No JandJ + Natural" & symptomatic.case==1, n_distinct(PatientID)]
JandJ[biweekly.period %in% c(1,2,3,4,5) & exposure == "No JandJ + Natural" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

JandJ[biweekly.period %in% c(1,2,3,4,5) & exposure == "No JandJ + No Natural" & symptomatic.case==1, n_distinct(PatientID)]
JandJ[biweekly.period %in% c(1,2,3,4,5) & exposure == "No JandJ + No Natural" & symptomatic.case==1  & labresult=="POSITIVE", n_distinct(PatientID)]

JandJ[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure == "JandJ only" & symptomatic.case==1, n_distinct(PatientID)]
JandJ[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure == "JandJ only" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

JandJ[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure == "JandJ + Natural" & symptomatic.case==1, n_distinct(PatientID)]
JandJ[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure == "JandJ + Natural" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

JandJ[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure == "No JandJ + Natural" & symptomatic.case==1, n_distinct(PatientID)]
JandJ[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure == "No JandJ + Natural" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

JandJ[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure == "No JandJ + No Natural" & symptomatic.case==1, n_distinct(PatientID)]
JandJ[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure == "No JandJ + No Natural" & symptomatic.case==1 & labresult=="POSITIVE", n_distinct(PatientID)]

JandJ[biweekly.period %in% c(1,2,3,4,5) & exposure == "JandJ only" & symptomatic.case==0, n_distinct(PatientID)]
JandJ[biweekly.period %in% c(1,2,3,4,5) & exposure == "JandJ only" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

JandJ[biweekly.period %in% c(1,2,3,4,5) & exposure == "JandJ + Natural" & symptomatic.case==0, n_distinct(PatientID)]
JandJ[biweekly.period %in% c(1,2,3,4,5) & exposure == "JandJ + Natural" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

JandJ[biweekly.period %in% c(1,2,3,4,5) & exposure == "No JandJ + Natural" & symptomatic.case==0, n_distinct(PatientID)]
JandJ[biweekly.period %in% c(1,2,3,4,5) & exposure == "No JandJ + Natural" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

JandJ[biweekly.period %in% c(1,2,3,4,5) & exposure == "No JandJ + No Natural" & symptomatic.case==0, n_distinct(PatientID)]
JandJ[biweekly.period %in% c(1,2,3,4,5) & exposure == "No JandJ + No Natural" & symptomatic.case==0  & labresult=="POSITIVE", n_distinct(PatientID)]

JandJ[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure == "JandJ only" & symptomatic.case==0, n_distinct(PatientID)]
JandJ[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure == "JandJ only" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

JandJ[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure == "JandJ + Natural" & symptomatic.case==0, n_distinct(PatientID)]
JandJ[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure == "JandJ + Natural" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

JandJ[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure == "No JandJ + Natural" & symptomatic.case==0, n_distinct(PatientID)]
JandJ[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure == "No JandJ + Natural" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]

JandJ[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure == "No JandJ + No Natural" & symptomatic.case==0, n_distinct(PatientID)]
JandJ[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & exposure == "No JandJ + No Natural" & symptomatic.case==0 & labresult=="POSITIVE", n_distinct(PatientID)]



JandJ$exposure <- relevel(factor(JandJ$exposure), ref="No JandJ + No Natural")

model.JandJ <-  glm(factor(labresult) ~ factor(exposure), data=JandJ[biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.JandJ)
1-exp(coef(model.JandJ ))
1-exp(confint.default(model.JandJ))


model.JandJ.1 <-  glm(factor(labresult) ~ factor(exposure) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                        + factor(racecat) + factor(agecat) + factor(biweekly.period) +factor(BMI_cat) + factor(comorbidity), data=JandJ[biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.JandJ.1)
1-exp(coef(model.JandJ.1))
1-exp(confint.default(model.JandJ.1))
beep()


model.JandJ.delta <-  glm(factor(labresult) ~ factor(exposure), data=JandJ[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.JandJ.delta)
1-exp(coef(model.JandJ.delta))
1-exp(confint.default(model.JandJ.delta))


model.JandJ.1.delta <-  glm(factor(labresult) ~ factor(exposure) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                      + factor(racecat) + factor(agecat) + factor(biweekly.period) +factor(BMI_cat), data=JandJ[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.JandJ.1.delta)
1-exp(coef(model.JandJ.1.delta))
1-exp(confint.default(model.JandJ.1.delta))
beep()


##By race/ethnicity
vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="White" & vaxstatus1=="Unvaccinated",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="White" & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="White" & vaxstatus1=="Partially",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="White" & vaxstatus1=="Partially" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="White" & vaxstatus1=="Fully",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="White" & vaxstatus1=="Fully" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="White" & vaxstatus1=="Unvaccinated",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="White" & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="White" & vaxstatus1=="Partially",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="White" & vaxstatus1=="Partially" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="White" & vaxstatus1=="Fully",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="White" & vaxstatus1=="Fully" & labresult=="POSITIVE",n_distinct(PatientID)]


#White
model.White <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="White",], family = binomial(link="logit"))
summary(model.White)
1-exp(coef(model.White))
1-exp(confint.default(model.White))

model.White.1 <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                      + factor(agecat) + factor(comorbidity) + factor(biweekly.period) + factor(BMI_cat), data=vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="White",], family = binomial(link="logit"))
summary(model.White.1)
1-exp(coef(model.White.1))
1-exp(confint.default(model.White.1))

model.White.delta <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="White",], family = binomial(link="logit"))
summary(model.White.delta)
1-exp(coef(model.White.delta))
1-exp(confint.default(model.White.delta))

model.White.1.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                             + factor(agecat) +  factor(comorbidity)+ factor(biweekly.period) + factor(BMI_cat), data=vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="White",], family = binomial(link="logit"))
summary(model.White.1.delta)
1-exp(coef(model.White.1.delta))
1-exp(confint.default(model.White.1.delta))
beep()


#Black
vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Black/AfrAm" & vaxstatus1=="Unvaccinated",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Black/AfrAm" & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Black/AfrAm" & vaxstatus1=="Partially",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Black/AfrAm" & vaxstatus1=="Partially" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Black/AfrAm" & vaxstatus1=="Fully",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Black/AfrAm" & vaxstatus1=="Fully" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Black/AfrAm" & vaxstatus1=="Unvaccinated",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Black/AfrAm" & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Black/AfrAm" & vaxstatus1=="Partially",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Black/AfrAm" & vaxstatus1=="Partially" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Black/AfrAm" & vaxstatus1=="Fully",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Black/AfrAm" & vaxstatus1=="Fully" & labresult=="POSITIVE",n_distinct(PatientID)]

model.Black <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Black/AfrAm",], family = binomial(link="logit"))
summary(model.Black)
1-exp(coef(model.Black))
1-exp(confint.default(model.Black))

model.Black.1 <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                      + factor(agecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Black/AfrAm",], family = binomial(link="logit"))
summary(model.Black.1)
1-exp(coef(model.Black.1))
1-exp(confint.default(model.Black.1))

model.Black.delta <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Black/AfrAm",], family = binomial(link="logit"))
summary(model.Black.delta)
1-exp(coef(model.Black.delta))
1-exp(confint.default(model.Black.delta))

model.Black.1.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                            + factor(agecat) +  factor(comorbidity)+ factor(biweekly.period), data=vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Black/AfrAm",], family = binomial(link="logit"))
summary(model.Black.1.delta)
1-exp(coef(model.Black.1.delta))
1-exp(confint.default(model.Black.1.delta))
beep()

#Hispanic
vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Hispanic" & vaxstatus1=="Unvaccinated",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Hispanic" & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Hispanic" & vaxstatus1=="Partially",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Hispanic" & vaxstatus1=="Partially" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Hispanic" & vaxstatus1=="Fully",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Hispanic" & vaxstatus1=="Fully" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Hispanic" & vaxstatus1=="Unvaccinated",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Hispanic" & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Hispanic" & vaxstatus1=="Partially",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Hispanic" & vaxstatus1=="Partially" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Hispanic" & vaxstatus1=="Fully",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Hispanic" & vaxstatus1=="Fully" & labresult=="POSITIVE",n_distinct(PatientID)]

model.Hispanic <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Hispanic",], family = binomial(link="logit"))
summary(model.Hispanic)
1-exp(coef(model.Hispanic))
1-exp(confint.default(model.Hispanic))

model.Hispanic.1 <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                      + factor(agecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Hispanic",], family = binomial(link="logit"))
summary(model.Hispanic.1)
1-exp(coef(model.Hispanic.1))
1-exp(confint.default(model.Hispanic.1))

model.Hispanic.delta <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Hispanic",], family = binomial(link="logit"))
summary(model.Hispanic.delta)
1-exp(coef(model.Hispanic.delta))
1-exp(confint.default(model.Hispanic.delta))

model.Hispanic.1.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                            + factor(agecat) +  factor(comorbidity)+ factor(biweekly.period), data=vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Hispanic",], family = binomial(link="logit"))
summary(model.Hispanic.1.delta)
1-exp(coef(model.Hispanic.1.delta))
1-exp(confint.default(model.Hispanic.1.delta))
beep()


#Asian
vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Asian" & vaxstatus1=="Unvaccinated",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Asian" & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Asian" & vaxstatus1=="Partially",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Asian" & vaxstatus1=="Partially" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Asian" & vaxstatus1=="Fully",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Asian" & vaxstatus1=="Fully" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Asian" & vaxstatus1=="Unvaccinated",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Asian" & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Asian" & vaxstatus1=="Partially",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Asian" & vaxstatus1=="Partially" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Asian" & vaxstatus1=="Fully",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Asian" & vaxstatus1=="Fully" & labresult=="POSITIVE",n_distinct(PatientID)]


model.Asian <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Asian",], family = binomial(link="logit"))
summary(model.Asian)
1-exp(coef(model.Asian))
1-exp(confint.default(model.Asian))

model.Asian.1 <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                         + factor(agecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Asian",], family = binomial(link="logit"))
summary(model.Asian.1)
1-exp(coef(model.Asian.1))
1-exp(confint.default(model.Asian.1))

model.Asian.delta <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Asian",], family = binomial(link="logit"))
summary(model.Asian.delta)
1-exp(coef(model.Asian.delta))
1-exp(confint.default(model.Asian.delta))

model.Asian.1.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                               + factor(agecat) +  factor(comorbidity)+ factor(biweekly.period), data=vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Asian",], family = binomial(link="logit"))
summary(model.Asian.1.delta)
1-exp(coef(model.Asian.1.delta))
1-exp(confint.default(model.Asian.1.delta))
beep()

#Native American
vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Native American/Alaskan Native/Pacific Islander" & vaxstatus1=="Unvaccinated",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Native American/Alaskan Native/Pacific Islander" & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Native American/Alaskan Native/Pacific Islander" & vaxstatus1=="Partially",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Native American/Alaskan Native/Pacific Islander" & vaxstatus1=="Partially" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Native American/Alaskan Native/Pacific Islander" & vaxstatus1=="Fully",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Native American/Alaskan Native/Pacific Islander" & vaxstatus1=="Fully" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Native American/Alaskan Native/Pacific Islander" & vaxstatus1=="Unvaccinated",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Native American/Alaskan Native/Pacific Islander" & vaxstatus1=="Unvaccinated" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Native American/Alaskan Native/Pacific Islander" & vaxstatus1=="Partially",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Native American/Alaskan Native/Pacific Islander" & vaxstatus1=="Partially" & labresult=="POSITIVE",n_distinct(PatientID)]

vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Native American/Alaskan Native/Pacific Islander" & vaxstatus1=="Fully",n_distinct(PatientID)]
vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Native American/Alaskan Native/Pacific Islander" & vaxstatus1=="Fully" & labresult=="POSITIVE",n_distinct(PatientID)]


model.NatAm <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Native American/Alaskan Native/Pacific Islander",], family = binomial(link="logit"))
summary(model.NatAm)
1-exp(coef(model.NatAm))
1-exp(confint.default(model.NatAm))

model.NatAm.1 <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                      + factor(agecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[biweekly.period %in% c(1,2,3,4,5) & racecat=="Native American/Alaskan Native/Pacific Islander",], family = binomial(link="logit"))
summary(model.NatAm.1)
1-exp(coef(model.NatAm.1))
1-exp(confint.default(model.NatAm.1))

model.NatAm.delta <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Native American/Alaskan Native/Pacific Islander",], family = binomial(link="logit"))
summary(model.NatAm.delta)
1-exp(coef(model.NatAm.delta))
1-exp(confint.default(model.NatAm.delta))

model.NatAm.1.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(Region) + factor(Exp.risk)
                            + factor(agecat) +  factor(comorbidity)+ factor(biweekly.period), data=vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & racecat=="Native American/Alaskan Native/Pacific Islander",], family = binomial(link="logit"))
summary(model.NatAm.1.delta)
1-exp(coef(model.NatAm.1.delta))
1-exp(confint.default(model.NatAm.1.delta))
beep()

##Borough
#Manhattan
model.Manhattan <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[biweekly.period %in% c(1,2,3,4,5) & Region=="Manhattan",], family = binomial(link="logit"))
summary(model.Manhattan)
1-exp(coef(model.Manhattan))
1-exp(confint.default(model.Manhattan))

model.Manhattan.1 <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(racecat) + factor(Exp.risk)
                      + factor(agecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[biweekly.period %in% c(1,2,3,4,5) & Region=="Manhattan",], family = binomial(link="logit"))
summary(model.Manhattan.1)
1-exp(coef(model.Manhattan.1))
1-exp(confint.default(model.Manhattan.1))

model.Manhattan.delta <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Region=="Manhattan",], family = binomial(link="logit"))
summary(model.Manhattan.delta)
1-exp(coef(model.Manhattan.delta))
1-exp(confint.default(model.Manhattan.delta))

model.Manhattan.1.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(racecat) + factor(Exp.risk)
                            + factor(agecat) +  factor(comorbidity)+ factor(biweekly.period), data=vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Region=="Manhattan",], family = binomial(link="logit"))
summary(model.Manhattan.1.delta)
1-exp(coef(model.Manhattan.1.delta))
1-exp(confint.default(model.Manhattan.1.delta))
beep()


#Brooklyn
model.Brooklyn <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[biweekly.period %in% c(1,2,3,4,5) & Region=="Brooklyn",], family = binomial(link="logit"))
summary(model.Brooklyn)
1-exp(coef(model.Brooklyn))
1-exp(confint.default(model.Brooklyn))

model.Brooklyn.1 <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(racecat) + factor(Exp.risk)
                          + factor(agecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[biweekly.period %in% c(1,2,3,4,5) & Region=="Brooklyn",], family = binomial(link="logit"))
summary(model.Brooklyn.1)
1-exp(coef(model.Brooklyn.1))
1-exp(confint.default(model.Brooklyn.1))

model.Brooklyn.delta <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Region=="Brooklyn",], family = binomial(link="logit"))
summary(model.Brooklyn.delta)
1-exp(coef(model.Brooklyn.delta))
1-exp(confint.default(model.Brooklyn.delta))

model.Brooklyn.1.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(racecat) + factor(Exp.risk)
                                + factor(agecat) +  factor(comorbidity)+ factor(biweekly.period), data=vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Region=="Brooklyn",], family = binomial(link="logit"))
summary(model.Brooklyn.1.delta)
1-exp(coef(model.Brooklyn.1.delta))
1-exp(confint.default(model.Brooklyn.1.delta))
beep()


#Queens
model.Queens <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[biweekly.period %in% c(1,2,3,4,5) & Region=="Queens",], family = binomial(link="logit"))
summary(model.Queens)
1-exp(coef(model.Queens))
1-exp(confint.default(model.Queens))

model.Queens.1 <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(racecat) + factor(Exp.risk)
                         + factor(agecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[biweekly.period %in% c(1,2,3,4,5) & Region=="Queens",], family = binomial(link="logit"))
summary(model.Queens.1)
1-exp(coef(model.Queens.1))
1-exp(confint.default(model.Queens.1))

model.Queens.delta <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Region=="Queens",], family = binomial(link="logit"))
summary(model.Queens.delta)
1-exp(coef(model.Queens.delta))
1-exp(confint.default(model.Queens.delta))

model.Queens.1.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(racecat) + factor(Exp.risk)
                               + factor(agecat) +  factor(comorbidity)+ factor(biweekly.period), data=vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Region=="Queens",], family = binomial(link="logit"))
summary(model.Queens.1.delta)
1-exp(coef(model.Queens.1.delta))
1-exp(confint.default(model.Queens.1.delta))
beep()

#Bronx
model.Bronx <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[biweekly.period %in% c(1,2,3,4,5) & Region=="Bronx",], family = binomial(link="logit"))
summary(model.Bronx)
1-exp(coef(model.Bronx))
1-exp(confint.default(model.Bronx))

model.Bronx.1 <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(racecat) + factor(Exp.risk)
                       + factor(agecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[biweekly.period %in% c(1,2,3,4,5) & Region=="Bronx",], family = binomial(link="logit"))
summary(model.Bronx.1)
1-exp(coef(model.Bronx.1))
1-exp(confint.default(model.Bronx.1))

model.Bronx.delta <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Region=="Bronx",], family = binomial(link="logit"))
summary(model.Bronx.delta)
1-exp(coef(model.Bronx.delta))
1-exp(confint.default(model.Bronx.delta))

model.Bronx.1.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(racecat) + factor(Exp.risk)
                             + factor(agecat) +  factor(comorbidity)+ factor(biweekly.period), data=vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Region=="Bronx",], family = binomial(link="logit"))
summary(model.Bronx.1.delta)
1-exp(coef(model.Bronx.1.delta))
1-exp(confint.default(model.Bronx.1.delta))
beep()

#Staten Island
model.Staten <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[biweekly.period %in% c(1,2,3,4,5) & Region=="Staten Island",], family = binomial(link="logit"))
summary(model.Staten)
1-exp(coef(model.Staten))
1-exp(confint.default(model.Staten))

model.Staten.1 <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(racecat) + factor(Exp.risk)
                      + factor(agecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[biweekly.period %in% c(1,2,3,4,5) & Region=="Staten Island",], family = binomial(link="logit"))
summary(model.Staten.1)
1-exp(coef(model.Staten.1))
1-exp(confint.default(model.Staten.1))

model.Staten.delta <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Region=="Staten Island",], family = binomial(link="logit"))
summary(model.Staten.delta)
1-exp(coef(model.Staten.delta))
1-exp(confint.default(model.Staten.delta))

model.Staten.1.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(racecat) + factor(Exp.risk)
                            + factor(agecat) +  factor(comorbidity)+ factor(biweekly.period), data=vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Region=="Staten Island",], family = binomial(link="logit"))
summary(model.Staten.1.delta)
1-exp(coef(model.Staten.1.delta))
1-exp(confint.default(model.Staten.1.delta))
beep()

#Long Island
model.LI <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[biweekly.period %in% c(1,2,3,4,5) & Region=="Long Island",], family = binomial(link="logit"))
summary(model.LI)
1-exp(coef(model.LI))
1-exp(confint.default(model.LI))

model.LI.1 <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(racecat) + factor(Exp.risk)
                       + factor(agecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[biweekly.period %in% c(1,2,3,4,5) & Region=="Long Island",], family = binomial(link="logit"))
summary(model.LI.1)
1-exp(coef(model.LI.1))
1-exp(confint.default(model.LI.1))

model.LI.delta <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Region=="Long Island",], family = binomial(link="logit"))
summary(model.LI.delta)
1-exp(coef(model.LI.delta))
1-exp(confint.default(model.LI.delta))

model.LI.1.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(racecat) + factor(Exp.risk)
                             + factor(agecat) +  factor(comorbidity)+ factor(biweekly.period), data=vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Region=="Long Island",], family = binomial(link="logit"))
summary(model.LI.1.delta)
1-exp(coef(model.LI.1.delta))
1-exp(confint.default(model.LI.1.delta))
beep()


#Westchester
model.MN <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[biweekly.period %in% c(1,2,3,4,5) & Region=="Metro North",], family = binomial(link="logit"))
summary(model.MN)
1-exp(coef(model.MN))
1-exp(confint.default(model.MN))

model.MN.1 <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(racecat) + factor(Exp.risk)
                   + factor(agecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[biweekly.period %in% c(1,2,3,4,5) & Region=="Metro North",], family = binomial(link="logit"))
summary(model.MN.1)
1-exp(coef(model.MN.1))
1-exp(confint.default(model.MN.1))

model.MN.delta <-  glm(factor(labresult) ~ factor(vaxstatus1), data=vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Region=="Metro North",], family = binomial(link="logit"))
summary(model.MN.delta)
1-exp(coef(model.MN.delta))
1-exp(confint.default(model.MN.delta))

model.MN.1.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Gender)  + factor(racecat) + factor(Exp.risk)
                         + factor(agecat) +  factor(comorbidity)+ factor(biweekly.period), data=vax.data[biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Region=="Metro North",], family = binomial(link="logit"))
summary(model.MN.1.delta)
1-exp(coef(model.MN.1.delta))
1-exp(confint.default(model.MN.1.delta))
beep()

##Sensitivity analysis
##PCR tests only

table(vax.data$Grouping)

pcr <- vax.data %>%
  filter(Grouping== "COVID PCR (Active)")

pcr <- as.data.table(pcr)

pcr[pcr$prev.inf=="No" & pcr$biweekly.period %in% c(1,2,3,4,5) & pcr$vaxstatus1=="Unvaccinated", n_distinct(PatientID)]
pcr[pcr$prev.inf=="No" & pcr$biweekly.period %in% c(1,2,3,4,5) & pcr$vaxstatus1=="Unvaccinated" & pcr$labresult=="POSITIVE", n_distinct(PatientID)]

pcr[pcr$prev.inf=="No" & pcr$biweekly.period %in% c(1,2,3,4,5) & pcr$vaxstatus1=="Partially", n_distinct(PatientID)]
pcr[pcr$prev.inf=="No" & pcr$biweekly.period %in% c(1,2,3,4,5) & pcr$vaxstatus1=="Partially" & pcr$labresult=="POSITIVE", n_distinct(PatientID)]

pcr[pcr$prev.inf=="No" & pcr$biweekly.period %in% c(1,2,3,4,5) & pcr$vaxstatus1=="Fully", n_distinct(PatientID)]
pcr[pcr$prev.inf=="No" & pcr$biweekly.period %in% c(1,2,3,4,5) & pcr$vaxstatus1=="Fully" & pcr$labresult=="POSITIVE", n_distinct(PatientID)]

pcr[pcr$prev.inf=="No" & pcr$biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & pcr$vaxstatus1=="Unvaccinated", n_distinct(PatientID)]
pcr[pcr$prev.inf=="No" & pcr$biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & pcr$vaxstatus1=="Unvaccinated" & pcr$labresult=="POSITIVE", n_distinct(PatientID)]

pcr[pcr$prev.inf=="No" & pcr$biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & pcr$vaxstatus1=="Partially", n_distinct(PatientID)]
pcr[pcr$prev.inf=="No" & pcr$biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & pcr$vaxstatus1=="Partially" & pcr$labresult=="POSITIVE", n_distinct(PatientID)]

pcr[pcr$prev.inf=="No" & pcr$biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & pcr$vaxstatus1=="Fully", n_distinct(PatientID)]
pcr[pcr$prev.inf=="No" & pcr$biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & pcr$vaxstatus1=="Fully" & pcr$labresult=="POSITIVE", n_distinct(PatientID)]

model.pcr.sens <- glm(factor(labresult) ~ factor(vaxstatus1), data=pcr[pcr$prev.inf=="No" & pcr$biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.pcr.sens)
1-exp(coef(model.pcr.sens))
1-exp(confint(model.pcr.sens))


model.pcr.sens.1 <- glm(factor(labresult) ~ factor(vaxstatus1) + factor(agecat) + factor(Gender) + factor(Region) + factor(Exp.risk)
                + factor(comorbidity) + factor(racecat) + factor(biweekly.period) + factor(BMI_cat), data=pcr[pcr$prev.inf=="No" & pcr$biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.pcr.sens.1)
1-exp(coef(model.pcr.sens.1))
1-exp(confint.default(model.pcr.sens.1))


model.pcr.sens.delta <- glm(factor(labresult) ~ factor(vaxstatus1), data=pcr[pcr$prev.inf=="No" & pcr$biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.pcr.sens.delta)
1-exp(coef(model.pcr.sens.delta))
1-exp(confint.default(model.pcr.sens.delta))
beep()

model.pcr.sens.delta.1 <- glm(factor(labresult) ~ factor(vaxstatus1) + factor(agecat) + factor(Gender) + factor(Region) + factor(Exp.risk)
                + factor(comorbidity) + factor(racecat) + factor(biweekly.period), data=pcr[pcr$prev.inf=="No" & pcr$biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.pcr.sens.delta.1)
1-exp(coef(model.pcr.sens.delta.1))
1-exp(confint.default(model.pcr.sens.delta.1))
beep()


pcr.JandJ <- JandJ %>%
  filter(Grouping== "COVID PCR (Active)")

pcr.JandJ$vaxstatus1 <- relevel(factor(pcr.JandJ$vaxstatus1), ref="Unvaccinated")

model.JandJ.sens<- glm(factor(labresult) ~ factor(vaxstatus1), data=pcr.JandJ[pcr.JandJ$prev.inf=="No" & pcr.JandJ$biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.JandJ.sens)
1-exp(coef(model.JandJ.sens))
1-exp(confint(model.JandJ.sens))


model.JandJ.sens.1 <- glm(factor(labresult) ~ factor(vaxstatus1) + factor(agecat) + factor(Gender) + factor(Region) + factor(Exp.risk)
                         + factor(racecat) + factor(biweekly.period) + factor(BMI_cat), data=pcr.JandJ[pcr.JandJ$prev.inf=="No" & pcr.JandJ$biweekly.period %in% c(1,2,3,4,5),], family = binomial(link="logit"))
summary(model.JandJ.sens.1)
1-exp(coef(model.JandJ.sens.1))
1-exp(confint.default(model.JandJ.sens.1))


model.JandJ.sens.delta <- glm(factor(labresult) ~ factor(vaxstatus1), data=pcr.JandJ[pcr.JandJ$prev.inf=="No" & pcr.JandJ$biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.JandJ.sens.delta)
1-exp(coef(model.JandJ.sens.delta))
1-exp(confint.default(model.JandJ.sens.delta))
beep()

model.JandJ.sens.delta.1 <- glm(factor(labresult) ~ factor(vaxstatus1) + factor(agecat) + factor(Gender) + factor(Region) + factor(Exp.risk)
                              + factor(racecat) + factor(biweekly.period), data=pcr.JandJ[pcr.JandJ$prev.inf=="No" & pcr.JandJ$biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15),], family = binomial(link="logit"))
summary(model.JandJ.sens.delta.1)
1-exp(coef(model.JandJ.sens.delta.1))
1-exp(confint.default(model.JandJ.sens.delta.1))
beep()




###Age-VE trend by gender
age1.m <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
                       + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=12 & Age<=15 & Gender=="M",] , family = binomial(link="logit"))
summary(age1.m)
1-exp(coef(age1.m))
1-exp(confint.default(age1.m))
beep()

age1.f <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=12 & Age<=15 & Gender=="F",] , family = binomial(link="logit"))
summary(age1.f)
1-exp(coef(age1.f))
1-exp(confint.default(age1.f))
beep()

age2.m <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
             + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=16 & Age<=20 & biweekly.period %in% c(3,4,5) & Gender=="M",], family = binomial(link="logit"))
summary(age2.m)
1-exp(coef(age2.m))
1-exp(confint.default(age2.m))
beep()

age2.f <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
             + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=16 & Age<=20 & biweekly.period %in% c(3,4,5) & Gender=="F",], family = binomial(link="logit"))
summary(age2.f)
1-exp(coef(age2.f))
1-exp(confint.default(age2.f))
beep()


age3.m <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=21 & Age<=25 & biweekly.period %in% c(3,4,5) & Gender=="M",], family = binomial(link="logit"))
summary(age3.m)
1-exp(coef(age3.m))
1-exp(confint.default(age3.m))
beep()

age3.f <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=21 & Age<=25 & biweekly.period %in% c(3,4,5) & Gender=="F",], family = binomial(link="logit"))
summary(age3.f)
1-exp(coef(age3.f))
1-exp(confint.default(age3.f))
beep()


age4.m <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=26 & Age<=30 & biweekly.period %in% c(3,4,5) & Gender=="M",], family = binomial(link="logit"))
summary(age4.m)
1-exp(coef(age4.m))
1-exp(confint.default(age4.m))
beep()

age4.f <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=26 & Age<=30 & biweekly.period %in% c(3,4,5) & Gender=="F",], family = binomial(link="logit"))
summary(age4.f)
1-exp(coef(age4.f))
1-exp(confint.default(age4.f))
beep()

age5.m <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=31 & Age<=35 & biweekly.period %in% c(1,2,3,4,5) & Gender=="M",], family = binomial(link="logit"))
summary(age5.m)
1-exp(coef(age5.m))
1-exp(confint.default(age5.m))
beep()

age5.f <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=31 & Age<=35 & biweekly.period %in% c(1,2,3,4,5) & Gender=="F",], family = binomial(link="logit"))
summary(age5.f)
1-exp(coef(age5.f))
1-exp(confint.default(age5.f))
beep()


age6.m <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=36 & Age<=40 & biweekly.period %in% c(1,2,3,4,5) & Gender=="M",], family = binomial(link="logit"))
summary(age6.m)
1-exp(coef(age6.m))
1-exp(confint.default(age6.m))
beep()

age6.f <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=36 & Age<=40 & biweekly.period %in% c(1,2,3,4,5) & Gender=="F",], family = binomial(link="logit"))
summary(age6.f)
1-exp(coef(age6.f))
1-exp(confint.default(age6.f))
beep()

age7.m <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=41 & Age<=45 & biweekly.period %in% c(1,2,3,4,5) & Gender=="M",], family = binomial(link="logit"))
summary(age7.m)
1-exp(coef(age7.m))
1-exp(confint.default(age7.m))
beep()

age7.f <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=41 & Age<=45 & biweekly.period %in% c(1,2,3,4,5) & Gender=="F",], family = binomial(link="logit"))
summary(age7.f)
1-exp(coef(age7.f))
1-exp(confint.default(age7.f))
beep()


age8.m <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=46 & Age<=50 & biweekly.period %in% c(1,2,3,4,5) & Gender=="M",], family = binomial(link="logit"))
summary(age8.m)
1-exp(coef(age8.m))
1-exp(confint.default(age8.m))
beep()

age8.f <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=46 & Age<=50 & biweekly.period %in% c(1,2,3,4,5) & Gender=="F",], family = binomial(link="logit"))
summary(age8.f)
1-exp(coef(age8.f))
1-exp(confint.default(age8.f))
beep()


age9.m <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=51 & Age<=55 & biweekly.period %in% c(1,2,3,4,5) & Gender=="M",], family = binomial(link="logit"))
summary(age9.m)
1-exp(coef(age9.m))
1-exp(confint.default(age9.m))
beep()

age9.f <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=51 & Age<=55 & biweekly.period %in% c(1,2,3,4,5) & Gender=="F",], family = binomial(link="logit"))
summary(age9.f)
1-exp(coef(age9.f))
1-exp(confint.default(age9.f))
beep()

age10.m <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=56 & Age<=60 & biweekly.period %in% c(1,2,3,4,5) & Gender=="M",], family = binomial(link="logit"))
summary(age10.m)
1-exp(coef(age10.m))
1-exp(confint.default(age10.m))
beep()

age10.f <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=56 & Age<=60 & biweekly.period %in% c(1,2,3,4,5) & Gender=="F",], family = binomial(link="logit"))
summary(age10.f)
1-exp(coef(age10.f))
1-exp(confint.default(age10.f))
beep()

age11.m <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
                + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=61 & Age<=65 & biweekly.period %in% c(1,2,3,4,5) & Gender=="M",], family = binomial(link="logit"))
summary(age11.m)
1-exp(coef(age11.m))
1-exp(confint.default(age11.m))
beep()

age11.f <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
                + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=61 & Age<=65 & biweekly.period %in% c(1,2,3,4,5) & Gender=="F",], family = binomial(link="logit"))
summary(age11.f)
1-exp(coef(age11.f))
1-exp(confint.default(age11.f))
beep()

age12.m <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
                + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=66 & Age<=70 & biweekly.period %in% c(1,2,3,4,5) & Gender=="M",], family = binomial(link="logit"))
summary(age12.m)
1-exp(coef(age12.m))
1-exp(confint.default(age12.m))
beep()

age12.f <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
                + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=66 & Age<=70 & biweekly.period %in% c(1,2,3,4,5) & Gender=="F",], family = binomial(link="logit"))
summary(age12.f)
1-exp(coef(age12.f))
1-exp(confint.default(age12.f))
beep()

age13.m <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
                + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=71 & Age<=75 & biweekly.period %in% c(1,2,3,4,5) & Gender=="M",], family = binomial(link="logit"))
summary(age13.m)
1-exp(coef(age13.m))
1-exp(confint.default(age13.m))
beep()

age13.f <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
                + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=71 & Age<=75 & biweekly.period %in% c(1,2,3,4,5) & Gender=="F",], family = binomial(link="logit"))
summary(age13.f)
1-exp(coef(age13.f))
1-exp(confint.default(age13.f))
beep()


age14.m <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
                + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=76 & Age<=80 & biweekly.period %in% c(1,2,3,4,5) & Gender=="M",], family = binomial(link="logit"))
summary(age14.m)
1-exp(coef(age14.m))
1-exp(confint.default(age14.m))
beep()

age14.f <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
                + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=76 & Age<=80 & biweekly.period %in% c(1,2,3,4,5) & Gender=="F",], family = binomial(link="logit"))
summary(age14.f)
1-exp(coef(age14.f))
1-exp(confint.default(age14.f))
beep()


age15.m <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
                + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=81 & Age<=85 & biweekly.period %in% c(1,2,3,4,5) & Gender=="M",], family = binomial(link="logit"))
summary(age15.m)
1-exp(coef(age15.m))
1-exp(confint.default(age15.m))
beep()

age15.f <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
                + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=81 & Age<=85 & biweekly.period %in% c(1,2,3,4,5) & Gender=="F",], family = binomial(link="logit"))
summary(age15.f)
1-exp(coef(age15.f))
1-exp(confint.default(age15.f))
beep()

age16.m <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
                + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=86 & biweekly.period %in% c(1,2,3,4,5) & Gender=="M",], family = binomial(link="logit"))
summary(age16.m)
1-exp(coef(age16.m))
1-exp(confint.default(age16.m))
beep()

age16.f <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
                + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=86 & biweekly.period %in% c(1,2,3,4,5) & Gender=="F",], family = binomial(link="logit"))
summary(age16.f)
1-exp(coef(age16.f))
1-exp(confint.default(age16.f))
beep()


age2.m.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=16 & Age<=20 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="M",], family = binomial(link="logit"))
summary(age2.m.delta)
1-exp(coef(age2.m.delta))
1-exp(confint.default(age2.m.delta))
beep()

age2.f.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=16 & Age<=20 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="F",], family = binomial(link="logit"))
summary(age2.f.delta)
1-exp(coef(age2.f.delta))
1-exp(confint.default(age2.f.delta))
beep()


age3.m.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=21 & Age<=25 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="M",], family = binomial(link="logit"))
summary(age3.m.delta)
1-exp(coef(age3.m.delta))
1-exp(confint.default(age3.m.delta))
beep()

age3.f.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=21 & Age<=25 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="F",], family = binomial(link="logit"))
summary(age3.f.delta)
1-exp(coef(age3.f.delta))
1-exp(confint.default(age3.f.delta))
beep()


age4.m.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=26 & Age<=30 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="M",], family = binomial(link="logit"))
summary(age4.m.delta)
1-exp(coef(age4.m.delta))
1-exp(confint.default(age4.m.delta))
beep()

age4.f.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=26 & Age<=30 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="F",], family = binomial(link="logit"))
summary(age4.f.delta)
1-exp(coef(age4.f.delta))
1-exp(confint.default(age4.f.delta))
beep()

age5.m.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=31 & Age<=35 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="M",], family = binomial(link="logit"))
summary(age5.m.delta)
1-exp(coef(age5.m.delta))
1-exp(confint.default(age5.m.delta))
beep()

age5.f.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=31 & Age<=35 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="F",], family = binomial(link="logit"))
summary(age5.f.delta)
1-exp(coef(age5.f.delta))
1-exp(confint.default(age5.f.delta))
beep()


age6.m.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=36 & Age<=40 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="M",], family = binomial(link="logit"))
summary(age6.m.delta)
1-exp(coef(age6.m.delta))
1-exp(confint.default(age6.m.delta))
beep()

age6.f.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=36 & Age<=40 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="F",], family = binomial(link="logit"))
summary(age6.f.delta)
1-exp(coef(age6.f.delta))
1-exp(confint.default(age6.f.delta))
beep()

age7.m.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=41 & Age<=45 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="M",], family = binomial(link="logit"))
summary(age7.m.delta)
1-exp(coef(age7.m.delta))
1-exp(confint.default(age7.m.delta))
beep()

age7.f.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=41 & Age<=45 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="F",], family = binomial(link="logit"))
summary(age7.f.delta)
1-exp(coef(age7.f.delta))
1-exp(confint.default(age7.f.delta))
beep()


age8.m.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=46 & Age<=50 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="M",], family = binomial(link="logit"))
summary(age8.m.delta)
1-exp(coef(age8.m.delta))
1-exp(confint.default(age8.m.delta))
beep()

age8.f.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=46 & Age<=50 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="F",], family = binomial(link="logit"))
summary(age8.f.delta)
1-exp(coef(age8.f.delta))
1-exp(confint.default(age8.f.delta))
beep()


age9.m.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=51 & Age<=55 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="M",], family = binomial(link="logit"))
summary(age9.m.delta)
1-exp(coef(age9.m.delta))
1-exp(confint.default(age9.m.delta))
beep()

age9.f.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
               + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=51 & Age<=55 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="F",], family = binomial(link="logit"))
summary(age9.f.delta)
1-exp(coef(age9.f.delta))
1-exp(confint.default(age9.f.delta))
beep()

age10.m.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
                + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=56 & Age<=60 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="M",], family = binomial(link="logit"))
summary(age10.m.delta)
1-exp(coef(age10.m.delta))
1-exp(confint.default(age10.m.delta))
beep()

age10.f.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
                + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=56 & Age<=60 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="F",], family = binomial(link="logit"))
summary(age10.f.delta)
1-exp(coef(age10.f.delta))
1-exp(confint.default(age10.f.delta))
beep()

age11.m.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
                + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=61 & Age<=65 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="M",], family = binomial(link="logit"))
summary(age11.m.delta)
1-exp(coef(age11.m.delta))
1-exp(confint.default(age11.m.delta))
beep()

age11.f.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
                + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=61 & Age<=65 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="F",], family = binomial(link="logit"))
summary(age11.f.delta)
1-exp(coef(age11.f.delta))
1-exp(confint.default(age11.f.delta))
beep()

age12.m.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
                + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=66 & Age<=70 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="M",], family = binomial(link="logit"))
summary(age12.m.delta)
1-exp(coef(age12.m.delta))
1-exp(confint.default(age12.m.delta))
beep()

age12.f.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
                + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=66 & Age<=70 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="F",], family = binomial(link="logit"))
summary(age12.f.delta)
1-exp(coef(age12.f.delta))
1-exp(confint.default(age12.f.delta))
beep()

age13.m.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
                + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=71 & Age<=75 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="M",], family = binomial(link="logit"))
summary(age13.m.delta)
1-exp(coef(age13.m.delta))
1-exp(confint.default(age13.m.delta))
beep()

age13.f.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
                + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=71 & Age<=75 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="F",], family = binomial(link="logit"))
summary(age13.f.delta)
1-exp(coef(age13.f.delta))
1-exp(confint.default(age13.f.delta))
beep()


age14.m.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
                + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=76 & Age<=80 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="M",], family = binomial(link="logit"))
summary(age14.m.delta)
1-exp(coef(age14.m.delta))
1-exp(confint.default(age14.m.delta))
beep()

age14.f.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
                + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=76 & Age<=80 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="F",], family = binomial(link="logit"))
summary(age14.f.delta)
1-exp(coef(age14.f.delta))
1-exp(confint.default(age14.f.delta))
beep()


age15.m.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
                + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=81 & Age<=85 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="M",], family = binomial(link="logit"))
summary(age15.m.delta)
1-exp(coef(age15.m.delta))
1-exp(confint.default(age15.m.delta))
beep()

age15.f.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
                + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=81 & Age<=85 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="F",], family = binomial(link="logit"))
summary(age15.f.delta)
1-exp(coef(age15.f.delta))
1-exp(confint.default(age15.f.delta))
beep()

age16.m.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
                + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=86 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="M",], family = binomial(link="logit"))
summary(age16.m.delta)
1-exp(coef(age16.m.delta))
1-exp(confint.default(age16.m.delta))
beep()

age16.f.delta <-  glm(factor(labresult) ~ factor(vaxstatus1) + factor(Region) + factor(Exp.risk)
                + factor(racecat) + factor(comorbidity) + factor(biweekly.period), data=vax.data[prev.inf=="No" &  Age>=86 & biweekly.period %in% c(6,7,8,9,10,11,12,13,14,15) & Gender=="F",], family = binomial(link="logit"))
summary(age16.f.delta)
1-exp(coef(age16.f.delta))
1-exp(confint.default(age16.f.delta))
beep()


####Ct values
#Violin plots

library(vioplot)

# Draw the plot
ORF1 <- Ct_Values_Dataset[Ct_Values_Dataset$Test=="SARS CoV 2 ORF 1 Gene",]
E <- Ct_Values_Dataset[Ct_Values_Dataset$Test=="SARS CoV 2 E Gene",]


with(ORF1 , vioplot( 
  Num.Res[Vaccinated=="Yes"] , Num.Res[Vaccinated=="No"],  
  col=rgb(0.1,0.4,0.7,0.7) , names=c("Vaccinated", "Unvaccinated") 
))

with(E , vioplot( 
  Num.Res[Vaccinated=="Yes"] , Num.Res[Vaccinated=="No"],  
  col=rgb(0.1,0.4,0.7,0.7) , names=c("Vaccinated", "Unvaccinated") 
))


#VE over time 
Time <- c("Apr 1- Apr 15",
          "Apr 16- Apr 29",
          "Apr 30 - May 13",
          "May 14 - May 27",
          "May 28 - 10 Jun",
          "11 Jun - 24 Jun",
          "25 Jun - 8 July",
          "9 Jul -  22 Jul",
          "23 Jul - 5 Aug",
          "6 Aug - 19 Aug",
          "20 Aug - 2 Sep",
          "3 Sep - 16 Sep",
          "17 Sep - 30 Sep",
          "1 Oct - 14 Oct",
          "15 Oct - 25th Oct")

VE <- c(0.88,
        0.87,
        0.91,
        0.88,
        0.82,
        0.69,
        0.64,
        0.61,
        0.67,
        0.68,
        0.59,
        0.47,
        0.44,
        0.54,
        0.28)

lb <- c(0.87,
        0.85,
        0.88,
        0.85,
        0.78,
        0.62,
        0.59,
        0.59,
        0.66,
        0.66,
        0.55,
        0.42,
        0.37,
        0.44,
        -0.07)

ub <- c(0.9,
        0.89,
        0.92,
        0.91,
        0.86,
        0.74,
        0.68,
        0.64,
        0.69,
        0.7,
        0.62,
        0.51,
        0.51,
        0.63,
        0.52)

VE.over.time <- as.data.frame(cbind(Time, VE, lb, ub))


ggplot(VE.over.time, aes(x = Time, y = as.numeric(VE)*100, group = 1)) + 
  geom_line(col='red') + 
  geom_ribbon(aes(ymin = as.numeric(lb)*100, ymax = as.numeric(ub)*100), alpha = 0.1) +
  scale_x_discrete(limits=c("Apr 1- Apr 15",
                            "Apr 16- Apr 29",
                            "Apr 30 - May 13",
                            "May 14 - May 27",
                            "May 28 - 10 Jun",
                            "11 Jun - 24 Jun",
                            "25 Jun - 8 July",
                            "9 Jul -  22 Jul",
                            "23 Jul - 5 Aug",
                            "6 Aug - 19 Aug",
                            "20 Aug - 2 Sep",
                            "3 Sep - 16 Sep",
                            "17 Sep - 30 Sep",
                            "1 Oct - 14 Oct",
                            "15 Oct - 25th Oct"))+
  theme(axis.text.x = element_text(angle = 90)) +
  ylab("Vaccine Effectiveness")







