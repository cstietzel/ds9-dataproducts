# Connecticut Public High School Map
Chuck Stietzel  
11/2/2017  



## Synopsis


## Data Preparation



```r
schools <- read_csv("~/EdDataCT/schools.csv", skip = 3, 
                    col_names  = c("OrganizationType", "District", "OrganizationCode",
                                   "OrganizationName", "Street", "City", "State", 
                                   "ZIP", "Phone", "Fax", "Website", "EdProgram",
                                   "ProgramType", "InterDistrictMagnet",
                                   "PreKindergarten", "Kindergarten", "Grade01",
                                   "Grade02", "Grade03", "Grade04", "Grade05",
                                   "Grade06", "Grade07", "Grade08", "Grade09",
                                   "Grade10", "Grade11", "Grade12")) %>% 
  ## Remove all '=' and '"' symbols from file
  mutate_all(function(x) gsub("[=\"]+", "", x)) %>% rowwise() %>%
  
  ## Grades are indicated by 1 or 0.  Assume NA is 0.
  mutate_at(vars(PreKindergarten:Grade12),
            function (x) if (is.na(x)) 0 else as.numeric(x)) %>%
  
  ## Select only Schools that offer 9-12 and are affiliated with a town district
  filter((Grade09 + Grade12 == 2) & (State == "CT") & 
           !is.na(District) & grepl("School", OrganizationType)) %>%
  
  ## Add a gps location column to botate coordinates
  mutate(gpsaddr = paste(OrganizationName, City, "CT", sep = ", "))

## Get gps locations
library(ggmap)
locs <- geocode(schools$gpsaddr)
noloc<-which(is.na(locs$lon))
swl <- mutate(schools[-noloc,], longitude = locs[-noloc,]$lon, latitude = locs[-noloc,]$lat)

## Retry for all schools that did not get a location the first time
## Due to timeout or going over API limit
locsna <- geocode(schools[noloc,]$gpsaddr)
swol <- mutate(schools[noloc,], longitude = locsna$lon, latitude=locsna$lat)
schools <- bind_rows(swl, swol)
```

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


```r
schools <- read_csv("~/coursera/ct_edu/school_w_loc.csv")
```

```
## Parsed with column specification:
## cols(
##   .default = col_integer(),
##   OrganizationType = col_character(),
##   District = col_character(),
##   OrganizationName = col_character(),
##   Street = col_character(),
##   City = col_character(),
##   State = col_character(),
##   ZIP = col_character(),
##   Phone = col_character(),
##   Fax = col_character(),
##   Website = col_character(),
##   EdProgram = col_character(),
##   ProgramType = col_character(),
##   InterDistrictMagnet = col_character(),
##   SchoolCT = col_character(),
##   longitude = col_double(),
##   latitude = col_double()
## )
```

```
## See spec(...) for full column specifications.
```

```r
leaflet(schools) %>% 
  addTiles() %>%
addMarkers(clusterOptions = markerClusterOptions(),
           popup = schools$OrganizationName)
```

```
## Assuming 'longitude' and 'latitude' are longitude and latitude, respectively
```

<!--html_preserve--><div id="htmlwidget-2b2ee2fadd4ad60e805e" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-2b2ee2fadd4ad60e805e">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"maxNativeZoom":null,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"continuousWorld":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":null,"unloadInvisibleTiles":null,"updateWhenIdle":null,"detectRetina":false,"reuseTiles":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addMarkers","args":[[41.7839665,41.2934662,41.3268268,41.3782134,41.8278753,41.871232,41.2876455,41.22511,41.1732849,41.22511,41.1889732,41.22511,41.1922148,41.6612449,41.6859286,41.4785235,41.8263299,41.7726195,41.895294,41.6843968,41.7510704,41.9480544,41.4901527,41.5768416,41.3381619,41.6864745,41.3420336,41.340947,41.3345319,41.81902,41.4136865,41.0621121,41.5495998,41.811456,41.2496685,41.5282037,41.575529,41.782122,41.6149599,41.4014741,41.4164699,41.0856753,41.3336597,41.9329394,41.493033,41.5824021,41.7547461,41.7560911,41.329034,41.3693373,41.9011308,41.9096994,41.313801,41.9786588,41.9294257,41.1495,41.186856,41.7015655,41.959961,41.0397676,41.5953532,41.3403414,41.3131182,41.367806,41.7478466,41.7726195,41.7731556,41.8002885,41.7748311,41.7613718,41.7650322,41.7650343,41.7652258,41.767257,41.7979146,41.7547741,41.7404581,41.7527771,41.7964781,41.3234617,41.7914702,41.8599411,41.74963,41.3374157,41.6224764,41.43796,41.7423231,41.2928204,41.7815756,41.5340038,41.576121,41.2192589,41.3489066,41.4573862,41.4917655,41.6499035,41.1286974,41.461007,41.3054918,41.3974636,41.2902012,41.3222521,41.3011808,41.312946,41.2817948,41.3245699,41.357904,41.3591203,41.7008554,41.4099636,41.3359304,41.3732322,41.4395732,41.0856093,41.0856093,41.1222631,41.1388873,41.2886268,41.3829347,41.7121151,41.6659273,41.6800602,41.6883192,41.9221427,41.9370764,41.3705524,41.3575551,41.7280262,41.9133932,41.6644022,41.2996665,41.7800978,41.7511213,41.4768822,41.5583302,41.5025524,41.4255144,41.464471,41.3185646,41.8028078,41.3250965,41.6700447,41.3851768,41.3200857,41.8707908,41.9800513,41.8421829,41.6166563,41.9754116,41.0593697,41.0620338,41.0846373,41.0916671,41.1969273,41.9807467,41.184048,41.9306359,41.977303,41.8717929,41.8161548,41.2645256,41.8569939,41.437635,41.4854566,41.5458635,41.5458901,41.5547227,41.5575389,41.604199,41.3416973,41.5949005,41.7361993,41.7943418,41.2984594,41.210428,41.1544307,41.7074175,41.2104705,41.723156,41.9213507,41.8443326,41.6070419,41.7834386,41.6215191,41.7593454,41.7604488,41.1889243,41.1638784,41.8707894,41.3003244,41.7416983,41.2005807,41.5291331,41.781937,41.7198896,41.7542869,41.7122098,41.825226,41.7510611,41.5777321,41.7502171,41.5318068,41.203944,41.301877,41.3056536,41.5270263,41.536985,41.7276524,41.5807761,41.6017389,41.3665767,41.2248721,41.6583116,41.2594427,41.951017],[-72.6973441,-72.9478799,-73.0656051,-73.3938593,-72.7260467,-72.7365741,-72.8014264,-73.1848239,-73.2071821,-73.1848239,-73.2054877,-73.1848239,-73.1668237,-72.9607174,-72.9220947,-73.3910219,-72.9166189,-72.6756774,-72.6530267,-72.8101891,-72.6838407,-72.5999146,-72.9070268,-72.3022522,-72.959558,-72.8056092,-72.9420807,-72.0169418,-73.063952,-71.894179,-73.4544617,-73.5477359,-72.0778441,-73.111748,-73.0464059,-72.6745955,-73.066431,-72.3121622,-72.6600495,-73.4413943,-73.4703176,-73.4911221,-73.0979851,-72.7280786,-72.4434614,-72.5127816,-72.6049089,-72.5954078,-72.83221,-72.2127308,-72.6116366,-72.4619707,-72.9033373,-72.5965511,-73.0757866,-73.263421,-73.234124,-72.5936241,-72.7926687,-73.6122393,-71.981407,-72.0129565,-72.7122669,-72.9223406,-72.672916,-72.6756774,-72.7014245,-72.7084163,-72.6819128,-72.5605992,-72.7010581,-72.7012709,-72.7010105,-72.6781972,-72.6699439,-72.6648944,-72.6351421,-72.660141,-72.7096237,-72.9261373,-72.6975367,-71.8732905,-72.6435964,-72.0717215,-72.2391093,-71.9952506,-73.2067537,-72.6193037,-72.51734,-72.8267207,-72.6788279,-73.0128241,-73.1942808,-72.1637177,-73.0719153,-72.7780388,-73.4895453,-73.5076093,-72.929631,-72.8369562,-72.9661733,-72.9394501,-72.9188338,-72.918814,-72.9285531,-72.9083016,-72.1069792,-72.1204912,-72.7357328,-73.2711026,-72.8003763,-72.8633704,-71.8858121,-73.4331532,-73.4331531,-73.389911,-73.4254035,-72.3969355,-73.1380897,-72.2148486,-72.858029,-73.0096944,-73.0342453,-71.9169209,-73.3623039,-72.4476591,-73.0077248,-73.2252195,-73.0471688,-72.3647604,-73.3390684,-72.9876048,-72.1053861,-72.6692571,-73.1897652,-73.1610296,-73.0825742,-72.5615717,-72.3250615,-72.2443372,-73.5277591,-72.6472796,-73.0986607,-73.1181234,-72.8215864,-72.4614876,-72.5544389,-72.8615408,-72.3070857,-73.5364836,-73.5317945,-73.546395,-73.5647645,-73.132864,-72.6927204,-73.1842439,-73.07244,-71.897964,-72.3396133,-73.1108784,-73.1928208,-72.4799723,-72.8184949,-72.8359585,-72.9728745,-73.0572223,-73.0388674,-73.0203011,-73.0265679,-72.1290064,-73.1014924,-72.7515446,-72.7510632,-72.4538044,-73.3780535,-73.328314,-72.6711394,-73.4330463,-72.2147817,-72.6430531,-72.6551022,-72.9795493,-72.8621958,-72.7527695,-72.6813585,-72.4216536,-73.1848355,-73.1957338,-72.7266739,-72.5325564,-72.7057918,-73.1634847,-72.83092,-72.5615473,-72.2203153,-72.6061475,-72.2161144,-71.889948,-72.8683481,-71.8977107,-72.6867744,-72.7728953,-73.0888519,-72.9222607,-72.9407046,-73.4247323,-72.082522,-71.9063661,-72.6226304,-73.3059186,-71.8605895,-73.1392684,-73.0958473,-72.9586057,-71.975534],null,null,null,{"clickable":true,"draggable":false,"keyboard":true,"title":"","alt":"","zIndexOffset":0,"opacity":1,"riseOnHover":false,"riseOffset":250},["Achievement First Hartford Academy Inc.","Amistad Academy","Ansonia High School","Bethel High School","Bloomfield High School","Global Experience Magnet School","Branford High School","Aerospace/Hydrospace, Engineering and Physical Sciences HS at the Fairchild-Wheeler Magnet Campus","Bassick High School","Biotechnology, Research and Zoological Studies HS at the Fairchild-Wheeler Magnet Campus","Central High School","Information Technology and Software Engineering High School at the Fairchild-Wheeler Magnet Campus","Warren Harding High School","Bristol Central High School","Bristol Eastern High School","Brookfield High School","Canton High School","Capital Preparatory Harbor School Inc.","Academy of Aerospace and Engineering","Academy of Science and Innovation","Greater Hartford Academy of  the Arts High School - Full Time","Public Safety Academy","Cheshire High School","Bacon Academy","Common Ground High School","E. C. Goodwin Technical High School","Eli Whitney Technical High School","Ella T. Grasso Technical High School","Emmett O'Brien Technical High School","H. H. Ellis Technical High School","Henry Abbott Technical High School","J. M. Wright Technical High School","Norwich Technical High School","Oliver Wolcott Technical High School","Platt Technical High School","Vinal Technical High School","W. F. Kaynor Technical High School","Coventry High School","Cromwell High School","Alternative Center For Excellence","Danbury High School","Darien High School","Derby High School","East Granby High School","Nathan Hale-Ray High School","East Hampton High School","East Hartford High School","Stevens Alternate High School","East Haven High School","East Lyme High School","East Windsor High School","Ellington High School","Elm City College Preparatory School","Enfield High School","Explorations","Fairfield Ludlowe High School","Fairfield Warde High School","Glastonbury High School","Granby Memorial High School","Greenwich High School","Griswold High School","Robert E. Fitch High School","Guilford High School","Hamden High School","Bulkeley High School","Capital Preparatory Magnet School","Classical Magnet School","Culinary Arts Academy at Weaver High School","Global Communications Academy","Great Path Academy at MCC","HPHS Engineering and Green Technology Academy","HPHS Law and Government Academy","HPHS Nursing and Health Sciences Academy","High School, Inc.","Journalism and Media Academy","Kinsella Magnet School of Performing Arts","Pathways Academy of Technology and Design","Sport and Medical Sciences Academy","University High School of Science and Engineering","Highville Charter School","Jumoke Academy","Killingly High School","Connecticut River Academy","Marine Science Magnet High School of Southeastern Connecticut","Lyman Memorial High School","Ledyard High School","Litchfield High School","Daniel Hand High School","Manchester High School","Orville H. Platt High School","Middletown High School","Joseph A. Foran High School","Masuk High School","Montville High School","Naugatuck High School","New Britain High School","New Canaan High School","New Fairfield High School","Cooperative High School","Cortlandt V.R. Creed Health and Sport Sciences High School","Engineering - Science University Magnet School","James Hillhouse High School","Metropolitan Business Academy","New Haven Academy","Sound School","Wilbur Cross High School","New London High School","Science and Technology Magnet School of Southeastern Connecticut","Newington High School","Newtown High School","North Branford High School","North Haven High School","Wheeler High School","Brien McMahon High School","Center for Global Studies","Norwalk High School","Norwalk Pathways Academy at Briggs","Old Saybrook Senior High School","Oxford High School","Path Academy","Plainville High School","Plymouth Alternative High School","Terryville High School","Putnam High School","Housatonic Valley Regional High School","Valley Regional High School","Amity Regional High School","Wamogo Regional High School","Northwestern Regional High School","RHAM High School","Joel Barlow High School","Lewis S. Mills High School","Parish Hill High School","Coginchaug Regional High School","Nonnewaug High School","Pomperaug Regional High School","Woodland Regional High School","Haddam-Killingworth High School","Lyme-Old Lyme High School","E. O. Smith High School","Ridgefield High School","Rocky Hill High School","Seymour High School","Shelton High School","Simsbury High School","Somers High School","South Windsor High School","Southington High School","Stafford High School","Stamford Academy","Stamford High School","The Academy of Information Technology","Westhill High School","Stratford High School","Suffield High School","The Bridge Academy","The Gilbert School","Tourtellotte Memorial High School","Tolland High School","Torrington High School","Trumbull High School","Rockville High School","Lyman Hall High School","Mark T. Sheehan High School","Crosby High School","John F. Kennedy High School","Waterbury Arts Magnet School (High)","Waterbury Career Academy","Wilby High School","Waterford High School","Watertown High School","Conard High School","Hall High School","Westbrook High School","Weston High School","Staples High School","Wethersfield High School","Wilton High School","Windham High School","Windsor Locks High School","Windsor High School","Wolcott High School","Avon High School","Berlin High School","Learning Academy at Bloomfield","Bolton High School","Achievement First Bridgeport Academy","Bridgeport Military Academy","Metropolitan Learning Center for Global and International Studies","The Morgan School","A. I. Prince Technical High School","Bullard-Havens Technical High School","H. C. Wilcox Technical High School","Howell Cheney Technical High School","Windham Technical High School","Connecticut IB Academy","Arts at the Capitol Theater Magnet School (ACT)","Quinebaug Middle College","Farmington High School","Griswold Alternative School","Hartford Magnet Trinity College Academy","Francis T. Maloney High School","Jonathan Law High School","High School In The Community","Hill Regional Career High School","New Milford High School","Norwich Free Academy","Plainfield High School","Portland High School","Shepaug Valley School","Stonington High School","Bunnell High School","Thomaston High School","West Haven High School","Woodstock Academy"],null,{"showCoverageOnHover":true,"zoomToBoundsOnClick":true,"spiderfyOnMaxZoom":true,"removeOutsideVisibleBounds":true,"spiderLegPolylineOptions":{"weight":1.5,"color":"#222","opacity":0.5},"freezeAtZoom":false},null,null,null,null]}],"limits":{"lat":[41.0397676,41.9807467],"lng":[-73.6122393,-71.8605895]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

