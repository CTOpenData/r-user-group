# https://portal.ct.gov/sots/election-services/early-voting/early-voting-locations---2024-august-primary

library(tidyverse)
# pak::pkg_install("tidygeocoder")
# pak::pkg_install("leaflet")

early_voting <-
  read_tsv(I(
    "City or Town 	Name of Early Voting Location	Address of Early Voting Location
Andover	Town Hall - Registrars of Voters Office	17 School Road, Andover, CT  06232
Ansonia	Community Room at Hilltop Hose Company	80 Pulaski Highway, Ansonia, CT  06401
Ashford	Town Office Building ‐ Room 306 	5 Town Hall Road, Ashford, CT  06278
Avon	Town Hall - Meeting Room and All Purpose Room	60 West Main Street, Avon, CT  06001
Barkhamsted	Town Hall	67 Ripley Hill Road, Barkhamsted, CT  06063
Beacon Falls	Town Hall, Registrars of Voters Office Vestibule	10 Maple Avenue, Beacon Falls, CT  06403
Berlin	Town Hall Meeting Room - 104 and 113	240 Kensington Road, Berlin, CT  06037
Bethany	Town Hall Commissioner Room 	40 Peck Road, Bethany, CT  06524
Bethel	Municipal Center - Meeting Room A	1 School Street, Bethel, CT  06801
Bethlehem	Town Hall - Meeting Room	36 Main Street South, Bethlehem, CT  06751
Bloomfield	Town Hall (Town Council Chambers, Conference Room 2)	800 Bloomfield Avenue, Bloomfield, CT  06002
Bolton	Temporary Municipal Center	104 Notch Road, Bolton, CT  06043
Bozrah	Town Hall, Upstairs Meeting Room	1 River Road, Bozrah, CT  06334
Branford	Joe Trapasso Community House	46 Church Street, Branford, CT  06405
Bridgeport	Margaret Morton Government Center - Conference Room C 	999 Broad Street, Bridgeport, CT  06604
Bridgewater	Hilltop Center (Bridgewater Senior Center)	132 Hut Hill Road, Bridgewater, CT  06752
Bristol	Council Chambers	111 North Main Street, Bristol, CT  06010
Brookfield	Town Hall, Meeting Room 129 	100 Pocono Road, Brookfield, CT  06804
Brooklyn	Town Hall	4 Wolf Den Road, Brooklyn, CT  06234
Burlington	Town Hall	200 Spielman Highway, Burlington, CT  06013
Canaan	Town Hall - Meeting Room	108 Main Street, Falls Village, CT  06031
Canterbury	Registrars' Office and East Conference Room 	1 Municipal Drive, Canterbury, CT  06331
Canton	Canton Community Center, Room C	40 Dyer Avenue, Canton, CT  06019
Chaplin	Town Hall	495 Phoenixville Road, Chaplin, CT  06235
Cheshire	Town Hall	84 South Main Street, Cheshire, CT  06410
Chester	Town Hall, 2nd Floor Conference Room	203 Middlesex Avenue, Chester, CT  06412
Clinton	Town Hall - Rose Room	54 East Main Street, Clinton, CT  06413
Colchester	Town Hall - Meeting Room 	127 Norwich Avenue, Colchester, CT  06415
Colebrook	Town Hall - Meeting Room (2nd Floor) 	562 Colebrook Road, Colebrook, CT  06021
Columbia	Yeomans Hall, located within Columbia Town Hall 	323 Route 87, Columbia, CT  06237
Cornwall	Town Hall - Meeting Room	24 Pine Street, Cornwall, CT  06753
Coventry	Town Hall / Registrar's Office	1712 Main Street, Coventry, CT  06238
Cromwell	Town Hall ‐ Elections Department 	41 West Street ‐ 2nd Floor, Cromwell, CT  06416
Danbury	City Hall - Conference Room 2	155 Deer Hill Avenue, Danbury CT  06810
Darien	Town Hall Room 119	2 Renshaw Road, Darien, CT  06820
Deep River	Deep River Public Library	150 Main Street, Deep River, CT  06417
Derby	City Hall, Registrar of Voters Office	1 Elizabeth Street, Derby, CT  06418
Durham	Town Hall, 2nd Floor Conference Room	30 Town House Road, Durham, CT  06422
East Granby	Mary Ellen Brown Senior Center & Community Center	20 Center Street, East Granby, CT  06026
East Haddam	Municipal Office Center	1 Plains Road, Moodus, CT  06469
East Hampton	Town Hall, Conference Room, First Floor	1 Community Drive, East Hampton, CT  06424
East Hartford	Town Hall	740 Main Street, East Hartford, CT 06108
East Haven	Town Hall (Lower-level Conference Room) 	250 Main Street, East Haven, CT 06512
East Lyme	Community Center	37 Society Road, Niantic, CT  06357
East Windsor	Town Hall Annex	25 School Street, East Windsor, CT  06088
Eastford	Town Office Building (Lower-level) 	16 Westford Road, Eastford, CT 06242
Easton	Easton Public Library Community Room	691 Morehouse Road, Easton CT  06612
Ellington	Ellington Registrars of Voters' Office (Lower-level of the Town Hall)	55 Main Street, Ellington, CT  06029
Enfield	Town Hall ‐ Enfield Room (Lower-level) 	820 Enfield Street, Enfield CT 06082
Essex	Town Hall 	29 West Avenue, Essex, CT 06426
Fairfield	Old Town Hall (2nd Floor)	611 Old Post Road, Fairfield, CT  06824
Farmington	Town Hall Pavilion	1 Monteith Drive, Farmington, CT  06032
Franklin	Town Hall (Upper-level)	7 Meetinghouse Hill Road, Franklin, CT  06254
Glastonbury	Town Hall, Lower-level, Conference Room D	2143 Main Street, Glastonbury, CT  06033
Goshen	Town Hall - Conference Room	42 North Street, Goshen, CT  06756
Granby	Town Hall	15 North Granby Road, Granby, CT  06035
Greenwich	Town Hall	101 Field Point Road, Greenwich, CT  06830
Griswold	Town Hall (Meeting Room)	28 Main Street, Jewett City, CT  06351
Groton	Community Center Classroom / ROV Office	61 Fort Hill Road Groton, CT  06340
Guilford	Town Hall	31 Park Street, Guilford, CT  06437
Haddam	Haddam Firehouse	439 Saybrook Rd, Higganum, CT  06441
Hamden	Thornton Wilder Auditorium (Miller Library)	2901 Dixwell Ave, Hamden, CT  06518
Hampton	Office of Registrar of Voters in the Town Hall 	164 Main Street, Hampton, CT 06247
Hartford	Registrar of Voters Office	550 Main Street, Hartford, CT  06103
Hartland	Town Hall (Center Meeting Room)	22 South Road, East Hartland, CT 06027‐1500
Harwinton	Town Hall (Assembly Room)	100 Bentley Drive, Harwinton, CT  06791
Hebron	Town Hall - Marion Celio Meeting Room (1st Floor)	15 Gilead Street, Hebron, CT  06248
Kent	Town Hall, 2nd Floor Registrars of Voters Office	41 Kent Green Boulevard, Kent, CT  06757
Killingly	Town Hall, Registrars' Office	175 Main Street, Danielson, CT  06239
Killingworth	Town Hall - Meeting Room	323 CT 81, Killingworth, CT  06419
Lebanon	Town Hall - Registrars' Office 	579 Exeter Rd, Lebanon CT 06249
Ledyard	Town Hall	741 Colonel Ledyard Highway, Ledyard, CT  06339
Lisbon	Newent Meeting House	12 South Burnham Highway, Lisbon CT  06351
Litchfield	Town Hall - Registrars' Office	74 West Street, Litchfield, CT  06759
Lyme	Town Hall, Meeting Room 	480 Hamburg Road, Lyme, CT 06371
Madison	Town Hall	8 Campus Drive Madison, CT  06443-2563
Manchester	Mahoney Recreation Center Leisure Lab 	110 Cedar Street, Manchester, CT 06040
Mansfield	Town Hall, Council Chambers	4 South Eagleville Road, Storrs, CT  06268
Marlborough	Town Hall - Large Conference Room	29 North Main Street, Marlborough, CT  06447
Meriden	City Hall	142 East Main Street, Meriden, CT  06450
Middlebury	Town Hall 	1212 Whittemore Road, Middlebury, CT 06762
Middlefield	Middlefield Community Center 	405 Main Street, Middlefield, CT 06455
Middletown	City Hall (Council Chamber)	245 DeKoven Drive, Middletown, CT  06457
Milford	Parsons Government Center	70 West River Street Milford, CT  06460
Monroe	Town Hall (Room 208)	7 Fan Hill Road, Monroe, CT  06468
Montville	Town Hall - Registrar of Voters Office	310 Norwich - New London Turnpike, Uncasville, CT  06382
Morris	Town Hall ‐ Registrar of Voters Office 	3 East Street, Morris, CT 06763
Naugatuck	Naugatuck Parks and Rec 	607 Rubber Avenue, Naugatuck, CT 06770
New Britain	Senior Center - Vermont Room	55 Pearl Street, New Britain CT  06051
New Canaan	Town Hall (Conference Room B First Floor) 	77 Main St, New Canaan, CT 06840
New Fairfield	Town Hall 	4 Brush Hill Road, New Fairfield, CT 06812
New Hartford	Town Hall, 1st Floor	530 Main Street, New Hartford, CT  06057
New Haven	City Hall	165 Church Street, Second Floor, New Haven, CT 06510
New London	City Hall 	181 State Street, New London, CT 06320
New Milford	Town Hall, Conference Room, Basement Level	10 Main Street, New Milford, CT  06776
Newington	Council Chambers and Conference Room (1st Floor)	200 Garfield Street, Newington, CT  06111
Newtown	Municipal Center, Registrar of Voters Office	3 Primrose Street, Newtown, CT  06470
Norfolk	Town Hall, 2nd Floor Meeting Room	19 Maple Avenue, Norfolk, CT 06058
North Branford	Town Hall Council Chambers	909 Foxon Road, North Branford, CT  06471
North Canaan	Town Hall, Office of the Registrars of Voters 	100 Pease Street, Canaan, CT 06018
North Haven	Town Hall, 1st Floor Conference Room	18 Church Street, North Haven, CT  06473
North Stonington	Education Center	298 Norwich-Westerly Road, North Stonington, CT  06359
Norwalk	City Hall	125 East Avenue, Norwalk, CT  06851
Norwich	City Hall	100 Broadway, Norwich, CT  06360
Old Lyme	Old Lyme Mezzanine Meeting Room 	52 Lyme Street, Old Lyme, CT 06371
Old Saybrook	Town Hall, Conference Room 1st Floor 	302 Main Street, Old Saybrook, CT 06475
Orange	Town Hall, Registrar of Voters Office	617 Orange Center Road, Orange, CT  06477-2499
Oxford	Oxford Town Hall - Meeting Room	486 Oxford Road, Oxford, CT  06478
Plainfield	Town Hall Lobby, Town Clerk's Office 	8 Community Avenue, Plainfield, CT 06374
Plainville	Town Hall	1 Central Square Plainville, CT  06062
Plymouth	Town Hall	80 Main Street, Terryville, CT  06786
Pomfret	Town Office Building - Conference Room	5 Haven Road, Pomfret Center, CT  06259
Portland	Town Hall Conference Room ‐ Ground floor 	33 E Main Street, Portland, CT 06480
Preston	Preston Public Library	389 Route 2, Preston, CT  06365
Prospect	Town Hall (Lower-level Meeting Room)	36 Center Street, Prospect, CT  06712
Putnam	Town Municipal Complex - Room #101	200 School Street, Putnam, CT  06260
Redding	Redding Community Center	37 Lonetown Road, Redding, CT  06896
Ridgefield	Town Hall - Large Conference Room	400 Main Street Ridgefield CT  06877
Rocky Hill	Town Hall	761 Old Main Street, Rocky Hill, CT 06067
Roxbury	Town Hall ‐ Lower Conference Room 	29 North Street, Roxbury, CT 06783
Salem	Town Hall, Registrars' Office	270 Hartford Road, Salem, CT 06420
Salisbury	Town Hall - Upstairs Meeting Room	27 Main Street, Salisbury, CT  06068
Scotland	Town Hall, First Selectman's Office	9 Devotion Road, Scotland, CT  06264
Seymour	Seymour Town Hall (Meeting Room)	1 First Street, Seymour, CT  06483
Sharon	Town Hall - Chapin Meeting Room	63 Main Street, Sharon, CT  06069
Shelton	City Hall, Room 104	54 Hill Street, Shelton, CT  06484
Sherman	Mallory Town Hall 	9 Route 39, North Sherman, CT 06784
Simsbury	Town Hall - Main Meeting Room	933 Hopmeadow Street, Simsbury, CT  06070
Somers	Town Hall Conference Room (Lower-level) 	600 Main Street, Somers, CT 06071
South Windsor	Town Council Chambers	1540 Sullivan Avenue, South Windsor, CT  06074
Southbury	Town Hall Room 205 	501 Main Street South, Southbury, CT 06488
Southington	Town Hall, Lower-level Conference Room	75 Main Street, Southington, CT  06489
Sprague	Town Hall	1 Main Street, Baltic, CT  06330
Stafford	Warren Memorial Town Hall, Veterans Meeting Room	1 Main Street, Stafford, CT  06076
Stamford	Government Center Cafeteria, Fourth Floor	888 Washington Boulevard, Stamford, CT  06904
Sterling	Sterling Municipal Building - Senior Center	1183 Plainfield Pike, Oneco, CT  06373
Stonington	Town Hall - Meeting Room 	152 Elm Street, Stonington, CT 06378
Stratford	Baldwin Center (Studio Room) 	1000 West Broad Street, Stratford, CT 06615
Suffield	Town Hall, Registrar of Voter's Office	83 Mountain Road, Suffield, CT  06078
Thomaston	Town Hall / Senior Center	158 Main Street, Thomaston, CT  06787
Thompson	Thompson Town Hall	815 Riverside Drive, North Grosvenordale, CT  06255
Tolland	Town Hall - Conference Room B	21 Tolland Green, Tolland, CT  06084
Torrington	City Hall - Auditorium	140 Main Street, Torrington, CT  06790
Trumbull	Town Hall - Registrars' Office and adjacent corridor if needed	5866 Main Street, Trumbull, CT  06611
Union	Town Hall - Meeting Room 	1043 Buckley Highway, Union CT 06076
Vernon	Center 375	375 Hartford Turnpike #118, Vernon, CT  06066
Voluntown	Town Hall - Meeting Room	115 Main Street, Voluntown, CT  06384
Wallingford	Town Hall (1st Floor)	45 South Main Street, Wallingford, CT  06492
Warren	Town Hall - Meeting Room 	50 Cemetery Road, Warren, CT 06754
Washington	Bryan Memorial Town Hall - Registrars' Office (Lower-level)	2 Bryan Plaza, Washington Depot, CT  06794
Waterbury	Veterans Hall	City Hall, 235 Grand Street, Waterbury, CT  06702
Waterford	Town Hall - Registrar of Voters Office / Appleby Room	15 Rope Ferry Road, Waterford, CT  06385
Watertown	Park and Rec - Conference Room	61 Echo Lake Road, Watertown, CT  06795
West Hartford	Town Hall - Auditorium	50 South Main Street, West Hartford, CT  06107
West Haven	City Hall Board of Education side of City Hall 1st Floor Separate Entrance	355 Main Street, West Haven, CT  06516
Westbrook	Town Hall	866 Boston Post Road, Westbrook, CT  06498
Weston	Town Hall - Meeting Room	56 Norfield Road, Weston, CT  06883
Westport	Town Hall, Rm 201	110 Myrtle Avenue, Westport CT  06880
Wethersfield	Town Hall ‐ Elections Office	505 Silas Deane Highway, Wethersfield, CT  06109
Willington	Town Office Building - Selectmen's Conference Room	40 Old Farms Road, Willington, CT  06279
Wilton	Town Hall - Room B	238 Danbury Road, Wilton, CT  06897
Winchester	Room 203, 2nd Floor of Town Hall	338 Main Street, Winstead, CT  06098
Windham	Windham Town Hall - Bellingham Auditorium	979 Main Street, Willimantic CT  06226
Windsor	Town Hall, Ludlow Room	275 Broad Street, Windsor, CT  06095
Windsor Locks	Town Hall	50 Church Street, Windsor Locks, CT  06096
Wolcott	Town Hall, Registrars' Office	10 Kenea Ave, Wolcott, CT  06716
Woodbridge	Town Hall - Office	11 Meetinghouse Lane, Woodbridge, CT  06525
Woodbury	Shove Building - Conference Room	281 Main Street South, Woodbury, CT  06798
Woodstock	Town Hall - Meeting Room A	415 Route 169, Woodstock, CT  06281"
  ),
  show_col_types = FALSE)

early_voting <-
  read_tsv("october_demo/voting_locations.tsv",
           show_col_types = FALSE)

glimpse(early_voting)

early_voting_geos <- 
  tidygeocoder::geocode(early_voting, 
                        address = `Address of Early Voting Location`, 
                        method = "arcgis")

early_voting_geos %>% filter(is.na(lat))

early_voting_map <- 
  early_voting_geos %>% 
  mutate(
    special_label = paste(`City or Town`, `Name of Early Voting Location`),
    special_popup = paste0(`Name of Early Voting Location`, ", ", `Address of Early Voting Location`)
  )

glimpse(early_voting_map)

ggplot(
  data = early_voting_map,
  aes(long, lat),
  color = "grey99"
) +
  geom_point() +
  theme_minimal()

leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::setView(
    lng = -72.74587984553425,
    lat = 41.60828560557468,
    zoom = 8.8
  )  %>%
  leaflet::addMarkers(
    data = early_voting_map,
    label = early_voting_map$`City or Town`,
    popup = early_voting_map$special_popup
  ) 
