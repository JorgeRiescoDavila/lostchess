
program lostchess
  ! use iso_fortran_env, only: int64

  implicit none  


  !game length
  integer,parameter::max_moves_per_game = 1024
  integer,parameter::max_moves_listed = 8192

  !team codes
  integer,parameter::team_white = 0
  integer,parameter::team_black = 1
  integer,parameter::team_none = 2
  integer,parameter::team_both = 3

  !piece codes
  integer,parameter::empty = 0
  integer,parameter::wp = 1
  integer,parameter::wn = 2
  integer,parameter::wb = 3
  integer,parameter::wr = 4
  integer,parameter::wq = 5
  integer,parameter::wk = 6
  integer,parameter::bp = 7
  integer,parameter::bn = 8
  integer,parameter::bb = 9
  integer,parameter::br = 10
  integer,parameter::bq = 11
  integer,parameter::bk = 12
  !piece lookups
  integer,parameter,dimension(0:12)::get_color = (/ team_none, &
  & team_white,team_white,team_white,team_white,team_white,team_white, &
  & team_black,team_black,team_black,team_black,team_black,team_black /)
  logical,parameter,dimension(0:12)::is_queen_rook = (/ .false., &
  & .false.,.false.,.false.,.true.,.true.,.false., &
  & .false.,.false.,.false.,.true.,.true.,.false. /)
  logical,parameter,dimension(0:12)::is_queen_bishop = (/ .false., &
  & .false.,.false.,.true.,.false.,.true.,.false., &
  & .false.,.false.,.true.,.false.,.true.,.false. /)
  logical,parameter,dimension(0:12)::is_knight = (/ .false., &
  & .false.,.true.,.false.,.false.,.false.,.false., &
  & .false.,.true.,.false.,.false.,.false.,.false. /)
  logical,parameter,dimension(0:12)::is_king = (/ .false., &
  & .false.,.false.,.false.,.false.,.false.,.true., &
  & .false.,.false.,.false.,.false.,.false.,.true. /)
  logical,parameter,dimension(0:12)::is_pawn = (/ .false., &
  & .true.,.false.,.false.,.false.,.false.,.false., &
  & .true.,.false.,.false.,.false.,.false.,.false. /)
  !piece directions
  integer,dimension(0:7),parameter::directions_rook        = (/  1, 16, -1,-16,  0,  0,  0,  0/)
  integer,dimension(0:7),parameter::directions_bishop      = (/ 15, 17,-15,-17,  0,  0,  0,  0/)
  integer,dimension(0:7),parameter::directions_rook_bishop = (/  1, 16, -1,-16, 15, 17,-15,-17/)
  integer,dimension(0:7),parameter::directions_knight      = (/ 14, 18, 31, 33,-14,-18,-31,-33/)

  !important squares
  integer,parameter::ob = -1
  integer,parameter::a1 = 0
  integer,parameter::b1 = 1
  integer,parameter::c1 = 2
  integer,parameter::d1 = 3
  integer,parameter::e1 = 4
  integer,parameter::f1 = 5
  integer,parameter::g1 = 6
  integer,parameter::h1 = 7
  integer,parameter::a8 = 112
  integer,parameter::b8 = 113
  integer,parameter::c8 = 114
  integer,parameter::d8 = 115
  integer,parameter::e8 = 116
  integer,parameter::f8 = 117
  integer,parameter::g8 = 118
  integer,parameter::h8 = 119

  !castling permissions
  integer,parameter::cp_wk = 1
  integer,parameter::cp_wq = 2
  integer,parameter::cp_bk = 4
  integer,parameter::cp_bq = 8
  integer,parameter::cp_all = cp_wq+cp_wk+cp_bq+cp_bk
  integer,dimension(0:127)::cp_table

  !move structure
  type type_move
    integer::ini
    integer::fin
    logical::is_capture
    integer::promotion_pie
    logical::is_pawnstart
    logical::is_enpassant
    logical::is_castling
    integer::captured_pie
  end type
  type(type_move),parameter::move_null = type_move(ob,ob,.FALSE.,empty,.FALSE.,.FALSE.,.FALSE.,empty)

  !hash tables structure
  type type_hash_table
    integer(8),dimension(0:12,0:127)::board
    integer(8)::side
    integer(8),dimension(0:15)::cp
    integer(8),dimension(-1:127)::ep
  end type
  type(type_hash_table)::hash_table

  !board
  integer,dimension(0:127)::board

  !game state structure
  type type_state
    integer::side
    integer::cp
    integer::ep
    integer::rule50
    integer(8)::hash
    integer,dimension(0:1)::kings
  end type
  type(type_state)::state

  !number of moves
  integer::ply

  !game history
  type type_history
    integer::cp
    integer::ep
    integer::rule50
    integer(8)::hash
    type(type_move)::move
  end type
  type(type_history),dimension(0:max_moves_per_game-1)::hist

  !moves list
  integer,dimension(0:max_moves_per_game-1)::moves_list_ind
  type(type_move),dimension(0:max_moves_listed-1)::moves_list
  integer,dimension(0:max_moves_listed-1)::moves_score

  !search engine
  integer,parameter::inf = 2**30
  integer,parameter::mate_score = 100000
  integer,parameter::ulti_depth = 32
  !search engine
  type type_search
    !constants
    integer,dimension(0:12,0:127)::piesq_mg
    integer,dimension(0:12,0:127)::piesq_eg
    integer,dimension(0:12)::piece_value
    !variabless
    integer::ss_material(0:1) 
    integer::ss_position_mg(0:1)
    integer::ss_position_eg(0:1)
    integer::ply
    integer::best_score
    type(type_move)::best_move
    type(type_move),dimension(0:1,0:ulti_depth)::killers
    !statistics
    integer::nodes,nodes_quies,chekmates,stalemates,fh,fhf
    real::t_ini
    real::t_cur
  end type
  type(type_search)::srch

  !principal variation table
  integer,dimension(0:ulti_depth-1)::PV_length
  type(type_move),dimension(0:ulti_depth-1,0:ulti_depth-1)::PV_table

  !transposition table
  !aparently storing all nodes as exact is better, for some reason nodes go down from 77k to 67k
  !it can be caused becuase this is a way to narrow window's search, so maybe it can malfunciton 
  integer,parameter::node_is_pv = 0
  integer,parameter::node_is_alpha = 0
  integer,parameter::node_is_beta = 0
  integer,parameter::TT_size = 2**16
  integer(8),dimension(0:TT_size-1,0:3)::TT
  integer::TT_reads,TT_saves

  !testing variables
  integer::selected_option,winner  

  !init program
  call init()

  do while (.true.)
    write(*,*) 'select options -----------------------------------------------------------------'
    write(*,*) '1 PVP | 2 PVE | 3 PerfTest | 4 EngineTest'
    write(*,*) '--------------------------------------------------------------------------------'
    read*, selected_option
    select case (selected_option)
      case (1)
        call restart_game()
        call write_board()
        do while(.true.)
          call player_handler()
          winner = check_end()
          if(winner /= team_none)then
            write(*,*)'WINNER ',winner
            exit
          end if
        end do
      case (2)
        call restart_game()  
        call write_board()        
        do while(.true.)
          call search_handler(6,3.)
          call make_move(srch%best_move)
          call write_board()
          winner = check_end()
          if(winner /= team_none)then
            write(*,*)'WINNER ',winner
            exit
          end if
          call player_handler()
          winner = check_end()
          if(winner /= team_none)then
            write(*,*)'WINNER ',winner
            exit
          end if
        end do
      case (3)
        call restart_game()
        call perft_handler()
      case (4)
        call test_engine()
      case default
        exit
    end select
  end do

  contains

!RESTART

  subroutine init()
    !castle permission updater table
    cp_table = cp_all
    cp_table(a1) = cp_table(a1) - cp_wq
    cp_table(e1) = cp_table(e1) - cp_wk - cp_wq
    cp_table(h1) = cp_table(h1) - cp_wk
    cp_table(a8) = cp_table(a8) - cp_bq
    cp_table(e8) = cp_table(e8) - cp_bk - cp_bq
    cp_table(h8) = cp_table(h8) - cp_bk
    call init_hash_table()
    call init_piesq_table()
  end subroutine

  subroutine init_hash_table()
    integer::sq,pie,cp_ind,ind
    integer(8),dimension(0:848)::runif
    integer(8),dimension(0:848)::runif2

    !much better with a more quality prng
    runif = (/ &
      & 1368675251, 1891880796,  700317253, 1739708780,  651533054, 2034454733, 1403393136, 1459392435, 1690483370, &
      &  515898766, 1774492347, 2126099084, 1928990136,  941175464,  614589881,   93915237, 1682327365,  603890448, &
      &   77824578, 1044779211, 1637743915, 1740701981, 1802680770,   24872660, 1889718487, 1093971559, 1827807901, &
      &  201034989, 1812439150,  237061379, 1374011684,  867095669,  512912468,  173592560,  915311602, 2096335550, &
      &  174691649, 1811085459,  895976538, 1291364740,  487833643,  522851953, 1665400754,   95569021,  253069350, &
      & 1678923300,  628922307,  405260802,   70469241,  301302659,  617044762,  800382971, 2092557816,  170720740, &
      & 1323407939, 1266501449, 2100661479, 1450633190, 1197698875, 2026857355, 1493622740,   22639567, 1324877131, &
      &  331865319,  139976822, 1146697312, 1774239402,  957225684, 1259585569, 1342485792,  736700813, 1774929579, &
      & 1616693891, 1009197804, 1468286637, 1873676712,  479334716,  257022746, 1816810744, 1242124778,  714463002, &
      & 1680295932,  272783368,  589792906, 1464376093,  992586424,  985911754, 1071507986,  161000905, 1362358246, &
      & 1425969532, 1204406163,  175123052, 1646772745, 2079909172, 1103029295, 1523952001,  358812253,  450897057, &
      & 1883585801,  428251962,  184303180,  871297259,  168872712,  327707807,  358690951, 1179009248, 1774558825, &
      & 1161346830, 1832735179, 1878146682,  672659655, 2138741910,  578713973,  664577404,  478109203,  638507223, &
      &  662174269,  744869771,  339996977,   69956676,  991076414, 1335023536,    7405682, 1947480647, 1981370246, &
      & 1728879968,  911889213, 2103521604, 1773681156, 1639445608,  506557139, 1123122198, 1734282179, 1514947212, &
      & 1221037147,  845501179,  970380948, 1641146629, 1918100650, 1729927056,  862554946,  356471597,  684030059, &
      &  453795799,  632269886,  592072808,   39315008, 1197532780,  700552620,  930911976,  664241197, 1390467640, &
      &  529547208,   63163986,  590870270, 1133154655,  854062662,  837245827,  264957692, 1012858234,  461768734, &
      & 1129659399, 1852283934,  443814975, 1232972618,  584691057,  892642857,  880758452,  771557392, 1093271014, &
      & 1888012976,  488135845, 1352134201,  698965884,  103772161,  794097625, 1446101864,  977956295,  741743903, &
      & 1163906952, 1727285306, 1088989154,  555277219, 1504042350, 1236589745, 1429494521, 1225402844,  916401482, &
      & 1273520557, 1148858044,  494634887, 1386880230, 1912646307, 2131272033, 2092846160,  666724257,  361033912, &
      & 1866868585,  733536686, 1645942185, 1463408470, 2101625716, 1967410506,  377376420, 2082042893, 1798558203, &
      &  776960029, 1336311393, 1520133638, 1128169705,   91329782,  348637257,  808469209,  419227013,  800941389, &
      & 1108131488,  421851252, 1878018706, 1684747861, 1365393682,  498977530, 2058491384,  411323555,  786370240, &
      & 1580276331,  659150119, 1558682832, 1990893926,  754679093, 1356124838, 1175275737, 1135740051, 1715798236, &
      & 1481434065,  494625495,  811593617, 1180965475,  980388849,  414224384, 1911357489, 1120482046,  164314072, &
      &   15628196, 1634952816,  501318511, 2043033107, 1494539244, 2119104635, 2083929149,  315128585, 1373793725, &
      & 1329944386,  619569047, 1807633336, 1574685114, 1510508678,  559686872, 2009766648,    8274532,  756490359, &
      &  157644343, 1710374322, 1105834764, 1390244568,  571009286, 1720241988,  267775211,  312167459, 1705118907, &
      &  809534895, 1343260457, 1350920866, 2052782081, 1907201837, 1652960554, 1414764605, 2121997273, 1595600434, &
      &  474968201, 1708394436,  644710369, 2147064334,  200402271, 1963200010, 2000643767,  362365252,  319177518, &
      & 1055057941, 1421227095,  243013990, 1302942106,   26440961, 1208906686,   16618804, 1956836130,  509749934, &
      &  469855099, 1714850879, 2022591826, 1260672159,  759842508, 1720607579,  196585683, 1642393186,  185558894, &
      & 1563892861, 1572420420, 1138212377, 2118253011, 1246465578, 1574982123,  208932061,  198399331,   36241262, &
      & 1273186794, 1323908930, 1613552756,  269939859, 2107344246,  688583843,  452882078, 1054653496, 1541198831, &
      &  493538432,  814621432, 1774154953,  636555274,  367717727, 1418616810, 1453678043,  494940302,  258131094, &
      &    6463741,  163582784,  694379663, 1381865788,   33275584, 1268689317,  229373924, 1257648985,  613705225, &
      &  298245027, 1483471461, 1812434500, 1094171683,  908585755,  474072084, 1608830879,  759676666,  908808591, &
      &  617676584, 1240846196,  288937466, 1597387965, 1680661494,  626440393, 1452998626, 1249211605, 1352514526, &
      & 1939374980, 1293411132,  635760990, 1351342715, 1669402521,  971107193, 1360598653, 1021600497,  207608344, &
      &  559056597,  957550632, 1979552134, 1289072581,  999016145,  998770724, 1193318150,  994775377, 1915288613, &
      & 1413349757, 1144626648, 1084227657, 1426613940,  767397953,  471749215, 1469511508, 1666971202, 2011396370, &
      &  863242629, 1190115024,  540780229,  794978229,  764512189, 1558362276, 1307435488, 1768835553, 1323629778, &
      & 2095868496,  836325077, 2107314822,  130660082,  563435039,  664145559, 1499473107,  486615559,  603683118, &
      & 1463921297,  845100803,  719656946,   92713341, 1126422330,  248132623, 1068429874,  981645630,  554559708, &
      &  102249205,  973838634, 1986427334, 1258917958, 1109661635,  687694634, 1947883707,   74569437, 1855620724, &
      & 1157861777, 1835660856, 1117740594,   44064648, 2013409135, 1693900691,  240850317, 1407600302, 1733653774, &
      &  262249414, 1611556571,   31043170,  488621028, 2050344900, 1210932003, 1839420181,  262081129,  509989091, &
      & 1162845655,  242908908,  327097477,  548488053,  318107385, 1789079610, 1382865639,   22645943,  318398656, &
      & 1288903428,  159514082, 1560355979,  349812128,   97332798,  230137582, 1282895986, 1729712102,  707010134, &
      & 1658455404, 1552327075,  568690469,    9147826,  494619161,  771044742, 1367817359, 1687039500, 2140631419, &
      &  934436322, 1683917466, 1863194320, 1980876215,  909957415, 2093958990, 2092997899, 2050035683,  398539797, &
      &  457137331, 1652348103,  233376211, 1214091511, 1188937674, 1444356906, 1041160832, 1545507444, 1904866530, &
      &  196853792,  357017975, 1988537821, 1069745844,   78385600,  124018427, 1264799883,  438822637,  681928695, &
      & 1287838197, 1131806394,  958040300, 1609527574, 1848289373,  711207397, 1627088412, 1820073558, 1006897883, &
      & 1011692324,  843714584,  529601150,  193653362,  110011313,  783708896, 1127518515,  261602748, 1748856404, &
      &  413212630,  577719883, 1276398554,  131637272, 1233179052, 1203821236,  643730744, 1747290293, 1886550994, &
      &  932105648,  553376438, 1921844492,  877522823,  494812329, 1566939119, 2109464455,  409098265, 1408915939, &
      &  516588622,  698453109, 1058368654, 2027455537,  400059243,  521293517, 1744898491,  948642597, 1164479459, &
      & 1196005634,  803476364, 1894425159, 1552654458, 2033101128, 1746843125, 1507101051,   21608228, 1970828525, &
      &  748085850, 1626477655, 1980096341, 1911214003, 1324839334, 1218018017, 1138425583,  598044894, 1000239965, &
      &   35562766, 1203044340,  338714144,  213962590, 1519515783, 1079071268, 1642502730,  943819983,  214154383, &
      &  485483187,  576666746,  663224537,  233823726,  490533397,   12166487, 1375145916,  574688759, 1229211900, &
      & 1039336071, 1926927539, 1747829208,   65541031, 1858981297, 1685390766, 1375902672, 1929833046, 2041586159, &
      & 1367798769, 2099036341,  548958229, 1337572621,  140779742, 1033037811, 1065536056, 1032117466, 1903149673, &
      & 1476304735,  263830187, 1011956358, 1917890683, 1190682788, 1045115830, 2121871206,  504182111, 2141351251, &
      & 2062197543, 1785549991, 1480505012, 1118307649,  912648733, 1345934177, 1963546073,  541141507, 1465181085, &
      &   44479336, 1324056472, 1763555186, 1337471229, 1400425308,  227556657, 2042909443, 1343440793,   12226849, &
      & 1158602358,  243041595,  264899034,  579650505, 1459541980, 1138244802, 1303656556, 1792246402, 1975196494, &
      & 1535595092,  999704175, 1784153779, 2092395985, 1513740011,  584550357, 1884337711, 1781723607, 1169976493, &
      &  506730255, 1660636896, 1687441285, 1715992348, 1410397575,  293967954,  834012093, 1750963808,  332308931, &
      & 1948209510,  416218232,  729745794,  832871411, 1820749023,  613147604,  391109575, 1462797014, 1771121107, &
      & 1639266679,  929545208, 1914677570,  424426096,  929269498, 1230194046, 1486198088,  535621122, 1787931421, &
      & 2136218848, 1489680225,  127687121, 1685257292,  484944940,  328755494,  573571695,  933862407,  396345121, &
      &  968783605, 2032225935,   46200306,  509726532, 1158128330, 1471528142,  298788765, 2133833282,  498210918, &
      & 1476541427, 1125497722,  146887462,  495466826, 1006003123,  117304508, 1271701341,  957589528, 1320873791, &
      &  909303905, 1778657935, 1432629120, 1817834267,  263161021, 1355368842, 1260863729,  314187877,   96944754, &
      &  312702613,  882255054,  316474831, 2045166031,   44694171,  384281515, 1472612853,  107622411,  111214481, &
      &  999553757, 1234639115, 1247490051, 1323176366,  309304612, 1084598753,   56364327,  970456102,  614128207, &
      &  345936167, 1552613337,  267735120,  243681299, 1824460563,  346532727,  930725588, 2082572948, 1979371680, &
      &  401417198, 1576533174, 1573889183,  167787651,  449518646, 1993073401,  988002041,  395795242,  752649766, &
      &  997239895, 1564917503,  130994717, 1088351501, 1191957495, 1734273297,  816007680, 1583159763, 1163896779, &
      &  959572419,  577356935,  195479170, 1610920079,  888958697,  892730463, 1985132519, 2028795034, 2073052801, &
      & 1978468401,  817876364, 1985507973, 1959481115, 1011761907,  937034151, 1880052996, 1616828929,  927985794, &
      & 1766175566,   91773396,  101287804,  667090049,  399948240,  645704978,  649625114,  913803261,  735051348, &
      & 1332693363,  499986423, 1853244871, 1418909652,  525087801,  189968772, 1028159526, 1433663950,  825350199, &
      &  825958631, 1374132224, 1776512269, 1663420990, 2134944232, 2125456794, 1503898652,  547912871,  256171456, &
      & 1693347303, 1081909382, 1870294216, 1826702407,  130834722, 2069032623, 1206596992,  472947606,  597400509, &
      &  782430017,  955324537,  878830927,  357316804, 1993658758,  692241135,  436414020, 1428159859, 1836980756, &
      & 1832389024, 1882949310,  109929389, 1608418058, 1665101702, 1583216675,  595712313, 1173808362, 2102661419, &
      & 1289660028, 1709248639, 1636548224, 1315734498, 1989243061,  100538733, 1811861607, 1482104704,  754766874, &
      &  611171000,    8556815,  529733985, 2085475007, 1965607736,  242536590,  901851600, 1051305198,  262860582, &
      &  708548420,  513206713,  475288924, 2030204209,  967133772,  724102234, 1696197022,  771388905,  549843727, &
      & 1622983668,  208958848,  261411685 /)

    do ind = 0,848
      runif2(ind) = floor(abs(sin(ind+1.)*2**31))
      runif2(ind) = ishft(runif2(ind),32)
      runif(ind) = runif(ind)+runif2(ind)
    end do
    
    ind = 0
    
    hash_table%board = 0
    do pie = 1,12
      do sq = 0,127
        if(iand(sq,136) == 0)then
          hash_table%board(pie,sq) = runif(ind)
          ind = ind+1
        end if
      end do
    end do
    
    hash_table%side = runif(ind)
    ind = ind+1
    
    do cp_ind = 0,15
      hash_table%cp(cp_ind) = runif(ind)
      ind = ind+1
    end do
    
    hash_table%ep(ob) = 0
    do sq = 0,127
      if(iand(sq,136) == 0)then
        hash_table%ep(sq) = runif(ind)
        ind = ind+1
      end if
    end do

  end subroutine

  subroutine restart_game()    
    call set_fen('rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1')
    !restart history
    hist%cp = cp_all
    hist%ep = ob
    hist%rule50 = 0
    hist%move = move_null
    !reastart list of moves
    moves_list_ind = 0
    moves_list = move_null
  end subroutine

  function hash_position() result(hash)
    integer(8)::hash
    integer::sq,pie
    hash = 0
    do sq = 0,127
      if(iand(sq,136) == 0)then
        pie = board(sq)
        hash = ieor(hash,hash_table%board(pie,sq))
      end if
    end do
    hash = ieor(hash,hash_table%side)
    hash = ieor(hash,hash_table%ep(state%ep))
    hash = ieor(hash,hash_table%cp(state%cp))
  end function

  subroutine inc_hash(inc)
    integer(8)::inc
    state%hash = ieor(state%hash,inc)
  end subroutine

!PLAYER

  subroutine player_handler()
    integer::king_sq
    type(type_move)::m=move_null
    character(len=5)::alg
    call gen_moves(.true.)
    write(*,'(A5)',advance='no') 'move '
    do while(.true.)
      read*, alg
      m = alg2move(alg)
      if(.not. equal_m(m,move_null))then
        call make_move(m)
          king_sq = state%kings(ieor(1,state%side))
          if(.not. is_attacked(king_sq,state%side))then
            exit
          end if
        call undo_last_move()
      end if
    end do
    call write_board()
  end subroutine

!END

  function check_end() result(winner)
    integer::winner,rule50_ind,reps,king_sq,legal_moves,m_ind
    winner = team_none
    reps = 1
    !50 move rule
    if(state%rule50 == 100)then
      winner = team_both
      return
    end if
    !threefold repetition
    do rule50_ind = 1,state%rule50
      if(hist(ply-rule50_ind)%hash == state%hash)then
        reps = reps+1
        if(reps == 3)then
          winner = team_both
          return
        end if
      end if
    end do
    !legal move counter
    legal_moves = 0
    call gen_moves(.true.)
    do m_ind = moves_list_ind(ply),moves_list_ind(ply+1)-1
      call make_move(moves_list(m_ind))
        if(is_attacked(state%kings(ieor(1,state%side)),state%side))then
          call undo_last_move()
          cycle
        end if
        legal_moves = legal_moves+1
      call undo_last_move()
    end do
    !checkmate/stalemate
    if(legal_moves == 0)then
      if(is_attacked(state%kings(state%side),ieor(1,state%side)))then
        winner = ieor(1,state%side)
      else
        winner = team_both
      end if
      return
    end if
  end function

!MOVE GENERATOR

  function get_row(sq) result(row)
    integer::sq,row
    row = ishft(sq,-4)
  end function

  function enemy_on(sq)
    logical::enemy_on
    integer::sq
    enemy_on = get_color(board(sq)) .eq. ieor(1,state%side)
  end function

  subroutine add_move(ini,fin,is_capture,promotion_pie,is_pawnstart,is_enpassant,is_castling,captured_pie)
    type(type_move)::m
    integer::ini,fin,promotion_pie,captured_pie,king_sq
    logical::is_capture,is_pawnstart,is_enpassant,is_castling
    m%ini = ini
    m%fin = fin
    m%is_capture = is_capture
    m%promotion_pie = promotion_pie
    m%is_pawnstart = is_pawnstart
    m%is_enpassant = is_enpassant
    m%is_castling = is_castling
    m%captured_pie = captured_pie
    
    moves_list(moves_list_ind(ply+1)) = m
    moves_list_ind(ply+1) = moves_list_ind(ply+1) + 1
  end subroutine

  subroutine gen_pawn_moves(ini,gen_quiet)
    integer::ini,move_direction,fin,promotion_pie,d_ind
    integer::last_row,start_row
    integer::promo_q,promo_r,promo_b,promo_n
    integer,dimension(0:1)::capture_directions
    logical::gen_quiet
    
    if(state%side == team_white)then
      move_direction = 16
      capture_directions = (/15,17/)
      last_row = 7
      start_row = 1
      promo_q = wq
      promo_r = wr
      promo_b = wb
      promo_n = wn
    end if
    
    if(state%side == team_black)then
      move_direction = -16
      capture_directions = (/-15,-17/)
      last_row = 0
      start_row = 6
      promo_q = bq
      promo_r = br
      promo_b = bb
      promo_n = bn
    end if
    
    do d_ind=0,1
      fin = ini+capture_directions(d_ind)
      if(iand(fin,136) == 0)then
        if(enemy_on(fin))then
          if(get_row(fin) == last_row)then
            call add_move(ini,fin,.TRUE.,promo_q,.FALSE.,.FALSE.,.FALSE.,board(fin))
            call add_move(ini,fin,.TRUE.,promo_r,.FALSE.,.FALSE.,.FALSE.,board(fin))
            call add_move(ini,fin,.TRUE.,promo_b,.FALSE.,.FALSE.,.FALSE.,board(fin))
            call add_move(ini,fin,.TRUE.,promo_n,.FALSE.,.FALSE.,.FALSE.,board(fin))
          else      
            call add_move(ini,fin,.TRUE.,empty,.FALSE.,.FALSE.,.FALSE.,board(fin))
          end if
        end if
        if(fin == state%ep)then !en passants doesnt have capture flag becuase they are handled separately
          call add_move(ini,fin,.FALSE.,empty,.FALSE.,.TRUE.,.FALSE.,empty)
        end if
      end if
    end do
    if(gen_quiet)then
      fin = ini+move_direction
      if(iand(fin,136) == 0)then
        if(board(fin) == empty)then
          if(get_row(fin) == last_row)then
            call add_move(ini,fin,.TRUE.,promo_q,.FALSE.,.FALSE.,.FALSE.,board(fin))
            call add_move(ini,fin,.TRUE.,promo_r,.FALSE.,.FALSE.,.FALSE.,board(fin))
            call add_move(ini,fin,.TRUE.,promo_b,.FALSE.,.FALSE.,.FALSE.,board(fin))
            call add_move(ini,fin,.TRUE.,promo_n,.FALSE.,.FALSE.,.FALSE.,board(fin))
          else
            call add_move(ini,fin,.FALSE.,empty,.FALSE.,.FALSE.,.FALSE.,empty)
            fin = ini+2*move_direction
            if(get_row(ini) == start_row .and. board(fin)==empty)then
              call add_move(ini,fin,.FALSE.,empty,.TRUE.,.FALSE.,.FALSE.,empty)
            end if
          end if
        end if
      end if
    end if
  end subroutine

  subroutine gen_castling_moves()
    if(state%side == team_white .and. board(e1)==wk)then
      if(iand(state%cp,cp_wq) /= 0)then
        if(board(b1)==empty .and. board(c1)==empty .and. board(d1)==empty) then
          if( .not. is_attacked(e1,team_black) .and. .not. is_attacked(d1,team_black))then
            call add_move(e1,c1,.FALSE.,empty,.FALSE.,.FALSE.,.TRUE.,empty)
          end if
        end if
      end if
      if(iand(state%cp,cp_wk) /= 0)then
        if(board(g1)==empty .and. board(f1)==empty) then
          if( .not. is_attacked(e1,team_black) .and. .not. is_attacked(f1,team_black))then
            call add_move(e1,g1,.FALSE.,empty,.FALSE.,.FALSE.,.TRUE.,empty)
          end if
        end if
      end if
    end if
    if(state%side == team_black .and. board(e8)==bk)then
      if(iand(state%cp,cp_bq) /= 0)then
        if(board(b8)==empty .and. board(c8)==empty .and. board(d8)==empty) then 
          if( .not. is_attacked(e8,team_white) .and. .not. is_attacked(d8,team_white))then
            call add_move(e8,c8,.FALSE.,empty,.FALSE.,.FALSE.,.TRUE.,empty)
          end if
        end if
      end if
      if(iand(state%cp,cp_bk) /= 0)then
        if(board(g8)==empty .and. board(f8)==empty) then
          if( .not. is_attacked(e8,team_white) .and. .not. is_attacked(f8,team_white))then
            call add_move(e8,g8,.FALSE.,empty,.FALSE.,.FALSE.,.TRUE.,empty)
          end if
        end if
      end if
    end if
  end subroutine

  subroutine gen_common_moves(ini,directions,is_slide,gen_quiet)
    integer,dimension(0:7)::directions
    integer::dir,dir_ind,fin,ini
    logical::is_slide,gen_quiet
    do dir_ind = 0,7
      dir = directions(dir_ind)
      if(dir == 0) exit
      fin = ini+dir
      do while (iand(fin,136) == 0)       
        if(board(fin) == empty)then
          if(gen_quiet)then
            call add_move(ini,fin,.FALSE.,empty,.FALSE.,.FALSE.,.FALSE.,empty)
          end if
        else if(get_color(board(fin)) == state%side) then
          exit
        else
          call add_move(ini,fin,.TRUE.,empty,.FALSE.,.FALSE.,.FALSE.,board(fin))
          exit
        end if       
        if(.not. is_slide) exit
        fin = fin+dir
      end do
    end do
  end subroutine

  subroutine gen_moves(gen_quiet)
    integer::ini,pie,col
    logical::gen_quiet
    moves_list_ind(ply+1) = moves_list_ind(ply)
    do ini = 0,127
      if(iand(ini,136) == 0 .and. board(ini) /= empty)then
        pie = board(ini)
        col = get_color(pie)
        if(col == state%side)then
          if(is_queen_bishop(pie))then
            call gen_common_moves(ini,directions_bishop,.TRUE.,gen_quiet)
          end if
          if(is_queen_rook(pie))then
            call gen_common_moves(ini,directions_rook,.TRUE.,gen_quiet)
          end if
          if(is_knight(pie))then
            call gen_common_moves(ini,directions_knight,.FALSE.,gen_quiet)
          end if
          if(is_king(pie))then
            call gen_common_moves(ini,directions_rook_bishop,.FALSE.,gen_quiet)
          end if
          if(is_pawn(pie))then
            call gen_pawn_moves(ini,gen_quiet)
          end if
        end if
      end if
    end do
    if(gen_quiet)then
      call gen_castling_moves()
    end if
  end subroutine

  subroutine make_move(m)
    type(type_move)::m
    integer,dimension(0:1)::pawn_dir = (/ 16,-16 /)
    integer::ep_pawn
    
    !copy state
    hist(ply)%cp = state%cp
    hist(ply)%ep = state%ep
    hist(ply)%rule50 = state%rule50
    hist(ply)%hash = state%hash
    hist(ply)%move = m
    
    !rule50
    if(m%is_capture .or. is_pawn(board(m%ini)))then
      state%rule50 = 0
    else
      state%rule50 = state%rule50 +1
    end if    
    
    !board
    call inc_hash(hash_table%board(board(m%ini),m%ini))
    call inc_hash(hash_table%board(board(m%fin),m%fin))
    board(m%fin) = board(m%ini)
    board(m%ini) = empty
    call inc_hash(hash_table%board(board(m%fin),m%fin))
    
    !board castling
    if(m%is_castling)then
      select case (m%fin)
        case (c1)
          call inc_hash(hash_table%board(wr,a1))
          call inc_hash(hash_table%board(wr,d1))
          board(d1) = board(a1)
          board(a1) = empty
          call inc_hash(hash_table%board(wr,d1))
        case (g1)
          call inc_hash(hash_table%board(wr,f1))
          call inc_hash(hash_table%board(wr,h1))
          board(f1) = board(h1)
          board(h1) = empty
          call inc_hash(hash_table%board(wr,h1))
        case (c8)
          call inc_hash(hash_table%board(br,a8))
          call inc_hash(hash_table%board(br,d8))
          board(d8) = board(a8)
          board(a8) = empty
          call inc_hash(hash_table%board(br,d8))
        case (g8)
          call inc_hash(hash_table%board(br,f8))
          call inc_hash(hash_table%board(br,h8))
          board(f8) = board(h8)
          board(h8) = empty
          call inc_hash(hash_table%board(br,h8))          
        case default
      end select
    end if
    
    !board enpassant
    if(m%is_enpassant)then
      ep_pawn = m%fin-pawn_dir(state%side)
      call inc_hash(hash_table%board(board(ep_pawn),ep_pawn))
      board(ep_pawn) = empty
    end if
    
    !board promotion
    if(m%promotion_pie /= empty)then
      call inc_hash(hash_table%board(board(m%fin),m%fin))
      board(m%fin) = m%promotion_pie
      call inc_hash(hash_table%board(board(m%fin),m%fin))
    end if
    
    !castling permission
    call inc_hash(hash_table%cp(state%cp))
    state%cp = iand(state%cp,cp_table(m%ini))
    state%cp = iand(state%cp,cp_table(m%fin))
    call inc_hash(hash_table%cp(state%cp))
    
    !enpassant
    call inc_hash(hash_table%ep(state%ep))
    if(m%is_pawnstart)then
      state%ep = m%ini+pawn_dir(state%side)
    else
      state%ep = ob
    end if
    call inc_hash(hash_table%ep(state%ep))
    
    !king square
    if(is_king(board(m%fin)))then
      state%kings(state%side) = m%fin
    end if
    
    !side
    call inc_hash(hash_table%side)
    state%side = ieor(1,state%side)

    ply = ply + 1
    moves_list_ind(ply+1) = moves_list_ind(ply)
  end subroutine

  subroutine undo_last_move()
    type(type_move)::m
    integer,dimension(0:1)::pawn = (/ wp,bp /)
    
    !restore state
    ply = ply-1
    m = hist(ply)%move
    state%cp = hist(ply)%cp
    state%ep = hist(ply)%ep
    state%rule50 = hist(ply)%rule50
    state%hash = hist(ply)%hash
    state%side = ieor(1,state%side)

    !board
    if(m%promotion_pie == empty)then
      board(m%ini) = board(m%fin)
    else
      board(m%ini) = pawn(state%side)
    end if
    board(m%fin) = m%captured_pie

    !castling
    if(m%is_castling)then
      select case (m%fin)
        case (c1)
          board(d1) = empty
          board(a1) = wr
        case (g1)
          board(f1) = empty
          board(h1) = wr
        case (c8)
          board(d8) = empty
          board(a8) = br
        case (g8)
          board(f8) = empty
          board(h8) = br  
        case default
      end select
    end if
    
    !enpassant
    if(m%is_enpassant)then
      if(state%side == team_white)then
        board(m%fin-16) = bp
      end if
      if(state%side == team_black)then
        board(m%fin+16) = wp
      end if
    end if

    !kings
    if(is_king(board(m%ini)))then
      state%kings(state%side) = m%ini
    end if

  end subroutine

  function is_attacked(sq,attacking_side)
    logical::is_attacked
    integer::sq,attacking_side
    integer,dimension(0:7)::directions
    integer::dir,dir_ind,fin,piece
    logical::is_slide
   
    is_attacked = .FALSE.
    
    directions = directions_rook
    do dir_ind = 0,7
      dir = directions(dir_ind)
      if(dir == 0) exit
      fin = sq+dir
      do while (iand(fin,136) == 0)
        piece = board(fin)
        if(piece /= empty .and. get_color(piece) == attacking_side .and. is_queen_rook(piece))then
          is_attacked = .TRUE.
          return
        end if
        if(piece /= empty)then
          exit
        end if
        fin = fin+dir
      end do
    end do
    
    directions = directions_bishop
    do dir_ind = 0,7
      dir = directions(dir_ind)
      if(dir == 0) exit
      fin = sq+dir
      do while (iand(fin,136) == 0)
        piece = board(fin)
        if(piece /= empty .and. get_color(piece) == attacking_side .and. is_queen_bishop(piece))then
          is_attacked = .TRUE.
          return
        end if
        if(piece /= empty)then
          exit
        end if
        fin = fin+dir
      end do
    end do
    
    directions = directions_knight
    do dir_ind = 0,7
      dir = directions(dir_ind)
      if(dir == 0) exit
      fin = sq+dir
      do while (iand(fin,136) == 0)
        piece = board(fin)
        if(piece /= empty .and. get_color(piece) == attacking_side .and. is_knight(piece))then
          is_attacked = .TRUE.
          return
        end if
        exit
      end do
    end do
    
    directions = directions_rook_bishop
    do dir_ind = 0,7
      dir = directions(dir_ind)
      if(dir == 0) exit
      fin = sq+dir
      do while (iand(fin,136) == 0)
        piece = board(fin)
        if(piece /= empty .and. get_color(piece) == attacking_side .and. is_king(piece))then
          is_attacked = .TRUE.
          return
        end if
        exit
      end do
    end do
    
    if(attacking_side == team_white)then
      if(board(sq-15) == wp .and. iand(sq-15,136) == 0 .or. board(sq-17) == wp .and. iand(sq-17,136) == 0)then
        is_attacked = .TRUE.
          return
      end if
    end if
    
    if(attacking_side == team_black)then
      if(board(sq+15) == bp .and. iand(sq+15,136) == 0 .or. board(sq+17) == bp .and. iand(sq+17,136) == 0)then
        is_attacked = .TRUE.
          return
      end if
    end if
    
  end function

!INPUT/OUTPUT UTILITIES

  subroutine mirror(board)
    integer,dimension(0:127)::board
    integer,dimension(0:7,0:15)::rows
    integer::row 
    do row=0,7
      rows(row,0:15) = board(16*row:16*row+15)
    end do
    do row=0,7
      board(16*row:16*row+15) = rows(7-row,0:15)
    end do
  end subroutine

  subroutine write_board()
    integer::row,sq
    character(len=1),dimension(0:127)::board_pie
    character(len=1),dimension(0:12):: fen_piece = (/'-','P','N','B','R','Q','K','p','n','b','r','q','k'/)
    do sq = 0,127
      board_pie(sq) = fen_piece(board(sq))
    end do
    write(*,*)get_fen()
    do row=7,0,-1
      write(*,'(a1,i1,a3,8(a3))') ' ',row+1,'  |',board_pie(16*row:16*row+7)
    end do
    write(*,*) '    --------------------------'
    write(*,*) '      a  b  c  d  e  f  g  h'
  end subroutine

  subroutine write_board_raw(board)
    integer,dimension(0:127)::board
    integer::row
    do row = 7,0,-1
      write(*,'(A1,I1,A3,16(I4))') ' ',row+1,'  |',board(16*row:16*row+15)
    end do
    write(*,*) '    ---------------------------------'
    write(*,*) '       a   b   c   d   e   f   g   h'    
  end subroutine

  function raw2alg(sq) result(alg)
    character(len=2)::alg
    integer::sq
    character(len=2),parameter,dimension(-1:127)::raw2alg_table = (/ '- ', & 
      &'a1','b1','c1','d1','e1','f1','g1','h1','ob','ob','ob','ob','ob','ob','ob','ob', &
      &'a2','b2','c2','d2','e2','f2','g2','h2','ob','ob','ob','ob','ob','ob','ob','ob', &
      &'a3','b3','c3','d3','e3','f3','g3','h3','ob','ob','ob','ob','ob','ob','ob','ob', &
      &'a4','b4','c4','d4','e4','f4','g4','h4','ob','ob','ob','ob','ob','ob','ob','ob', &
      &'a5','b5','c5','d5','e5','f5','g5','h5','ob','ob','ob','ob','ob','ob','ob','ob', &
      &'a6','b6','c6','d6','e6','f6','g6','h6','ob','ob','ob','ob','ob','ob','ob','ob', &
      &'a7','b7','c7','d7','e7','f7','g7','h7','ob','ob','ob','ob','ob','ob','ob','ob', &
      &'a8','b8','c8','d8','e8','f8','g8','h8','ob','ob','ob','ob','ob','ob','ob','ob' /)
    alg = raw2alg_table(sq)
  end function

  function alg2raw(alg) result(sq)
    character(len=2)::alg
    integer::sq
    do sq=0,127
      if(raw2alg(sq) == alg)exit
    end do
  end function

  function move2alg(m) result(alg)
    type(type_move)::m
    character(len=5)::alg
    character(len=1),dimension(0:12):: fen_piece = (/' ','P','N','B','R','Q','K','p','n','b','r','q','k'/)
    alg = raw2alg(m%ini)//raw2alg(m%fin)//fen_piece(m%promotion_pie)
  end function

  function alg2move(alg) result(m)
    type(type_move)::m
    character(len=5)::alg
    integer::ini,fin,m_ind,pie
    character(len=1),dimension(0:12):: fen_piece = (/' ','P','N','B','R','Q','K','p','n','b','r','q','k'/)
    
    ini = alg2raw(alg(1:2))
    fin = alg2raw(alg(3:4))
    do pie = 0,12
      if(alg(5:5) == fen_piece(pie))then
        exit
      end if
    end do
    
    do m_ind = moves_list_ind(ply),moves_list_ind(ply+1)-1
      m = moves_list(m_ind)
      if(m%ini == ini .and. m%fin == fin .and. m%promotion_pie == pie) return
    end do
    m = move_null
  end function

  subroutine write_moves()
    integer::m_ind
    type(type_move)::m
    write(*,*)'List of moves:'
    do m_ind = moves_list_ind(ply),moves_list_ind(ply+1)-1
      m=moves_list(m_ind)
      write(*,'(A4,I8,A1,I4,I4,L2,I4,L2,L2,L2,I4)')move2alg(m),m_ind,'|',m
    end do
  end subroutine

!FEN

  function get_fen() result(fen)
    character(len=100)::fen
    integer::emptys,sq,row,col
    character(len=1),dimension(0:12):: fen_piece = (/' ','P','N','B','R','Q','K','p','n','b','r','q','k'/)
    character(len=1),dimension(0:8):: fen_sq = (/ ' ','1','2','3','4','5','6','7','8' /)
    character(len=3),dimension(0:1):: fen_side = (/ 'w','b'/)
    character(len=4)::fen_cp
    character(len=12)::fen_rule50,fen_ply !has to be len=12 to cast the integer here
    fen = ''
    !fen board
    emptys = 0
    do row = 7,0,-1
      do col = 0,7
        sq = 16*row+col
        if(iand(sq,136) == 0)then
          if(board(sq) == empty)then
            emptys = emptys + 1
          else
            fen = trim(fen)//trim(fen_sq(emptys))//fen_piece(board(sq))
            emptys = 0
          end if
        end if
      end do
      fen = trim(fen)//trim(fen_sq(emptys))
      emptys = 0
      if(row /= 0)then
        fen = trim(fen)//'/'
      end if
    end do
    !fen side
    fen = trim(fen)//' '//fen_side(state%side)
    !fen castle perm
    fen_cp = ''
    if(iand(state%cp,cp_wk) /= 0)then
      fen_cp = trim(fen_cp)//'K'
    end if
    if(iand(state%cp,cp_wq) /= 0)then
      fen_cp = trim(fen_cp)//'Q'
    end if
    if(iand(state%cp,cp_bk) /= 0)then
      fen_cp = trim(fen_cp)//'k'
    end if
    if(iand(state%cp,cp_bq) /= 0)then
      fen_cp = trim(fen_cp)//'q'
    end if
    if(fen_cp == '    ')then
      fen_cp = '-'
    end if
    fen = trim(fen)//' '//trim(fen_cp)
    !fen en passant
    fen = trim(fen)//' '//raw2alg(state%ep)
    !fen rule 50
    write(fen_rule50,*)state%rule50
    fen = trim(fen)//' '//adjustl(fen_rule50)
    !fen full move counter
    if(state%side == team_white)then
      write(fen_ply,*)(ply+2)/2
    end if
    if(state%side == team_black)then
      write(fen_ply,*)(ply+1)/2
    end if
    fen = trim(fen)//' '//adjustl(fen_ply)
  end function

  subroutine set_fen(fen_in) 
    character(*)::fen_in
    character(len=100)::fen
    character(len=72)::fen_board,fen_board_expanded
    character(len=8),dimension(0:7)::fen_row
    character(len=8)::fen_row_r
    character(len=6)::fen_side,fen_cp,fen_ep
    character(len=4)::fen_rule50
    character(len=8),dimension(1:8)::fen_empty = &
    & (/'-       ','--      ','---     ','----    ','-----   ','------  ','------- ','--------'/)
    character(len=1),dimension(0:8):: int2char = (/ ' ','1','2','3','4','5','6','7','8' /)
    character(len=1),dimension(0:12)::pie = (/'-','P','N','B','R','Q','K','p','n','b','r','q','k'/)
    integer::row_ind,col_ind,pie_ind,fen_board_ind,sq,emptys,ep_ind
    integer,dimension(0:1)::pie_cnt
    logical::is_empty
    
    fen = trim(fen_in)
    
    !parse fen
    fen_board = fen(1:index(fen,' '))
    fen = fen(index(fen,' ')+1:100)
    fen_side = fen(1:index(fen,' '))
    fen = fen(index(fen,' ')+1:100)
    fen_cp = fen(1:index(fen,' '))
    fen = fen(index(fen,' ')+1:100)
    fen_ep = fen(1:index(fen,' '))
    fen = fen(index(fen,' ')+1:100)
    fen_rule50 = fen(1:index(fen,' '))
    fen = fen(index(fen,' ')+1:100)
    
    !expand fen_board
    fen_board_expanded = ''
    do fen_board_ind = 1,100
      is_empty = .false.
      do emptys = 1,8
        if(fen_board(fen_board_ind:fen_board_ind) == int2char(emptys))then
          is_empty = .true.
          fen_board_expanded = trim(fen_board_expanded)//fen_empty(emptys)
          exit
        end if
      end do
      if(.not. is_empty)then
        fen_board_expanded = trim(fen_board_expanded)//fen_board(fen_board_ind:fen_board_ind)
      end if
    end do
   
    !parse fen_board_expanded into fen_row
    do row_ind=7,1,-1
      fen_row(row_ind) = fen_board_expanded(1:index(fen_board_expanded,'/')-1)
      fen_board_expanded = fen_board_expanded(index(fen_board_expanded,'/')+1:100)
    end do
    fen_row(0) = fen_board_expanded

    !read fen_row to set board
    do row_ind = 0,7
      fen_row_r = fen_row(row_ind)
      do col_ind = 0,7
        sq = 16*row_ind+col_ind
        do pie_ind = 0,12
          if(pie(pie_ind) == fen_row_r(col_ind+1:col_ind+1))then
            board(sq) = pie_ind
          end if
        end do
      end do
    end do
    
    !set fen side
    if(fen_side == 'w') state%side = team_white
    if(fen_side == 'b') state%side = team_black
    
    !set fen cp
    state%cp = 0 
    if(index(fen_cp,'K') /= 0)then
      state%cp = state%cp + cp_wk
    end if
    if(index(fen_cp,'Q') /= 0)then
      state%cp = state%cp + cp_wq
    end if
    if(index(fen_cp,'k') /= 0)then
      state%cp = state%cp + cp_bk
    end if
    if(index(fen_cp,'q') /= 0)then
      state%cp = state%cp + cp_bq
    end if

    !set fen ep
    do ep_ind = -1,127
      if(fen_ep == raw2alg(ep_ind))then
        state%ep = ep_ind
        exit
      end if
    end do
    
    !set fen rule50
    read(fen_rule50,*)state%rule50
    
    !set fen full move counter
    read(fen,*)ply
    ply = 2*ply -2+state%side
    
    !set piece list
    do sq = 0,127
      if(is_king(board(sq)))then
        if(get_color(board(sq)) == team_white)then
          state%kings(team_white) = sq
        end if
        if(get_color(board(sq)) == team_black)then
          state%kings(team_black) = sq
        end if
      end if
    end do

    !get position hash
    state%hash = hash_position()
    
  end subroutine

!PERFT

  recursive function perft(depth) result(nodes)
    integer::nodes,depth,m_ind,king_sq
    integer::row
    nodes = 0
    if(depth == 0) then
      nodes = 1
      return
    end if
    
    !read TT
    row = mod(state%hash,TT_size)
    if(TT(row,0) ==  state%hash .and. TT(row,1) == depth)then
      nodes = TT(row,2)
      TT_reads = TT_reads+nodes
      return
    end if
    
    !next depth
    call gen_moves(.true.)
    do m_ind = moves_list_ind(ply),moves_list_ind(ply+1)-1
      call make_move(moves_list(m_ind))
        srch%ply = srch%ply+1
        king_sq = state%kings(ieor(1,state%side))
        if(is_attacked(king_sq,state%side))then
          call undo_last_move()
          srch%ply = srch%ply-1
          cycle
        end if
        nodes = nodes + perft(depth-1)
      call undo_last_move()
      srch%ply = srch%ply-1
    end do
    
    !write TT
    if(srch%ply >= 3)then !there are no transposition for lower plys
      TT(mod(state%hash,TT_size),0) = state%hash
      TT(mod(state%hash,TT_size),1) = depth
      TT(mod(state%hash,TT_size),2) = nodes
    end if
      
  end function

  subroutine perft_handler !6.5s
    type type_position
      character(len=20)::desc
      character(len=100)::fen
      integer,dimension(1:6)::nodes
    end type
    type(type_position),dimension(1:5)::positions
    integer::p_ind,depth,nodes
    integer(8)::hash_ini
    real::time_ini,time_fin
    
    positions(1)%desc = 'starting position'
    positions(1)%fen = 'rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1'
    positions(1)%nodes = (/ 20,400,8902,197281,4865609,119060324 /)
    
    positions(2)%desc = 'kiwipete'
    positions(2)%fen = 'r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1'
    positions(2)%nodes = (/ 48,2039,97862,4085603,193690690,inf /)
    
    positions(3)%desc = 'perft position 3'
    positions(3)%fen = '8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1'
    positions(3)%nodes = (/ 14,191,2812,43238,674624,11030083 /)
    
    positions(4)%desc = 'perft position 4'
    positions(4)%fen = 'r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1'
    positions(4)%nodes = (/ 6,264,9467,422333,15833292,inf /)
    
    positions(5)%desc = 'perft position 5'
    positions(5)%fen = 'rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8'
    positions(5)%nodes = (/ 44,1486,62379,2103487,89941194,inf /)
    
    TT = 0
    srch%ply = 0
    TT_reads = 0
    
    call cpu_time(time_ini)
    
    do p_ind = 1,5
      write(*,*) positions(p_ind)%desc
      call set_fen(positions(p_ind)%fen)
      hash_ini = state%hash
      do depth = 1,6
        if(positions(p_ind)%nodes(depth) < 12000000)then
          nodes = perft(depth)
          write(*,*) nodes,positions(p_ind)%nodes(depth),nodes==positions(p_ind)%nodes(depth)
        end if
      end do
      write(*,'(A14,L12)')'unchanged hash',hash_ini == state%hash
      write(*,'(A14,L12)')'unchanged fen ',positions(p_ind)%fen == get_fen()
    end do
    
    call cpu_time(time_fin)
    write(*,*)'time: ',time_fin-time_ini
    write(*,*)'TT entrys/size ',TT_entries(),TT_size
    write(*,*)'TT skiped nodes ',TT_reads
    
  end subroutine

!TRANSPOSITION TABLES

  function TT_entries() result(cnt)
  integer::cnt,ind
  cnt = 0
  do ind = 0,TT_size-1
    if(TT(ind,0) /= 0)then
      cnt = cnt+1
    end if
  end do
  end function

  function TT_read(depth,alpha,beta) result(score)
    integer::row,score,alpha,beta,depth
    row = mod(state%hash,TT_size)
    if(TT(row,0) ==  state%hash .and. TT(row,1) >= depth)then
      if(TT(row,3) == node_is_pv)then
        TT_reads = TT_reads+1
        score = TT(row,2)
        return
      end if
      if(TT(row,3) == node_is_alpha .and. TT(row,2) <= alpha)then
        TT_reads = TT_reads+1
        score = alpha
        return
      end if
      if(TT(row,3) == node_is_beta .and. TT(row,2) >= beta)then
        TT_reads = TT_reads+1
        score = beta
        return
      end if
    end if
    score = inf+1
  end function

  subroutine TT_save(depth,score,TT_flag)
    integer::row,depth,score,TT_flag
    TT_saves = TT_saves+1
    
    row = mod(state%hash,TT_size)
    if(depth >= TT(row,1))then
      TT(row,0) = state%hash
      TT(row,1) = depth
      TT(row,2) = score
      TT(row,3) = TT_flag
    end if
    !why? becuase if for some reason i reach very high depth, that node will last forever
    !even tought the position wont appear again on the game, so i want overwrite it
    if(TT(row,0) /= state%hash .and. depth >= TT(row,1)-2)then
      TT(row,0) = state%hash
      TT(row,1) = depth
      TT(row,2) = score
      TT(row,3) = TT_flag
    end if
  end subroutine

!SEARCH ENGINE

  subroutine init_piesq_table()
    integer::p_ind
    srch%piece_value = (/ 0,100,310,320,500,950,100000,100,310,320,500,950,100000 /)
  
    srch%piesq_mg(wp,0:127) = (/ & 
      &  0,  0,  0,  0,  0,  0,  0,  0, 0,0,0,0,0,0,0,0, &
      & 50, 50, 50, 50, 50, 50, 50, 50, 0,0,0,0,0,0,0,0, &
      & 10, 10, 20, 30, 30, 20, 10, 10, 0,0,0,0,0,0,0,0, &
      &  5,  5, 10, 25, 25, 10,  5,  5, 0,0,0,0,0,0,0,0, &
      &  0,  0,  0, 20, 20,  0,  0,  0, 0,0,0,0,0,0,0,0, &
      &  5, -5,-10,  0,  0,-10, -5,  5, 0,0,0,0,0,0,0,0, &
      &  5, 10, 10,-20,-20, 10, 10,  5, 0,0,0,0,0,0,0,0, &
      &  0,  0,  0,  0,  0,  0,  0,  0, 0,0,0,0,0,0,0,0 /)
      
    srch%piesq_mg(wn,0:127) = (/ & 
      & -50,-40,-30,-30,-30,-30,-40,-50, 0,0,0,0,0,0,0,0, &
      & -40,-20,  0,  0,  0,  0,-20,-40, 0,0,0,0,0,0,0,0, &
      & -30,  0, 10, 15, 15, 10,  0,-30, 0,0,0,0,0,0,0,0, &
      & -30,  5, 15, 20, 20, 15,  5,-30, 0,0,0,0,0,0,0,0, &
      & -30,  0, 15, 20, 20, 15,  0,-30, 0,0,0,0,0,0,0,0, &
      & -30,  5, 10, 15, 15, 10,  5,-30, 0,0,0,0,0,0,0,0, &
      & -40,-20,  0,  5,  5,  0,-20,-40, 0,0,0,0,0,0,0,0, &
      & -50,-40,-30,-30,-30,-30,-40,-50, 0,0,0,0,0,0,0,0 /)
      
    srch%piesq_mg(wb,0:127) = (/ & 
      &  -20,-10,-10,-10,-10,-10,-10,-20, 0,0,0,0,0,0,0,0, &
      &  -10,  0,  0,  0,  0,  0,  0,-10, 0,0,0,0,0,0,0,0, &
      &  -10,  0,  5, 10, 10,  5,  0,-10, 0,0,0,0,0,0,0,0, &
      &  -10,  5,  5, 10, 10,  5,  5,-10, 0,0,0,0,0,0,0,0, &
      &  -10,  0, 10, 10, 10, 10,  0,-10, 0,0,0,0,0,0,0,0, &
      &  -10, 10, 10, 10, 10, 10, 10,-10, 0,0,0,0,0,0,0,0, &
      &  -10,  5,  0,  0,  0,  0,  5,-10, 0,0,0,0,0,0,0,0, &
      &  -20,-10,-10,-10,-10,-10,-10,-20, 0,0,0,0,0,0,0,0 /)

    srch%piesq_mg(wr,0:127) = (/ & 
      &  0,  0,  0,  0,  0,  0,  0,  0, 0,0,0,0,0,0,0,0, &
      &  5, 10, 10, 10, 10, 10, 10,  5, 0,0,0,0,0,0,0,0, &
      & -5,  0,  0,  0,  0,  0,  0, -5, 0,0,0,0,0,0,0,0, &
      & -5,  0,  0,  0,  0,  0,  0, -5, 0,0,0,0,0,0,0,0, &
      & -5,  0,  0,  0,  0,  0,  0, -5, 0,0,0,0,0,0,0,0, &
      & -5,  0,  0,  0,  0,  0,  0, -5, 0,0,0,0,0,0,0,0, &
      & -5,  0,  0,  0,  0,  0,  0, -5, 0,0,0,0,0,0,0,0, &
      &  0,  0,  0,  5,  5,  0,  0,  0,  0,0,0,0,0,0,0,0 /)
      
    srch%piesq_mg(wq,0:127) = (/ & 
      &  -20,-10,-10, -5, -5,-10,-10,-20, 0,0,0,0,0,0,0,0, &
      &  -10,  0,  0,  0,  0,  0,  0,-10, 0,0,0,0,0,0,0,0, &
      &  -10,  0,  5,  5,  5,  5,  0,-10, 0,0,0,0,0,0,0,0, &
      &   -5,  0,  5,  5,  5,  5,  0, -5, 0,0,0,0,0,0,0,0, &
      &    0,  0,  5,  5,  5,  5,  0, -5, 0,0,0,0,0,0,0,0, &
      &  -10,  5,  5,  5,  5,  5,  0,-10, 0,0,0,0,0,0,0,0, &
      &  -10,  0,  5,  0,  0,  0,  0,-10, 0,0,0,0,0,0,0,0, &
      &  -20,-10,-10, -5, -5,-10,-10,-20,  0,0,0,0,0,0,0,0 /)

    srch%piesq_mg(wk,0:127) = (/ & 
      &  -30,-40,-40,-50,-50,-40,-40,-30, 0,0,0,0,0,0,0,0, &
      &  -30,-40,-40,-50,-50,-40,-40,-30, 0,0,0,0,0,0,0,0, &
      &  -30,-40,-40,-50,-50,-40,-40,-30, 0,0,0,0,0,0,0,0, &
      &  -30,-40,-40,-50,-50,-40,-40,-30, 0,0,0,0,0,0,0,0, &
      &  -20,-30,-30,-40,-40,-30,-30,-20, 0,0,0,0,0,0,0,0, &
      &  -10,-20,-20,-20,-20,-20,-20,-10, 0,0,0,0,0,0,0,0, &
      &   20, 20,  0,  0,  0,  0, 20, 20, 0,0,0,0,0,0,0,0, &
      &   20, 30, 10,  0,  0, 10, 30, 20,  0,0,0,0,0,0,0,0 /)
      
    srch%piesq_mg(7:12,0:127) = srch%piesq_mg(1:6,0:127)
    do p_ind=1,6
      call mirror(srch%piesq_mg(p_ind,0:127))
    end do
    
    srch%piesq_eg(wp,0:127) = (/ & 
      &   0,   0,   0,   0,   0,   0,   0,   0, 0,0,0,0,0,0,0,0, &
      & 180, 180, 160, 140, 140, 160, 180, 180, 0,0,0,0,0,0,0,0, &
      & 100, 100,  80,  60,  60,  80, 100, 100, 0,0,0,0,0,0,0,0, &
      &  30,  30,  20,   0,   0,  20,  30,  30, 0,0,0,0,0,0,0,0, &
      &   0,   0,   0,   0,   0,   0,   0,   0, 0,0,0,0,0,0,0,0, &
      &   5,   5,   5,   0,   0,   5,   5,   5, 0,0,0,0,0,0,0,0, &
      &  10,  10,  10,  10,  10,  10,  10,  10, 0,0,0,0,0,0,0,0, &
      &   0,   0,   0,   0,   0,   0,   0,   0, 0,0,0,0,0,0,0,0 /)
   
    srch%piesq_eg(wn,0:127) = srch%piesq_mg(wn,0:127)
    srch%piesq_eg(wb,0:127) = srch%piesq_mg(wb,0:127)
    srch%piesq_eg(wr,0:127) = srch%piesq_mg(wr,0:127)
    srch%piesq_eg(wq,0:127) = srch%piesq_mg(wq,0:127)
    
    srch%piesq_eg(wk,0:127) = (/ & 
      & -50,-40,-30,-20,-20,-30,-40,-50, 0,0,0,0,0,0,0,0, &
      & -30,-20,-10,  0,  0,-10,-20,-30, 0,0,0,0,0,0,0,0, &
      & -30,-10, 20, 30, 30, 20,-10,-30, 0,0,0,0,0,0,0,0, &
      & -30,-10, 30, 40, 40, 30,-10,-30, 0,0,0,0,0,0,0,0, &
      & -30,-10, 30, 40, 40, 30,-10,-30, 0,0,0,0,0,0,0,0, &
      & -30,-10, 20, 30, 30, 20,-10,-30, 0,0,0,0,0,0,0,0, &
      & -30,-30,  0,  0,  0,  0,-30,-30, 0,0,0,0,0,0,0,0, &
      & -50,-30,-30,-30,-30,-30,-30,-50, 0,0,0,0,0,0,0,0 /)
   
  srch%piesq_eg(7:12,0:127) = srch%piesq_eg(1:6,0:127)
    do p_ind=1,6
      call mirror(srch%piesq_eg(p_ind,0:127))
    end do
    
  end subroutine

  function static_eval() result(white_score)
    integer::white_score,sq,pie,col,opening,endgame,phase
    integer,dimension(0:12)::pie_phase = (/ 0, 0,1,1,2,4,0, 0,1,1,2,4,0 /)
    srch%ss_material = 0
    srch%ss_position_mg = 0
    srch%ss_position_eg = 0
    
    phase = 32
    
    do sq = 0,127
      if(iand(sq,136) == 0)then
        pie = board(sq)
        if(pie /= empty)then
          col = get_color(pie)
          srch%ss_material(col) = srch%ss_material(col) + srch%piece_value(pie)
          srch%ss_position_mg(col) = srch%ss_position_mg(col) + srch%piesq_mg(pie,sq)
          srch%ss_position_eg(col) = srch%ss_position_eg(col) + srch%piesq_eg(pie,sq)
          phase = phase - pie*pie_phase(pie)
        end if
      end if
    end do
    
    phase =  (phase * 256 + 16) / 32
    
    opening = srch%ss_position_mg(team_white) - srch%ss_position_mg(team_black)
    endgame = srch%ss_position_eg(team_white) - srch%ss_position_eg(team_black)
    white_score = srch%ss_material(team_white) - srch%ss_material(team_black) &
              & + (((opening * (256 - phase)) + (endgame * phase)) / 256 )
              
    if(state%side == team_black) white_score = -white_score         
  end function

  function equal_m(m1,m2) result(eq)
    type(type_move)::m1,m2
    logical::eq 
    eq = .false.
    if(m1%ini == m2%ini)then
      if(m1%fin == m2%fin)then
        if(m1%is_capture .eqv. m2%is_capture)then
          if(m1%promotion_pie == m2%promotion_pie)then
            if(m1%is_pawnstart .eqv. m2%is_pawnstart)then
              if(m1%is_enpassant .eqv. m2%is_enpassant)then
                if(m1%is_castling .eqv. m2%is_castling)then
                  if(m1%captured_pie == m2%captured_pie)then
                    eq = .true. !i know, eqv operator sux
                  end if
                end if 
              end if
            end if
          end if
        end if
      end if
    end if
  end function

  subroutine score_moves()
    integer::m_ind,pie,captured
    type(type_move)::m
    integer,dimension(0:12)::reward,hunter
    
    reward = (/ 0,1000,3100,3200,5000,9500,100000,1000,3100,3200,5000,9500,1000000 /)
    hunter = (/ 0,-10,-31,-32,-50,-95,-100,-10,-31,-32,-50,-95,-100/)
    
    do m_ind = moves_list_ind(ply),moves_list_ind(ply+1)-1
      moves_score(m_ind) = 0
      m = moves_list(m_ind)
      if(equal_m(m,srch%killers(1,srch%ply)))then
         moves_score(m_ind) = 100
      end if
      if(equal_m(m,srch%killers(0,srch%ply)))then
         moves_score(m_ind) = 200
      end if
      if(m%is_capture)then
        pie = board(m%ini)
        captured = m%captured_pie
        moves_score(m_ind) = moves_score(m_ind) + reward(captured)
        moves_score(m_ind) = moves_score(m_ind) + hunter(pie)
      end if
      if(m%promotion_pie /= empty)then
        moves_score(m_ind) = moves_score(m_ind) + reward(m%promotion_pie)
      end if
    end do
  end subroutine

  function get_best_ind() result(m_ind)
  integer::m_ind,max_score
    max_score = maxval(moves_score(moves_list_ind(ply):moves_list_ind(ply+1)-1))
    do m_ind = moves_list_ind(ply),moves_list_ind(ply+1)-1
      if(moves_score(m_ind) == max_score)then
        moves_score(m_ind) = -inf
        return
      end if
    end do
  end function

  subroutine search_handler(depth,time_max)
    integer::depth,d_ind,pv_ind
    real::time_max,time_start
    write(*,'(A:)',advance='no') '  score dp       nodes  time  move'
    write(*,'(A:)',advance='no') '         fhf/fh ratio      TT w/r/entry  '
    write(*,'(A:)',advance='no') ' pv '
    write(*,*)
    pv_table = move_null
    call cpu_time(time_start)
    TT = 0
    TT_reads = 0
    TT_saves = 0
    !iterate depths
    do d_ind = 1,depth
      call cpu_time(srch%t_ini)
      if(srch%t_ini - time_start < time_max)then
        !init
        srch%nodes = 0
        srch%best_score = -inf
        srch%best_move = move_null
        srch%ply = 0
        srch%chekmates = 0
        srch%stalemates = 0
        srch%nodes_quies = 0
        srch%fh = 0
        srch%fhf = 0
        !alpha beta
        srch%best_score = alpha_beta(d_ind,-inf,inf)
        call cpu_time(srch%t_cur)
        !write results
        write(*,'(I7,I3,I12,I6,A6)',advance='no') &
          &  srch%best_score, &
          &  d_ind, &
          &  srch%nodes, &
          &  floor(1000*(srch%t_cur-srch%t_ini)), &
          &  move2alg(srch%best_move)
        !tt and node sorting
        write(*,'(I8,I8,f5.2,I8,I7,I5)',advance='no') &
          &  srch%fhf, srch%fh, 1. * srch%fhf/(srch%fh+1), &
          &  TT_saves, TT_reads, TT_entries()
        !pv line
        write(*,'(A:)',advance='no') ' pv '
        do pv_ind = 0,PV_length(0)-1
          write(*,'(A5)',advance='no') move2alg(PV_table(0,pv_ind))
        end do
        !line break
        write(*,*)
      end if
    end do
  end subroutine

  recursive function alpha_beta(depth,alpha_in,beta) result(score)
    integer::depth,alpha_in,alpha,beta,score
    integer::m_ind,legal_moves,rule50_ind,next_ply,TT_flag
    type(type_move)::m
    alpha = alpha_in

    score = TT_read(depth,alpha,beta)
    if(score <= inf)then
      return
    end if

    PV_length(srch%ply) = srch%ply

    TT_flag = node_is_alpha

    !too much depth
    if(srch%ply >= ulti_depth-2)then
      score = static_eval()
      return
    end if

    !draw rule50
    if(state%rule50 == 100)then
      score = 0
      return
    end if
    
    !draw threefold
    do rule50_ind = 1,state%rule50
      if(hist(ply-rule50_ind)%hash == state%hash)then
        score = 0
        return
      end if
    end do

    !leaf node
    if(depth == 0)then
      srch%nodes = srch%nodes+1
      score = quies(alpha,beta)
      return
    end if
    
    !generate moves
    call gen_moves(.true.)
    
    !set moves scores
    call score_moves()
    
    !count legal moves
    legal_moves = 0
    
    do m_ind = moves_list_ind(ply),moves_list_ind(ply+1)-1
      m = moves_list(get_best_ind())

      call make_move(m)
        srch%ply = srch%ply+1
        if(is_attacked(state%kings(ieor(1,state%side)),state%side))then
          call undo_last_move()
          srch%ply = srch%ply-1
          cycle
        end if
        legal_moves = legal_moves+1
        score = -alpha_beta(depth-1,-beta,-alpha)
      call undo_last_move()
      srch%ply = srch%ply-1
      
      !this position is too good, openent wont choose this path
      if(score >= beta)then
        srch%fh = srch%fh+1
        if(legal_moves == 1) srch%fhf = srch%fhf+1
        call TT_save(depth,beta,node_is_beta)
        srch%killers(1,srch%ply) = srch%killers(0,srch%ply)
        srch%killers(0,srch%ply) = m
        score = beta
        return
      end if
      
      !better move found
      if(score > alpha)then
        TT_flag = node_is_pv
        alpha = score

        PV_table(srch%ply,srch%ply) = m
        do next_ply = srch%ply+1,PV_length(srch%ply+1)-1
          pv_table(srch%ply,next_ply) = PV_table(srch%ply+1,next_ply)
        end do
        PV_length(srch%ply) = PV_length(srch%ply+1)
        
        if(srch%ply == 0)then
          srch%best_move = m
        end if
      end if
    end do
    
    !checkmate/stalemate
    if(legal_moves == 0)then
      if(is_attacked(state%kings(state%side),ieor(1,state%side)))then
        srch%chekmates = srch%chekmates+1
        score = -mate_score+srch%ply
      else
        srch%stalemates = srch%stalemates+1
        score = 0
      end if
      return
    end if
    
    call TT_save(depth,alpha,TT_flag)
    score = alpha
  end function

  recursive function quies(alpha_in,beta) result(score)
    integer::alpha_in,alpha,beta,score
    integer::m_ind,legal_moves,rule50_ind,next_ply
    type(type_move)::m
    alpha = alpha_in
    srch%nodes_quies = srch%nodes_quies+1
    
    !too much depth
    if(srch%ply >= ulti_depth-2)then
      score = static_eval()
      return
    end if
    
    !static eval
    score = static_eval()
    if(score >= beta)then
      score = beta
      return
    end if
    if(score > alpha)then
      alpha = score
    end if
    
    !generate moves
    call gen_moves(.false.)
        
    !set moves scores
    call score_moves()
    
    do m_ind = moves_list_ind(ply),moves_list_ind(ply+1)-1
      m = moves_list(get_best_ind())
      call make_move(m)
        srch%ply = srch%ply+1
        if(is_attacked(state%kings(ieor(1,state%side)),state%side))then
          call undo_last_move()
          srch%ply = srch%ply-1
          cycle
        end if
        ! write(*,*) move2alg(m),srch%ply
        score = -quies(-beta,-alpha)
      call undo_last_move()
      srch%ply = srch%ply-1
      
      !this position is too good, openent wont choose this path
      if(score >= beta)then
        score = beta
        return
      end if
      
      !better move found
      if(score > alpha)then
        alpha = score
      end if
    end do
    
    score = alpha
  end function

! TEST

  subroutine test_engine
  type type_test
    character(len=30)::desc
    character(len=100)::fen
    character(len=5)::bm
    character(len=5)::pm
  end type
  type(type_test),dimension(1:24)::bratko_kopek
  type(type_test),dimension(1:5)::finals
  integer::t_ind,score
  type(type_move)::m
  real::t_ini,t_cur
  
  
  finals(1)%desc = 'get oposition'
  finals(1)%fen  = '8/8/3k4/8/1PK5/8/8/8 w - - 0 1'
  finals(1)%bm   = 'c4b5'
  
  finals(2)%desc = 'avoid oposition'
  finals(2)%fen  = '8/4k3/4P3/4K3/8/8/8/8 b - - 0 1'
  finals(2)%bm   = 'e7e8'
  
  finals(3)%desc = 'under promo knight'
  finals(3)%fen  = '4q3/k2P4/8/K2B4/8/8/8/8 w - - 0 1'
  finals(3)%bm   = 'd7e8N'
  
  finals(4)%desc = 'pass pawn'
  finals(4)%fen  = '8/5ppp/8/5PPP/8/8/8/K1k5 w - - 0 1'
  finals(4)%bm   = 'g5g6'
  
  finals(5)%desc = 'transposition table'
  finals(5)%fen  = '6K1/4k1P1/8/8/8/7r/8/5R2 w - - 0 1'
  finals(5)%bm   = 'f1e1'
  
  bratko_kopek(1)%fen  = '1k1r4/pp1b1R2/3q2pp/4p3/2B5/4Q3/PPP2B2/2K5 b - - 0 1'  
  bratko_kopek(1)%bm = 'd6d1'
  bratko_kopek(2)%fen  = '3r1k2/4npp1/1ppr3p/p6P/P2PPPP1/1NR5/5K2/2R5 w - - 0 1'  
  bratko_kopek(2)%bm = 'd4d5'
  bratko_kopek(3)%fen  = '2q1rr1k/3bbnnp/p2p1pp1/2pPp3/PpP1P1P1/1P2BNNP/2BQ1PRK/7R b - - 0 1'  
  bratko_kopek(3)%bm = 'f6f5'
  bratko_kopek(4)%fen  = 'rnbqkb1r/p3pppp/1p6/2ppP3/3N4/2P5/PPP1QPPP/R1B1KB1R w KQkq - 0 1'  
  bratko_kopek(4)%bm = 'e5e6'
  bratko_kopek(5)%fen  = 'r1b2rk1/2q1b1pp/p2ppn2/1p6/3QP3/1BN1B3/PPP3PP/R4RK1 w - - 0 1'  
  bratko_kopek(5)%bm = 'c3d5' !'a2a4'
  bratko_kopek(6)%fen  = '2r3k1/pppR1pp1/4p3/4P1P1/5P2/1P4K1/P1P5/8 w - - 0 1'  
  bratko_kopek(6)%bm = 'g5g6'
  bratko_kopek(7)%fen  = '1nk1r1r1/pp2n1pp/4p3/q2pPp1N/b1pP1P2/B1P2R2/2P1B1PP/R2Q2K1 w - - 0 1'  
  bratko_kopek(7)%bm = 'h5f6'
  bratko_kopek(8)%fen  = '4b3/p3kp2/6p1/3pP2p/2pP1P2/4K1P1/P3N2P/8 w - - 0 1'  
  bratko_kopek(8)%bm = 'f4f5'
  bratko_kopek(9)%fen  = '2kr1bnr/pbpq4/2n1pp2/3p3p/3P1P1B/2N2N1Q/PPP3PP/2KR1B1R w - - 0 1'  
  bratko_kopek(9)%bm = 'f4f5'
  bratko_kopek(10)%fen = '3rr1k1/pp3pp1/1qn2np1/8/3p4/PP1R1P2/2P1NQPP/R1B3K1 b - - 0 1'  
  bratko_kopek(10)%bm = 'c6e5'
  bratko_kopek(11)%fen = '2r1nrk1/p2q1ppp/bp1p4/n1pPp3/P1P1P3/2PBB1N1/4QPPP/R4RK1 w - - 0 1'  
  bratko_kopek(11)%bm = 'f2f4'
  bratko_kopek(12)%fen = 'r3r1k1/ppqb1ppp/8/4p1NQ/8/2P5/PP3PPP/R3R1K1 b - - 0 1'  
  bratko_kopek(12)%bm = 'd7f5'
  bratko_kopek(13)%fen = 'r2q1rk1/4bppp/p2p4/2pP4/3pP3/3Q4/PP1B1PPP/R3R1K1 w - - 0 1'  
  bratko_kopek(13)%bm = 'b2b4'
  bratko_kopek(14)%fen = 'rnb2r1k/pp2p2p/2pp2p1/q2P1p2/8/1Pb2NP1/PB2PPBP/R2Q1RK1 w - - 0 1'  
  bratko_kopek(14)%bm = 'd1d2' !'d1e1'
  bratko_kopek(15)%fen = '2r3k1/1p2q1pp/2b1pr2/p1pp4/6Q1/1P1PP1R1/P1PN2PP/5RK1 w - - 0 1'  
  bratko_kopek(15)%bm = 'g4g7'
  bratko_kopek(16)%fen = 'r1bqkb1r/4npp1/p1p4p/1p1pP1B1/8/1B6/PPPN1PPP/R2Q1RK1 w kq - 0 1'  
  bratko_kopek(16)%bm = 'd2e4'
  bratko_kopek(17)%fen = 'r2q1rk1/1ppnbppp/p2p1nb1/3Pp3/2P1P1P1/2N2N1P/PPB1QP2/R1B2RK1 b - - 0 1'  
  bratko_kopek(17)%bm = 'h7h5'
  bratko_kopek(18)%fen = 'r1bq1rk1/pp2ppbp/2np2p1/2n5/P3PP2/N1P2N2/1PB3PP/R1B1QRK1 b - - 0 1'  
  bratko_kopek(18)%bm = 'c5b3'
  bratko_kopek(19)%fen = '3rr3/2pq2pk/p2p1pnp/8/2QBPP2/1P6/P5PP/4RRK1 b - - 0 1'  
  bratko_kopek(19)%bm = 'e8e4'
  bratko_kopek(20)%fen = 'r4k2/pb2bp1r/1p1qp2p/3pNp2/3P1P2/2N3P1/PPP1Q2P/2KRR3 w - - 0 1'  
  bratko_kopek(20)%bm = 'g3g4'
  bratko_kopek(21)%fen = '3rn2k/ppb2rpp/2ppqp2/5N2/2P1P3/1P5Q/PB3PPP/3RR1K1 w - - 0 1'  
  bratko_kopek(21)%bm = 'f5h6'
  bratko_kopek(22)%fen = '2r2rk1/1bqnbpp1/1p1ppn1p/pP6/N1P1P3/P2B1N1P/1B2QPP1/R2R2K1 b - - 0 1'  
  bratko_kopek(22)%bm = 'b7e4'
  bratko_kopek(23)%fen = 'r1bqk2r/pp2bppp/2p5/3pP3/P2Q1P2/2N1B3/1PP3PP/R4RK1 b kq - 0 1'  
  bratko_kopek(23)%bm = 'f7f6'
  bratko_kopek(24)%fen = 'r2qnrnk/p2b2b1/1p1p2pp/2pPpp2/1PP1P3/PRNBB3/3QNPPP/5RK1 w - - 0 1'  
  bratko_kopek(24)%bm = 'f2f4'
  
  !test the test
  do t_ind = 1,5
    call set_fen(finals(t_ind)%fen)
    call gen_moves(.true.)
    m = alg2move(finals(t_ind)%bm)
    if(equal_m(m,move_null))then
      write(*,*)'move is not in list ',t_ind
    end if
    call make_move(m)
    if(is_attacked(state%kings(ieor(1,state%side)),state%side))then
      write(*,*)'move is illegal ',t_ind
    end if
  end do
  
  do t_ind = 1,24
    call set_fen(bratko_kopek(t_ind)%fen)
    call gen_moves(.true.)
    m = alg2move(bratko_kopek(t_ind)%bm)
    if(equal_m(m,move_null))then
      write(*,*)'move is not in list ',t_ind
    end if
    call make_move(m)
    if(is_attacked(state%kings(ieor(1,state%side)),state%side))then
      write(*,*)'move is illegal ',t_ind
    end if
  end do
  
  write(*,*)'bratko-kopek'
  call cpu_time(t_ini)
  do t_ind = 1,24
    call set_fen(bratko_kopek(t_ind)%fen)
    call search_handler(8,1.)
    write(*,*)move2alg(srch%best_move),bratko_kopek(t_ind)%bm
    if(move2alg(srch%best_move) == bratko_kopek(t_ind)%bm)then
      score = score+1
    end if
  end do
  call cpu_time(t_cur)
  write(*,*)score,t_cur-t_ini
  
  ! write(*,*)'finals'
  ! call cpu_time(t_ini)
  ! do t_ind = 1,5
    ! call set_fen(finals(t_ind)%fen)
    ! call search_handler(10,1.)
    ! write(*,*)move2alg(srch%best_move),finals(t_ind)%bm
    ! if(move2alg(srch%best_move) == finals(t_ind)%bm)then
      ! score = score+1
    ! end if
  ! end do
  call cpu_time(t_cur)
  write(*,*)score,t_cur-t_ini
  
  end subroutine

end program lostchess
