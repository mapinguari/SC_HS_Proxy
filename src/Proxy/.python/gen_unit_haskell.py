
def fix(name):
    return name.replace("_", "")

def convert_bool(b):
    return "True" if b else "False"

class Function(object):
    def __init__(self, name):
        self.name = name
        self.cases = []

    def addcase(self, case):
        case_name, code = case
        self.cases.append((fix(case_name), code))

    def gen(self):
        code = ""
        for case_name, case_code in self.cases:
            case = "%(f_name)s %(c_name)s = %(code)s\n" % {"f_name":self.name,"c_name":case_name,"code":case_code}
            code += case
        return code
            

class CodeGen(object):
    def addfunc(self, name):
        self.functions[name] = Function(name)

    def addcase(self, func_name, case):
        self.functions[func_name].addcase(case)

    def gen(self):
        code = "module Proxy.Unit where\nimport Proxy.UnitTypes\nimport Proxy.Game\n\n"
        for name in self.functions:
            #print name, self.functions[name]
            code += self.functions[name].gen()
            #print code
            code += "\n"
        return code
    
    def __init__(self):
        self.functions = {}
        def addfunc(name):
            self.functions[name] = Function(name)
        addfunc("race")
        addfunc("mineralsCost")
        addfunc("gasCost")
        addfunc("maxHitPoints")
        addfunc("maxShields")
        addfunc("maxEnergy")
        addfunc("buildTime")
        addfunc("canAttack")
        addfunc("canMove")
        addfunc("tileWidth")
        addfunc("tileHeight")
        addfunc("supplyRequired")
        addfunc("supplyProvided")
        addfunc("sightRange")
        addfunc("groundMinRange")
        addfunc("groundMaxRange")
        addfunc("groundDamage")
        addfunc("airRange")
        addfunc("airDamage")
        addfunc("isBuilding")
        addfunc("isFlyer")
        addfunc("isSpellCaster")
        addfunc("isWorker")
        addfunc("whatBuilds")

code_gen = CodeGen()

class UnitTypes(object):
    Terran_Marine = 0
    Terran_Ghost = 1
    Terran_Vulture = 2
    Terran_Goliath = 3
    Terran_Siege_Tank_Tank_Mode = 5
    Terran_SCV = 7
    Terran_Wraith = 8
    Terran_Science_Vessel = 9
    Terran_Dropship = 11
    Terran_Battlecruiser = 12
    Terran_Vulture_Spider_Mine = 13
    Terran_Nuclear_Missile = 14
    Terran_Siege_Tank_Siege_Mode = 30
    Terran_Firebat = 32
    Spell_Scanner_Sweep = 33
    Terran_Medic = 34
    Zerg_Larva = 35
    Zerg_Egg = 36
    Zerg_Zergling = 37
    Zerg_Hydralisk = 38
    Zerg_Ultralisk = 39
    Zerg_Broodling = 40
    Zerg_Drone = 41
    Zerg_Overlord = 42
    Zerg_Mutalisk = 43
    Zerg_Guardian = 44
    Zerg_Queen = 45
    Zerg_Defiler = 46
    Zerg_Scourge = 47
    Zerg_Infested_Terran = 50
    Terran_Valkyrie = 58
    Zerg_Cocoon = 59
    Protoss_Corsair = 60
    Protoss_Dark_Templar = 61
    Zerg_Devourer = 62
    Protoss_Dark_Archon = 63
    Protoss_Probe = 64
    Protoss_Zealot = 65
    Protoss_Dragoon = 66
    Protoss_High_Templar = 67
    Protoss_Archon = 68
    Protoss_Shuttle = 69
    Protoss_Scout = 70
    Protoss_Arbiter = 71
    Protoss_Carrier = 72
    Protoss_Interceptor = 73
    Protoss_Reaver = 83
    Protoss_Observer = 84
    Protoss_Scarab = 85
    Critter_Rhynadon = 89
    Critter_Bengalaas = 90
    Critter_Scantid = 93
    Critter_Kakaru = 94
    Critter_Ragnasaur = 95
    Critter_Ursadon = 96
    Zerg_Lurker_Egg = 97
    Zerg_Lurker = 103
    Spell_Disruption_Web = 105
    Terran_Command_Center = 106
    Terran_Comsat_Station = 107
    Terran_Nuclear_Silo = 108
    Terran_Supply_Depot = 109
    Terran_Refinery = 110
    Terran_Barracks = 111
    Terran_Academy = 112
    Terran_Factory = 113
    Terran_Starport = 114
    Terran_Control_Tower = 115
    Terran_Science_Facility = 116
    Terran_Covert_Ops = 117
    Terran_Physics_Lab = 118
    Terran_Machine_Shop = 120
    Terran_Engineering_Bay = 122
    Terran_Armory = 123
    Terran_Missile_Turret = 124
    Terran_Bunker = 125
    Special_Crashed_Norad_II = 126
    Special_Ion_Cannon = 127
    Zerg_Infested_Command_Center = 130
    Zerg_Hatchery = 131
    Zerg_Lair = 132
    Zerg_Hive = 133
    Zerg_Nydus_Canal = 134
    Zerg_Hydralisk_Den = 135
    Zerg_Defiler_Mound = 136
    Zerg_Greater_Spire = 137
    Zerg_Queen_s_Nest = 138
    Zerg_Evolution_Chamber = 139
    Zerg_Ultralisk_Cavern = 140
    Zerg_Spire = 141
    Zerg_Spawning_Pool = 142
    Zerg_Creep_Colony = 143
    Zerg_Spore_Colony = 144
    Zerg_Sunken_Colony = 146
    Special_Overmind_With_Shell = 147
    Special_Overmind = 148
    Zerg_Extractor = 149
    Special_Mature_Chrysalis = 150
    Special_Cerebrate = 151
    Special_Cerebrate_Daggoth = 152
    Protoss_Nexus = 154
    Protoss_Robotics_Facility = 155
    Protoss_Pylon = 156
    Protoss_Assimilator = 157
    Protoss_Observatory = 159
    Protoss_Gateway = 160
    Protoss_Photon_Cannon = 162
    Protoss_Citadel_Of_Adun = 163
    Protoss_Cybernetics_Core = 164
    Protoss_Templar_Archives = 165
    Protoss_Forge = 166
    Protoss_Stargate = 167
    Special_Stasis_Cell_Prison = 168
    Protoss_Fleet_Beacon = 169
    Protoss_Arbiter_Tribunal = 170
    Protoss_Robotics_Support_Bay = 171
    Protoss_Shield_Battery = 172
    Special_Khaydarin_Crystal_Form = 173
    Special_Protoss_Temple = 174
    Special_XelNaga_Temple = 175
    Resource_Mineral_Patch1 = 176
    Resource_Vespene_Geyser = 188
    Special_Warp_Gate = 189
    Special_Psi_Disrupter = 190
    Special_Power_Generator = 200
    Special_Overmind_Cocoon = 201
    Spell_Dark_Swarm = 202
    NoneUnitType = 228
    UnknownUnitType = 229

def convert_unit_type(i):
    for attr, value in UnitTypes.__dict__.iteritems():
        if value == i:
            return fix(attr)

class UnitType(object):
    def __init__(self,  id,  race,  name,  mineralsCost,  gasCost,  maxHitPoints,  maxShields,  maxEnergy,  buildTime,  canAttack,  canMove, tileWidth,  tileHeight,  supplyRequired,  supplyProvided, sightRange,  groundMinRange,  groundMaxRange,  groundDamage, airRange,  airDamage,  building,  flyer, spellCaster,  worker,  whatBuilds):
        def addcase(func_name, code):
            code_gen.addcase(func_name, (fix(name), code))
        addcase("race", fix(race))
        addcase("mineralsCost", mineralsCost.__str__())
        addcase("gasCost", gasCost.__str__())
        addcase("maxHitPoints", maxHitPoints.__str__())
        addcase("maxShields", maxShields.__str__())
        addcase("maxEnergy", maxEnergy.__str__())
        addcase("buildTime", buildTime.__str__())
        addcase("canAttack", convert_bool(canAttack))
        addcase("canMove", convert_bool(canMove))
        addcase("tileWidth", tileWidth.__str__())
        addcase("tileHeight", tileHeight.__str__())
        addcase("supplyRequired", supplyRequired.__str__())
        addcase("supplyProvided", supplyProvided.__str__())
        addcase("sightRange", sightRange.__str__())
        addcase("groundMinRange", groundMinRange.__str__())
        addcase("groundMaxRange", groundMaxRange.__str__())
        addcase("groundDamage", groundDamage.__str__())
        addcase("airRange", airRange.__str__())
        addcase("airDamage", airDamage.__str__())
        addcase("isBuilding", convert_bool(building))
        addcase("isFlyer", convert_bool(flyer))
        addcase("isSpellCaster", convert_bool(spellCaster))
        addcase("isWorker", convert_bool(worker))
        addcase("whatBuilds", convert_unit_type(whatBuilds))

def getUnitTypes():
    result = {}
    result[0] = UnitType( 0 ,"Terran","Terran_Marine",50,0,40,0,0,360,False,False,1,1,2,0,7,128,0,6,128,6,False,False,False,False,111)
    result[1] = UnitType( 1 ,"Terran","Terran_Ghost",25,75,45,0,200,750,False,False,1,1,2,0,9,224,0,10,224,10,False,False,False,False,111)
    result[2] = UnitType( 2 ,"Terran","Terran_Vulture",75,0,80,0,0,450,False,False,1,1,4,0,8,160,0,20,0,0,False,False,False,False,113)
    result[3] = UnitType( 3 ,"Terran","Terran_Goliath",100,50,125,0,0,600,False,False,1,1,4,0,8,192,0,12,160,10,False,False,False,False,113)
    result[5] = UnitType( 5 ,"Terran","Terran_Siege_Tank_Tank_Mode",150,100,150,0,0,750,False,False,1,1,4,0,10,224,0,30,0,0,False,False,False,False,113)
    result[7] = UnitType( 7 ,"Terran","Terran_SCV",50,0,60,0,0,300,False,False,1,1,2,0,7,10,0,5,0,0,False,False,False,True,106)
    result[8] = UnitType( 8 ,"Terran","Terran_Wraith",150,100,120,0,200,900,False,False,1,1,4,0,7,160,0,8,160,20,False,False,False,False,114)
    result[9] = UnitType( 9 ,"Terran","Terran_Science_Vessel",100,225,200,0,200,1200,False,False,2,2,4,0,10,0,0,0,0,0,False,False,False,False,114)
    result[11] = UnitType( 11 ,"Terran","Terran_Dropship",100,100,150,0,0,750,False,False,2,2,4,0,8,0,0,0,0,0,False,False,False,False,114)
    result[12] = UnitType( 12 ,"Terran","Terran_Battlecruiser",400,300,244,0,200,2000,False,False,2,2,12,0,11,192,0,25,192,25,False,False,False,False,114)
    result[13] = UnitType( 13 ,"Terran","Terran_Vulture_Spider_Mine",1,0,20,0,0,1,False,False,1,1,0,0,3,10,0,125,0,0,False,False,False,False,228)
    result[14] = UnitType( 14 ,"Terran","Terran_Nuclear_Missile",200,200,100,0,0,1500,False,False,1,1,16,0,3,0,0,0,0,0,False,False,False,False,228)
    result[30] = UnitType( 30 ,"Terran","Terran_Siege_Tank_Siege_Mode",150,100,150,0,0,750,False,False,1,1,4,0,10,384,64,70,0,0,False,False,False,False,113)
    result[32] = UnitType( 32 ,"Terran","Terran_Firebat",50,25,50,0,0,360,False,False,1,1,2,0,7,32,0,8,0,0,False,False,False,False,111)
    result[33] = UnitType( 33 ,"Terran","Spell_Scanner_Sweep",0,0,0,0,0,1,False,False,1,1,0,0,10,0,0,0,0,0,False,False,False,False,228)
    result[34] = UnitType( 34 ,"Terran","Terran_Medic",50,25,60,0,200,450,False,False,1,1,2,0,9,0,0,0,0,0,False,False,False,False,111)
    result[35] = UnitType( 35 ,"Zerg","Zerg_Larva",1,1,25,0,0,1,False,False,1,1,0,0,4,0,0,0,0,0,False,False,False,False,131)
    result[36] = UnitType( 36 ,"Zerg","Zerg_Egg",1,1,200,0,0,1,False,False,1,1,0,0,4,0,0,0,0,0,False,False,False,False,35)
    result[37] = UnitType( 37 ,"Zerg","Zerg_Zergling",50,0,35,0,0,420,False,False,1,1,1,0,5,15,0,5,0,0,False,False,False,False,35)
    result[38] = UnitType( 38 ,"Zerg","Zerg_Hydralisk",75,25,80,0,0,420,False,False,1,1,2,0,6,128,0,10,128,10,False,False,False,False,35)
    result[39] = UnitType( 39 ,"Zerg","Zerg_Ultralisk",200,200,144,0,0,900,False,False,2,2,8,0,7,25,0,20,0,0,False,False,False,False,35)
    result[40] = UnitType( 40 ,"Zerg","Zerg_Broodling",1,1,30,0,0,1,False,False,1,1,0,0,5,2,0,4,0,0,False,False,False,False,228)
    result[41] = UnitType( 41 ,"Zerg","Zerg_Drone",50,0,40,0,0,300,False,False,1,1,2,0,7,32,0,5,0,0,False,False,False,True,35)
    result[42] = UnitType( 42 ,"Zerg","Zerg_Overlord",100,0,200,0,0,600,False,False,2,2,0,16,9,0,0,0,0,0,False,False,False,False,35)
    result[43] = UnitType( 43 ,"Zerg","Zerg_Mutalisk",100,100,120,0,0,600,False,False,2,2,4,0,7,96,0,9,96,9,False,False,False,False,35)
    result[44] = UnitType( 44 ,"Zerg","Zerg_Guardian",50,100,150,0,0,600,False,False,2,2,4,0,11,256,0,20,0,0,False,False,False,False,43)
    result[45] = UnitType( 45 ,"Zerg","Zerg_Queen",100,100,120,0,200,750,False,False,2,2,4,0,10,0,0,0,0,0,False,False,False,False,35)
    result[46] = UnitType( 46 ,"Zerg","Zerg_Defiler",50,150,80,0,200,750,False,False,1,1,4,0,10,0,0,0,0,0,False,False,False,False,35)
    result[47] = UnitType( 47 ,"Zerg","Zerg_Scourge",25,75,25,0,0,450,False,False,1,1,1,0,5,0,0,0,3,110,False,False,False,False,35)
    result[50] = UnitType( 50 ,"Zerg","Zerg_Infested_Terran",100,50,60,0,0,600,False,False,1,1,2,0,5,3,0,500,0,0,False,False,False,False,130)
    result[58] = UnitType( 58 ,"Terran","Terran_Valkyrie",250,125,200,0,0,750,False,False,2,2,6,0,8,0,0,0,192,6,False,False,False,False,114)
    result[59] = UnitType( 59 ,"Zerg","Zerg_Cocoon",1,1,200,0,0,1,False,False,1,1,0,0,4,0,0,0,0,0,False,False,False,False,43)
    result[60] = UnitType( 60 ,"Protoss","Protoss_Corsair",150,100,100,80,200,600,False,False,1,1,4,0,9,0,0,0,160,5,False,False,False,False,167)
    result[61] = UnitType( 61 ,"Protoss","Protoss_Dark_Templar",125,100,80,40,0,750,False,False,1,1,4,0,7,15,0,40,0,0,False,False,False,False,160)
    result[62] = UnitType( 62 ,"Zerg","Zerg_Devourer",150,50,250,0,0,600,False,False,2,2,4,0,10,0,0,0,192,25,False,False,False,False,43)
    result[63] = UnitType( 63 ,"Protoss","Protoss_Dark_Archon",0,0,25,200,200,300,False,False,1,1,8,0,10,0,0,0,0,0,False,False,False,False,61)
    result[64] = UnitType( 64 ,"Protoss","Protoss_Probe",50,0,20,20,0,300,False,False,1,1,2,0,8,32,0,5,0,0,False,False,False,True,154)
    result[65] = UnitType( 65 ,"Protoss","Protoss_Zealot",100,0,100,60,0,600,False,False,1,1,4,0,7,15,0,8,0,0,False,False,False,False,160)
    result[66] = UnitType( 66 ,"Protoss","Protoss_Dragoon",125,50,100,80,0,750,False,False,1,1,4,0,8,128,0,20,128,20,False,False,False,False,160)
    result[67] = UnitType( 67 ,"Protoss","Protoss_High_Templar",50,150,40,40,200,750,False,False,1,1,4,0,7,0,0,0,0,0,False,False,False,False,160)
    result[68] = UnitType( 68 ,"Protoss","Protoss_Archon",0,0,10,350,0,300,False,False,1,1,8,0,8,64,0,30,64,30,False,False,False,False,67)
    result[69] = UnitType( 69 ,"Protoss","Protoss_Shuttle",200,0,80,60,0,900,False,False,2,1,4,0,8,0,0,0,0,0,False,False,False,False,155)
    result[70] = UnitType( 70 ,"Protoss","Protoss_Scout",275,125,150,100,0,1200,False,False,2,1,6,0,8,128,0,8,128,14,False,False,False,False,167)
    result[71] = UnitType( 71 ,"Protoss","Protoss_Arbiter",100,350,200,150,200,2400,False,False,2,2,8,0,9,160,0,10,160,10,False,False,False,False,167)
    result[72] = UnitType( 72 ,"Protoss","Protoss_Carrier",350,250,44,150,0,2100,False,False,2,2,12,0,11,0,0,0,0,0,False,False,False,False,167)
    result[73] = UnitType( 73 ,"Protoss","Protoss_Interceptor",25,0,40,40,0,300,False,False,1,1,0,0,6,128,0,6,128,6,False,False,False,False,72)
    result[83] = UnitType( 83 ,"Protoss","Protoss_Reaver",200,100,100,80,0,1050,False,False,1,1,8,0,10,0,0,0,0,0,False,False,False,False,155)
    result[84] = UnitType( 84 ,"Protoss","Protoss_Observer",25,75,40,20,0,600,False,False,1,1,2,0,9,0,0,0,0,0,False,False,False,False,155)
    result[85] = UnitType( 85 ,"Protoss","Protoss_Scarab",15,0,20,10,0,105,False,False,1,1,0,0,5,128,0,100,0,0,False,False,False,False,83)
    result[89] = UnitType( 89 ,"OtherRace","Critter_Rhynadon",1,1,60,0,0,1,False,False,1,1,0,0,7,0,0,0,0,0,False,False,False,False,228)
    result[90] = UnitType( 90 ,"OtherRace","Critter_Bengalaas",1,1,60,0,0,1,False,False,1,1,0,0,7,0,0,0,0,0,False,False,False,False,228)
    result[93] = UnitType( 93 ,"OtherRace","Critter_Scantid",1,1,60,0,0,1,False,False,1,1,0,0,7,0,0,0,0,0,False,False,False,False,228)
    result[94] = UnitType( 94 ,"OtherRace","Critter_Kakaru",1,1,60,0,0,1,False,False,1,1,0,0,7,0,0,0,0,0,False,False,False,False,228)
    result[95] = UnitType( 95 ,"OtherRace","Critter_Ragnasaur",1,1,60,0,0,1,False,False,1,1,0,0,7,0,0,0,0,0,False,False,False,False,228)
    result[96] = UnitType( 96 ,"OtherRace","Critter_Ursadon",1,1,60,0,0,1,False,False,1,1,0,0,7,0,0,0,0,0,False,False,False,False,228)
    result[97] = UnitType( 97 ,"Zerg","Zerg_Lurker_Egg",1,1,200,0,0,1,False,False,1,1,0,0,4,0,0,0,0,0,False,False,False,False,38)
    result[103] = UnitType( 103 ,"Zerg","Zerg_Lurker",50,100,125,0,0,600,False,False,1,1,4,0,8,192,0,20,0,0,False,False,False,False,38)
    result[105] = UnitType( 105 ,"OtherRace","Spell_Disruption_Web",250,250,32,0,0,2400,False,False,4,3,0,0,8,0,0,0,0,0,False,False,False,False,228)
    result[106] = UnitType( 106 ,"Terran","Terran_Command_Center",400,0,220,0,0,1800,False,False,4,3,0,20,10,0,0,0,0,0,False,False,False,False,7)
    result[107] = UnitType( 107 ,"Terran","Terran_Comsat_Station",50,50,244,0,200,600,False,False,2,2,0,0,10,0,0,0,0,0,False,False,False,False,106)
    result[108] = UnitType( 108 ,"Terran","Terran_Nuclear_Silo",100,100,88,0,0,1200,False,False,2,2,0,0,8,0,0,0,0,0,False,False,False,False,106)
    result[109] = UnitType( 109 ,"Terran","Terran_Supply_Depot",100,0,244,0,0,600,False,False,3,2,0,16,8,0,0,0,0,0,False,False,False,False,7)
    result[110] = UnitType( 110 ,"Terran","Terran_Refinery",100,0,238,0,0,600,False,False,4,2,0,0,8,0,0,0,0,0,False,False,False,False,7)
    result[111] = UnitType( 111 ,"Terran","Terran_Barracks",150,0,232,0,0,1200,False,False,4,3,0,0,8,0,0,0,0,0,False,False,False,False,7)
    result[112] = UnitType( 112 ,"Terran","Terran_Academy",150,0,88,0,0,1200,False,False,3,2,0,0,8,0,0,0,0,0,False,False,False,False,7)
    result[113] = UnitType( 113 ,"Terran","Terran_Factory",200,100,226,0,0,1200,False,False,4,3,0,0,8,0,0,0,0,0,False,False,False,False,7)
    result[114] = UnitType( 114 ,"Terran","Terran_Starport",150,100,20,0,0,1050,False,False,4,3,0,0,10,0,0,0,0,0,False,False,False,False,7)
    result[115] = UnitType( 115 ,"Terran","Terran_Control_Tower",50,50,244,0,0,600,False,False,2,2,0,0,8,0,0,0,0,0,False,False,False,False,114)
    result[116] = UnitType( 116 ,"Terran","Terran_Science_Facility",100,150,82,0,0,900,False,False,4,3,0,0,10,0,0,0,0,0,False,False,False,False,7)
    result[117] = UnitType( 117 ,"Terran","Terran_Covert_Ops",50,50,238,0,0,600,False,False,2,2,0,0,8,0,0,0,0,0,False,False,False,False,116)
    result[118] = UnitType( 118 ,"Terran","Terran_Physics_Lab",50,50,88,0,0,600,False,False,2,2,0,0,8,0,0,0,0,0,False,False,False,False,116)
    result[120] = UnitType( 120 ,"Terran","Terran_Machine_Shop",50,50,238,0,0,600,False,False,2,2,0,0,8,0,0,0,0,0,False,False,False,False,113)
    result[122] = UnitType( 122 ,"Terran","Terran_Engineering_Bay",125,0,82,0,0,900,False,False,4,3,0,0,8,0,0,0,0,0,False,False,False,False,7)
    result[123] = UnitType( 123 ,"Terran","Terran_Armory",100,50,238,0,0,1200,False,False,3,2,0,0,8,0,0,0,0,0,False,False,False,False,7)
    result[124] = UnitType( 124 ,"Terran","Terran_Missile_Turret",75,0,200,0,0,450,False,False,2,2,0,0,11,0,0,0,224,20,False,False,False,False,7)
    result[125] = UnitType( 125 ,"Terran","Terran_Bunker",100,0,94,0,0,450,False,False,3,2,0,0,10,0,0,0,0,0,False,False,False,False,7)
    result[126] = UnitType( 126 ,"NoneRace","Special_Crashed_Norad_II",800,600,188,0,0,4800,False,False,3,2,0,0,10,0,0,0,0,0,False,False,False,False,228)
    result[127] = UnitType( 127 ,"NoneRace","Special_Ion_Cannon",200,0,208,0,0,900,False,False,3,2,0,0,8,0,0,0,0,0,False,False,False,False,228)
    result[130] = UnitType( 130 ,"Zerg","Zerg_Infested_Command_Center",1,1,220,0,0,1800,False,False,4,3,0,0,10,0,0,0,0,0,False,False,False,False,228)
    result[131] = UnitType( 131 ,"Zerg","Zerg_Hatchery",300,0,226,0,0,1800,False,False,4,3,0,2,9,0,0,0,0,0,False,False,False,False,41)
    result[132] = UnitType( 132 ,"Zerg","Zerg_Lair",150,100,8,0,0,1500,False,False,4,3,0,2,10,0,0,0,0,0,False,False,False,False,131)
    result[133] = UnitType( 133 ,"Zerg","Zerg_Hive",200,150,196,0,0,1800,False,False,4,3,0,2,11,0,0,0,0,0,False,False,False,False,132)
    result[134] = UnitType( 134 ,"Zerg","Zerg_Nydus_Canal",150,0,250,0,0,600,False,False,2,2,0,0,8,0,0,0,0,0,False,False,False,False,41)
    result[135] = UnitType( 135 ,"Zerg","Zerg_Hydralisk_Den",100,50,82,0,0,600,False,False,3,2,0,0,8,0,0,0,0,0,False,False,False,False,41)
    result[136] = UnitType( 136 ,"Zerg","Zerg_Defiler_Mound",100,100,82,0,0,900,False,False,4,2,0,0,8,0,0,0,0,0,False,False,False,False,41)
    result[137] = UnitType( 137 ,"Zerg","Zerg_Greater_Spire",100,150,232,0,0,1800,False,False,2,2,0,0,8,0,0,0,0,0,False,False,False,False,141)
    result[138] = UnitType( 138 ,"Zerg","Zerg_Queen_s_Nest",150,100,82,0,0,900,False,False,3,2,0,0,8,0,0,0,0,0,False,False,False,False,41)
    result[139] = UnitType( 139 ,"Zerg","Zerg_Evolution_Chamber",75,0,238,0,0,600,False,False,3,2,0,0,8,0,0,0,0,0,False,False,False,False,41)
    result[140] = UnitType( 140 ,"Zerg","Zerg_Ultralisk_Cavern",150,200,88,0,0,1200,False,False,3,2,0,0,8,0,0,0,0,0,False,False,False,False,41)
    result[141] = UnitType( 141 ,"Zerg","Zerg_Spire",200,150,88,0,0,1800,False,False,2,2,0,0,8,0,0,0,0,0,False,False,False,False,41)
    result[142] = UnitType( 142 ,"Zerg","Zerg_Spawning_Pool",200,0,238,0,0,1200,False,False,3,2,0,0,8,0,0,0,0,0,False,False,False,False,41)
    result[143] = UnitType( 143 ,"Zerg","Zerg_Creep_Colony",75,0,144,0,0,300,False,False,2,2,0,0,10,0,0,0,0,0,False,False,False,False,41)
    result[144] = UnitType( 144 ,"Zerg","Zerg_Spore_Colony",50,0,144,0,0,300,False,False,2,2,0,0,10,0,0,0,224,15,False,False,False,False,143)
    result[146] = UnitType( 146 ,"Zerg","Zerg_Sunken_Colony",50,0,44,0,0,300,False,False,2,2,0,0,10,224,0,40,0,0,False,False,False,False,143)
    result[147] = UnitType( 147 ,"NoneRace","Special_Overmind_With_Shell",1,1,136,0,0,1,False,False,5,3,0,0,8,0,0,0,0,0,False,False,False,False,228)
    result[148] = UnitType( 148 ,"NoneRace","Special_Overmind",1,1,196,0,0,1,False,False,5,3,0,0,8,0,0,0,0,0,False,False,False,False,228)
    result[149] = UnitType( 149 ,"Zerg","Zerg_Extractor",50,0,238,0,0,600,False,False,4,2,0,0,7,0,0,0,0,0,False,False,False,False,41)
    result[150] = UnitType( 150 ,"NoneRace","Special_Mature_Chrysalis",0,0,250,0,0,0,False,False,2,2,0,0,8,0,0,0,0,0,False,False,False,False,228)
    result[151] = UnitType( 151 ,"NoneRace","Special_Cerebrate",0,0,220,0,0,0,False,False,3,2,0,0,8,0,0,0,0,0,False,False,False,False,228)
    result[152] = UnitType( 152 ,"NoneRace","Special_Cerebrate_Daggoth",0,0,220,0,0,0,False,False,3,2,0,0,8,0,0,0,0,0,False,False,False,False,228)
    result[154] = UnitType( 154 ,"Protoss","Protoss_Nexus",400,0,238,750,0,1800,False,False,4,3,0,18,11,0,0,0,0,0,False,False,False,False,64)
    result[155] = UnitType( 155 ,"Protoss","Protoss_Robotics_Facility",200,200,244,500,0,1200,False,False,3,2,0,0,10,0,0,0,0,0,False,False,False,False,64)
    result[156] = UnitType( 156 ,"Protoss","Protoss_Pylon",100,0,44,300,0,450,False,False,2,2,0,16,8,0,0,0,0,0,False,False,False,False,64)
    result[157] = UnitType( 157 ,"Protoss","Protoss_Assimilator",100,0,194,450,0,600,False,False,4,2,0,0,10,0,0,0,0,0,False,False,False,False,64)
    result[159] = UnitType( 159 ,"Protoss","Protoss_Observatory",50,100,250,250,0,450,False,False,3,2,0,0,10,0,0,0,0,0,False,False,False,False,64)
    result[160] = UnitType( 160 ,"Protoss","Protoss_Gateway",150,0,244,500,0,900,False,False,4,3,0,0,10,0,0,0,0,0,False,False,False,False,64)
    result[162] = UnitType( 162 ,"Protoss","Protoss_Photon_Cannon",150,0,100,100,0,750,False,False,2,2,0,0,11,224,0,20,224,20,False,False,False,False,64)
    result[163] = UnitType( 163 ,"Protoss","Protoss_Citadel_Of_Adun",150,100,194,450,0,900,False,False,3,2,0,0,10,0,0,0,0,0,False,False,False,False,64)
    result[164] = UnitType( 164 ,"Protoss","Protoss_Cybernetics_Core",200,0,244,500,0,900,False,False,3,2,0,0,10,0,0,0,0,0,False,False,False,False,64)
    result[165] = UnitType( 165 ,"Protoss","Protoss_Templar_Archives",150,200,244,500,0,900,False,False,3,2,0,0,10,0,0,0,0,0,False,False,False,False,64)
    result[166] = UnitType( 166 ,"Protoss","Protoss_Forge",150,0,38,550,0,600,False,False,3,2,0,0,10,0,0,0,0,0,False,False,False,False,64)
    result[167] = UnitType( 167 ,"Protoss","Protoss_Stargate",150,150,88,600,0,1050,False,False,4,3,0,0,10,0,0,0,0,0,False,False,False,False,64)
    result[168] = UnitType( 168 ,"NoneRace","Special_Stasis_Cell_Prison",150,0,208,0,0,1,False,False,4,3,0,0,8,0,0,0,0,0,False,False,False,False,228)
    result[169] = UnitType( 169 ,"Protoss","Protoss_Fleet_Beacon",300,200,244,500,0,900,False,False,3,2,0,0,10,0,0,0,0,0,False,False,False,False,64)
    result[170] = UnitType( 170 ,"Protoss","Protoss_Arbiter_Tribunal",200,150,244,500,0,900,False,False,3,2,0,0,10,0,0,0,0,0,False,False,False,False,64)
    result[171] = UnitType( 171 ,"Protoss","Protoss_Robotics_Support_Bay",150,100,194,450,0,450,False,False,3,2,0,0,10,0,0,0,0,0,False,False,False,False,64)
    result[172] = UnitType( 172 ,"Protoss","Protoss_Shield_Battery",100,0,200,200,200,450,False,False,3,2,0,0,10,0,0,0,0,0,False,False,False,False,64)
    result[173] = UnitType( 173 ,"NoneRace","Special_Khaydarin_Crystal_Form",250,0,160,0,0,1,False,False,4,3,0,0,10,0,0,0,0,0,False,False,False,False,228)
    result[174] = UnitType( 174 ,"NoneRace","Special_Protoss_Temple",250,0,220,0,0,1,False,False,7,3,0,0,10,0,0,0,0,0,False,False,False,False,228)
    result[175] = UnitType( 175 ,"NoneRace","Special_XelNaga_Temple",1500,500,136,0,0,4800,False,False,5,4,0,0,10,0,0,0,0,0,False,False,False,False,228)
    result[176] = UnitType( 176 ,"OtherRace","Resource_Mineral_Patch1",1,1,160,0,0,1,False,False,2,1,0,0,9,0,0,0,0,0,False,False,False,False,228)
    result[188] = UnitType( 188 ,"OtherRace","Resource_Vespene_Geyser",1,1,160,0,0,1,False,False,4,2,0,0,9,0,0,0,0,0,False,False,False,False,228)
    result[189] = UnitType( 189 ,"NoneRace","Special_Warp_Gate",600,200,188,0,0,2400,False,False,3,2,0,0,8,0,0,0,0,0,False,False,False,False,228)
    result[190] = UnitType( 190 ,"NoneRace","Special_Psi_Disrupter",1000,400,208,0,0,4800,False,False,5,3,0,0,10,0,0,0,0,0,False,False,False,False,228)
    result[200] = UnitType( 200 ,"NoneRace","Special_Power_Generator",200,50,32,0,0,2400,False,False,4,3,0,0,8,0,0,0,0,0,False,False,False,False,228)
    result[201] = UnitType( 201 ,"NoneRace","Special_Overmind_Cocoon",1000,500,196,0,0,2400,False,False,3,2,0,0,10,0,0,0,0,0,False,False,False,False,228)
    result[202] = UnitType( 202 ,"OtherRace","Spell_Dark_Swarm",250,200,32,0,0,2400,False,False,5,5,0,0,8,0,0,0,0,0,False,False,False,False,228)
    result[228] = UnitType( 228 ,"NoneRace","NoneUnitType",0,0,0,0,0,0,False,False,0,0,0,0,0,0,0,0,0,0,False,False,False,False,228)
    result[229] = UnitType( 229 ,"UnknownRace","UnknownUnitType",0,0,0,0,0,0,False,False,0,0,0,0,0,0,0,0,0,0,False,False,False,False,229)
    return result

getUnitTypes()

code = code_gen.gen()
f = open("Unit.hs", 'w')
f.write(code)
f.close()
