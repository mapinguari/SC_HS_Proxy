
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
        code = "module Proxy.Upgrade where\nimport Proxy.UnitTypes\nimport Proxy.UpgradeTypes\n\n"
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
        addfunc("whatResearches")
        addfunc("repeats")
        addfunc("mineralsBase")
        addfunc("mineralsFactor")
        addfunc("gasBase")
        addfunc("gasFactor")

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
    Protoss_Citadel_of_Adun = 163
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
    Resource_Mineral_Field = 176
    Resource_Vespene_Geyser = 188
    Special_Warp_Gate = 189
    Special_Psi_Disrupter = 190
    Special_Power_Generator = 200
    Special_Overmind_Cocoon = 201
    Spell_Dark_Swarm = 202
    NoneUnitType = 228
    UnknownUnitType = 229

def convert_unit_id(i):
    for attr, value in UnitTypes.__dict__.iteritems():
        if value == i:
            return fix(attr)

class UpgradeType(object):
    def __init__(self,  id,  name,  whatResearchesID,  repeats,  mineralsBase,  mineralsFactor,  gasBase,  gasFactor):
        def addcase(func_name, code):
            code_gen.addcase(func_name, (fix(name), code))
        addcase("whatResearches", convert_unit_id(whatResearchesID))
        addcase("repeats", repeats.__str__())
        addcase("mineralsBase", mineralsBase.__str__())
        addcase("mineralsFactor", mineralsFactor.__str__())
        addcase("gasBase", gasBase.__str__())
        addcase("gasFactor", gasFactor.__str__())

def getUpgradeTypes( ):
    result = {}
    result[0] = UpgradeType(0,"Terran_Infantry_Armor",122,3,100,75,75,75)
    result[1] = UpgradeType(1,"Terran_Vehicle_Plating",123,3,100,75,75,75)
    result[2] = UpgradeType(2,"Terran_Ship_Plating",123,3,150,75,75,75)
    result[3] = UpgradeType(3,"Zerg_Carapace",139,3,150,75,75,75)
    result[4] = UpgradeType(4,"Zerg_Flyer_Carapace",141,3,150,75,75,75)
    result[5] = UpgradeType(5,"Protoss_Armor",166,3,100,75,75,75)
    result[6] = UpgradeType(6,"Protoss_Plating",164,3,150,75,75,75)
    result[7] = UpgradeType(7,"Terran_Infantry_Weapons",122,3,100,75,75,75)
    result[8] = UpgradeType(8,"Terran_Vehicle_Weapons",123,3,100,75,75,75)
    result[9] = UpgradeType(9,"Terran_Ship_Weapons",123,3,100,50,50,50)
    result[10] = UpgradeType(10,"Zerg_Melee_Attacks",139,3,100,50,50,50)
    result[11] = UpgradeType(11,"Zerg_Missile_Attacks",139,3,100,50,50,50)
    result[12] = UpgradeType(12,"Zerg_Flyer_Attacks",141,3,100,75,75,75)
    result[13] = UpgradeType(13,"Protoss_Ground_Weapons",166,3,100,50,50,50)
    result[14] = UpgradeType(14,"Protoss_Air_Weapons",164,3,100,75,75,75)
    result[15] = UpgradeType(15,"Protoss_Plasma_Shields",166,3,200,100,100,100)
    result[16] = UpgradeType(16,"U_238_Shells",112,1,150,0,0,0)
    result[17] = UpgradeType(17,"Ion_Thrusters",120,1,100,0,0,0)
    result[19] = UpgradeType(19,"Titan_Reactor",116,1,150,0,0,0)
    result[20] = UpgradeType(20,"Ocular_Implants",117,1,100,0,0,0)
    result[21] = UpgradeType(21,"Moebius_Reactor",117,1,150,0,0,0)
    result[22] = UpgradeType(22,"Apollo_Reactor",115,1,200,0,0,0)
    result[23] = UpgradeType(23,"Colossus_Reactor",118,1,150,0,0,0)
    result[24] = UpgradeType(24,"Ventral_Sacs",132,1,200,0,0,0)
    result[25] = UpgradeType(25,"Antennae",132,1,150,0,0,0)
    result[26] = UpgradeType(26,"Pneumatized_Carapace",132,1,150,0,0,0)
    result[27] = UpgradeType(27,"Metabolic_Boost",142,1,100,0,0,0)
    result[28] = UpgradeType(28,"Adrenal_Glands",142,1,200,0,0,0)
    result[29] = UpgradeType(29,"Muscular_Augments",135,1,150,0,0,0)
    result[30] = UpgradeType(30,"Grooved_Spines",135,1,150,0,0,0)
    result[31] = UpgradeType(31,"Gamete_Meiosis",138,1,150,0,0,0)
    result[32] = UpgradeType(32,"Metasynaptic_Node",136,1,150,0,0,0)
    result[33] = UpgradeType(33,"Singularity_Charge",164,1,150,0,0,0)
    result[34] = UpgradeType(34,"Leg_Enhancements",163,1,150,0,0,0)
    result[35] = UpgradeType(35,"Scarab_Damage",171,1,200,0,0,0)
    result[36] = UpgradeType(36,"Reaver_Capacity",171,1,200,0,0,0)
    result[37] = UpgradeType(37,"Gravitic_Drive",171,1,200,0,0,0)
    result[38] = UpgradeType(38,"Sensor_Array",159,1,150,0,0,0)
    result[39] = UpgradeType(39,"Gravitic_Boosters",159,1,150,0,0,0)
    result[40] = UpgradeType(40,"Khaydarin_Amulet",165,1,150,0,0,0)
    result[41] = UpgradeType(41,"Apial_Sensors",165,1,100,0,0,0)
    result[42] = UpgradeType(42,"Gravitic_Thrusters",169,1,200,0,0,0)
    result[43] = UpgradeType(43,"Carrier_Capacity",169,1,100,0,0,0)
    result[44] = UpgradeType(44,"Khaydarin_Core",170,1,150,0,0,0)
    result[47] = UpgradeType(47,"Argus_Jewel",169,1,100,0,0,0)
    result[49] = UpgradeType(49,"Argus_Talisman",165,1,150,0,0,0)
    result[51] = UpgradeType(51,"Caduceus_Reactor",112,1,150,0,0,0)
    result[52] = UpgradeType(52,"Chitinous_Plating",140,1,150,0,0,0)
    result[53] = UpgradeType(53,"Anabolic_Synthesis",140,1,200,0,0,0)
    result[54] = UpgradeType(54,"Charon_Booster",120,1,100,0,0,0)
    result[61] = UpgradeType(61,"NoneUpgrade",228,0,0,0,0,0)
    result[62] = UpgradeType(62,"UnknownUpgrade",228,0,0,0,0,0)
    return result

getUpgradeTypes()

code = code_gen.gen()
f = open("Upgrade.hs", 'w')
f.write(code)
f.close()
