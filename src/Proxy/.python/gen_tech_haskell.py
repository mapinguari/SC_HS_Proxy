
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
        code = "module Proxy.Tech where\nimport Proxy.UnitTypes\nimport Proxy.TechTypes\n\n"
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
        addfunc("mineralsCost")
        addfunc("gasCost")

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

class TechType(object):
    def __init__(self, id, name, whatResearchesID, mineralsCost, gasCost):
        def addcase(func_name, code):
            code_gen.addcase(func_name, (fix(name), code))
        addcase("whatResearches", convert_unit_id(whatResearchesID))
        addcase("mineralsCost", mineralsCost.__str__())
        addcase("gasCost", gasCost.__str__())

def getTechTypes():
    result = {}
    result[0] = TechType(0,"Stim_Packs",112,100,100)
    result[1] = TechType(1,"Lockdown",117,200,200)
    result[2] = TechType(2,"EMP_Shockwave",116,200,200)
    result[3] = TechType(3,"Spider_Mines",120,100,100)
    result[4] = TechType(4,"Scanner_Sweep",228,0,0)
    result[5] = TechType(5,"Tank_Siege_Mode",120,150,150)
    result[6] = TechType(6,"Defensive_Matrix",228,0,0)
    result[7] = TechType(7,"Irradiate",116,200,200)
    result[8] = TechType(8,"Yamato_Gun",118,100,100)
    result[9] = TechType(9,"Cloaking_Field",115,150,150)
    result[10] = TechType(10,"Personnel_Cloaking",117,100,100)
    result[11] = TechType(11,"Burrowing",131,100,100)
    result[12] = TechType(12,"Infestation",228,0,0)
    result[13] = TechType(13,"Spawn_Broodlings",138,100,100)
    result[14] = TechType(14,"Dark_Swarm",228,0,0)
    result[15] = TechType(15,"Plague",136,200,200)
    result[16] = TechType(16,"Consume",136,100,100)
    result[17] = TechType(17,"Ensnare",138,100,100)
    result[18] = TechType(18,"Parasite",228,0,0)
    result[19] = TechType(19,"Psionic_Storm",165,200,200)
    result[20] = TechType(20,"Hallucination",165,150,150)
    result[21] = TechType(21,"Recall",170,150,150)
    result[22] = TechType(22,"Stasis_Field",170,150,150)
    result[23] = TechType(23,"Archon_Warp",228,0,0)
    result[24] = TechType(24,"Restoration",228,100,100)
    result[25] = TechType(25,"Disruption_Web",169,200,200)
    result[27] = TechType(27,"Mind_Control",165,200,200)
    result[28] = TechType(28,"Dark_Archon_Meld",228,0,0)
    result[29] = TechType(29,"Feedback",228,0,0)
    result[30] = TechType(30,"Optical_Flare",112,100,100)
    result[31] = TechType(31,"Maelstrom",165,100,100)
    result[32] = TechType(32,"Lurker_Aspect",135,200,200)
    result[34] = TechType(34,"Healing",228,0,0)
    result[44] = TechType(44,"NoneTechType",228,0,0)
    result[45] = TechType(45,"UnknownTechType",228,0,0)
    result[46] = TechType(46,"Nuclear_Strike",228,0,0)
    return result

getTechTypes()

code = code_gen.gen()
f = open("Tech.hs", 'w')
f.write(code)
f.close()
