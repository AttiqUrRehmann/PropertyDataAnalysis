
suburbs = [
    "Acton", "Ainslie", "Amaroo", "Aranda", "Banks", "Barton", "Belconnen", "Bonner",
    "Bonython", "Booroomba", "Braddon", "Bruce", "Calwell", "Campbell", "Casey", "Chapman",
    "Charnwood", "Chifley", "Chisholm", "City", "Conder", "Cook", "Coombs", "Crace",
    "Curtin", "Deakin", "Dickson", "Downer", "Duffy", "Dunlop", "Evatt", "Fadden",
    "Farrer", "Fisher", "Florey", "Flynn", "Forde", "Forrest", "Franklin", "Fraser",
    "Fyshwick", "Garran", "Gilmore", "Giralang", "Gordon", "Gowrie", "Greenway",
    "Griffith", "Gungahlin", "Hackett", "Hall", "Harrison", "Hawker", "Higgins",
    "Holder", "Holt", "Hughes", "Hume", "Isaacs", "Isabella Plains", "Jacka",
    "Kaleen", "Kambah", "Kingston", "Latham", "Lawson", "Lyneham", "Lyons",
    "Macarthur", "Macgregor", "Macquarie", "Mawson", "McKellar", "Melba",
    "Mitchell", "Monash", "Moncrieff", "Narrabundah", "Ngunnawal", "Nicholls",
    "Oaks Estate", "O'Connor", "O'Malley", "Oxley", "Page", "Palmerston",
    "Parkes", "Pearce", "Phillip", "Pialligo", "Red Hill", "Reid", "Richardson",
    "Rivett", "Russell", "Scullin", "Spence", "Stirling", "Swinger Hill",
    "Symonston", "Tharwa", "Theodore", "Torrens", "Tuggeranong", "Turner",
    "Wanniassa", "Waramanga", "Watson", "Weetangera", "Weston", "Weston Creek",
    "Wright", "Yarralumla"
]

postal_codes = [
    2601, 2602, 2914, 2614, 2906, 2600, 2617, 2914,
    2905, 2620, 2612, 2617, 2905, 2612, 2913, 2611,
    2615, 2606, 2905, 2601, 2906, 2614, 2611, 2911,
    2605, 2600, 2602, 2602, 2611, 2615, 2617, 2904,
    2607, 2615, 2615, 2615, 2914, 2603, 2913, 2615,
    2609, 2605, 2905, 2617, 2906, 2904, 2900,
    2603, 2912, 2602, 2618, 2914, 2614, 2615,
    2611, 2615, 2605, 2620, 2607, 2905, 2914,
    2617, 2902, 2604, 2615, 2617, 2602, 2620,
    2615, 2615, 2614, 2607, 2617, 2615,
    2911, 2904, 2914, 2604, 2913, 2913,
    2620, 2602, 2606, 2903, 2614, 2913,
    2600, 2607, 2606, 2609, 2603, 2612, 2905,
    2611, 2600, 2615, 2615, 2611, 2611,
    2609, 2620, 2905, 2607, 2901, 2612,
    2903, 2611, 2602, 2614, 2611, 2611,
    2611, 2600
]

# Combine into a dictionary
suburb_postcode_map = dict(zip(suburbs, postal_codes))
