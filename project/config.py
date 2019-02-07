SERVER_IDS = ['Goloman', 'Hands', 'Holiday', 'Welsh', 'Wilkes']

# Ports: 11389 <= x <= 11396
SERVER_PORT = { 
    'Goloman' : 11389, 
    'Hands' :   11390, 
    'Holiday' : 11391, 
    'Welsh' :   11392,  
    'Wilkes' :  11393
}

# Goloman talks with Hands, Holiday and Wilkes.
# Hands talks with Wilkes.
# Holiday talks with Welsh and Wilkes.

SERVER_FLOOD = {
    'Goloman' : ['Hands', 'Holiday', 'Wilkes'], 
    'Hands' :   ['Goloman', 'Wilkes'], 
    'Holiday' : ['Goloman', 'Welsh', 'Wilkes'], 
    'Welsh' :   ['Holiday'], 
    'Wilkes' :  ['Goloman', 'Hands', 'Holiday']
}


GOOGLE_API_KEY = 'enter key here'