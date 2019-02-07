import sys
import config
import logging
import asyncio
import time
import aiohttp
import json
import datetime

GOOGLE_API_URL = "https://maps.googleapis.com/maps/api/place/nearbysearch/json"


class EchoServerClientProtocol(asyncio.Protocol):
    client_stamps = {}

    def __init__(self, server_id):
        self.server_id = server_id
        self.flood_list = config.SERVER_FLOOD[server_id]

    def connection_made(self, transport):
        peername = transport.get_extra_info('peername')
        logger.info('Connected from {!r}'.format(peername))
        self.transport = transport
        
        # TA
        self.pending_buffer = ""

    def data_received(self, data):
        message = data.decode()
        logger.info('Data received: {!r}'.format(message))

        # TA
        self.pending_buffer += message
        message_list = self.pending_buffer.split()
        
        # IAMAT kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997
        if message_list[0] == 'IAMAT' and self.check_args(message_list):
            timestamp = time.time() - float(message_list[3])
            timestamp = '{:.9f}'.format(timestamp)
            if float(timestamp) > 0:
                timestamp = '+' + timestamp
            stamp =('AT ' + self.server_id 
                                + ' ' + timestamp 
                                + ' ' + message_list[1] 
                                + ' ' + message_list[2] 
                                + ' ' + message_list[3]) 
            self.transport.write(stamp.encode())
            logger.info('Respond message: {!r}'.format(stamp))
            if self.update_client_stamp(message_list[1], stamp):
                flood_input = stamp + ' ' + self.server_id
                self.flood_neighbor(flood_input)
            peername = self.transport.get_extra_info('peername')
            logger.info('Disconnect: {!r}'.format(peername))
            self.transport.close()
            
        # WHATSAT kiwi.cs.ucla.edu 10 5
        elif message_list[0] == 'WHATSAT' and self.check_args(message_list):
            self.run_WHATSAT(message_list[1], message_list[2], message_list[3])

        # AT Goloman +0.263873386 kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997 [server_id]
        elif message_list[0] == 'AT' and self.check_args(message_list):
            flooded_server_list = message_list[6:]
            client_stamp = ' '.join(message_list[0:6])
            self.update_client_stamp(message_list[3], client_stamp)
            origin_input = ' '.join(message_list)
            flood_input = origin_input + ' ' + self.server_id
            self.flood_neighbor(flood_input)
            peername = self.transport.get_extra_info('peername')
            logger.info('Disconnect: {!r}'.format(peername))
            self.transport.close()

        else:
            error_msg = '? ' + message
            self.transport.write(error_msg.encode())
            logger.info('Respond message: {!r}'.format(error_msg))
            peername = self.transport.get_extra_info('peername')
            logger.info('Disconnect: {!r}'.format(peername))
            self.transport.close()
 

    def update_client_stamp(self, client_id, client_stamp):
        # AT Goloman +8135936.221702099 kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997
        stamp = client_stamp.split()
        print(stamp)
        try:
            if float(stamp[5]) > float(self.client_stamps[client_id].split()[5]):
                self.client_stamps[client_id] = client_stamp
                logger.info('Update stamp: {}'.format(client_id))
                return True
            else:
                logger.info('Didnt update stamp: {}'.format(client_id))
                return False
        except:
            self.client_stamps[client_id] = client_stamp
            logger.info('New stamp: {}'.format(client_id))
            return True


    def flood_neighbor(self, flood_input):
        flooded = flood_input.split()[6:]
        flood_input = flood_input.split() + config.SERVER_FLOOD[self.server_id]
        flood_input = ' '.join(flood_input)
        for server_id in self.flood_list:
            if server_id not in flooded:
                self.propagate(server_id, flood_input)

    def propagate(self, server_id, flood_input):
        port = config.SERVER_PORT[server_id]
        coro = loop.create_connection(lambda: EchoClientProtocol(flood_input, server_id),
                                     '127.0.0.1', port)
        loop.create_task(coro)

    def check_args(self, args):
        if args[0] == 'IAMAT':
            # IAMAT kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997
            if len(args) != 4:
                logger.error('Invalid args for WHATSAT')
                return False
            if not self.check_location(args[2], args[1]):
                logger.error('Invalid location')
                return False
            try:
                time = float(args[3])
                datetime.datetime.utcfromtimestamp(time)
            except ValueError:
                print('false')
                return False         
        elif args[0] == 'WHATSAT':
            # WHATSAT kiwi.cs.ucla.edu 10 5
            if len(args) != 4:
                logger.error('Invalid args for WHATSAT')
                return False            
            try:
                self.client_stamps[args[1]]
            except KeyError:
                logger.error('Invalid client id')
                return False
            try:
                radius = float(args[2])
                if radius >= 50 or radius < 0:
                    logger.error('Invalid radius')
                    return False
            except ValueError:
                logger.error('Invalid type for radius')
                return False
            try:
                bound = int(args[3])                
                if bound >= 20 or bound < 0:
                    logger.error('Invalid bound')
                    return False
            except ValueError:
                logger.error('Invalid type for bound')
                return False
        elif args[0] == 'AT':
            # AT Goloman +0.263873386 kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997 server_id
            if not self.check_location(args[4], args[3]):
                logger.error('Invalid location')
                return False
            try:
                time = float(args[5])
                datetime.datetime.utcfromtimestamp(time)
            except ValueError:
                print('false')
                return False             
        return True

    def check_location(self, location, client_id):
        client_location = ['','']
        index = 0
        try:
            for c in location:
                if c == '+' or c == '-' and index == 0:
                    index = 1
                    client_location[index-1] += c
                elif c == '+' or c == '-' and index == 1:
                    index = 2
                    client_location[index-1] += c
                else:
                    client_location[index-1] += c
            client_location[0] = float(client_location[0])
            client_location[1] = float(client_location[1])
        except ValueError:
            return False
        if client_location[0] > 90 or client_location[0] < -90: 
            return False
        if client_location[1] > 180 or client_location[1] < -180: 
            return False
        return True

    def run_WHATSAT(self, client_id, radius, bound):
        # WHATSAT kiwi.cs.ucla.edu 10 5
        location = self.client_stamps[client_id].split()[4]

        client_location = ['','']
        index = 0
        for c in location:
            if c == '+' or c == '-' and index == 0:
                index = 1
                client_location[index-1] += c
            elif c == '+' or c == '-' and index == 1:
                index = 2
                client_location[index-1] += c
            else:
                client_location[index-1] += c

        logger.info('Location of {}: {} {}'.format(client_id, client_location[0], client_location[1]))
        
        radius = str(float(radius) * 1000)

        url = GOOGLE_API_URL + '?key=' + config.GOOGLE_API_KEY
        url += '&location=' + client_location[0] + ',' + client_location[1]
        url += '&radius=' + radius

        loop = asyncio.get_event_loop()
        loop.create_task(self.request_url(url, int(bound), self.client_stamps[client_id]))

    # TA course
    async def request_url(self, url, bound, stamp):
        async with aiohttp.ClientSession() as session:
            html = await self.fetch(session, url)
            my_obj = json.loads(html)
            self.parse_json(my_obj, bound, stamp)

    async def fetch(self, session, url):
        async with session.get(url) as response:
            return await response.text()

    def parse_json(self, json_obj, bound, stamp):
        if len(json_obj['results']) > bound:
            json_obj['results'] = json_obj['results'][:bound]
        response = stamp + '\n' + json.dumps(json_obj, indent=2) + '\n\n'

        self.transport.write(response.encode())
        logger.info('Response json: {!r}'.format(response))
        self.transport.close()
        peername = self.transport.get_extra_info('peername')
        logger.info('Disconnect: {!r}'.format(peername))

class EchoClientProtocol(asyncio.Protocol):
    def __init__(self, message, server_id):
        self.message = message
        self.server_id = server_id

    def connection_made(self, transport):
        self.transport = transport
        transport.write(self.message.encode())
        logger.info('Connect and propagate to {}'.format(self.server_id))

    def connection_lost(self, exc):
        logger.info('Disconnect: {}'.format(self.server_id))
        self.transport.close()
        

def create_server(server_id):
    port = config.SERVER_PORT[server_id]

    # https://docs.python.org/3/library/asyncio-protocol.html#protocol-classes
    coro = loop.create_server(lambda: EchoServerClientProtocol(server_id), 
                              '127.0.0.1', port)
    server = loop.run_until_complete(coro)
    
    # https://docs.python.org/3/library/logging.html
    fmt = '[%(name)s] INFO %(asctime)s : %(message)s'
    log_formatter = logging.Formatter(fmt)

    log_dest = './log/' + server_id.lower() + '.log'
    file_handler = logging.FileHandler(log_dest, mode='w')
    file_handler.setFormatter(log_formatter)

    stream_handler = logging.StreamHandler()
    stream_handler.setFormatter(log_formatter)

    logger.setLevel('INFO')
    logger.addHandler(file_handler)
    logger.addHandler(stream_handler)

    logger.info('Serving on {}'.format(server.sockets[0].getsockname()))

    # Serve requests until Ctrl+C is pressed
    try:
        loop.run_forever()
    except KeyboardInterrupt:
        pass

    # Close the server
    server.close()
    loop.run_until_complete(server.wait_closed())
    loop.close()

def catch_exceptions(loop, context):
    try:
        exception = context['exception']
        if isinstance(exception, ConnectionRefusedError):
            logger.error('Failed to connect!')
        else:
            logger.error('Error: {}'.format(context['exception']))
    except KeyError:
        logger.error('Error: {}'.format(context['message']))


if __name__ == "__main__":
    try:
        if sys.argv[1] in config.SERVER_IDS:
            logger = logging.getLogger(sys.argv[1])
            loop = asyncio.get_event_loop()
            loop.set_exception_handler(catch_exceptions)
            create_server(sys.argv[1])
        else: 
            sys.exit('Please provide valid server name.')
    except IndexError:
        sys.exit('Please provide server name.')
