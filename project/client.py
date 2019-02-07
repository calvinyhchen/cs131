import asyncio
import sys
import config

class EchoClientProtocol(asyncio.Protocol):
    def __init__(self, message, loop):
        self.message = message
        self.loop = loop

    def connection_made(self, transport):
        transport.write(self.message.encode())
        print('Data sent: {!r}'.format(self.message))

    def data_received(self, data):
        print('Data received: {!r}'.format(data.decode()))

    def connection_lost(self, exc):
        print('The server closed the connection')
        print('Stop the event loop')
        self.loop.stop()


def create_client(server_id, message):
    port = config.SERVER_PORT[server_id]

    loop = asyncio.get_event_loop()
    coro = loop.create_connection(lambda: EchoClientProtocol(message, loop), 
                                  '127.0.0.1', port)
    loop.run_until_complete(coro)
    loop.run_forever()
    loop.close()

if __name__ == '__main__':
    try:
        if sys.argv[1] in config.SERVER_IDS:
            create_client(sys.argv[1], sys.argv[2])
        else: 
            sys.exit('Please provide valid client name.')
    except IndexError:
        sys.exit('Please provide valid input.')

# python3 client.py Hands 'IAMAT kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997'
# python3 client.py Goloman 'WHATSAT kiwi.cs.ucla.edu 10 5'
# python3 client.py Hands 'AT Goloman +0.263873386 kiwi.cs.ucla.edu +34.068930-118.445127 1520023934.918963997'
