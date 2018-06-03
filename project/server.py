import asyncio
import aiohttp
import time
import json
import logging
import sys


# Mappings
server_mapping = {  'Goloman': ['Hands', 'Holiday', 'Wilkes'], 
                    'Hands': ['Goloman', 'Wilkes'],
                    'Holiday': ['Goloman', 'Welsh', 'Wilkes'],
                    'Welsh': ['Holiday'],
                    'Wilkes': ['Goloman', 'Hands', 'Holiday']}
port_mapping = {    'Goloman': 12437,
                    'Hands': 12438,
                    'Holiday': 12439,
                    'Welsh': 12440,
                    'Wilkes': 12441 }

# Server variables
name = None
port = None
ports = []
client_info = {}
readers, writers = [], []


class PlacesProxyServer(asyncio.Protocol):
    def connection_made(self, transport):
        # Set up connection
        self.transport = transport
        self.peername = transport.get_extra_info('peername')

        # Log connection
        log = 'Connection from {}'.format(self.peername)
        logger.info(log)

    def data_received(self, data):
        # Parse message
        message = data.decode()
        tokens = message.split()

        # Log message
        log = 'Received: {!r}'.format(message)
        logger.info(log)

        # Determine message type
        if self.is_IAMAT(tokens):
            asyncio.ensure_future(self.do_IAMAT(tokens))
        elif self.is_WHATSAT(tokens):
            asyncio.ensure_future(self.do_WHATSAT(tokens))
        elif self.is_ATSERVER(tokens):
            asyncio.ensure_future(self.do_ATSERVER(tokens))
        elif self.is_CONNECT(tokens):
            asyncio.ensure_future(self.do_CONNECT(tokens))
        else:
            asyncio.ensure_future(self.do_invalid(message))

    def connection_lost(self, exc):
        # Log message
        log = 'Lost connection of {}'.format(self.peername)
        logger.info(log)
        
        # Close connection
        self.transport.close()

    def respond(self, message):
        # Log message
        log = "Sent: {!r}".format(message)
        logger.info(log)

        # Respond to client
        data = message.encode()
        self.transport.write(data)

    async def do_invalid(self, message):
        message = "? {0}".format(message)
        self.respond(message)

    def is_IAMAT(self, tokens):
        def validate_timestamp(timestamp):
            tokens = timestamp.split('.')
            if len(tokens) == 2 \
                and tokens[0].isdigit() and tokens[1].isdigit():
                return True
            return False

        def validate_coordinate(coordinate):
            tokens = coordinate.replace('-', ' ').replace('+', ' ').replace('.', ' ').split()
            if len(tokens) == 4 \
                and tokens[0].isdigit() and tokens[1].isdigit() and tokens[2].isdigit() and tokens[3].isdigit() \
                and int(tokens[0]) <= 180 and int(tokens[2]) <= 180:
                return True
            return False

        return len(tokens) == 4 \
                and tokens[0] == 'IAMAT' \
                and validate_coordinate(tokens[2]) \
                and validate_timestamp(tokens[3])

    async def do_IAMAT(self, tokens):
        # Store info in database
        time_delta = time.time() - float(tokens[3])
        if '+' in tokens[2][1:]:
            i = tokens[2][1:].index('+') + 1
        else:
            i = tokens[2][1:].index('-') + 1
        lat = tokens[2][0:i]
        lon = tokens[2][i:]
        coordinate = (lat, lon)
        client_info[tokens[1]] = (coordinate, time_delta)

        # Flood data to other servers
        for writer in writers:
            #print("ping")
            message = "ATSERVER {0} {1} {2} {3}".format(tokens[1], lat, lon, time_delta)
            writer.write(message.encode())

        message = "AT {0} {1:+f} {2}\r\n".format(name, time_delta, " ".join(tokens[1:]))
        self.respond(message)

    def is_WHATSAT(self, tokens):
        def validate_id(id):
            return id in client_info.keys()

        def validate_radius(radius):
            return radius.isdigit() and int(tokens[2]) > 0 and int(radius) <= 50

        def validate_infocount(infocount):
            return infocount.isdigit() and int(infocount) > 0 and int(infocount) <= 20

        return len(tokens) == 4 \
                and tokens[0] == 'WHATSAT' \
                and validate_id(tokens[1]) \
                and validate_radius(tokens[2]) \
                and validate_infocount(tokens[3])

    async def do_WHATSAT(self, tokens):
        async def fetch(session, url):
            async with session.get(url) as response:
                return await response.text()

        async def do_proxy(coordinate, radius):
            async with aiohttp.ClientSession() as session:
                url = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?location={0},{1}&radius={2}&key=AIzaSyDm54c7TzP5uqyOTa37IiWnzIy2ag_BwTM".format(coordinate[0], coordinate[1], radius)
                html = await fetch(session, url)
                loaded_html = json.loads(html)
                
                # Check if valid request
                if loaded_html['status'] == 'INVALID_REQUEST':
                    return [], False

                return loaded_html['results'], True

        coordinate = client_info[tokens[1]][0]
        time_delta = client_info[tokens[1]][1]
        message = "AT {0} {1:+f} {2} {3}{4} {5}\r\n".format(name, time_delta, tokens[1], coordinate[0], coordinate[1], time.time())
        self.respond(message)

        # GET JSON response from Google Places
        results, valid = await asyncio.ensure_future(do_proxy(coordinate, tokens[2]))
        if not valid:
            message = "? {0}".format(" ".join(tokens))
            self.respond(message)
        else:
            message = json.dumps(results[0:int(tokens[3])], sort_keys=True, indent=4, separators=(',', ': ')) + "\n"
            self.respond(message)

    def is_ATSERVER(self, tokens):
        return len(tokens) == 5 \
                and tokens[0] == 'ATSERVER'

    async def do_ATSERVER(self, tokens):
        if tokens[1] in client_info.keys() and client_info[tokens[1]] == ((tokens[2], tokens[3]), float(tokens[4])):
            return
        else:
            #print("ding")
            # Store info in database
            client_info[tokens[1]] = ((tokens[2], tokens[3]), float(tokens[4]))

            # Flood data to other servers
            for writer in writers:
                message = " ".join(tokens)
                writer.write(message.encode())

    def is_CONNECT(self, tokens):
        return len(tokens) == 2 \
                and tokens[0] == 'CONNECT'

    def do_CONNECT(self, tokens):
        try:
            reader, writer = yield from asyncio.open_connection('127.0.0.1', tokens[1], loop=loop)
            readers.append(reader)
            writers.append(writer)
            #print('yay')
        except ConnectionRefusedError:
            pass

@asyncio.coroutine
def connect_servers(ports, loop):
    readers, writers = [], []
    for port in ports:
        try:
            reader, writer = yield from asyncio.open_connection('127.0.0.1', port, loop=loop)
            readers.append(reader)
            writers.append(writer)
        except ConnectionRefusedError:
            #print(port)
            pass

    return readers, writers


if __name__ == '__main__':
    # Parse cmdline arguments
    if len(sys.argv) != 2 or sys.argv[1] not in server_mapping.keys():
        print("Invalid command line arguments")
        sys.exit(1)
    else:
        name = sys.argv[1]
        port = port_mapping[name]
        ports = [port_mapping[server] for server in server_mapping[sys.argv[1]]]

    # Set up logger
    logger = logging.getLogger('PlacesProxyServerLog')
    logger.setLevel(logging.DEBUG)
    logger.addHandler(logging.FileHandler('{0}.log'.format(name)))
    logger.info('---PlacesProxyServerLog_{0}---'.format(name))

    # Setup server
    loop = asyncio.get_event_loop()
    coro = loop.create_server(PlacesProxyServer, '127.0.0.1', port)
    server = loop.run_until_complete(coro)
    
    # Connect to other servers
    readers, writers = loop.run_until_complete(connect_servers(ports, loop))
    message = 'CONNECT {}'.format(port)
    for writer in writers:
        writer.write(message.encode())

    # Run server
    try:
        loop.run_forever()
    except KeyboardInterrupt:
        pass

    # Close
    server.close()
    loop.run_until_complete(server.wait_closed())
    loop.close()
