if __name__ == '__main__2':
    import sys
    if len(sys.argv) < 2:
        print("bad")
    else:
        print(sys.argv[1])
    import time

    print(time.time())

elif True:
    import asyncio
    import aiohttp
    import time
    import json
    import logging

    class PlacesProxyServer(asyncio.Protocol):
        def __init__(self, name):
            self.name = name
            self.client_info = {}

            # Set up logger
            self.logger = logging.getLogger('PlacesProxyServerLog')
            self.logger.setLevel(logging.DEBUG)
            self.logger.addHandler(logging.FileHandler('{0}.log'.format(self.name)))
            self.logger.info('---PlacesProxyServerLog_{0}---'.format(self.name))

        def connection_made(self, transport):
            # Set up connection
            self.transport = transport
            self.peername = transport.get_extra_info('peername')

            # Log connection
            log = 'Connection from {}'.format(self.peername)
            self.logger.info(log)

        def data_received(self, data):
            # Parse message
            message = data.decode()
            tokens = message.split()

            # Log message
            log = 'Received: {!r}'.format(message)
            self.logger.info(log)

            # Determine message type
            if len(tokens) == 0:
                asyncio.ensure_future(self.do_invalid(message))
            elif self.is_IAMAT(tokens):
                asyncio.ensure_future(self.do_IAMAT(tokens))
            elif self.is_WHATSAT(tokens):
                asyncio.ensure_future(self.do_WHATSAT(tokens))
            else:
                asyncio.ensure_future(self.do_invalid(message))

        def connection_lost(self, exc):
            # Log message
            log = 'Lost connection of {}'.format(self.peername)
            self.logger.info(log)
            
            # Close connection
            self.transport.close()

        async def do_invalid(self, message):
            message = "? {0}".format(message)
            
            # Log message
            log = "Sent: {!r}".format(message)
            self.logger.info(log)

            # Respond to client
            data = message.encode()
            self.transport.write(data)

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

            return tokens[0] == 'IAMAT' \
                    and len(tokens) == 4 \
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
            self.client_info[tokens[1]] = (coordinate, time_delta)

            # Flood data to other servers
            pass

            message = "AT {0} {1:+f} {2}\r\n".format(self.name, time_delta, " ".join(tokens))

            # Log message
            log = "Sent: {!r}".format(message)
            self.logger.info(log)

            # Respond to client
            data = message.encode()
            self.transport.write(data)

        def is_WHATSAT(self, tokens):
            def validate_id(id):
                return id in self.client_info.keys()

            def validate_radius(radius):
                return radius.isdigit() and int(tokens[2]) >= 0 and int(radius) <= 50

            def validate_infocount(infocount):
                return infocount.isdigit() and int(infocount) > 0 and int(infocount) <= 20

            return tokens[0] == 'WHATSAT' \
                    and len(tokens) == 4 \
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

            coordinate = self.client_info[tokens[1]][0]
            time_delta = self.client_info[tokens[1]][1]
            message = "AT {0} {1:+f} {2} {3}{4} {5}\r\n".format(self.name, time_delta, tokens[1], coordinate[0], coordinate[1], time.time())
            
            # Log message
            log = "Sent: {!r}".format(message)
            self.logger.info(log)

            # Respond to client
            data = message.encode()
            self.transport.write(data)

            # GET JSON response from Google Places
            results, valid = await asyncio.ensure_future(do_proxy(coordinate, tokens[2]))
            if not valid:
                message = "? {0}".format(" ".join(tokens))

                # Log message
                log = "Sent: {!r}".format(message)
                self.logger.info(log)

                # Respond to client
                data = message.encode()
                self.transport.write(data)
            else:
                message = json.dumps(results[0:int(tokens[3])], sort_keys=True, indent=4, separators=(',', ': ')) + "\n"

                # Log message
                log = "Sent: {!r}".format(message)
                self.logger.info(log)

                # Respond to client
                data = message.encode()
                self.transport.write(data)

    loop = asyncio.get_event_loop()
    coro = loop.create_server(lambda: PlacesProxyServer("Goloman"), '127.0.0.1', 12437)
    server = loop.run_until_complete(coro)

    print('Serving on {}'.format(server.sockets[0].getsockname()))
    try:
        loop.run_forever()
    except KeyboardInterrupt:
        pass

    # Close
    server.close()
    loop.run_until_complete(server.wait_closed())
    loop.close()
