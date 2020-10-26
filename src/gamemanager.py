import sys
from optparse import OptionParser
from http.server import HTTPServer, BaseHTTPRequestHandler


class GameManager(HTTPServer):
    class RequestHandler(BaseHTTPRequestHandler):
        def do_POST(self):
            try:
                print(self.server)
                sys.stdout.write('received POST')
            except Exception as e:
                sys.stderr.write('Error: ' + str(e) + '\n')
                return

        def do_GET(self):
            try:
                print(self.server)
                sys.stdout.write('received GET')
            except Exception as e:
                sys.stderr.write('Error: ' + str(e) + '\n')
                return

    def __init__(self, port):
        HTTPServer.__init__(self, ('', port), self.RequestHandler)
        self.players = list()

    def add_player(self, name, host, port):



DEFAULT_PORT = 5600

if __name__ == "__main__":
    """""""""""""""
    PARSE ARGUMENTS
    """""""""""""""
    parser = OptionParser()
    parser.add_option('-p', '--port', dest='port',
                      help='the port on which the manager listens for messages', metavar='PORT',
                      default=DEFAULT_PORT)
    (opts, args) = parser.parse_args()
    """""""""""""""
    RUN GAMEMANAGER
    """""""""""""""
    manager = GameManager(int(opts.port))
    try:
        manager.serve_forever()
    except KeyboardInterrupt:
        print('Shutting down player...')
        manager.socket.close()