import logging
import time
import traceback
from http.server import BaseHTTPRequestHandler
from utils.messaging.message import Message


class MessageHandler(BaseHTTPRequestHandler):
    def do_POST(self):
        try:
            rcv_time = time.time()
            length = int(self.headers['content-length'])
            msg = self.rfile.read(length).decode('unicode_escape')
            print(f"<= {repr(msg)}")
            msg = Message.parse(msg)
            response = self.server.handle_message(msg, rcv_time)
            if response is not None:
                self.respond(str(response))

        except Exception as e:
            logging.error('Error: ' + str(e) + '\n')
            traceback.print_exc()

    def respond(self, text):
        self.send_response(200, 'OK')
        if text is not None:
            self.send_header('Content-type', 'text/acl')
            self.send_header('Content-length', str(len(text)))
            self.end_headers()
            self.wfile.write(text.encode('utf-8'))
            print(f"=> {repr(text)}")

    def log_message(self, format, *args):
        """Overrides default log printing to keep from crowding the output console."""
        pass
