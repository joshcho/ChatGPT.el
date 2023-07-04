# chatgpt.py
import pkg_resources
from epc.server import EPCServer
from lwe import ApiBackend

server = EPCServer(('localhost', 0))
bot = None

@server.register_function
def query(query):
    global bot
    if bot is None:
        bot = ApiBackend()
    success, response, message = bot.ask(query)
    if success:
        return response
    return message

server.print_port()
server.serve_forever()
