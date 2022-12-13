# chatgpt.py

from epc.server import EPCServer
from chatgpt_wrapper import ChatGPT

server = EPCServer(('localhost', 0))

@server.register_function
def query(query):
    bot = ChatGPT()
    return bot.ask(query)

server.print_port()
server.serve_forever()
