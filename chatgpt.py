# chatgpt.py

from epc.server import EPCServer
from chatgpt_wrapper import ChatGPT

server = EPCServer(('localhost', 0))

bot = None

@server.register_function
def query(query):
    global bot
    if bot == None:
        bot = ChatGPT()
    return bot.ask(query)

server.print_port()
server.serve_forever()
