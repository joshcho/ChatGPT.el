# chatgpt-emacs.py

from revChatGPT.revChatGPT import Chatbot
from epc.server import EPCServer
import json
import os
import time

server = EPCServer(('localhost', 0))
with open(os.path.expanduser("~/config.json"), 'r') as file:
    config = json.load(file)
chatbot = Chatbot(config)

@server.register_function
def query(query):
    return chatbot.get_chat_response(query)

server.print_port()
server.serve_forever()
