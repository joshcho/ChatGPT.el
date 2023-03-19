# chatgpt.py

import pkg_resources
from epc.server import EPCServer
from chatgpt_wrapper import ChatGPT

MIN_BREAKCHANGE_VERSION = "0.5.0"
IS_BREAKING_CHANGE = pkg_resources.get_distribution("ChatGPT").parsed_version \
                     >= pkg_resources.parse_version(MIN_BREAKCHANGE_VERSION)

server = EPCServer(('localhost', 0))
bot = None

@server.register_function
def query(query):
    global bot
    if bot is None:
        bot = ChatGPT()
    response = bot.ask(query)
    if IS_BREAKING_CHANGE:
        # the return values have changed since 0.5.0
        # https://github.com/mmabrouk/chatgpt-wrapper/commit/bc13f3dfc838aaa9299a5137723718081acd8eac#diff-b335630551682c19a781afebcf4d07bf978fb1f8ac04c6bf87428ed5106870f5R21
        success, response, message = response
    return response

server.print_port()
server.serve_forever()
