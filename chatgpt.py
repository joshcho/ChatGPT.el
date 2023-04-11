# chatgpt.py

import pkg_resources
from epc.server import EPCServer
# Hedge against breaking changes in chatgpt-wrapper >= 0.5.0
try:
    from chatgpt_wrapper import ChatGPT
    from chatgpt_wrapper.config import Config
except ImportError:
    from chatgpt_wrapper.backends.browser.chatgpt import ChatGPT
    from chatgpt_wrapper.core.config import Config

server = EPCServer(('localhost', 0))
bot = None

@server.register_function
def query(query):
    global bot
    if bot is None:
        config = Config()
        config.set("backend", "chatgpt-browser")
        bot = ChatGPT(config)
        bot.launch_browser()

    # Hedge against more breaking changes in chatgpt-wrapper >= 0.5.0
    response = bot.ask(query)
    try:
        success, response, message = response
    except ValueError:
        pass

    return response

server.print_port()
server.serve_forever()
