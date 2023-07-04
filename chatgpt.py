# chatgpt.py

from epc.server import EPCServer
from lwe.backends.api.backend import ApiBackend
from lwe.backends.browser.backend import BrowserBackend
from lwe.core.config import Config

server = EPCServer(("localhost", 0))
bot = None


@server.register_function
def query(query, backend="browser", model="default"):
    global bot
    if bot is None:
        config = Config()
        if backend == "browser":
            config.set("backend", backend)
            config.set("chat.model", model)
            bot = BrowserBackend(config)
            bot.launch_browser()
        elif backend == "api":
            config.set("backend", backend)
            config.set("chat.model", model)
            bot = ApiBackend(config)
        else:
            return (
                f"ChatGPT.el: Unknown chatgpt-wrapper backend: '{backend}'. "
                f"Options are: 'browser', 'api'."
            )

    response = bot.ask(query)
    try:
        success, response, message = response
    except ValueError:
        pass

    return response


server.print_port()
server.serve_forever()
