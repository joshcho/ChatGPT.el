# chatgpt.py

from epc.server import EPCServer
from lwe.backends.api.backend import ApiBackend
from lwe.core.config import Config

server = EPCServer(("localhost", 0))
bot = None


@server.register_function
def query(query, backend="api", model="default"):
    global bot
    if bot is None:
        config = Config()
        if backend == "api":
            config.set("backend", backend)
            config.set("chat.model", model)
            bot = ApiBackend(config)
        elif backend == "browser":
            return "The 'browser' backend is deprecated. Please switch to 'api'."
        else:
            return (
                f"ChatGPT.el: Unknown chatgpt-wrapper backend: '{backend}'. "
                f"Options are: 'api'."
            )

    response = bot.ask(query)
    try:
        success, response, message = response
    except ValueError:
        pass

    return response


server.print_port()
server.serve_forever()
