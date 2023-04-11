# chatgpt.py

from epc.server import EPCServer
# Hedge against breaking changes in chatgpt-wrapper >= 0.5.0
try:
    from chatgpt_wrapper.config import Config
    import chatgpt_wrapper.constants as constants
except ImportError:
    from chatgpt_wrapper.core.config import Config
    import chatgpt_wrapper.core.constants as constants

server = EPCServer(('localhost', 0))
bot = None

@server.register_function
def query(query, backend="chatgpt-browser", model="default"):
    global bot
    if bot is None:
        config = Config()
        chatgpt_wrapper_browser_models = list(constants.RENDER_MODELS.keys())
        chatgpt_wrapper_api_models = list(
            constants.OPENAPI_CHAT_RENDER_MODELS.keys())
        if backend == "chatgpt-browser":
            config.set("backend", backend)
            if model not in chatgpt_wrapper_browser_models:
                return (f"ChatGPT.el: Unknown chatgpt-wrapper model '{model}' "
                   f"for '{backend}' backend. "
                   f"Options are: {chatgpt_wrapper_browser_models}.")
            config.set("chat.model", model)
            # Hedge against breaking changes in chatgpt-wrapper >= 0.7.0
            try:
                from chatgpt_wrapper import ChatGPT
            except ImportError:
                from chatgpt_wrapper.backends.browser.chatgpt import ChatGPT
            bot = ChatGPT(config)
            bot.launch_browser()
        elif backend == "chatgpt-api":
            config.set("backend", backend)
            if model not in chatgpt_wrapper_api_models:
                return (f"ChatGPT.el: Unknown chatgpt-wrapper model '{model}' "
                   f"for '{backend}' backend. "
                   f"Options are: {chatgpt_wrapper_api_models}.")
            config.set("chat.model", model)
            # NOTE: This will not retain conversation history in its current
            # form.
            # See: https://github.com/mmabrouk/chatgpt-wrapper/issues/285
            from chatgpt_wrapper import OpenAIAPI
            bot = OpenAIAPI(config)
        else:
            return (f"ChatGPT.el: Unknown chatgpt-wrapper backend: '{backend}'. "
               f"Options are: 'chatgpt-browser', 'chatgpt-api'.")

    response = bot.ask(query)
    # Hedge against more breaking changes in chatgpt-wrapper >= 0.5.0
    try:
        success, response, message = response
    except ValueError:
        pass

    return response

server.print_port()
server.serve_forever()
