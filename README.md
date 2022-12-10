# ChatGPT.el

ChatGPT.el is an Emacs package that provides integration with the [ChatGPT](https://chat.openai.com/chat) model. With ChatGPT.el, you can easily query ChatGPT and receive responses within Emacs, allowing you to quickly get answers to your questions and suggestions for improving your code.

## Quickstart

1.  Install the ChatGPT.el package using straight.el or quelpa. For more information, see the [Installation](#org8eb7662) section.
2. Set the `chatgpt-repo-path` variable to the path of your ChatGPT repository. If you installed the ChatGPT.el package using straight.el, you can set the variable in your `.emacs` file as follows:

       (setq chatgpt-repo-path "/path/to/ChatGPT/repository")

If you installed the ChatGPT.el package using quelpa, you can set the variable in your `.emacs` file as follows:

       (setq chatgpt-repository-path (expand-file-name "ChatGPT" quelpa-build-dir))

Make sure to replace `/path/to/ChatGPT/repository` with the actual path of your ChatGPT repository on your system.
3.  Install the revChatGPT dependency using pip. Run the following command:

        pip3 install revChatGPT --upgrade
4.  Initialize the config.json file in your **home directory** to authenticate API access. For more information, see the [documentation for revChatGPT](https://github.com/acheong08/ChatGPT/wiki/Setup).
5.  Add a keybinding for the chatgpt-query function in your `.emacs` file. For example:

        (global-set-key (kbd "C-c q") 'chatgpt-query)
6.  Start Emacs and query ChatGPT by pressing `C-c q` and entering your query in the minibuffer.
7.  (Optional) Select a region of text and press `C-c q` to query ChatGPT with the selected text.


<a id="org8eb7662"></a>

## Installation


### straight.el

To install the ChatGPT.el package using straight.el, first make sure you have installed straight.el by following the instructions on the [official repository](https://github.com/raxod502/straight.el).

Once you have installed straight.el, use the `straight-use-package` function to clone and install the package.

    (straight-use-package
     '(chatgpt
       :type git
       :host github
       :repo "joshcho/ChatGPT.el"))

This will clone the repository and use straight.el to manage the package.


### quelpa

To install the ChatGPT.el package using quelpa, follow these steps:

1.  Install [use-package](https://github.com/jwiegley/use-package).
2.  Install quelpa by adding the following code to your `.emacs` configuration file and restarting Emacs:

    (use-package quelpa
      :ensure t
      :config
      (quelpa
       '(quelpa-use-package
         :fetcher git
         :url "https://github.com/quelpa/quelpa-use-package.git")))

    (require 'quelpa-use-package)

1.  Create a recipe for the ChatGPT.el package by adding the following code to your `.emacs` file:

    (quelpa-defrecipe chatgpt
      :fetcher github
      :repo "joshcho/ChatGPT.el"
      :files ("*.el"))

1.  Use the `quelpa` function to install the ChatGPT.el package using the recipe you created. You can do this by adding the following code to your `.emacs` file and restarting Emacs:

    (quelpa '(chatgpt :fetcher github :repo "joshcho/ChatGPT.el"))

After running this code, quelpa will download the ChatGPT.el package from GitHub and install it in your `.emacs.d` directory. You can then use the package by adding the appropriate `use-package` declaration to your `.emacs` file, for example:

    (use-package chat-gpt
      :bind
      ("C-c q" . chatgpt-query))


## Query Region

The `chatgpt-query` function allows users to easily query ChatGPT with the contents of an active region in their buffer. When called, the function prompts the user to select a query type from a customizable list of options. The selected query type determines the prompt that is generated and sent to ChatGPT. This feature makes it easy to quickly and conveniently interact with ChatGPT from within the buffer, improving the development workflow.

To make the most of this feature, we recommend using the expand-region.el package. This package provides a convenient way to quickly select the desired region of text for use with the `chatgpt-query` function. To install expand-region.el, follow the instructions on the official repository: <https://github.com/magnars/expand-region.el>.

Once installed, you can bind the `expand-region` command to a keybinding in your `.emacs` file, such as:

    (global-set-key (kbd "C-=") 'er/expand-region)

With this keybinding in place, you can simply press `C-=` to expand the selected region and easily select the text you want to query with ChatGPT. This can greatly improve your development workflow and make using the chatgpt-query function more efficient and enjoyable.


## Customization


### Prompt Customization

Query types and prompts can be customized by setting the `chatgpt-query-types` variable. For example:

    (setq chatgpt-query-types '(
                                ;; ChatGPT.el defaults
                                ("doc" . "Please write the documentation for the following function.\n\n%s")
                                ("bug" . "There is a bug in the following function, please help me fix it.\n\n%s")
                                ("understand" . "What does the following function do?\n\n%s")
                                ("improve" . "Please improve the following code.\n\n%s")
                                ;; your new prompt
                                ("my-custom-type" . "My custom prompt.\n\n%s")))

This allows users to define their own query types and corresponding prompts. Note that you shouldn't use "custom" as your custom type, as that is bound to buildng the prompt interactively.


### Display Customization


## Updating the Package

To update the package installed through straight, use the `straight-pull-package` function.

    (straight-pull-package 'chatgpt)


## Future Roadmap

-   Simplify install
-   Authenticate using browser
-   Add more query types
-   Add structure to \*ChatGPT\* buffer.

For more detailed usage instructions, please refer to the documentation included with `ChatGPT.el` package. Thank you for using `ChatGPT.el`!
