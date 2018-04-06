A script parser and interpreter web crawler library.

Parses a string of commands and crawls the web according to the script using an instance of chrome/chromium browser.

Requires for chrome/chromium to be installed.

Script syntax: actions seperated by a `\`

"navigateTo http://example.com\in div having id main\..."

Actions:

navigateToDownload <url>    - Navigates to the url and accepts any download. Useful when the url points to a page that initiates a download with JS.
navigateTo <url>            - Navigates to the <url> and selects the document root (puts it in the element stack)
in <element>                - Selects an element to perform following actions on (puts it in the element stack)
typeIn <text>               - Puts in the <text> string into the selected element (stack top)
click                       - Sends a click command to the selected element (stack top)
clickDownload               - Sends a click command to the selected element and awaits a download
onCurrentPage               - Selects document root (clears all stack elements down to document root element)
up                          - Selects a previously selected element (discards stack top)
forAllElems <element>       - Selects multiple elements matching the <element> description for following action
    (the implementation simply unfolds the current script into multiple scripts replacing the current forAllElems action with `in` action)
findContainingInLastResult <text> - Matches any leaf dom element that contains the <text> substring and clicks on them awaiting for a download.

Elements:

Can be any of the following followed by an optional discriminator:
form, input, anchor ("<a>"), div, span, td, tr, label, paragraph ("<p>"), anyElement
examples:
"anchor", "input having id user", "anyElement having id accept-terms"

special cases:
    anyElement - matches any element, best use when an element type is unknownt but can be identified by an attribute.
    customSelector <value> - <value> can be any CSS selector.

Discriminator: Since a discriminator is optional, it starts with a word "having" followed by a discriminator itself.

discriminators: id, name, title, value, containsText
special case: containsText searches for a substring contained in the whole element, including attributes.