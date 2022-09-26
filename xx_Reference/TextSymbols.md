### Text Symbols

Text symbols belong to a specific program and can be translated into different languages. When a statement using a text symbol is executed, the system automatically takes into account the logon language of the user and supplies the text in this language.

A text symbol is identified by means of a **three-character alphanumeric ID**, such as `xxx`. There are two ways to address them:
* `TEXT-xxx`, where `xxx` stands for the three-character text symbol ID
* `'...'(abc)`, where the ellipsis `...` is the original language text, e.g. `WRITE 'Hello'(abc)`

There are two ways to access the screen where you can define a text symbol:
* choose *Goto → Text Elements → Text Symbols*
* define the text symbol ID in the source code, then double-click it; the Editor asks you if you want to create a text symbol, then opens the Text Symbols screen (works in ABAP Editor)

You can maintain translations for text symbols on the *Text Elements* screen. Choose *Goto → Translation* and enter the relevant language and translation for the text symbols.
