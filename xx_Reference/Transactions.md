## Transactions

Often used transactions:
* `SE01` - Transport Organizer
* `SE11` - ABAP Dictionary (create Database Tables, Data Elements, Domains)
* `SE16` - Data Browser
* `SE18` - BAdI Builder
* `SE24` - Class Builder
* `SE37` - Function Builder (create and analyze function modules)
* `SE38` - ABAP Editor
* `SE51` - Screen Painter
* `SE91` - Message Maintenance
* `ST22` - used for inspecting the short dumps

### Possible Entries in the OK Code Field

Press `F1` in the `Command` field to display the input options for this field. The following entries are possible:
* `/n` to cancel the current transaction
* `/nXXXX` to call transaction XXXX directly from another transaction. Without the prefix, you can only call XXXX from the SAP Easy Access screen
* `/o` to display an overview of sessions
* `/oXXXX` to call transaction `XXXX` in a new session directly from another transaction
* `/nend` to end the logon session with a confirmation dialog box
* `/nex` to end the logon session without a confirmation dialog box
* `/i` to delete the session you are currently using
