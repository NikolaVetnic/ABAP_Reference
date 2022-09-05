## Program Lifecycle

Lifecycle events:

-   `LOAD-OF-PROGRAM` - triggered once before the program is loaded into memory
-   `INITIALIZATION` - called before any selection screens are processed
-   `AT SELECTION-SCREEN` - can be used for enforcing validations for example
-   `START-OF-SELECTION` - triggered after the user has pressed the `Execute` button (optional to use if no other events are defined, because it’s the default event that will begin executing your code)
