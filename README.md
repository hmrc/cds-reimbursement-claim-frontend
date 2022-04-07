# cds-reimbursement-claim-frontend

This is a placeholder README.md for a new repository

### License

This code is open source software licensed under the [Apache 2.0 License]("http://www.apache.org/licenses/LICENSE-2.0.html").

### Generating Welsh translations:
In sbt you can issue the `welshExport` command.

This will generate 2 new files:
- a new NEW_WELSH_messages.cy file (from the english version + git history)
- a CdsReimbursementNewOrChanged.csv file to send to the translation team with all the missing translations

When issuing the command, the original messages.cy is only used to get the keys out.
The generated NEW_WELSH_messages.cy file will have nothing to do with the original
messages.cy file, it is freshly generated from the english file!

When you have new Welsh translations, just paste them into the messages.cy file,
and then execute the `welshExport` command. The generated NEW_WELSH_messages.cy will
have the same order -line by line- as the english file, but this time containing
all your changes moved into the middle of the file!

