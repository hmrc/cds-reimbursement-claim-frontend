## Create Welsh messages file

### Background

The scripts were created to parse inconsistent documents by its format. Going forward this is not the way we wish to deal with translations.  

### create-welsh-translations.py

Given a spreadsheet with columns

| CDRS Messages                    |                          |                                    |
|----------------------------------|:-------------------------|:-----------------------------------|
|Message key                       | Message value            | Welsh Translation of message value |
|#                                 |                          |                                    |
|#  GLOBAL MESSAGES                |                          |                                    |
|#                                 |                          |                                    |
|generic.day                       | Day                      | Diwrnod                            |
|generic.month                     | Month                    | Mis                                |
|generic.year                      | Year                     | Blwyddyn                           |
|generic.yes                       | Yes                      | Iawn                               |

The script converts them into a file containing **key** and **Welsh translation** as follows
```
#===================================================
#  GLOBAL MESSAGES
#===================================================
generic.day=Diwrnod
generic.month=Mis
generic.year=Blwyddyn
generic.yes=Iawn
```
To use a script run command
```
python create-welsh-translations.py test.csv
```
where `test.csv` is the input CSV file name including path.   
 
Default outcome file name is `messages.cy`. If you wish to change it, you can specify it in the command line arguments.
```
python create_welsh_translations.py test.csv my_outcome_file_name.txt
```
Output
```
% » python create-welsh-translations.py translations.csv
Finished
Invalid records:  2
* ['Upload files page', '', '', '', '']
* ['Wait a few seconds and then select ‘continue’', '', '', '', '']
```

### create-welsh-messages-no-keys.py
Given a spreadsheet with columns

| English text                            | English text                         |
|-----------------------------------------|--------------------------------------|
|Claim for reimbursement of import duties | Hawlio ad-daliad am dollau mewnforio |
|en                                       | en                                   | 

The script converts them into a file containing **key** and **Welsh translation** as follows
```
landing.title=Hawlio ad-daliad am dollau mewnforio
lang=en
```
where keys been obtained from the existing message file which is passed in the command line arguments.   

To use a script run
```
python create-welsh-messages-no-keys.py test.csv
```
where `test.csv` is the input CSV file name including path.

Default outcome file name is `messages.cy` and existing English messages file should be within same directory as the script and have a new `messages`.    
If you wish to change it, you can specify names in the command arguments.
```
python create-welsh-messages-no-keys.py test.csv english_messages_file_name my_outcome_file_name.txt
```
 
Output
```
% » python create-welsh-translations-no-keys.py test.csv                                                                                2 ↵
Finished
Could not match:  13
* "{0}">create a new government gateway user id</a>. you can then use it to create a capital gains tax on uk property account.
* "{0}">sign in</a> with the same government gateway user id you used to create your capital gains tax on uk property account.
* enter a valid company name
* enter a valid date
* an importer can be a person or company bringing the goods into the country.
* a representative company can be a person or company acting on behalf of an importer. this could be an agent or a broker or a freight forwarder.
* you cannot enter more than 500 characters.
* "{0}">update the calculation</a>
* please enter a valid account number
* schedule of mrns
* additional supporting documents
* other
* claimant’s details
```

The scripts developed on Python 3.
