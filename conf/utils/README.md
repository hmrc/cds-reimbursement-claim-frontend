## Create Welsh messages file

### create_welsh_messages.py

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

converts them into a file containing **key** and **Welsh translation** like that
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
python create_welsh_messages.py test.csv
```
where `test.csv` is the input CSV file name including path.   
 
The outcome file name is `messages.cy`. If you wish to change file name you can specify it is as the second argument of the script. 
```
python create_welsh_messages.py test.csv my_outcome_file_name.txt
```

The script were developed on Python 3.
