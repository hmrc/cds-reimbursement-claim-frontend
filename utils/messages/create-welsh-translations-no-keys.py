import sys
import csv

def write_welsh_translations(doc, messages, output):
    with open(messages, 'r') as msg:
        # read messages to match keys by English text
        records = [line for line in msg.readlines() if not line.strip() == '' and not line.startswith('#')]
        keyValues = {item[1].strip().lower(): item[0] for item in map(lambda record: record.split('='), records)}
        # read csv and create output file
        outcome = open(output, 'w+')
        try:
            with open(doc, newline='') as data:
                translations = csv.reader(data, delimiter=',')
                next(translations, None)
                not_matched = []
                for translation in translations:
                    english_sentence = translation[0].strip().lower() 
                    key = keyValues.get(english_sentence)
                    if key == None:
                        not_matched.append(english_sentence)
                    else:
                        welsh_sentence = translation[1].strip() 
                        outcome.write('{}={}\n'.format(key, welsh_sentence))
                print('Finished')
                print('Could not match: ', len(not_matched))
                for x in not_matched:
                    print('* {}'.format(x))
        except IOError:
            print("Error writing translations")
        outcome.close()

if __name__ == '__main__':
    messages = 'messages'
    output = 'messages.cy'
    if len(sys.argv) < 2:
        print('Error: please provide CSV file name including full path in command line arguments')
        print('Usage: create-welsh-messages-no-keys.py [csv file] [keys file] [output file]\n')
        print('Note: the keys and output files are optional, default name for keys file is "{}" and for output file "{}"'.format(messages, output))
    else:
        write_welsh_translations(
            sys.argv[1], 
            sys.argv[2] if len(sys.argv) > 2 else messages, 
            sys.argv[3] if len(sys.argv) > 3 else output)
