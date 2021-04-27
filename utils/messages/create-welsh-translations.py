import csv
import sys


def write_welsh_translations(csv_file_name, output_file_name):
    with open(csv_file_name, newline='') as csv_file:
        messages = csv.reader(csv_file, delimiter=',')
        output_file = open(output_file_name, 'w+')
        # skip headers
        for i in range(2):
            next(messages, None)
        # write translations
        try:
            invalid = []
            for message in messages:
                key = message[0].strip()
                welsh = message[2].strip()
                if not key:
                    output_file.write('\n')
                elif key.startswith('#'):
                    if len(key) == 1:
                        output_file.write(key + '===================================================\n')
                    else:
                        output_file.write(key + '\n')
                elif len(welsh) > 0:
                    output_file.write('{}={}\n'.format(key, welsh))
                else:
                    invalid.append(message)
            print('Finished')
            print('Invalid records: ', len(invalid))
            for x in invalid:
                print('* {}'.format(x))
        except IOError:
            print("Error writing translations")
        output_file.close()


if __name__ == '__main__':
    output_file_name = "messages.cy"
    if len(sys.argv) < 2:
        print('Error: please provide the source CSV file name including fullpath in command line arguments')
        print('Usage: create_welsh_messages.py [CSV file name] [output file name]\n')
        print('Note: output file name is optional, default name is "{}"'.format(output_file_name))
    else:
        write_welsh_translations(sys.argv[1], sys.argv[2] if len(sys.argv) == 3 else output_file_name)
