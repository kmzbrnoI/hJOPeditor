import sys
import configparser


class CasePreserveConfigParser(configparser.ConfigParser):
    def optionxform(self, optionstr):
        return optionstr


if __name__ == '__main__':
    if len(sys.argv) < 4:
        sys.stderr.write(f'Usage: {sys.argv[0]} oldfile.opnl freshfile.opnl outfile.opnl\n')
        sys.exit(1)

    oldfn, freshfn, outfn = sys.argv[1:4]

    oldconfig = CasePreserveConfigParser()
    oldconfig.read(oldfn, encoding='utf-8-sig')

    freshconfig = CasePreserveConfigParser()
    freshconfig.read(freshfn, encoding='utf-8-sig')

    with open(outfn, 'w', encoding='utf-8-sig') as outfile:
        freshconfig.write(outfile, space_around_delimiters=False)
