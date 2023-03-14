import sys
from configparser import ConfigParser
from typing import Dict, List, Callable


class CasePreserveConfigParser(ConfigParser):
    def optionxform(self, optionstr):
        return optionstr


def _migrate(tosection, fromsection, items: List[str]):
    migrated: List[str] = []
    for item in items:
        if item in fromsection and ((item in tosection and fromsection[item] != tosection[item]) or item not in tosection):
            tosection[item] = fromsection[item]
            migrated.append(item)
    if migrated:
        print(f'Migrated {fromsection.name} to {tosection.name}:', str(', '.join(migrated)))


def _sdict(config: ConfigParser, block_str: str, secthashgetter: Callable) -> Dict[str, str]:
    result: Dict[str, str] = {}
    count = int(config['P'][block_str])
    for i in range(count):
        sect = config[f'{block_str}{i}']
        result[secthashgetter(sect)] = sect.name
    return result


def migrate(old: ConfigParser, fresh: ConfigParser, block_str: str, hasher: Callable, to_migrate: List[str]) -> None:
    if block_str not in old['P']:
        return

    freshS = _sdict(fresh, block_str, hasher)

    old_count = int(old['P'][block_str])
    for i in range(old_count):
        old_section = old[block_str + str(i)]
        if hasher(old_section) in freshS.keys():
            fresh_sect_name = freshS[hasher(old_section)]
            _migrate(fresh[fresh_sect_name], old_section, to_migrate)


if __name__ == '__main__':
    if len(sys.argv) < 4:
        sys.stderr.write(f'Usage: {sys.argv[0]} oldfile.opnl freshfile.opnl outfile.opnl\n')
        sys.exit(1)

    oldfn, freshfn, outfn = sys.argv[1:4]

    oldconfig = CasePreserveConfigParser()
    oldconfig.read(oldfn, encoding='utf-8-sig')

    freshconfig = CasePreserveConfigParser()
    freshconfig.read(freshfn, encoding='utf-8-sig')

    migrate(oldconfig, freshconfig, 'U', lambda sect: sect['S'], ['B', 'OR', 'R', 'N'])
    migrate(oldconfig, freshconfig, 'V', lambda sect: sect['X']+';'+sect['Y'], ['B', 'OR', 'P'])
    migrate(oldconfig, freshconfig, 'N', lambda sect: sect['X']+';'+sect['Y'], ['B', 'OR'])
    migrate(oldconfig, freshconfig, 'Uv', lambda sect: sect['X']+';'+sect['Y'], ['B', 'OR', 'D'])
    migrate(oldconfig, freshconfig, 'UvS', lambda sect: sect['X']+';'+sect['Y'], ['B', 'OR', 'VD', 'C'])
    migrate(oldconfig, freshconfig, 'Vyk', lambda sect: sect['X']+';'+sect['Y'], ['B', 'OR', 'T'])
    migrate(oldconfig, freshconfig, 'R', lambda sect: sect['X']+';'+sect['Y'], ['B', 'OR'])
    migrate(oldconfig, freshconfig, 'PSt', lambda sect: sect['X']+';'+sect['Y'], ['B', 'OR'])
    migrate(oldconfig, freshconfig, 'PRJ', lambda sect: sect['BP']+';'+sect['SP'], ['B', 'OR'])

    with open(outfn, 'w', encoding='utf-8-sig') as outfile:
        freshconfig.write(outfile, space_around_delimiters=False)
