

import argparse
from struct import Struct
import sys

from mpsevent import *


def run(file):
    # Cache frequently-used values in local variables.
    read = file.read
    header_desc = HeaderDesc
    header_size = HEADER_SIZE
    event_dict = EVENT

    # Special handling for Intern.
    Intern_code = Event.Intern.code
    Intern_struct = Struct(Event.Intern.format)
    Intern_size = Intern_struct.size
    Intern_unpack = Intern_struct.unpack

    # Build unpacker functions.
    header_struct = Struct(HEADER_FORMAT)
    assert header_struct.size == header_size
    header_unpack = header_struct.unpack
    event_unpack = {}
    for code, desc in event_dict.items():
        s = Struct(desc.format)
        assert code == Intern_code or s.size == desc.maxsize
        event_unpack[code] = s.unpack

    while True:
        header_data = read(header_size)
        if not header_data:
            break
        header = header_desc(*header_unpack(header_data))
        code = header.code
        event_desc = event_dict[code]
        size = header.size - header_size
        if code == Intern_code:
            assert size <= event_desc.maxsize
            e = (Intern_unpack(read(Intern_size))
                 + (read(size - Intern_size).rstrip(b'\0'),)
        else:
            assert size == event_desc.maxsize
            e = event_unpack[code](read(size))
        print(event_desc.name, *header, event_desc.format, *e)


def main():
    parser = argparse.ArgumentParser(description="Memory Pool System Monitor.")
    parser.add_argument('telemetry', metavar='FILENAME', nargs='?',
                        type=argparse.FileType('rb'), default=sys.stdin,
                        help="telemetry output from the MPS instance")
    args = parser.parse_args()
    run(args.telemetry)

if __name__ == '__main__':
    main()
