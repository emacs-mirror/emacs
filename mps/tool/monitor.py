

import argparse
import struct
import sys

import mpsevent


def run(file):
    _read = file.read
    _unpack = struct.unpack
    _any_desc = mpsevent.EventAnyDesc
    _any_size = mpsevent.EVENT_ANY_SIZE
    _any_format = mpsevent.EVENT_ANY_FORMAT
    _event = mpsevent.EVENT
    while True:
        header_data = _read(_any_size)
        if not header_data:
            break
        header = _any_desc(*_unpack(_any_format, header_data))
        event_desc = _event[header.code]
        size = header.size - _any_size
        assert size <= event_desc.maxsize
        event_data = _read(size)
        print(event_desc.name, event_desc.format, len(event_data))
        event = _unpack(event_desc.format, event_data)


def main():
    parser = argparse.ArgumentParser(description="Memory Pool System Monitor.")
    parser.add_argument('telemetry', metavar='FILENAME', nargs='?',
                        type=argparse.FileType('rb'), default=sys.stdin,
                        help="telemetry output from the MPS instance")
    args = parser.parse_args()
    run(args.telemetry)

if __name__ == '__main__':
    main()
