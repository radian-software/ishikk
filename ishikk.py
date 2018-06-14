#!/usr/bin/env python3

import dateutil.parser
import icalendar
import os
import sys

def usage():
    return """\
usage: python3 -m ishikk <subcommand>

subcommands:
    read [--from <from-date>] [--to <to-date>] <vdir>\
"""

def die_with_usage():
    print(usage(), file=sys.stderr)
    sys.exit(1)

def die(message):
    print("ishikk: " + message, file=sys.stderr)
    sys.exit(1)

def read_vdir(vdir, from_date=None, to_date=None):
    try:
        items = [os.path.join(vdir, name) for name in os.listdir(vdir)]
    except OSError as e:
        die("could not list directory {}: {}".format(repr(vdir), str(e)))
    events = []
    for item in items:
        try:
            with open(item) as f:
                cal = icalendar.Calendar.from_ical(f.read())
        except OSError as e:
            die("could not read file {}: {}".format(repr(item), str(e)))
        except ValueError as e:
            die("malformed calendar file {}: {}".format(repr(item), str(e)))

def main(args):
    if not args:
        die_with_usage()
    if args[0] in ("-h", "-?", "-help", "--help", "help"):
        print(usage())
        sys.exit(0)
    if args[0] in ("-v", "-V", "-version", "--version", "version"):
        print("ishikk development version")
        sys.exit(0)
    if args[0] != "read":
        die_with_usage()
    args = args[1:]
    from_date = None
    to_date = None
    vdir = None
    while args:
        if args[0] == "--from":
            if not args[1:]:
                die_with_usage()
            try:
                from_date = dateutil.parser.parse(args[1])
            except ValueError:
                die("malformed date: " + args[1])
            args = args[2:]
            continue
        if args[0] == "--to":
            if not args[1:]:
                die_with_usage()
            try:
                to_date = dateutil.parser.parse(args[1])
            except ValueError:
                die("malformed date: " + args[1])
            args = args[2:]
            continue
        if vdir is None:
            vdir = args[0]
            args = args[1:]
            continue
        die_with_usage()
    read_vdir(vdir, from_date=from_date, to_date=to_date)

if __name__ == "__main__":
    main(sys.argv[1:])
