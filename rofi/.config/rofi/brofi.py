#!/usr/bin/env python3

import sys
import subprocess

def main():
    if len(sys.argv) == 1:
        # Initial call from Rofi
        res = get_tabs()
        print(res)
        sys.exit(0)
    else:
        bt_id = convert_arg_to_bt_id(sys.argv[1])
        open_bt_id(bt_id)

def get_tabs():
    raw = subprocess.run(["bt", "list"], capture_output=True)
    out = raw.stdout.decode("utf-8")
    return out

def convert_arg_to_bt_id(arg):
    split = arg.split()
    return split[0]

def open_bt_id(bt_id):
    subprocess.run(["bt", "activate", bt_id])


if __name__ == "__main__":
    main()
