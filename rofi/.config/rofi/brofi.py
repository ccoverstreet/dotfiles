#!/usr/bin/env python3

import sys
import subprocess
import sys 

BT_COMMAND = "/home/coverstreet/.local/bin/bt"

def main():
    #with open("/home/coverstreet/rofi.txt", "a") as f:
    #    f.write("ASDAS")
    #    f.write(sys.argv)
    #    res = get_tabs()
    #    for r in res:
    #        f.write(r)
    if len(sys.argv) == 1:
        # Initial call from Rofi
        res = get_tabs()
        print(res)
        sys.exit(0)
    else:
        bt_id = convert_arg_to_bt_id(sys.argv[1])
        open_bt_id(bt_id)

def get_tabs():
    raw = subprocess.run([BT_COMMAND, "list"], capture_output=True)
    out = raw.stdout.decode("latin-1")
    return out

def convert_arg_to_bt_id(arg):
    split = arg.split()
    return split[0]

def open_bt_id(bt_id):
    subprocess.run([BT_COMMAND, "activate", "--focused", bt_id])
    subprocess.run([BT_COMMAND, "activate", "--focused", bt_id])


if __name__ == "__main__":
    main()
