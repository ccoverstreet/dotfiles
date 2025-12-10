#!/usr/bin/env python
import sys
import subprocess
import urllib.parse as urlparse
import re

def main():
    if len(sys.argv) == 1:
        print("google.com")
        return
    else:
        # Check if there is only 1 argument and
        # if it has the structure <stuff>.<stuff>
        if len(sys.argv[1:]) == 1 and re.match(r".*\..*", sys.argv[1]):
            p = subprocess.Popen(["firefox", sys.argv[1]],
                                 stdout=subprocess.DEVNULL,
                                 stderr=subprocess.DEVNULL)
            return



        term = " ".join(sys.argv[1:])
        term_urlencoded = urlparse.quote_plus(term)
        p = subprocess.Popen([
            "firefox", f"https://duckduckgo.com/search?q={term_urlencoded}"
        ], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)

    return 0


if __name__ == "__main__":
    main()
