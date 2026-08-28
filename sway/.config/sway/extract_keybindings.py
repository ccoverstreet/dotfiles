

with open("config") as f:
    for line in f:
        if "bindsym" not in line: continue

        stripped = line.replace("bindsym", "").strip()
        print(stripped)
