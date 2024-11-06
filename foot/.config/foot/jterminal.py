#!/usr/bin/env python
import subprocess
from dataclasses import dataclass
import os

@dataclass
class TmuxSession:
    name: str
    n_windows: int
    created: str
    is_attached: bool


def main():
    try:
        sessions = get_tmux_sessions()
    except:
        os.execvp("tmux", ["new"])
        return
        sessions = []


    for i, s in enumerate(sessions):
        print(f"{('(' + str(i) + ')'):5} |{s.name}| : with {s.n_windows} windows created {s.created} {"(attached)" if s.is_attached else "(not attached)"}")

    while True:

        try:
            choice = input("Select a session number or press 'n' for new session: ")
        except:
            print()
            return

        if choice == "n":
            os.execvp("tmux", ["new"])
        try:
            val = int(choice)
            os.execlp("tmux", "tmux", "attach-session", r"-t", (sessions[val].name))
        except:
            print("Must enter an integer or 'n': ")
    


def get_tmux_sessions():
    out = subprocess.run(["tmux", "ls"], stdout=subprocess.PIPE)
    if out.returncode != 0:
        raise Exception("Failed to retrieve tmux sessions")

    raw_out = out.stdout.decode("utf8").split("\n")

    sessions = []
    for line in raw_out:
        if len(line) < 2: continue
        split = line.split(":")
        name = split[0]

        split_2 = split[1].split()
        n_windows = int(split_2[0])
        is_attached = split_2[-1] == "(attached)"
        created = " ".join(split_2[2:-1])

        session = TmuxSession(name, n_windows, created, is_attached)
        sessions.append(session)

    return sessions


if __name__ == "__main__":
    main()
