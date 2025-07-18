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

    display_tmux_sessions(sessions)

    while True:

        try:
            choice = input("Select a session number, press 'n' for new session, or 't' for temporary\n$ ")
        except:
            print()
            return

        if choice == "n":
            os.execvp("tmux", ["new"])
        elif choice == "t":
            os.execvp("zsh", ["-i"])
        elif choice == "ls":
            sessions = get_tmux_sessions()
            display_tmux_sessions(sessions)

        try:
            val = int(choice)
            os.execlp("tmux", "tmux", "attach-session", r"-t", (sessions[val].name))
        except:
            print("Must enter an integer, 'n', or 't': ")
    


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

def display_tmux_sessions(sessions):
    # find max name length
    max_len = 0
    for s in sessions:
        max_len = len(s.name) if len(s.name) > max_len else max_len

    print("\nAvailable sessions")
    for i, s in enumerate(sessions):
        print(f"{('(' + str(i) + ')'):5} |{(s.name)}{" " * (max_len - len(s.name))}| : with {s.n_windows} windows created {s.created} {"(attached)" if s.is_attached else "(not attached)"}")
    print()



if __name__ == "__main__":
    main()
