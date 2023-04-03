# CCO Dotfiles

This is a repository containing various config files I use. This repo uses `stow` to symlink all files to their correct location and makes management a bit easier

## Using

### Adding untracked files

1. Copy config files to correct path inside `dotfiles`
	- `.config/nvim/init.vim` -> `dotfiles/nvim/.config/nvim/init.vim`
	- `.tmux.conf` -> `dotfiles/tmux/.tmux.conf`
2. Remove file from original location
	- In this case I prefer to copy first instead of move just in case I mess up an operand

### Downloading and Installing Config

1. Clone repo: `git clone https://github.com/ccoverstreet/dotfiles`
	- Can also bare clone if necessary
2. Navigate into repository: `cd dotfiles`
3. Install various packages using `stow`
	- Use `stow directoryname`
