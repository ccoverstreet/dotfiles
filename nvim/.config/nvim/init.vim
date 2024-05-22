:command Dg1 diffget 1
:command Dg2 diffget 2

:set relativenumber

:noremap <C-k> :bnext<cr>
:noremap <C-j> :bprevious<cr>
:noremap <C-t> :tabnew<cr>
:noremap <C-l> :tabnext<cr>
:noremap <C-h> :tabprevious<cr>
:noremap \b cw\begin{<C-R>"}<CR>\end{<C-R>"}


:set clipboard+=unnamedplus

call plug#begin("~/.nvim")
Plug 'jiangmiao/auto-pairs'
Plug 'pangloss/vim-javascript'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'rust-lang/rust.vim'
Plug 'tpope/vim-fugitive'
Plug 'ziglang/zig.vim'
Plug 'begriffs/haskell-vim-now'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'jschmold/sweet-dark.vim'
Plug 'EdenEast/nightfox.nvim'
Plug 'catppuccin/nvim', { 'as': 'catppuccin'}
Plug 'sheerun/vim-polyglot'
Plug 'vim-pandoc/vim-pandoc-syntax'
Plug 'g2boojum/vim-mcnp'
Plug 'neovimhaskell/haskell-vim'
Plug 'othree/html5.vim'
Plug 'evanleck/vim-svelte', {'branch': 'main'}
Plug 'JuliaEditorSupport/julia-vim'
Plug 'rebelot/kanagawa.nvim'
Plug 'lukas-reineke/indent-blankline.nvim'
Plug 'jpalardy/vim-slime', { 'for': 'python' }
Plug 'hanschen/vim-ipython-cell', { 'for': 'python' }
Plug 'slint-ui/vim-slint'
call plug#end()

let g:indent_blankline_char = '|'

"set termguicolors
colorscheme kanagawa
set foldmethod=syntax
hi Normal ctermbg=NONE guibg=NONE
hi LineNr ctermbg=NONE guibg=NONE

set fillchars=|
" hi VertSplit ctermfg=15 ctermbg=15 guifg=15 guibg=15

" Turn of autopairs enable shortcut
let g:AutoPairsShortcutToggle = ''

" Note taking (Mkd) related code
autocmd BufWritePost *.mkd call MkdWritePost()
function MkdWritePost() 
	cd %:p:h 
	!mkd2pdf %
	cd -
endfunction

let g:slime_target = 'tmux'

" fix paste issues in ipython
let g:slime_python_ipython = 1

" always send text to the top-right pane in the current tmux tab without asking
let g:slime_default_config = {
            \ 'socket_name': get(split($TMUX, ','), 0),
            \ 'target_pane': '{top-right}' }

let g:slime_dont_ask_default = 1


" map <Leader>s to start IPython
nnoremap <Leader>s :SlimeSend1 ipython<CR>

" map <Leader>r to run script
nnoremap <Leader>r :IPythonCellRun<CR>

" map <Leader>R to run script and time the execution
nnoremap <Leader>R :IPythonCellRunTime<CR>

" map <Leader>c to execute the current cell
nnoremap <Leader>c :IPythonCellExecuteCell<CR>

" map <Leader>C to execute the current cell and jump to the next cell
nnoremap <Leader>C :IPythonCellExecuteCellJump<CR>

" map <Leader>l to clear IPython screen
nnoremap <Leader>l :IPythonCellClear<CR>

" map <Leader>x to close all Matplotlib figure windows
nnoremap <Leader>x :IPythonCellClose<CR>

" map [c and ]c to jump to the previous and next cell header
nnoremap [c :IPythonCellPrevCell<CR>
nnoremap ]c :IPythonCellNextCell<CR>


:command MkdView !jopen %:p:h/%:p:t:r.pdf
" End note taking (Mkd) related code

" Golang Setup
au FileType go set noexpandtab
au FileType go set shiftwidth=4
au FileType go set softtabstop=4
au FileType go set tabstop=4

let g:go_fold_enable = ['block', 'import', 'varconst']
let g:go_def_mapping_enabled = 0
let g:go_highlight_build_constraints = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_operators = 1
let g:go_highlight_structs = 1
let g:go_highlight_types = 1


:set noet ci pi sts=0 sw=4 ts=4
filetype plugin on
autocmd FileType c,cpp,java setlocal noet ci pi sts=0 sw=4 ts=4
autocmd BufRead,BufNewFile *.py setlocal et ts=4 sw=4
autocmd BufRead,BufNewFile *.md setfiletype pandoc

let g:python_recommended_style = 1
