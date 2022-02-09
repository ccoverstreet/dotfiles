:set noet ci pi sts=0 sw=4 ts=4
autocmd FileType c,cpp,java,python setlocal noet ci pi sts=0 sw=4 ts=4

:set relativenumber

:noremap <C-k> :bnext<cr>
:noremap <C-j> :bprevious<cr>
:noremap \b cw\begin{<C-R>"}<CR>\end{<C-R>"}

call plug#begin("~/.nvim")
Plug 'jiangmiao/auto-pairs'
Plug 'pangloss/vim-javascript'
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
Plug 'rust-lang/rust.vim'
Plug 'ziglang/zig.vim'
Plug 'begriffs/haskell-vim-now'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'jschmold/sweet-dark.vim'
Plug 'mattn/emmet-vim'
Plug 'catppuccin/nvim', { 'as': 'catppuccin'}
call plug#end()

set termguicolors

colorscheme catppuccin

set foldmethod=syntax

let g:go_fold_enable = ['block', 'import', 'varconst']

" Turn of autopairs enable shortcut
let g:AutoPairsShortcutToggle = ''

" Setting colorscheme
"let g:airline_theme="one"
"colorscheme onedark
"set background=dark

autocmd BufWritePost *.mkd !mkd2pdf % 


" Golang Setup
au FileType go set noexpandtab
au FileType go set shiftwidth=4
au FileType go set softtabstop=4
au FileType go set tabstop=4

let g:go_highlight_build_constraints = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_operators = 1
let g:go_highlight_structs = 1
let g:go_highlight_types = 1

:lua require("setup")
