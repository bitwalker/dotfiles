" Autoload
call pathogen#infect()

"=================
" GENERAL
"=================

set number                  " line numbers
syntax on                   " enable syntax highlighting

filetype plugin indent on   " enable filetype plugin

set autoread                " reload files on change

" a map leader enables extra key combos
" ex: <leader>w saves current file
let mapleader=","
let g:mapleader=","

" fast save
nmap <leader>w :w!<cr>
" fast .vimrc editing
map <leader>e :e! ~/.vimrc<cr>

" reload vimrc when edited
autocmd! bufwritepost vimrc source ~/.vimrc

"=================
" UI
"=================

" set backspace configuration
set backspace=eol,start,indent
set whichwrap+=<,>,h,l

set ignorecase              " case-insensitive searching
set smartcase

set hlsearch                " highlight search results
set incsearch               " search incrementally

set nolazyredraw            " do not redraw while executing macros

set magic                   " set magic on for regexes

set showmatch               " show matching braces
set mat=2                   " how often the cursor blinks

" no error sounds
set noerrorbells
set novisualbell
set t_vb=
set tm=500

"=================
" Theme
"=================

" OS-specific fonts
set gfn=Monospace\ 10
set shell=/bin/zsh

if has("gui_running")
	set guioptions-=T
	set background=dark
	colorscheme solarized
	set nonu
else
	colorscheme solarized
	let g:solarized_termtrans=1 " enable transparent background
	set background=dark
	set nonu
endif


"=================
" Files
"=================

" turn backup off
set nobackup
set nowb
set noswapfile

"=================
" Text
"=================

set expandtab
set shiftwidth=2
set tabstop=2
set smarttab

set lbr
set tw=500

set ai " auto indent
set si " smart indent
set wrap " wrap lines

"=================
" Command-T
"=================

let g:CommandTMaxHeight=15
set wildignore+=*.o,*.obj,.git,*.pyc
noremap <leader>j :CommandT<cr>
noremap <leader>y :CommandTFlush<cr>

"=================
" Assorted
"=================

" Remove ^M when encoding gets jacked
noremap <leader>m mmHmt:%s/<C-V><cr>//ge<cr>'tzt'm

