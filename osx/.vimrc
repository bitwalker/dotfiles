set nocompatible
filetype off
""""""""""""""""""""""
" Plugin Configuration
""""""""""""""""""""""
call plug#begin('~/.vim/bundle')

" Notes and Examples
" - Make sure you use single quotes
" - On-demand loading:
"     Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
"     Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
" - Using git URL:
"     Plug 'https://github.com/junegunn/vim-github-dashboard.git'
" - Plugin options:
"     Plug 'nsf/gocode', { 'tag': 'go.weekly.2012-03-13', 'rtp': 'vim' }
" - Plugin outside ~/.vim/bundle with post-update hook:
"     Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': 'yes \| ./install' }
" - Unmanaged plugin (manually installed and updated):
"     Plug '~/my-prototype-plugin'

" Color Schemes
" Plug 'blerins/flattown'
" Plug 'zefei/cake16'
" Plug 'duythinht/vim-coffee'
" Plug 'tomasr/molokai'
" Plug 'altercation/vim-colors-solarized'
" Plug 'ajh17/Spacegray.vim'
" Plug 'CruizeMissile/Revolution.vim'
" Plug 'gertjanreynaert/cobalt2-vim-theme'
Plug 'abra/vim-abra'
" General plugins
Plug 'tpope/vim-surround'
Plug 'kien/ctrlp.vim'
Plug 'scrooloose/nerdtree'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'tpope/vim-fugitive'
" UI plugins
Plug 'bling/vim-airline'
" Language support
Plug 'othree/html5.vim'
Plug 'pangloss/vim-javascript'
Plug 'tpope/vim-markdown'


call plug#end()
filetype plugin indent on

"""""""""""""""""""""""
" General Configuration
"""""""""""""""""""""""
syntax enable

" Change shell
set shell=bash " Vim expects a POSIX-compliant shell, which Fish (my default shell) is not

" UI
set background = dark
try
  colorscheme abra
catch
endtry
set guifont=Fantasque\ Sans\ Mono\ Regular:h14
" Set extra options when running in GUI mode
if has("gui_running")
  set guioptions-=T
  set guioptions-=e
  set t_Co=256
  set guitablabel=%M\ %t
endif
set lsp=5
set linespace=0

" In many terminal emulators the mouse works just fine, thus enable it.
if has('mouse')
  set mouse=a
endif
" Switch from block-cursor to vertical-line-cursor when going into/out of
" insert mode
let &t_SI = "\<Esc>]50;CursorShape=1\x7"
let &t_EI = "\<Esc>]50;CursorShape=0\x7"

" highlight current line
au WinLeave * set nocursorline nocursorcolumn
au WinEnter * set cursorline cursorcolumn
set cursorline cursorcolumn

" set leader to ,
let mapleader=","
let g:mapleader=","

" general settings
set history=1000
set undolevels=1000
set nocompatible
set confirm                    " prompt when existing from an unsaved file
set backspace=indent,eol,start " More powerful backspacing
set report=0                   " always report number of lines changed
set nowrap                     " dont wrap lines
set scrolloff=5                " 5 lines above/below cursor when scrolling
set number                     " show line numbers
set showmatch                  " show matching bracket (briefly jump)
set showcmd                    " show typed command in status bar
set title                      " show file in titlebar
set laststatus=2               " use 2 lines for the status bar
set matchtime=2                " show matching bracket for 0.2 seconds
set matchpairs+=<:>            " specially for html
set shortmess+=I               " hide the launch screen
set clipboard=unnamed          " normal OS clipboard interaction
set autoread                   " automatically reload files changed outside of Vim
set ignorecase                 " Ignore case when searching
set smartcase                  " When searching try to be smart about cases
set hlsearch                   " Highlight search results
set incsearch                  " Makes search act like search in modern browsers
set lazyredraw                 " Don't redraw while executing macros (good performance config)
set magic                      " For regular expressions turn magic on

" Turn backup off, since most stuff is in SVN, git et.c anyway...
set nobackup
set nowb
set noswapfile

" wildmenu
set wildmenu                   " make tab completion for files/buffers act like bash
set wildmode=list:full         " show a list when pressing tab and complete
" first full match
set wildignore=*.o,*~,*.pyc,*.swp,*.bak,*.class
if has("win16") || has("win32")
  set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/.DS_Store
else
  set wildignore+=.git\*,.hg\*,.svn\*
endif

" indentation
" Use spaces instead of tabs
set expandtab
" Be smart when using tabs
set smarttab
" 1 tab == 4 spaces
set shiftwidth=4
set tabstop=4
" auto indent
set ai
" smart indent
set si

" disable sound on errors
set noerrorbells
set novisualbell
set t_vb=
set tm=500

" encoding
set encoding=utf-8
set termencoding=utf-8
set fileencoding=utf-8
set fileencodings=ucs-bom,utf-8,big5,gb2312,latin1

" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk

" w!! to sudo & write a file
cmap w!! %!sudo tee >/dev/null %

" Airline
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline_theme = "luna"
let g:airline_enable_branch = 1

" ctrlp
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'ra'
" Ignore common directories
let g:ctrlp_custom_ignore = {
  \ 'dir': 'node_modules\|bower_components',
  \ }

" Close vim if last open buffer is NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" Use The Silver Searcher over grep, iff possible
if executable('ag')
  " Use ag over grep
  set grepprg=ag\ --nogroup\ --nocolor
  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
endif

" Conflict markers {{{
" highlight conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

