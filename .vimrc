" Autoload
call pathogen#infect()

"=================
" GENERAL
"=================

set nu                      " line numbers
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

" statusline
:set statusline==%F%m%r%h%w\ [FORMAT=%{&ff}]\ [TYPE=%Y]\ [ASCII=\%03.3b]\ [HEX=\%02.2B]\ [POS=%04l,%04v][%p%%]\ [LEN=%L]
:set laststatus=2

"=================
" Theme
"=================

" OS-specific fonts
set gfn=Monospace\ 10
let g:Powerline_symbols = 'fancy'
set shell=/bin/bash

if has("gui_running")
	set guioptions-=T
	set background=light
	colorscheme solarized
else
	set background=dark
  let g:solarized_termcolors=256
	colorscheme solarized
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

" Yank text to the OS X clipboard
noremap <leader>y "*y
noremap <leader>yy "*Y

" Preserve indentation while pasting text from the OS X clipboard
noremap <leader>p :set paste<CR>:put  *<CR>:set nopaste<CR>

" Fix keybindings for command-t
set ttimeoutlen=50

if !has('gui_running')
  let g:CommandTCancelMap     = ['<ESC>', '<C-c>']
  let g:CommandTSelectNextMap = ['<C-n>', '<C-j>', '<ESC>OB']
  let g:CommandTSelectPrevMap = ['<C-p>', '<C-k>', '<ESC>OA']
endif

" Fix mousing in terminal
if has('mouse')
  set mouse=a
  if !has('gui_running')
    " for some reason, doing this directly with 'set ttymouse=xterm2'
    " doesn't work -- 'set ttymouse?' returns xterm2 but the mouse
    " makes tmux enter copy mode instead of selecting or scrolling
    " inside Vim -- but luckily, setting it up from within autocmds
    " works                   
    autocmd VimEnter * set ttymouse=xterm2
    autocmd FocusGained * set ttymouse=xterm2
    autocmd BufEnter * set ttymouse=xterm2
  endif
endif

"==================
" Rainbow Parens
"==================
let g:rbpt_colorpairs = [
    \ ['brown',       'RoyalBlue3'],
    \ ['Darkblue',    'SeaGreen3'],
    \ ['darkgray',    'DarkOrchid3'],
    \ ['darkgreen',   'firebrick3'],
    \ ['darkcyan',    'RoyalBlue3'],
    \ ['darkred',     'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['brown',       'firebrick3'],
    \ ['gray',        'RoyalBlue3'],
    \ ['black',       'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['Darkblue',    'firebrick3'],
    \ ['darkgreen',   'RoyalBlue3'],
    \ ['darkcyan',    'SeaGreen3'],
    \ ['darkred',     'DarkOrchid3'],
    \ ['red',         'firebrick3'],
    \ ]
let g:rbpt_max = 16
let g:rbpt_loadcmd_toggle = 0
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

"==================
" VimClojure
"==================
" Ensure the Nailgun has been installed and is in $PATH
" Toggle Nailgun on/off
let vimclojure#WantNailgun = 0

