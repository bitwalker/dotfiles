" Autoload
call pathogen#infect()

"=================
" GENERAL
"=================

set shell=/bin/bash            " self-explanatory
set nu                         " line numbers
syntax on                      " enable syntax highlighting
filetype plugin indent on      " enable filetype plugin
set autoread                   " reload files on change
set ignorecase                 " case-insensitive searching
set smartcase
autocmd! bufwritepost vimrc source ~/.vimrc " reload vimrc when edited

"==================
" Keybindings
"==================

let mapleader=","              " a map leader enables extra key combos
let g:mapleader=","            " ex: <leader>w saves current file
set pastetoggle=<F2>           " Turn on paste mode to prevent VIM's shitty auto 
                               " formatting of pasted text

" No need to use shift to access normal mode commands
nnoremap ; :
" fast save
nmap <leader>w :w!<cr>         
" fast .vimrc editing
map <leader>e :e! ~/.vimrc<cr> 
"fix VIM's borked regex handling
nnoremap / /\v
vnoremap / /\v
" Forgot sudo? Use w!!
cmap w!! w !sudo tee % >/dev/null 
" Yank text to the OS X clipboard
noremap <leader>y "*y          
noremap <leader>yy "*Y
" Command-T keybindings
noremap <leader>j :CommandT<cr>     
noremap <leader>y :CommandTFlush<cr>
" Remove ^M when encoding gets jacked
noremap <leader>m mmHmt:%s/<C-V><cr>//ge<cr>'tzt'm
" Preserve indentation while pasting text from the OS X clipboard
noremap <leader>p :set paste<CR>:put  *<CR>:set nopaste<CR>
" Open new vertical split and switch to it
nnoremap <leader>w <C-w>v<C-w>l
" Make window split navigation more intuitive
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l


"=================
" Movement
"=================
nnoremap <tab> %               " use tab to match bracket pairs
vnoremap <tab> %

"=================
" UI
"=================

set backspace=eol,start,indent " set backspace configuration
set whichwrap+=<,>,h,l         " Backspace and cursor keys wrap
set hlsearch                   " highlight search results
set incsearch                  " search incrementally
set nolazyredraw               " do not redraw while executing macros
set magic                      " set magic on for regexes
set mat=2                      " how often the cursor blinks
set cursorline                 " show current cursor location
set relativenumber             " line numbers show relative distance from current line


set noerrorbells               " no audible error bell
set novisualbell               " or visual bell for that matter
set t_vb=
set tm=500

"=================
" Statusline
"=================

set statusline==%F%m%r%h%w\ [FORMAT=%{&ff}]\ [TYPE=%Y]\ [ASCII=\%03.3b]\ [HEX=\%02.2B]\ [POS=%04l,%04v][%p%%]\ [LEN=%L]
set laststatus=2

"=================
" Theme
"=================

"set gfn=Monospace\ 10
set guifont=Liberation\ Mono\ for\ Powerline\ 10
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

set nobackup         " turn backup off
set nowb
set noswapfile       " no swap files either

"=================
" Text
"=================

set tabstop=4        " a tab is 4 spaces
set shiftwidth=4     " number of spaces to use for autoindenting
set softtabstop=4    " let backspace delete the indent
set expandtab
set shiftround       " use multiple of shiftwidth when indenting using < or >
set showmatch        " show matching parens
set smarttab
set nowrap           " don't wrap lines

set lbr
set tw=500

set ai               " auto indent
set si               " smart indent
set wrap             " wrap lines


set encoding=utf-8   " use utf-8 by default

"=================
" Command-T
"=================

let g:CommandTMaxHeight=15
set ttimeoutlen=50                   " Fix keybindings for command-t
set wildignore+=*.o,*.obj,.git,*.pyc " File types to ignore

if !has('gui_running')
  let g:CommandTCancelMap     = ['<ESC>', '<C-c>']
  let g:CommandTSelectNextMap = ['<C-n>', '<C-j>', '<ESC>OB']
  let g:CommandTSelectPrevMap = ['<C-p>', '<C-k>', '<ESC>OA']
endif

"=================
" Assorted
"=================

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
" NOTE: Ensure Nailgun has been installed and is in $PATH
"==================

let vimclojure#WantNailgun = 0   " Toggle Nailgun on/off

"==================
" Powerline
"==================
set rtp+=~/.vim/bundle/powerline/powerline/bindings/vim
