if &compatible
  " `:set nocp` has many side effects. Therefore this should be done
  " only when 'compatible' is set.
  set nocompatible
endif
filetype indent plugin on | syn on

set packpath^=~/.config/nvim

function! PackInit() abort
  packadd minpac

  call minpac#init()

  " minpac must have {'type': 'opt'} so that it can be loaded with `packadd`.
  call minpac#add('k-takata/minpac', {'type': 'opt'})

" Color Schemes
" blerins/flattown
" zefei/cake16
" duythinht/vim-coffee
" tomasr/molokai
" altercation/vim-colors-solarized
" ajh17/Spacegray.vim
" CruizeMissile/Revolution.vim
" gertjanreynaert/cobalt2-vim-theme
" drewtempelmeyer/palenight.vim
" arcticicestudio/nord-vim
" daylerees/colour-schemes, {'subdir': 'vim'}
  call minpac#add('ayu-theme/ayu-vim')

" UI
  call minpac#add('vim-airline/vim-airline-themes')
  call minpac#add('bling/vim-airline')
  call minpac#add('tmux-plugins/vim-tmux-focus-events')

" General
  call minpac#add('tpope/vim-surround')
  call minpac#add('tpope/vim-fugitive')
  call minpac#add('kien/ctrlp.vim')
  call minpac#add('scrooloose/nerdtree')
  call minpac#add('editorconfig/editorconfig-vim')

" Langs
  call minpac#add('rust-lang/rust.vim')
  call minpac#add('othree/html5.vim')
  call minpac#add('pangloss/vim-javascript')
  call minpac#add('tpope/vim-markdown')
  call minpac#add('elixir-lang/vim-elixir')
  call minpac#add('jimenezrick/vimerl')
  call minpac#add('dag/vim-fish')
  call minpac#add('keith/swift.vim')
endfunction

packadd! llvm
packadd! mlir

command! PackUpdate call PackInit() | call minpac#update()
command! PackClean  call PackInit() | call minpac#clean()
command! PackStatus call PackInit() | call minpac#status()

"""""""""""""""""""""""
" General Configuration
"""""""""""""""""""""""

" Change shell
set shell=bash " Vim expects a POSIX-compliant shell, which Fish (my default shell) is not

" UI
if (has("nvim"))
  "For Neovim 0.1.3 and 0.1.4 < https://github.com/neovim/neovim/pull/2198 >
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif
"For Neovim > 0.1.5 and Vim > patch 7.4.1799 < https://github.com/vim/vim/commit/61be73bb0f965a895bfb064ea3e55476ac175162 >
"Based on Vim patch 7.4.1770 (`guicolors` option) < https://github.com/vim/vim/commit/8a633e3427b47286869aa4b96f2bfc1fe65b25cd >
" < https://github.com/neovim/neovim/wiki/Following-HEAD#20160511 >
if (has("termguicolors"))
  set termguicolors
endif
try
  let ayucolor="mirage" " light, dark, mirage
  colorscheme ayu
  "colorscheme nord
catch
endtry
hi Search cterm=None ctermfg=0 ctermbg=10
set guifont=Fantasque\ Sans\ Mono\ Regular:h14
" Set extra options when running in GUI mode
if has("gui_running")
  hi Search guibg=#000000 guifg=#232b32
  set guioptions-=T
  set guioptions-=e
  set t_Co=256
  set guitablabel=%M\ %t
endif
set lsp=5
set linespace=0

" fzf for fuzzy finding
set rtp+=/usr/local/opt/fzf

" In many terminal emulators the mouse works just fine, thus enable it.
if has('mouse')
  set mouse=a
endif
" Switch from block-cursor to vertical-line-cursor when going into/out of
" insert mode
let &t_SI = "\<Esc>]50;CursorShape=1\x7"
let &t_EI = "\<Esc>]50;CursorShape=0\x7"

" highlight current line
"au WinLeave * set nocursorline nocursorcolumn
"au WinEnter * set cursorline cursorcolumn
"set cursorline cursorcolumn

" set leader to ,
let mapleader=","
let g:mapleader=","

" general settings
set history=1000
set undolevels=1000
set nocompatible
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
"let g:airline_theme = "wombat"
"let g:airline_theme='luna'
"let g:airline_theme='papercolor'
let g:airline_theme='ayu'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#branch#enabled = 1

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

" rg > ag > grep
if executable('rg')
    set grepprg=rg\ --no-heading\ --color=never
    " Use rg for listing files, it is fast, and respects various ignore files
    let g:ctrlp_user_command = 'rg --color=never --files --sort=path %s'
    " Don't cache, rg is fast enough
    let g:ctrlp_use_caching = 0
elseif executable('ag')
    " same situation as rg, just slightly different invocations
    set grepprg=ag\ --nogroup\ --color=never
    let g:ctrlp_user_command = 'ag -l --nocolor -g ""'
    let g:ctrlp_use_caching = 0
endif

" Conflict markers {{{
" highlight conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

" Shortcut for toggling display of invisibles
nmap <leader>l :set list!<CR>

" Use the same symbols as textmate for tabstops and EOLs
set listchars=nbsp:·,tab:▸\ ,eol:¬,trail:·,extends:>,precedes:<

" Toggle search highlighting
nmap <leader>h :set hlsearch!<CR>

" ack via motion commands, i.e. aiw to ack word under cursor
nnoremap <silent> \a :set opfunc=<SID>AckMotion<CR>g@
xnoremap <silent> \a :<C-U>call <SID>AckMotion(visualmode())<CR>
             
function! s:CopyMotionForType(type)
    if a:type ==# 'v'
        silent execute "normal! `<" . a:type . "`>y"
    elseif a:type ==# 'char'
        silent execute "normal! `[v`]y"
    endif
endfunction
                                                     
function! s:AckMotion(type) abort
    let reg_save = @@

    call s:CopyMotionForType(a:type)

    execute "normal! :Ack! --literal " . shellescape(@@) . "\<cr>"

    let @@ = reg_save
endfunction
