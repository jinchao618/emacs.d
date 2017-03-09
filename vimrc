
"let mapleader=";"
set nocompatible
filetype off

set runtimepath+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/nerdcommenter'
Plugin 'flazz/vim-colorschemes.git'
Plugin 'dyng/ctrlsf.vim'
"Plugin 'Valloric/YouCompleteMe'
"Bundle 'vim-latex-1.8.23/.git'
call vundle#end()

filetype plugin indent on

if has("gui_running")
  set gfn=Monospace\ 12
  colorscheme solarized
  set background=dark
endif
set t_Co=256

syntax on
set encoding=utf8
set hidden
set wildmenu
set backspace=indent,eol,start
set history=50
set ruler
set nomodeline
set suffixes=~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
set wildignore=*.o,*.obj
set nobackup
set visualbell
set incsearch
set shellpipe=2>&1\ \|tee
set grepprg=grep\ -nH
set shiftwidth=2
set softtabstop=2
set expandtab
set ignorecase
set smartcase
set mouse=a
set cino=(0
set hlsearch
set path=.,,**
set nu

" CtrlP Setting
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlPBuffer'
let g:ctrlp_max_files=0
let g:ctrlp_max_depth=40

if has("cscope")
  set csprg=gtags-cscope
  set csto=0
  set cst
  set nocsverb
  " add any database in current directory
  if filereadable("GTAGS")
    cs add GTAGS
    " else add database pointed to by environment
  elseif $CSCOPE_DB != ""
    cs add $CSCOPE_DB
  endif
  set csverb
  nmap <C-\>s :cs find s <C-R>=expand("<cword>")<CR>
  nmap <C-\>g :cs find g <C-R>=expand("<cword>")<CR>
  nmap <C-\>c :cs find c <C-R>=expand("<cword>")<CR>
  nmap <C-\>t :cs find t <C-R>=expand("<cword>")<CR>
  nmap <C-\>e :cs find e <C-R>=expand("<cword>")<CR>
  nmap <C-\>f :cs find f <C-R>=expand("<cfile>")<CR>
  nmap <C-\>i :cs find i <C-R>=expand("<cfile>")<CR>
  nmap <C-\>d :cs find d <C-R>=expand("<cword>")<CR>
endif

"colorscheme solarized
"set background=dark
"set t_Co=256
set shellslash
set grepprg=grep\ -nH\$*
let g:tex_flavor='latex'
