fun! ShowFuncName()
  let lnum = line(".")
  let col = col(".")
  echohl ModeMsg
  echo getline(search("^[^ \t#/]\\{2}.*[^:]\s*$", 'bW'))
  echohl None
  call search("\\%" . lnum . "l" . "\\%" . col . "c")
endfun
map f :call ShowFuncName() <CR>

set bg=dark
set enc=utf-8
set fenc=utf-8
set termencoding=utf-8
set nocompatible
set autoindent
set smartindent
set ts=4
set sts=4
set sw=4
set t_Co=256
syntax on
set number
set showmatch
map <F12> :set number!<CR>
map <S-F12> :set smartindent!<CR>:set autoindent!<CR>
set showcmd
set incsearch
set textwidth=0
filetype on
filetype plugin on
filetype indent on
runtime macros/matchit.vim
set laststatus=2
set statusline+=%F
