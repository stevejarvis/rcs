syntax on

set autoindent
set smartindent
set expandtab
set tabstop=4
set shiftwidth=4

" I want colored numbers
" Check here for all colors:
" http://vim.wikia.com/wiki/Xterm256_color_names_for_console_Vim
set nu
highlight LineNr ctermfg=187

" Automatically change window's cwd to file's dir
set autochdir

" Vundle!
set nocompatible               " be iMproved
filetype off                   " required!

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required! 
Bundle 'gmarik/vundle'

" My Bundles here:
"
" vim scripts
Bundle 'Python-mode-klen'
Bundle 'indentjava.vim'

filetype plugin indent on     " required!
"
" Brief help
" :BundleList          - list configured bundles
" :BundleInstall(!)    - install(update) bundles
" :BundleSearch(!) foo - search(or refresh cache first) for foo
" :BundleClean(!)      - confirm(or auto-approve) removal of unused bundles
"
" see :h vundle for more details or wiki for FAQ
" NOTE: comments after Bundle command are not allowed..

" Python-mode-klen options
let g:pymode_syntax_indent_errors = 0
let g:pymode_syntax_space_errors = 0
let g:pymode_folding = 0
let g:pymode_lint = 0
let g:pymode_rope = 0
