let mapleader = ","

syntax on
set autoindent
set smartindent
set tabstop=4
set shiftwidth=4
set backspace=indent,eol,start
set expandtab
set autochdir
set ls=2
set nu
set background=dark

" Vundle!
" $ git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
set nocompatible               " be iMproved
filetype off                   " required!

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required! 
Bundle 'gmarik/vundle'

" My Bundles here:
"
" github
Bundle 'xolox/vim-misc'
" vim scripts
Bundle 'Python-mode-klen'
Bundle 'Syntastic'
Bundle 'The-NERD-tree'
Bundle 'easytags.vim'

filetype indent plugin on     " required!

" Plugin specific options
"
" Python-mode-klen 
let g:pymode_syntax_indent_errors = 0
let g:pymode_syntax_space_errors = 0
let g:pymode_folding = 0
let g:pymode_lint = 0
let g:pymode_rope = 0

" Syntastic!
let g:syntastic_auto_loc_list = 1
nnoremap <Leader>e :Error<CR>

" NERDTree
let g:NERDTreeShowHidden = 1
let g:NERDTreeIgnore = ['\.DS_Store$']
nnoremap <Leader>nt :NERDTreeToggle

" Easytags
set tags='./tags';
let g:easytags_dynamic_files = 1
let g:easytags_auto_highlight = 0
