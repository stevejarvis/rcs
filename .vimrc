" General
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
set ruler

" Window nav
nmap <C-h> <C-w>h
nmap <C-j> <C-w>j
nmap <C-k> <C-w>k
nmap <C-l> <C-w>l

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
Bundle 'derekwyatt/vim-fswitch'
Bundle 'stevejarvis/cscope.vim'
Bundle 'scrooloose/syntastic'
Bundle 'scrooloose/nerdtree'
" vim scripts
Bundle 'Python-mode-klen'
Bundle 'easytags.vim'
Bundle 'Tagbar'

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
let g:syntastic_auto_loc_list = 0
" Check is too slow to do every write?
let g:syntastic_mode_map = { 'mode': 'passive' }
nnoremap <Leader>sc :SyntasticCheck<CR>
nnoremap <Leader>e :Error<CR>

" NERDTree
let g:NERDTreeShowHidden = 1
let g:NERDTreeIgnore = ['\.DS_Store$']
nnoremap <Leader>nt :NERDTreeToggle<CR>

" Easytags
set tags=./tags;
let g:easytags_updatetime_warn = 0
let g:easytags_updatetime_min = 10000
let g:easytags_dynamic_files = 1
let g:easytags_auto_highlight = 0

" Tagbar
nnoremap <Leader>tb :TagbarToggle<CR>

" FSwitch
nmap <silent> <Leader>of :FSHere<cr>
nmap <silent> <Leader>oH :FSSplitLeft<cr>
nmap <silent> <Leader>oL :FSSplitRight<cr>

" cscope
let g:cscope_autoclose = 1
