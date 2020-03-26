set nocompatible              " required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" add all your plugins here (note older versions of Vundle
" used Bundle instead of Plugin)

" ...

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

set splitbelow
set splitright

"split navigations
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Enable folding
set foldmethod=indent
set foldlevel=99
" Enable folding with the spacebar
nnoremap <space> za

Plugin 'tmhedberg/SimpylFold'



au BufNewFile,BufRead *.py
    \ set tabstop=4
    \|set softtabstop=4
    \|set shiftwidth=4
    \|set textwidth=79
    \|set expandtab
    \|set autoindent
    \|set fileformat=unix


au BufNewFile,BufRead *.js, *.html, *.css
    \ set tabstop=2
    \|set softtabstop=2
    \|set shiftwidth=2


Plugin 'vim-scripts/indentpython.vim'


set encoding=utf-8


Bundle 'Valloric/YouCompleteMe'


let g:ycm_autoclose_preview_window_after_completion=1
map <leader>g  :YcmCompleter GoToDefinitionElseDeclaration<CR>



Plugin 'vim-syntastic/syntastic'


Plugin 'nvie/vim-flake8'


let python_highlight_all=1
syntax on


Plugin 'jnurmine/Zenburn'
Plugin 'altercation/vim-colors-solarized'
Plugin 'dracula/vim', { 'name': 'dracula' }
Plugin 'NLKNguyen/papercolor-theme'
Plugin 'rakr/vim-one'
Plugin 'arcticicestudio/nord-vim'

" set background=dark
colorscheme one

call togglebg#map("<F5>")


Plugin 'scrooloose/nerdtree'
nmap <F6> :NERDTreeToggle<CR>

Plugin 'jistr/vim-nerdtree-tabs'
" autocmd VimEnter * NERDTree

let NERDTreeIgnore=['\.pyc$', '\~$'] "ignore files in NERDTree


Plugin 'kien/ctrlp.vim'


set nu


Plugin 'Lokaltog/powerline', {'rtp': 'powerline/bindings/vim/'}
Plugin 'itchyny/lightline.vim'

set noshowmode

set clipboard=unnamed


"set editing-mode vi


Plugin 'vim-airline/vim-airline'


let g:lightline = {
     \ 'colorscheme': 'one',}

set shortmess=F
let g:airline_theme='simple'
let g:airline#extensions#tabline#left_alt_sep = ' '
let g:airline#extensions#tabline#formatter = 'default'


" let g:netrw_banner = 0
" let g:netrw_liststyle = 3
" let g:netrw_browse_split = 4
" let g:netrw_altv = 1
let g:netrw_winsize = 25
" augroup ProjectDrawer
"  autocmd!
"  autocmd VimEnter * :Vexplore
" augroup END

:set number relativenumber

