source $HOME/.config/nvim/settings.vim
lua require('plugins')
lua require('tree')
" nnoremap <leader>sv :source $MYVIMRC<CR>
:imap <C-L> <Esc>
:imap <C-G> <Esc>
" insert empty lines wihtout leaving normal mode
nnoremap <Leader>O o<Esc>0"_D
nnoremap <Leader>o O<Esc>0"_D
tnoremap <Esc> <C-\><C-n>
" let g:python_folding = 1
" et g:SimpylFold_fold_while = 1
" set foldmethod=indent
" set foldmethod=expr
" set foldexpr=nvim_treesitter#foldexpr()
" Use alt + hjkl to resize windows
nnoremap <M-j>    :resize -2<CR>
nnoremap <M-k>    :resize +2<CR>
nnoremap <M-h>    :vertical resize -2<CR>
nnoremap <M-l>    :vertical resize +2<CR>

" inoremap jk <Esc>
" inoremap kj <Esc>
nnoremap <Up> <C-y>
nnoremap <Down> <C-e>

" Easier Moving between splits
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" forgot why i added this!
autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif

autocmd Filetype lua setlocal ts=2 sw=2
" set completeopt-=preview

let g:neovide_no_idle=v:true
set guifont=Go\ Mono:h10.5
" set guifont=Hack\ Nerd\ Font:h10.5
let g:neovide_cursor_animation_length=0.1
" set guifont=Monoid\ Nerd\ Font:h9

nnoremap <silent><leader>fs :write<CR>
nnoremap <silent><leader>hh :set hls<CR>
nnoremap <silent><leader>hg :set nohls<CR>

" https://github.com/glepnir/lspsaga.nvim
" nnoremap <silent><leader>ca :Lspsaga code_action<CR>
" vnoremap <silent><leader>ca :<C-U>Lspsaga range_code_action<CR>
" nnoremap <silent> gh :Lspsaga lsp_finder<CR>
" nnoremap <silent>gR :Lspsaga rename<CR>

" GoTo code navigation.
" nmap <silent> gd <Plug>(coc-definition)
" nmap <silent> gy <Plug>(coc-type-definition)
" nmap <silent> gi <Plug>(coc-implementation)
" nmap <silent> gr <Plug>(coc-references)
" nmap <leader>rn <Plug>(coc-rename)
" nmap <silent>gR :Telescope coc references<CR>
" nmap <silent>gs :Telescope coc document_symbols<CR>


" lsp outline
" nnoremap <silent><leader>so :SymbolsOutline<CR>
" nnoremap <silent>go :SymbolsOutline<CR>

" set foldmethod=expr
" set foldexpr=nvim_treesitter#foldexpr()
" highlight RedundantSpaces ctermbg=red guibg=red 
" match RedundantSpaces /\s\+$/
" highlight ColorColumn ctermbg=0 guibg=lightgray
" set list listchars=tab:>\ ,trail:-,eol:~

" Add a new named configuration
        " \ 'eol': '←',
call cyclist#add_listchar_option_set('default', {
        \ 'eol': '',
        \ 'tab': '» ',
        \ 'trail': '·',
        \ 'extends': '<',
        \ 'precedes': '>',
        \ 'conceal': '┊',
        \ 'nbsp': '␣',
        \ })
call cyclist#add_listchar_option_set('limited', {
        \ 'eol': '↲',
        \ 'tab': '» ',
        \ 'trail': '·',
        \ 'extends': '<',
        \ 'precedes': '>',
        \ 'conceal': '┊',
        \ 'nbsp': '␣',
        \ })
call cyclist#add_listchar_option_set('busy', {
        \ 'eol': '←',
        \ 'tab': '»·',
        \ 'space': '␣',
        \ 'trail': '-',
        \ 'extends': '☛',
        \ 'precedes': '☚',    
        \ 'conceal': '┊',
        \ 'nbsp': '☠',
        \ })
" Cycle to the next configuration
nmap <leader>cn <Plug>CyclistNext
" nmap <leader>cp <Plug>CyclistPrev


" let g:nvim_tree_ignore = [ '.git', 'node_modules', '.cache' ]
" let g:nvim_tree_gitignore = 1
" let g:nvim_tree_window_picker_exclude = {
"     \   'filetype': [
"     \     'packer',
"     \     'qf'
"     \   ],
"     \   'buftype': [
"     \     'terminal'
"     \   ]
"     \ }

" nnoremap <leader>tt :NvimTreeToggle<CR>
" nnoremap <leader>tr :NvimTreeRefresh<CR>
" nnoremap <leader>tf :NvimTreeFindFile<CR>

