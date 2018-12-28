"use Space as the leader 
let mapleader = "\<Space>" 

" Specify a directory for plugins 
" - For Neovim: ~/.local/share/nvim/plugged 
" - Avoid using standard Vim directory names like 'plugin' 
call plug#begin('~/.vim/plugged') 

" GUI enhancements 
Plug 'vim-airline/vim-airline'
Plug 'machakann/vim-highlightedyank' 
Plug 'morhetz/gruvbox'

" Git 
Plug 'tpope/vim-fugitive'     

" Auto complete 
" Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' } 
Plug 'Shougo/neopairs.vim' 
" Plug 'zchee/deoplete-go', { 'do': 'make'} 

Plug 'rust-lang/rust.vim'
Plug 'majutsushi/tagbar'

Plug 'elzr/vim-json' 

" Linter
Plug 'w0rp/ale'

" Semantic language support 
Plug 'autozimu/LanguageClient-neovim', {     
	\ 'branch': 'next',     
	\ 'do': 'bash install.sh',     
	\ } 
Plug 'mattn/webapi-vim' 

" Golang 
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' } 

" Fish function 
Plug 'dag/vim-fish' 

" Completion plugins 
Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp'
Plug 'ncm2/ncm2-bufword' 
Plug 'ncm2/ncm2-tmux' 
Plug 'ncm2/ncm2-path' 
Plug 'filipekiss/ncm2-look.vim' " Completion for common words
Plug 'jiangmiao/auto-pairs'

" Fuzzy finder 
Plug 'airblade/vim-rooter' 
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' } 
Plug 'junegunn/fzf.vim' 

" Shorthand notation; fetches https://github.com/junegunn/vim-easy-align 
Plug 'junegunn/vim-easy-align' 

" On-demand loading 
Plug 'scrooloose/nerdtree' 
Plug 'Xuyuanp/nerdtree-git-plugin' 

" Initialize plugin system 
call plug#end() 

filetype plugin indent on 

" Use [number]+j, [number]+k to navigate 
set relativenumber  " Relative line numbers 
set number
set colorcolumn=120 " and give me a colored column 
set hidden 
set ruler 
set synmaxcol=2000 
set showcmd 
set clipboard+=unnamedplus " copy to clipboard

" show existing tab with 4 spaces width 
set tabstop=4 " when indenting with '>', use 4 spaces width 
set shiftwidth=4 " On pressing tab, insert 4 spaces 
set expandtab 
" in makefiles, don't expand tabs to spaces, since actual tab characters are
" needed, and have indentation at 8 chars to be sure that all indents are tabs
autocmd FileType make set noexpandtab shiftwidth=8 softtabstop=0

" Colors 
set background=dark
colorscheme gruvbox

" ============================================================================= 
" # Keyboard shortcuts 
" ============================================================================= 
" ; as : 
nnoremap ; : 

" Open hotkeys 
map <C-p> :Files<CR> 
nmap <C-b> :Buffers<CR> 

" Quick-save 
nmap <leader>w :w<CR> 
map <C-a> <esc>ggVG<CR> 

" <leader><leader> toggles between buffers 
nnoremap <leader><leader> <c-^> 

" Neat X clipboard integration 
" ,p will paste clipboard into buffer 
" ,c will copy entire buffer into clipboard 
" noremap <leader>p :read !xsel --clipboard --output<cr> 
" noremap <leader>c :w !xsel -ib<cr><cr> 
set clipboard+=unnamedplus 

" Permanent undo 
set undodir=~/.vimdid 
set undofile 

" Proper search 
set incsearch 
set ignorecase 
set smartcase 

" Search results centered please 
nnoremap <silent> n nzz 
nnoremap <silent> N Nzz 
nnoremap <silent> * *zz 
nnoremap <silent> # #zz 
nnoremap <silent> g* g*zz 

" Javascript 
let javaScript_fold=0 

" Linter 
" let g:ale_sign_column_always = 1 " only lint on save 
" let g:ale_completion_enabled = 1 
let g:ale_lint_on_text_changed = 'never' 
let g:ale_lint_on_save = 1
let g:ale_lint_on_enter = 0 
let g:ale_rust_cargo_use_check = 1 
let g:ale_rust_cargo_check_all_targets = 1 
let g:airline#extensions#ale#enabled = 1

" language server protocol 
let g:LanguageClient_rootMarkers = {     
	\ 'go': ['go.mod'],     
	\ } 
let g:LanguageClient_settingsPath = "/home/kaka/.vim/settings.json" 
let g:LanguageClient_serverCommands = {     
	\ 'rust': ['env', 'CARGO_TARGET_DIR=/home/kaka/data/rust/cargo-target/rls', 'rls'],     
	\ 'javascript': ['/usr/bin/javascript-typescript-stdio'],     
	\ 'javascript.jsx': ['tcp://127.0.0.1:2089'],     
	\ } 
" 'go': ['bingo', '--mode', 'stdio', '--logfile', '/tmp/lspserver.log','--trace', '--pprof', ':6060'], 
let g:LanguageClient_autoStart = 1 
nnoremap <silent> K :call LanguageClient_textDocument_hover()<CR> 
nnoremap <silent> gd :call LanguageClient_textDocument_definition()<CR> 
nnoremap <silent> <F2> :call LanguageClient_textDocument_rename()<CR> 

" Tagbar
nmap <F8> :TagbarToggle<CR>

" Syntastic config 
" let g:syntastic_always_populate_loc_list = 1
" let g:syntastic_auto_loc_list = 1
" let g:syntastic_check_on_open = 1
" let g:syntastic_check_on_wq = 0

" Auto complete 
" let g:deoplete#enable_at_startup = 1 
" call deoplete#custom#source('LanguageClient', 'converters', 
"	\ ['converter_auto_delimiter', 'converter_remove_overlap', 'converter_truncate_abbr', 'converter_truncate_menu', 'converter_auto_paren']) 
" inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>" 
" inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>" 
" Completion
autocmd BufEnter * call ncm2#enable_for_buffer()
set completeopt=noinsert,menuone,noselect

" Golang vim-go shortcuts 
autocmd FileType go nmap <leader>t  <Plug>(go-test) 
autocmd FileType go nmap <leader>b  <Plug>(go-build) 
autocmd FileType go nmap <Leader>c  <Plug>(go-coverage-toggle) 
autocmd FileType go nmap <Leader>i  <Plug>(go-info) 
autocmd FileType go nmap <leader>r  <Plug>(go-run) 

" Rust.vim
let g:rustfmt_autosave = 1

" NerdTree 
let NERDTreeShowHidden=1
" let g:NERDTreeDirArrowExpandable='▶' 
" let g:NERDTreeDirArrowCollapsible='▼' 

" Open a NERDTree automatically when vim starts up if no files were specified
autocmd StdinReadPre * let s:std_in=1 
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif 
" Open NERDTree automatically when vim starts up on opening a directory
autocmd VimEnter * if argc() == 1 && isdirectory(argv()[0]) && !exists("s:std_in") | exe 'NERDTree' argv()[0] | wincmd p | ene | endif 
" Close vim if the only window left open is a NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif 

" airline
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#buffer_idx_mode = 1

" Tab navigation like Firefox.
" nnoremap <C-S-Tab> :bprevious<CR>
" nnoremap <C-Tab>   :bnext<CR>

" ============================================================================= 
" # Custome commands 
" ============================================================================= 
" JSON format 
:command Format %!python -m json.tool
