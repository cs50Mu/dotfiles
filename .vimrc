" Use Vim settings, rather then Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible
" fix backspace key doesn't work in Insert mode
" see also: http://vimdoc.sourceforge.net/htmldoc/options.html#%27backspace%27
set backspace=2

call plug#begin()

Plug 'dense-analysis/ale'
Plug 'hdima/python-syntax'
Plug 'ervandew/supertab'
"Plug 'majutsushi/tagbar'
Plug 'fatih/vim-go', {'do': ':GoUpdateBinaries'}
Plug 'SirVer/ultisnips'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
" 用于快速注释
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-sensible'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" plugin for elixir
"Plug 'elixir-editors/vim-elixir'
" for automatically adding `end` after `def`, `do`, etc
Plug 'tpope/vim-endwise'
" for elixir autoformat
"Plug 'mhinz/vim-mix-format'
"
" for rust
" Plugin 'rust-lang/rust.vim'
" Plugin 'racer-rust/vim-racer'

" Vim plugin that provides additional text objects
Plug 'wellle/targets.vim'
" fzf
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

" focus.nvim
Plug 'beauwilliams/focus.nvim'

Plug 'jiangmiao/auto-pairs'

" Seamless navigation between tmux panes and vim splits
Plug 'christoomey/vim-tmux-navigator'

" Vim sugar for the UNIX shell commands that need it the most.
Plug 'tpope/vim-eunuch'

" Automatically create any non-existent directories before writing the buffer.
Plug 'pbrisbin/vim-mkdir'

" Make the yanked region apparent!
Plug 'machakann/vim-highlightedyank'

" Initialize plugin system
call plug#end()

syntax on

" settings for vim-mix-format
let g:mix_format_on_save = 1
let g:mix_format_silent_errors = 1

" " settings for ctrlp
" let g:ctrlp_map = ',,'
" let g:ctrlp_open_multiple_files = 'v'
" set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc
" let g:ctrlp_custom_ignore = {
"             \'dir': '\v[\/]\.(git)$',
"             \'file': '\v\.(jpg|png|jpeg)$',
"             \}
" " Ignore files in .gitignore
" let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files -co --exclude-standard']

" vim-go configeration
" auto import dependencies
let g:go_fmt_command = "goimports"
" Code highlighting
let g:go_highlight_build_constraints = 1
let g:go_highlight_extra_types = 1
let g:go_highlight_fields = 1
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_operators = 1
let g:go_highlight_structs = 1
let g:go_highlight_types = 1
" go def 默认用guru
let g:go_def_mode = 'gopls'
au FileType go nmap gv <Plug>(go-def-vertical)
au FileType go nmap gx <Plug>(go-def-split)

" 配合coc.nvim
" 暂时禁用，因为它们跟`vim-endwise`插件冲突
" refer: https://github.com/neoclide/coc.nvim/wiki/Completion-with-sources#improve-the-completion-experience
" Use <Tab> and <S-Tab> to navigate the completion list
" inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
" inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
" Use <cr> to confirm completion
" inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

" fzf settings
set rtp+=/usr/local/opt/fzf
nnoremap ,, :GFiles<Cr>

" focus.nvim
"You must run setup() to begin using focus
if has('nvim')
    lua require("focus").setup()
endif


"set fenc=utf-8 " default fileencoding
set fencs=utf-8,gb18030,gbk,gb2312,cp936,ucs-bom,euc-jp,

set history=50		" keep 50 lines of command line history
set ruler		" show the cursor position all the time
set showcmd		" display incomplete commands
set incsearch		" do incremental searching
set ignorecase
set smartcase   "启用智能大小写
"简化显示
set go=
"设置gui字体
"set guifont=monospace\ 12
"代码折叠方案
set foldmethod=manual
"set foldlevel=99
nnoremap <space> za
vnoremap <space> zf
" highlight folded
" https://stackoverflow.com/questions/24626476/how-to-change-the-folded-part-color-on-vim
highlight Folded ctermbg=red
"设置配色
"colorscheme molokai
"let g:molokai_original = 1
"let g:rehash256 = 1
"显示行号
set relativenumber
set number
"设置缩进有三个取值cindent(c风格)、smartindent(智能模式，其实不觉得有什么智能)、autoindent(简单的与上一行保持一致)
set smartindent
"用空格键替换制表符
set expandtab
"制表符占4个空格
set tabstop=4
"默认缩进4个空格大小
set shiftwidth=4
"高亮搜索
set hlsearch
"突出显示当前行
set cursorline
" set statusline
set laststatus=2
set statusline=%F:\ %l,%c
" use tab to switch between different buffers
set hidden "in order to switch between buffers with unsaved change
map <s-tab> :bp<cr>
map <tab> :bn<cr>
" 在上下移动光标时，光标的上方或下方至少会保留显示的行数
set scrolloff=7

"auto complete
autocmd FileType python set omnifunc=pythoncomplete#Complete

" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
" (happens when dropping a file on gvim).
" Also don't do it when the mark is in the first line, that is the default
" position when opening a file.
autocmd BufReadPost *
\ if line("'\"") > 1 && line("'\"") <= line("$") |
\   exe "normal! g`\"" |
\ endif

"新建文件，自动插入文件头 
autocmd BufNewFile *.sh,*.php,*.py exec ":call SetTitle()" 
""定义函数SetTitle，自动插入文件头 
func! SetTitle() 
	"如果文件类型为.sh文件 
	if &filetype == 'sh' 
		call setline(1, "\#!/bin/bash") 
		call append(1, "\#########################################################################") 
		call append(2, "\# Author: ".$USER) 
		call append(3, "\# Created Time: ".strftime("%Y-%m-%d %H:%M:%S"))
		call append(4, "\# File Name: ".expand("%")) 
		call append(5, "\# Description: ")
		call append(6, "\#########################################################################") 
	elseif &filetype == 'php'
		call setline(1, "<?php") 
		call append(1, "\#########################################################################") 
		call append(2, "\# Author: ".$USER) 
		call append(3, "\# Created Time: ".strftime("%Y-%m-%d %H:%M:%S"))
		call append(4, "\# File Name: ".expand("%")) 
		call append(5, "\# Description: ")
		call append(6, "\#########################################################################") 
		call append(7, "") 
		call append(8, "class ".substitute(expand("%"), '.php', '', 1)." extends ") 
	elseif &filetype == 'python'
		call setline(1, "\#!/usr/bin/env python")
		call append(1, "\#-*- coding: utf-8 -*-")
		call append(2, "\#########################################################################")
		call append(3, "\# Author: ".$USER) 
		call append(4, "\# Created Time: ".strftime("%Y-%m-%d %H:%M:%S"))
		call append(5, "\#########################################################################")
	endif
	"新建文件后，自动定位到文件末尾
	autocmd BufNewFile * normal G
endfunc 

"Set mapleader
let mapleader = ";"
""Fast reloading of the .vimrc
map <silent> <leader>ss :source ~/.vimrc<cr>
"Fast editing of .vimrc
map <silent> <leader>ee :e ~/.vimrc<cr>
""When .vimrc is edited, reload it
autocmd bufwritepost .vimrc source ~/.vimrc
" Golang: list funcs in current dir
au FileType go nmap <leader>aa :GoDeclsDir<cr>
" hotkey for remove highlight
nnoremap <leader><space> :noh<cr>


" nmap <silent> gd <Plug>(coc-definition)

" rust go to defination
au FileType rust nmap <buffer> gv :call CocAction('jumpDefinition', 'vsplit')<cr>
au FileType rust nmap <buffer> gs :call CocAction('jumpDefinition', 'split')<cr>

"settings for ale(Asynchronous Lint Engine)
let g:ale_linters = {
\   'python': ['flake8'],
\   'go': ['gofmt', 'golint', 'go build', 'gotype'],
\}
nmap <silent> <C-k> <Plug>(ale_previous_wrap)
nmap <silent> <C-j> <Plug>(ale_next_wrap)
" Set this in your vimrc file to disabling highlighting
let g:ale_set_highlights = 0

"settings for python-syntax
let python_highlight_all = 1

" => Splits and Tabbed Files
" ref: https://gitlab.com/dwt1/dotfiles/-/blob/master/.config/nvim/init.vim
" 如何debug a mapping:
" https://vi.stackexchange.com/questions/7722/how-to-debug-a-mapping
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" neovim 报错，暂时禁用
" set term=xterm-256color

set splitbelow splitright

" Remap splits navigation to just CTRL + hjkl
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Make adjusing split sizes a bit more friendly
noremap <silent> <C-Left> :vertical resize +3<CR>
noremap <silent> <C-Right> :vertical resize -3<CR>
noremap <silent> <C-Up> :resize +3<CR>
noremap <silent> <C-Down> :resize -3<CR>

" Removes pipes | that act as seperators on splits
set fillchars+=vert:\ 
