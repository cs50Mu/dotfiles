" Use Vim settings, rather then Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible
" fix backspace key doesn't work in Insert mode
" see also: http://vimdoc.sourceforge.net/htmldoc/options.html#%27backspace%27
set backspace=2

call plug#begin()

Plug 'dense-analysis/ale'
Plug 'hdima/python-syntax'
" Plug 'ervandew/supertab'
"Plug 'majutsushi/tagbar'
Plug 'fatih/vim-go', {'do': ':GoUpdateBinaries'}
Plug 'SirVer/ultisnips'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
" 用于快速注释
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-vinegar'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" plugin for elixir
"Plug 'elixir-editors/vim-elixir'
" for automatically adding `end` after `def`, `do`, etc
Plug 'tpope/vim-endwise'
" for elixir autoformat
"Plug 'mhinz/vim-mix-format'
"

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
" system-copy
Plug 'christoomey/vim-system-copy'

" Vim sugar for the UNIX shell commands that need it the most.
Plug 'tpope/vim-eunuch'

" Automatically create any non-existent directories before writing the buffer.
Plug 'pbrisbin/vim-mkdir'

" Make the yanked region apparent!
Plug 'machakann/vim-highlightedyank'

" 让fish shell script，比如config.fish 有语法高亮
Plug 'khaveesh/vim-fish-syntax'

" ********* rust stuff **********
"
" ref: https://sharksforarms.dev/posts/neovim-rust/
"
" Collection of common configurations for the Nvim LSP client
Plug 'neovim/nvim-lspconfig'

" Completion framework
Plug 'hrsh7th/nvim-cmp'

" LSP completion source for nvim-cmp
Plug 'hrsh7th/cmp-nvim-lsp'

" Snippet completion source for nvim-cmp
Plug 'hrsh7th/cmp-vsnip'

" Other usefull completion sources
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-buffer'

" See hrsh7th's other plugins for more completion sources!

" To enable more of the features of rust-analyzer, such as inlay hints and more!
Plug 'simrat39/rust-tools.nvim'

" Snippet engine
Plug 'hrsh7th/vim-vsnip'
" Plug 'rust-lang/rust.vim'

" Fuzzy finder
" Optional
" Plug 'nvim-lua/popup.nvim'
" Plug 'nvim-lua/plenary.nvim'
" Plug 'nvim-telescope/telescope.nvim'

" Color scheme used in the GIFs!
" Plug 'arcticicestudio/nord-vim'

" ********* rust stuff **********

" Initialize plugin system
call plug#end()

syntax on
filetype on

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
" 如果定义是在同一个文件内，则不会跳转到新的 split 窗口里
" 参考：https://github.com/fatih/vim-go/issues/2911
au FileType go nmap gv <Plug>(go-def-vertical)
au FileType go nmap gs <Plug>(go-def-split)

"""""""""""coc.nvim"""""""""""
" 参考：https://octetz.com/docs/2019/2019-04-24-vim-as-a-go-ide/
" https://github.com/neoclide/coc.nvim#example-vim-configuration

" Use tab for trigger completion with characters ahead and navigate.
" Use command ':verbose imap <tab>' to make sure tab is not mapped by other plugin.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction
" 若想自动选择补全的第一个，需要在:CocConfig里设置 suggest.noselect = false
" 更多配置，可以通过`:h coc-config-suggest` 来查看
" 参考：https://github.com/neoclide/coc.nvim/issues/2058

"""""""""""coc.nvim"""""""""""

" fzf settings
set rtp+=/usr/local/opt/fzf
nnoremap ,, :Files<Cr>

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

"Set mapleader
" let mapleader = ";"
let mapleader = "'"
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

"settings for ale(Asynchronous Lint Engine)
let g:ale_linters = {
\   'python': ['flake8'],
\   'go': ['gofmt', 'golint', 'go build', 'gotype'],
\}

" 不同的文件类型对于同样的功能使用相同的快捷键
" 参考：https://vi.stackexchange.com/questions/10664/file-type-dependent-key-mapping
autocmd FileType go nnoremap <buffer> <silent> g[  :ALENext<cr>
autocmd FileType go nnoremap <buffer> <silent> [g :ALEPrevious<cr>
" Set this in your vimrc file to disabling highlighting
let g:ale_set_highlights = 0

"settings for python-syntax
let python_highlight_all = 1

"""""""""rust config stuff"""""""""
" Set completeopt to have a better completion experience
" :help completeopt
" menuone: popup even when there's only one match
" noinsert: Do not insert text until a selection is made
" noselect: Do not select, force user to select one from the menu
set completeopt=menuone,noinsert,noselect

" Avoid showing extra messages when using completion
set shortmess+=c

" Configure LSP through rust-tools.nvim plugin.
" rust-tools will configure and enable certain LSP features for us.
" See https://github.com/simrat39/rust-tools.nvim#configuration
lua <<EOF
local nvim_lsp = require'lspconfig'

local opts = {
    tools = { -- rust-tools options
        autoSetHints = false,
        hover_with_actions = true,
        inlay_hints = {
            show_parameter_hints = false,
            parameter_hints_prefix = "",
            other_hints_prefix = "",
        },
    },

    -- all the opts to send to nvim-lspconfig
    -- these override the defaults set by rust-tools.nvim
    -- see https://github.com/neovim/nvim-lspconfig/blob/master/doc/server_configurations.md#rust_analyzer
    server = {
        -- on_attach is a callback called when the language server attachs to the buffer
        -- on_attach = on_attach,
        settings = {
            -- to enable rust-analyzer settings visit:
            -- https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/user/generated_config.adoc
            ["rust-analyzer"] = {
                -- enable clippy on save
                checkOnSave = {
                    command = "clippy"
                },
            }
        }
    },
}

require('rust-tools').setup(opts)
EOF

" Setup Completion
" See https://github.com/hrsh7th/nvim-cmp#basic-configuration
lua <<EOF
local cmp = require'cmp'
cmp.setup({
  -- Enable LSP snippets
  snippet = {
    expand = function(args)
        vim.fn["vsnip#anonymous"](args.body)
    end,
  },
  mapping = {
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-n>'] = cmp.mapping.select_next_item(),
    -- Add tab support
    ['<S-Tab>'] = cmp.mapping.select_prev_item(),
    ['<Tab>'] = cmp.mapping.select_next_item(),
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<CR>'] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Insert,
      select = true,
    })
  },

  -- Installed sources
  sources = {
    { name = 'nvim_lsp' },
    { name = 'vsnip' },
    { name = 'path' },
    { name = 'buffer' },
  },
})
EOF


" Code navigation shortcuts
autocmd FileType rust noremap <silent> K     <cmd>lua vim.lsp.buf.hover()<CR>
autocmd FileType rust noremap <silent> gD    <cmd>lua vim.lsp.buf.implementation()<CR>
" 与窗口管理冲突了
" autocmd FileType rust noremap <silent> <c-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
autocmd FileType rust noremap <silent> 1gD   <cmd>lua vim.lsp.buf.type_definition()<CR>
autocmd FileType rust noremap <silent> gr    <cmd>lua vim.lsp.buf.references()<CR>
autocmd FileType rust noremap <silent> g0    <cmd>lua vim.lsp.buf.document_symbol()<CR>
autocmd FileType rust noremap <silent> gW    <cmd>lua vim.lsp.buf.workspace_symbol()<CR>

autocmd FileType rust nnoremap <silent> gd    <cmd>lua vim.lsp.buf.definition()<CR>
autocmd FileType rust noremap <silent> gv    :vs<CR><cmd>lua vim.lsp.buf.definition()<CR>
autocmd FileType rust noremap <silent> gs    :sp<CR><cmd>lua vim.lsp.buf.definition()<CR>
autocmd FileType rust noremap <silent> ga    <cmd>lua vim.lsp.buf.code_action()<CR>

" rename
nnoremap <silent> <leader>lr    <cmd>lua vim.lsp.buf.rename()<CR>

" Set updatetime for CursorHold
" 300ms of no cursor movement to trigger CursorHold
set updatetime=300
" Show diagnostic popup on cursor hold
autocmd CursorHold * lua vim.diagnostic.open_float(nil, { focusable = false })

" Goto previous/next diagnostic warning/error
autocmd FileType rust nnoremap <buffer> <silent> g[ <cmd>lua vim.lsp.diagnostic.goto_prev()<CR>
autocmd FileType rust nnoremap <buffer> <silent> [g <cmd>lua vim.lsp.diagnostic.goto_next()<CR>

" format on save
autocmd BufWritePre *.rs lua vim.lsp.buf.formatting_sync(nil, 200)

"""""""""rust config stuff"""""""""


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
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""



""""""""""""""""keybinding to run rust testcase""""""""""""""""
" steal from https://github.com/davidpdrsn/dotfiles/blob/master/nvim/init.vim
function! s:run_rust_tests()
  if &modified
    write
  end
  call SmartRun("cargo test --all --all-features")
endfunction

nnoremap <leader>T :call <SID>run_rust_tests()<cr>

" <test-running-functions>
  " Functions used to run tests in a terminal split and automatically closing
  " the split if the tests are green. If they're red, jump forward to the
  " word 'Failure'
  function! TerminalRun(cmd)
    execute "new"
    call termopen(a:cmd, {
          \ 'on_exit': function('TerminalOnExit'),
          \ 'buf': expand('<abuf>')
          \})
    execute "normal i"
  endfunction

  function! TerminalOnExit(job_id, exit_code, event) dict
    if a:exit_code == 0
      execute "bd! " . s:test_buffer_number
      wincmd =
    else
      wincmd =
    endif
  endfunction

  function! TerminalOnTermClose(buf)
    let s:test_buffer_number = a:buf
  endfunction
" </test-running-functions>

function! FifoRun(cmd)
  let pwd = getcwd()
  execute "silent !runner --pwd " . pwd . " --cmd '" . a:cmd . "'"
endfunction

function! SmartRun(cmd)
  silent! let output = system('runner --check')

  if output == "Found at least one instance running\n"
    call FifoRun(a:cmd)
  else
    call TerminalRun(a:cmd)
  endif
endfunction

augroup neorun
  autocmd!
  autocmd TermClose * :call TerminalOnTermClose(0+expand('<abuf>'))
augroup end
""""""""""""""""keybinding to run rust testcase""""""""""""""""
