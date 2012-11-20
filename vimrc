" ========= Setup ========

set nocompatible
filetype off

if &shell == "/usr/bin/sudosh"
  set shell=/bin/bash
endif

" Use Pathogen to load bundles
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

filetype plugin indent on

" Highlight trailing whitespace
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd BufRead,InsertLeave * match ExtraWhitespace /\s\+$/

" Set up highlight group & retain through colorscheme changes
highlight ExtraWhitespace ctermbg=red guibg=red
autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red

"

" ========= Options ========

" Basic options
syntax enable
set encoding=utf-8
set scrolloff=3
set autoindent
set showmode
set nopaste
set hidden
set wildmenu
set wildmode=list:longest
set visualbell
set ttyfast
set ruler
set number
set backspace=indent,eol,start
set laststatus=1                  " Show the status line all the time
set wildignore+=*.o,*.obj,.git,*.pyc,.DS_Store
set t_Co=256
set showbreak=↪

" Security
set modelines=0

" Tabs/spaces
set expandtab       " set up spaces as tabs
set tabstop=2
set sts=2           " 2 spaces
set shiftwidth=2
set shiftround      " when at 3 spaces, and I hit > ... go to 4, not 5
set smarttab
set smartindent     " Indent based on the previous line

" Scroll the viewport faster
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

" Searching
set ignorecase
set smartcase
set incsearch
set showmatch
set hlsearch

" backup to ~/.tmp
set nobackup
set nowritebackup
set noswapfile

" Soft/hard wrapping
set wrap
set textwidth=79
set formatoptions=qrn1

" Color
set background=dark
colorscheme ir_ben

" File Types
au BufNewFile,BufRead *.less set filetype=less
au BufNewFile,BufRead rebar.config set filetype=erlang
au BufNewFile,BufRead *.mustache setf mustache
runtime! ftdetect/*.vim

" Folding
set foldmethod=indent
set foldlevel=2
set nofoldenable

" Go
let go_highlight_trailing_whitespace_error = 0
autocmd Filetype go setlocal textwidth=0 nosmartindent tabstop=8 shiftwidth=8 softtabstop=8 noexpandtab

" ========= Plugin Options ========

" Ctrlp
set wildignore+=*/tmp/*,*.so,*.swp,*.zip     " MacOSX/Linux
let g:ctrlp_max_files = 0
let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files']

" NERDTree
let NERDTreeShowFiles=1
let NERDTreeShowHidden=1
let NERDTreeMinimalUI=1
let NERDTreeDirArrows=1
let NERDTreeQuitOnOpen=1
let NERDTreeIgnore=['.DS_Store']

" Surrond
let g:surround_{char2nr("t")} = "<\1\r..*\r&\1>\r</\1\r..*\r&\1>"

" Vimerl
let erlang_show_errors = 0

" Powerline
let g:powerline_cache_file = "~/.vim"

" Vimux
let VimuxUseNearestPane = 1
let g:VimuxOrientation = "h"
let g:VimuxHeight = "40"

"

" ========= Navigation Shortcuts ========

" Easier split navigation
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

" Alternate Files
map <leader>aa :A<CR>
map <leader>as :AS<CR>
map <leader>av :AV<CR>

" Buffers
noremap <leader><leader> <C-^>
noremap <leader>be :EasyBuffer<CR>
noremap <leader>bs <C-w>s:EasyBuffer<CR>
noremap <leader>bv <C-w>v:EasyBuffer<CR>


" CommandT
map <leader>ff :CtrlP<Enter>
map <leader>fb :CtrlPBuffer<Enter>
map <leader>fr :CtrlPClearAllCaches<Enter>

" NERDTree
map <leader>nt :NERDTreeToggle<Enter>
map <leader>nt :NERDTreeToggle<Enter>
map <leader>nf :NERDTreeFind<Enter>

" Create window splits easier. The default
" " way is Ctrl-w,v and Ctrl-w,s. I remap
" " this to vv and ss
nnoremap <silent> vv <C-w>v
nnoremap <silent> ss <C-w>s

" ========= Coding Shortcuts ========

" Make Y behave like other capitals
map Y y$

" TComment
map <leader>cc :TComment<CR>

" Prompt for a command to run
map <Leader>vp :PromptVimTmuxCommand<CR>

" Run last command executed by RunVimTmuxCommand
map <Leader>rl :RunLastVimTmuxCommand<CR>

" Inspect runner pane
map <Leader>vi :InspectVimTmuxRunner<CR>

" Close all other tmux panes in current window
map <Leader>vx :CloseVimTmuxPanes<CR>

" Interrupt any command running in the runner pane
map <Leader>ve :InterruptVimTmuxRunner<CR>

vmap <LocalLeader>vs "vy :call RunVimTmuxCommand(@v . "\n", 0)<CR>


" Surround
map <Leader>' cs"'
map <Leader>" cs'"

" ========= Utility Shortcuts ========

" Quck git grep
nnoremap <silent> <Leader>gw :GitGrepWord<CR>

" Quick Editing
nnoremap <leader>ev :e $MYVIMRC<cr>
nnoremap <leader>eo :e ~/Dropbox/notes<cr>
nnoremap <leader>es :e ~/.vim/snippets

" " Better comand-line editing
cnoremap <C-j> <t_kd>
cnoremap <C-k> <t_ku>
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap <C-f> <right>
cnoremap <C-b> <left>

" Don't have to use Shift to get into command mode, just hit semicolon
nnoremap ; :

" Insert new lines without going into insert mode
nmap t o<ESC>k
nmap T O<ESC>j

" Clear Search
map <leader>nh :noh<Enter>

" Quickly reload the vimrc file
nmap <silent> <leader>vis :so $MYVIMRC<CR>

" Rebuild Tags
map <silent> <LocalLeader>rt :!ctags -R --exclude=".git\|.svn\|log\|tmp\|db\|pkg" --extra=+f<CR>

" Folding
map <leader>fe :set foldenable<CR>
map <leader>fd :set nofoldenable<CR>

" TagBar
map <Leader>tb :TagbarToggle<CR>

" ========= Insert Shortcuts ========

imap <C-l> <SPACE>=><SPACE>
imap jj <esc>

" ========= Commands ========

command! Note :set laststatus=0 nonumber
command! NoteOff :set laststatus=2 number

" ========= Functions ========

function! GitGrepWord()
  cgetexpr system("git grep -n '" . expand("<cword>") . "'")
  cwin
  echo 'Number of matches: ' . len(getqflist())
endfunction
command! -nargs=0 GitGrepWord :call GitGrepWord()

function! SpecCommand()
  if system("grep 'rspec' Gemfile -s | wc -l") == "0\n"
    return "m"
  else
    return "rspec"
  endif
endfunction

function! RunFocsedTest()
  call VimuxRunCommand("clear;".SpecCommand() . " " . expand("%") . " -l " . line("."))
endfunction

function! RunTests()
  call VimuxRunCommand("clear;".SpecCommand() . " " . expand("%"))
endfunction
