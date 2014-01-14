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

" autocmd VimEnter,BufNewFile,BufReadPost * silent! call HardMode()

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
set wildmode=longest,list
set wildmenu
set visualbell
set ttyfast
set ruler
set number
set backspace=indent,eol,start
set laststatus=2                  " Show the status line all the time
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
colorscheme base16-ocean
set background=dark

" File Types
au BufNewFile,BufRead *.less set filetype=less
au BufNewFile,BufRead rebar.config set filetype=erlang
au BufNewFile,BufRead *.mustache setf mustache
au BufNewFile,BufRead *.txt set filetype=markdown

autocmd FileType mail set spell
autocmd FileType mail call search("^$")
autocmd Filetype mail highlight ExtraWhitespace ctermbg=none guibg=none
autocmd Filetype go setlocal textwidth=0 nosmartindent tabstop=2 shiftwidth=2 softtabstop=2 noexpandtab

runtime! ftdetect/*.vim

" Folding
set foldmethod=indent
set foldlevel=2
set nofoldenable

" Go
let go_highlight_trailing_whitespace_error = 0
autocmd Filetype go setlocal textwidth=0 nosmartindent tabstop=2 shiftwidth=2 softtabstop=2 noexpandtab
au FileType go au BufWritePre <buffer> Fmt

" ========= Plugin Options ========

" Ctrlp
let g:ctrlp_max_files = 0
let g:ctrlp_user_command = {
  \ 'types': {
    \ 1: ['.git', 'cd %s && git ls-files . -co --exclude-standard'],
    \ },
  \ 'fallback': 'find %s -type f'
  \ }

" Surrond
"let g:surround_{char2nr("t")} = "<\1\r..*\r&\1>\r</\1\r..*\r&\1>"

" Vimerl
let erlang_show_errors = 0

" Vimux
let VimuxUseNearestPane = 1
let g:VimuxOrientation = "h"
let g:VimuxHeight = "40"

" netrw
let g:netrw_banner = 0
let g:netrw_bufsettings = 'noma nomod nu nowrap ro nobl'

"

" ========= Navigation Shortcuts ========

" Buffers
noremap <leader>be :EasyBuffer<CR>
noremap <leader>bs <C-w>s:EasyBuffer<CR>
noremap <leader>bv <C-w>v:EasyBuffer<CR>


" CommandT
map <leader>ff :CtrlP<Enter>

" ========= Coding Shortcuts ========

" Make Y behave like other capitals
map Y y$

" TComment
map <leader>cc :TComment<CR>

" Prompt for a command to run
map <Leader>vp :VimuxPromptCommand<CR>

" Run last command executed by RunVimTmuxCommand
map <Leader>rl :call _RunLast()<CR>

" Inspect runner pane
map <Leader>vi :VimuxInspectRunner<CR>

" Close all other tmux panes in current window
map <Leader>vx :VimuxCloseRunner<CR>

" Interrupt any command running in the runner pane
map <Leader>ve :VimuxInterruptRunner<CR>

vmap <LocalLeader>vs "vy :call VimuxSendText(@v)<CR>:call VimuxSendKeys("Enter")<CR>

" Surround
"map <Leader>' cs"'
"map <Leader>" cs'"

" ========= Utility Shortcuts ========

" Quck git grep
nnoremap <silent> <Leader>gw :GitGrepWord<CR>

" Better comand-line editing
cnoremap <C-j> <t_kd>
cnoremap <C-k> <t_ku>
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap <C-f> <right>
cnoremap <C-b> <left>

" Clear Search
map <leader>nh :noh<Enter>

" Quickly reload the vimrc file
nmap <silent> <leader>vis :so $MYVIMRC<CR>

" Rebuild Tags
map <silent> <LocalLeader>rt :!ctags -R --exclude=".git\|.svn\|log\|tmp\|db\|pkg" --extra=+f<CR>

" netrw
map <Leader>nf :e%:h<CR>

" clear whitespace
map <Leader>cw :%s/\s\+$//g<CR>

" ========= Insert Shortcuts ========

imap <C-l> <SPACE>=><SPACE>

" ========= Commands ========

command! Note :set laststatus=0 nonumber
command! NoteOff :set laststatus=2 number
command! NERDTreeToggle :e.

" ========= Functions ========

function! GitGrepWord()
  cgetexpr system("git grep -n '" . expand("<cword>") . "'")
  cwin
  echo 'Number of matches: ' . len(getqflist())
endfunction
command! -nargs=0 GitGrepWord :call GitGrepWord()

function! _IsInferiorSlimeRunning()
  if system("ps axo command | grep inferior-slime | grep -v grep") == ""
    return 0
  else
    return 1
  end
endfunction

function! _RunLast()
  if _IsInferiorSlimeRunning()
    execute "InferiorSlimeSpecLast"
  else
    execute "VimuxRunLastCommand"
  endif
endfunction

function! TmuxAwareNavigate(direction)
  let nr = winnr()
  let tmux_last_pane = (a:direction == 'p' && s:tmux_is_last_pane)
  if !tmux_last_pane
    " try to switch windows within vim
    exec 'wincmd ' . a:direction
  endif
  " Forward the switch panes command to tmux if:
  " a) we're toggling between the last tmux pane;
  " b) we tried switching windows in vim but it didn't have effect.
  if tmux_last_pane || nr == winnr()
    let cmd = 'tmux select-pane -' . tr(a:direction, 'phjkl', 'lLDUR')
    silent call system(cmd)
    let s:tmux_is_last_pane = 1
  else
    let s:tmux_is_last_pane = 0
  endif
endfunction
