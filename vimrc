set nocompatible
filetype off

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



" Options
" ============

" Basic options
syntax enable
set encoding=utf-8
set scrolloff=3
set autoindent
set showmode
set showcmd
set nopaste
set hidden
set wildmenu
set wildmode=list:longest
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

" Use the same symbols as TextMate for tabstops and EOLs
set listchars=tab:▸\ ,eol:¬,precedes:>,extends:<,nbsp:.,trail:.
set nolist

" Color
set background=dark
colorscheme ir_ben

" File Types
au BufNewFile,BufRead *.less set filetype=less
au BufNewFile,BufRead rebar.config set filetype=erlang
au BufNewFile,BufRead *.mustache        setf mustache
runtime! ftdetect/*.vim

" Folding
set foldmethod=indent
set foldlevel=2
set nofoldenable

" Quick Editing
nnoremap <leader>ev :e $MYVIMRC<cr>
nnoremap <leader>eo :e ~/Dropbox/notes<cr>
nnoremap <leader>es :e ~/.vim/snippets

" Better comand-line editing
cnoremap <C-j> <t_kd>
cnoremap <C-k> <t_ku>
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap <C-f> <right>
cnoremap <C-b> <left>

" Make Y behave like other capitals
map Y y$

" Easier split navigation
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

" Don't have to use Shift to get into command mode, just hit semicolon
nnoremap ; :

" Insert new lines without going into insert mode
nmap t o<ESC>k
nmap T O<ESC>j



" Plugin Options
" ============

" Conque
let g:ConqueTerm_ReadUnfocused = 1
let g:ConqueTerm_InsertOnEnter = 0

" CommandT
let g:CommandTMaxHeight=10

" NERDTree
let NERDTreeShowFiles=1
let NERDTreeShowHidden=1
let NERDTreeMinimalUI=1
let NERDTreeDirArrows=1
let NERDTreeQuitOnOpen=1
let NERDTreeIgnore=['.DS_Store']

" Surrond
let g:surround_{char2nr("t")} = "<\1\r..*\r&\1>\r</\1\r..*\r&\1>"

" CamelCaseMotion
map <silent> w <Plug>CamelCaseMotion_w
map <silent> b <Plug>CamelCaseMotion_b
map <silent> e <Plug>CamelCaseMotion_e
sunmap w
sunmap b
sunmap e

" Note.vim
let g:notes_suffix = ".txt"
let g:notes_title_sync = "change_title"
let g:notes_directory = "~/Dropbox/notes"
let g:notes_list_bullets = ['*', '◦', '▸', '▹', '▪', '▫']
highlight notesAtxHeading ctermfg=blue
highlight notesAtxHeading ctermfg=blue
highlight notesTitle ctermfg=magenta ctermbg=none

" Vimerl
let erlang_show_errors = 0

" Powerline
let g:powerline_cache_file = "~/.vim"


" Shortcuts
" ============

" Alternate Files
map <leader>aa :A<CR>
map <leder>as :AS<CR>
map <leder>av :AV<CR>

" Buffers
map <leader>bb :e#<CR>
map <leader>be :BufExplorer<Enter>

" Clear Search
map <leader>/ :noh<Enter>

" CommandT
map <leader>ff :CommandT<Enter>
map <leader>fb :CommandTBuffer<Enter>
map <leader>fr :CommandTFlush<Enter>
nnoremap <leader>fw <C-w>v<C-w>l:CommandT<Enter>

" NERDTree
map <leader>nt :NERDTreeToggle<Enter>
map <leader>nf :NERDTreeFind<Enter>

" TComment
map <leader>cc :TComment<CR>

" Ack
map <leader>ak :Ack

" Quickly reload the vimrc file
nmap <silent> <leader>vis :so $MYVIMRC<CR>

" Rebuild Tags
map <silent> <LocalLeader>rt :!ctags -R --exclude=".git\|.svn\|log\|tmp\|db\|pkg" --extra=+f<CR>

" Ruby Focused Unit Test
" map <leader>rb :call RunVimTmuxCommand("clear && rspec " . bufname("%"))<CR>
" map <leader>rf :call RunVimTmuxCommand("clear && rspec " . bufname("%") . " -l " . line("."))<CR>
" map <leader>rx :CloseVimTmuxWindows<CR>
" map<leader>rl :RunLastVimTmuxCommand<CR>
" map <leader>rf :RunRubyFocusedUnitTest<CR>
" map <leader>rc :RunRubyFocusedContext<CR>
" map <leader>rb :RunAllRubyTests<CR>
" map <leader>rl :RunLastRubyTest<CR>

" Folding
map <leader>fe :set foldenable<CR>
map <leader>fd :set nofoldenable<CR>

" TagBar
map <Leader>tb :TagbarToggle<CR>

" Surrount
map <Leader>' cs"'
map <Leader>" cs'"

" Create window splits easier. The default
" " way is Ctrl-w,v and Ctrl-w,s. I remap
" " this to vv and ss
nnoremap <silent> vv <C-w>v
nnoremap <silent> ss <C-w>s



" Insert Shortcuts
" ============

imap <C-l> <SPACE>=><SPACE>

function! Strip(str)
  return substitute(substitute(substitute(a:str, '^\s*\(.\{-}\)\s*$', '\1', ''), '\n\n', '\1', ''), '\n$', '\1', '')
endfunction
