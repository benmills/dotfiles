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
map <silent> <LocalLeader>ws :highlight clear ExtraWhitespace<CR>



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
set gdefault

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
colorscheme pure

" File Types
autocmd BufRead *.as set filetype=actionscript
autocmd BufRead *.mxml set filetype=mxml
au BufNewFile,BufRead *.less set filetype=less
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



" Plugin Options
" ============

" CommandT
let g:CommandTMaxHeight=10

" NERDTree
let NERDTreeShowFiles=1
let NERDTreeShowHidden=1
let NERDTreeIgnore=['.DS_Store']

" Surrond
let g:surround_{char2nr("t")} = "<\1\r..*\r&\1>\r</\1\r..*\r&\1>"

" Notes
let g:notes_directory = "~/Dropbox/notes"
highlight notesAtxHeading ctermfg=blue



" Shortcuts
" ============

" Last buffer
map <leader>bb :e#<CR>

" Clear Search
map <leader>nh :noh<Enter>

" Working with windows
nnoremap <leader>w <C-w>v<C-w>l:CommandT<Enter>

" CommandT
map <C-f> :CommandT<Enter>
map <leader>fb :CommandTBuffer<Enter>

" NERDTree
map <leader>nt :NERDTreeToggle<Enter>

" TComment
map <leader>cc :TComment<CR>

" BufferExplorer
map <leader>e :BufExplorer<Enter>

" Ack
map <leader>ak :Ack

" Quickly reload the vimrc file
nmap <silent> <leader>vis :so $MYVIMRC<CR>

" Rebuild Tags
map <silent> <LocalLeader>rt :!ctags -R --exclude=".git\|.svn\|log\|tmp\|db\|pkg" --extra=+f<CR>

" Ruby Focused Unit Test
map <leader>rf :RunRubyFocusedUnitTest<CR>
map <leader>rc :RunRubyFocusedContext<CR>
map <leader>rb :RunAllRubyTests<CR>
map <leader>rl :RunLastRubyTest<CR>

" Folding
map <leader>fe :set foldenable<CR>
map <leader>fd :set nofoldenable<CR>

" TagBar
map <Leader>tb :TagbarToggle<CR>

" Compile erlang
map <Leader>ce :call CompileErlang()<CR>
"map <Leader>re b"fyw/module<CR>f("myi(:call RunErlangFunction("<C-r>m:<C-r>f")<CR>
map <Leader>re :call RunErlangFunction()<CR>

function! RunErlangFunction()
  let modname = split(bufname("%"), '\.')[0]
  let funname = expand("<cword>")
  let fun = modname . ":" . funname
  let curline = getline('.')
  call inputsave()
  let args = input('Args for ' . fun . ': ', "[]")
endfunction

function! OldRunErlangFunction(fun)
  let curline = getline('.')
  call inputsave()
  let args = input('Args for ' . a:fun . ': ', "[]")
  call Send_to_Tmux(a:fun . "(" . args . ").\n")
endfunction

function! CompileErlang()
  call setqflist([])
  let l:result = system("erlc " . bufname("%"))
  if l:result == ""
    echo "Ok"
    exec ":cclose"
  else
    cexpr l:result
    copen
  endif
endfunction
