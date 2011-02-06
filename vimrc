set nocompatible

filetype off

" Use Pathogen to load bundles
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

filetype plugin indent on

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

" Basic options
syntax enable
set encoding=utf-8
set scrolloff=3
set autoindent
set showmode
set showcmd
set hidden
set wildmenu
set wildmode=list:longest
set visualbell
set ttyfast
set ruler
set number
set backspace=indent,eol,start
set laststatus=2                  " Show the status line all the time
set cursorline

" Useful status information at bottom of screen
set statusline=[%n]\ %<%.99f\ %h%w%m%r%y\ %{exists('*CapsLockStatusline')?CapsLockStatusline():''}%=%-16(\ %l,%c-%v\ %)%P

" Scroll the viewport faster
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

" Leader
let mapleader = ","

" Searching
set ignorecase
set smartcase
set incsearch
set showmatch
set hlsearch
set gdefault
runtime macros/matchit.vim

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
colorscheme ir_black
if has("gui_running")
	colorscheme liquidcarbon
  
endif

" Clear Search
map <leader><space> :noh<Enter>

" Flex syntax
autocmd BufRead *.as set filetype=actionscript 
autocmd BufRead *.mxml set filetype=mxml 

" mustace!
runtime! ftdetect/*.vim
au BufNewFile,BufRead *.mustache        setf mustache

" Working with windows
nnoremap <leader>w <C-w>v<C-w>l:FuzzyFinderTextMate<Enter>

" Close window
nnoremap <leader>cw <C-w>c

" Black hole
map <leader>b "_d

" Taglist
map <leader>tl :Tlist<Enter>
map <leader>to :TlistOpen<Enter>/

" Terminal
map <leader>tev :ConqueTermVSplit zsh<CR>
map <leader>teh :ConqueTermSplit zsh<CR>
map <leader>te :ConqueTerm zsh<CR>
map <leader>rk :ConqueTermSplit zsh<CR>rake<Enter>

" FuzzyFinder
map <leader>f :FuzzyFinderTextMate<Enter>
map <leader>t :FuzzyFinderTextMate <c-r>=expand("<cword>")<CR><Enter>

" NERDTree
map <leader>nt :NERDTreeToggle<Enter>
let NERDTreeShowFiles=1
let NERDTreeShowHidden=1

" Yankring
map <leader>y :YRShow<cr>
let g:yankring_history_dir = "~/.vim/"

" BufferExplorer
map <leader>e :BufExplorer<Enter>

" Ack
map <leader>ak :Ack 
map <leader>aw :Ack <c-r>=expand("<cword>")<CR>

" Fix stupid window line breaks
map <leader>stupidwindow :%s/\r/\r/g<enter>

" Quickly edit/reload the vimrc file
nmap <silent> <leader>vie :e $MYVIMRC<CR>
nmap <silent> <leader>vis :so $MYVIMRC<CR>

" Use the damn hjkl keys
map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>

" IndentGuide
let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_guide_size = 1

" shift+arrow for better window nav
noremap <C-J> <C-W>j
noremap <C-K> <C-W>k
noremap <C-H> <C-W>h
noremap <C-L> <C-W>l
noremap <C-Down>  <C-W>j
noremap <C-Up>    <C-W>k
noremap <C-Left>  <C-W>h
noremap <C-Right> <C-W>l

" Faster Esc
inoremap jj <ESC>

" Save when losing focus
au FocusLost * :wa

" Adding tag support for other langs
let tlist_actionscript_settings = 'actionscript;c:class;f:method;p:property;v:variable'

" one less keystroke...
nnoremap ; :

" Hide MacVim menubar
if has("gui_running")
    set guioptions=ic
    set guioptions-=m  "remove menu bar
    set guioptions-=r  "remove right-hand scroll bar
endif

" Set font
set guifont="Meslo LG M DZ":h10
set antialias
set linespace=4

" Continue ConqueTerm shell when it's not the current, focused buffer
let g:ConqueTerm_ReadUnfocused = 1

" Django Surrond
let g:surround_{char2nr("b")} = "{% block\1 \r..*\r &\1%}\r{% endblock %}"
let g:surround_{char2nr("i")} = "{% if\1 \r..*\r &\1%}\r{% endif %}"
let g:surround_{char2nr("w")} = "{% with\1 \r..*\r &\1%}\r{% endwith %}"
let g:surround_{char2nr("c")} = "{% comment\1 \r..*\r &\1%}\r{% endcomment %}"
let g:surround_{char2nr("f")} = "{% for\1 \r..*\r &\1%}\r{% endfor %}"

" Trying out arrow keys as 'text shifters'
function! DelEmptyLineAbove()
    if line(".") == 1
        return
    endif
    let l:line = getline(line(".") - 1)
    if l:line =~ '^\s*$'
        let l:colsave = col(".")
        .-1d
        silent normal! <C-y>
        call cursor(line("."), l:colsave)
    endif
endfunction
 
function! AddEmptyLineAbove()
    let l:scrolloffsave = &scrolloff
    " Avoid jerky scrolling with ^E at top of window
    set scrolloff=0
    call append(line(".") - 1, "")
    if winline() != winheight(0)
        silent normal! <C-e>
    endif
    let &scrolloff = l:scrolloffsave
endfunction
 
function! DelEmptyLineBelow()
    if line(".") == line("$")
        return
    endif
    let l:line = getline(line(".") + 1)
    if l:line =~ '^\s*$'
        let l:colsave = col(".")
        .+1d
        ''
        call cursor(line("."), l:colsave)
    endif
endfunction
 
function! AddEmptyLineBelow()
    call append(line("."), "")
endfunction
 
" Arrow key remapping: Up/Dn = move line up/dn; Left/Right = indent/unindent
function! SetArrowKeysAsTextShifters()
    " normal mode
    nmap <silent> <Left> <<
    nmap <silent> <Right> >>
    nnoremap <silent> <Up> <Esc>:call DelEmptyLineAbove()<CR>
    nnoremap <silent> <Down>  <Esc>:call AddEmptyLineAbove()<CR>
 
    " visual mode
    vmap <silent> <Left> <
    vmap <silent> <Right> >
    vnoremap <silent> <Up> <Esc>:call DelEmptyLineAbove()<CR>gv
    vnoremap <silent> <Down>  <Esc>:call AddEmptyLineAbove()<CR>gv
    vnoremap <silent> <C-Up> <Esc>:call DelEmptyLineBelow()<CR>gv
    vnoremap <silent> <C-Down> <Esc>:call AddEmptyLineBelow()<CR>gv
endfunction
 
call SetArrowKeysAsTextShifters()

" vim-LaText
let g:tex_flavor='latex'

" Change status line color when in insert
" first, enable status line always
set laststatus=2
