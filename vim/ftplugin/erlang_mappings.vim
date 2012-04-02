map <Leader>rc :call TmuxCompileErlang()<CR>
map <Leader>rf :call TmuxRunErlangFunction()<CR>

command! Erl :call RunVimTmuxCommand("erl")

function! TmuxCompileErlang()
  let modname = split(bufname("%"), '\.')[0]
  call RunVimTmuxCommand("c(" . modname . ").")
endfunction

function! TmuxRunErlangFunction()
  let l:modname = split(bufname("%"), '\.')[0]
  let l:funname = expand("<cword>")
  let l:fun = modname . ":" . funname
  let l:curline = getline('.')
  let l:args = input('Args for ' . fun . ': ')
  let l:tmfun = fun . "(" . args . ")."

  call RunVimTmuxCommand(tmfun)
endfunction
