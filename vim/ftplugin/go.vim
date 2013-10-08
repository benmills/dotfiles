setlocal omnifunc=gocomplete#Complete
map <silent> <LocalLeader>rb :wa<CR> :GolangTestCurrentPackage<CR>
map <silent> <LocalLeader>rf :wa<CR> :GolangTestFocused<CR>
