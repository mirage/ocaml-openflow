if &cp | set nocp | endif
let s:cpo_save=&cpo
set cpo&vim
inoremap <silent> <Plug>(neocomplcache_start_omni_complete) 
inoremap <silent> <Plug>(neocomplcache_start_auto_complete_no_select) 
inoremap <silent> <Plug>(neocomplcache_start_auto_complete) =neocomplcache#mappings#popup_post()
inoremap <silent> <expr> <Plug>(neocomplcache_start_unite_quick_match) unite#sources#neocomplcache#start_quick_match()
inoremap <silent> <expr> <Plug>(neocomplcache_start_unite_complete) unite#sources#neocomplcache#start_complete()
inoremap <expr> <BS> neocomplcache#smart_close_popup()."\"
noremap  h
nmap 	 :NERDTreeToggle
noremap <NL> j
noremap  k
noremap  l
nmap OD h
nmap OC l
nmap OB j
nmap OA k
vnoremap = :call OcpIndentRange()
map == :call OcpIndentRange()
xmap S <Plug>VSurround
nmap [xx <Plug>unimpaired_line_xml_encode
xmap [x <Plug>unimpaired_xml_encode
nmap [x <Plug>unimpaired_xml_encode
nmap [uu <Plug>unimpaired_line_url_encode
xmap [u <Plug>unimpaired_url_encode
nmap [u <Plug>unimpaired_url_encode
nmap [yy <Plug>unimpaired_line_string_encode
xmap [y <Plug>unimpaired_string_encode
nmap [y <Plug>unimpaired_string_encode
nmap [p <Plug>unimpairedPutAbove
nnoremap [ox :set cursorline cursorcolumn
nnoremap [ow :set wrap
nnoremap [os :set spell
nnoremap [or :set relativenumber
nnoremap [on :set number
nnoremap [ol :set list
nnoremap [oi :set ignorecase
nnoremap [oh :set hlsearch
nnoremap [od :diffthis
nnoremap [ou :set cursorcolumn
nnoremap [oc :set cursorline
xmap [e <Plug>unimpairedMoveUp
nmap [e <Plug>unimpairedMoveUp
nmap [  <Plug>unimpairedBlankUp
omap [n <Plug>unimpairedContextPrevious
nmap [n <Plug>unimpairedContextPrevious
nmap [o <Plug>unimpairedOPrevious
nmap [f <Plug>unimpairedDirectoryPrevious
nmap <silent> [T <Plug>unimpairedTFirst
nmap <silent> [t <Plug>unimpairedTPrevious
nmap <silent> [ <Plug>unimpairedQPFile
nmap <silent> [Q <Plug>unimpairedQFirst
nmap <silent> [q <Plug>unimpairedQPrevious
nmap <silent> [ <Plug>unimpairedLPFile
nmap <silent> [L <Plug>unimpairedLFirst
nmap <silent> [l <Plug>unimpairedLPrevious
nmap <silent> [B <Plug>unimpairedBFirst
nmap <silent> [b <Plug>unimpairedBPrevious
nmap <silent> [A <Plug>unimpairedAFirst
nmap <silent> [a <Plug>unimpairedAPrevious
nnoremap \i :call OcpIndentBuffer()
vnoremap \i :call OcpIndentRange()
vmap <silent> \t :call Ocaml_print_type("visual")`<
nmap <silent> \t :call Ocaml_print_type("normal")
omap <silent> \t :call Ocaml_print_type("normal")
map \S :call OCaml_switch(1)
map \s :call OCaml_switch(0)
nmap \\u <Plug>CommentaryUndo
nmap \\\ <Plug>CommentaryLine
nmap \\ <Plug>Commentary
xmap \\ <Plug>Commentary
map \bta :!/usr/local/bin/ctags -R .
map \ta :TlistToggle
nmap ]xx <Plug>unimpaired_line_xml_decode
xmap ]x <Plug>unimpaired_xml_decode
nmap ]x <Plug>unimpaired_xml_decode
nmap ]uu <Plug>unimpaired_line_url_decode
xmap ]u <Plug>unimpaired_url_decode
nmap ]u <Plug>unimpaired_url_decode
nmap ]yy <Plug>unimpaired_line_string_decode
xmap ]y <Plug>unimpaired_string_decode
nmap ]y <Plug>unimpaired_string_decode
nmap ]p <Plug>unimpairedPutBelow
nnoremap ]ox :set nocursorline nocursorcolumn
nnoremap ]ow :set nowrap
nnoremap ]os :set nospell
nnoremap ]or :set norelativenumber
nnoremap ]on :set nonumber
nnoremap ]ol :set nolist
nnoremap ]oi :set noignorecase
nnoremap ]oh :set nohlsearch
nnoremap ]od :diffoff
nnoremap ]ou :set nocursorcolumn
nnoremap ]oc :set nocursorline
xmap ]e <Plug>unimpairedMoveDown
nmap ]e <Plug>unimpairedMoveDown
nmap ]  <Plug>unimpairedBlankDown
omap ]n <Plug>unimpairedContextNext
nmap ]n <Plug>unimpairedContextNext
nmap ]o <Plug>unimpairedONext
nmap ]f <Plug>unimpairedDirectoryNext
nmap <silent> ]T <Plug>unimpairedTLast
nmap <silent> ]t <Plug>unimpairedTNext
nmap <silent> ] <Plug>unimpairedQNFile
nmap <silent> ]Q <Plug>unimpairedQLast
nmap <silent> ]q <Plug>unimpairedQNext
nmap <silent> ] <Plug>unimpairedLNFile
nmap <silent> ]L <Plug>unimpairedLLast
nmap <silent> ]l <Plug>unimpairedLNext
nmap <silent> ]B <Plug>unimpairedBLast
nmap <silent> ]b <Plug>unimpairedBNext
nmap <silent> ]A <Plug>unimpairedALast
nmap <silent> ]a <Plug>unimpairedANext
nnoremap cox :set =&cursorline && &cursorcolumn ? 'nocursorline nocursorcolumn' : 'cursorline cursorcolumn'
nnoremap cod :=&diff ? 'diffoff' : 'diffthis'
nmap cs <Plug>Csurround
nmap ds <Plug>Dsurround
nmap gx <Plug>NetrwBrowseX
nmap gcu <Plug>CommentaryUndo
nmap gcc <Plug>CommentaryLine
nmap gc <Plug>Commentary
xmap gc <Plug>Commentary
xmap gS <Plug>VgSurround
nmap ySS <Plug>YSsurround
nmap ySs <Plug>YSsurround
nmap yss <Plug>Yssurround
nmap yS <Plug>YSurround
nmap ys <Plug>Ysurround
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#NetrwBrowseX(expand("<cWORD>"),0)
nnoremap <silent> <Plug>(vimshell_create) :VimShellCreate
nnoremap <silent> <Plug>(vimshell_switch) :VimShell
xnoremap <silent> <Plug>unimpairedMoveDown :exe 'exe "normal! m`"|''<,''>move''>+'.v:count1``
xnoremap <silent> <Plug>unimpairedMoveUp :exe 'exe "normal! m`"|''<,''>move--'.v:count1``
nmap <silent> <Plug>unimpairedOPrevious <Plug>unimpairedDirectoryPrevious:echohl WarningMSG|echo "[o is deprecated. Use [f"|echohl NONE
nmap <silent> <Plug>unimpairedONext <Plug>unimpairedDirectoryNext:echohl WarningMSG|echo "]o is deprecated. Use ]f"|echohl NONE
nnoremap <silent> <Plug>unimpairedTLast :exe "".(v:count ? v:count : "")."tlast"
nnoremap <silent> <Plug>unimpairedTFirst :exe "".(v:count ? v:count : "")."tfirst"
nnoremap <silent> <Plug>unimpairedTNext :exe "".(v:count ? v:count : "")."tnext"
nnoremap <silent> <Plug>unimpairedTPrevious :exe "".(v:count ? v:count : "")."tprevious"
nnoremap <silent> <Plug>unimpairedQNFile :exe "".(v:count ? v:count : "")."cnfile"
nnoremap <silent> <Plug>unimpairedQPFile :exe "".(v:count ? v:count : "")."cpfile"
nnoremap <silent> <Plug>unimpairedQLast :exe "".(v:count ? v:count : "")."clast"
nnoremap <silent> <Plug>unimpairedQFirst :exe "".(v:count ? v:count : "")."cfirst"
nnoremap <silent> <Plug>unimpairedQNext :exe "".(v:count ? v:count : "")."cnext"
nnoremap <silent> <Plug>unimpairedQPrevious :exe "".(v:count ? v:count : "")."cprevious"
nnoremap <silent> <Plug>unimpairedLNFile :exe "".(v:count ? v:count : "")."lnfile"
nnoremap <silent> <Plug>unimpairedLPFile :exe "".(v:count ? v:count : "")."lpfile"
nnoremap <silent> <Plug>unimpairedLLast :exe "".(v:count ? v:count : "")."llast"
nnoremap <silent> <Plug>unimpairedLFirst :exe "".(v:count ? v:count : "")."lfirst"
nnoremap <silent> <Plug>unimpairedLNext :exe "".(v:count ? v:count : "")."lnext"
nnoremap <silent> <Plug>unimpairedLPrevious :exe "".(v:count ? v:count : "")."lprevious"
nnoremap <silent> <Plug>unimpairedBLast :exe "".(v:count ? v:count : "")."blast"
nnoremap <silent> <Plug>unimpairedBFirst :exe "".(v:count ? v:count : "")."bfirst"
nnoremap <silent> <Plug>unimpairedBNext :exe "".(v:count ? v:count : "")."bnext"
nnoremap <silent> <Plug>unimpairedBPrevious :exe "".(v:count ? v:count : "")."bprevious"
nnoremap <silent> <Plug>unimpairedALast :exe "".(v:count ? v:count : "")."last"
nnoremap <silent> <Plug>unimpairedAFirst :exe "".(v:count ? v:count : "")."first"
nnoremap <silent> <Plug>unimpairedANext :exe "".(v:count ? v:count : "")."next"
nnoremap <silent> <Plug>unimpairedAPrevious :exe "".(v:count ? v:count : "")."previous"
nnoremap <silent> <Plug>SurroundRepeat .
nnoremap <F4> :call UpdateTags()
inoremap <expr>  neocomplcache#cancel_popup()
imap S <Plug>ISurround
imap s <Plug>Isurround
inoremap <expr>  neocomplcache#undo_completion()
inoremap <expr>  neocomplcache#smart_close_popup()."\"
inoremap <expr> 	 pumvisible() ? "\" : "\	"
inoremap <expr>  neocomplcache#complete_common_string()
imap  <Plug>Isurround
inoremap <expr>  neocomplcache#close_popup()
imap OD hi
imap OC li
imap OB ji
imap OA ki
let &cpo=s:cpo_save
unlet s:cpo_save
set autoindent
set autoread
set backspace=2
set completefunc=neocomplcache#complete#manual_complete
set completeopt=menuone
set noequalalways
set expandtab
set fileencodings=ucs-bom,utf-8,default,latin1
set helplang=en
set history=50
set hlsearch
set ignorecase
set incsearch
set laststatus=2
set omnifunc=merlin#Complete
set printoptions=paper:a4
set ruler
set runtimepath=~/.vim/bundle/vundle,~/.vim/bundle/vim-pathogen,~/.vim/bundle/vim-colors-solarized,~/.vim/bundle/vim-fugitive,~/.vim/bundle/nerdtree,~/.vim/bundle/vim-surround,~/.vim/bundle/tabular,~/.vim/bundle/vim-unimpaired,~/.vim/bundle/vim-endwise,~/.vim/bundle/syntastic,~/.vim/bundle/gist-vim,~/.vim/bundle/grep.vim,~/.vim/bundle/vim-commentary,~/.vim/bundle/minibufexpl.vim,~/.vim/bundle/OmniCppComplete,~/.vim/bundle/c.vim,~/.vim/bundle/vim-diff,~/.vim/bundle/detectindent,~/.vim/bundle/vimshell.vim,~/.vim/bundle/neocomplcache,~/.vim,/var/lib/vim/addons,/usr/share/vim/vimfiles,/usr/share/vim/vim73,/usr/share/vim/vimfiles/after,/var/lib/vim/addons/after,~/.vim/after,~/.vim/bundle/vundle/,~/.vim/bundle/vundle/after,~/.vim/bundle/vim-pathogen/after,~/.vim/bundle/vim-colors-solarized/after,~/.vim/bundle/vim-fugitive/after,~/.vim/bundle/nerdtree/after,~/.vim/bundle/vim-surround/after,~/.vim/bundle/tabular/after,~/.vim/bundle/vim-unimpaired/after,~/.vim/bundle/vim-endwise/after,~/.vim/bundle/syntastic/after,~/.vim/bundle/gist-vim/after,~/.vim/bundle/grep.vim/after,~/.vim/bundle/vim-commentary/after,~/.vim/bundle/minibufexpl.vim/after,~/.vim/bundle/OmniCppComplete/after,~/.vim/bundle/c.vim/after,~/.vim/bundle/vim-diff/after,~/.vim/bundle/detectindent/after,~/.vim/bundle/vimshell.vim/after,~/.vim/bundle/neocomplcache/after,~/.opam/4.00.1/share/ocamlmerlin/vim,~/.opam/4.00.1/share/ocamlmerlin/vimbufsync
set shiftwidth=2
set smartcase
set smarttab
set softtabstop=2
set statusline=\ %{HasPaste()}%<%-15.25(%f%)%m%r%h\ %w\ \ \ \ \ [%{&ff}/%Y]\ \ \ %<%20.30(%{hostname()}:%{CurDir()}%)\ %=%-10.(%l,%c%V%)\ %p%%/%L
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
set textwidth=80
set timeoutlen=500
set title
set titlestring=%F\ -\ Vim
set updatetime=1500
set wildignore=*.o,*.class,*.pdf,*._aux,*.aux,*.bbl,*.big,*.brf,*.blg,*.dvi,*.div,*.ilg,*.lof,*.log,*._log,*.nlo,*.nls,*.out,*.ps,*.tdo,*.tex.project.vim,*.toc
" vim: set ft=vim :
