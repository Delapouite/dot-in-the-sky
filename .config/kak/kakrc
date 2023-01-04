colorscheme dracula

set-option global tabstop 2
set-option global indentwidth 2
set-option global scrolloff 5,1
set-option global grepcmd 'rg --column'
set-option global completers filename word=all
set-option global startup_info_version 20420101
set-option -add global matching_pairs ‹ › « » “ ” ‘ ’
set-option -add global ui_options ncurses_assistant=none ncurses_change_colors=true ncurses_status_on_top=true

define-command update-status %{ evaluate-commands %sh{
  printf %s 'set-option buffer modelinefmt %{'
  if [ "$kak_opt_lsp_diagnostic_error_count" -ne 0 ]; then
    printf %s '{DiagnosticError}%opt{lsp_diagnostic_error_count}!{Default} '
  fi
  if [ "$kak_opt_lsp_diagnostic_warning_count" -ne 0 ]; then
    printf %s '{DiagnosticWarning}%opt{lsp_diagnostic_warning_count}!{Default} '
  fi
  printf %s '{{context_info}} {{mode_info}} '
  printf %s ' · %val{bufname} [%opt{filetype}]'
  printf %s ' · %val{cursor_line}:%val{cursor_char_column}/%val{buf_line_count}'
  case "$kak_client" in client*) ;; *) printf %s " · $kak_client";; esac
  case "$kak_session" in ''|*[!0-9]*) printf %s " @$kak_session";; esac
  printf %s '}'
}}
hook global WinDisplay .* update-status
hook global BufSetOption lsp_diagnostic_error_count=.* update-status
hook global BufSetOption lsp_diagnostic_warning_count=.* update-status

# needs those kitty map for disambiguation hack:
# map ctrl+h send_text all \u24D7 // ⓗ
# map ctrl+i send_text all \u24D8 // ⓘ
# map ctrl+j send_text all \u24D9 // ⓙ
# map ctrl+m send_text all \u24DC // ⓜ
# map ctrl+return send_text all \u240D // ␍
# map ctrl+space send_text all \u2420 // ␠

map global normal <c-i> '<tab>'

# <a-k> equivalent is defined afer auto-percent is loaded
map global normal <a-j> ': extend-line-down %val{count}<ret>' -docstring 'extend line down'
map global normal C '<a-j>'                                   -docstring 'join lines'
map global normal <a-C> '<a-J>'                               -docstring 'join lines and select spaces'

map global normal <c-j> 'C'                                   -docstring 'copy selection on next line'
map global normal <c-k> '<a-C>'                               -docstring 'copy selection on previous line'

map global normal <c-l> ']p;'                                 -docstring 'next paragraph'
map global normal <c-h> '[p;'                                 -docstring 'previous paragraph'

map global normal <backspace> ,                               -docstring 'remove all selections except main'
map global normal <a-backspace> <a-,>                         -docstring 'remove main sel'

map global normal . ':'                                       -docstring 'enter command prompt'
map global normal : '.'                                       -docstring 'repeat last insert'

map global normal '#' ': comment-line<ret>'                   -docstring 'comment-line'
map global normal '<a-#>' ': comment-block<ret>'              -docstring 'comment-block'
map global normal '=' ': format<ret>'                         -docstring 'format'

map global insert <a-j> '<a-;>'                               -docstring 'temp escape to normal mode'
map global insert <a-k> '<esc>'                               -docstring 'escape to normal mode'
map global insert <a-p> '<c-r>"'                              -docstring 'paste'

map global goto <lt> <esc><c-o>                               -docstring 'jump backward'
map global goto <gt> <esc><tab>                               -docstring 'jump forward'
map global goto m '<esc>m;'                                   -docstring 'matching next char'
map global goto n '<esc>]p;'                                  -docstring 'next paragraph'
map global goto p '<esc>[p;'                                  -docstring 'previous paragraph'
map global goto <a-m> '<esc><a-m>;'                           -docstring 'matching previous char'

map global user <space> :                                     -docstring 'command (: alias)'
map global user g ': grep '                                   -docstring 'grep'

alias global h doc
alias global u enter-user-mode

hook global InsertChar j %{ try %{
  execute-keys -draft hH <a-k>jj<ret> d
  execute-keys <esc>
}}

hook global InsertChar k %{ try %{
  execute-keys -draft hH <a-k>jk<ret> d
  execute-keys <esc>
  write
}}

hook global InsertCompletionShow .* %{
  map window insert <tab> <c-n>
  map window insert <s-tab> <c-p>
}

hook global InsertCompletionHide .* %{
  unmap window insert <tab> <c-n>
  unmap window insert <s-tab> <c-p>
}

hook global WinSetOption filetype=rust %{
  set-option window formatcmd 'rustfmt'
}

hook global WinSetOption filetype=json %{
  set-option window formatcmd 'jq .'
}

hook global WinSetOption filetype=scss %{
  set-option window formatcmd 'prettier --stdin --parser scss'
}

hook global WinSetOption filetype=javascript %{
  set-option window lintcmd 'eslint --format ~/eslint-kakoune.js'
  set-option window formatcmd 'prettier --no-semi --trailing-comma all --jsx-bracket-same-line --single-quote --stdin --parser babel'
}

hook global WinSetOption filetype=json %{
  set-option window formatcmd 'prettier --stdin --parser json'
}

hook global BufWritePre .* %{ evaluate-commands %sh{
  container=$(dirname "$kak_hook_param")
  test -d "$container" || mkdir --parents "$container"
}}

# display matching char in insert mode (the one before cursor)
# https://github.com/mawww/kakoune/issues/1192#issuecomment-277637280
declare-option -hidden range-specs show_matching_range
hook global InsertChar [[(<{}>)\]] %{
  evaluate-commands -draft %{
    try %{
      execute-keys -no-hooks <esc>\;hm<a-k>..<ret>\;
      set-option window show_matching_range "%val{timestamp} %val{selection_desc}|MatchingChar"
    }

    hook window -once InsertChar [^[(<{}>)\]] %{
      set-option window show_matching_range
    }
  }
}

hook global ModeChange pop:insert:normal %{
  set-option buffer show_matching_range
}

declare-option -hidden regex curword

hook global NormalIdle .* %{
  evaluate-commands -draft -no-hooks %{ try %{
    execute-keys <space><a-i>w <a-k>\A\w+\z<ret>
    set-option window curword "\b\Q%val{selection}\E\b"
  } catch %{
    set-option window curword ''
  }}
}

hook global WinCreate .* %{
  add-highlighter window/ number-lines -hlcursor
  add-highlighter window/ show-whitespaces
  add-highlighter window/trailing-whitespaces regex '\h+$' 0:Error
  add-highlighter window/ dynregex '%opt{curword}' 0:+ub
  add-highlighter window/ show-matching
  add-highlighter window/ ranges show_matching_range
}

source '~/.config/kak/plugins/plug.kak/rc/plug.kak'

plug 'alexherbo2/connect.kak' %{
  map global user r %{:connect lf %val{buffile} <ret>} -docstring 'lf'
}

plug 'alexherbo2/explore.kak'

plug 'alexherbo2/pad-number.kak'

plug 'alexherbo2/split-object.kak' %{
  # <c-space>
  map global normal ␠ ': enter-user-mode split-object<ret>' -docstring 'split object mode'
}

plug 'alexherbo2/volatile-highlighter.kak' %{
  set-face global Volatile +bi
}

plug 'andreyorst/fzf.kak' config %{
  map global user f ': fzf-mode<ret>' -docstring 'fzf'
  } defer 'fzf' %{
  set-option global fzf_file_command 'rg'
  set-option global fzf_highlight_command 'bat'
}

plug 'delapouite/kakoune-auto-percent' %{
  map global normal <a-k> ': extend-line-up %val{count}<ret>'   -docstring 'extend line up'
  map global normal D '<a-k>'                                   -docstring 'keep selections matching regex'
  map global normal <a-D> '<a-K>'                               -docstring 'keep selections not matching regex'
}

plug 'delapouite/kakoune-buffers' %{
  map global normal ^ q
  map global normal <a-^> Q
  map global normal q b
  map global normal Q B
  map global normal <a-q> <a-b>
  map global normal <a-Q> <a-B>
  map global normal b ': enter-buffers-mode<ret>' -docstring 'buffers mode'
  map global normal B ': enter-user-mode -lock buffers<ret>' -docstring 'buffers mode lock'
}

plug 'delapouite/kakoune-cd' %{
  map global user c ': enter-user-mode cd<ret>' -docstring 'cd'
  alias global pwd print-working-directory
}

plug 'delapouite/kakoune-hump' %{
  map global normal « ': select-previous-hump<ret>' -docstring 'select prev hump'
  map global normal » ': select-next-hump<ret>'     -docstring 'select next hump'
  map global normal ‹ ': extend-previous-hump<ret>' -docstring 'extend prev hump'
  map global normal › ': extend-next-hump<ret>'     -docstring 'extend next hump'
}

plug 'delapouite/kakoune-i3' %{
  map global user 3 ': enter-user-mode i3<ret>' -docstring 'i3 mode'
}

plug 'delapouite/kakoune-goto-file' %{
  map global goto f '<esc>: goto-file<ret>' -docstring 'file'
  map global goto F f -docstring 'file (legacy)'
}

plug 'delapouite/kakoune-mirror' %{
  map global normal "'" ': enter-user-mode -lock mirror<ret>' -docstring 'mirror mode lock'
}

plug 'delapouite/kakoune-select-view' %{
  map global view s '<esc>: select-view<ret>' -docstring 'select view'
}

plug 'delapouite/kakoune-text-objects' %{
  map global selectors 's' ': if-cursor s s<ret>'                            -docstring 'auto-percent s'
  map global selectors 'p' ': if-cursor S S<ret>'                            -docstring 'auto-percent S'
  map global selectors '<ret>' ': if-cursor <lt>a-s> <lt>a-s> no-hooks<ret>' -docstring 'auto-percent <a-s>'
  map global selectors 'm' ': if-cursor <lt>a-S> <lt>a-S> no-hooks<ret>'     -docstring 'auto-percent <a-S>'
  map global selectors 'f' ': if-cursor <lt>a-s><lt>a-k> <lt>a-k><ret>'      -docstring 'auto-percent <a-k>'
  map global selectors 'r' ': if-cursor <lt>a-s><lt>a-K> <lt>a-K><ret>'      -docstring 'auto-percent <a-K>'
  map global selectors 'v' ': select-view<ret>'                              -docstring 'select view'
  map global normal s ': enter-user-mode selectors<ret>'                     -docstring 'selectors'
}

plug 'delapouite/kakoune-user-modes' %{
  alias global u enter-user-mode
  map global normal <semicolon> ': enter-user-mode anchor<ret>'   -docstring 'anchor mode'
  map global user s ': enter-user-mode echo<ret>'       -docstring 'echo mode'
  map global user / ': enter-user-mode search<ret>'     -docstring 'search mode'
  map global user l ': enter-user-mode lint<ret>'       -docstring 'lint mode'
  map global user L ': enter-user-mode -lock lint<ret>' -docstring 'lint mode lock'
  map global user k ': enter-user-mode keep<ret>'       -docstring 'keep mode'
  map global user _ ': enter-user-mode trim<ret>'       -docstring 'trim mode'
  map global user = ': enter-user-mode trim<ret>'       -docstring 'format mode'
}

plug 'https://gitlab.com/FlyingWombat/case.kak' %{
  map global normal '`' ': enter-user-mode case<ret>' -docstring 'case mode'
}

plug 'https://gitlab.com/Screwtapello/kakoune-inc-dec' %{
  map global normal <c-a> ': inc-dec-modify-numbers + %val{count}<ret>' -docstring 'increment'
  map global normal <c-x> ': inc-dec-modify-numbers - %val{count}<ret>' -docstring 'decrement'
}

plug "lePerdu/kakboard" %{
  hook global WinCreate .* %{ kakboard-enable }
}

plug 'occivink/kakoune-expand' %{
  map global normal + ': expand<ret>' -docstring 'expand'
}

plug 'occivink/kakoune-phantom-selection' %{
  map global normal æ ': phantom-selection-add-selection<ret>'                       -docstring 'phantom add'
  map global normal ® ': phantom-selection-select-all; phantom-selection-clear<ret>' -docstring 'phantom select'
  map global normal é ': phantom-selection-iterate-prev<ret>'                        -docstring 'phantom prev'
  map global normal è ': phantom-selection-iterate-next<ret>'                        -docstring 'phantom next'
}

plug 'occivink/kakoune-vertical-selection'

plug "ul/kak-lsp" do %{
  cargo install --locked --force --path .
  } %{
  set-option global lsp_cmd "kak-lsp -s %val{session} -vvv --log /tmp/kak-lsp.log"
  hook global WinSetOption filetype=(typescript) %{
    lsp-auto-hover-enable
    lsp-enable-window
    set-option global lsp_hover_anchor true
    map global user l ': enter-user-mode lsp<ret>' -docstring 'lsp'
  }
}

plug "ul/kak-tree" do %{
  cargo install --force --path . --features "css html javascript json typescript"
  } %{
  declare-user-mode tree
  map global tree h ': tree-select-previous-node<ret>' -docstring 'select previous'
  map global tree l ': tree-select-next-node<ret>' -docstring 'select next'
  map global tree k ': tree-select-parent-node<ret>' -docstring 'select parent'
  map global tree j ': tree-select-children<ret>' -docstring 'select children'
  map global tree f ': tree-select-first-child<ret>' -docstring 'select first child'
  map global tree s ': tree-node-sexp<ret>' -docstring 'show info'
  map global user t ': enter-user-mode tree<ret>' -docstring 'tree mode'
  map global normal þ ': enter-user-mode -lock tree<ret>' -docstring 'tree mode lock'
}

define-command -hidden -params 1 extend-line-down %{
  execute-keys "<a-:>%arg{1}X"
}

define-command -hidden  -params 1 extend-line-up %{
  execute-keys "<a-:><a-;>%arg{1}K<a-;>"
  try %{
    execute-keys -draft ';<a-K>\n<ret>'
    execute-keys X
  }
  execute-keys '<a-;><a-X>'
}

