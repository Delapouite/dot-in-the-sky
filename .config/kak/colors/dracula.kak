# dracula theme
# https://draculatheme.com/

evaluate-commands %sh{
    black="rgb:282a36"
    gray="rgb:44475a"
    white="rgb:f8f8f2"

    pink="rgb:ff79c6"
    purple="rgb:bd93f9"
    blue="rgb:6272a4"
    cyan="rgb:8be9fd"
    green="rgb:50fa7b"
    yellow="rgb:f1fa8c"
    orange="rgb:ffb86c"
    red="rgb:ff5555"

    echo "
         face global value $green
         face global type $purple
         face global variable $red
         face global function $red
         face global module $red
         face global string $yellow
         face global error $red
         face global keyword $cyan
         face global operator $orange
         face global attribute $pink
         face global comment $blue+i
         face global meta $red
         face global builtin $white+b

         face global title $red
         face global header $orange
         face global bold $pink
         face global italic $purple
         face global mono $green
         face global block $cyan
         face global link $green
         face global bullet $green
         face global list $white

         face global Default $white,$black

         face global PrimarySelection $black,$pink
         face global PrimaryCursor $black,$cyan
         face global PrimaryCursorEol $black,$cyan

         face global SecondarySelection $black,$purple
         face global SecondaryCursor $black,$orange
         face global SecondaryCursorEol $black,$orange

         face global MatchingChar $black,$blue
         face global Search $blue,$green
         face global CurrentWord $white,$blue

         # listchars
         face global Whitespace $gray,$black+f
         # ~ lines at EOB
         face global BufferPadding $gray,$black
         # must use wrap -marker hl
         face global WrapMarker Whitespace

         face global LineNumbers $gray,$black
         # must use -hl-cursor
         face global LineNumberCursor $white,$gray+b
         face global LineNumbersWrapped $gray,$black+i

         # when item focused in menu
         face global MenuForeground $blue,$white+b
         # default bottom menu and autocomplete
         face global MenuBackground $white,$blue
         # complement in autocomplete like path
         face global MenuInfo $cyan,$blue
         # clippy
         face global Information $yellow,$gray
         face global Error $black,$red

         # all status line: what we type, but also client@[session]
         face global StatusLine $white,$black
         # insert mode, prompt mode
         face global StatusLineMode $black,$green
         # message like '1 sel'
         face global StatusLineInfo $purple,$black
         # count
         face global StatusLineValue $orange,$black
         face global StatusCursor $white,$blue
         # like the word 'select:' when pressing 's'
         face global Prompt $black,$green
    "
}
