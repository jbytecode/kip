" Quit when a syntax file was already loaded.
if exists('b:current_syntax')
  finish
endif

" Match Turkish letters used in Kip identifiers.
let s:kip_letter = '[A-Za-zÇĞİÖŞÜçğıöşüÂÎÛâîû]'
let s:kip_ident_core = s:kip_letter . '\+' . '\%(-' . s:kip_letter . '\+\)*'
let s:kip_suffix = "'". s:kip_letter . '\+'
let s:kip_cond_suffix = '\%(se\|sa\|yse\|ysa\)\?'
let s:apos = "'"

" Strings and string suffixes.
syntax match kipEscape '\\.' contained
syntax region kipString start='"' skip='\\.' end='"' contains=kipEscape containedin=ALLBUT,kipComment
execute 'syntax match kipStringSuffix /\%("\)\@<=' . s:kip_suffix . '/ containedin=ALLBUT,kipComment'

" Numbers and numeric suffixes.
execute 'syntax match kipNumber /\<[0-9]\+\%(\.[0-9]\+\)\?\%(' . s:apos . '\|[[:space:],;.)]\|$\)\@=/ containedin=ALLBUT,kipComment'
execute 'syntax match kipNumberSuffix /\<[0-9]\+\%(\.[0-9]\+\)\?\zs' . s:kip_suffix . '/ containedin=ALLBUT,kipComment'

" Keywords (ported from VSCode grammar).
syntax match kipKeywordDecl '\<\(Bir\|yerleşiktir\|olsun\)\>' containedin=ALLBUT,kipComment
syntax match kipKeywordCtrl '\<ya\>\s\+\<da\>' containedin=ALLBUT,kipComment
syntax match kipKeywordCtrl '\<\(ya\|olabilir\|var\|için\|olarak\|dersek\)\>' containedin=ALLBUT,kipComment

" Identifiers.
execute 'syntax match kipIdentifier /\<' . s:kip_ident_core . '\%(' . s:kip_suffix . '\)\?' . s:kip_cond_suffix . '\%(' . s:apos . '\|[[:space:],;.()]\|$\)\@=/ containedin=ALLBUT,kipComment'

" Delimiters.
syntax match kipDelimiter '[()[\],;.]' containedin=ALLBUT,kipComment

" Comments (define late to ensure it wins over generic delimiters/identifiers).
syntax keyword kipTodo TODO FIXME NOTE XXX contained
syntax region kipComment matchgroup=kipCommentDelim start='(\*' end='\*)' keepend contains=kipTodo,kipComment,@Spell

" Highlight links.
hi def link kipComment Comment
hi def link kipCommentDelim Comment
hi def link kipTodo Todo
hi def link kipEscape SpecialChar
hi def link kipString String
hi def link kipStringSuffix Special
hi def link kipNumber Number
hi def link kipNumberSuffix Special
hi def link kipKeywordDecl Keyword
hi def link kipKeywordCtrl Keyword
hi def link kipIdentifier Identifier
hi def link kipDelimiter Delimiter

" Cleanup script-local helpers.
unlet s:kip_letter
unlet s:kip_ident_core
unlet s:kip_suffix
unlet s:kip_cond_suffix
unlet s:apos

let b:current_syntax = 'kip'
