" Kip Vim/Neovim plugin.
" Adds LSP setup helpers mirroring VSCode settings:
" - g:kip_language_server_path (default: auto-detect, fallback "kip-lsp")
" - g:kip_language_server_args (default: [])
" - g:kip_trace_server (default: "off" | "messages" | "verbose")
" - g:kip_lsp_autostart (default: 1)

if exists('g:loaded_kip_plugin')
  finish
endif
let g:loaded_kip_plugin = 1

function! s:default_kip_lsp_path() abort
  if executable('kip-lsp')
    return 'kip-lsp'
  endif
  let l:candidates = [
        \ expand('~/.local/bin/kip-lsp'),
        \ expand('~/.cabal/bin/kip-lsp'),
        \ expand('~/.stack/bin/kip-lsp'),
        \ ]
  for l:candidate in l:candidates
    if executable(l:candidate)
      return l:candidate
    endif
  endfor
  return 'kip-lsp'
endfunction

if !exists('g:kip_language_server_path')
  let g:kip_language_server_path = s:default_kip_lsp_path()
endif

if !exists('g:kip_language_server_args')
  let g:kip_language_server_args = []
endif

if !exists('g:kip_trace_server')
  let g:kip_trace_server = 'off'
endif

if !exists('g:kip_lsp_autostart')
  let g:kip_lsp_autostart = 1
endif

if !exists('g:kip_procedural_type_highlight')
  let g:kip_procedural_type_highlight = 1
endif

if !exists('g:kip_procedural_type_highlight_max_tokens')
  let g:kip_procedural_type_highlight_max_tokens = 12000
endif

let s:kip_letter = '[A-Za-zÇĞİÖŞÜçğıöşüÂÎÛâîû]'
let s:kip_ident_core = s:kip_letter . '\+' . '\%(-' . s:kip_letter . '\+\)*'
let s:kip_token_pattern =
      \ '\d\+\%(''' . s:kip_letter . '\+\)\?\|' .
      \ s:kip_ident_core . '\%(''' . s:kip_letter . '\+\)\?\|[(),.]'
let s:kip_keywords = {
      \ 'Bir': 1,
      \ 'bir': 1,
      \ 'ya': 1,
      \ 'da': 1,
      \ 'olabilir': 1,
      \ 'var': 1,
      \ 'olamaz': 1,
      \ 'değilse': 1,
      \ 'olsun': 1,
      \ 'olarak': 1,
      \ 'dersek': 1,
      \ 'için': 1,
      \ 'yerleşik': 1,
      \ }

" vim-lsp diagnostics defaults (only set when user has not configured them).
if !exists('g:lsp_diagnostics_enabled')
  let g:lsp_diagnostics_enabled = 1
endif
if !exists('g:lsp_diagnostics_echo_cursor')
  let g:lsp_diagnostics_echo_cursor = 1
endif
if !exists('g:lsp_diagnostics_signs_enabled')
  let g:lsp_diagnostics_signs_enabled = 1
endif

function! s:normalize_trace(trace) abort
  if type(a:trace) == type('')
    if a:trace ==# 'messages' || a:trace ==# 'verbose'
      return a:trace
    endif
  endif
  return 'off'
endfunction

function! s:lsp_cmd() abort
  return [g:kip_language_server_path] + g:kip_language_server_args
endfunction

function! s:nvim_start_lsp() abort
  if !has('nvim') || !get(g:, 'kip_lsp_autostart', 1)
    return
  endif
  if exists('b:kip_nvim_lsp_started')
    return
  endif
  let b:kip_nvim_lsp_started = 1
  let g:kip__nvim_lsp_cmd = s:lsp_cmd()
  let g:kip__nvim_lsp_trace = s:normalize_trace(g:kip_trace_server)
  lua << EOF
local cmd = vim.g.kip__nvim_lsp_cmd
if type(cmd) ~= "table" then
  return
end
if vim.lsp then
  local trace = vim.g.kip__nvim_lsp_trace or "off"
  if vim.lsp.set_log_level and vim.lsp.log_levels then
    local level = vim.lsp.log_levels.OFF
    if trace == "messages" then
      level = vim.lsp.log_levels.WARN
    elseif trace == "verbose" then
      level = vim.lsp.log_levels.DEBUG
    end
    pcall(vim.lsp.set_log_level, level)
  end
  if vim.lsp.get_clients then
    for _, client in ipairs(vim.lsp.get_clients({ bufnr = 0 })) do
      if client.name == "kip-lsp" then
        return
      end
    end
  elseif vim.lsp.get_active_clients then
    for _, client in ipairs(vim.lsp.get_active_clients({ bufnr = 0 })) do
      if client.name == "kip-lsp" then
        return
      end
    end
  end
  if vim.lsp.start then
    pcall(vim.lsp.start, {
      name = "kip-lsp",
      cmd = cmd,
      root_dir = vim.fn.getcwd(),
    })
  end
end
EOF
endfunction

function! s:register_vim_lsp() abort
  if !exists('*lsp#register_server') || exists('g:kip_vim_lsp_registered')
    return
  endif
  let g:kip_vim_lsp_registered = 1
  call lsp#register_server({
        \ 'name': 'kip-lsp',
        \ 'cmd': {server_info -> s:lsp_cmd()},
        \ 'allowlist': ['kip'],
        \ })
endfunction

function! s:start_vim_lsp() abort
  if !get(g:, 'kip_lsp_autostart', 1)
    return
  endif
  call s:register_vim_lsp()
  if exists(':LspStart')
    silent! execute 'LspStart'
  elseif exists('*lsp#enable')
    silent! call lsp#enable()
  endif
endfunction

function! s:register_coc() abort
  if !exists('*coc#config') || exists('g:kip_coc_registered')
    return
  endif
  let g:kip_coc_registered = 1
  call coc#config('languageserver.kip', {
        \ 'command': g:kip_language_server_path,
        \ 'args': g:kip_language_server_args,
        \ 'filetypes': ['kip'],
        \ 'trace.server': s:normalize_trace(g:kip_trace_server),
        \ })
endfunction

function! s:clear_type_highlight() abort
  if !exists('b:kip_type_match_ids')
    return
  endif
  for l:id in b:kip_type_match_ids
    " Match IDs can become stale when windows/buffers change; ignore missing IDs.
    silent! call matchdelete(l:id)
  endfor
  let b:kip_type_match_ids = []
endfunction

function! s:is_skip_syntax(line, col) abort
  if !has('syntax') || !exists('*synID')
    return 0
  endif
  let l:name = synIDattr(synID(a:line, a:col, 1), 'name')
  return l:name =~# '\v^(kipComment|kipCommentDelim|kipString|kipStringSuffix)$'
endfunction

function! s:tokenize_buffer() abort
  let l:tokens = []
  let l:max_tokens = get(g:, 'kip_procedural_type_highlight_max_tokens', 12000)
  for l:lnum in range(1, line('$'))
    let l:text = getline(l:lnum)
    let l:start = 0
    while 1
      let [l:m, l:s, l:e] = matchstrpos(l:text, s:kip_token_pattern, l:start)
      if l:s < 0
        break
      endif
      if l:e <= l:s
        let l:start = l:s + 1
        continue
      endif
      let l:col = l:s + 1
      if !s:is_skip_syntax(l:lnum, l:col)
        if l:m ==# '(' || l:m ==# ')'
          let l:kind = 'paren'
        elseif l:m ==# ','
          let l:kind = 'comma'
        elseif l:m ==# '.'
          let l:kind = 'period'
        elseif l:m =~# '^\d'
          let l:kind = 'number'
        else
          let l:kind = 'word'
        endif
        call add(l:tokens, {'token': l:m, 'kind': l:kind, 'line': l:lnum, 'col': l:col, 'len': l:e - l:s})
        if len(l:tokens) > l:max_tokens
          return []
        endif
      endif
      let l:start = l:e
    endwhile
  endfor
  return l:tokens
endfunction

function! s:add_type_index(type_indices, idx) abort
  let a:type_indices[string(a:idx)] = 1
endfunction

function! s:compute_type_indices(tokens) abort
  let l:type_indices = {}
  let l:n = len(a:tokens)

  let l:i = 0
  while l:i < l:n
    if a:tokens[l:i].kind ==# 'word' && a:tokens[l:i].token ==# 'Bir'
      let l:j = l:i + 1
      while l:j < l:n
        if a:tokens[l:j].kind ==# 'word'
              \ && (a:tokens[l:j].token ==# 'ya' || a:tokens[l:j].token ==# 'olsun')
          let l:k = l:i + 1
          while l:k < l:j
            if a:tokens[l:k].kind ==# 'word'
              call s:add_type_index(l:type_indices, l:k)
            endif
            let l:k += 1
          endwhile
          break
        endif
        let l:j += 1
      endwhile
    endif
    let l:i += 1
  endwhile

  let l:i = 0
  while l:i < l:n - 1
    if a:tokens[l:i].kind ==# 'word'
          \ && a:tokens[l:i].token ==# 'ya'
          \ && a:tokens[l:i + 1].kind ==# 'word'
          \ && a:tokens[l:i + 1].token ==# 'bir'
      let l:j = l:i + 2
      while l:j < l:n
        if a:tokens[l:j].kind ==# 'word' && a:tokens[l:j].token ==# 'ya'
          let l:word_indices = []
          let l:k = l:i + 2
          while l:k < l:j
            if a:tokens[l:k].kind ==# 'word'
              call add(l:word_indices, l:k)
            endif
            let l:k += 1
          endwhile
          if len(l:word_indices) >= 2
            let l:last = l:word_indices[len(l:word_indices) - 1]
            for l:idx in l:word_indices
              if l:idx != l:last
                call s:add_type_index(l:type_indices, l:idx)
              endif
            endfor
          endif
          break
        endif
        let l:j += 1
      endwhile
    endif
    let l:i += 1
  endwhile

  let l:i = 0
  while l:i < l:n
    if a:tokens[l:i].kind ==# 'word' && a:tokens[l:i].token ==# 'ya'
      let l:start = l:i + 1
      if l:start < l:n
            \ && a:tokens[l:start].kind ==# 'word'
            \ && a:tokens[l:start].token ==# 'da'
        let l:start += 1
      endif
      let l:end_index = -1
      let l:j = l:start
      while l:j < l:n
        if a:tokens[l:j].kind ==# 'word'
              \ && (a:tokens[l:j].token ==# 'ya' || a:tokens[l:j].token ==# 'olabilir')
          let l:end_index = l:j
          break
        endif
        let l:j += 1
      endwhile
      if l:end_index != -1 && l:start < l:end_index
        let l:word_indices = []
        let l:j = l:start
        while l:j < l:end_index
          if a:tokens[l:j].kind ==# 'word'
            call add(l:word_indices, l:j)
          endif
          let l:j += 1
        endwhile
        if len(l:word_indices) >= 2
          let l:last = l:word_indices[len(l:word_indices) - 1]
          for l:idx in l:word_indices
            if l:idx != l:last
              call s:add_type_index(l:type_indices, l:idx)
            endif
          endfor
        endif
      endif
    endif
    let l:i += 1
  endwhile

  let l:def_start = 0
  let l:i = 0
  while l:i <= l:n
    if l:i < l:n && a:tokens[l:i].kind !=# 'period'
      let l:i += 1
      continue
    endif
    let l:comma_index = -1
    let l:j = l:def_start
    while l:j < l:i
      if a:tokens[l:j].kind ==# 'comma'
        let l:comma_index = l:j
        break
      endif
      let l:j += 1
    endwhile
    let l:stack = []
    let l:seen_top = 0
    let l:j = l:def_start
    while l:j < l:i
      let l:tok = a:tokens[l:j]
      if l:tok.kind ==# 'paren'
        if l:tok.token ==# '('
          let l:eligible = (l:comma_index != -1 && l:j < l:comma_index && !l:seen_top)
          call add(l:stack, {'eligible': l:eligible, 'word_indices': [], 'has_number': 0, 'has_eligible_child': 0})
        elseif len(l:stack) > 0
          let l:top = remove(l:stack, -1)
          if len(l:stack) > 0 && l:top.eligible
            let l:stack[-1].has_eligible_child = 1
          endif
          if l:top.eligible && !l:top.has_number && len(l:top.word_indices) > 1
            let l:k = 1
            while l:k < len(l:top.word_indices)
              call s:add_type_index(l:type_indices, l:top.word_indices[l:k])
              let l:k += 1
            endwhile
          elseif l:top.eligible && !l:top.has_number && len(l:top.word_indices) == 1 && l:top.has_eligible_child
            call s:add_type_index(l:type_indices, l:top.word_indices[0])
          endif
        endif
        let l:j += 1
        continue
      endif
      if len(l:stack) == 0
        if l:tok.kind ==# 'word' || l:tok.kind ==# 'number'
          let l:seen_top = 1
        endif
        let l:j += 1
        continue
      endif
      let l:top = l:stack[-1]
      if l:top.eligible
        if l:tok.kind ==# 'number'
          let l:stack[-1].has_number = 1
        elseif l:tok.kind ==# 'word'
          call add(l:stack[-1].word_indices, l:j)
        endif
      endif
      let l:j += 1
    endwhile
    let l:def_start = l:i + 1
    let l:i += 1
  endwhile

  return l:type_indices
endfunction

function! s:apply_type_highlight() abort
  if &filetype !=# 'kip'
    return
  endif
  if !get(g:, 'kip_procedural_type_highlight', 1)
    call s:clear_type_highlight()
    return
  endif
  call s:clear_type_highlight()
  let l:tokens = s:tokenize_buffer()
  if empty(l:tokens)
    return
  endif
  let l:type_indices = s:compute_type_indices(l:tokens)
  let l:positions = []
  for l:i in range(0, len(l:tokens) - 1)
    if !has_key(l:type_indices, string(l:i))
      continue
    endif
    let l:tok = l:tokens[l:i]
    if l:tok.kind !=# 'word' || has_key(s:kip_keywords, l:tok.token)
      continue
    endif
    call add(l:positions, [l:tok.line, l:tok.col, l:tok.len])
  endfor
  if empty(l:positions)
    return
  endif
  hi def link KipTypeProc Type
  if !exists('b:kip_type_match_ids')
    let b:kip_type_match_ids = []
  endif
  let l:idx = 0
  while l:idx < len(l:positions)
    let l:chunk = l:positions[l:idx : l:idx + 7]
    let l:id = matchaddpos('KipTypeProc', l:chunk, 20)
    if l:id > 0
      call add(b:kip_type_match_ids, l:id)
    endif
    let l:idx += 8
  endwhile
endfunction

function! s:restart_nvim_lsp() abort
  if !has('nvim')
    return
  endif
  lua << EOF
if vim.lsp then
  if vim.lsp.get_clients then
    for _, client in ipairs(vim.lsp.get_clients({ name = "kip-lsp" })) do
      pcall(vim.lsp.stop_client, client.id)
    end
  elseif vim.lsp.get_active_clients then
    for _, client in ipairs(vim.lsp.get_active_clients()) do
      if client.name == "kip-lsp" then
        pcall(vim.lsp.stop_client, client.id)
      end
    end
  end
end
EOF
  if &filetype ==# 'kip'
    unlet! b:kip_nvim_lsp_started
    call s:nvim_start_lsp()
  endif
endfunction

function! KipLspRestart() abort
  call s:register_vim_lsp()
  call s:register_coc()
  call s:restart_nvim_lsp()
  call s:start_vim_lsp()
  if exists(':LspRestart')
    silent! execute 'LspRestart'
  elseif exists(':CocRestart')
    silent! execute 'CocRestart'
  endif
  echom '[kip] LSP restarted with current kip settings.'
endfunction

function! KipLspInfo() abort
  echom '[kip] languageServerPath=' . g:kip_language_server_path
  echom '[kip] languageServerArgs=' . string(g:kip_language_server_args)
  echom '[kip] trace.server=' . s:normalize_trace(g:kip_trace_server)
  echom '[kip] autostart=' . string(get(g:, 'kip_lsp_autostart', 1))
endfunction

command! KipLspRestart call KipLspRestart()
command! KipLspInfo call KipLspInfo()

augroup kip_lsp
  autocmd!
  autocmd User lsp_setup call s:register_vim_lsp()
  autocmd VimEnter * call s:register_vim_lsp()
  autocmd VimEnter * call s:register_coc()
  autocmd FileType kip call s:start_vim_lsp()
  autocmd FileType kip call s:nvim_start_lsp()
augroup END

augroup kip_type_highlight
  autocmd!
  autocmd FileType kip call s:apply_type_highlight()
  autocmd BufEnter *.kip call s:apply_type_highlight()
  autocmd TextChanged,TextChangedI *.kip call s:apply_type_highlight()
  autocmd InsertLeave *.kip call s:apply_type_highlight()
  autocmd BufLeave *.kip call s:clear_type_highlight()
augroup END
