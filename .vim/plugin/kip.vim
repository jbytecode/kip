" Kip Vim/Neovim plugin.
" Adds LSP setup helpers mirroring VSCode settings:
" - g:kip_language_server_path (default: "kip-lsp")
" - g:kip_language_server_args (default: [])
" - g:kip_trace_server (default: "off" | "messages" | "verbose")
" - g:kip_lsp_autostart (default: 1)

if exists('g:loaded_kip_plugin')
  finish
endif
let g:loaded_kip_plugin = 1

if !exists('g:kip_language_server_path')
  let g:kip_language_server_path = 'kip-lsp'
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
