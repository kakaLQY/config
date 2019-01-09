call ale#linter#Define('javascript', {
\   'name': 'eslint',
\   'output_stream': 'both',
\   'executable_callback': 'ale#handlers#eslint#GetExecutable',
\   'command_callback': 'ale#handlers#eslint#GetCommand',
\   'callback': 'ale#handlers#eslint#Handle',
\})

set shiftwidth=2 " On pressing tab, insert 2 spaces 

let g:ale_fixers.javascript = ['eslint']
let g:ale_fix_on_save = 1

