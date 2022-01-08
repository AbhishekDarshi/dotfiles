-- Install packer
-- local install_path = vim.fn.stdpath 'data' .. '/site/pack/packer/start/packer.nvim'



local use = require('packer').use
require('packer').startup(function()
  use 'wbthomason/packer.nvim' -- Package manager
  -- use 'tpope/vim-fugitive' -- Git commands in nvim
  use 'tpope/vim-commentary' -- "gc" to comment visual regions/lines

  -- UI to select things (files, grep results, open buffers...)
  use { 'nvim-telescope/telescope.nvim', requires = { { 'nvim-lua/popup.nvim' }, { 'nvim-lua/plenary.nvim' } } }
  use {'nvim-telescope/telescope-fzf-native.nvim', run = 'make' }
  use {'tjdevries/cyclist.vim'}
  -- Add indentation guides even on blank lines
  -- use 'lukas-reineke/indent-blankline.nvim'

  -- Add git related info in the signs columns and popups
  -- use { 'lewis6991/gitsigns.nvim', requires = { 'nvim-lua/plenary.nvim' } }

  -- Highlight, edit, and navigate code using a fast incremental parsing library
  use {'nvim-treesitter/nvim-treesitter', run = ':TSUpdate'}
  -- -- Additional textobjects for treesitter
  use 'nvim-treesitter/nvim-treesitter-textobjects'

  use 'neovim/nvim-lspconfig' -- Collection of configurations for built-in LSP client
  use 'hrsh7th/nvim-cmp' -- Autocompletion plugin
  use 'hrsh7th/cmp-nvim-lsp'
  use 'saadparwaiz1/cmp_luasnip'
  use 'L3MON4D3/LuaSnip' -- Snippets plugin
  -- use 'hrsh7th/nvim-compe' -- Autocompletion plugin
  use 'wbthomason/lsp-status.nvim'
  -- use 'glepnir/lspsaga.nvim'

  -- use 'onsails/lspkind-nvim'
  -- use {
  --     'kyazdani42/nvim-tree.lua',
  --     requires = 'kyazdani42/nvim-web-devicons'
  -- }
  -- use {
  --   "folke/trouble.nvim",
  --   requires = "kyazdani42/nvim-web-devicons",
  --   config = function()
  --     require("trouble").setup {
  --       -- your configuration comes here
  --       -- or leave it empty to use the default settings
  --     }
  --   end
  -- }
  use 'tjdevries/colorbuddy.vim'
  use 'tjdevries/gruvbuddy.nvim'
  use 'rktjmp/lush.nvim'
  -- use 'metalelf0/jellybeans-nvim'
  use 'kyazdani42/nvim-web-devicons'
  -- use 'Th3Whit3Wolf/spacebuddy'
  -- use 'folke/tokyonight.nvim'
  -- use 'glepnir/galaxyline.nvim'
  -- use 'famiu/feline.nvim'
  -- use 'windwp/windline.nvim'
  use 'norcalli/nvim-colorizer.lua'
  -- use 'ishan9299/modus-theme-vim'
 -- use 'monsonjeremy/onedark.nvim'
  -- use 'Th3Whit3Wolf/one-nvim'
  use 'tomasiser/vim-code-dark'
  -- use 'projekt0n/github-nvim-theme'
  use 'bluz71/vim-moonfly-colors'

end)

--require('lspkind').init({
--    -- enables text annotations
--    --
--    -- default: true
--    with_text = true,

--    -- default symbol map
--    -- can be either 'default' or
--    -- 'codicons' for codicon preset (requires vscode-codicons font installed)
--    --
--    -- default: 'default'
--    preset = 'codicons',

--    -- override preset symbols
--    --
--    -- default: {}
--    symbol_map = {
--      Text = "",
--      Method = "",
--      Function = "",
--      Constructor = "",
--      Field = "ﰠ",
--      Variable = "",
--      Class = "ﴯ",
--      Interface = "",
--      Module = "",
--      Property = "ﰠ",
--      Unit = "塞",
--      Value = "",
--      Enum = "",
--      Keyword = "",
--      Snippet = "",
--      Color = "",
--      File = "",
--      Reference = "",
--      Folder = "",
--      EnumMember = "",
--      Constant = "",
--      Struct = "פּ",
--      Event = "",
--      Operator = "",
--      TypeParameter = ""
--    },
--})


-- https://github.com/simrat39/symbols-outline.nvim
-- vim.g.symbols_outline = {
--   auto_preview = false
-- }

--Incremental live completion
vim.o.inccommand = 'nosplit'

--Set highlight on search
vim.o.hlsearch = false

--Make line numbers default
vim.wo.number = true

--Do not save when switching buffers
vim.o.hidden = true

--Enable mouse mode
vim.o.mouse = 'a'

--Enable break indent
-- vim.o.breakindent = true

--Save undo history
vim.cmd [[set undofile]]

--Case insensitive searching UNLESS /C or capital in search
vim.o.ignorecase = true
vim.o.smartcase = true

--Decrease update time
vim.o.updatetime = 250
vim.wo.signcolumn = 'yes'

--Set colorscheme (order is important here)
vim.o.termguicolors = true
-- vim.g.onedark_terminal_italics = 2
vim.g.tokyonight_style = "night"
-- vim.g.tokyonight_italic_functions = false
-- vim.g.tokyonight_sidebars = { "qf", "vista_kind", "terminal", "packer" }
vim.cmd [[colorscheme moonfly]]
-- vim.g.modus_faint_syntax = 1
-- vim.cmd('colorscheme modus-vivendi') 

-- require('github-theme').setup()
-- require('onedark').setup()
-- require('colorbuddy').colorscheme('gruvbuddy')
-- require('colorbuddy').colorscheme('onebuddy')


--Remap space as leader key
vim.api.nvim_set_keymap('', '<Space>', '<Nop>', { noremap = true, silent = true })
vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

--Remap for dealing with word wrap
vim.api.nvim_set_keymap('n', 'k', "v:count == 0 ? 'gk' : 'k'", { noremap = true, expr = true, silent = true })
vim.api.nvim_set_keymap('n', 'j', "v:count == 0 ? 'gj' : 'j'", { noremap = true, expr = true, silent = true })

--Map blankline
-- vim.g.indent_blankline_char = '┊'
-- vim.g.indent_blankline_show_first_indent_level = true
-- vim.g.indent_blankline_filetype_exclude = { 'help', 'packer' }
-- vim.g.indent_blankline_buftype_exclude = { 'terminal', 'nofile' }
-- -- vim.g.indent_blankline_char_highlight = 'LineNr'
-- vim.g.indent_blankline_show_trailing_blankline_indent = false
-- -- vim.g.indent_blankline_context_patterns = {'class', 'function', 'method', 'if', 'while', 'for', 'object', 'table', 'block', 'arguments', 'def', 'try', 'except', 'let', 'fn', 'pub fn', 'impl', 'trait', 'struct', 'pub trait', 'pub struct', 'impl'}
-- -- vim.g.indent_blankline_show_current_context = true
-- vim.g.indent_blankline_show_end_of_line = true
-- -- vim.g.indent_blankline_use_treesitter = true

-- Gitsigns
-- require('gitsigns').setup {
--   signs = {
--     add = { hl = 'GitGutterAdd', text = '+' },
--     change = { hl = 'GitGutterChange', text = '~' },
--     delete = { hl = 'GitGutterDelete', text = '_' },
--     topdelete = { hl = 'GitGutterDelete', text = '‾' },
--     changedelete = { hl = 'GitGutterChange', text = '~' },
--   },
-- }

-- Telescope
local status_ok, actions = pcall(require, "telescope.actions")
require('telescope').setup {
  extensions = {
    -- frecency is not working, maybe some setup issue i think
    frecency = {
      show_scores = false,
      show_unindexed = true,
      ignore_patterns = {"*.git/*", "*/tmp/*"},
    },
    fzf = {
      fuzzy = false
    },
    -- fzy_native = {
    --   override_generic_sorter = true,
    --   override_file_sorter = true,
    -- },
  },
  defaults = {
    vimgrep_arguments = {
      "rg",
      "--color=never",
      "--no-heading",
      "--with-filename",
      "--line-number",
      "--column",
      "--smart-case"
    },
    mappings = {
      -- taken from lunarvim
      i = {
        ["<C-j>"] = actions.cycle_history_next,
        ["<C-k>"] = actions.cycle_history_prev,
        ["<C-c>"] = actions.close,
        ["<C-n>"] = actions.move_selection_next,
        ["<C-p>"] = actions.move_selection_previous,
        ["<C-q>"] = actions.smart_send_to_qflist + actions.open_qflist,
        ["<CR>"] = actions.select_default + actions.center,
        -- To disable a keymap, put [map] = false
        -- So, to not map "<C-n>", just put
        -- ["<c-t>"] = trouble.open_with_trouble,
        -- ["<c-x>"] = false,
        -- ["<esc>"] = actions.close,
        -- Otherwise, just set the mapping to the function that you want it to be.
        -- ["<C-i>"] = actions.select_horizontal,
        -- Add up multiple actions
        -- You can perform as many actions in a row as you like
        -- ["<CR>"] = actions.select_default + actions.center + my_cool_custom_action,
      },
      n = {
        ["<C-j>"] = actions.move_selection_next,
        ["<C-k>"] = actions.move_selection_previous,
        ["<C-q>"] = actions.smart_send_to_qflist + actions.open_qflist,
        -- ["<c-t>"] = trouble.open_with_trouble,
        -- ["<C-i>"] = my_cool_custom_action,
      },
    },
  },
}

-- require('telescope').load_extension('coc')
require('telescope').load_extension('fzf')
-- require('telescope').load_extension('frecency')
-- require('telescope').load_extension('fzy_native')
--Add leader shortcuts
-- vim.api.nvim_set_keymap("n", "<leader><leader>", "<Cmd>lua require('telescope').extensions.frecency.frecency()<CR>", {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<leader><space>', [[<cmd>lua require('telescope.builtin').buffers()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>ff', [[<cmd>lua require('telescope.builtin').find_files({previewer = false})<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>fg', [[<cmd>lua require('telescope.builtin').current_buffer_fuzzy_find()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>fb', [[<cmd>lua require('telescope.builtin').buffers()<CR>]], { noremap = true, silent = true })
-- vim.api.nvim_set_keymap('n', '<leader>sh', [[<cmd>lua require('telescope.builtin').help_tags()<CR>]], { noremap = true, silent = true })
-- vim.api.nvim_set_keymap('n', '<leader>st', [[<cmd>lua require('telescope.builtin').tags()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>fp', [[<cmd>lua require('telescope.builtin').grep_string()<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>fd', [[<cmd>lua require('telescope.builtin').live_grep()<CR>]], { noremap = true, silent = true })
-- this is very slow :(
-- vim.api.nvim_set_keymap('n', '<leader>gr', [[<cmd>lua require('telescope.builtin').lsp_references()<CR>]], { noremap = true, silent = true })
-- vim.api.nvim_set_keymap('n', '<leader>so', [[<cmd>lua require('telescope.builtin').tags{ only_current_buffer = true }<CR>]], { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>?', [[<cmd>lua require('telescope.builtin').oldfiles()<CR>]], { noremap = true, silent = true })

-- Highlight on yank
vim.api.nvim_exec(
  [[
  augroup YankHighlight
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank()
  augroup end
]],
  false
)

-- Y yank until the end of line
vim.api.nvim_set_keymap('n', 'Y', 'y$', { noremap = true })

-- Make runtime files discoverable to the server
local runtime_path = vim.split(package.path, ';')
table.insert(runtime_path, 'lua/?.lua')
table.insert(runtime_path, 'lua/?/init.lua')

-- LSP settings
-- local nvim_lsp = require 'lspconfig'
-- local on_attach = function(_, bufnr)
--   vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

--   local opts = { noremap = true, silent = true }
--   vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
--   vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
--   -- vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
--   vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
--   -- vim.api.nvim_buf_set_keymap(bufnr, 'n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
--   -- vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
--   -- vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
--   -- vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
--   -- vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
--   vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
--   vim.api.nvim_buf_set_keymap(bufnr, 'n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
--   vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
--   -- vim.api.nvim_buf_set_keymap(bufnr, 'v', '<leader>ca', '<cmd>lua vim.lsp.buf.range_code_action()<CR>', opts)
--   vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
--   -- vim.api.nvim_buf_set_keymap(bufnr, 'n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
--   -- vim.api.nvim_buf_set_keymap(bufnr, 'n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
--   vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
--   vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>so', [[<cmd>lua require('telescope.builtin').lsp_document_symbols()<CR>]], opts)
--   vim.cmd [[ command! Format execute 'lua vim.lsp.buf.formatting()' ]]
-- end

-- nvim_lsp[name].setup {
--     cmd = cmd,
--     on_attach = on_attach,
-- }
-- local saga = require 'lspsaga'
-- saga.init_lsp_saga()


-- local capabilities = vim.lsp.protocol.make_client_capabilities()
-- capabilities.textDocument.completion.completionItem.snippetSupport = true

-- -- Enable the following language servers
-- -- local servers = { 'clangd', 'rust_analyzer', 'pyright', 'tsserver' }

-- local servers = { 'pyright', 'rust_analyzer' }
-- for _, lsp in ipairs(servers) do
--   nvim_lsp[lsp].setup {
--     on_attach = on_attach,
--     -- capabilities = capabilities,
--     flags = {
--       debounce_text_changes = 150,
--     }
--   }
-- end
-- require("lspconfig").pylsp.setup{}

-- require'lspconfig'.pyright.setup{}
-- vim.lsp.set_log_level("debug")

-- require('lspconfig').pyright.setup {
--   on_attach=my_custom_on_attach,
--   settings = {
--     python =  {
--         analysis = {
--         autoSearchPaths = true,
--         useLibraryCodeForTypes = true,
--         diagnosticMode = 'openFilesOnly',
--       }
--     }
--   }
-- }

-- local name = "pyright"
-- local cmd = { "pyright-langserver", "--stdio"} -- needed for elixirls, omnisharp, sumneko_lua
local nvim_lsp = require('lspconfig')

-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  -- Enable completion triggered by <c-x><c-o>
  -- buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap=true, silent=true }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap('n', 'gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
  buf_set_keymap('n', 'K', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
  buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
  buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
  buf_set_keymap('n', '<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  buf_set_keymap('n', '<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
  buf_set_keymap('n', '<space>D', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
  buf_set_keymap('n', '<space>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
  buf_set_keymap('n', '<space>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
  buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
  buf_set_keymap('n', '<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
  buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
  buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
  -- buf_set_keymap('n', '<space>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
  -- buf_set_keymap('n', '<space>f', '<cmd>lua vim.lsp.buf.formatting()<CR>', opts)

end

-- Use a loop to conveniently call 'setup' on multiple servers and
-- map buffer local keybindings when the language server attaches
-- local servers = { 'jedi_language_server', 'rust_analyzer', 'tsserver' }
local servers = { 'pyright', 'rust_analyzer' }
for _, lsp in ipairs(servers) do
  nvim_lsp[lsp].setup {
    on_attach = on_attach,
    flags = {
      debounce_text_changes = 150,
    }
  }
end


-- require("lspconfig").pyright.setup{}
-- require("lspconfig").pylsp.setup{}
-- require("lspconfig").jedi-language-server.setup{}


-- -- Treesitter configuration
-- -- Parsers must be installed manually via :TSInstall
require('nvim-treesitter.configs').setup {
  ensure_installed = "maintained",
  highlight = {
    enable = true, -- false will disable the whole extension
    -- additional_vim_regex_highlighting = true,
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = 'gnn',
      node_incremental = 'grn',
      scope_incremental = 'grc',
      node_decremental = 'grm',
    },
  },
  autotag = {
    enable = true,
  },
  indent = {
    enable = true,
  },
  textobjects = {
    select = {
      enable = true,
      lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ['af'] = '@function.outer',
        ['if'] = '@function.inner',
        ['ac'] = '@class.outer',
        ['ic'] = '@class.inner',
      },
    },
    swap = {
      enable = true,
      swap_next = {
          ["<leader>s"] = "@parameter.inner"
      },
      swap_previous = {
          ["<leader>S"] = "@parameter.inner"
      }
    },
    move = {
      enable = true,
      set_jumps = true, -- whether to set jumps in the jumplist
      goto_next_start = {
        [']m'] = '@function.outer',
        [']]'] = '@class.outer',
      },
      goto_next_end = {
        [']M'] = '@function.outer',
        [']['] = '@class.outer',
      },
      goto_previous_start = {
        ['[m'] = '@function.outer',
        ['[['] = '@class.outer',
      },
      goto_previous_end = {
        ['[M'] = '@function.outer',
        ['[]'] = '@class.outer',
      },
    },
  },
}

-- vim.wo.foldmethod = 'expr'
-- vim.wo.foldexpr = 'nvim_treesitter#foldexpr()'

-- nvim-cmp supports additional completion capabilities
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

-- Set completeopt to have a better completion experience
vim.o.completeopt = 'menuone,noselect'

-- luasnip setup
local luasnip = require 'luasnip'

-- nvim-cmp setup
local cmp = require 'cmp'
cmp.setup {
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  mapping = {
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-n>'] = cmp.mapping.select_next_item(),
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<CR>'] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    },
    ['<Tab>'] = function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end,
    ['<S-Tab>'] = function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end,
  },
  sources = {
    { name = 'nvim_lsp' },
    { name = 'luasnip' },
  },
}
