vim.g.mapleader = " "
vim.g.maplocalleader = " "

vim.opt.foldmethod = "indent"
vim.opt.foldcolumn = "1"
vim.opt.foldenable = false
vim.opt.clipboard = "unnamedplus"

-- Snippet taken from
-- https://github.com/folke/lazy.nvim#-installation
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end
vim.opt.rtp:prepend(lazypath)
vim.opt.conceallevel = 1
-- Dark colorscheme variant
vim.o.background = "dark"

vim.opt.guicursor = "i:block"
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
-- vim.opt.expandtab = true
-- Disable Vim's mode indicator (i.e. -- INSERT --) so
-- it doesn't visually conflict with `lualine.nvim`
vim.opt.showmode = false
-- Show current line number on cursorline
vim.opt.number = true
-- Relative line numbers
vim.opt.relativenumber = true
vim.opt.swapfile = false
-- Highlight search results
vim.o.hlsearch = true
-- Enable mouse
vim.o.mouse = "a"
vim.o.breakindent = true
vim.o.undofile = true
vim.o.ignorecase = true
-- Shorter update time ~ faster user experience
vim.o.updatetime = 250
vim.o.timeout = true
vim.o.timeoutlen = 300
--vim.o.completeopt = 'menuone,noselect'
if vim.fn.has("termguicolors") == 1 then
	vim.o.termguicolors = true
end

local lsp_helpers = require("plugins.lsp")
local lsp_on_attach = lsp_helpers.lsp_on_attach

require("lazy").setup({
	-- Productivity and AI
	-- {
	-- 	"folke/which-key.nvim",
	-- 	event = "VeryLazy",
	-- 	opts = {
	-- 		-- your configuration comes here
	-- 		-- or leave it empty to use the default settings
	-- 		-- refer to the configuration section below
	-- 	},
	-- 	keys = {
	-- 		{
	-- 			"<leader>?",
	-- 			function()
	-- 				require("which-key").show({ global = false })
	-- 			end,
	-- 			desc = "Buffer Local Keymaps (which-key)",
	-- 		},
	-- 	},
	-- },
	{
		'nvim-orgmode/orgmode',
		event = 'VeryLazy',
		ft = { 'org' },
		config = function()
			-- Setup orgmode
			require('orgmode').setup({
				org_agenda_files = '~/orgfiles/**/*',
				org_default_notes_file = '~/orgfiles/refile.org',
			})

			-- NOTE: If you are using nvim-treesitter with ~ensure_installed = "all"~ option
			-- add ~org~ to ignore_install
			-- require('nvim-treesitter.configs').setup({
			--   ensure_installed = 'all',
			--   ignore_install = { 'org' },
			-- })
		end,
	},
	-- Completion
	{
		"saghen/blink.compat",
		version = "2.*",
		lazy = true,
		opts = {},
	},
	{
		"saghen/blink.cmp",
		version = "1.*",
		dependencies = {
			"L3MON4D3/LuaSnip",
			"PaterJason/cmp-conjure",
			"saghen/blink.compat",
		},
		---@module "blink.cmp"
		opts = {
			snippets = { preset = "luasnip" },
			keymap = {
				preset = "default",
				["<CR>"] = { "fallback" },
				["<Tab>"] = { "fallback" },
				["<S-Tab>"] = { "fallback" },
			},
			sources = {
				default = { "lsp", "path", "snippets", "buffer", "conjure" },
				providers = {
					conjure = {
						name = "conjure",
						module = "blink.compat.source",
					},
				},
			},
		},
	},
	-- Navigation and files
	{
		'stevearc/oil.nvim',
		opts = {},
		-- Optional dependencies
		dependencies = { "nvim-tree/nvim-web-devicons" },
	},
	{
		"nvim-java/nvim-java",
		config = false,
	},
	-- {
	-- 	"luckasRanarison/tailwind-tools.nvim",
	-- 	name = "tailwind-tools",
	-- 	build = ":UpdateRemotePlugins",
	-- 	dependencies = {
	-- 		"nvim-treesitter/nvim-treesitter",
	-- 		"nvim-telescope/telescope.nvim", -- optional
	-- 		-- "neovim/nvim-lspconfig", -- optional
	-- 	},
	-- 	opts = {} -- your configuration
	-- },
	{
		"neovim/nvim-lspconfig",
		lazy = false,
		config = function()
			local project_file = vim.fs.find({ "project.janet" }, { upward = true })[1]
			local root_dir = project_file and vim.fs.dirname(project_file) or vim.loop.cwd()

			vim.lsp.config('janet', {
				cmd = { "janet-lsp" },
				filetypes = { "janet", "jdn" },
				root_dir = root_dir,
				capabilities = vim.lsp.protocol.make_client_capabilities(),
				on_attach = function(client, bufnr)
					lsp_on_attach(client, bufnr)
					-- Remove this 👇 if you don't want to be notified
					-- print("starting janet-lsp")
					-- Optional keymaps here 👇
					vim.keymap.set("n", "K", vim.lsp.buf.hover, { buffer = 0 })
					vim.keymap.set("n", "<leader>k", vim.lsp.buf.hover, { buffer = 0 })
					vim.keymap.set("n", "<leader>gf", vim.lsp.buf.format, {})
				end,
			})
		end,
	},
	{
		"tpope/vim-fugitive"
	},
	-- Editing
	{
		"mfussenegger/nvim-lint",
		config = function()
			vim.api.nvim_create_autocmd({ "BufWritePost" }, {
				callback = function()
					-- try_lint without arguments runs the linters defined in `linters_by_ft`
					-- for the current filetype
					require("lint").try_lint()
					-- You can call `try_lint` with a linter name or a list of names to always
					-- run specific linters, independent of the `linters_by_ft` configuration
					-- require("lint").try_lint("cspell")
				end,
			})
		end
	},
	{
		"kylechui/nvim-surround",
		version = "*", -- Use for stability; omit to use `main` branch for the latest features
		event = "VeryLazy",
		config = function()
			---@diagnostic disable-next-line: missing-fields
			require("nvim-surround").setup({
				-- Configuration here, or leave empty to use defaults
			})
		end
	},
	{
		"julienvincent/nvim-paredit",
		config = function()
			local paredit = require("nvim-paredit")
			paredit.setup({
				filetypes = { "janet", "clojure", "lisp", "scheme", "fennel" },
			})
			vim.keymap.set("n", "<Leader>w",
				function()
					-- place cursor and set mode to `insert`
					paredit.cursor.place_cursor(
					-- wrap element under cursor with `( ` and `)`
						paredit.wrap.wrap_element_under_cursor("( ", ")"),
						-- cursor placement opts
						{ placement = "inner_start", mode = "insert" }
					)
				end)

			vim.keymap.set("n", "<Leader>W",
				function()
					paredit.cursor.place_cursor(
						paredit.wrap.wrap_element_under_cursor("(", ")"),
						{ placement = "inner_end", mode = "insert" }
					)
				end)

			vim.keymap.set("n", "<Leader>i",
				function()
					paredit.cursor.place_cursor(
						paredit.wrap.wrap_enclosing_form_under_cursor("( ", ")"),
						{ placement = "inner_start", mode = "insert" }
					)
				end)
			vim.keymap.set("n", "<Leader>I",
				function()
					paredit.cursor.place_cursor(
						paredit.wrap.wrap_enclosing_form_under_cursor("(", ")"),
						{ placement = "inner_end", mode = "insert" }
					)
				end)
		end
	},
	-- janet syntax for formatter to work (2025-01-03: still needed)
	{ "bakpakin/janet.vim" },
	{
		"Olical/conjure",
		ft = { "clojure", "fennel", "janet", "lisp", "scheme" }, -- etc
		---@diagnostic disable-next-line: unused-local
		config = function(_, opts)
			require("conjure.main").main()
			require("conjure.mapping")["on-filetype"]()
		end,
		init = function()
			-- Set configuration options here
			vim.g["conjure#debug"] = false
			vim.g["conjure#client#scheme#stdio#command"] = "csi -quiet -:c"
			vim.g["conjure#client#scheme#stdio#prompt_pattern"] = "\n-#;%d-> "
		end,
	},
	{
		"ThePrimeagen/harpoon",
		name = "harpoon",
		branch = "harpoon2",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-telescope/telescope.nvim",
		},

		config = function()
			local harpoon = require("harpoon")
			---@diagnostic disable-next-line: missing-fields
			harpoon.setup({})
			-- keymaps
			vim.keymap.set("n", "<leader>ha", function() harpoon:list():add() end)
			vim.keymap.set("n", "<C-e>", function() harpoon.ui:toggle_quick_menu(harpoon:list()) end)

			vim.keymap.set("n", "<leader>h1", function() harpoon:list():select(1) end)
			vim.keymap.set("n", "<leader>h2", function() harpoon:list():select(2) end)
			vim.keymap.set("n", "<leader>h3", function() harpoon:list():select(3) end)
			vim.keymap.set("n", "<leader>h4", function() harpoon:list():select(4) end)


			-- Toggle previous & next buffers stored within Harpoon list
			vim.keymap.set("n", "<C-S-P>", function() harpoon:list():prev() end)
			vim.keymap.set("n", "<C-S-N>", function() harpoon:list():next() end)

			-- Telescope integration
			local conf = require("telescope.config").values
			local function toggle_telescope(harpoon_files)
				local file_paths = {}
				for _, item in ipairs(harpoon_files.items) do
					table.insert(file_paths, item.value)
				end

				require("telescope.pickers").new({}, {
					prompt_title = "Harpoon",
					finder = require("telescope.finders").new_table({
						results = file_paths,
					}),
					previewer = conf.file_previewer({}),
					sorter = conf.generic_sorter({}),
				}):find()
			end
			vim.keymap.set("n", "<leader><C-e>", function() toggle_telescope(harpoon:list()) end,
				{ desc = "Open harpoon window" })
		end,
	},
	{
		"xiyaowong/transparent.nvim",
		config = function()
			require("transparent").setup({
				groups = { -- table: default groups
					'Normal', 'NormalNC', 'Comment', 'Constant', 'Special', 'Identifier',
					'Statement', 'PreProc', 'Type', 'Underlined', 'Todo', 'String', 'Function',
					'Conditional', 'Repeat', 'Operator', 'Structure', 'LineNr', 'NonText',
					'SignColumn', 'CursorLine', 'CursorLineNr', 'StatusLine', 'StatusLineNC',
					'EndOfBuffer',
				},
				extra_groups = {}, -- table: additional groups that should be cleared
				exclude_groups = {}, -- table: groups you don't want to clearj
			})
		end,
	},
	{
		"catppuccin/nvim",
		name = "catppuccin"
	},
	{ "jinh0/eyeliner.nvim" },
	{
		"windwp/nvim-autopairs",
		config = function()
			require("nvim-autopairs").setup {}
		end,
	},
	{
		"nvim-treesitter/nvim-treesitter",
		build = function()
			pcall(require("nvim-treesitter.install").update({ with_sync = true }))
		end,
	},
	{
		"nvim-treesitter/nvim-treesitter-textobjects",
		dependencies = {
			{ "nvim-treesitter/nvim-treesitter" },
		},
		config = function()
			local tsConfigs = require 'nvim-treesitter.configs'
			---@diagnostic disable-next-line: missing-fields
			tsConfigs.setup {
				textobjects = {
					select = {
						enable = true,

						-- Automatically jump forward to textobj, similar to targets.vim
						lookahead = true,

						keymaps = {
							-- You can use the capture groups defined in textobjects.scm
							["af"] = "@function.outer",
							["if"] = "@function.inner",
							["ac"] = "@class.outer",
							-- You can optionally set descriptions to the mappings (used in the desc parameter of
							-- nvim_buf_set_keymap) which plugins like which-key display
							["ic"] = { query = "@class.inner", desc = "Select inner part of a class region" },
							-- You can also use captures from other query groups like `locals.scm`
							["as"] = { query = "@scope", query_group = "locals", desc = "Select language scope" },
						},
						-- You can choose the select mode (default is charwise 'v')
						--
						-- Can also be a function which gets passed a table with the keys
						-- * query_string: eg '@function.inner'
						-- * method: eg 'v' or 'o'
						-- and should return the mode ('v', 'V', or '<c-v>') or a table
						-- mapping query_strings to modes.
						selection_modes = {
							['@parameter.outer'] = 'v', -- charwise
							['@function.outer'] = 'V', -- linewise
							['@class.outer'] = '<c-v>', -- blockwise
						},
						-- If you set this to `true` (default is `false`) then any textobject is
						-- extended to include preceding or succeeding whitespace. Succeeding
						-- whitespace has priority in order to act similarly to eg the built-in
						-- `ap`.
						--
						-- Can also be a function which gets passed a table with the keys
						-- * query_string: eg '@function.inner'
						-- * selection_mode: eg 'v'
						-- and should return true of false
						include_surrounding_whitespace = true,
					},
					swap = {
						enable = true,
						swap_next = {
							["<Leader>a"] = "@parameter.inner",
						},
						swap_previous = {
							["<Leader>A"] = "@parameter.inner",
						},
					},
					move = {
						enable = true,
						set_jumps = true, -- whether to set jumps in the jumplist
						goto_next_start = {
							["]m"] = "@function.outer",
							["]]"] = { query = "@class.outer", desc = "Next class start" },
							--
							-- You can use regex matching (i.e. lua pattern) and/or pass a list in a "query" key to group multiple queries.
							["]o"] = "@loop.*",
							-- ["]o"] = { query = { "@loop.inner", "@loop.outer" } }
							--
							-- You can pass a query group to use query from `queries/<lang>/<query_group>.scm file in your runtime path.
							-- Below example nvim-treesitter's `locals.scm` and `folds.scm`. They also provide highlights.scm and indent.scm.
							["]s"] = { query = "@scope", query_group = "locals", desc = "Next scope" },
							["]z"] = { query = "@fold", query_group = "folds", desc = "Next fold" },
						},
						goto_next_end = {
							["]M"] = "@function.outer",
							["]["] = "@class.outer",
						},
						goto_previous_start = {
							["[m"] = "@function.outer",
							["[["] = "@class.outer",
						},
						goto_previous_end = {
							["[M"] = "@function.outer",
							["[]"] = "@class.outer",
						},
						-- Below will go to either the start or the end, whichever is closer.
						-- Use if you want more granular movements
						-- Make it even more gradual by adding multiple queries and regex.
						goto_next = {
							["]d"] = "@conditional.outer",
						},
						goto_previous = {
							["[d"] = "@conditional.outer",
						}
					},
				},
			}
		end,
	},
	{
		"nvim-telescope/telescope.nvim",
		tag = "0.1.8",
		dependencies = {
			"nvim-lua/plenary.nvim",
			{
				"nvim-telescope/telescope-fzf-native.nvim",
				build = "make"
			}
		},
		config = function()
			local custom_pickers = require('telescope_custom_pickers')
			require('telescope').setup {
				extensions = {
					fzf = {}
				},
				pickers = {
					live_grep = {
						path_display = { 'shorten' },
						mappings = {
							i = {
								['<c-f>'] = custom_pickers.actions.set_extension,
								['<c-l>'] = custom_pickers.actions.set_folders,
							},
						},
					},
				}
			}
			require('telescope').load_extension('fzf')
		end
	},
	{
		"nvim-lualine/lualine.nvim",
		dependencies = { "nvim-tree/nvim-web-devicons", opt = true },
		opts = {
			options = {
				icons_enabled = false,
				theme = "onedark",
				component_separators = "|",
				section_separators = "",
			},
		},
	},
	-- "fatih/vim-go",
	{
		"nvimtools/none-ls.nvim",
		ft = { "go", "python" },
	},
	{
		"folke/todo-comments.nvim",
		dependencies = { "nvim-lua/plenary.nvim" },
		opts = {
			highlight = {
				multiline = true,                 -- enable multine todo comments
				multiline_pattern = "^.",         -- lua pattern to match the next multiline from the start of the matched keyword
				multiline_context = 10,           -- extra lines that will be re-evaluated when changing a line
				before = "bg",                    -- "fg" or "bg" or empty
				keyword = "wide",                 -- "fg", "bg", "wide", "wide_bg", "wide_fg" or empty. (wide and wide_bg is the same as bg, but will also highlight surrounding characters, wide_fg acts accordingly but with fg)
				after = "fg",                     -- "fg" or "bg" or empty
				pattern = [[.*<(KEYWORDS)(\([^\)]+\))?\s*:]], -- pattern or table of patterns, used for highlighting (vim regex)
				-- pattern = [[.*<(KEYWORDS)\s*:]], -- pattern or table of patterns, used for highlighting (vim regex)
				comments_only = true,             -- uses treesitter to match keywords in comments only
				max_line_len = 400,               -- ignore lines longer than this
				exclude = {},                     -- list of file types to exclude highlighting
			},
			-- highlight = {
			-- 	before = "bg",
			-- 	after = "bg",
			-- },
			-- your configuration comes here
			-- or leave it empty to use the default settings
			-- refer to the configuration section below
		}
	},
	-- {
	-- 	"folke/flash.nvim",
	-- 	event = "VeryLazy",
	-- 	---@type Flash.Config
	-- 	opts = {},
	-- 	-- stylua: ignore
	-- 	keys = {
	-- 		{ "s",     mode = { "n", "x", "o" }, function() require("flash").jump() end,              desc = "Flash" },
	-- 		{ "S",     mode = { "n", "x", "o" }, function() require("flash").treesitter() end,        desc = "Flash Treesitter" },
	-- 		{ "r",     mode = "o",               function() require("flash").remote() end,            desc = "Remote Flash" },
	-- 		{ "R",     mode = { "o", "x" },      function() require("flash").treesitter_search() end, desc = "Treesitter Search" },
	-- 		{ "<c-s>", mode = { "c" },           function() require("flash").toggle() end,            desc = "Toggle Flash Search" },
	-- 	},
	-- },
	-- LSP and tooling
	{
		"williamboman/mason-lspconfig.nvim",
		dependencies = {
			"neovim/nvim-lspconfig",
			"williamboman/mason.nvim",
			"folke/lazydev.nvim",
			"saghen/blink.cmp",
		},
		config = function()
			-- Mason setup
			require("mason").setup()

			-- Global capabilities FIRST
			vim.lsp.config("*", {
				capabilities = require("blink.cmp").get_lsp_capabilities(),
			})

			-- mason-lspconfig
			require("mason-lspconfig").setup({
				ensure_installed = {},
				automatic_enable = false,
			})

			-- Global diagnostics (your existing code)
			vim.diagnostic.config({
				signs = false,
				virtual_text = true,
				underline = false,
			})
		end,
	},
	{
		"L3MON4D3/LuaSnip",
		config = function()
			local ls = require("luasnip")
			vim.keymap.set({ "i", "s" }, "<C-L>", function() ls.jump(1) end, { silent = true })
			vim.keymap.set({ "i", "s" }, "<C-J>", function() ls.jump(-1) end, { silent = true })

			vim.keymap.set({ "i", "s" }, "<C-E>", function()
				if ls.choice_active() then
					ls.change_choice(1)
				end
			end, { silent = true })
		end
	},
	{
		'mrcjkb/rustaceanvim',
		version = '^6', -- Recommended
		lazy = false, -- This plugin is already lazy
		dependencies = { "williamboman/mason.nvim" },
		config = function()
			local tools = {
				autoSetHints = true,
				runnables = {
					use_telescope = true,
				},
				inlay_hints = {
					show_parameter_hints = true,
				},
				hover_actions = {
					auto_focus = true
				},
				reload_workspace_from_cargo_toml = true,
				on_initialized = function()
					vim.api.nvim_create_autocmd({ "BufWritePost", "BufEnter", "CursorHold", "InsertLeave" }, {
						pattern = { "*.rs" },
						callback = function()
							local _, _ = pcall(vim.lsp.codelens.refresh)
						end
					})
				end
			}
			require("mason").setup()
			local extension_path = vim.fn.expand("$MASON/packages/codelldb/extension/")
			local codelldb_path = extension_path .. "adapter/codelldb"
			local liblldb_path = extension_path .. "lldb/lib/liblldb.so"
			local cfg = require('rustaceanvim.config')
			vim.g.rustaceanvim = function()
				return {
					tools = tools,
					server = {
						on_attach = lsp_on_attach,
						-- capabilities = capabilities,
						flags = { debounce_text_changes = 150 },
					},
					dap = {
						adapter = cfg.get_codelldb_adapter(codelldb_path, liblldb_path),
					}
				}
			end
			--      require('rust-tools-debug').setup()
		end
	},
	-- Debugging
	"mfussenegger/nvim-dap",
	{
		"leoluz/nvim-dap-go",
		ft = "go",
		dependencies = "mfussenegger/nvim-dap",
		config = function(_, opts)
			require("dap-go").setup(opts)
			local config = {
				args = function()
					-- Prompt the user for input
					local user_input = vim.fn.input("Program args: ")

					-- Split the input on whitespace
					local result = {}
					for word in string.gmatch(user_input, "%S+") do
						table.insert(result, word)
					end

					return result
				end,
				buildFlags = "",
				name = "Debug Package (Arguments)",
				program = "${fileDirname}",
				request = "launch",
				type = "go"
			}
			table.insert(require("dap").configurations["go"], config)
		end,
	},
	{
		'mfussenegger/nvim-dap-python',
		dependencies = "mfussenegger/nvim-dap",
		ft = 'python',
		config = function()
			print('dap python loading')
			require("dap-python").setup("python")
		end
	},
	{
		"rcarriga/nvim-dap-ui",
		dependencies = "nvim-neotest/nvim-nio",
		config = function()
			require("dapui").setup()
			local dap, dapui = require("dap"), require("dapui")
			-- local sidebar_toggle = function()
			--         local widgets = require("dap.ui.widgets")
			--         local sidebar = widgets.sidebar(widgets.scopes)
			--         sidebar.open()
			-- end
			vim.keymap.set("n", "<Leader>dx", ":DapTerminate<CR>")
			vim.keymap.set("n", "<Leader>dt", ":DapToggleBreakpoint<CR>")
			vim.keymap.set("n", "<Leader>do", ":DapStepOver<CR>")
			vim.keymap.set("n", "<Leader>di", ":DapStepInto<CR>")
			vim.keymap.set("n", "<Leader>de", ":DapEval<CR>")
			vim.keymap.set("v", "<M-k>", "<Cmd>lua require(\"dapui\").eval()<CR>")
			vim.keymap.set("n", "<M-k>", "<Cmd>lua require(\"dapui\").eval()<CR>")
			-- vim.keymap.set("n", "<Leader>dus", sidebar_toggle)
			dap.listeners.after.event_initialized["dapui_config"] = function()
				dapui.open()
			end
			dap.listeners.after.event_terminated["dapui_config"] = function()
				dapui.close()
			end
			dap.listeners.after.event_exited["dapui_config"] = function()
				dapui.close()
			end
			dap.configurations.python = {
				{
					type = 'python',
					request = 'launch',
					name = "Launch file",
					program = "${file}",
					pythonPath = function()
						return '/usr/bin/python'
					end,
				},
			}
		end
	},
	{
		"terrortylor/nvim-comment",
		config = function()
			require("nvim_comment").setup({
				operator_mapping = "<leader>/",
			})
		end,
	},
	"CreaturePhil/vim-handmade-hero",
})

lsp_helpers.setup()

-- vim.keymap.set("n", "<M-b>", ":Ex<CR>")

local function bind(op, outer_opts)
	outer_opts = outer_opts or { noremap = true }
	return function(lhs, rhs, opts)
		opts = vim.tbl_extend("force",
			outer_opts,
			opts or {}
		)
		-- vim.api.nvim_set_keymap(op, lhs, rhs, opts)
		vim.keymap.set(op, lhs, rhs, opts)
	end
end

-- local nmap = bind("n", { noremap = false })
local nnoremap = bind("n")
-- local vnoremap = bind("v")
-- local xnoremap = bind("x")
local inoremap = bind("i")

nnoremap("<leader>pv", "<cmd>Ex<CR>")

-- sensible scroll/search
nnoremap("<C-u>", "<C-u>zz")
nnoremap("<C-d>", "<C-d>zz")
nnoremap("n", "nzzzv")
nnoremap("N", "Nzzzv")

-- unbind F1
nnoremap("<F1>", ":echo<CR>")
inoremap("<F1>", "<C-o>:echo<CR>")

-- See `:help telescope.builtin`
local builtin = require("telescope.builtin")

nnoremap("<leader>?", builtin.oldfiles, { desc = "[?] Find recently opened files" })
-- vim.keymap.set("n", "<leader>?", builtin.oldfiles, { desc = "[?] Find recently opened files" })
vim.keymap.set("n", "<leader><space>", builtin.buffers, { desc = "[ ] Find existing buffers" })
vim.keymap.set("n", "<leader>f", function()
	builtin.current_buffer_fuzzy_find(require("telescope.themes").get_dropdown({
		winblend = 10,
		previewer = false,
	}))
end, { desc = "[/] Fuzzily search in current buffer" })

-- pasza: figure out better keybinds, perhaps which key
vim.keymap.set("n", "<space><space>x", "<cmd>source %<CR>")
vim.keymap.set("n", "<space>x", ":.lua<CR>")
vim.keymap.set("v", "<space>x", ":lua<CR>")
vim.keymap.set("n", "<leader>p", builtin.find_files, { desc = "[S]earch [F]iles" })
vim.keymap.set("n", "<leader>np",
	function()
		builtin.find_files {
			cwd = vim.fn.stdpath("config")
		}
	end,
	{ desc = "[S]earch [F]iles" }
)
vim.keymap.set("n", "<M-p>", builtin.find_files, { desc = "[S]earch [F]iles" })
vim.keymap.set("n", "<leader>sh", builtin.help_tags, { desc = "[S]earch [H]elp" })
vim.keymap.set("n", "<leader>sw", builtin.grep_string, { desc = "[S]earch current [W]ord" })
vim.keymap.set("n", "<leader>sg", builtin.live_grep, { desc = "[S]earch by [G]rep" })
vim.keymap.set("n", "<leader>sd", builtin.diagnostics, { desc = "[S]earch [D]iagnostics" })
vim.keymap.set("n", "<leader>ss", builtin.lsp_dynamic_workspace_symbols, { desc = "[S]earch [S]ymbols" })
vim.keymap.set("n", "<leader>ds", builtin.lsp_document_symbols, { desc = "[S]earch [S]ymbols" })
vim.keymap.set("n", "<leader>hh", function() vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled()) end,
	{ desc = "Inlay [Hints] Toggle" })
vim.keymap.set("n", "<leader>sj", builtin.jumplist, { desc = "[S]earch [J]umplist" })
-- TREESITTER
---@diagnostic disable-next-line: missing-fields
require("nvim-treesitter.configs").setup({
	ensure_installed = { "c", "lua", "vim", "go", "javascript", "typescript", "rust" },
	highlight = {
		enable = true,
	},
})

-- COLORSCHEME
vim.cmd.colorscheme("default")

local colorscheme = "catppuccin"

local colorscheme_ok, _ = pcall(vim.cmd.colorscheme, colorscheme)
if not colorscheme_ok then
	vim.notify(colorscheme .. " colorscheme not found!", vim.log.levels.ERROR)
end

-- Adding the same comment color in each theme
local custom_comment_color_group = vim.api.nvim_create_augroup("CustomCommentCollor", { clear = true })
vim.api.nvim_create_autocmd("VimEnter", {
	group = custom_comment_color_group,
	pattern = "*",
	callback = function()
		vim.api.nvim_set_hl(0, "Comment", { fg = "#2ea542" })
	end,
})

-- Disable annoying match brackets and all the jaz
-- local custom_hi_group = vim.api.nvim_create_augroup("CustomHI", { clear = true })
-- vim.api.nvim_create_autocmd("VimEnter", {
--   group = custom_hi_group,
--   pattern = "*",
--   command = ":NoMatchParen",
-- })
