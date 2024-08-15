vim.g.mapleader = " "
vim.g.maplocalleader = " "

vim.opt.foldmethod = "indent"
vim.opt.foldcolumn = "1"
vim.o.foldenable = false
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

-- format on save
local augrup = vim.api.nvim_create_augroup("LspFormatting", {})
local lsp_on_attach = function(client, bufnr)
	if client.supports_method("textDocument/formatting") then
		vim.api.nvim_clear_autocmds({
			group = augrup,
			buffer = bufnr,
		})
		vim.api.nvim_create_autocmd("BufWritePre", {
			group = augrup,
			buffer = bufnr,
			callback = function()
				vim.lsp.buf.format({ bufnr = bufnr })
			end
		})
	end
	-- vim.keymap.set("n", "<Leader>k", "<cmd> lua rt.hover_actions.hover_actions()<CR>", { buffer = bufnr })
	-- vim.keymap.set("n", "<Leader>a", "<cmd> lua rt.code_action_group.code_action_group()<CR>", { buffer = bufnr })
	vim.keymap.set("n", "<Leader>k", "<cmd> lua vim.lsp.buf.code_action()<CR>", { buffer = bufnr })
	vim.keymap.set("n", "<Leader>lj", "<cmd> lua vim.diagnostic.goto_next()<CR>", { buffer = bufnr })
	vim.keymap.set("n", "<Leader>lk", "<cmd> lua vim.diagnostic.goto_prev()<CR>", { buffer = bufnr })
	vim.keymap.set("n", "<Leader>lc", "<cmd> lua vim.lsp.codelens.run()<CR>", { buffer = bufnr })
	vim.keymap.set("n", "<Leader>gd", "<cmd> lua vim.lsp.buf.definition()<CR>", { buffer = bufnr })
	vim.keymap.set("n", "<Leader>lrn", "<cmd> lua vim.lsp.buf.rename()<CR>", { buffer = bufnr })
	vim.keymap.set("n", "<Leader>lfr", "<cmd> lua vim.lsp.buf.references()<CR>", { buffer = bufnr })
	vim.keymap.set("n", "<Leader>hs", "<cmd> lua vim.lsp.buf.signature_help()<CR>", { buffer = bufnr })
	vim.keymap.set("n", "<Leader>ld", "<cmd> lua vim.diagnostic.open_float()<CR>", { buffer = bufnr })
	vim.keymap.set("n", "<Leader>hd", "<cmd> lua vim.lsp.buf.hover()<CR>", { buffer = bufnr })
end
-- local lsp_capabilities = ..
local cmpSetup = function()
	local cmp = require("cmp")
	local cmp_select = { behavior = cmp.SelectBehavior.Select }
	cmp.setup {
		mapping = {
			['<Tab>'] = function(fallback)
				if cmp.visible() then
					cmp.select_next_item()
				else
					fallback()
				end
			end,
			['<C-p>'] = cmp.mapping.select_prev_item(cmp_select),
			['<C-n>'] = cmp.mapping.select_next_item(cmp_select),
			['<C-y>'] = cmp.mapping.confirm({ select = true }),
			["<C-Space>"] = cmp.mapping.complete(),
		},
		sources = {
			{ name = "copilot",  group_index = 2 },
			{ name = "nvim_lsp", group_index = 2 },
			{ name = "path",     group_index = 2 },
			{ name = "luasnip",  group_index = 2 },
		},
	}
end

-- Pasza: this is my implementation based on: https://github.com/julienvincent/nvim-paredit-fennel/blob/master/lua/nvim-paredit-fennel/extension.lua
-- form_types are mine, coming from treesitter's :InspectTree
local createPareditExtension = function(config)
	local form_types = config.form_types or {}
	local whitespace_chars = config.whitespace_chars or { " ", "," }
	local common = require("nvim-paredit.utils.common")
	local traversal = require("nvim-paredit.utils.traversal")
	local function find_next_parent_form(current_node)
		-- FIXME(@pasza): this is to prevent npe in 4j, but it does not return sensible value
		if not current_node then
			return nil
		end
		if common.included_in_table(form_types, current_node:type()) then
			return current_node
		end

		local parent = current_node:parent()
		if parent then
			return find_next_parent_form(parent)
		end

		return current_node
	end

	local function unwrap_form(node)
		if common.included_in_table(form_types, node:type()) then
			return node
		end
		local child = node:named_child(0)
		if child then
			return unwrap_form(child)
		end
	end
	local function node_is_form(node)
		if unwrap_form(node) then
			return true
		else
			return false
		end
	end
	local function get_node_root(node)
		local search_point = node
		if node_is_form(node) then
			search_point = node:parent()
		end

		local root = find_next_parent_form(search_point)
		return traversal.find_root_element_relative_to(root, node)
	end
	local node_is_comment = function(node)
		return node:type() == "comment"
	end
	local get_form_edges = function(node)
		local node_range = { node:range() }

		local form = unwrap_form(node)
		local form_range = { form:range() }

		local left_range = { node_range[1], node_range[2] }
		left_range[3] = form_range[1]
		left_range[4] = form_range[2] + 1

		local right_range = { form:range() }
		right_range[1] = right_range[3]
		right_range[2] = right_range[4] - 1

		local left_text = vim.api.nvim_buf_get_text(0, left_range[1], left_range[2], left_range[3], left_range[4], {})
		local right_text = vim.api.nvim_buf_get_text(0, right_range[1], right_range[2], right_range[3],
			right_range[4],
			{})

		return {
			left = {
				text = left_text[1],
				range = left_range,
			},
			right = {
				text = right_text[1],
				range = right_range,
			},
		}
	end
	local M = {
		whitespace_chars = whitespace_chars,
		get_node_root = get_node_root,
		unwrap_form = unwrap_form,
		node_is_form = node_is_form,
		node_is_comment = node_is_comment,
		get_form_edges = get_form_edges,
	}
	return M
end

require("lazy").setup({
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
		'stevearc/oil.nvim',
		opts = {},
		-- Optional dependencies
		dependencies = { "nvim-tree/nvim-web-devicons" },
	},
	-- {
	-- 	"epwalsh/obsidian.nvim",
	-- 	version = "*", -- recommended, use latest release instead of latest commit
	-- 	lazy = true,
	-- 	ft = "markdown",
	-- 	-- Replace the above line with this if you only want to load obsidian.nvim for markdown files in your vault:
	-- 	-- event = {
	-- 	--   -- If you want to use the home shortcut '~' here you need to call 'vim.fn.expand'.
	-- 	--   -- E.g. "BufReadPre " .. vim.fn.expand "~" .. "/my-vault/**.md"
	-- 	--   "BufReadPre path/to/my-vault/**.md",
	-- 	--   "BufNewFile path/to/my-vault/**.md",
	-- 	-- },
	-- 	dependencies = {
	-- 		-- Required.
	-- 		"nvim-lua/plenary.nvim",
	--
	-- 		-- see below for full list of optional dependencies 👇
	-- 	},
	-- 	opts = {
	-- 		workspaces = {
	-- 			{
	-- 				name = "it",
	-- 				path = "~/GoogleDrive/DriveSyncFiles/IT Knowledge",
	-- 			},
	-- 			{
	-- 				name = "bible",
	-- 				path = "~/GoogleDrive/DriveSyncFiles/Bible Study PL",
	-- 			},
	-- 		},
	--
	-- 		-- see below for full list of options 👇
	-- 	},
	-- },
	--
	-- janet lsp: it works, but it's quite buggy
	-- {
	-- 	"neovim/nvim-lspconfig",
	-- 	lazy = false,
	-- 	config = function()
	-- 		local lspconfig = require("lspconfig")
	-- 		local configs = require("lspconfig.configs")
	--
	-- 		if not configs.janet then
	-- 			configs.janet = {
	-- 				default_config = {
	-- 					cmd = { "janet-lsp" },
	-- 					filetypes = { "janet", "jdn" },
	-- 					root_dir = lspconfig.util.root_pattern("project.janet"),
	-- 					single_file_support = true,
	-- 					settings = {},
	-- 				},
	-- 			}
	-- 		end
	--
	-- 		lspconfig.janet.setup({
	-- 			capabilities = vim.lsp.protocol.make_client_capabilities(),
	-- 			on_attach = function(client, bufnr)
	-- 				lsp_on_attach(client, bufnr)
	-- 				-- Remove this 👇 if you don't want to be notified
	-- 				print("starting janet-lsp")
	-- 				-- Optional keymaps here 👇
	-- 				vim.keymap.set("n", "K", vim.lsp.buf.hover, { buffer = 0 })
	-- 				vim.keymap.set("n", "<leader>k", vim.lsp.buf.hover, { buffer = 0 })
	-- 				vim.keymap.set("n", "<leader>gf", vim.lsp.buf.format, {})
	-- 			end,
	-- 		})
	-- 	end,
	-- },
	-- {
	-- 	"gpanders/nvim-parinfer"
	-- },
	{
		"tpope/vim-fugitive"

	},
	{
		"kylechui/nvim-surround",
		version = "*", -- Use for stability; omit to use `main` branch for the latest features
		event = "VeryLazy",
		config = function()
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
				filetypes = { "janet", "clojure", "lisp", "scheme" },
			})
			paredit.extension.add_language_extension("janet", createPareditExtension(
				{
					form_types = {
						"par_tup_lit",
						"sqr_tup_lit",
						"qq_lit",
						"struct_lit",
						"unquote_lit",
						-- "short_fn_lit",
						-- "long_str_lit",
					}
				}))
			paredit.extension.add_language_extension("lisp", createPareditExtension({
				form_types = {
					"list_lit",
					"defun",
				}
			}))
			paredit.extension.add_language_extension("scheme", createPareditExtension({
				form_types = {
					"list",
				}
			}))
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
	-- janet syntax for formatter to work
	{ "bakpakin/janet.vim" },
	{
		"Olical/conjure",
		ft = { "clojure", "fennel", "janet", "lisp", "scheme" }, -- etc
		-- [Optional] cmp-conjure for cmp
		dependencies = {
			{
				"PaterJason/cmp-conjure",
				config = function()
					local cmp = require("cmp")
					local config = cmp.get_config()
					table.insert(config.sources, {
						name = "buffer",
						option = {
							sources = {
								{ name = "conjure" },
							},
						},
					})
					cmp.setup(config)
				end,
			},
		},
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
	-- {
	-- 	"ThePrimeagen/harpoon",
	-- 	name = "harpoon",
	-- 	branch = "harpoon2",
	-- 	dependencies = {
	-- 		"nvim-lua/plenary.nvim",
	-- 		"nvim-telescope/telescope.nvim",
	-- 	},
	--
	-- 	config = function()
	-- 		local harpoon = require("harpoon")
	-- 		harpoon.setup()
	-- 		-- keymaps
	-- 		vim.keymap.set("n", "<leader>h", function() harpoon:list():append() end)
	-- 		vim.keymap.set("n", "<C-e>", function() harpoon.ui:toggle_quick_menu(harpoon:list()) end)
	--
	-- 		vim.keymap.set("n", "<C-h>", function() harpoon:list():select(1) end)
	-- 		vim.keymap.set("n", "<C-t>", function() harpoon:list():select(2) end)
	-- 		vim.keymap.set("n", "<C-n>", function() harpoon:list():select(3) end)
	-- 		vim.keymap.set("n", "<C-s>", function() harpoon:list():select(4) end)
	--
	-- 		-- Toggle previous & next buffers stored within Harpoon list
	-- 		vim.keymap.set("n", "<C-S-P>", function() harpoon:list():prev() end)
	-- 		vim.keymap.set("n", "<C-S-N>", function() harpoon:list():next() end)
	--
	-- 		-- Telescope integration
	-- 		local conf = require("telescope.config").values
	-- 		local function toggle_telescope(harpoon_files)
	-- 			local file_paths = {}
	-- 			for _, item in ipairs(harpoon_files.items) do
	-- 				table.insert(file_paths, item.value)
	-- 			end
	--
	-- 			require("telescope.pickers").new({}, {
	-- 				prompt_title = "Harpoon",
	-- 				finder = require("telescope.finders").new_table({
	-- 					results = file_paths,
	-- 				}),
	-- 				previewer = conf.file_previewer({}),
	-- 				sorter = conf.generic_sorter({}),
	-- 			}):find()
	-- 		end
	-- 		vim.keymap.set("n", "<leader>se", function() toggle_telescope(harpoon:list()) end,
	-- 			{ desc = "Open harpoon window" })
	-- 	end,
	-- },
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
	-- {
	-- 	"zbirenbaum/copilot.lua",
	-- 	cmd = "Copilot",
	-- 	event = "InsertEnter",
	-- 	config = function()
	-- 		require("copilot").setup({
	-- 			suggestion = { enabled = false },
	-- 			panel = { enabled = false },
	-- 		})
	-- 	end,
	-- },
	-- {
	-- 	"zbirenbaum/copilot-cmp",
	-- 	config = function()
	-- 		require("copilot_cmp").setup({})
	-- 	end
	-- },
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
				},
			}
		end,
	},
	{
		"nvim-telescope/telescope.nvim",
		tag = "0.1.4",
		dependencies = { "nvim-lua/plenary.nvim" },
	},
	{
		"nvim-lualine/lualine.nvim",
		dependencies = { "kyazdani42/nvim-web-devicons", opt = true },
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
		"jose-elias-alvarez/null-ls.nvim",
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
	{
		"VonHeikemen/lsp-zero.nvim",
		branch = "v1.x",
		dependencies = {
			{ "neovim/nvim-lspconfig" },                  -- Required
			{ "folke/neodev.nvim",                config = true }, -- Optional
			{ "williamboman/mason.nvim" },                -- Optional
			{ "williamboman/mason-lspconfig.nvim" },      -- Optional
			{ "hrsh7th/nvim-cmp" },                       -- Required
			{ "hrsh7th/cmp-nvim-lsp" },                   -- Required
			{ "hrsh7th/cmp-buffer" },                     -- Optional
			{ "hrsh7th/cmp-path" },                       -- Optional
			{ "saadparwaiz1/cmp_luasnip" },               -- Optional
			{ "hrsh7th/cmp-nvim-lua" },                   -- Optional
			{ "L3MON4D3/LuaSnip" },                       -- Required
			{ "rafamadriz/friendly-snippets" },           -- Optional
		},
		config = function()
			local lsp = require("lsp-zero")
			lsp.preset("recommended")

			lsp.ensure_installed({
				"tsserver",
				-- "gopls",
				"eslint",
				"rust_analyzer",
				"pyright",
			})

			lsp.set_preferences({
				sign_icons = {},
			})

			lsp.on_attach(function(client, bufnr)
				local opts = { buffer = bufnr, remap = false }
				vim.keymap.set("n", "gd", function()
					vim.lsp.buf.definition()
				end, opts)
			end)

			lsp.setup()

			vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics,
				{
					signs = false,
					virtual_text = true,
					underline = false,
				})
			cmpSetup()
		end,
	},
	{
		"simrat39/rust-tools.nvim",
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
			local registry = require("mason-registry")
			local codelldb = registry.get_package("codelldb")
			local extension_path = codelldb:get_install_path() .. "/extension/"
			local codelldb_path = extension_path .. "adapter/codelldb"
			local liblldb_path = extension_path .. "lldb/lib/liblldb.so"
			require('rust-tools').setup({
				tools = tools,
				server = {
					on_attach = lsp_on_attach,
					-- capabilities = capabilities,
					flags = { debounce_text_changes = 150 },
					settings = {
						["rust-analyzer"] = {
							lens = {
								enable = true,
							},
							checkOnSave = {
								enable = true,
								command = "clippy",
							},
							locationLinks = false,
						},
					},
				},
				dap = {
					adapter = require("rust-tools.dap").get_codelldb_adapter(codelldb_path, liblldb_path),
				}
			})
			--      require('rust-tools-debug').setup()
		end
	},
	"mfussenegger/nvim-dap",
	{
		"leoluz/nvim-dap-go",
		ft = "go",
		dependencies = "mfussenegger/nvim-dap",
		config = function(_, opts)
			require("dap-go").setup(opts)
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
		"akinsho/toggleterm.nvim",
		version = "*",
		opts = {
			direction = "horizontal",
			size = 15,
			open_mapping = [[<M-j>]],
		},
	},
	-- "jhlgns/naysayer88.vim",
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

-- lua lsp setup
local lspconfig = require('lspconfig')
lspconfig.lua_ls.setup {
	on_attach = lsp_on_attach,
	settings = {
		Lua = {
			workspace = {
				checkThirdParty = false,
			},
			format = {
				enable = true,
				defaultConfig = {
					indent_style = "space", -- does not seem to work
					indent_size = "2",
				},
			},
		}
	}
}

-- go lsp setup
local lsp_util = require("lspconfig/util")
lspconfig.gopls.setup {
	on_attach = function(client, buffnr)
		lsp_on_attach(client, buffnr)
		local dap_go = require("dap-go")
		vim.keymap.set("n", "<Leader>dgt", function()
			dap_go.debug_test()
		end)
		vim.keymap.set("n", "<Leader>dlt", function()
			dap_go.debug_last_test()
		end)
	end,
	-- capabilities = capabilities,
	cmd = { "gopls" },
	filetypes = { "go", "gomod", "gowork", "gotmpl" },
	root_dir = lsp_util.root_pattern("go.work", "go.mod", ".git"),
	settings = {
		gopls = {
			completeUnimported = true, -- auto import modules
			usePlaceholders = true, -- suggest function params (doesn't seem to work)
			analyses = {
				unusedparams = true, -- warn about unused params (doesn't seem to work)
			},
			staticcheck = true,
			hints = {
				assignVariableTypes = true,
				compositeLiteralFields = true,
				compositeLiteralTypes = true,
				constantValues = true,
				functionTypeParameters = true,
				parameterNames = true,
				rangeVariableTypes = true,
			},
		}
	}
}
-- python lsp setup
lspconfig.pyright.setup({
	on_attach = function(client, buffnr)
		lsp_on_attach(client, buffnr)
		-- additional python specific stuff here
	end,
	-- capabilities = capabilities,
})

vim.filetype.add({
	extension = {
		templ = "templ",
	},
})
lspconfig.templ.setup {
	on_attach = lsp_on_attach,
	flags = {
		debounce_text_changes = 150,
	},
}
local c = vim.lsp.protocol.make_client_capabilities()
c.textDocument.completion.completionItem.snippetSupport = true
c.textDocument.completion.completionItem.resolveSupport = {
	properties = {
		'documentation',
		'detail',
		'additionalTextEdits',
	},
}
local ocaml_capabilities = require("cmp_nvim_lsp").default_capabilities(c)
lspconfig.ocamllsp.setup {
	on_attach = function(client, buffnr)
		lsp_on_attach(client, buffnr)
		if client.server_capabilities.codeLensProvider then
			local codelens = vim.api.nvim_create_augroup(
				'LSPCodeLens',
				{ clear = true }
			)
			vim.api.nvim_create_autocmd({ 'BufEnter', 'InsertLeave', 'CursorHold' }, {
				group = codelens,
				callback = function()
					vim.lsp.codelens.refresh()
				end,
				buffer = buffnr,
			})
		end
	end,
	capabilities = ocaml_capabilities,
}
require('lspconfig').elixirls.setup {
	-- cmd = { "/home/pasza/.local/share/nvim/mason/packages/elixir-ls/language_server.sh" },
	on_attach = lsp_on_attach,
}
local null_ls = require("null-ls")
null_ls.setup({
	sources = {
		null_ls.builtins.formatting.gofumpt,
		null_ls.builtins.formatting.goimports_reviser,
		-- null_ls.builtins.formatting.golines,
		-- null_ls.builtins.formatting.stylua,
		null_ls.builtins.diagnostics.mypy,
		null_ls.builtins.diagnostics.ruff,
	}
})
-- pasza: vim.keymap.set("i", "jj", "<Esc>")


-- vim.keymap.set("n", "<M-b>", ":Ex<CR>")
require('lspconfig').tailwindcss.setup {
	hovers = true,
	suggestions = true,
}

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

local nmap = bind("n", { noremap = false })
local nnoremap = bind("n")
local vnoremap = bind("v")
local xnoremap = bind("x")
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
vim.keymap.set("n", "<leader>p", builtin.find_files, { desc = "[S]earch [F]iles" })
vim.keymap.set("n", "<M-p>", builtin.find_files, { desc = "[S]earch [F]iles" })
vim.keymap.set("n", "<leader>sh", builtin.help_tags, { desc = "[S]earch [H]elp" })
vim.keymap.set("n", "<leader>sw", builtin.grep_string, { desc = "[S]earch current [W]ord" })
vim.keymap.set("n", "<leader>sg", builtin.live_grep, { desc = "[S]earch by [G]rep" })
vim.keymap.set("n", "<leader>sd", builtin.diagnostics, { desc = "[S]earch [D]iagnostics" })
vim.keymap.set("n", "<leader>ss", builtin.lsp_dynamic_workspace_symbols, { desc = "[S]earch [S]ymbols" })

vim.keymap.set("n", "<leader>hh", function() vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled()) end,
	{ desc = "Inlay [Hints] Toggle" })
-- TREESITTER
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
