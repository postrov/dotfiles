vim.g.mapleader = " "
vim.g.maplocalleader = " "

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
end
-- local lsp_capabilities = ..
require("lazy").setup({
	{
		"catppuccin/nvim",
		name = "catppuccin"
	},
	-- {
	--   "ellisonleao/gruvbox.nvim",
	--   opts = {
	--   contrast = "hard",
	--   palette_overrides = {
	--     gray = "#2ea542",
	--   },
	--   },
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
		"nvim-telescope/telescope.nvim",
		tag = "0.1.1",
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
		ft = "go",
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
			local mason_registry = require("mason-registry")
			local codelldb = mason_registry.get_package("codelldb")
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
		"rcarriga/nvim-dap-ui",
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
	"jhlgns/naysayer88.vim",
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
		}
	}
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
local null_ls = require("null-ls")
null_ls.setup({
	sources = {
		null_ls.builtins.formatting.gofumpt,
		null_ls.builtins.formatting.goimports_reviser,
		null_ls.builtins.formatting.golines,
		-- null_ls.builtins.formatting.stylua,
	}
})
-- pasza: vim.keymap.set("i", "jj", "<Esc>")


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
