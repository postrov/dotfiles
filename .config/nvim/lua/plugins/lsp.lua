local M = {}

local formatting_augroup = vim.api.nvim_create_augroup("LspFormatting", {})

function M.mk_lsp_on_attach(t)
	setmetatable(t, { __index = { do_format_on_save = true } })
	return function(client, bufnr)
		if client:supports_method("textDocument/formatting") then
			vim.api.nvim_clear_autocmds({
				group = formatting_augroup,
				buffer = bufnr,
			})
			vim.api.nvim_create_autocmd("BufWritePre", {
				group = formatting_augroup,
				buffer = bufnr,
				callback = function()
					vim.lsp.buf.format({ bufnr = bufnr })
				end
			})
		end
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
end

M.lsp_on_attach = M.mk_lsp_on_attach { do_format_on_save = true }

function M.setup()
	local lsp_on_attach = M.lsp_on_attach
	local mk_lsp_on_attach = M.mk_lsp_on_attach
	-- lua lsp setup
	vim.lsp.config('lua_ls', {
		on_attach = lsp_on_attach,
		settings = {
			Lua = {
				diagnostics = {
					globals = { "vim" },
				},
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
	})

	-- go lsp setup
	vim.lsp.config('gopls', {
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
		cmd = { "gopls" },
		filetypes = { "go", "gomod", "gowork", "gotmpl" },
		root_dir = vim.fs.dirname(vim.fs.find({ "go.work", "go.mod", ".git" }, { upward = true })[1]),
		settings = {
			gopls = {
				completeUnimported = true, -- auto import modules
				usePlaceholders = true, -- suggest function params (doesn't seem to work)
				analyses = {
					unusedparams = true, -- warn about unused params (doesn't seem to work)
				},
				staticcheck = false,
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
	})

	-- python lsp setup
	vim.lsp.config('pyright', {
		on_attach = lsp_on_attach,
	})

	-- java lsp setup
	vim.lsp.config('jdtls', {
		on_attach = lsp_on_attach,
	})

	-- c lsp setup
	vim.lsp.config('clangd', {
		filetypes = { "c", "cpp", "objc", "objcpp", "cuda", "proto", "hpp" },
		on_attach = lsp_on_attach,
	})

	-- astro lsp setup
	vim.lsp.config('astro', {
		on_attach = lsp_on_attach,
	})

	vim.filetype.add({
		extension = {
			templ = "templ",
		},
	})

	vim.lsp.config('templ', {
		on_attach = lsp_on_attach,
		flags = {
			debounce_text_changes = 150,
		},
	})

	vim.lsp.config('ocamllsp', {
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
	})

	vim.lsp.config('elixirls', {
		on_attach = lsp_on_attach,
	})

	vim.lsp.config('ts_ls', {
		on_attach = mk_lsp_on_attach { do_format_on_save = true },
	})

	local null_ls = require("null-ls")
	null_ls.setup({
		sources = {
			null_ls.builtins.formatting.gofumpt,
			null_ls.builtins.formatting.goimports_reviser,
			null_ls.builtins.diagnostics.mypy,
			null_ls.builtins.formatting.prettier,
		}
	})

	vim.lsp.config('nixd', {
		cmd = { "nixd" },
		on_attach = lsp_on_attach,
		settings = {
			nixd = {
				nixpkgs = {
					expr = "import <nixpkgs> { }",
				},
				formatting = {
					command = { "alejandra" },
				},
			},
		},
	})

	vim.lsp.config('emmet_language_server', {
		filetypes = { "css", "eruby", "html", "javascript", "javascriptreact", "less", "sass", "scss", "pug", "typescriptreact" },
		init_options = {
			includeLanguages = {},
			excludeLanguages = {},
			extensionsPath = {},
			preferences = {},
			showAbbreviationSuggestions = true,
			showExpandedAbbreviation = "always",
			showSuggestionsAsSnippets = false,
			syntaxProfiles = {},
			variables = {},
		},
	})

	vim.lsp.config('zls', {
		on_attach = lsp_on_attach,
	})

	vim.lsp.config('tinymist', {
		on_attach = lsp_on_attach,
		settings = {
			formatterMode = "typstyle",
			exportPdf = "onType",
			semanticTokens = "disable"
		}
	})

	vim.lsp.enable('janet')
	vim.lsp.enable('lua_ls')
	vim.lsp.enable('gopls')
	vim.lsp.enable('pyright')
	vim.lsp.enable('jdtls')
	vim.lsp.enable('clangd')
	vim.lsp.enable('astro')
	vim.lsp.enable('templ')
	vim.lsp.enable('ocamllsp')
	vim.lsp.enable('elixirls')
	vim.lsp.enable('ts_ls')
	vim.lsp.enable('nixd')
	vim.lsp.enable('emmet_language_server')
	vim.lsp.enable('zls')
	vim.lsp.enable('tinymist')
end

return M
