## neovim setup

### Package management
* Done using packer
* edit ~/.config/nvim/lua/pasza/packer.lua
* :so
* :PackerSync

### LSP

1. Install lsp server for given language
    - lua: sumneko_lua
    - python: pywright
    - rust: rust-analyzer
2. Adjust paths if necessary (after/plugin/{lang}.lua)

### TODO
* keymaps for LSP
* make common setup common (custom_attach?)
* setup java


