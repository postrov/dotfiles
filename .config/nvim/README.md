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
#### JAVA
- roughly works, `:lua require('pasza.jj').jjattach()`
- project root, `.classpath` entries are needed for dependencies: ``
`<classpathentry kind="lib" path="/home/ttt/.m2/repository/org/apache/commons/commons-lang3/3.8.1/commons-lang3-3.8.1.jar"/>`
 
### TODO
* keymaps for LSP
* make common setup common (custom_attach?)
* debugger rust/java
 
