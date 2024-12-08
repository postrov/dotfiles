local wezterm = require 'wezterm'
local config = wezterm.config_builder()
local act = wezterm.action

config.font = wezterm.font 'Cascadia Mono PL'
config.font_size = 11.0
config.window_background_opacity = 0.75
-- config.max_fps = 500
-- This is where you actually apply your config choices

-- For example, changing the color scheme:
-- config.color_scheme = 'AdventureTime'
config.color_scheme = 'Catppuccin Mocha'
-- config.color_scheme = 'CGA'

config.enable_wayland = true
config.leader = {
	key = 'b',
	mods = 'CTRL',
	timeout_milliseconds = 2000,
}
config.tab_bar_at_bottom = true
config.enable_tab_bar = true
config.keys = {
	{
		key = 'c',
		mods = 'LEADER',
		action = wezterm.action.SpawnTab 'CurrentPaneDomain',
	},
	{
		key = '[',
		mods = 'LEADER',
		action = wezterm.action.ActivateCopyMode,
	},
	{
		key = 'n',
		mods = 'LEADER',
		action = wezterm.action.ActivateTabRelative(1),
	},
	{
		key = 'p',
		mods = 'LEADER',
		action = wezterm.action.ActivateTabRelative(-1),
	},
	{
		key = ',',
		mods = 'LEADER',
		action = wezterm.action.PromptInputLine {
			description = 'Enter new name for tab',
			action = wezterm.action_callback(
				function(window, pane, line)
					if line then
						window:active_tab():set_title(line)
					end
				end
			),
		},
	},
	{
		key = 'w',
		mods = 'LEADER',
		action = act.ShowTabNavigator,
	},
	{
		-- |
		key = '%',
		mods = 'LEADER|SHIFT',
		action = act.SplitPane {
			direction = 'Right',
			size = { Percent = 50 },
		},
	},
	-- Horizontal split
	{
		-- -
		key = '"',
		mods = 'LEADER|SHIFT',
		action = act.SplitPane {
			direction = 'Down',
			size = { Percent = 50 },
		},
	},
	-- Attach to muxer
	{
		key = 'a',
		mods = 'LEADER',
		action = act.AttachDomain 'unix',
	},

	-- Detach from muxer
	{
		key = 'd',
		mods = 'LEADER',
		action = act.DetachDomain { DomainName = 'unix' },
	},
	-- Rename current session; analagous to command in tmux
	{
		key = '$',
		mods = 'LEADER|SHIFT',
		action = act.PromptInputLine {
			description = 'Enter new name for session',
			action = wezterm.action_callback(
				function(window, pane, line)
					if line then
						wezterm.mux.rename_workspace(
							window:mux_window():get_workspace(),
							line
						)
					end
				end
			),
		},
	},
	-- Show list of workspaces
	{
		key = 's',
		mods = 'LEADER',
		action = act.ShowLauncherArgs { flags = 'WORKSPACES' },
	},
	-- Session manager bindings
	{
		key = 's',
		mods = 'LEADER|SHIFT',
		action = act({ EmitEvent = "save_session" }),
	},
	{
		-- this is not implemented yet (supposed to give a choice of session to load)
		key = 'L',
		mods = 'LEADER|SHIFT',
		action = act({ EmitEvent = "load_session" }),
	},
	{
		key = 'R',
		mods = 'LEADER|SHIFT',
		action = act({ EmitEvent = "restore_session" }),
	},
	{
		key = 'h',
		mods = 'LEADER',
		action = act.ActivatePaneDirection 'Left',
	},
	{
		key = 'l',
		mods = 'LEADER',
		action = act.ActivatePaneDirection 'Right',
	},
	{
		key = 'j',
		mods = 'LEADER',
		action = act.ActivatePaneDirection 'Down',
	},
	{
		key = 'k',
		mods = 'LEADER',
		action = act.ActivatePaneDirection 'Up',
	},
	{
		key = 'z',
		mods = 'LEADER',
		action = wezterm.action.TogglePaneZoomState,
	},
}

for i = 1, 9 do
	table.insert(config.keys, {
		key = tostring(i),
		mods = "LEADER",
		action = act.ActivateTab(i - 1),
	})
end

-- local direction_keys = {
--     h = "Left",
--     j = "Down",
--     k = "Up",
--     l = "Right",
-- }



-- Make it look like tabs, with better GUI controls
config.use_fancy_tab_bar = true
-- Don't let any individual tab name take too much room
config.tab_max_width = 32
-- config.colors = {
-- 	tab_bar = {
-- 		active_tab = {
-- 			-- I use a solarized dark theme; this gives a teal background to the active tab
-- 			fg_color = '#073642',
-- 			bg_color = '#2aa198'
-- 		}
-- 	}
-- }
config.window_frame = {
	font_size = 8.0,
	font = wezterm.font { family = 'Cascadia Mono PL', weight = 'Bold' }
}
-- Switch to the last active tab when I close a tab
config.switch_to_last_active_tab_when_closing_tab = true


config.window_padding = {
	left = 0,
	right = 0,
	top = 0,
	bottom = 0,
}

-- and finally, return the configuration to wezterm
config.unix_domains = {
	{
		name = 'unix',
	},
}

-- session persistence
local session_manager = require 'wezterm-session-manager/session-manager'
wezterm.on("save_session", function(window) session_manager.save_state(window) end)
wezterm.on("load_session", function(window) session_manager.load_state(window) end)
wezterm.on("restore_session", function(window) session_manager.restore_state(window) end)

return config
