---@type LazySpec
return {
  {
    "AstroNvim/astrocore",
    ---@type AstroCoreOpts
    opts = {
      features = {
        large_buf = { size = 1024 * 256, lines = 10000 },
        autopairs = true,
        cmp = true,
        diagnostics_mode = 3,
        highlighturl = true,
        notifications = true,
      },
      diagnostics = {
        virtual_text = true,
        underline = true,
        update_in_insert = false,
      },
      options = {
        opt = {
          clipboard = "unnamedplus",
          confirm = true,
          cursorline = true,
          expandtab = true,
          ignorecase = true,
          mouse = "a",
          number = true,
          relativenumber = true,
          scrolloff = 6,
          shiftwidth = 2,
          signcolumn = "yes",
          smartcase = true,
          softtabstop = 2,
          splitbelow = true,
          splitright = true,
          tabstop = 2,
          termguicolors = true,
          timeoutlen = 400,
          undofile = true,
          updatetime = 200,
          wrap = false,
        },
      },
      mappings = {
        n = {
          -- Doom-style high-frequency navigation.
          ["<Leader>SPC"] = { "<Cmd>lua require('snacks').picker.commands()<CR>", desc = "Commands" },
          ["<Leader>0"] = { "<Cmd>Neotree focus<CR>", desc = "Focus explorer" },
          ["<Leader>j"] = {
            function() require("snacks").picker.lines() end,
            desc = "Jump to line",
          },
          ["<Leader>v"] = { "<Cmd>vsplit<CR>", desc = "Vertical split" },

          -- Buffer and project switching.
          ["<Leader>,"] = {
            function() require("snacks").picker.buffers() end,
            desc = "Switch buffer",
          },
          ["<Leader>pp"] = {
            function() require("snacks").picker.projects() end,
            desc = "Switch project",
          },
          ["<Leader>pf"] = {
            function() require("snacks").picker.files() end,
            desc = "Find project file",
          },
          ["<Leader>ps"] = {
            function() require("snacks").picker.grep() end,
            desc = "Search project text",
          },

          -- Diff workflow.
          ["<Leader>gd"] = { "<Cmd>DiffviewOpen<CR>", desc = "Working tree diff" },
          ["<Leader>gD"] = { "<Cmd>DiffviewClose<CR>", desc = "Close diff" },
          ["<Leader>gh"] = { "<Cmd>DiffviewFileHistory %<CR>", desc = "File history" },

          -- Keep the familiar format/diagnostics vocabulary from doom-v2.
          ["<Leader>cf"] = {
            function() require("astrolsp").format() end,
            desc = "Format buffer",
          },
          ["<Leader>cd"] = {
            function() require("snacks").picker.diagnostics_buffer() end,
            desc = "Buffer diagnostics",
          },
          ["<Leader>cD"] = {
            function() require("snacks").picker.diagnostics() end,
            desc = "Workspace diagnostics",
          },
        },
        v = {
          ["<Leader>cf"] = {
            function() require("astrolsp").format() end,
            desc = "Format selection",
          },
        },
      },
    },
  },
  {
    "AstroNvim/astroui",
    ---@type AstroUIOpts
    opts = {
      colorscheme = "astrodark",
    },
  },
  {
    "AstroNvim/astrolsp",
    ---@type AstroLSPOpts
    opts = {
      features = {
        codelens = true,
        inlay_hints = false,
        semantic_tokens = true,
      },
      formatting = {
        format_on_save = { enabled = true },
        timeout_ms = 3000,
      },
    },
  },
  {
    "nvim-neo-tree/neo-tree.nvim",
    opts = {
      window = { width = 30 },
      filesystem = {
        follow_current_file = { enabled = true },
        filtered_items = {
          hide_dotfiles = false,
          hide_gitignored = true,
        },
      },
    },
  },
  {
    "folke/snacks.nvim",
    opts = {
      dashboard = { enabled = true },
      picker = {
        hidden = true,
        sources = {
          files = { hidden = true },
          grep = { hidden = true },
        },
      },
    },
  },
  {
    "mfussenegger/nvim-dap",
    dependencies = {
      {
        "rcarriga/nvim-dap-ui",
        opts = {
          layouts = {
            {
              elements = {
                { id = "scopes", size = 0.55 },
                { id = "breakpoints", size = 0.20 },
                { id = "stacks", size = 0.25 },
              },
              position = "left",
              size = 45,
            },
            {
              elements = {
                { id = "repl", size = 0.5 },
                { id = "console", size = 0.5 },
              },
              position = "bottom",
              size = 12,
            },
          },
        },
      },
    },
  },
  {
    "mg979/vim-visual-multi",
    branch = "master",
    init = function()
      vim.g.VM_theme = "iceblue"
      vim.g.VM_maps = {
        ["Find Under"] = "<C-n>",
        ["Find Subword Under"] = "<C-n>",
        ["Select All"] = "<C-a>",
        ["Skip Region"] = "<C-x>",
        ["Remove Region"] = "<C-p>",
        ["Add Cursor Down"] = "<M-j>",
        ["Add Cursor Up"] = "<M-k>",
      }
    end,
  },
  {
    "akinsho/toggleterm.nvim",
    opts = {
      persist_mode = true,
      persist_size = true,
      start_in_insert = true,
      winbar = {
        enabled = true,
        name_formatter = function(term) return ("Terminal %d · %s"):format(term.id, term:_display_name()) end,
      },
    },
    dependencies = {
      {
        "AstroNvim/astrocore",
        opts = {
          mappings = {
            n = {
              ["<Leader>tN"] = {
                function()
                  local Terminal = require("toggleterm.terminal").Terminal
                  Terminal:new { direction = "float", dir = "git_dir" }:toggle()
                end,
                desc = "New terminal",
              },
              ["<Leader>ts"] = { "<Cmd>TermSelect<CR>", desc = "Select terminal" },
              ["<Leader>tr"] = { "<Cmd>ToggleTermSetName<CR>", desc = "Rename terminal" },
              ["<Leader>ta"] = { "<Cmd>ToggleTermToggleAll<CR>", desc = "Toggle all terminals" },
              ["<Leader>tq"] = {
                function()
                  local terms = require "toggleterm.terminal"
                  local _, term = terms.identify()
                  if not term then
                    vim.notify("Current buffer is not a managed terminal", vim.log.levels.WARN)
                    return
                  end
                  term:shutdown()
                end,
                desc = "Delete current terminal",
              },
              ["<Leader>t1"] = { "<Cmd>1ToggleTerm<CR>", desc = "Toggle terminal 1" },
              ["<Leader>t2"] = { "<Cmd>2ToggleTerm<CR>", desc = "Toggle terminal 2" },
              ["<Leader>t3"] = { "<Cmd>3ToggleTerm<CR>", desc = "Toggle terminal 3" },
              ["<Leader>t4"] = { "<Cmd>4ToggleTerm<CR>", desc = "Toggle terminal 4" },
            },
            t = {
              ["<Esc><Esc>"] = { [[<C-\><C-n>]], desc = "Terminal normal mode" },
            },
          },
        },
      },
    },
  },
  {
    "rest-nvim/rest.nvim",
    dependencies = {
      "j-hui/fidget.nvim",
      {
        "AstroNvim/astrocore",
        opts = {
          mappings = {
            n = {
              ["<Leader>re"] = { "<Cmd>Rest env select<CR>", desc = "Select HTTP environment" },
              ["<Leader>rE"] = { "<Cmd>Rest env show<CR>", desc = "Show HTTP environment" },
            },
          },
        },
      },
    },
  },
  {
    "kristijanhusak/vim-dadbod-ui",
    dependencies = {
      {
        "AstroNvim/astrocore",
        opts = {
          options = {
            g = {
              db_ui_execute_on_save = 0,
              db_ui_show_database_icon = 1,
              db_ui_use_nerd_fonts = 1,
            },
          },
          mappings = {
            n = {
              ["<Leader>D"] = { desc = "Database" },
              ["<Leader>Du"] = { "<Cmd>DBUIToggle<CR>", desc = "Toggle database UI" },
              ["<Leader>Da"] = { "<Cmd>DBUIAddConnection<CR>", desc = "Add database connection" },
              ["<Leader>Df"] = { "<Cmd>DBUIFindBuffer<CR>", desc = "Find database buffer" },
            },
          },
        },
      },
    },
  },
  {
    "tpope/vim-dadbod",
    dependencies = {
      {
        "AstroNvim/astrocore",
        opts = {
          autocmds = {
            dadbod_sql_mappings = {
              {
                event = "FileType",
                pattern = { "sql", "mysql", "plsql" },
                desc = "Configure SQL execution mappings",
                callback = function(args)
                  vim.keymap.set("n", "<LocalLeader>e", function()
                    local node = vim.treesitter.get_node { bufnr = args.buf }
                    while node and not node:type():match "statement$" do
                      node = node:parent()
                    end
                    if not node then
                      vim.notify("No SQL statement found at cursor", vim.log.levels.ERROR)
                      return
                    end

                    local start_row, start_col, end_row, end_col = node:range()
                    local lines = vim.api.nvim_buf_get_text(args.buf, start_row, start_col, end_row, end_col, {})
                    local query = table.concat(lines, "\n")
                    if vim.trim(query) == "" then
                      vim.notify("SQL statement is empty", vim.log.levels.ERROR)
                      return
                    end
                    vim.cmd { cmd = "DB", args = { query } }
                  end, { buffer = args.buf, desc = "Execute SQL statement at cursor" })

                  vim.keymap.set("x", "<LocalLeader>e", ":DB<CR>", {
                    buffer = args.buf,
                    desc = "Execute selected SQL",
                    silent = true,
                  })
                  vim.keymap.set("n", "<LocalLeader>b", "<Cmd>%DB<CR>", {
                    buffer = args.buf,
                    desc = "Execute SQL buffer",
                    silent = true,
                  })
                end,
              },
            },
          },
        },
      },
    },
  },
  {
    "nvim-treesitter/nvim-treesitter",
    opts = {
      ensure_installed = {
        "comment",
        "diff",
        "git_config",
        "git_rebase",
        "gitattributes",
        "gitcommit",
        "gitignore",
        "query",
        "regex",
        "vim",
        "vimdoc",
      },
    },
  },
}
