---@type LazySpec
return {
  "AstroNvim/astrocommunity",

  -- Deterministic verification loop: diagnostics, tests, and project tasks.
  { import = "astrocommunity.diagnostics.trouble-nvim" },
  { import = "astrocommunity.test.neotest" },
  { import = "astrocommunity.code-runner.overseer-nvim" },

  -- HTTP requests and database exploration from source files.
  { import = "astrocommunity.programming-language-support.rest-nvim" },
  { import = "astrocommunity.pack.full-dadbod" },
  { import = "astrocommunity.pack.sql" },

  -- High-signal editing tools that fill gaps without replacing core navigation.
  { import = "astrocommunity.search.grug-far-nvim" },

  -- Language support matching the non-AI programming stack in doom-v2.
  { import = "astrocommunity.pack.go" },
  { import = "astrocommunity.pack.python" },
  { import = "astrocommunity.pack.typescript" },
  { import = "astrocommunity.pack.rust" },
  { import = "astrocommunity.pack.lua" },
  { import = "astrocommunity.pack.bash" },
  { import = "astrocommunity.pack.json" },
  { import = "astrocommunity.pack.yaml" },
  { import = "astrocommunity.pack.docker" },
  { import = "astrocommunity.pack.markdown" },

  -- Traditional editor workflows: projects, diffs, and debugger feedback.
  { import = "astrocommunity.project.project-nvim" },
  { import = "astrocommunity.git.diffview-nvim" },
  { import = "astrocommunity.debugging.nvim-dap-virtual-text" },
}
