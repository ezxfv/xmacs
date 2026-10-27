-- Final, non-plugin-specific behavior.
vim.filetype.add {
  extension = {
    gotmpl = "gotmpl",
    tpl = "gotmpl",
  },
  filename = {
    [".envrc"] = "sh",
    ["Dockerfile"] = "dockerfile",
  },
  pattern = {
    [".*%.env%..*"] = "sh",
    [".*helmfile.*%.ya?ml"] = "yaml",
  },
}
