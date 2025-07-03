;;; keybindings/languages.el -*- lexical-binding: t; -*-

;; Org-mode 快捷键配置
(after! org
  (map! :localleader
        :map org-mode-map
        :desc "Eval Block" "e" 'ober-eval-block-in-repl
        (:prefix "s"
         :desc "Tags" "t" 'org-set-tags
         :desc "Roam Bibtex" "b" 'orb-note-actions
         (:prefix ("p" . "Properties")
          :desc "Set" "s" 'org-set-property
          :desc "Delete" "d" 'org-delete-property
          :desc "Actions" "a" 'org-property-action
          )
         )
        (:prefix ("l" . "Insert")
         :desc "Link/Image" "l" 'org-insert-link
         :desc "Item" "o" 'org-toggle-item
         :desc "Citation" "c" 'org-ref-helm-insert-cite-link
         :desc "Footnote" "f" 'org-footnote-action
         :desc "Table" "t" 'org-table-create-or-convert-from-region
         :desc "Screenshot" "s" 'org-download-screenshot
         (:prefix ("m" . "Math")
          :desc "Bold" "f" 'org-make-bold-math
          :desc "Blackboard" "b" 'org-make-blackboard-math
          :desc "Vert" "v" 'org-make-vert-math
          )
         (:prefix ("h" . "Headings")
          :desc "Normal" "h" 'org-insert-heading
          :desc "Todo" "t" 'org-insert-todo-heading
          (:prefix ("s" . "Subheadings")
           :desc "Normal" "s" 'org-insert-subheading
           :desc "Todo" "t" 'org-insert-todo-subheading
           )
          )
         (:prefix ("e" . "Exports")
          :desc "Dispatch" "d" 'org-export-dispatch
          )
         )
        )
  )

;; Markdown 快捷键配置
(map! :localleader
      :map markdown-mode-map
      :prefix ("i" . "Insert")
      :desc "Blockquote"    "q" 'markdown-insert-blockquote
      :desc "Bold"          "b" 'markdown-insert-bold
      :desc "Code"          "c" 'markdown-insert-code
      :desc "Emphasis"      "e" 'markdown-insert-italic
      :desc "Footnote"      "f" 'markdown-insert-footnote
      :desc "Code Block"    "s" 'markdown-insert-gfm-code-block
      :desc "Image"         "i" 'markdown-insert-image
      :desc "Link"          "l" 'markdown-insert-link
      :desc "List Item"     "n" 'markdown-insert-list-item
      :desc "Pre"           "p" 'markdown-insert-pre
      (:prefix ("h" . "Headings")
       :desc "One"   "1" 'markdown-insert-atx-1
       :desc "Two"   "2" 'markdown-insert-atx-2
       :desc "Three" "3" 'markdown-insert-atx-3
       :desc "Four"  "4" 'markdown-insert-atx-4
       :desc "Five"  "5" 'markdown-insert-atx-5
       :desc "Six"   "6" 'markdown-insert-atx-6)) 