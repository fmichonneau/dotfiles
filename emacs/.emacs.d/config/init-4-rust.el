;; rust

(use-package rustic
  :ensure t
  :defer t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil)))
  ;; autoformat on save
  (setq rust-format-on-save t)
  ;; symbol prettify
  (add-hook 'rust-mode-hook
            (lambda () (prettify-symbols-mode)))

  (add-hook 'rust-mode-hook #'lsp)
  )
