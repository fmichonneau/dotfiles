;; Magit -----------------------------------------------------------------------
(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status)
  :init
  (setq magit-diff-options (quote ("--word-diff")))
  (setq magit-diff-refine-hunk 'all)
  )

;; YASnippet 
(use-package yasnippet
  :ensure t
  :defer t
  :config (yas-global-mode t)
  )

;; == ws-butler ==
;; This cleans up any whitespace I have at the end of my lines.
(use-package ws-butler
  :ensure t
  :init
  (ws-butler-global-mode)
  :diminish ws-butler-mode
  )

;; == Projectile ==
(use-package projectile
  :ensure t
  :defer t
  :diminish projectile-mode
  :init
  (projectile-mode)
  )

;; == ag ==
;; Note that 'ag' (the silver searcher) needs to be installed.
;; Ubuntu: sudo apt-get install silversearcher-ag
;; OSX: brew install ag
(use-package ag
  :ensure t
  :defer t
  )


;; editorconfig ----------------------------------------------------------------
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; CSS colors ------------------------------------------------------------------
;; CSS color values colored by themselves
;; http://xahlee.org/emacs/emacs_html.html
(defvar hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property
	 (match-beginning 0)
	 (match-end 0)
	 'face (list :background
		     (match-string-no-properties 0)))))))

(defun hexcolour-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolour-keywords))
(add-hook 'css-mode-hook 'hexcolour-add-to-font-lock)

;; Fill to end -----------------------------------------------------------------
(defun fill-to-end ()
  (interactive)
  (save-excursion
    (end-of-line)
    (while (< (current-column) 80)
      (insert-char ?-))))


(defun fill-fence ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((n (- (line-end-position) (line-beginning-position))))
      (while (< (current-column) (- 50 n))
        (insert-char ?:))
      (if (> n 0) (insert-char ? ))
      (if (eq n 0) (insert-char ?:))
      )
    )
  )



;; Dockerfile editing ----------------------------------------------------------

(use-package dockerfile-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
  )


;; Earthfile editing

(use-package earthfile-mode
  :ensure t)

;; Aggressive-indent mode ------------------------------------------------------
(use-package aggressive-indent
  :ensure t
  :defer t
  :init
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'mhtml-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'yaml-mode)
  (add-hook 'ess-mode-hook #'(lambda ()
                               (when (and (stringp buffer-file-name)
                                          (string-match "\\.Rmd\\'" buffer-file-name))
                                 (aggressive-indent-mode 0))))
  )

;; Always use spaces for indentation
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; Color keywords
(add-hook 'prog-common-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIX\\|FIXME\\|TODO\\|BUG\\|HACK\\|\\WARNING):" 1 font-lock-warning-face t)))))

;; lsp-mode
(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (html-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  )

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)

;; Disable line wrapping and indentation in HTML mode
(add-hook 'mhtml-mode-hook
          (lambda ()
            (visual-fill-column-mode -1)
            (visual-line-mode -1)
            (electric-indent-local-mode -1)))

;; company
(use-package company
  :ensure
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
	("C-n". company-select-next)
	("C-p". company-select-previous)
	("M-<". company-select-first)
	("M->". company-select-last))
  (:map company-mode-map
	("<tab>". tab-indent-or-complete)
	("TAB". tab-indent-or-complete))
  )

;; flycheck
(use-package flycheck :ensure)
