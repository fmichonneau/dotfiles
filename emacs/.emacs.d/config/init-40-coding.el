;;; Commentary:

;;; Code:

;; == company-mode ==
(use-package company
  :ensure t
  :defer t
  :after general
  :diminish company-mode
  :init (add-hook 'after-init-hook 'global-company-mode)
  (general-define-key
   :keymaps 'company-active-map
   "C-j" 'company-select-next
   "C-k" 'company-select-previous
   "C-l" 'company-complete-selection)

  :config
  (use-package company-irony :ensure t :defer t)
  (setq company-idle-delay              nil
	company-minimum-prefix-length   2
	company-show-numbers            t
	company-tooltip-limit           20
	company-dabbrev-downcase        nil
	)
  )


;; === flx-ido ===
(use-package flx-ido
  :ensure t
  :init
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  :config
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)
  (setq gc-cons-threshold 20000000)
  )

;; === Tools ===

;; == YASnippet ==
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


;; == compile ==

;; https://emacs.stackexchange.com/questions/8135/why-does-compilation-buffer-show-control-characters
(use-package ansi-color
  :ensure t
  :defer t
  :config (progn
            (defun my/ansi-colorize-buffer ()
              (let ((buffer-read-only nil))
                (ansi-color-apply-on-region (point-min) (point-max))))
            (add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)))

;; Magit -----------------------------------------------------------------------
(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status)
  :init
  (setq magit-diff-options (quote ("--word-diff")))
  (setq magit-diff-refine-hunk 'all)
  )

;; flycheck --------------------------------------------------------------------
;; (use-package flycheck
;;   :ensure t
;;   :diminish flycheck-mode
;;   :init
;;   (add-hook 'after-init-hook #'global-flycheck-mode)
;;   )

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
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)

;; Disable line wrapping and indentation in HTML mode
(add-hook 'mhtml-mode-hook
          (lambda ()
            (visual-fill-column-mode -1)
            (visual-line-mode -1)
            (electric-indent-local-mode -1)
            (toggle-truncate-lines -1)))
