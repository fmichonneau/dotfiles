;; yaml

(use-package yaml-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
  ;; remove auto-fill for yaml mode
  (add-hook 'yaml-mode-hook
	    '(lambda ()
	       (auto-fill-mode -1)
	       ))
  )

;; yasnippet -------------------------------------------------------------------
(use-package yasnippet
  :ensure t
  :defer t
  :init
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets"))
  )

;; polymode --------------------------------------------------------------------
(use-package poly-R
  :ensure t)

(use-package poly-markdown
  :ensure t)

(use-package poly-noweb
  :ensure t)

(use-package polymode
  :ensure t
  :init
  (require 'poly-R)
  (require 'poly-markdown)
  (require 'poly-noweb)
  :config
  (add-to-list 'auto-mode-alist
               '("\\.[rR]md\\'" . poly-markdown+r-mode))
  (add-to-list 'auto-mode-alist
               '("\\.[rR]markdown\\'" . poly-markdown+r-mode)))


(use-package quarto-mode
  :ensure t
  :mode (("\\.[qrR]md" . poly-quarto-mode))
  )


;; ESS -------------------------------------------------------------------------
(use-package ess
  :ensure t
  :init (require 'ess-site)
  :commands R
  :bind
  ;; _ is <-
  (:map ess-r-mode-map
        ("_" . ess-insert-assign))
  (:map inferior-ess-r-mode-map
        ("_" . ess-insert-assign))

  :custom
  (inferior-R-args "--quiet --no-save --no-restore")
  (ess-style 'RStudio-)
  (ess-offset-arguments 'prev-line)
  (ess-eval-visibility 'nowait)

  ;; don't ask for working directory
  (ess-ask-for-ess-directory nil)

  ;; No history no save
  (ess-history-file nil)

  ;; All help buffers are shown in one dedicated frame
  (ess-help-own-frame 'one)

  ;; Cursor always at the end of eval (from ESS-help 20110911)
  (comint-scroll-to-bottom-on-input t)
  (comint-scroll-to-bottom-on-output t)
  (comint-move-point-for-output t)
  (comint-scroll-show-maximum-output t)

  (ess-R-font-lock-keywords
   (quote
    ((ess-R-fl-keyword:modifiers . t)
     (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-R-fl-keyword:constants . t)
     (ess-fl-keyword:fun-calls . t)
     (ess-fl-keyword:numbers . t)
     (ess-fl-keyword:operators . t)
     (ess-fl-keyword:delimiters . t)
     (ess-fl-keyword:= . t)
     (ess-R-fl-keyword:F&T . t))
    )
   )
  (ess-auto-newline t)
  (ess-fill-calls-newlines t)
  (ess-function-template " <- function( ) {

}
")

  (ess-swv-processor (quote knitr))

  (ess-roxy-tags-param
   (quote
    ("author" "aliases" "concept" "details" "examples" "format" "keywords" "method" "md" "exportMethod" "name" "note" "param" "include" "references" "return" "seealso" "source" "docType" "title" "TODO" "usage" "import" "exportClass" "exportPattern" "S3method" "inheritParams" "importFrom" "importClassesFrom" "importMethodsFrom" "useDynLib" "rdname" "section" "slot" "template" "family")))

  (ess-roxy-template-alist
   (quote
    (("description" . ".. content for \\description{} (no empty lines) ..")
     ("details" . ".. content for \\details{} ..")
     ("title" . "")
     ("param" . "")
     ("return" . "")
     ("export"))))



  :config
  (setenv "IS_ESS" "true")

  ;; ;; Rd mode
  (add-to-list 'auto-mode-alist '("\\.rd\\'" . Rd-mode))
  (add-hook 'Rd-mode-hook
            #'(lambda ()
                (abbrev-mode 1)
                (font-lock-mode 1)))


  ;; redefine previous/commands
  (define-key comint-mode-map [(meta ?p)] 'comint-previous-matching-input-from-input)
  (define-key comint-mode-map [(meta ?n)] 'comint-next-matching-input-from-input)

  ;; use shift+enter for everything!
  ;; https://kieranhealy.org/blog/archives/2009/10/12/make-shift-enter-do-a-lot-in-ess/
  (defun my-ess-start-R ()
    (interactive)
    (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
        (progn
          (delete-other-windows)
          (setq w1 (selected-window))
          (setq w1name (buffer-name))
          (setq w2 (split-window w1 nil t))
          (R)
          (set-window-buffer w2 "*R*")
          (set-window-buffer w1 w1name))))
  (defun my-ess-eval ()
    (interactive)
    (my-ess-start-R)
    (if (and transient-mark-mode mark-active)
        (call-interactively 'ess-eval-region)
      (call-interactively 'ess-eval-line-and-step)))
  (add-hook 'ess-mode-hook
            '(lambda()
               (local-set-key [(shift return)] 'my-ess-eval)))
  (add-hook 'inferior-ess-mode-hook
            '(lambda()
               (local-set-key [C-up] 'comint-previous-input)
               (local-set-key [C-down] 'comint-next-input)))
  (add-hook 'Rnw-mode-hook
            '(lambda()
               (local-set-key [(shift return)] 'my-ess-eval)))


  ;; use Air to format the content of the file
  (defun run-air-on-r-save ()
    "Run Air after saving .R files and refresh buffer."
    (when (and (stringp buffer-file-name)
               (string-match "\\.R$" buffer-file-name))
      (let ((current-buffer (current-buffer)))
        (shell-command (concat "air format " buffer-file-name))
        ;; Refresh buffer from disk
        (with-current-buffer current-buffer
          (revert-buffer nil t t)))))

  (add-hook 'after-save-hook 'run-air-on-r-save)
  )

;; Key combo -------------------------------------------------------------------
(use-package key-combo
  :config
  (global-key-combo-mode t)
  ;;(key-combo-define-global "C-a" `(back-to-indentation move-beginning-of-line
  ;;                                 beginning-of-buffer key-combo-return))
  ;;(key-combo-define-global "C-e" '(move-end-of-line end-of-buffer key-combo-return))

  (defvar key-combo-ess-default
    '(
      (">"   . (" > " " |> " " >> "))
      ("d"   . ("d" "dd" ".data$"))
      ("t"   . ("t" "tt" "`tidyverse`"))
      ("$"   . ("$" " %$% "))
      ("<>"  . " %<>% ")
      ("+"   . (" + " "+"))
      ("-"   . (" - " "-"))
      ("*"   . (" * " "*"))
      ("^"   . ("^" " ^ "))
      ("/"   . ("/" " / "))
      ("~"   . ("~" " ~ "))
      (":"   . (":" "::" ":::"))
      (":="  . " := ") ; data.table
      ("I"   . ("I" " %in% "))
      ("->"  . " -> ")))

  (key-combo-define-hook '(ess-mode-hook inferior-ess-mode-hook)
		         'ess-key-combo-load-default
		         key-combo-ess-default)
  )


;; Start R in Docker
;; TODO need to figure password
(defun R-docker ()
  (interactive)
  (let ((ess-r-customize-alist
         (append ess-r-customize-alist
                 '((inferior-ess-program . "/home/francois/start-r-docker.sh"))))
        (ess-R-readline t))
    (R)))

;; R process in its own window (to the right)
(defun fm/r()
  "start R with a reasonable layout."
  (interactive)
  ;; Create new window right of the current one
  ;; Current window is 80 characters (columns) wide
  (split-window-right 120)
  ;; Go to next window
  (other-window 1)
  ;; Create new window below current one
  (split-window-below)
  ;; Start R in current window
  (R)
  ;; Go to previous window
  (other-window -1)
  ;; never open any buffer in window with shell
  (set-window-dedicated-p (nth 1 (window-list)) t))

;; ESS gd to display plots within an Emacs buffer
(use-package essgd
  :straight (:type git :host github :repo "sje30/essgd"))
