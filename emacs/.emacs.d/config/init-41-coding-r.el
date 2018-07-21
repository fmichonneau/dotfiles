;;; init-41-coding-r.el --- R and friends

;;; Commentary:
;;  - the tools needed to edit R and files that contain R

;;; Code:

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

;; polymode --------------------------------------------------------------------
(use-package polymode
  :ensure t
  :init
  (require 'poly-R)
  (require 'poly-markdown)
  :config
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rpres" . poly-markdown+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rhtml" . poly-html+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rcpp" . poly-r+c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cppR" . poly-c++r-mode))
  )

;; ESS -------------------------------------------------------------------------
(use-package ess
  :ensure t
  :commands R
  :config
  (setq ess-eval-visibly t)

  ;; R process in its own buffer
  (setq inferior-ess-same-window nil)


  ;; ESS style
  (setq ess-default-style 'RStudio)

  ;; All help buffers are shown in one dedicated frame
  (setq ess-help-own-frame 'one)

  ;; Rd mode
  (add-to-list 'auto-mode-alist '("\\.rd\\'" . Rd-mode))
  (add-hook 'Rd-mode-hook
	    (lambda ()
	      (abbrev-mode 1)
	      (font-lock-mode 1)))
  ;; Cursor always at the end of eval (from ESS-help 20110911)
  (setq comint-scroll-to-bottom-on-input t)
  (setq comint-scroll-to-bottom-on-output t)
  (setq comint-move-point-for-output t)
  (setq comint-scroll-show-maximum-output t)

  ;; redefine previous/commands
  (define-key comint-mode-map [(meta ?p)] 'comint-previous-matching-input-from-input)
  (define-key comint-mode-map [(meta ?n)] 'comint-next-matching-input-from-input)
  )

;; Key combo -------------------------------------------------------------------
(use-package key-combo
  :config
  (global-key-combo-mode t)
  ;;(key-combo-define-global "C-a" `(back-to-indentation move-beginning-of-line
  ;;                                 beginning-of-buffer key-combo-return))
  ;;(key-combo-define-global "C-e" '(move-end-of-line end-of-buffer key-combo-return))

  (defvar key-combo-ess-default
    (append key-combo-common-default
	    '((">"   . (" > " " %>% "))
	      ("$"   . ("$" " %$% "))
	      ("<>"  . " %<>% ")
	      ("+"   . ("+" " + " "+"))
	      ("-"   . ("-" " - " "-"))
	      ("*"   . ("*" " * "))
	      ("^"   . ("^" " ^ "))
	      ("/"   . ("/" " / "))
	      ("~"   . ("~" " ~ "))
	      (":"   . (":" "::" ":::"))
	      (":="  . " := ") ; data.table
	      ("I"   . ("I" " %in% "))
	      ("->"  . " -> "))))
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
