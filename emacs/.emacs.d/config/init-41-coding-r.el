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
(use-package poly-markdown)

(use-package poly-R
  :config
  (require 'ess-mode))


;; ESS -------------------------------------------------------------------------
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


(use-package ess-r-mode
  :ensure ess
  :commands R
  :bind
  ;; _ is <-
  (:map ess-r-mode-map
        ("_" . ess-insert-assign))
  (:map inferior-ess-r-mode-map
        ("_" . ess-insert-assign))

  :config
  (setenv "IS_ESS" "true")
  (setq ess-eval-visibly 'nowait)

  ;; don't ask for working directory
  (setq ess-ask-for-ess-directory nil)

  ;; No history no save
  (setq ess-history-file nil)
  (setq inferior-R-args "--no-restore-history --no-save ")

  ;; ESS style
  (setq ess-style 'RStudio-)
  (setq ess-offset-arguments 'prev-line)

  ;; All help buffers are shown in one dedicated frame
  (setq ess-help-own-frame 'one)

  ;; ;; Rd mode
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
