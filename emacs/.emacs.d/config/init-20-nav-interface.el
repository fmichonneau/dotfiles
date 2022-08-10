;;; init-20-nav-interface --- general commands for code and text edition

;;; Commentary:
;;;   - general edition

;;; Code:
(require 'use-package)

;; Parenthese matching, permet de verifier au fur et à mesure de la frappe que
;; l'on ferme bien ce que l'on ouvre, aussi bien pour les parenthèses que les
;; crochets ou les accolades.
(require 'paren)
(show-paren-mode 1)
(setq-default hilight-paren-expression t)


;; Ferme automatiquement parenthèses, crochets, guillemets
;; au cours de la frappe
(setq skeleton-pair t)
(global-set-key "(" 'skeleton-pair-insert-maybe)
(global-set-key "[" 'skeleton-pair-insert-maybe)
(global-set-key "\"" 'skeleton-pair-insert-maybe)
(global-set-key "{" 'skeleton-pair-insert-maybe)

;;; Delete empty pairs of characters
(defvar skeletons-alist
  '((?\( . ?\))
    (?\' . ?\')
    (?\" . ?\")
    (?[  . ?])
    (?{  . ?})
    (?$  . ?$)))

(defadvice delete-backward-char (before delete-empty-pair activate)
  (if (eq (cdr (assq (char-before) skeletons-alist)) (char-after))
      (and (char-after) (delete-char 1))))

;; Delete beginning of line
;; from: https://stackoverflow.com/questions/3880731/delete-line-backwards-emacs
(global-set-key "\M-k" '(lambda () (interactive) (kill-line 0)) ) ;M-k kills to the left

;; converts an Emacs paragraph into a single line
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(defun center-text ()
  (interactive)
  (setq visual-fill-column-center-text 't)
  (redraw-display)
  )

(define-key global-map "\M-Q" 'unfill-paragraph)
(define-key global-map "\C-\M-q" 'unfill-region)

;; from: https://stackoverflow.com/questions/9597391/emacs-move-point-to-last-non-whitespace-character
(defun my-move-end-of-line-before-whitespace ()
  "Move to the last non-whitespace character in the current line."
  (interactive)
  (move-end-of-line nil)
  (re-search-backward "^\\|[^[:space:]]"))

(define-key global-map "\C-e" 'my-move-end-of-line-before-whitespace)

;;; use visual fill for text width
(use-package visual-fill-column
  :ensure t
  :init
  (dolist (hook '(text-mode-hook emacs-lisp-mode-hook))
    (add-hook hook #'visual-fill-column-mode))
  :config
  ;; remove the margin before splitting buffers
  (setq split-window-preferred-function "visual-fill-column-split-window-sensibly")
  (add-hook 'text-mode-hook 'turn-on-visual-line-mode)
  (add-hook 'polymode-hook 'turn-on-viusal-line-mode)
  (add-hook 'poly-head-tail-mode-hook 'turn-on-viusal-line-mode)
  (add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
  )

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook emacs-lisp-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode))
  )

(use-package tramp
  :ensure t
  :defer t
  )

;; == Evil Mode ==
(use-package evil
  :ensure t
  ;;:diminish undo-tree-mode
  :config
  :init
  (evil-mode 0)
  (define-key evil-ex-map "b " 'helm-mini)
  (define-key evil-ex-map "e " 'helm-find-files)

  ;; Resolve sentence jumping issue
  (setq sentence-end-double-space nil)
  )

;; (use-package general
;;   :ensure t
;;   :after evil
;;   )

(use-package neotree
  :ensure t
  :defer t
  :config
  (global-set-key [f8] 'neotree-toggle)
  )

;; File name in title bar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
