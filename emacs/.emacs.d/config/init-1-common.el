(fset 'yes-or-no-p 'y-or-n-p)
(normal-erase-is-backspace-mode 1)

;; Put all backups in the same place
(setq backup-directory-alist `(("." . "~/.emacs.d/auto-backups")))



;; File name in title bar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; parens matching
(require 'paren)
(show-paren-mode 1)
(setq-default hilight-paren-expression t)

;; automatically close parens
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

;; always revert image buffer
;; i.e., to keep R-generated plots up to date
(add-hook 'image-mode-hook
          (lambda ()
            (auto-revert-mode)
            (auto-image-file-mode)))
