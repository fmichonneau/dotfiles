;; Code:
(require 'use-package)

;; Splash Screen to Org-mode
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'markdown-mode)

;; == Load Custom Theme ==

(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t)
  )

;; ;; focus-mode (dims surrounding sections of buffer)
;; (use-package focus
;;   :ensure t
;;   :config
;;   (focus-mode 1)
;;   )

;; Beacon mode
;; (use-package beacon
;;   :ensure t
;;   :config
;;   (beacon-mode 1)
;;   )


;; consolas font
(add-to-list 'default-frame-alist
             '(font . "Consolas-10"))
(set-cursor-color "#aaccee")

;; icons
(use-package all-the-icons
  :ensure t
  )

;; emoji
(use-package emojify
  :ensure t
  :hook (after-init . global-emojify-mode)
  )

;; Set default fill column
(setq-default fill-column 80)

;; quiet, please! No dinging!
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; Disable menu bars, etc.
(if window-system (scroll-bar-mode -1))
(tool-bar-mode -1)
(menu-bar-mode -1)

;; No Backup Files
(setq make-backup-files nil)

;; ;; initial window
;; (setq initial-frame-alist
;;       '((width . 102)   ; characters in a line
;;         (height . 54))) ; number of lines

;; ;; sebsequent frame
;; (setq default-frame-alist
;;       '((width . 100)   ; characters in a line
;;         (height . 52))) ; number of lines

                                        ; open emacs full screen
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; abbrev don't ask
(setq save-abbrevs 'silently)

;; Diminish extraneous info in the modeline
(diminish 'abbrev-mode)
(defun sk/diminish-auto-revert ()
  "Diminishes the 'auto-revert-mode' in the mode line."
  (interactive)
  (diminish 'auto-revert-mode ""))
(add-hook 'auto-revert-mode-hook 'sk/diminish-auto-revert)

;;; Customize the modeline
(setq line-number-mode 1)
(setq column-number-mode 1)
(setq ns-use-srgb-colorspace nil)
(use-package spaceline-config
  :config
  ;; Set some parameters of the spaceline
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  

  ;; Define a better buffer position line
  (spaceline-define-segment gjstein-buffer-position
    "a better buffer position display"
    (let ((buffer-position (format-mode-line "%p")))
      (if (string= buffer-position "Top") "top"
  	(if (string= buffer-position "Bottom") "bot"
  	  (if (string= buffer-position "All") "all"
  	    "%p")))
      )
    )

  ;; Removes the " Git:" from the 'version-control' segment.
  (spaceline-define-segment gjstein-version-control
    "Version control information."
    (when vc-mode
      (powerline-raw
       (s-trim (concat
  		(let ((backend (symbol-name (vc-backend (buffer-file-name)))))
  		  (substring vc-mode (+ (length backend) 2)))
  		(when (buffer-file-name)
  		  (pcase (vc-state (buffer-file-name))
  		    (`up-to-date " ")
  		    (`edited "*")
  		    (`added "@")
  		    (`unregistered "?")
  		    (`removed "-")
  		    (`needs-merge " Con")
  		    (`needs-update " Upd")
  		    (`ignored "!")
  		    (_ " Unk"))))))))

  ;; Makes a shorter org-clock string.
  (defun gjstein-org-clock-get-clock-string ()
    "Makes a clock string for org."
    (let ((clocked-time (org-clock-get-clocked-time)))
      (if org-clock-effort
  	  (let* ((effort-in-minutes
  		  (org-duration-string-to-minutes org-clock-effort))
  		 (work-done-str
  		  (propertize
  		   (org-minutes-to-clocksum-string clocked-time)
  		   'face (if (and org-clock-task-overrun (not org-clock-task-overrun-text))
  			     'org-mode-line-clock-overrun 'org-mode-line-clock)))
  		 (effort-str (org-minutes-to-clocksum-string effort-in-minutes))
  		 (clockstr (propertize
  			    (concat  "%s/" effort-str
  				     " " (replace-regexp-in-string "%" "%%" org-clock-heading))
  			    'face 'org-mode-line-clock)))
  	    (format clockstr work-done-str))
  	(propertize (concat (org-minutes-to-clocksum-string clocked-time)
  			    (format " %s" org-clock-heading))
  		    'face 'org-mode-line-clock))))
  (setq spaceline-org-clock-format-function 'gjstein-org-clock-get-clock-string)

  (spaceline-compile
   'gjstein
   ;; Left side of the mode line (all the important stuff)
   '(((buffer-modified buffer-size input-method) :face highlight-face)
     '(buffer-id remote-host major-mode)
     ((point-position line-column gjstein-buffer-position) :separator "|" )
     process
     ((flycheck-error flycheck-warning flycheck-info) :separator "" :when active)
     ((which-function projectile-root (gjstein-version-control :when active)) :separator ":")
     )
   ;; Right segment (the unimportant stuff)
   '((org-clock)
     ((minor-modes :separator " ") :when active)
     (mu4e-alert-segment)))

  (spaceline-helm-mode)
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-gjstein)))))

(use-package powerline
  :ensure t
  :after spaceline-config
  :config
  (setq
   powerline-height (truncate (* 1.0 (frame-char-height)))
   powerline-default-separator 'utf-8
   powerline-color1 "gray30"
   powerline-color2 "gray45"
   )
)

(fset 'yes-or-no-p 'y-or-n-p)
(normal-erase-is-backspace-mode 1)
