;; Resources
;; - file structure from: https://github.com/gjstein/emacs.d/tree/master/config
;; - https://github.com/howardabrams/dot-files/blob/master/emacs.org




;; User Info

(setq user-full-name "François Michonneau")
(setq user-mail-address "francois.michonneau@gmail.com")


;; Install use-package if necessary
(require 'package)
(setq package-enable-at-startup nil)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "marmalade" (concat proto "://marmalade-repo.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;; Set the path variable
(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

;; === Face Customization ===
(load-file "~/.emacs.d/config/init-10-face.el")

;; === ====
(load-file "~/.emacs.d/config/init-20-nav-interface.el")

;; === LaTeX and Markdown modes ===
(load-file "~/.emacs.d/config/init-30-md+tex.el")


;; === coding and editing ===
(load-file "~/.emacs.d/config/init-40-coding.el")
(load-file "~/.emacs.d/config/init-41-coding-r.el")

;; init.el ends
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "xelatex")
 '(auth-sources (quote ("~/.authinfo.gpg" "~/.authinfo" "~/.netrc")))
 '(blink-cursor-mode nil)
 '(c-basic-offset 4)
 '(c-default-style "bsd")
 '(case-fold-search t)
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "98cc377af705c0f2133bb6d340bf0becd08944a588804ee655809da5d8140de6" "5dc0ae2d193460de979a463b907b4b2c6d2c9c4657b2e9e66b8898d2592e3de5" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "7122873f9ac192e4f2cfafe1679fe6b3db658ac64593efe4bc10c52d7573c6c1" "90d329edc17c6f4e43dbc67709067ccd6c0a3caa355f305de2041755986548f2" "19ba41b6dc0b5dd34e1b8628ad7ae47deb19f968fe8c31853d64ea8c4df252b8" "986e44c951ca6f5afc46b2942881fa95f9793e120256a209d85a19ae9e4bbf7a" "4f249287f4867a103144a3e14e0cb59e36bfbfdb9013492fbe6128c75bbe0d5d" "9e609fed2a991a7f75c17e57f3277d21539baedc8b56ad9f6807a7febf0fdec0" default)))
 '(ess-R-font-lock-keywords
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
     (ess-R-fl-keyword:F&T . t))))
 '(ess-auto-newline t)
 '(ess-fill-calls-newlines t)
 '(ess-function-template " <- function( ) {

}
")
 '(ess-roxy-tags-param
   (quote
    ("author" "aliases" "concept" "details" "examples" "format" "keywords" "method" "exportMethod" "name" "note" "param" "include" "references" "return" "seealso" "source" "docType" "title" "TODO" "usage" "import" "exportClass" "exportPattern" "S3method" "inheritParams" "importFrom" "importClassesFrom" "importMethodsFrom" "useDynLib" "rdname" "section" "slot" "template" "family")))
 '(ess-roxy-template-alist
   (quote
    (("description" . ".. content for \\description{} (no empty lines) ..")
     ("details" . ".. content for \\details{} ..")
     ("title" . "")
     ("param" . "")
     ("return" . "")
     ("export"))))
 '(ess-swv-processor (quote knitr))
 '(flycheck-r-linters "with_defaults(object_camel_case_linter=NULL)")
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "_site")))
 '(inferior-R-args "--no-restore-history --no-save ")
 '(magit-commit-arguments (quote ("--gpg-sign=037DBA3399ECB8E4")))
 '(package-selected-packages
   (quote
    (all-the-icons easy-jekyll jekyll-modes evil-tutor beacon focus dockerfile-mode company-irony emacs-material-theme yasnippet yaml-mode writeroom-mode use-package string-inflection ssh-agency polymode nlinum multiple-cursors math-symbol-lists markdown-mode magithub linum-off keychain-environment key-combo git-commit-insert-issue gh flymake-yaml flycheck flx-ido exec-path-from-shell ess electric-spacing company-flx checkbox auto-package-update auctex ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
