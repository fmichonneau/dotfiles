;; == Markdown ==
(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("\\.text\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'". gfm-mode))
  :init (setq markdown-command "pandoc")
  )

(use-package flyspell
  :defer t
  :diminish (flyspell-mode . " φ"))

;; == LaTex / AucTeX ==
(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-PDF-mode t)
  (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
  
  ;; Don't use Helm for the reftex-citation lookup
  (eval-after-load 'helm-mode
    '(add-to-list 'helm-completing-read-handlers-alist '(reftex-citation . nil))
    )
  
)


;; ==== Writeroom mode ====
(use-package writeroom-mode
  :defer t
  :config
  (with-eval-after-load 'writeroom-mode
    (defcustom writeroom-text-scale 2 "Text scale for writeroom" :type 'integer)
    (add-to-list 'writeroom-global-functions
		 (lambda (arg)
		   (require 'face-remap)
		   (if arg (progn
			     (set (make-local-variable '*local-text-scale*) text-scale-mode-amount)
			     (text-scale-set writeroom-text-scale))
		     (text-scale-set
		      (if (boundp '*local-text-scale*) *local-text-scale* 0)))))) )


;; ==== jekyll modes ====
(use-package jekyll-modes
  :defer t
  :config
  ((add-to-list 'auto-mode-alist '("\\.md$" . jekyll-markdown-mode))
   (add-to-list 'auto-mode-alist '("\\.html" . jekyll-html-mode))
   )
  ;; disable auto-fill-modes in jekyll modes
  (add-hook 'jekyll-markdown-mode-hook
	    '(lambda ()
	       (auto-fill-mode -1)
	       ))
  (add-hook 'jekyll-html-mode-hook
	    '(lambda ()
	       (auto-fill-mode -1)
	       ))
  )
