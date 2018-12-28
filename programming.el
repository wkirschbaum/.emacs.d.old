(use-package markdown-mode :ensure t)
(use-package yaml-mode :ensure t)

(add-hook 'prog-mode (lambda () (setq truncate-lines t)))

(use-package haskell-mode :ensure t)
(use-package intero
  :ensure t
  :config
(intero-global-mode 1))
