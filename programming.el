(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config))

(use-package markdown-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package dockerfile-mode :ensure t)

(use-package web-mode
  :ensure t
    :mode (("\\.html?\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.html.eex\\'" . web-mode)
         )
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-css-colorization t))

(add-hook 'prog-mode (lambda () (setq truncate-lines t)))

(use-package emmet-mode
  :ensure t
  :hook web-mode
  :hook css-mode)

(use-package haskell-mode :ensure t)
(use-package intero
  :ensure t
  :config
  (intero-global-mode 1))

;; Ruby and Rails
(use-package projectile-rails
  :ensure t
  :config
  (projectile-rails-global-mode))

(use-package inf-ruby :ensure t
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package rspec-mode
  :ensure t
  :config
  (rspec-install-snippets))

(use-package yari :ensure t) ;; ruby documentation

(use-package alchemist
  :ensure t)

(use-package terraform-mode
  :ensure t)
;; END
