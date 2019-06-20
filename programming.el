(use-package smartparens
  :straight t
  :config
  (require 'smartparens-config))

(use-package markdown-mode
  :straight t
  :config
  (setq markdown-command "/usr/bin/pandoc"))
(use-package markdown-preview-mode :straight t)
(use-package yaml-mode :straight t)
(use-package dockerfile-mode :straight t)

(use-package web-mode
  :straight
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
  :straight t
  :hook web-mode
  :hook css-mode)

(use-package haskell-mode :straight t)
(use-package intero
  :straight t
  :config
  (intero-global-mode 1))

(use-package bundler :straight t)

;; Ruby and Rails
(use-package inf-ruby
  :straight t
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package inflections :straight t)
(use-package rake :straight t)
(use-package projectile-rails
  :straight t
  :config
  (projectile-rails-global-mode))

(use-package rspec-mode
  :straight t
  :config
  (rspec-install-snippets))

(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(use-package robe
  :straight t
  :config
  (global-robe-mode))

(eval-after-load 'company
  '(push 'company-robe company-backends))

(use-package yari :straight t) ;; ruby documentation
(use-package rubocop :straight t)
(use-package feature-mode :straight t) ;; cucumber

(use-package alchemist :straight t)
(use-package terraform-mode :straight t)
;; END

;; Python
(use-package elpy
  :straight t
  :config
  (elpy-enable))

(use-package pyenv-mode
  :straight t
  :config
  (pyenv-mode))
;; END
