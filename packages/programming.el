(use-package yasnippet  :straight t
  :hook (prog-mode . yas-global-mode)
  :config
  (setq yas-verbosity 1
        yas-wrap-around-region t))

(use-package yasnippet-snippets
  :straight t)

(use-package inf-ruby
  :straight t)

(use-package f
  :straight t)

(use-package rake
  :straight t)

(use-package inflections
  :straight t)

(use-package markdown-preview-mode
  :straight t)

(use-package yaml-mode
  :straight t)

(use-package dockerfile-mode
  :straight t)

(use-package web-mode
  :straight t
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

(use-package bundler
  :straight t)

(use-package rspec-mode
  :straight t
  :config
  (setq-default rspec-use-spring-when-possible t)
  (rspec-install-snippets))

(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(use-package projectile-rails
  :straight t
  :config
  (projectile-rails-global-mode))

(use-package robe
  :straight t
  :config
  (global-robe-mode))

(eval-after-load 'company
  '(push 'company-robe company-backends))

(use-package rubocop :straight t)
(use-package feature-mode :straight t) ;; cucumber

(use-package alchemist :straight t)
(use-package terraform-mode :straight t)

(use-package groovy-mode
  :straight t)

(use-package coverage
  :straight t
  :config
  :bind ("C-c , ,". coverage-mode))
