(use-package yasnippet  :ensure t
  :hook (prog-mode . yas-global-mode)
  :config
  (setq yas-verbosity 1
        yas-wrap-around-region t))

(use-package yasnippet-snippets
  :ensure t)

(use-package inf-ruby
  :ensure t)

(use-package f
  :ensure t)

(use-package rake
  :ensure t)

(use-package inflections
  :ensure t)

(use-package markdown-preview-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

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

(use-package bundler
  :ensure t)

(use-package rspec-mode
  :ensure t
  :config
  (setq-default rspec-use-spring-when-possible t)
  (rspec-install-snippets))

(add-hook 'after-init-hook 'inf-ruby-switch-setup)

(use-package projectile-rails
  :ensure t
  :config
  (projectile-rails-global-mode))

(use-package rubocop :ensure t)
(use-package feature-mode :ensure t) ;; cucumber

(use-package terraform-mode :ensure t)

(use-package groovy-mode
  :ensure t)

(use-package elixir-mode
  :ensure t)

(use-package alchemist
  :ensure t)

(use-package flycheck-mix
  :ensure t)

(use-package flycheck-credo
  :ensure t)

(use-package grip-mode
  :ensure t
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode)))

(use-package company-restclient
  :ensure t
  :config
  (add-to-list 'company-backends 'company-restclient))

;; Completion

;; (use-package lsp-mode
;;   :ensure t
;;   :hook (prog-mode . lsp)
;;   :commands lsp
;;   :config
;;   (add-to-list 'lsp-language-id-configuration '(js2-mode "javascript")))

;; (use-package company-lsp
;;   :ensure t
;;   :config
;;   (add-to-list 'company-backends 'company-lsp))

;; (use-package lsp-ui :commands lsp-ui-mode)
