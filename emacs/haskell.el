;; ## haskell-mode
(require 'haskell)
(require 'haskell-indentation)

(add-hook 'haskell-mode-hook 'turn-on-haskell-identation)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(define-key haskell-mode-map (kbd "C-c l") 'haskell-check)
(define-key haskell-mode-map (kbd "C-c m") 'compile)
