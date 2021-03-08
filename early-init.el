;; Make libgccjit able to use gcc which installed from homebrew
(setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/10:/usr/local/opt/gcc/lib/gcc/10/gcc/x86_64-apple-darwin20/10.2.0")

(setq warning-minimum-level :error)
