;; [HACK: Native Comp + Library Paths]
;; For whatever reason, I need to do this to avoid errors when
;; invoking the native compilation driver?
(when (equal system-type 'darwin)
  (setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/11:/usr/local/opt/libgccjit/lib/gcc/11:/usr/local/opt/gcc/lib/gcc/11/gcc/x86_64-apple-darwin21/11/"))
